;;; ensime-auto-complete.el
;;
;;;; License
;;
;;     Copyright (C) 2010 Aemon Cannon
;;
;;     This program is free software; you can redistribute it and/or
;;     modify it under the terms of the GNU General Public License as
;;     published by the Free Software Foundation; either version 2 of
;;     the License, or (at your option) any later version.
;;
;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;     GNU General Public License for more details.
;;
;;     You should have received a copy of the GNU General Public
;;     License along with this program; if not, write to the Free
;;     Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;;     MA 02111-1307, USA.

(require 'auto-complete)
(require 'yasnippet)
(require 'scala-mode2-syntax)

(defcustom ensime-completion-style 'company
  "Should be either 'company or 'auto-complete."
  :type 'symbol
  :group 'ensime-ui)

(defcustom ensime-ac-enable-argument-placeholders t
  "If non-nil, insert placeholder arguments in the buffer on completion."
  :type 'boolean
  :group 'ensime-ui)

(defcustom ensime-ac-override-settings t
    "If non-nil, override auto-complete settings."
    :type 'boolean
    :group 'ensime-ui)

(defcustom ensime-ac-case-sensitive nil
  "If non-nil, omit completions that don't match the case of prefix."
  :type 'boolean
  :group 'ensime-ui)

(defvar ensime-ac-max-results 30
  "Maximum number of completions to request in one call to server.")

(defun ensime-ac-completion-candidates (&optional prefix)
  "Return candidate list."
  (let* ((completions
	  (plist-get (ensime--ask-server-for-completions) :candidates)))
    (mapcar (lambda (m) (propertize m 'summary (ensime-ac-get-doc m)))
	    completions)))

(defun ensime-ac-brief-type-sig (type-sig)
  "Return doc for given item."
  ;;(ensime-ac-brief-type-sig '(((("aemon" "Person"))) "Dude"))
  (let* ((sections (car type-sig))
	 (return-type (cadr type-sig)))
    (if sections
	(format "%s: %s"
		(mapconcat
		 (lambda (section)
		   (format "(%s)"
			   (mapconcat
			    (lambda (param-pair)
			      (format "%s: %s" (car param-pair) (cadr param-pair)))
			    section ", ")))
		 sections "=>") return-type)
      return-type)))

(defun ensime-ac-get-doc (item)
  "Return doc for given item."
  (ensime-ac-brief-type-sig (get-text-property 0 'type-sig item)))

(defun ensime-ac-candidate-to-insert (item)
  "Return to-insert for given item."
  (get-text-property 0 'to-insert item))

(defun ensime-pt-at-end-of-prev-line ()
  (save-excursion (forward-line -1)
		  (min
		   (- (point) 1)
		   (point-at-eol))))

(defun ensime-ac-completion-prefix ()
  "Starting at current point. Find the point of completion."
  (let ((point (re-search-backward "\\(\\W\\|[\t ]\\)\\([^\\. ]*\\)?"
				   (point-at-bol) t)))
    (if point (1+ point))
    ))


(defun ensime-ac-complete-action (&optional candidate-in)
  "Defines action to perform when user selects a completion candidate.
If the candidate is a callable symbol, add the meta-info about the
params and param types as text-properties of the completed name. This info will
be used later to give contextual help when entering arguments."
  (let* (;; When called by auto-complete-mode, grab from dynamic environment.
	 (candidate (or candidate-in candidate))
	 (name candidate)
	 (type-id (get-text-property 0 'type-id candidate))
	 (is-callable (get-text-property 0 'is-callable candidate))
	 (to-insert (ensime-ac-candidate-to-insert candidate))
	 (name-start-point (- (point) (length name))))

    ;; If an alternate to-insert string is available, delete the
    ;; candidate inserted into buffer and replace with to-insert
    (when to-insert
      (delete-char (- (length name)))
      (insert to-insert))

    ;; If this member is callable, use the type-id to lookup call completion
    ;; information to show parameter hints.
    (when is-callable

      (let* ((call-info (ensime-rpc-get-call-completion type-id))
	     (param-sections (ensime-type-param-sections call-info)))
	(when (and call-info param-sections)

	  ;; Insert space or parens depending on the nature of the
	  ;; call
	  (save-excursion
	    (let* ((is-operator
		    (and (= 1 (length param-sections))
			 (= 1 (length (plist-get
				       (car param-sections) :params)))
			 (null (string-match "[A-z]" name)))))
	      (if ensime-ac-enable-argument-placeholders
		  (let ((args (ensime-ac-call-info-argument-list
			       call-info is-operator)))
		    (cond
		     (is-operator (insert (concat " " args)))
		     (t (insert args))))
		(cond
		 (is-operator (insert " "))
		 (t (insert "()"))))))

	  (if (car param-sections)
	      (progn
		;; Save param info as a text properties of the member name..
		(add-text-properties name-start-point
				     (+ name-start-point (length name))
				     (list 'call-info call-info
					   ))

		;; Setup hook function to show param help later..
		(add-hook 'post-command-hook
			  'ensime-ac-update-param-help nil t)
		;; This command should trigger help hook..
		(forward-char))

	    ;; Otherwise, skip to the end
	    (forward-char 2))

	  )))))

(defun ensime-in-string-or-comment (pos)
  "A helper to determine if the text at point is in a string
   or comment, and therefore should not be considered as part
   of a paren-balancing calculation.

   TODO: Currently this relies on font-lock-mode. Could be
   better."
  (let ((face (plist-get (text-properties-at pos) 'face)))
    (and face
     (or
      (equal face 'font-lock-doc-face)
      (equal face 'font-lock-string-face)
      (equal face 'font-lock-comment-face)))))

(defun ensime-ac-get-active-param-info ()
  "Search backward from point for the param info of the call that
   we are currently completing."
  (save-excursion
    (catch 'return
      (let ((lbound (point-at-bol)) ;; TODO <-- what about multiline param lists
	    (balance 0))
	(backward-char 1)
	(while (> (point) lbound)
	  (cond
	   ((ensime-in-string-or-comment (point)) nil)
	   ((looking-at "\\s)") (decf balance))
	   ((looking-at "\\s(") (incf balance))
	   (t
	    (let ((call-info (get-text-property (point) 'call-info)))
	      (if (and (or (> balance 0)) call-info)
		  (throw 'return (list
				  :name-end-point (point)
				  :call-info call-info))))))
	  (backward-char 1))))))


(defun ensime-ac-update-param-help ()
  "When entering the arguments to a call, display a tooltip
   with the param names and types of the call."
  (let ((info (ensime-ac-get-active-param-info)))
    (if info
	(let* (;; To be used for tooltip positioning..
	       (name-end (plist-get info :name-end-point))
	       (call-info (plist-get info :call-info))
	       (signature (ensime-ac-call-info-signature call-info)))
	  (message signature))
      (remove-hook 'post-command-hook 'ensime-ac-update-param-help t))))

(defun ensime--yasnippet-escape (s) (s-replace "$" "\\$" s))

(defun ensime--build-yasnippet-for-call
    (param-sections &optional infix pass-function-block)
  "Returns a yasnippet template for a method call, where each argument is a
 tab-stop."
  (let ((tab-stop 0)
	(section-count 0))
     (mapconcat
      (lambda (sect)
	(incf section-count)
	(let* ((params (plist-get sect :params)))

	  (if (and pass-function-block
		   (= section-count (length param-sections)))

	      ;; If requested, expand the last section as an inline block.
	      (let* ((param-type (cadr (car params)))
		     (type-args (plist-get param-type :type-args))
		     (arg-types (-take (- (length type-args) 1) type-args))
		     (result-type (car (last type-args))))
		(if (ensime-type-is-by-name-p param-type) " { $0 }"
		  (concat
		   " { "
		   (let ((param-list
			  (mapconcat
			   (lambda (tpe)
			     (let ((type-name (ensime--yasnippet-escape
					       (ensime-type-name-with-args tpe))))
			       (format "${%s:%s}" (incf tab-stop) type-name)))
			   arg-types ", ")))
		     (if (> (length arg-types) 1)
			 (format "(%s)" param-list) param-list))
		   (let ((result-type-name (ensime--yasnippet-escape
					    (ensime-type-name-with-args result-type))))
		     (format " => ${%s:%s} }$0" (incf tab-stop) result-type-name)))))

	    ;; Otherwise build template for a standard parameter list.
	    (concat (if infix " " "(")
		    (mapconcat
		     (lambda (nm-and-tp)
		       (let ((param-name (ensime--yasnippet-escape (car nm-and-tp)))
			     (type-name (ensime--yasnippet-escape
						   (ensime-type-name-with-args
						    (cadr nm-and-tp)))))
			 (format "${%s:%s: %s}"
				 (incf tab-stop)
				 param-name type-name)))
		     params ", ")
		    (if infix "" ")")))))
      param-sections
      "")
     ))

(defun ensime-ac-call-info-argument-list (call-info &optional is-operator)
  "Return a pretty string representation of argument list."
  (let ((param-sections (plist-get call-info :param-sections)))
    (mapconcat
     (lambda (sect)
       (let* ((params (plist-get sect :params))
	      (is-implicit (plist-get sect :is-implicit))
	      (result
	       (concat (if is-operator "" "(")
		       (mapconcat
			(lambda (nm-and-tp)
			  (format
			   "%s:%s"
			   (propertize (car nm-and-tp)
				       'face font-lock-variable-name-face)
			   (propertize (ensime-type-name-with-args
					(cadr nm-and-tp))
				       'face font-lock-type-face)
			   ))
			params ", ") (if is-operator "" ")"))))
	 (if is-implicit
	     (propertize result 'face font-lock-comment-face)
	   result)
	 ))
     param-sections "=>" )))


(defun ensime-ac-call-info-signature (call-info)
  "Return a pretty string representation of a call-info object."
  (let ((param-sections (plist-get call-info :param-sections))
	(result-type (plist-get call-info :result-type)))
    (concat
     (ensime-ac-call-info-argument-list call-info)
     " => "
     (propertize
      (ensime-type-name-with-args result-type)
      'face font-lock-type-face)
     )))


(ac-define-source ensime-completions
  '((document . ensime-ac-get-doc)
    (candidates . (ensime-ac-completion-candidates ac-prefix))
    (prefix . ensime-ac-completion-prefix)
    (action . ensime-ac-complete-action)
    (requires . 0)
    (symbol . "f")
    ))


(defun ensime-ac-enable ()
  (when ensime-ac-override-settings
    (make-local-variable 'ac-sources)
	(setq ac-sources '(ac-source-ensime-completions))

	(make-local-variable 'ac-use-comphist)
	(setq ac-use-comphist nil)

	(make-local-variable 'ac-auto-show-menu)
	(setq ac-auto-show-menu 0.5)

	(make-local-variable 'ac-candidates-cache)
	(setq ac-candidates-cache nil)

	(make-local-variable 'ac-auto-start)
	(setq ac-auto-start nil)

	(make-local-variable 'ac-expand-on-auto-complete)
	(setq ac-expand-on-auto-complete t)

	(make-local-variable 'ac-use-fuzzy)
	(setq ac-use-fuzzy nil)

	(make-local-variable 'ac-dwim)
	(setq ac-dwim nil)

	(make-local-variable 'ac-use-quick-help)
	(setq ac-use-quick-help t)

;;    (defvar ac-delete-dups)
	(make-local-variable 'ac-delete-dups)
	(setq ac-delete-dups nil)

	(make-local-variable 'ac-ignore-case)
	(setq ac-ignore-case t)

	(make-local-variable 'ac-trigger-key)
	(ac-set-trigger-key "TAB")

	(auto-complete-mode 1)
  ))

(defun ensime-ac-disable ()
  (auto-complete-mode 0))

(defun ensime--company-unique-candidate-at-point ()
  "If the identifier preceding point is already complete, returns it as a fully
 annotated candidate. Otherwise returns nil."
  (let ((prefix (ensime-company 'prefix)))
    (when (> (length prefix) 0)
      (ensime-write-buffer nil t)
      (let* ((info (ensime-rpc-completions-at-point
		    2 ensime-ac-case-sensitive))
	     (candidates (ensime--annotate-completions (plist-get info :completions))))
	(when (and (= (length candidates) 1)
		   (string= prefix (car candidates)))
	  (car candidates))))))

(defun ensime-company-try-completion ()
  "Attempts a company-mode completion at point. Returns nil if
 completion is not available at point."
  (when company-mode
    (let ((unique-candidate (ensime--company-unique-candidate-at-point)))
      (cond
       ;; If the identifier is already complete, we must invoke parameter
       ;; expansion manually.
       (unique-candidate
	(ensime--yasnippet-complete-action unique-candidate)
	t)

       ((company-manual-begin)
	(company-complete-common)
	t)

       (t nil)))))

(defun ensime--at-bol ()
  (not (string-match "[^\s-]" (buffer-substring-no-properties
			       (point-at-bol)
			       (point)))))

(defun ensime-company-complete-or-indent ()
  "Try to complete, falling back to indentation."
  (interactive)
  (when (or (ensime--at-bol)
	    (not (ensime-company-try-completion)))
    (indent-according-to-mode)))

(defun ensime-company-enable ()
  (set (make-local-variable 'company-backends) '(ensime-company))
  (company-mode)
  (yas-minor-mode-on)
  (set (make-local-variable 'company-idle-delay) 0)
  (set (make-local-variable 'company-minimum-prefix-length) 2)
  (local-set-key [tab] 'ensime-company-complete-or-indent))

(defun ensime--yasnippet-complete-action (&optional candidate-in force-block)
  "If the candidate is a callable symbol, expand a yasnippet template for the
 argument list."
  (let* (;; When called by auto-complete-mode, grab from dynamic environment.
	 (candidate (or candidate-in candidate))
	 (name candidate)
	 (type-id (get-text-property 0 'type-id candidate))
	 (is-callable (get-text-property 0 'is-callable candidate))
	 (to-insert (ensime-ac-candidate-to-insert candidate))
	 (name-start-point (- (point) (length name)))
	 (call-info
	  (when is-callable (ensime-rpc-get-call-completion type-id)))
	 (param-sections
	  (when is-callable
	    (-filter
	     (lambda (sect)
	       (not (plist-get sect :is-implicit)))
	     (ensime-type-param-sections call-info))))
	 (is-operator
	  (and is-callable
	       (= 1 (length param-sections))
	       (= 1 (length (plist-get
			     (car param-sections) :params)))
	       (null (string-match "[A-z]" name))))
	 (is-field-assigner (s-ends-with? "_=" name)))


    (when is-field-assigner
      (delete-char (- 2))
      (insert " ="))

    ;; If we've completed an operator, get rid of superfluous '.'
    (when is-operator
      (delete-char (- (+ 1 (length name))))
      (insert " ")
      (insert name))

    ;; If an to-insert is available, delete the candidate inserted
    ;; into buffer and replace with to-insert
    (when to-insert
      (delete-char (- (length name)))
      (insert to-insert))

    (when is-callable
	(when (and call-info param-sections)
	  (let* ((maybe-braces (ensime-param-section-accepts-block-p
				(car (last param-sections))))
		 (pass-function-block
		  (and maybe-braces
		       (eq
			(or force-block
			    (read-char-choice
			     "Choose '{' or '(' " '( ?\{ ?\( ))) ?\{)))
		 (snippet
		  (ensime--build-yasnippet-for-call
		   param-sections
		   (or is-operator is-field-assigner)
		   pass-function-block)))
	      (yas-expand-snippet snippet (point) (point))
	    )))))

(defun ensime--annotate-completions (completions)
  "Maps plist structures to propertized elisp strings."
  (mapcar
   (lambda (m)
     (let* ((type-sig (plist-get m :type-sig))
	    (type-id (plist-get m :type-id))
	    (is-callable (plist-get m :is-callable))
	    (to-insert (plist-get m :to-insert))
	    (name (plist-get m :name))
	    (candidate name))
       (propertize candidate
		   'symbol-name name
		   'type-sig type-sig
		   'type-id type-id
		   'is-callable is-callable
		   'to-insert to-insert
		   ))) completions))

(defun ensime--ask-server-for-completions-async (callback)
  (ensime-write-buffer nil t)
  (ensime-rpc-async-completions-at-point 1000000 ensime-ac-case-sensitive
   (lexical-let ((continuation callback))
     (lambda (info)
       (let* ((candidates (ensime--annotate-completions
			   (plist-get info :completions))))
	 (funcall continuation candidates))))))

(defun ensime--ask-server-for-completions ()
  (ensime-write-buffer nil t)
  (let* ((info
	  (ensime-rpc-completions-at-point
	   ensime-ac-max-results
	   ensime-ac-case-sensitive))
	 (result (list :prefix (plist-get info :prefix)
		       :candidates (ensime--annotate-completions
				    (plist-get info :completions)))))
    result))


(defconst ensime--prefix-char-class "[a-zA-Z\\$0-9_#:<=>@!%&*+/?\\\\^|~-]")
(defun ensime--get-completion-prefix-at-point ()
  "Returns the prefix to complete."
  ;; As an optimization, first get an upper bound on the length of prefix using
  ;; ensime--prefix-char-class. Emacs's looking-back function is sloooooww.
  (let ((i (point)))
    (while (string-match ensime--prefix-char-class (char-to-string (char-before i)))
      (decf i))
    (let ((s (buffer-substring-no-properties i (point))))
      ;; Then use a proper scala identifier regex to verify.
      (if (string-match (concat scala-syntax:plainid-re "$") s)
	  (match-string 1 s) ""))))

(defun ensime-company (command &optional arg &rest _args)
  "Ensime backend for company-mode."
  (interactive (list 'interactive))
  (pcase command
    (`interactive (company-begin-backend 'ensime-company))

    (`prefix (ensime--get-completion-prefix-at-point))

    (`candidates
     ;; Just ignore if there's no connection.
     (when (ensime-connected-p)
       `(:async . ensime--ask-server-for-completions-async)))

    ;; Don't do client-side sorting (preserve server-side rankings).
    (`sorted t)

    ;; We handle dup removal on the server.
    (`duplicates nil)

    ;; We request *all* completions, so it's ok to let company manage caching.
    (`no-cache nil)

    ;; Show an inline signature for callable completions.
    (`annotation
     (concat (if (get-text-property 0 'is-callable arg) "" ": ")
	     (ensime-ac-brief-type-sig (get-text-property 0 'type-sig arg))))

    ;; Expand function formal parameters if we've completed a call.
    (`post-completion (ensime--yasnippet-complete-action arg))

    (`ignore-case t)
    (`require-match `never)
    (`doc-buffer nil) ;; TODO for docs!
    (`meta nil) ;; TODO for short docs!
    (`location nil) ;; TODO Maybe use at some point to link to definitions.
    (_ nil)
    ))

(defun ensime-completion-at-point-function ()
  "Standard Emacs 24+ completion function, handles completion-at-point requests.
 See: https://www.gnu.org/software/emacs/manual/html_node/elisp/Completion-in-Buffers.html"
  (let* ((prefix (ensime--get-completion-prefix-at-point))
	 (start (- (point) (length prefix)))
	 (end (point))
	 (props '(:annotation-function
		  (lambda (m)
		    (when (get-text-property 0 'is-callable m)
		      (ensime-ac-brief-type-sig
		       (get-text-property 0 'type-sig m))))
		  :exit-function
		  (lambda (m status)
		    (when (eq status 'finished)
		      (ensime-ac-complete-action m)))))
	 (completion-func
	  (lambda (prefix pred action)
	    (cond
	     ((eq action 'metadata)
	      '(metadata . ((display-sort-function . identity))))
	     (t
	      (complete-with-action
	       action (plist-get (ensime--ask-server-for-completions)
				 :candidates) prefix pred))))))
    `(,start ,end ,completion-func . ,props)))


(provide 'ensime-auto-complete)

;; Local Variables:
;; no-byte-compile: t
;; End:

