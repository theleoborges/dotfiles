;;Custom functions
(defun duplicate-line ()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank))

(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
    (filename (buffer-file-name)))
    (if (not filename)
    (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
      (message "A buffer named '%s' already exists!" new-name)
    (progn
      (rename-file name new-name 1)
      (rename-buffer new-name)
      (set-visited-file-name new-name)
      (set-buffer-modified-p nil))))))

(defun extract-local-variable (var-name)
  (interactive "sVar Name: ")
  (let ((code (delete-and-extract-region (region-beginning)
                                         (region-end))))
    (insert var-name)
    (newline)
    (beginning-of-line)
    (previous-line)
    (setq start  (point))
    (insert var-name
            " = " code)
    (indent-region start (point))
    (insert "\n")
    ))

;;custom key bindings
(global-set-key "\C-C\C-D" 'duplicate-line)
(global-set-key "\C-C\C-N" 'rename-file-and-buffer)
(add-hook 'ruby-mode-hook
	  (lambda ()
	    (define-key ruby-mode-map
              "\C-C\C-C" 'comment-or-uncomment-region)))
(add-hook 'clojure-mode-hook
          '(lambda ()
             (define-key clojure-mode-map 
               "\C-C\C-C" 'comment-or-uncomment-region)))
