;;Custom functions
(defun duplicate-line ()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank))


(defun paredit-duplicate-custom ()
  (interactive)
  (paredit-kill)
  (yank)
  (yank)
  (backward-char)
  (paredit-backward-up))

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

;;custom key bindings
(global-set-key (kbd "C-c d") 'duplicate-line)
(global-set-key (kbd "C-c D")   'paredit-duplicate-custom)

(global-set-key "\C-C\C-N" 'rename-file-and-buffer)
(add-hook 'ruby-mode-hook
	  (lambda ()
	    (define-key ruby-mode-map
              "\C-C\C-C" 'comment-or-uncomment-region)))
(add-hook 'clojure-mode-hook
          '(lambda ()
             (define-key clojure-mode-map 
               "\C-C\C-C" 'comment-or-uncomment-region)))
