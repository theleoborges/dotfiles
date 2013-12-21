;;Custom functions
(defun duplicate-line ()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank))
(global-set-key (kbd "C-c d") 'duplicate-line)

(defun paredit-duplicate-custom ()
  (interactive)
  (paredit-kill)
  (yank)
  (yank)
  (backward-char)
  (paredit-backward-up))
(global-set-key (kbd "C-c D")   'paredit-duplicate-custom)

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
(global-set-key "\C-C\C-N" 'rename-file-and-buffer)

(defun fonts-big ()
  (interactive)
  (text-scale-adjust 4))
(define-key global-map [f7] 'fonts-big)

(defun fonts-normal ()
  (interactive)
  (text-scale-adjust 0))
(define-key global-map [f8] 'fonts-normal)

;;custom key bindings
(add-hook 'ruby-mode-hook
	  (lambda ()
	    (define-key ruby-mode-map
              "\C-C\C-C" 'comment-or-uncomment-region)))
(add-hook 'clojure-mode-hook
          '(lambda ()
             (define-key clojure-mode-map 
               "\C-C\C-C" 'comment-or-uncomment-region)))
