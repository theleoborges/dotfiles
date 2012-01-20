;; slime config
(add-to-list 'load-path "~/.emacs.d/lisp/slime/")  ; your SLIME directory
;;(setq inferior-lisp-program "/opt/local/bin/sbcl") ; your Lisp system
(require 'slime)
(slime-setup)

;;clojure config
(add-to-list 'load-path "~/.emacs.d/")
(require 'package)
(add-to-list 'package-archives
	          '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)
;;(setq inferior-lisp-program "cake repl") ; your Lisp system
(setq inferior-lisp-program "lein repl") ; your Lisp system
(setq lisp-indent-offset 2) ; reduce identation length. It's huge by default

;;paredit
(add-to-list 'load-path "~/.emacs.d/list/paredit.el")
(autoload 'paredit-mode "paredit"
      "Minor mode for pseudo-structurally editing Lisp code." t)	
(add-hook 'lisp-mode-hook             (lambda () (paredit-mode +1)))
(add-hook 'scheme-mode-hook           (lambda () (paredit-mode +1)))
(add-hook 'clojure-mode-hook           (lambda () (paredit-mode +1)))

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

;;custom key bindings
(global-set-key "\C-C\C-D" 'duplicate-line)
(global-set-key "\C-C\C-N" 'rename-file-and-buffer)

;;encoding
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;;meta key fix
(set-keyboard-coding-system nil)


;;syntax highlighting
(add-to-list 'load-path "~/.emacs.d/lisp/color-theme/")
(add-to-list 'load-path "~/.emacs.d/lisp/color-theme/emacs-color-theme-solarized/")

;; color theme
(require 'color-theme)
(require 'color-theme-solarized)
(color-theme-initialize)
(load-file "~/.emacs.d/lisp/custom-themes/pastels-on-dark-theme.el")
(load-file "~/.emacs.d/lisp/custom-themes/color-theme-blackboard.el")
;(color-theme-pastels-on-dark)
(color-theme-charcoal-black)

;;rails inferior mode
;;(add-to-list 'load-path (expand-file-name "~/.emacs.d/rails-minor-mode"))
;;(require 'rails)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(gud-gdb-command-name "gdb --annotate=1")
 '(large-file-warning-threshold nil)
 '(scheme-program-name "/usr/local/lib/mit-scheme-x86-64/mit-scheme"))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
