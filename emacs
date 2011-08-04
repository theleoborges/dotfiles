;; slime config
(add-to-list 'load-path "~/hacking/lisp/slime/")  ; your SLIME directory
;;(setq inferior-lisp-program "/opt/local/bin/sbcl") ; your Lisp system
(require 'slime)
(slime-setup)

;;clojure config
(add-to-list 'load-path "~/.emacs.d/")
(require 'package)
(add-to-list 'package-archives
	          '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)
(setq inferior-lisp-program "cake repl") ; your Lisp system
(setq lisp-indent-offset 2) ; reduce identation length. It's huge by default

;;encoding
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;;meta key fix
(set-keyboard-coding-system nil)


;;syntax highlighting
(add-to-list 'load-path "~/hacking/lisp/color-theme/")
(add-to-list 'load-path "~/hacking/lisp/color-theme/emacs-color-theme-solarized/")

;; color theme
(require 'color-theme)
(require 'color-theme-solarized)
(color-theme-initialize)
(load-file "~/hacking/lisp/custom-themes/pastels-on-dark-theme.el")
(load-file "~/hacking/lisp/custom-themes/color-theme-blackboard.el")
;;(color-theme-pastels-on-dark)

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
