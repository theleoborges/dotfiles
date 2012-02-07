(add-to-list 'load-path "~/.emacs.d/custom/")
(load "init-default-packages")
(load "init-project-roots")
(load "init-ido-customizations")
(load "init-custom-functions")

;;Ruby/Rails
(add-to-list 'load-path "~/.emacs.d/rhtml/")
(require 'rhtml-mode)

;;Customizations
(setq inhibit-startup-message t)        ; Do without annoying startup msg.

;;Paredit
(add-hook 'ruby-mode-hook       'esk-paredit-nonlisp)

;;Color themes
(add-to-list 'load-path "/Users/lborges/.emacs.d/themes/emacs-color-theme-solarized/")
(require 'color-theme-solarized)
(color-theme-solarized-dark)

;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inferior-lisp-program "lein repl"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
