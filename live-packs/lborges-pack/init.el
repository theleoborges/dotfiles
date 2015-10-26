;; User pack init file
;;
;; Use this file to initiate the pack configuration.
;; See README for more information.

;; Use Marmalade
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
;; Use Melpa
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

;; Customise the location for installed packages
(setq package-user-dir "~/.live-packs/lborges-pack/lib/elpa")

;; Add all packages to the load path
(let ((base "~/.live-packs/lborges-pack/lib/elpa"))
  (add-to-list 'load-path base)
  (dolist (f (directory-files base))
    (let ((name (concat base "/" f)))
      (when (and (file-directory-p name)
                 (not (equal f ".."))
                 (not (equal f ".")))
        (add-to-list 'load-path name)))))

(global-auto-revert-mode)

;; Projectile
(require 'projectile)
(projectile-global-mode)



;; Load bindings config
(live-load-config-file "bindings.el")
(setq live-disable-zone t)

(setq js-indent-level 2)
(setq js2-basic-offset 2)
(setq js2-bounce-indent-p nil)
(setq c-basic-offset 2)


(setq js2-mode-hook
      '(lambda () (progn
                    (set-variable 'indent-tabs-mode nil))))

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
  (paredit-backward-up)
  (open-line 1))
(global-set-key (kbd "C-c D")   'paredit-duplicate-custom)


;;;;
;;;; JS
;;;;


;; FFIP
(setq ffip-find-options "-not -regex \".*node_modules.*\"")

;; SLIME
;; (live-add-pack-lib "slime")
;; (require 'slime-autoloads)
;; (eval-after-load 'js2-mode
;;   '((lambda ()
;;       (define-key js2-mode-map (kbd "C-c C-l") 'slime-eval-buffer)
;;       (define-key js2-mode-map (kbd "C-c C-r") 'slime-eval-region)
;;       (setq ffip-patterns (cons "*.json" ffip-patterns)))))

(setq js-indent-level 4)
(setq js2-basic-offset 4)
(setq js2-bounce-indent-p false)

;; Faster buffer navigation
(eval-after-load 'paredit
    '(dolist (key '("M-<up>" "M-<down>" "M-<right>" "M-<left>"))
       (define-key paredit-mode-map (read-kbd-macro key) nil)))
(eval-after-load 'paredit
  '((lambda ()
      (define-key paredit-mode-map (kbd "C-x )") 'paredit-close-round-and-newline)
      (define-key paredit-mode-map (kbd "C-M-k") 'kill-sexp)
      )))

(global-set-key (kbd "M-<left>")  'windmove-left)
(global-set-key (kbd "M-<right>") 'windmove-right)
(global-set-key (kbd "M-<up>")    'windmove-up)
(global-set-key (kbd "M-<down>")  'windmove-down)

;; Expand Region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C--") 'er/contract-region)


;;Multiple cursors
(live-add-pack-lib "multiple-cursors")
(require 'multiple-cursors)

(global-set-key (kbd "C-x r t") 'mc/edit-lines)
(global-set-key (kbd "C->")     'mc/mark-next-like-this)
(global-set-key (kbd "C-<")     'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; hs-minor-mode
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'java-mode-hook       'hs-minor-mode)
(add-hook 'lisp-mode-hook       'hs-minor-mode)
(add-hook 'clojure-mode-hook    'hs-minor-mode)

(setq hs-show-all-p t)
(defun hs-mode-toggle-show-all ()
  (interactive)
  (if hs-show-all-p
    (hs-hide-all)
    (hs-show-all))
  (setq hs-show-all-p (not hs-show-all-p)))

(global-set-key (kbd "C-.")     'hs-toggle-hiding)
(global-set-key (kbd "C-c C-.") 'hs-mode-toggle-show-all)


;; Emacs server
;; opens files from emacs client in new frame
(add-hook 'server-switch-hook
          (lambda nil
            (let ((server-buf (current-buffer)))
              (bury-buffer)
              (switch-to-buffer-other-frame server-buf))))



;; Markdown
(setq ispell-program-name "/usr/local/bin/aspell")

(defun make-markdown-look-better ()
  (interactive)
;;  (text-scale-increase 1)
  (linum-mode -1)
  (set-window-margins nil 2 1))


(add-hook 'markdown-mode-hook 'make-markdown-look-better)

;;;;
;;;; Scala
;;;;

;;(add-to-list 'load-path "~/libraries/ensime-latest/elisp")
;;(add-to-list 'load-path "~/libraries/ensime-emacs")
;; (require 'ensime)
;; (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
;; (eval-after-load 'ensime
;;   '((lambda ()
;;       (define-key ensime-mode-map (kbd "C-c C-r") 'ensime-inf-eval-region)
;;       (define-key ensime-mode-map (kbd "C-x C-e") 'ensime-inf-eval-definition)
;;       (define-key ensime-mode-map (kbd "C-c C-l") 'ensime-inf-load-file)
;;       (define-key ensime-mode-map (kbd "C-c C-b i") 'ensime-import-type-at-point)
;;       )))


;;
;; Disable bloody emacs dialogs
;;
(defadvice yes-or-no-p (around prevent-dialog activate)
  "Prevent yes-or-no-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))
(defadvice y-or-n-p (around prevent-dialog-yorn activate)
  "Prevent y-or-n-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))

(setq debug-on-error t)


;; neotree
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(global-set-key [f9] 'neotree-dir)
(setq projectile-switch-project-action 'neotree-projectile-action)


;; minibuffer
;; (lookup-key minibuffer-local-filename-must-match-map (kbd "<right>"))
;; (lookup-key minibuffer-local-filename-completion-map (kbd "<right>"))
;; (lookup-key (current-global-map) (kbd "<right>"))

;; (define-key minibuffer-local-completion-map (kbd "C-n") 'cycle-mini-next-completion)
;; (define-key minibuffer-local-completion-map (kbd "C-p") 'cycle-mini-previous-completion)

;; cider
(setq cider-test-infer-test-ns (lambda (ns) ns))

;; Buffers
(defun switch-to-messages-buffer ()
  (interactive)
  (switch-to-buffer "*Messages*"))

(global-set-key (kbd "C-c C-s") 'switch-to-messages-buffer)


;; Purescript
(add-hook 'purescript-mode-hook 'turn-on-purescript-indentation)


;; Projectile
(global-set-key (kbd "C-c p s a") 'projectile-ag)
