;;Project Settings
(add-to-list 'load-path "/Users/lborges/.emacs.d/project-root/")
(require 'project-root)
(setq project-roots
      `(
        ("Clojure projects"
         :root-contains-files ("project.clj")
         ;;:filename-regex ,(regexify-ext-list '(clj jsp css js xml html))
         )
        ("Grunt projects"
         :root-contains-files ("Gruntfile.js")
         ;;:filename-regex ,(regexify-ext-list '(clj jsp css js xml html))
         )
        ("Rails Projects"
         :root-contains-files ("app" "config" "db" "lib" "script")
         :exclude-paths '("log"))
        ("Git Projects"
         :root-contains-files (".git"))
        ("Generic Projects"
         :root-contains-files ("root"))        
        ))
      

;;(global-set-key (kbd "C-c p f") 'project-root-find-file)
(global-set-key (kbd "C-c p f") 'my-ido-project-files)
(global-set-key (kbd "C-c p g") 'project-root-grep)
(global-set-key (kbd "C-c p a") 'project-root-ack)
(global-set-key (kbd "C-c p d") 'project-root-goto-root)
(global-set-key (kbd "C-c p l") 'project-root-browse-seen-projects)

;;starts shell at root of project
(global-set-key (kbd "C-c p s")
                (lambda () (interactive)
                  (with-project-root
                      (ansi-term (getenv "SHELL")
                                 (concat (car project-details) "-shell")))))

(custom-set-variables
     '(haskell-mode-hook '(turn-on-haskell-indentation)))
