
(setq user-full-name "Aaron Bedra")
(setq user-mail-address "aaron@aaronbedra.com")

(setenv "PATH" (concat "/usr/local/bin:/Users/abedra/.cabal/bin:" (getenv "PATH")))
(require 'cl)

(load "package")
(package-initialize)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(setq package-archive-enable-alist '(("melpa" deft magit)))

(defvar abedra/packages '(ac-slime
                          auto-complete
                          autopair
                          clojure-mode
                          clojure-test-mode
                          coffee-mode
                          csharp-mode
                          deft
                          erlang
                          feature-mode
                          flycheck
                          gist
                          go-mode
                          haml-mode
                          haskell-mode
                          htmlize
                          magit
                          markdown-mode
                          marmalade
                          nodejs-repl
                          nrepl
                          o-blog
                          org
                          paredit
                          php-mode
                          puppet-mode
                          restclient
                          rvm
                          scala-mode
                          smex
                          sml-mode
                          solarized-theme
                          web-mode
                          writegood-mode
                          yaml-mode)
  "Default packages")

(defun abedra/packages-installed-p ()
  (loop for pkg in abedra/packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))

(unless (abedra/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg abedra/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

(setq inhibit-splash-screen t
      initial-scratch-message nil)

(when (locate-library "clojure-mode")
  (setq initial-major-mode 'clojure-mode))

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b"))))

(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

(setq tab-width 2
      indent-tabs-mode nil)

(setq make-backup-files nil)

(defalias 'yes-or-no-p 'y-or-n-p)

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-c C-k") 'compile)

(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell t)
(show-paren-mode t)

(defvar abedra/vendor-dir (expand-file-name "vendor" user-emacs-directory))
(add-to-list 'load-path abedra/vendor-dir)

(dolist (project (directory-files abedra/vendor-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

(setq org-log-done t
      org-todo-keywords '((sequence "TODO" "INPROGRESS" "DONE"))
      org-todo-keyword-faces '(("INPROGRESS" . (:foreground "blue" :weight bold))))
(add-hook 'org-mode-hook
          (lambda ()
            (flyspell-mode)))
(add-hook 'org-mode-hook
          (lambda ()
            (writegood-mode)))

(global-set-key (kbd "C-c a") 'org-agenda)
(setq org-agenda-show-log t)
(setq org-agenda-files (list "~/Dropbox/org/personal.org"
                             "~/Dropbox/org/groupon.org"))

(require 'org)
(require 'org-install)
(require 'org-habit)
(add-to-list 'org-modules "org-habit")
(setq org-habit-preceding-days 7
      org-habit-following-days 1
      org-habit-graph-column 80
      org-habit-show-habits-only-for-today t
      org-habit-show-all-today t)

(require 'ob)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((sh . t)
   (ditaa . t)))

(add-to-list 'org-babel-tangle-lang-exts '("clojure" . "clj"))

(defvar org-babel-default-header-args:clojure
  '((:results . "silent") (:tangle . "yes")))

(defun org-babel-execute:clojure (body params)
  (lisp-eval-string body)
  "Done!")

(provide 'ob-clojure)

(setq org-src-fontify-natively t
      org-confirm-babel-evaluate nil)

(add-hook 'org-babel-after-execute-hook (lambda ()
                                          (condition-case nil
                                              (org-display-inline-images)
                                            (error nil)))
          'append)

(setq org-ditaa-jar-path "~/.emacs.d/vendor/ditaa0_9.jar")

(setq deft-directory "~/Dropbox/deft")
(setq deft-use-filename-as-title t)
(setq deft-extension "org")
(setq deft-text-mode 'org-mode)

(setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(ido-mode t)
(setq ido-enable-flex-matching t
      ido-use-virtual-buffers t)

(setq column-number-mode t)

(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(require 'autopair)

(setq lisp-modes '(lisp-mode
                   emacs-lisp-mode
                   common-lisp-mode
                   scheme-mode
                   clojure-mode))

(defvar lisp-power-map (make-keymap))
(define-minor-mode lisp-power-mode "Fix keybindings; add power."
  :lighter " (power)"
  :keymap lisp-power-map
  (paredit-mode t))
(define-key lisp-power-map [delete] 'paredit-forward-delete)
(define-key lisp-power-map [backspace] 'paredit-backward-delete)

(defun abedra/engage-lisp-power ()
  (lisp-power-mode t))

(dolist (mode lisp-modes)
  (add-hook (intern (format "%s-hook" mode))
            #'abedra/engage-lisp-power))

(setq inferior-lisp-program "clisp")
(setq scheme-program-name "racket")

(require 'auto-complete-config)
(ac-config-default)

(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))

(defun cleanup-region (beg end)
  "Remove tmux artifacts from region."
  (interactive "r")
  (dolist (re '("\\\\│\·*\n" "\W*│\·*"))
    (replace-regexp re "" nil beg end)))

(global-set-key (kbd "C-x M-t") 'cleanup-region)
(global-set-key (kbd "C-c n") 'cleanup-buffer)

(setq-default show-trailing-whitespace t)

(setq flyspell-issue-welcome-flag nil)
(setq-default ispell-program-name "/usr/local/bin/aspell")
(setq-default ispell-list-command "list")

(add-hook 'ruby-mode-hook
          (lambda ()
            (autopair-mode)))

(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile" . ruby-mode))

(rvm-use-default)

(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

(defun coffee-custom ()
  "coffee-mode-hook"
  (make-local-variable 'tab-width)
  (set 'tab-width 2))

(add-hook 'coffee-mode-hook 'coffee-custom)

(defun js-custom ()
  "js-mode-hook"
  (setq js-indent-level 2))

(add-hook 'js-mode-hook 'js-custom)

(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdown$" . markdown-mode))
(add-hook 'markdown-mode-hook (lambda () (visual-line-mode t)))
(setq markdown-command "pandoc --smart -f markdown -t html")
(setq markdown-css-path (expand-file-name "markdown.css" abedra/vendor-dir))

(define-derived-mode cpsa-mode scheme-mode
  (setq mode-name "CPSA")
  (setq cpsa-keywords '("defmacro" "defprotocol" "defrole" "defskeleton" "defstrand"))
  (setq cpsa-functions '("cat" "hash" "enc" "string" "ltk" "privk" "pubk" "invk" "send" "recv"  "non-orig" "uniq-orig" "trace" "vars"))
  (setq cpsa-types '("skey" "akey" "name" "text"))
  (setq cpsa-keywords-regexp (regexp-opt cpsa-keywords 'words))
  (setq cpsa-functions-regexp (regexp-opt cpsa-functions 'words))
  (setq cpsa-types-regexp (regexp-opt cpsa-types 'words))
  (setq cpsa-font-lock-keywords
        `(
          (,cpsa-keywords-regexp . font-lock-keyword-face)
          (,cpsa-functions-regexp . font-lock-function-name-face)
          (,cpsa-types-regexp . font-lock-type-face)))
  (setq font-lock-defaults '((cpsa-font-lock-keywords))))

(add-to-list 'auto-mode-alist '("\\.cpsa$" . cpsa-mode))

(if window-system
    (load-theme 'solarized-light t)
  (load-theme 'wombat t))
