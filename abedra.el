
(setq user-full-name "Aaron Bedra")
(setq user-mail-address "aaron@aaronbedra.com")

(setenv "PATH" (concat "/usr/local/bin:/opt/local/bin:/usr/bin:/bin:/home/abedra/.cabal/bin" (getenv "PATH")))
(setenv "GOPATH" (concat (getenv "HOME") "/src/golang"))
(add-to-list 'exec-path (concat (getenv "GOPATH") "/bin"))
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
                          coffee-mode
                          csharp-mode
                          deft
                          erlang
                          feature-mode
                          flycheck
                          gist
                          go-autocomplete
                          go-eldoc
                          go-mode
                          graphviz-dot-mode
                          haml-mode
                          haskell-mode
                          htmlize
                          idris-mode
                          magit
                          markdown-mode
                          marmalade
                          nodejs-repl
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
      initial-scratch-message nil
      initial-major-mode 'org-mode)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)

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
(global-set-key (kbd "C-x g") 'magit-status)

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
(setq org-agenda-show-log t
      org-agenda-todo-ignore-scheduled t
      org-agenda-todo-ignore-deadlines t)
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
   (ditaa . t)
   (plantuml . t)
   (dot . t)
   (ruby . t)
   (js . t)
   (C . t)))

(add-to-list 'org-src-lang-modes (quote ("dot". graphviz-dot)))
(add-to-list 'org-src-lang-modes (quote ("plantuml" . fundamental)))
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

(add-hook 'org-mode-hook (lambda () (abbrev-mode 1)))

(define-skeleton skel-org-block-elisp
  "Insert an emacs-lisp block"
  ""
  "#+begin_src emacs-lisp\n"
  _ - \n
  "#+end_src\n")

(define-abbrev org-mode-abbrev-table "elsrc" "" 'skel-org-block-elisp)

(define-skeleton skel-org-block-js
  "Insert a JavaScript block"
  ""
  "#+begin_src js\n"
  _ - \n
  "#+end_src\n")

(define-abbrev org-mode-abbrev-table "jssrc" "" 'skel-org-block-js)

(define-skeleton skel-header-block
  "Creates my default header"
  ""
  "#+TITLE: " str "\n"
  "#+AUTHOR: Aaron Bedra\n"
  "#+EMAIL: aaron@aaronbedra.com\n"
  "#+OPTIONS: toc:3 num:nil\n"
  "#+STYLE: <link rel=\"stylesheet\" type=\"text/css\" href=\"http://thomasf.github.io/solarized-css/solarized-light.min.css\" />\n")

(define-abbrev org-mode-abbrev-table "sheader" "" 'skel-header-block)

(define-skeleton skel-org-html-file-name
  "Insert an HTML snippet to reference the file by name"
  ""
  "#+HTML: <strong><i>"str"</i></strong>")

(define-abbrev org-mode-abbrev-table "fname" "" 'skel-org-html-file-name)

(define-skeleton skel-ngx-config
  "Template for NGINX module config file"
  ""
  "ngx_addon_name=ngx_http_" str  "_module\n"
  "HTTP_MODULES=\"$HTTP_MODULES ngx_http_" str "_module\"\n"
  "NGX_ADDON_SRCS=\"$NGX_ADDON_SRCS $ngx_addon_dir/ngx_http_" str "_module.c\"")

(define-abbrev fundamental-mode-abbrev-table "ngxcnf" "" 'skel-ngx-config)

(define-skeleton skel-ngx-module
  "Template for NGINX modules"
  ""
  "#include <nginx.h>\n"
  "#include <ngx_config.h>\n"
  "#include <ngx_core.h>\n"
  "#include <ngx_http.h>\n\n"

  "ngx_module_t ngx_http_" str "_module;\n\n"

  "static ngx_int_t\n"
  "ngx_http_" str "_handler(ngx_http_request_t *r)\n"
  "{\n"
  >"if (r->main->internal) {\n"
  >"return NGX_DECLINED;\n"
  "}" > \n
  \n
  >"ngx_log_error(NGX_LOG_ERR, r->connection->log, 0, \"My new module\");\n\n"
  > _ \n
  >"return NGX_OK;\n"
  "}" > "\n\n"

  "static ngx_int_t\n"
  "ngx_http_"str"_init(ngx_conf_t *cf)\n"
  "{\n"
  >"ngx_http_handler_pt *h;\n"
  >"ngx_http_core_main_conf_t *cmcf;\n\n"

  >"cmcf = ngx_http_conf_get_module_main_conf(cf, ngx_http_core_module);\n"
  >"h = ngx_array_push(&cmcf->phases[NGX_HTTP_ACCESS_PHASE].handlers);\n\n"

  >"if (h == NULL) {\n"
  >"return NGX_ERROR;\n"
  "}" > \n
  \n
  >"*h = ngx_http_"str"_handler;\n\n"

  >"return NGX_OK;\n"
  "}" > \n
  \n
  "static ngx_http_module_t ngx_http_"str"_module_ctx = {\n"
  >"NULL,                 /* preconfiguration */\n"
  >"ngx_http_"str"_init,  /* postconfiguration */\n"
  >"NULL,                 /* create main configuration */\n"
  >"NULL,                 /* init main configuration */\n"
  >"NULL,                 /* create server configuration */\n"
  >"NULL,                 /* merge server configuration */\n"
  >"NULL,                 /* create location configuration */\n"
  >"NULL                  /* merge location configuration */\n"
  "};" > \n
  \n

  "ngx_module_t ngx_http_"str"_module = {\n"
  >"NGX_MODULE_V1,\n"
  >"&ngx_http_"str"_module_ctx,  /* module context */\n"
  >"NULL,                        /* module directives */\n"
  >"NGX_HTTP_MODULE,             /* module type */\n"
  >"NULL,                        /* init master */\n"
  >"NULL,                        /* init module */\n"
  >"NULL,                        /* init process */\n"
  >"NULL,                        /* init thread */\n"
  >"NULL,                        /* exit thread */\n"
  >"NULL,                        /* exit process */\n"
  >"NULL,                        /* exit master */\n"
  >"NGX_MODULE_V1_PADDING\n"
  "};" >)

(require 'cc-mode)
(define-abbrev c-mode-abbrev-table "ngxmod" "" 'skel-ngx-module)

(define-skeleton skel-ngx-append-header
  "Template for header appending function for NGINX modules"
  ""
  "static void append_header(ngx_http_request_t *r)\n"
  "{\n"
  > "ngx_table_elt_t *h;\n"
  > "h = ngx_list_push(&r->headers_out.headers);\n"
  > "h->hash = 1;\n"
  > "ngx_str_set(&h->key, \"X-NGINX-Hello\");\n"
  > "ngx_str_set(&h->value, \"Hello NGINX!\");\n"
  "}\n")

(define-abbrev c-mode-abbrev-table "ngxhdr" "" 'skel-ngx-append-header)

(setq org-ditaa-jar-path "~/.emacs.d/vendor/ditaa0_9.jar")

(setq org-plantuml-jar-path "~/.emacs.d/vendor/plantuml.jar")

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
(if (eq system-type 'darwin)
    (setq-default ispell-program-name "/usr/local/bin/aspell")
  (setq-default ispell-program-name "/usr/bin/aspell"))
(setq-default ispell-list-command "list")

(add-to-list 'auto-mode-alist '("\\.zsh$" . shell-script-mode))

(add-to-list 'auto-mode-alist '("\\.gitconfig$" . conf-mode))

(add-to-list 'auto-mode-alist '("\\.hbs$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb$" . web-mode))

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
(add-to-list 'auto-mode-alist '("Guardfile" . ruby-mode))

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
(add-hook 'markdown-mode-hook
          (lambda ()
            (visual-line-mode t)
            (writegood-mode t)
            (flyspell-mode t)))
(setq markdown-command "pandoc --smart -f markdown -t html")
(setq markdown-css-paths (expand-file-name "markdown.css" abedra/vendor-dir))

(setq idris-interpreter-path "/usr/local/bin/idris")

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

(require 'go-autocomplete)

(defun go-setup ()
  (go-eldoc-setup)
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (local-set-key (kbd "M-.") 'godef-jump))

(add-hook 'go-mode 'go-setup)

(if window-system
    (load-theme 'solarized-light t)
  (load-theme 'wombat t))

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
