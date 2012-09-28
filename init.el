(copy-file
 (concat user-emacs-directory "init.el")
 (concat user-emacs-directory "init.el.base"))

(require 'org-install)
(require 'ob-tangle)
(org-babel-load-file (expand-file-name "init.org" user-emacs-directory))
