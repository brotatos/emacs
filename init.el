(require 'package)
(package-initialize)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(when (not package-archive-contents)
  (package-refresh-contents))

(eval-when-compile (require 'use-package))

(load (expand-file-name "c.el" user-emacs-directory))
(load (expand-file-name "core.el" user-emacs-directory))
(load (expand-file-name "completion.el" user-emacs-directory))
(load (expand-file-name "editor.el" user-emacs-directory))
(load (expand-file-name "evil.el" user-emacs-directory))
(load (expand-file-name "git.el" user-emacs-directory))
(load (expand-file-name "java.el" user-emacs-directory))
(load (expand-file-name "mac.el" user-emacs-directory))
(load (expand-file-name "markdown.el" user-emacs-directory))
(load (expand-file-name "org.el" user-emacs-directory))
(load (expand-file-name "project-management.el" user-emacs-directory))
(load (expand-file-name "shell.el" user-emacs-directory))


(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)
