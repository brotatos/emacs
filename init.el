(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
(load "~/.emacs.d/helpers.el")

(setq package-enable-at-startup nil)
(package-initialize)

;; Make sure to have downloaded archive description.
(or (file-exists-p package-user-dir)
  (package-refresh-contents))

(ensure-package-installed
  'telephone-line
  'company
  'dumb-jump
  'company-jedi
  'jedi
  'use-package
  'helm
  'helm-projectile
  'helm-gtags
  'eclim
  'fish-mode
  'auto-complete
  'ranger
  'ansi-color
  'highlight-indent-guides
  'fill-column-indicator
  'whitespace
  'git-gutter+
  'yaml-mode
  'ido
  'projectile
  'jbeans-theme
  'evil
  'magit)

(use-package telephone-line
  :config
  (telephone-line-mode))

(use-package company
  :ensure t
  :defer t
  :init (global-company-mode)
  :config
  (progn
    ;; Use Company for completion
    (bind-key [remap completion-at-point] #'company-complete company-mode-map)
    (setq company-tooltip-align-annotations t
          ;; Easy navigation to candidates with M-<n>
          company-show-numbers t)
    (setq company-dabbrev-downcase nil)))

(use-package dumb-jump
    :config
    (dumb-jump-mode))

(global-set-key (kbd "C-c g") 'dumb-jump-go)

(use-package jedi
  :ensure t
  :init
  :config
  (use-package company-jedi
  :ensure t
  :init
  (add-hook 'python-mode-hook
            (lambda () (add-to-list 'company-backends 'company-jedi)))
  (setq company-jedi-python-bin "python")))

(use-package fill-column-indicator
  :init
  (setq fci-rule-width 80)
  (setq fci-rule-color "red")
  :hook ((after-change-major-mode . fci-mode)))

(use-package eclim
  :init
  (setq eclimd-autostart t)
  :config
  (global-eclim-mode))

(use-package projectile
  :config
  (projectile-mode +1)
  :bind (("s-p" . projectile-command-map)
         ("C-c p" . projectile-command-map)))

(use-package helm
  :bind (("M-x" . helm-M-x)
         ("C-x r b" . helm-filtered-bookmarks)
         ("C-x C-f" . helm-projectile-find-file))
  :config
  (helm-mode 1))

(use-package helm-gtags
  :init
  (setq helm-gtags-ignore-case t)
  (setq helm-gtags-auto-update t)
  (setq helm-gtags-use-input-at-curose t)
  (setq helm-gtags-opulse-at-cursor t)
  (setq helm-gtags-prefix-key "\C-cg")
  (setq helm-gtags-suggested-key-mapping t)
  :config
  (helm-gtags-mode))

(use-package evil
  :init
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-shift-round nil)
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode)
  :bind (:map evil-normal-state-map
         ("M-." . nil)))

;; For whatever reason, :hook causes insert to trigger globally.
;; Use evil's insert state for git commit messages.
(add-hook 'git-commit-mode-hook 'evil-insert-state)

(use-package git-gutter+
  :config
  (global-git-gutter+-mode))

(use-package jbeans-theme
  :config
  (load-theme 'jbeans t))

(use-package highlight-indent-guides
  :init
  (setq highlight-indentation-mode t)
  (setq highlight-indent-guides-method 'character)
  :config
  (highlight-indent-guides-mode 1)
  :hook ((prog-mode . highlight-indent-guides-mode)
         (text-mode . highlight-indent-guides-mode)))

(use-package paren
    :init
    (setq show-paren-delay 0)
    :config
    (show-paren-mode +1))

;; Highlight the current line the cursor is on
(use-package hl-line
  :init
  (set-face-background hl-line-face "gray13")
  :config
  (global-hl-line-mode +1))

(use-package rainbow-delimiters
  :ensure t
  :hook ((prog-mode . rainbow-delimiters-mode)
         (text-mode . rainbow-delimiters-mode)))

(use-package whitespace
    :init
    (setq prelude-whitespace nil)
    ;; Highlight trailing whitespace
    (setq whitespace-style '(face empty tabs trailing))
    :config
    (global-whitespace-mode t))

;; Line numbers
(global-linum-mode t)

;; Show (row, col)
(setq column-number-mode t)

;; Use separate directory for backups.
(setq backup-directory-alist
      `(("." . ,(expand-file-name "~/.emacs-backups"))))
(setq auto-save-file-name-transforms
      `((".*" , (expand-file-name "~/.emacs-backups"))))

;; 4 spaces is one tab
(setq-default tab-width 4)
;; Insert spaces instead of tabs
(setq-default indent-tabs-mode nil)
;; Insert-tab on newline
(setq indent-line-function 'insert-tab)

;; Always follow symlinks
(setq vc-follow-symlinks t)

;; Syntax highlighting
(global-font-lock-mode t)

;; Word wrap
(global-visual-line-mode t)

;; Delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Don't show top menu bar
(menu-bar-mode -1)

;; Use java style indentation for c/c++
(defun foouser-c++-indent-setup ()
  (c-set-style "java")
  (c-set-offset (quote innamespace) 0 nil))

(add-hook 'c++-mode-hook 'foouser-c++-indent-setup)
