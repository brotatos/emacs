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
 'hl-todo
 'exec-path-from-shell
 'project-explorer
 'doom-modeline
 'elpy
 'smart-tab
 'markdown-mode
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
 'fill-column-indicator
 'whitespace
 'git-gutter+
 'yaml-mode
 'ido
 'projectile
 'jbeans-theme
 'evil-vimish-fold
 'vimish-fold
 'evil
 'magit)

(use-package hl-todo
    :init
    (global-hl-todo-mode))

(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize))

(use-package project-explorer
  :ensure t
  :bind
  ("C-c C-j" . project-explorer-toggle)
  ("C-c j" . project-explorer-helm))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

(use-package elpy
  :ensure t
  :init
  (elpy-enable))

(use-package smart-tab
  :ensure t
  :defer t
  :init
  (setq-default indent-tabs-mode nil)
  :config
  (global-smart-tab-mode 1))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown")
  (setq markdown-command "/usr/local/bin/pandoc"))

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
  (setq fci-rule-color "blue")
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

  (setq evil-emacs-state-cursor '("red" box))
  (setq evil-normal-state-cursor '("white" box))
  (setq evil-visual-state-cursor '("orange" box))
  (setq evil-insert-state-cursor '("white" bar))
  (setq evil-replace-state-cursor '("red" bar))
  (setq evil-operator-state-cursor '("red" hollow))

  :config
  (evil-mode)
  :bind (:map evil-normal-state-map ("M-." . nil)))

;; For whatever reason, :hook causes insert to trigger globally.
;; Use evil's insert state for git commit messages.
(add-hook 'git-commit-mode-hook 'evil-normal-state)

(use-package vimish-fold
  :config
  (vimish-fold-global-mode 1))

(use-package evil-vimish-fold
  :config
  (evil-vimish-fold-mode 1))

(use-package git-gutter+
  :config
  (global-git-gutter+-mode))

(use-package jbeans-theme
  :config
  (load-theme 'jbeans t))

(use-package paren
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
      `(("." . ,(expand-file-name "~/.emacs-backups/"))))
(setq auto-save-file-name-transforms
      `((".*" , (expand-file-name "~/.emacs-backups/"))))

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

;; Line break for text modes.
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Line break for comments in source code.
(defun comment-auto-fill()
  (setq-local comment-auto-fill-only-comments t)
  (auto-fill-mode 1))

(add-hook 'prog-mode-hook 'comment-auto-fill)

;; M-x display-ansi-colors to explicitly decode ANSI color escape sequences
(defun display-ansi-colors ()
  (interactive)
  (format-decode-buffer 'ansi-colors))

;; Decode ANSI color escape sequences for .log files
(add-to-list 'auto-mode-alist '("\\.log\\'" . display-ansi-colors))

(setq org-log-done 'time)
;; hide welcome screen
(setq inhibit-splash-screen t)

(add-hook 'prog-mode-hook #'hs-minor-mode)
(global-set-key (kbd "C-c l") 'hs-show-block)
(global-set-key (kbd "C-c h") 'hs-hide-block)


(defun my-frame-config (&optional frame)
  (with-selected-frame (or frame (selected-frame))
    (set-default-font "Menlo-14")
    ;; hide toolbar
    (tool-bar-mode -1)
    ;; hide scrollbar
    (toggle-scroll-bar -1)
    ;; hide menu bar
    (menu-bar-mode -1)
    (set-frame-parameter (selected-frame) 'alpha '(95 . 95))
  )
)

(add-hook 'after-make-frame-functions 'my-frame-config)
(add-hook 'after-init-hook 'my-frame-config)

(global-visual-line-mode 1)
(toggle-word-wrap)
