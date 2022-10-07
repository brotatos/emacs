;;te Show (row, col)
(setq column-number-mode t)

;; 2 spaces is one tab
(setq-default tab-width 2)

;; Insert spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; hide welcome screen
(setq inhibit-splash-screen t)

;; Line numbers
(global-linum-mode t)

;; Don't show top menu bar
(menu-bar-mode -1)

;; Display time in the modeline.
(display-time-mode)

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

(use-package hl-todo
  :ensure t
  :init
  (global-hl-todo-mode))

;; Highlight the current line the cursor is on
(use-package hl-line
  :ensure t
  :config
  (global-hl-line-mode +1))

(use-package mode-icons
  :ensure t
  :config (mode-icons-mode))

(use-package paren
  :ensure t
  :init
  (setq show-paren-delay 0.005)
  :config
  (show-paren-mode +1))

(use-package rainbow-delimiters
  :ensure t
  :hook ((prog-mode . rainbow-delimiters-mode)
         (text-mode . rainbow-delimiters-mode)))

(use-package tree-sitter-langs :ensure t)

(use-package tree-sitter
  :ensure t
  :after tree-sitter-langs
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package whitespace
  :init
  (setq prelude-whitespace nil)
  (setq whitespace-line-column 100)
  ;; Highlight trailing whitespace
  (setq whitespace-style
        ;; There's a bug in empty where the face doesn't go away after
        ;; typing below the newline.
        '(face trailing lines-tail indentation::space tab-mark)) ;empty))
  :config
  (global-whitespace-mode 1))

(use-package jbeans-theme
  :ensure t
  :config
  (load-theme 'jbeans t))

;; M-x display-ansi-colors to explicitly decode ANSI color escape sequences
(defun display-ansi-colors ()
  (interactive)
  (format-decode-buffer 'ansi-colors))

;; Decode ANSI color escape sequences for .log files
(add-to-list 'auto-mode-alist '("\\.log\\'" . display-ansi-colors))

(set-face-attribute 'default nil :height 70)
