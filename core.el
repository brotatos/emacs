;; Show (row, col)
(setq column-number-mode t)

;; Use separate directory for backups.
(setq backup-directory-alist
      `(("." . ,(expand-file-name "~/.emacs.d/backups/"))))
(setq auto-save-file-name-transforms
      `((".*" , (expand-file-name "~/.emacs.d/backups/" t))))


;; 2 spaces is one tab
(setq-default tab-width 2)
;; Insert spaces instead of tabs
(setq-default indent-tabs-mode nil)
;; Insert-tab on newline
(setq indent-line-function 'insert-tab)

;; Always follow symlinks
(setq vc-follow-symlinks t)

;; Line numbers
(global-linum-mode t)

;; Syntax highlighting
(global-font-lock-mode t)

;; Word wrap
(global-visual-line-mode t)

;; Delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Line break for text modes.
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Line break for comments in source code.
(defun comment-auto-fill()
  (setq-local comment-auto-fill-only-comments t)
  (auto-fill-mode 1))

(add-hook 'prog-mode-hook 'comment-auto-fill)

(global-set-key (kbd "C-c l") 'hs-show-block)
(global-set-key (kbd "C-c h") 'hs-hide-block)
(global-set-key (kbd "C-c a") 'hs-hide-all)
(global-set-key (kbd "C-c s") 'hs-show-all)
(add-hook 'prog-mode-hook 'hs-minor-mode)

(use-package diminish :ensure t)

(use-package bind-key :ensure t)

(use-package ivy
  :defer t
  :ensure t
  :diminish
  :bind
  (("C-c C-r" . ivy-resume)
  ("C-x b"   . ivy-switch-buffer)
  ("C-x B"   . ivy-switch-buffer-other-window))
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-display-style 'fancy)
  (ivy-use-virtual-buffers t)
  :config
  (define-key ivy-minibuffer-map (kbd "RET") 'ivy-alt-done)
  (setq projectile-completion-system 'ivy)
  (setq ivy-on-del-error-function #'ignore)
  (ivy-mode))

(use-package smart-tab
  :ensure t
  :defer t
  :init
  (setq-default indent-tabs-mode nil)
  :config
  (global-smart-tab-mode 1))
