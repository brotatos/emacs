(use-package evil
  :ensure t
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
  (add-hook 'git-commit-mode-hook 'evil-normal-state)
  (evil-mode)
  :bind (:map evil-normal-state-map ("M-." . nil)))

(use-package evil-leader
  :ensure t
  :after evil-magit
  :config
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key "gs" 'magit-status)
  (evil-leader/set-key "gb" 'magit-blame)
  (evil-leader/set-key "t" 'air-org-set-tags)
  (global-evil-leader-mode))

(use-package evil-org
  :ensure t
  :hook (org-mode-hook evil-org-mode))

(use-package evil-magit
  :ensure t
  :after evil)
