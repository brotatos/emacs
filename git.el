(use-package git-messenger
  :ensure t
  :bind
  ("C-c M" . git-messenger:popup-message)
  :config
  (setq git-messenger:show-detail t)
  (setq git-messenger:use-magit-popup t))


(use-package diff-hl
  :ensure t
  :demand
  :config
  (global-diff-hl-mode)
  :hook
  (dired-mode-hook diff-hl-dired-mode))
