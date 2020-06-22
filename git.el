(use-package git-messenger
  :ensure t
  :bind
  ("C-c M" . git-messenger:popup-message)
  :config
  (setq git-messenger:show-detail t)
  (setq git-messenger:use-magit-popup t))

;; For whatever reason, :hook causes insert to trigger globally.
;; Use evil's insert state for git commit messages.
(add-hook 'git-commit-mode-hook 'evil-normal-state)

(use-package diff-hl
  :ensure t
  :demand
  :config
  (diff-hl-margin-mode)
  (global-diff-hl-mode)
  :hook
  (dired-mode-hook diff-hl-dired-mode))
