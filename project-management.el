(use-package project-explorer
  :ensure t
  :init
  (setq projectile-indexing-method 'alien)
  :bind
  ("C-c C-j" . project-explorer-toggle)
  ("C-c j" . project-explorer-helm))

(use-package projectile
  :ensure t
  :init
  :config
  (setq projectile-enable-caching t)
  (setq projectile-globally-ignored-directories
        (append '("*__pycache__/" "*build/") projectile-globally-ignored-directories))
  (projectile-mode +1)
  :bind (("s-p" . projectile-command-map)
       ("C-c p" . projectile-command-map)))

(use-package helm
  :ensure t
  :demand t
  :diminish helm-mode
  :bind (("M-x" . helm-M-x)
         ("C-x r b" . helm-filtered-bookmarks)
         ("C-x C-f" . helm-projectile-find-file))
  :config
  (helm-mode 1))
