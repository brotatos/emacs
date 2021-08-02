(use-package neotree
  :ensure t
  :init
  (setq neo-smart-open t
        neo-modern-sidebar t))

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
  :bind ("M-x" . helm-M-x)
  :config
  (helm-mode 1))
