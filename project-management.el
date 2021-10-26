(use-package ggtags
  :ensure t
  :after evil
  :config
  (setq ggtags-update-on-save t)
  (global-set-key (kbd "C-c ??") 'ggtags-show-definition)
  (ggtags-mode 1)
  (evil-make-overriding-map ggtags-mode-map 'normal)
  (add-hook 'ggtags-mode-hook #'evil-normalize-keymaps)
  (evil-define-key 'normal ggtags-mode-map
    (kbd "C-]") 'ggtags-find-tag-dwim)
    (kbd "C-t") 'ggtags-prev-mark)

(use-package neotree
  :ensure t
  :init
  (setq neo-smart-open t
        neo-modern-sidebar t)
  :config
  (setq neo-theme 'arrow))

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
