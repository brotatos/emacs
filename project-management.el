(use-package ggtags
  :diminish ggtags-mode
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

(use-package treemacs
  :ensure t
  :defer t
  :config
  (setq treemacs-indentation 2
        treemacs-eldoc-display t
        treemacs-follow-after-init t
        treemacs-expand-after-init t
        treemacs-filewatch-mode t
        treemacs-git-integration t
        treemacs-git-mode 'extended
        treemacs-silent-refresh t
        treemacs-sorting 'alphabetic-asc
        treemacs-position 'left
        treemacs-show-hidden-files t
        treemacs-tag-follow-cleanup t
        treemacs-tag-follow-delay 1.0)
  (treemacs-fringe-indicator-mode 'always)
  (treemacs-follow-mode t)
  (treemacs-project-follow-mode t))


(use-package treemacs-evil
  :after treemacs evil
  :ensure t)

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

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

(use-package helm-xref
  :ensure t
  :demand t
  :config
  (define-key global-map [remap find-file] #'helm-find-files)
  (define-key global-map [remap execute-extended-command] #'helm-M-x)
  (define-key global-map [remap switch-to-buffer] #'helm-mini))

(use-package helm-lsp
  :ensure t
  :demand t
  :commands helm-lsp-workspace-symbol)

(use-package helm
  :ensure t
  :demand t
  :diminish helm-mode
  :bind ("M-x" . helm-M-x)
  :config
  (helm-mode 1))
