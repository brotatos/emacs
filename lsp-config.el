(use-package hydra :ensure t)

(use-package flycheck
  :ensure t
  :diminish flycheck-mode)

(use-package which-key
  :ensure t
  :init (which-key-mode)
  :diminish (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3))

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :bind (("M-r" . lsp-find-references))
  :hook ((c-mode . lsp-deferred)
         (c-mode-common . lsp-deferred)
         (c++-mode . lsp-deferred)
         (python-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :config
  (setq gc-cons-threshold (* 100 1024 1024)
        read-process-output-max (* 1024 1024)
        lsp-idle-delay 0.1)  ;; clangd is fast
  (lsp-enable-which-key-integration t)
  :commands (lsp lsp-defferred))

(use-package lsp-treemacs
  :ensure t
  :config
  (setq treemacs-space-between-root-nodes nil)
  :commands lsp-treemacs-errors-list)

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-flycheck-enable t
        lsp-ui-doc-enable t
        lsp-ui-doc-position 'top
        lsp-ui-doc-show-with-cursor t
        lsp-ui-doc-show-with-mouse t
        )
  :commands lsp-ui-mode)

(use-package yasnippet
  :ensure t
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))
