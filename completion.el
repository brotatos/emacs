(use-package company
  :ensure t
  :defer t
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind
  (:map company-active-map ("<tab>" . company-complete-selection))
  (:map lsp-mode-map ("<tab>" . company-indent-or-complete-common))
  :config
  (progn
    ;; Use Company for completion
    ;(bind-key [remap completion-at-point] #'company-complete company-mode-map)
    (setq company-tooltip-align-annotations t
          company-idle-delay 0.0
          company-minimum-prefix-length 1
          company-lsp-cache-candidates nil
          company-lsp-async t
          company-transformers nil
          ;; Easy navigation to candidates with M-<n>
          company-show-numbers t
          company-dabbrev-downcase nil))
  (global-company-mode))
