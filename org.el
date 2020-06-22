(use-package org-bullets
  :ensure t
  :diminish t
  :hook (org-mode-hook . (lambda () (org-bullets-mode 1))))

(setq org-log-done 'time)
