; Show keystrokes as they are typed in
(setq echo-keystrokes .1)

;; Make X primary selection work with emacs.
(setq x-select-enable-clipboard t)

;; Use separate directory for backups.
(setq backup-directory-alist
      `(("." . ,(expand-file-name "~/.emacs.d/backups/"))))
(setq auto-save-file-name-transforms
      `((".*" , (expand-file-name "~/.emacs.d/backups/" t))))

;; Always follow symlinks
(setq vc-follow-symlinks t)

;; Delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Line break for text modes.
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Line break for comments in source code.
(defun comment-auto-fill()
  (setq-local comment-auto-fill-only-comments t)
  (auto-fill-mode 1))

(add-hook 'prog-mode-hook 'comment-auto-fill)

(global-set-key (kbd "C-c l") 'hs-show-block)
(global-set-key (kbd "C-c h") 'hs-hide-block)
(global-set-key (kbd "C-c a") 'hs-hide-all)
(global-set-key (kbd "C-c s") 'hs-show-all)
(add-hook 'prog-mode-hook 'hs-minor-mode)

(use-package diminish :ensure t)

(use-package bind-key :ensure t)
