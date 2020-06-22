;; hide welcome screen
(setq inhibit-splash-screen t)

;; Don't show top menu bar
(menu-bar-mode -1)

;; Display time in the modeline.
(display-time-mode)

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

(use-package highlight-indent-guides
  :ensure t
  :hook ((prog-mode . highlight-indent-guides-mode))
  :config
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-character ?\ǀ)
  (setq highlight-indent-guides-responsive 'top)
  ;; https://github.com/DarthFennec/highlight-indent-guides/issues/40#issuecomment-451553492
  (defadvice ivy-cleanup-string (after my-ivy-cleanup-hig activate)
    (let ((pos 0) (next 0) (limit (length str)) (prop 'highlight-indent-guides-prop))
      (while (and pos next)
        (setq next (text-property-not-all pos limit prop nil str))
        (when next
          (setq pos (text-property-any next limit prop nil str))
          (remove-text-properties next pos '(display nil face nil) str)))))
   :init
   (highlight-indent-guides-mode)
   (highlight-indent-guides-auto-set-faces))

(use-package hl-todo
  :ensure t
  :init
  (global-hl-todo-mode))

;; Highlight the current line the cursor is on
(use-package hl-line
  :ensure t
  :config
  (set-face-background hl-line-face "gray13")
  (global-hl-line-mode +1))

(use-package mode-icons
  :ensure t
  :config
  (mode-icons-mode))

(use-package paren
  :ensure t
  :init
  (setq show-paren-delay 0.005)
  :config
  (show-paren-mode +1))

(use-package rainbow-delimiters
  :ensure t
  :hook ((prog-mode . rainbow-delimiters-mode)
         (text-mode . rainbow-delimiters-mode)))

(use-package whitespace
  :ensure t
  :init
  (setq prelude-whitespace nil)
  (setq whitespace-line-column 80)
  ;; Highlight trailing whitespace
  (setq whitespace-style '(face trailing lines-tail empty indentation::space tab-mark))
  :config
  (global-whitespace-mode t))

(use-package jbeans-theme
  :ensure t
  :config
  (load-theme 'jbeans t))

;; M-x display-ansi-colors to explicitly decode ANSI color escape sequences
(defun display-ansi-colors ()
  (interactive)
  (format-decode-buffer 'ansi-colors))

;; Decode ANSI color escape sequences for .log files
(add-to-list 'auto-mode-alist '("\\.log\\'" . display-ansi-colors))
