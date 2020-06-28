(defun my-frame-config (&optional frame)
  (with-selected-frame (or frame (selected-frame))
    ;; hide toolbar
    (tool-bar-mode -1)
    ;; hide scrollbar
    (toggle-scroll-bar -1)
    ;; hide menu bar
    (menu-bar-mode -1)
  )
)

(add-hook 'after-make-frame-functions 'my-frame-config)
(add-hook 'after-init-hook 'my-frame-config)

;; Don't use ls --dired on Mac OS X.
(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil))
