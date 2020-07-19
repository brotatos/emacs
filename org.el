(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook 'org-bullets-mode))

(setq org-log-done 'time)
;; https://orgmode.org/manual/HTML-preamble-and-postamble.html
;; disable author + date + validate link at end of HTML exports
(setq org-html-postamble nil)

(setq org-publish-project-alist
      '(("noir"
         :base-directory "~/noir/"
         :publishing-directory "~/Dropbox/apps/noir"
         :section-numbers nil
         :table-of-contents nil
         :recursive t
         :publishing-function org-html-publish-to-html
         )
        ("noir-static"
         :base-directory "~/noir/static/"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :publishing-directory "~/Dropbox/apps/noir/static/"
         :recursive t
         :publishing-function org-publish-attachment
         )))

(with-eval-after-load 'org

  (setq auto-indent-start-org-indent t
        auto-indent-fix-org-return t
        auto-indent-delete-backward-char t
        auto-indent-fix-org-move-beginning-of-line t
        auto-indent-fix-org-yank t)
  )
