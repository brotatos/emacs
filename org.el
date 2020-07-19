(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook 'org-bullets-mode))

(use-package ox-twbs :ensure t)

(use-package org-journal
  :ensure t
  :defer t
  :config
  (setq org-journal-dir "~/noir/journal/log"
        org-journal-file-format "%Y-%m-%d.org"
        org-journal-date-format "%A, %d %B %Y"
        org-journal-file-type 'daily
        org-journal-enable-cache t))

(with-eval-after-load 'org
  (setq org-agenda-files (directory-files-recursively "~/noir" ".*\.org"))

  (setq auto-indent-start-org-indent t
        auto-indent-fix-org-return t
        auto-indent-delete-backward-char t
        auto-indent-fix-org-move-beginning-of-line t
        auto-indent-fix-org-yank t)

  ;; Mobile Org
  ;; Location of org files on your local system
  (setq org-directory "~/noir")
  ;; Where new notes will be stored
  (setq org-mobile-inbox-for-pull "~/noir/flagged.org")
  (setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")

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
           :publishing-function org-twbs-publish-to-html
           )
          ("noir-static"
           :base-directory "~/noir/static/"
           :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
           :publishing-directory "~/Dropbox/apps/noir/static/"
           :recursive t
           :publishing-function org-publish-attachment
           )))

  ;; https://blog.aaronbieber.com/2016/03/05/playing-tag-in-org-mode.html#comment-4976528210
  (defun air--org-swap-tags (tags)
    "Replace any tags on the current headline with TAGS.

    The assumption is that TAGS will be a string conforming to Org Mode's
    tag format specifications, or nil to remove all tags."
    (let ((old-tags (org-get-tags-string))
          (tags (if tags
                    (concat " " tags)
                  "")))
      (save-excursion
        (beginning-of-line)
        (re-search-forward
         (concat "[ \t]*" (regexp-quote old-tags) "[ \t]*$")
         (line-end-position) t)
        (replace-match tags)
        (org-set-tags t))))

  (defun air-org-set-tags (tag)
    "Add TAG if it is not in the list of tags, remove it otherwise.

    TAG is chosen interactively from the global tags completion table."
    (interactive
     (list (let ((org-last-tags-completion-table
                  (if (derived-mode-p 'org-mode)
                      (org-uniquify
                       (delq nil (append (org-get-buffer-tags)
                                         (org-global-tags-completion-table))))
                    (org-global-tags-completion-table))))
             (org-icompleting-read
              "Tag: " 'org-tags-completion-function nil nil nil
              'org-tags-history))))
    (let* ((cur-list (org-get-tags))
           (new-tags (mapconcat 'identity
                                (if (member tag cur-list)
                                    (delete tag cur-list)
                                  (append cur-list (list tag)))
                                ":"))
           (new (if (> (length new-tags) 1) (concat " :" new-tags ":")
                  nil)))
      (air--org-swap-tags new)))
  )
