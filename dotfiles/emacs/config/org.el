(use-package pcre2el)
(use-package org-find-file
  :load-path "packages/")
(use-package org-web-tools)
(use-package org
  :preface
  (defun my/copy-to-blog-dir ()
    (interactive)
    (let* ((current-directory (file-name-directory buffer-file-name))
	       (blog-dir-root "~/ghq/github.com/horimislime/horimisli.me")
	       (raw-blog-post-id (file-name-base (directory-file-name current-directory)))
	       (blog-post-id (replace-regexp-in-string "_" "-" 
						       (replace-regexp-in-string "^[0-9]+-" "" raw-blog-post-id)))
	       (blog-post-dir (format "%s/posts/blog/%s/%s" blog-dir-root (format-time-string "%Y") blog-post-id))
;	       (blog-image-dir (format "%s/public/images" blog-dir-root))
	       (blog-content-file "content.org")
	       (blog-image-files (directory-files current-directory nil "\\(\\.png\\|\\.jpg\\|\\.jpeg\\|\\.gif\\)$")))

	  (unless (file-exists-p blog-post-dir)
	    (make-directory blog-post-dir t))
	  (copy-file (concat current-directory blog-content-file) (format "%s/%s" blog-post-dir blog-content-file) t)
	  (dolist (file blog-image-files)
	    (copy-file (concat current-directory file) (format "%s/%s" blog-post-dir file) t))
	  (message "Successfully copied entry data."))
    )
    
  (defun my/org-screenshot ()
    (interactive)
    (setq image-file-name (concat (make-temp-name (format-time-string "%Y%m%d_%H%M%S_")) ".jpg")
	  image-full-path (concat (file-name-directory buffer-file-name) image-file-name))
    (call-process "~/.homebrew/bin/pngpaste" nil nil nil image-full-path)
    (insert (format "[[./%s]]" image-file-name))
    (org-redisplay-inline-images))
  (defun my/write-to-task-file (content)
    (write-region content
		  nil "~/.config/emacs/clock-task.txt"
		  nil 'quiet))
  (defun org-set-status-to-doing ()
    (if (org-clocking-p)
	(org-todo "DOING"))
    (my/write-to-task-file (substring-no-properties org-clock-current-task)))
  (defun my/org-empty-current-task-file ()
    (my/write-to-task-file ""))
  (defun my/create-org-file-with-name ()
    (let* ((slug (read-string "slug: ")))
      (format "%s/note/%s/content.org" org-directory slug)))

  (defun my/create-web-archive ()
    (let* ((timestamp (format-time-string "%Y%m%d%H%M%S")))
      (format "%s/bookmark/%s/content.org" org-directory timestamp)))
  (defun my/fetch-page-body-as-org (url)
    "Based on org-web-tools--url-as-readable-org"
    (-let* ((url (or url (org-web-tools--get-first-url)))
            (dom (plz 'get url :as #'org-web-tools--sanitized-dom))
            ((title . readable) (org-web-tools--eww-readable dom))
            (converted (org-web-tools--html-to-org-with-pandoc readable)))
      (with-temp-buffer
	(org-mode)
	(insert converted)
	(org-web-tools--demote-headings-below 2)
	(goto-char (point-min))
	(buffer-string))))

  (defun my/get-title-from-url (url)
    (let ((title))
      (with-current-buffer (url-retrieve-synchronously url)
	(car (dom-strings (dom-by-tag (libxml-parse-html-region) 'title))))))
  (defun my/paste-url-with-title ()
    (interactive)
    (insert (format "[[%s][%s]]"
		    (current-kill 0)
		    (my/get-title-from-url (car kill-ring)))))
  (defun my/find-location-under-week-headline (type)
    "Find or create my default journal tree"
    (let* ((week-begin-date-string (if (string-equal "Sun" (format-time-string "%a"))
				      (format-time-string "%Y/%m/%d (\%a)")
				    (format-time-string "%Y/%m/%d (\%a)" (org-read-date nil t "-Sun"))))
	  (week-end-date-string (format-time-string "%Y/%m/%d (\%a)" (org-read-date nil t "Sat")))
	  (hd (format "%s - %s" week-begin-date-string week-end-date-string)))

    (goto-char (point-min))
    (unless (derived-mode-p 'org-mode)
      (error
       "Target buffer \"%s\" for jww/find-journal-tree should be in Org mode"
       (current-buffer)))
    (if (re-search-forward
	 (format org-complex-heading-regexp-format (regexp-quote hd))
	 nil t)
	(goto-char (point-at-bol))
      (progn (org-insert-heading)
	     (insert hd)))

    (org-narrow-to-subtree)
    (if (re-search-forward
	 (format org-complex-heading-regexp-format (regexp-quote type))
	 nil t)
	(goto-char (point-at-bol))
      (or (bolp) (insert "\n"))
      (org-end-of-subtree)
      (insert "\n** " type "\n")
      (beginning-of-line 0))
    (widen)
    ))
  (defun my/find-k-under-headline ()
    (my/find-location-under-week-headline "KEEP"))
  (defun my/find-p-under-headline ()
    (my/find-location-under-week-headline "PROBLEM"))
  (defun my/org-update-statistics-cookies-after-refile ()
    (run-at-time "0.01 sec" nil (lambda () (org-update-statistics-cookies 'all))))

  (defun my/blog-draft-mode-hook ()
    "Hook function for blog-draft-mode to auto-copy on save."
    (my/copy-to-blog-dir))

  (define-minor-mode blog-draft-mode
    "Minor mode for blog draft files that auto-copies to blog directory on save."
    :lighter " BlogDraft"
    :group 'org
    (if blog-draft-mode
        (add-hook 'after-save-hook #'my/blog-draft-mode-hook nil t)
      (remove-hook 'after-save-hook #'my/blog-draft-mode-hook t)))

  :bind
  (("C-c c" . org-capture)
   ("C-c j" . org-journal-new-entry)
   ("C-c o f b" . (lambda () (interactive) (org-find-file (format "%s/bookmark" org-directory))))
   ("C-c o f n" . (lambda () (interactive) (org-find-file (format "%s/note" org-directory))))
   :map org-mode-map
   ("C-c C-p C-v" . my/org-screenshot)
   ("C-c C-u C-v" . my/paste-url-with-title)
   ("C-c o c" . my/copy-to-blog-dir)
   ("C-c o d" . blog-draft-mode))

  :hook
  ((org-mode . visual-line-mode)
   (org-clock-in . org-set-status-to-doing)
   (org-clock-out . my/org-empty-current-task-file)
   (org-after-refile-insert . my/org-update-statistics-cookies-after-refile))

  :init
  (require 'org-protocol)
  (setq org-directory "~/Dropbox/org"
	org-backlog-file (format "%s/inbox.org" org-directory)
	org-bookmark-file (format "%s/bookmark.org" org-directory)
	org-daily-tasks-file (format "%s/note/tasks/content.org" org-directory)
	org-kpt-file (format "%s/note/kpt/content.org" org-directory)
	org-a-file (format "%s/note/a-log/content.org" org-directory)
	elfeed-source-csv (format "%s/assets/elfeed-source.csv" org-directory))

  :custom
  (org-image-actual-width 900)
  (org-clock-idle-time 60)
  (org-startup-folded 'content)
  (org-startup-with-inline-images t)
  (org-use-speed-commands t)
  (org-todo-keywords '((sequence "TODO" "DOING"  "|" "DONE")))
  (org-capture-templates
   '(("d" "Weekdays TODO" entry (file org-daily-tasks-file) "%[~/GoogleDrive/org/assets/weekdays-todo.org]" :prepend t)
     ("w" "Weekends TODO" entry (file org-daily-tasks-file) "%[~/GoogleDrive/org/assets/weekends-todo.org]" :prepend t)
     ("n" "Create Note" plain (file my/create-org-file-with-name) "%[~/GoogleDrive/org/assets/note.org]")
     ("t" "Put work task into inbox" entry (file+headline org-backlog-file "Work") "* TODO %?\n" :prepend t)
     ("h" "Put private task into inbox" entry (file+headline org-backlog-file "Private") "* TODO %?\n" :prepend t)
     ("b" "Bookmark" plain (file my/create-web-archive) "%[~/GoogleDrive/org/assets/bookmark.org]")
     ("k" "Keep" entry (file+function org-kpt-file my/find-k-under-headline) "*** %?\n")
     ("p" "Problem" entry (file+function org-kpt-file my/find-p-under-headline) "*** %?\n")
     ("f" "Subscribe Feed" plain (file elfeed-source-csv) "%(my/get-title-from-url \"%:link\"),%:link\n" :prepend t :immediate-finish t)
     )
   ))

(use-package org-pomodoro
  :custom
  (org-pomodoro-play-sounds nil))

(use-package org-journal
  :custom
  (org-journal-dir (concat org-directory  "/journal"))
  (org-journal-file-format "%Y%m%d/journal.org")
  (org-journal-date-format "%Y/%m/%d (%a)"))

(use-package org-modern
  :custom
  (org-modern-progress '("○" "◔" "◑" "◕" "✅"))
  :hook
  ((org-mode . org-modern-mode)
  (org-agenda-finalize . org-modern-agenda)))

(use-package org-roam
  :preface
  (require 'org-roam-protocol)
  (defun my/org-open-at-point-same-buffer ()
    "Configure org links to always open in the same buffer."
    (interactive)
    (let ((org-link-frame-setup '((file . find-file))))
      (org-open-at-point)))
  :custom
  (org-roam-directory (file-truename (format "%s/roam" org-directory)))
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :target (file+head "${slug}/content.org" "#+title: ${title}\n")
      :unnarrowed t)))
  (org-roam-capture-ref-templates
   '(("r" "ref" plain "%?"
      :target (file+head
	       "b-%<%Y%m%d-%H%M>/content.org"
	       "#+title: ${title}\n#+date: %U\n#+filetags: :bookmark:\n\n %(my/fetch-page-body-as-org \"${ref}\")\n")
      :unnarrowed t)))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
	 ("C-c n o" . my/org-open-at-point-same-buffer)
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag))) 
  (org-roam-db-autosync-mode))

;; org-babel

(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)))

(defun my-org-confirm-babel-evaluate (lang body)
  "Show prompt if some keywords exist in command line"
  (or 
   (string-match-p "rm -rf" body)
   (string-match-p "apply" body)
   (string-match-p "delete" body)))
(setq org-confirm-babel-evaluate #'my-org-confirm-babel-evaluate)

(defun my/org-remove-all-results ()
  "Remove all #+RESULTS: block within current buffer"
  (interactive)
  (org-babel-map-src-blocks nil
    (org-babel-remove-result)))

