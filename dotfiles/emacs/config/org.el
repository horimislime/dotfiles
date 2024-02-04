(use-package pcre2el)
(use-package org-find-file
  :load-path "packages/")

(use-package org
  :preface
  (defun my/copy-to-blog-dir ()
    (interactive)
    (if (yes-or-no-p "Copy entry to repo?")
	(let* ((current-directory (file-name-directory buffer-file-name))
	       (blog-dir-root "~/ghq/github.com/horimislime/horimisli.me")
	       (blog-post-id (file-name-base (directory-file-name current-directory)))
	       (blog-post-dir (format "%s/posts/blog/%s/%s" blog-dir-root (format-time-string "%Y") blog-post-id))
	       (blog-image-dir (format "%s/public/images" blog-dir-root))
	       (blog-content-file "content.org")
	       (blog-image-files (directory-files current-directory nil "\\(\\.png\\|\\.jpg\\|\\.jpeg\\|\\.gif\\)$")))

	  (unless (file-exists-p blog-post-dir)
	    (make-directory blog-post-dir t))
	  (copy-file (concat current-directory blog-content-file) (format "%s/%s" blog-post-dir blog-content-file) t)
	  (dolist (file blog-image-files)
	    (copy-file (concat current-directory file) (format "%s/%s" blog-image-dir file) t))
	  (message "Successfully copied entry data."))
      (message "Cancelled")))
    
  (defun my/org-screenshot ()
    (interactive)
    (setq image-file-name (concat (make-temp-name (format-time-string "%Y%m%d_%H%M%S_")) ".png")
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
    (let ((week-begin-date-string (if (string-equal "Sun" (format-time-string "%a"))
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
	(progn (goto-char (point-at-bol))
	       (org-end-of-subtree))
      (progn (insert "* " hd "\n")
	     (outline-previous-visible-heading 1))
      )
    (if (re-search-backward
	 (format org-complex-heading-regexp-format (regexp-quote type))
	 nil t)
	(goto-char (point-at-bol))
      (or (bolp) (insert "\n"))
      (org-end-of-subtree)
      (insert "\n** " type "\n")
      (beginning-of-line 0))
    ))
  (defun my/find-k-under-headline ()
    (my/find-location-under-week-headline "KEEP"))
  (defun my/find-p-under-headline ()
    (my/find-location-under-week-headline "PROBLEM"))

  :bind
  (("C-c c" . org-capture)
   ("C-c j" . org-journal-new-entry)
   ("C-c o f" . org-find-file)
   :map org-mode-map
   ("C-c C-p C-v" . my/org-screenshot)
   ("C-c C-u C-v" . my/paste-url-with-title)
   ("C-c o c" . my/copy-to-blog-dir))

  :hook
  ((org-mode . visual-line-mode)
   (org-clock-in . org-set-status-to-doing)
   (org-clock-out . my/org-empty-current-task-file))

  :init
  (require 'org-protocol)
  (setq org-directory "~/Dropbox/org"
	org-backlog-file (format "%s/inbox.org" org-directory)
	org-bookmark-file (format "%s/bookmark.org" org-directory)
	org-daily-tasks-file (format "%s/tasks.org" org-directory)
	org-kpt-file (format "%s/kpt.org" org-directory)
	elfeed-source-csv "~/Dropbox/emacs/elfeed-source.csv")

  :custom
  (org-image-actual-width 900)
  (org-clock-idle-time 60)
  (org-startup-folded 'content)
  (org-startup-with-inline-images t)
  (org-use-speed-commands t)
  (org-todo-keywords '((type "TODO" "WAITING" "DOING"  "|" "DONE")))
  (org-capture-templates
   '(("d" "Weekdays TODO" entry (file org-daily-tasks-file) "%[~/.config/emacs/assets/org-templates/weekdays-todo.org]" :prepend t)
     ("w" "Weekends TODO" entry (file org-daily-tasks-file) "%[~/.config/emacs/assets/org-templates/weekends-todo.org]" :prepend t)
     ("n" "Create Note" plain (file my/create-org-file-with-name) "%[~/.config/emacs/assets/org-templates/note.org]")
     ("t" "Put work task into inbox" entry (file+headline org-backlog-file "Work") "* TODO %?\n" :prepend t)
     ("h" "Put private task into inbox" entry (file+headline org-backlog-file "Private") "* TODO %?\n" :prepend t)
     ("b" "Bookmark" entry (file+headline org-bookmark-file "Bookmarks") "%[~/.config/emacs/assets/org-templates/bookmark.org]" :prepend t)
     ("r" "Read Later" entry (file+headline org-bookmark-file "Read Later") "%[~/.config/emacs/assets/org-templates/bookmark.org]" :prepend t)
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
