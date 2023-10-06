(use-package pcre2el)
(use-package org-find-file
  :load-path "packages/")

(use-package org
  :preface
  (defun my/org-screenshot ()
    (interactive)
    (setq image-file-name (concat (make-temp-name (format-time-string "%Y%m%d_%H%M%S_")) ".png")
	  image-relative-path (concat "images/" image-file-name)
	  image-full-path (concat (file-name-directory buffer-file-name) image-relative-path))
    (call-process "~/.homebrew/bin/pngpaste" nil nil nil image-full-path)
    (insert (format "[[./%s]]" image-relative-path))
    (org-redisplay-inline-images))
  (defun my/write-to-task-file (content)
    (write-region content
		  nil "~/.emacs.d/clock-task.txt"
		  nil 'quiet))
  (defun org-set-status-to-doing ()
    (if (org-clocking-p)
	(org-todo "DOING"))
    (my/write-to-task-file (substring-no-properties org-clock-current-task)))
  (defun my/org-empty-current-task-file ()
    (my/write-to-task-file ""))
  (defun my/create-org-file-with-name ()
    (let* ((slug (read-string "slug: ")))
      (format "%s/%s.org" org-directory slug)))
  (defun my/get-title-from-url (url)
    (let ((title))
      (with-current-buffer (url-retrieve-synchronously url)
	(car (dom-strings (dom-by-tag (libxml-parse-html-region) 'title))))))
  (defun my/paste-url-with-title ()
    (interactive)
    (insert (format "[[%s][%s]]"
		    (current-kill 0)
		    (my/get-title-from-url (car kill-ring)))))

  :bind
  (("C-c c" . org-capture)
   ("C-c j" . org-journal-new-entry)
   ("C-c o" . org-find-file)
   :map org-mode-map
   ("C-c C-p C-v" . my/org-screenshot)
   ("C-c C-u C-v" . my/paste-url-with-title))

  :hook
  ((org-mode . visual-line-mode)
   (org-clock-in . org-set-status-to-doing)
   (org-clock-out . my/org-empty-current-task-file))

  :init
  (require 'org-protocol)
  (setq org-directory "~/Google Drive/My Drive/Org"
	org-backlog-file (format "%s/inbox.org" org-directory)
	org-bookmark-file (format "%s/bookmark.org" org-directory)
	org-daily-tasks-file (format "%s/tasks.org" org-directory))

  :custom
  (org-image-actual-width 900)
  (org-clock-idle-time 60)
  (org-startup-folded 'content)
  (org-startup-with-inline-images t)
  (org-use-speed-commands t)
  (org-todo-keywords '((type "TODO" "WAITING" "DOING"  "|" "DONE")))
  (org-capture-templates
   '(("d" "Weekdays TODO" entry (file org-daily-tasks-file) "%[~/.emacs.d/assets/org-templates/weekdays-todo.org]" :prepend t)
     ("w" "Weekends TODO" entry (file org-daily-tasks-file) "%[~/.emacs.d/assets/org-templates/weekends-todo.org]" :prepend t)
     ("n" "Create Note" plain (file my/create-org-file-with-name) "%[~/.emacs.d/assets/org-templates/note.org]")
     ("t" "Put task into inbox" entry (file+headline org-backlog-file "Work") "* TODO %?\n" :prepend t)
     ("p" "Put task into inbox" entry (file+headline org-backlog-file "Private") "* TODO %?\n" :prepend t)
     ("b" "Bookmark" entry (file+headline org-bookmark-file "Bookmarks") "%[~/.emacs.d/assets/org-templates/bookmark.org]")
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

