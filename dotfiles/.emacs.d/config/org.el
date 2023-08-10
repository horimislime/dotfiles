(setq my/org-base-dir "~/Google Drive/My Drive/Org")

(use-package org
  :preface
  (defun my/org-screenshot ()
    (interactive)
    (setq image-file-name (concat (make-temp-name (format-time-string "%Y%m%d_%H%M%S_")) ".png")
	  image-full-path (concat (file-name-directory buffer-file-name) image-file-name)
	  image-relative-path (concat "./images/" image-file-name))
    (call-process "~/.homebrew/bin/pngpaste" nil nil nil image-full-path)
    (insert (concat "[[" image-relative-path "]]")))

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
  :bind
  (("C-c c" . org-capture)
   ("C-c j" . org-journal-new-entry)
   ("C-c o" . org-find-file)
   :map org-mode-map
   ("C-c C-v" . my/org-screenshot)
   )
  :hook
  (org-mode-hook . (lambda () (setq truncate-lines nil)))
  :custom
  (org-image-actual-width 900)
  (org-clock-idle-time 60)
  (org-startup-folded 'content)
  (org-startup-with-inline-images t)
  (org-use-speed-commands t)
  (org-todo-keywords '((type "TODO" "WAITING" "DOING"  "|" "DONE")))
  (org-clock-in-hook org-set-status-to-doing)
  (org-clock-out-hook my/org-empty-current-task-file)  
  :config
  (setq org-clock-in-hook 'org-set-status-to-doing)
  (setq org-clock-out-hook 'my/org-empty-current-task-file)
  (custom-set-faces
   '(org-block-begin-line
     ((((background dark))
       (:foreground "#669966" :weight bold)) ;; :background "#444444"
      (t (:foreground "#CC3333" :weight bold)))) ;; :background "#EFEFEF"
   '(org-block-end-line
     ((((background dark)) (:foreground "#CC3333" :weight bold))
      (t (:foreground "#669966" :weight bold))))))

(setq org-capture-templates
      '(
        ("a" "Archive" plain (file (lambda ()
                                     (let* ((slug (read-string "slug: ")))
                                       (require 'org-id)
                                       (make-directory dir t)
                                       (concat my/org-base-dir "/" slug ".org"))))
         "#+TITLE: %?\n#+DATE: %T\n#+TAGS: draft\n#+EID: %(org-id-uuid)\n\n")
        ("b" "Blog" plain (file (lambda ()
                                  (let* ((slug (read-string "slug: ")))
                                    (require 'org-id)
                                    (make-directory dir t)
                                    (concat my/org-base-dir "/" (format-time-string "%Y-%m-%d_") slug ".org"))))
         "#+TITLE: %?\n#+DATE: %T\n#+TZ: %(format-time-string \"%z (%Z)\")\n#+TAGS: draft\n#+EID: %(org-id-uuid)\n\n")
        ))

(use-package org-pomodoro
  :custom
  (org-pomodoro-play-sounds nil))

(use-package org-journal
  :custom
  (org-journal-dir (concat my/org-base-dir  "/journal"))
  (org-journal-date-format "%A, %d %B %Y"))

(use-package org-modern
  :hook
  ((org-mode-hook . org-modern-mode)
  (org-agenda-finalize-hook . org-modern-agenda)))

