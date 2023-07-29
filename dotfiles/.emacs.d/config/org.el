(add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))
(setq my/org-base-dir "~/Google Drive/My Drive/Org")

(defun my/org-screenshot ()
  (interactive)
  (setq filename
        (concat
         (make-temp-name
          (concat (file-name-directory buffer-file-name)
                  (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
  (call-process "~/.homebrew/bin/pngpaste" nil nil nil filename)
  (insert (concat "[[" filename "]]")))

(use-package org
  :bind
  ("C-c C-x i" . my/org-screenshot)
  :config
  (custom-set-faces
   '(org-block-begin-line
     ((((background dark))
       (:foreground "#669966" :weight bold)) ;; :background "#444444"
      (t (:foreground "#CC3333" :weight bold)))) ;; :background "#EFEFEF"
   '(org-block-end-line
     ((((background dark)) (:foreground "#CC3333" :weight bold))
      (t (:foreground "#669966" :weight bold))))))

;;(setq org-directory "~/Documents/org")
(setq org-use-speed-commands t)
(setq org-todo-keywords
      '((type "TODO" "WAITING" "DOING"  "|" "DONE")))

(defun my/write-to-task-file (content)
  (write-region content
              nil "~/.emacs.d/clock-task.txt"
              nil 'quiet)
  )

(defun org-set-status-to-doing ()
  (if (org-clocking-p)
      (org-todo "DOING"))
  (my/write-to-task-file (substring-no-properties org-clock-current-task))
  )

(defun my/org-empty-current-task-file ()
  (my/write-to-task-file "")
  )
(setq org-clock-in-hook 'org-set-status-to-doing)
(setq org-clock-out-hook 'my/org-empty-current-task-file)

;(add-to-list 'org-speed-commands '("t" org-todo "TODO"))
;(add-to-list 'org-speed-commands '("d" org-todo "DONE"))
(define-key global-map (kbd "C-c c") 'org-capture)
(define-key global-map (kbd "C-c C-x M") 'org-insert-todo-heading)
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

(use-package org-pomodoro)
(setq org-pomodoro-play-sounds nil)

(use-package org-journal
  :custom
  (org-journal-dir "~/Google Drive/My Drive/Org/journal")
  (org-journal-date-format "%A, %d %B %Y"))
