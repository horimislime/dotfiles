(require 'anything-startup)
(when (require 'recentf nil t)
  (setq recentf-max-saved-items 2000)
  (setq recentf-exclude '(".recentf"))
  (setq recentf-auto-cleanup 10)
  (setq recentf-auto-save-timer
        (run-with-idle-timer 30 t 'recentf-save-list))
  (recentf-mode 1)
  (require 'recentf-ext))

(defun anything-custom-filelist ()
  (interactive)
  (anything-other-buffer
   (append
    '(anything-c-source-ffap-line
      anything-c-source-ffap-guesser
      anything-c-source-buffers+
      )
    (anything-c-sources-git-project-for)
    '(anything-c-source-recentf
      anything-c-source-bookmarks
      anything-c-source-file-cache
      anything-c-source-filelist
      ))
   "*anything file list*"))

(defun chomp (str)
  (replace-regexp-in-string "[\n\r]+$" "" str))

(defun anything-git-project-project-dir ()
  (chomp
   (shell-command-to-string "git rev-parse --show-toplevel")))

(defun anything-c-sources-git-project-for ()
  (loop for elt in
        '(("Modified files (%s)" . "--modified")
          ("Untracked files (%s)" . "--others --exclude-standard")
          ("All controlled files in this project (%s)" . ""))
        collect
        `((name . ,(format (car elt) (anything-git-project-project-dir)))
          (init . (lambda ()
                    (unless (and ,(string= (cdr elt) "") ;update candidate buffer every time except for that of all project files
                                 (anything-candidate-buffer))
                      (with-current-buffer
                          (anything-candidate-buffer 'global)
                        (insert
                         (shell-command-to-string
                          ,(format "git ls-files --full-name $(git rev-parse --show-cdup) %s"
                                   (cdr elt))))))))
          (candidates-in-buffer)
          (display-to-real . (lambda (name)
                               (format "%s/%s"
                                       (anything-git-project-project-dir) name)))
          (type . file))))

(defun anything-git-project ()
  (interactive)
  (let* ((sources (anything-c-sources-git-project-for)))
    (anything-other-buffer sources
                           (format "*Anything git project in %s*"
                                   (anything-git-project-project-dir)))))

;; Key binding
(global-set-key (kbd "C-x C-r") 'eval-buffer)
(bind-key "\C-x\C-g" 'goto-line)
;(global-set-key (kbd "\C-c f") 'anything-custom-filelist)
(global-set-key (kbd "C-c f") 'anything-for-files)
(global-set-key (kbd "C-c t") 'term+mux-new-other-window)
