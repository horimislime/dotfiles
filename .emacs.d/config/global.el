;; Basic
(require 'server)
(unless (server-running-p)
    (server-start))

(prefer-coding-system 'utf-8)
(setq split-width-threshold nil)
(setq-default indent-tabs-mode nil)
(setq mac-emulate-three-button-mouse nil)
(global-auto-revert-mode 1)
(setq vc-follow-symlinks t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(setq backup-directory-alist '(("." . "/tmp"))
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 20
      kept-old-versions 5)

;; Appearance
(load-theme 'misterioso t)

(defface hlline-face
  '((((class color)
      (background dark))
     (:background "dark slate gray"))
    (((class color)
      (background light))
     (:background  "#98FB98"))
    (t
     ()))
  "*Face used by hl-line.")
(setq hl-line-face 'hlline-face)
(global-hl-line-mode)

(when (and (executable-find "cmigemo")
           (require 'migemo nil t))
  (setq migemo-command "/usr/local/bin/cmigemo")
  (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
  (setq migemo-options '("-q" "--emacs"))

  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix)
  (load-library "migemo")
  (migemo-init))

(require 'quickrun)
(require 'popwin)
(push '("*quickrun*") popwin:special-display-config)

(require 'powerline)
(powerline-default-theme)

;; Anything
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
(global-set-key (kbd "\C-c \C-f") 'anything-custom-filelist)
(global-set-key (kbd "C-x b") 'anything-for-files)
(global-set-key (kbd "C-c t") 'term+mux-new-other-window)
                
