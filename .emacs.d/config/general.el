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

(require 'neotree)
(global-set-key (kbd "C-x t") 'neotree-toggle)

(add-to-list 'auto-mode-alist '("\\Fastfile\\'" . ruby-mode))

(tool-bar-mode 0)
(set-scroll-bar-mode nil)