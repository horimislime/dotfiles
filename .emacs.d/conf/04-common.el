;;reload firefox
(require 'moz)

(defun moz-browser-reload ()
  (interactive)
  (comint-send-string
   (inferior-moz-process)
   "BrowserReload();"))

;; Clipboard setting
(setq x-select-enable-clipboard t)
(set-clipboard-coding-system 'utf-8)

;; Distinguish files with same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; Load environment values
(load-file (expand-file-name "~/.emacs.d/shellenv.el"))
(dolist (path (reverse (split-string (getenv "PATH") ":")))
  (add-to-list 'exec-path path))

;; auto-complete
(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)

;(require 'auto-complete-config)
(ac-config-default)
(setq ac-auto-start nil)
(ac-set-trigger-key "TAB")

;; e-Mail
(setq user-mail-address "horimi_soichiro@cyberagent.co.jp")
(setq user-full-name "Soichiro HORIMI")
(setq smtpmail-smtp-server "mail.cyberagent.co.jp")
(setq mail-user-agent 'message-user-agent)
(setq message-send-mail-function 'message-smtpmail-send-it)

;; Emacs.app as a server
(require 'server)
(unless (server-running-p)
  (server-start))

(global-set-key "\C-x\C-g" 'goto-line)

;; migemo
;;(require 'migemo)
;(setq migemo-command "cmigemo")
;(setq migemo-options '("-q" "--emacs"))
;(setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
;(setq migemo-user-dictionary nil)
;(setq migemo-regex-dictionary nil)
;(setq migemo-coding-system 'utf-8-mac)
;(load-library "migemo")
;(migemo-init)

;(add-hook 'kill-emacs-hook 'migemo-kill)
;(add-hook 'kill-emacs-hook 'migemo-pattern-alist-clear)


