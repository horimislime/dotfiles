;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; package.el / el-get
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq package-enable-at-startup nil)
(require 'package)
(setq package-archives
  '(("gnu" . "https://elpa.gnu.org/packages/")
  ("melpa" . "https://melpa.org/packages/")
))
(package-initialize)
(package-refresh-contents)

(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))
(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-recipes")

(package-install 'darcula-theme)
(package-install 'smart-cursor-color)
(el-get-bundle auto-complete)
(el-get-bundle editorconfig)
(el-get-bundle emoji-fontset)
(el-get-bundle exec-path-from-shell)
(el-get-bundle fuzzy)
(el-get-bundle git-modes)
(el-get-bundle git-gutter-fringe)
(el-get-bundle helm)
(el-get-bundle helm-ghq)
(el-get-bundle js2-mode)
(el-get-bundle json)
(el-get-bundle magit)
(el-get-bundle markdown-mode)
(el-get-bundle neotree)
(el-get-bundle popup)
(el-get-bundle popwin)
(el-get-bundle powerline)
(el-get-bundle rainbow-delimiters)
(el-get-bundle recentf-ext)
(el-get-bundle ruby-block)
(el-get-bundle ruby-end)
(el-get-bundle ruby-mode)
(el-get-bundle use-package)
(el-get-bundle xterm-color)
(el-get-bundle yaml-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (auto-save-buffers-enhanced ivy undo-tree markdown-mode helm-core darcula-theme nil smart-cursor-color))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
      Kept-old-versions 5)

;; Interact with macOS clipboard
(defun paste-to-clipboard (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))
(setq interprogram-cut-function 'paste-to-clipboard)

; Clear contents using erase-buffer
(put 'erase-buffer 'disabled nil)

;; Do not create .#lockfile
(setq create-lockfiles nil)
;; Automatically backup scratch buffer
(use-package auto-save-buffers-enhanced
  :ensure t
  :init
  (auto-save-buffers-enhanced t)
  :config
  (setq auto-save-buffers-enhanced-interval 3600)
  (setq auto-save-buffers-enhanced-exclude-regexps '(".+"))
  (setq auto-save-buffers-enhanced-save-scratch-buffer-to-file-p t)
  (setq auto-save-buffers-enhanced-file-related-with-scratch-buffer (locate-user-emacs-file ".scratch-backup")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Appearance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package darcula-theme)
;(require 'smart-cursor-color-mode)
(defface hlline-face
  '((((class color)
      (background dark))
      (:background "dark slate gray"))
     (((class color)
       (background light))
      (:background  "#939595"))
     (t
      ()))
   "*Face used by hl-line.")
(setq hl-line-face 'hlline-face)
(global-hl-line-mode +1)
(smart-cursor-color-mode +1)

; Prefer global-display-line-number-mode over global-linum-mode
(if (version<= "26.0" emacs-version)
    (global-display-line-numbers-mode)
  (global-linum-mode)
)

(require 'neotree)
(global-set-key (kbd "C-x t") 'neotree-toggle)

;; No initial message on scratch buffer
(setq initial-scratch-message nil)

(require 'popwin)
(popwin-mode 1)
(setq popwin:popup-window-position 'bottom)
(push "*Buffer List*" popwin:special-display-config)
(push "*scratch*" popwin:special-display-config)
(push "*Warnings*" popwin:special-display-config)

;; Delete current buffer
(defun delete-buffer ()
  "clear current buffer"
  (interactive)
  (let ((start (point-min))
        (end   (point-max)))
    (delete-region start end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Git
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'magit)
(require 'git-modes)
(global-git-gutter-mode t)
(setq vc-handled-backends ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'helm-config)
(require 'helm-for-files)
(helm-mode 1)
(setq helm-ff-file-name-history-use-recentf t)

(setq helm-display-function #'display-buffer)
(when (require 'popwin)
  (setq popwin:special-display-config
    '(("*complitation*" :noselect t)
      ("helm" :regexp t :height 0.4))))

(defun custom-helm-sources ()
  (interactive)
  (helm :sources '(helm-source-recentf
                   helm-source-files-in-current-dir
                   helm-source-buffers-list)
        :buffer "*Custom helm sources*"))

(define-key global-map (kbd "C-x C-f") 'helm-find-files)
(define-key global-map (kbd "C-x C-r") 'custom-helm-sources)
(define-key global-map (kbd "M-x")     'helm-M-x)
(define-key global-map (kbd "M-y")     'helm-show-kill-ring)
(define-key global-map (kbd "C-x b")   'helm-buffers-list)
(define-key global-map (kbd "M-r")     'helm-resume)
(define-key global-map (kbd "C-M-h")   'helm-apropos)
(define-key helm-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; neotree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'neotree)

(setq neo-show-hidden-files t)
(setq neo-persist-show t)
(setq neo-keymap-style 'concise)
(setq neo-smart-open t)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
(when neo-persist-show
  (add-hook 'popwin:before-popup-hook
            (lambda () (setq neo-persist-show nil)))
  (add-hook 'popwin:after-popup-hook
            (lambda () (setq neo-persist-show t))))

(global-set-key (kbd "C-x t") 'neotree-toggle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Markdown
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode)))

(defun markdown-insert-link ()
  "Insert link tag template"
  (interactive)
  (insert "[](")
  (save-excursion
    (insert ")")))

(defun markdown-insert-image ()
  "Insert image link tag template"
  (interactive)
  (insert "![](")
  (save-excursion
    (insert ")")))

(defun markdown-preview-file ()
  "run Marked on the current file and revert the buffer"
  (interactive)
  (shell-command
   (format "open -a /opt/homebrew-cask/Caskroom/marked/latest/Marked2.app %s"
       (shell-quote-argument (buffer-file-name))))
)
(global-set-key "\C-cm" 'markdown-preview-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ruby
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun custom-fastfile-mode ()
  (when (and (stringp buffer-file-name)
             (string-match "\\Fastfile\\'" buffer-file-name))
    (insert "OK")
    (ruby-mode)))

(add-hook 'find-file-hook 'custom-fastfile-mode)
