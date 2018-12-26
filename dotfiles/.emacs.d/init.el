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
(el-get-bundle smart-cursor-color :url "https://raw.githubusercontent.com/syohex/smart-cursor-color-mode/master/smart-cursor-color-mode.el")
(el-get-bundle auto-complete)
(el-get-bundle color-theme-zenburn)
(el-get-bundle editorconfig)
(el-get-bundle emoji-fontset)
(el-get-bundle exec-path-from-shell)
(el-get-bundle fuzzy)
(el-get-bundle gist)
(el-get-bundle git-modes)
(el-get-bundle git-gutter-fringe)
(el-get-bundle helm)
(el-get-bundle helm-ghq)
(el-get-bundle js2-mode)
(el-get-bundle json)
(el-get-bundle magit)
(el-get-bundle markdown-mode)
(el-get-bundle neotree)
(el-get-bundle open-junk-file)
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

(add-to-list 'custom-theme-load-path "~/.emacs.d/el-get/atom-one-dark-theme/")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (nil smart-cursor-color))))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Appearance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package darcula-theme)
(require 'smart-cursor-color-mode)
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

(global-linum-mode t)

(require 'neotree)
(global-set-key (kbd "C-x t") 'neotree-toggle)

;; No initial message on scratch buffer
(setq initial-scratch-message nil)

(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)
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
(global-git-gutter-mode)
(setq vc-handled-backends ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'helm-config)
(helm-mode 1)

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
  :mode ("\\.md\\'" . gfm-mode))

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
