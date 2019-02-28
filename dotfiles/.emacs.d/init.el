;;;; package.el

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure t))

;;;; General / Appearance

(use-package benchmark-init
  :config
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

(require 'uniquify)

(prefer-coding-system 'utf-8)
(setq indent-tabs-mode nil) ;; Use soft tab
(global-auto-revert-mode 1) ;; Reload if opening file is modified by other program
(setq vc-follow-symlinks t) ;; Always follow symbolic links
(setq create-lockfiles nil) ;; Do not create .#lockfile
(setq vc-handled-backends ()) ;; Disable vc-mode
(setq split-width-threshold nil) ;; Always split window vertically
(put 'erase-buffer 'disabled nil) ;; Clear contents using erase-buffer
(setq initial-scratch-message nil) ;; No initial message on scratch buffer
(menu-bar-mode 0) ;; Hide menu bar

;; Interact with macOS clipboard
(defun paste-to-clipboard (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))
(setq interprogram-cut-function 'paste-to-clipboard)

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

(global-display-line-numbers-mode t)

;; Automatically backup scratch buffer
(use-package auto-save-buffers-enhanced
  :init
  (auto-save-buffers-enhanced t)
  :config
  (setq auto-save-buffers-enhanced-interval 3600)
  (setq auto-save-buffers-enhanced-exclude-regexps '(".+"))
  (setq auto-save-buffers-enhanced-save-scratch-buffer-to-file-p t)
  (setq auto-save-buffers-enhanced-file-related-with-scratch-buffer (locate-user-emacs-file ".scratch-backup")))

(use-package darcula-theme)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))

(use-package flycheck
  :config
  (flycheck-define-checker textlint
    "textlint"
    :command ("textlint" "--format" "unix" source-inplace)
    :error-patterns
    ((warning line-start (file-name) ":" line ":" column ": "
              (id (one-or-more (not (any " "))))
              (message (one-or-more not-newline)
		       (zero-or-more "\n" (any " ") (one-or-more not-newline)))
              line-end))
    :modes (text-mode markdown-mode gfm-mode))
  (add-to-list 'flycheck-checkers 'textlint)
  :hook
  ((gfm-mode . flycheck-mode)
   (text-mode . flycheck-mode)))

(use-package smart-cursor-color
  :config
  (smart-cursor-color-mode +1))

(use-package neotree
  :config
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
  :bind (("C-x C-t" . neotree-toggle))
  )

(use-package popwin
  :init
  (setq popwin:popup-window-position 'bottom)
  (setq popwin:special-display-config
        '(("*Warnings*" :regexp t)))
  :config
  (popwin-mode +1))

(use-package powerline
  :config
  (powerline-nano-theme))

(use-package auto-complete
  :config
  (require 'auto-complete-config))

(with-eval-after-load 'auto-complete-config
  (ac-config-default)
  (setq ac-use-menu-map t)
  (setq ac-use-fuzzy t))

;;;; Git

(use-package magit)

(use-package git-gutter
  :config
  (global-git-gutter-mode t))

;;;; Helm

(use-package helm
  :init
  (setq helm-ff-file-name-history-use-recentf t)
  (setq helm-display-function #'display-buffer)
  :bind
  (("M-x" . 'helm-M-x)
   ("C-x C-f" . helm-find-files)
   ("C-x C-r" . helm-for-files)
   ("C-x C-y" . helm-show-kill-ring)
   ("C-x C-b" . helm-buffers-list)
   :map helm-map
   ("C-h" . delete-backward-char)
   :map helm-find-files-map
   ("C-h" . delete-backward-char)
   ("TAB" . helm-execute-persistent-action)
   :map helm-read-file-map
   ("TAB" . helm-execute-persistent-action))
  :config
  (helm-mode 1))

(use-package helm-ghq
  :bind
  (("C-x C-g" . helm-ghq)))

;;;; Language

(use-package js2-mode)

(use-package markdown-mode
  :mode
  (("\\.md\\'" . gfm-mode)
   ("\\.markdown\\'" . gfm-mode))
  :bind
  (("C-c C-p". markdown-preview)))

(use-package rainbow-delimiters
  :init
  (require 'cl-lib)
  (require 'color)
  (setq rainbow-delimiters-outermost-only-face-count 1)
  :config
  (rainbow-delimiters-mode +1)
  (set-face-foreground 'rainbow-delimiters-depth-1-face "#9a4040")
  (set-face-foreground 'rainbow-delimiters-depth-2-face "#ff5e5e")
  (set-face-foreground 'rainbow-delimiters-depth-3-face "#ffaa77")
  (set-face-foreground 'rainbow-delimiters-depth-4-face "#dddd77")
  (set-face-foreground 'rainbow-delimiters-depth-5-face "#80ee80")
  (set-face-foreground 'rainbow-delimiters-depth-6-face "#66bbff")
  (set-face-foreground 'rainbow-delimiters-depth-7-face "#da6bda")
  (set-face-foreground 'rainbow-delimiters-depth-8-face "#afafaf")
  (set-face-foreground 'rainbow-delimiters-depth-9-face "#f0f0f0")
  :hook
  ((emacs-lisp-mode . rainbow-delimiters-mode)
   (js2-mode-hook . rainbow-delimiters-mode)))

(use-package ruby-mode
  :mode
  (("\\Fastfile\\'" . ruby-mode)))

(with-eval-after-load 'ruby-block-autoloads
  (setq ruby-block-highlight-toggle t))

(use-package ruby-end)
(use-package yaml-mode)
