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

(set-default 'buffer-file-coding-system 'utf-8)
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
;;(setq backup-directory-alist '(("." . user-emacs-directory)))

;; Interact with macOS clipboard
(defun my/paste-to-clipboard (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(if (display-graphic-p)
    (tool-bar-mode -1)
  (setq interprogram-cut-function 'my/paste-to-clipboard))

(when (memq window-system '(mac ns))
  (setq initial-frame-alist
        (append
         '((ns-transparent-titlebar . t)
	   (ns-appearance . dark)
           (vertical-scroll-bars . nil)
           (internal-border-width . 0)))))
(setq default-frame-alist initial-frame-alist)

;; Automatically save opening files
(setq auto-save-visited-interval 60) ;; should be set before enabling the mode
(auto-save-visited-mode t)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))

(defun my/load (configs)
  (dolist (config configs)
    (let ((now (current-time))
        (force-load-messages))
    (load (concat user-emacs-directory "config/" config)  nil 'nomessage)
    (message nil)
    ))
  )
(my/load '("ui.el"
           "helm.el"
	   "org.el"
	   ))

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

;;;; Language

(use-package markdown-mode
  :mode
  (("\\.md\\'" . gfm-mode)
   ("\\.markdown\\'" . gfm-mode))
  :bind
  (("C-c C-p". markdown-preview)))

(use-package yaml-mode
  :hook (js2-mode . prettier-js)
  )

(use-package prettier-js)

(use-package terraform-mode
  :custom
  (setq terraform-format-on-save-mode t))
(use-package lsp-mode
  :custom
  ;; terraform
  (setq lsp-enable-links t)
  (setq lsp-terraform-ls-enable-show-reference t)
  (setq lsp-terraform-ls-prefill-required-fields t)
  (setq lsp-terraform-ls-validate-on-save t)
  (setq lsp-semantic-tokens-enable t)
  (setq lsp-semantic-tokens-honor-refresh-requests t)
  :hook
  ((terraform-mode . lsp-deferred)))
(use-package lsp-dart
  :hook
  ((dart-mode . lsp)))

(with-eval-after-load 'projectile
  (add-to-list 'projectile-project-root-files-bottom-up "pubspec.yaml")
  (add-to-list 'projectile-project-root-files-bottom-up "BUILD"))

(use-package projectile)
(use-package lsp-ui)
(use-package company)

(use-package tempel
  :bind (("M-+" . tempel-complete)
         ("M-*" . tempel-insert)))

(use-package nov
  :mode
  (("\\.epub\\'" . nov-mode)))
