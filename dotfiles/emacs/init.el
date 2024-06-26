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

(use-package emacs
  :preface
  ;; Interact with macOS clipboard
  (defun my/paste-to-clipboard (text &optional push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
	(process-send-string proc text)
	(process-send-eof proc))))
  (defun my/create-parent-directory ()
    "Create the parent directory of the current buffer's file if it doesn't exist."
    (when buffer-file-name
      (let ((dir (file-name-directory buffer-file-name)))
	(unless (file-exists-p dir)
          (make-directory dir t)))))
  (add-hook 'find-file-hook 'my/create-parent-directory)
  :bind
  (("s-{" . (lambda () (interactive) (select-window (previous-window))))
   ("s-}" . (lambda () (interactive) (select-window (next-window))))
   ("C-x C-l" . (lambda () (interactive) (load-file buffer-file-name)))
   ("C-x C-h" . 'help-for-help)
   ("C-h" . 'delete-backward-char))
  :init
  (set-default 'buffer-file-coding-system 'utf-8)
  (set-language-environment "Japanese")
  (prefer-coding-system 'utf-8)
  (auto-save-visited-mode t)
  (global-auto-revert-mode 1) ;; Reload if opening file is modified by other program
  (put 'erase-buffer 'disabled nil) ;; Clear contents using erase-buffer
  (menu-bar-mode 0) ;; Hide menu bar
  (setq auto-save-visited-interval 60 ;; should be set before enabling the mode
	backup-by-copying t
	backup-directory-alist '(("." . "~/.config/emacs/backup"))
	browse-url-browser-function 'eww
	completion-cycle-threshold 3
	create-lockfiles nil ;; Do not create .#lockfile
	delete-old-versions t
	indent-tabs-mode nil ;; Use soft tab
	initial-scratch-message nil ;; No initial message on scratch buffer
	kept-new-versions 5
	kept-old-versions 2
	large-file-warning-threshold nil
	recentf-max-saved-items 200
	split-width-threshold nil ;; Always split window vertically
	tab-always-indent 'complete
	vc-follow-symlinks t ;; Always follow symbolic links
	vc-handled-backends () ;; Disable vc-mode
	version-control t
	warning-minimum-level :emergency)
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
  (setq default-frame-alist initial-frame-alist))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
  (add-to-list 'exec-path-from-shell-variables "SSH_AUTH_SOCK")
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

(use-package all-the-icons)
(use-package neotree
  :after
  (all-the-icons)
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
  :bind
  (("C-c t" . neotree-toggle))
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

(use-package magit
  :bind
  ("C-c g m" . 'magit))

(use-package browse-at-remote
  :bind
  ("C-c g g" . 'browse-at-remote))

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
  (terraform-format-on-save t))
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

(use-package treesit-auto
  :config
  (setq treesit-auto-install t)
  (global-treesit-auto-mode))

(use-package eglot
  :preface
  (defun my/eglot-organize-imports ()
    (call-interactively 'eglot-code-action-organize-imports))
  :mode
  (("\\.ts\\'" . typescript-ts-mode)
   ("\\.js\\'" . js-ts-mode)
   ("\\.tsx\\'" . tsx-ts-mode)
   ("Dockerfile" . dockerfile-ts-mode))
  :hook
  ((dart-mode . eglot-ensure)
   (js-ts-mode . eglot-ensure)
   (tsx-ts-mode . eglot-ensure)
   (typescript-ts-mode . eglot-ensure)
   (before-save . eglot-format-buffer)
   (before-save . my/eglot-organize-imports))
  :bind
  (:map eglot-mode-map
   ("C-c e a" . eglot-code-actions)))

(use-package rg)
(use-package ripgrep)
(use-package projectile
  :init
  (projectile-mode +1)
  :bind
  (:map projectile-mode-map
	("C-c p" . projectile-command-map))
  :custom
  (projectile-project-search-path '(("~/ghq/github.com" . 2))))

(use-package lsp-ui)

(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  :init
  (global-corfu-mode))

(use-package tempel
  :bind
  (("C-c TAB" . tempel-complete)
   ("M-*" . tempel-insert)))

(use-package nov
  :mode
  (("\\.epub\\'" . nov-mode)))

(use-package pdf-tools
  :bind
  (:map pdf-view-mode-map
        ("C-s" . isearch-forward))
  :hook
  (pdf-view-mode . (lambda ()
        	     (display-line-numbers-mode 0)))
  :init
  (pdf-tools-install)
  :custom
  (pdf-annot-activate-created-annotations t)
  (pdf-view-resize-factor 0.6))

(use-package pdf-view-restore
  :after pdf-tools
  :hook
  ((pdf-view-mode . pdf-view-restore-mode))
   :config
  (add-hook 'pdf-view-mode-hook 'pdf-view-restore-mode)
  :custom
  (pdf-view-restore-filename "~/.config/emacs/.pdf-view-restore"))

(use-package chatgpt-shell
  :hook
  ((chatgpt-shell-mode . (lambda ()
			   (if (not chatgpt-shell-openai-key)
			       (setq chatgpt-shell-openai-key
				     (shell-command-to-string "echo -n `op item get chatgpt-token --fields label=credential`"))))))
  :custom
  (chatgpt-shell-model-version "gpt-4"))

(use-package vterm
  :bind
  (:map vterm-mode-map
	("C-g" . vterm--self-insert)
	("C-x C-c" . vterm--self-insert))
  :hook
  ((vterm-mode . (lambda ()
        	   (display-line-numbers-mode 0)
		   (setq-local global-hl-line-mode nil)
		   (set (make-local-variable 'buffer-face-mode-face) '(:family "Hack Nerd Font"))
		   (buffer-face-mode t)))))

(use-package elfeed
  :preface
  (defun my/elfeed-load-feed ()
    (interactive)
    (setq elfeed-feeds
        (with-temp-buffer
          (insert-file-contents "~/Dropbox/emacs/elfeed-source.csv")
          (mapcar
           (lambda (line) 
             (let ((items (split-string line ",")))
               (string-trim (cadr items))))
           (split-string (buffer-string) "\n" t)))))
  :config
  (my/elfeed-load-feed))
