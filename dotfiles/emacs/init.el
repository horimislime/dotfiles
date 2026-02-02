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
  (benchmark-init/activate)
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

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
  (context-menu-mode 1)
  (set-face-attribute 'default nil :height 160)
  (setq auto-save-visited-interval 60 ;; should be set before enabling the mode
	backup-by-copying t
	backup-directory-alist '(("." . "~/.config/emacs/backup"))
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
	warning-minimum-level :emergency
	frame-resize-pixelwise t
	treesit-auto-install 'prompt
	use-short-answers t
	pixel-scroll-precision-mode t)
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

;; ==========
;; Interface
;; ==========

(use-package nerd-icons)
(use-package ef-themes
  :config
  (load-theme 'ef-maris-dark  :no-confirm)
  ;; Enable fontification of DONE headlines
  (setq org-fontify-done-headline t)
  ;; Customize the face for DONE headlines
  (custom-set-faces
   '(org-done ((t (:foreground "gray" :weight normal :strike-through t)))))
  )

(use-package minions
    :custom
    (minions-mode-line-lighter "...")
    (minions-mode-line-delimiters '("" . ""))
    :config
    (minions-mode +1))

(use-package smart-cursor-color
  :config
  (smart-cursor-color-mode +1))

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

;; ============
;; completions
;; ============

(fido-vertical-mode 1)
(recentf-mode 1)

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  :config
  (add-hook 'minibuffer-setup-hook
            (lambda ()
              (setq-local completion-styles '(orderless basic)) t)))

(use-package consult
  :bind
  (("C-c M-x" . consult-mode-command)
   ("C-x C-f" . find-file)
   ("C-x C-r" . consult-recent-file)
   ("C-x C-y" . consult-yank-pop)
   ("C-x C-b" . consult-buffer)
   ("C-s" . consult-line)
   ("C-M-s" . consult-ripgrep)
   ("M-g g" . consult-goto-line)
   ("M-g o" . consult-outline))
  :custom
  (consult-async-min-input 1)
  :config
  (consult-customize consult-recent-file :preview-key nil)
  (setq consult-narrow-key "<"))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(global-set-key (kbd "C-c e") 'embark-act)
(use-package embark
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; =========
;; org-mode
;; =========

(use-package pcre2el)
(use-package org-find-file
  :load-path "packages/")
(use-package org-web-tools)
(use-package org
  :preface
  (defun my/copy-to-blog-dir ()
    (interactive)
    (let* ((current-directory (file-name-directory buffer-file-name))
	       (blog-dir-root "~/ghq/github.com/horimislime/horimisli.me")
	       (raw-blog-post-id (file-name-base (directory-file-name current-directory)))
	       (blog-post-id (replace-regexp-in-string "_" "-" 
						       (replace-regexp-in-string "^[0-9]+-" "" raw-blog-post-id)))
	       (blog-post-dir (format "%s/posts/blog/%s/%s" blog-dir-root (format-time-string "%Y") blog-post-id))
;	       (blog-image-dir (format "%s/public/images" blog-dir-root))
	       (blog-content-file "content.org")
	       (blog-image-files (directory-files current-directory nil "\\(\\.png\\|\\.jpg\\|\\.jpeg\\|\\.gif\\)$")))

	  (unless (file-exists-p blog-post-dir)
	    (make-directory blog-post-dir t))
	  (copy-file (concat current-directory blog-content-file) (format "%s/%s" blog-post-dir blog-content-file) t)
	  (dolist (file blog-image-files)
	    (copy-file (concat current-directory file) (format "%s/%s" blog-post-dir file) t))
	  (message "Successfully copied entry data."))
    )
    
  (defun my/org-screenshot ()
    (interactive)
    (setq image-file-name (concat (make-temp-name (format-time-string "%Y%m%d_%H%M%S_")) ".jpg")
	  image-full-path (concat (file-name-directory buffer-file-name) image-file-name))
    (call-process "~/.homebrew/bin/pngpaste" nil nil nil image-full-path)
    (insert (format "[[./%s]]" image-file-name))
    (org-redisplay-inline-images))
  (defun my/write-to-task-file (content)
    (write-region content
		  nil "~/.config/emacs/clock-task.txt"
		  nil 'quiet))
  (defun org-set-status-to-doing ()
    (if (org-clocking-p)
	(org-todo "DOING"))
    (my/write-to-task-file (substring-no-properties org-clock-current-task)))
  (defun my/org-empty-current-task-file ()
    (my/write-to-task-file ""))
  (defun my/create-org-file-with-name ()
    (let* ((slug (read-string "slug: ")))
      (format "%s/note/%s/content.org" org-directory slug)))

  (defun my/create-web-archive ()
    (let* ((timestamp (format-time-string "%Y%m%d%H%M%S")))
      (format "%s/bookmark/%s/content.org" org-directory timestamp)))
  (defun my/fetch-page-body-as-org (url)
    "Based on org-web-tools--url-as-readable-org"
    (-let* ((url (or url (org-web-tools--get-first-url)))
            (dom (plz 'get url :as #'org-web-tools--sanitized-dom))
            ((title . readable) (org-web-tools--eww-readable dom))
            (converted (org-web-tools--html-to-org-with-pandoc readable)))
      (with-temp-buffer
	(org-mode)
	(insert converted)
	(org-web-tools--demote-headings-below 2)
	(goto-char (point-min))
	(buffer-string))))

  (defun my/get-title-from-url (url)
    (let ((title))
      (with-current-buffer (url-retrieve-synchronously url)
	(car (dom-strings (dom-by-tag (libxml-parse-html-region) 'title))))))
  (defun my/paste-url-with-title ()
    (interactive)
    (insert (format "[[%s][%s]]"
		    (current-kill 0)
		    (my/get-title-from-url (car kill-ring)))))
  (defun my/find-location-under-week-headline (type)
    "Find or create my default journal tree"
    (let* ((week-begin-date-string (if (string-equal "Sun" (format-time-string "%a"))
				      (format-time-string "%Y/%m/%d (\%a)")
				    (format-time-string "%Y/%m/%d (\%a)" (org-read-date nil t "-Sun"))))
	  (week-end-date-string (format-time-string "%Y/%m/%d (\%a)" (org-read-date nil t "Sat")))
	  (hd (format "%s - %s" week-begin-date-string week-end-date-string)))

    (goto-char (point-min))
    (unless (derived-mode-p 'org-mode)
      (error
       "Target buffer \"%s\" for jww/find-journal-tree should be in Org mode"
       (current-buffer)))
    (if (re-search-forward
	 (format org-complex-heading-regexp-format (regexp-quote hd))
	 nil t)
	(goto-char (point-at-bol))
      (progn (org-insert-heading)
	     (insert hd)))

    (org-narrow-to-subtree)
    (if (re-search-forward
	 (format org-complex-heading-regexp-format (regexp-quote type))
	 nil t)
	(goto-char (point-at-bol))
      (or (bolp) (insert "\n"))
      (org-end-of-subtree)
      (insert "\n** " type "\n")
      (beginning-of-line 0))
    (widen)
    ))
  (defun my/find-k-under-headline ()
    (my/find-location-under-week-headline "KEEP"))
  (defun my/find-p-under-headline ()
    (my/find-location-under-week-headline "PROBLEM"))
  (defun my/org-update-statistics-cookies-after-refile ()
    (run-at-time "0.01 sec" nil (lambda () (org-update-statistics-cookies 'all))))

  (defun my/blog-draft-mode-hook ()
    "Hook function for blog-draft-mode to auto-copy on save."
    (my/copy-to-blog-dir))

  (define-minor-mode blog-draft-mode
    "Minor mode for blog draft files that auto-copies to blog directory on save."
    :lighter " BlogDraft"
    :group 'org
    (if blog-draft-mode
        (add-hook 'after-save-hook #'my/blog-draft-mode-hook nil t)
      (remove-hook 'after-save-hook #'my/blog-draft-mode-hook t)))

  :bind
  (("C-c c" . org-capture)
   ("C-c j" . org-journal-new-entry)
   ("C-c o f b" . (lambda () (interactive) (org-find-file (format "%s/bookmark" org-directory))))
   ("C-c o f n" . (lambda () (interactive) (org-find-file (format "%s/note" org-directory))))
   :map org-mode-map
   ("C-c C-p C-v" . my/org-screenshot)
   ("C-c C-u C-v" . my/paste-url-with-title)
   ("C-c o c" . my/copy-to-blog-dir)
   ("C-c o d" . blog-draft-mode))

  :hook
  ((org-mode . visual-line-mode)
   (org-clock-in . org-set-status-to-doing)
   (org-clock-out . my/org-empty-current-task-file)
   (org-after-refile-insert . my/org-update-statistics-cookies-after-refile))

  :init
  (require 'org-protocol)
  (setq org-directory "~/OneDrive/org"
	org-backlog-file (format "%s/inbox.org" org-directory)
	org-bookmark-file (format "%s/bookmark.org" org-directory)
	org-daily-tasks-file (format "%s/roam/tasks_latest/content.org" org-directory)
	org-kpt-file (format "%s/note/kpt/content.org" org-directory)
	org-a-file (format "%s/note/a-log/content.org" org-directory)
	elfeed-source-csv (format "%s/assets/elfeed-source.csv" org-directory))

  :custom
  (org-image-actual-width 900)
  (org-clock-idle-time 60)
  (org-startup-folded 'content)
  (org-startup-with-inline-images t)
  (org-use-speed-commands t)
  (org-todo-keywords '((sequence "TODO" "DOING"  "|" "DONE")))
  (org-capture-templates
   '(("d" "Weekdays TODO" entry (file org-daily-tasks-file) "%[~/OneDrive/org/assets/weekdays-todo.org]" :prepend t :empty-lines 1)
     ("w" "Weekends TODO" entry (file org-daily-tasks-file) "%[~/OneDrive/org/assets/weekends-todo.org]" :prepend t)
     ("n" "Create Note" plain (file my/create-org-file-with-name) "%[~/OneDrive/org/assets/note.org]")
     ("t" "Put work task into inbox" entry (file+headline org-backlog-file "Work") "* TODO %?\n" :prepend t)
     ("h" "Put private task into inbox" entry (file+headline org-backlog-file "Private") "* TODO %?\n" :prepend t)
     ("b" "Bookmark" entry 
         (file+headline "~/OneDrive/org/bookmark.org" "Bookmarks")
         "%[~/GoogleDrive/org/assets/bookmark2.org]" :prepend t)

     ("k" "Keep" entry (file+function org-kpt-file my/find-k-under-headline) "*** %?\n")
     ("p" "Problem" entry (file+function org-kpt-file my/find-p-under-headline) "*** %?\n")
     ("f" "Subscribe Feed" plain (file elfeed-source-csv) "%(my/get-title-from-url \"%:link\"),%:link\n" :prepend t :immediate-finish t)
     ))
  (org-refile-targets '(("~/Library/CloudStorage/OneDrive/org/roam/tasks_latest/content.org" :maxlevel . 2)
                        ("~/Library/CloudStorage/OneDrive/org/inbox.org" :maxlevel . 2))))

(use-package org-pomodoro
  :custom
  (org-pomodoro-play-sounds nil))

(use-package org-journal
  :custom
  (org-journal-dir (concat org-directory  "/journal"))
  (org-journal-file-format "%Y%m%d/journal.org")
  (org-journal-date-format "%Y/%m/%d (%a)"))

(use-package org-roam
  :preface
  (require 'org-roam-protocol)
  (defun my/org-open-at-point-same-buffer ()
    "Configure org links to always open in the same buffer."
    (interactive)
    (let ((org-link-frame-setup '((file . find-file))))
      (org-open-at-point)))
  :custom
  (org-roam-directory (file-truename (format "%s/roam" org-directory)))
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :target (file+head "${slug}/content.org" "#+title: ${title}\n")
      :unnarrowed t)))
  (org-roam-capture-ref-templates
   '(("r" "ref" plain "%?"
      :target (file+head
	       "b-%<%Y%m%d-%H%M>/content.org"
	       "#+title: ${title}\n#+date: %U\n#+filetags: :bookmark:\n\n %(my/fetch-page-body-as-org \"${ref}\")\n")
      :unnarrowed t)))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
	 ("C-c n o" . my/org-open-at-point-same-buffer)
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag))) 
  (org-roam-db-autosync-mode))

;; org-babel

(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)))

(defun my-org-confirm-babel-evaluate (lang body)
  "Show prompt if some keywords exist in command line"
  (or 
   (string-match-p "rm -rf" body)
   (string-match-p "apply" body)
   (string-match-p "delete" body)))
(setq org-confirm-babel-evaluate #'my-org-confirm-babel-evaluate)

(defun my/org-remove-all-results ()
  "Remove all #+RESULTS: block within current buffer"
  (interactive)
  (org-babel-map-src-blocks nil
    (org-babel-remove-result)))

;; ========
;; Writing
;; ========

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
   (text-mode . flycheck-mode)
   (js-ts-mode . flycheck-mode)
   (tsx-ts-mode . flycheck-mode)
   (typescript-ts-mode . flycheck-mode)))

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
  :custom
  (neo-window-fixed-size nil) ; allow flexible resizing
  (neo-autorefresh t)
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

;;;; Git

(use-package magit
  :bind
  ("C-c g m" . 'magit))

(use-package browse-at-remote
  :bind
  ("C-c g g" . 'browse-at-remote))

(use-package git-gutter
  :custom-face
  (git-gutter:modified ((t (:background "#682d0f"))))
  (git-gutter:added    ((t (:background "#1b4721"))))
  (git-gutter:deleted  ((t (:background "#ff79c6"))))
  :config
  (global-git-gutter-mode t))

;;;; Language

(use-package markdown-mode
  :mode
  (("\\.md\\'" . gfm-mode)
   ("\\.markdown\\'" . gfm-mode))
  :bind
  (("C-c C-p". markdown-preview)))

(use-package yaml-mode)

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

(use-package eglot
  :preface
  (defun my/eglot-organize-imports ()
    (call-interactively 'eglot-code-action-organize-imports))
  (define-derived-mode helmfile-mode yaml-mode "helm")
  (defun my/apply-helmfile-mode ()
    "Enable helmfile-mode for files in specific directory structure."
    (when (and buffer-file-name
               (string-match-p "/charts/.*/templates/.*\.ya?ml" buffer-file-name))
      (helmfile-mode)))
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
   (yaml-mode . eglot-ensure)
   (helmfile-mode . eglot-ensure)
;   (before-save . eglot-format-buffer)
;   (before-save . my/eglot-organize-imports)
   (find-file . my/apply-helmfile-mode))
  :bind
  (:map eglot-mode-map
	("C-c e a" . eglot-code-actions))
  :config
  (add-to-list 'eglot-server-programs '(helmfile-mode "helm_ls" "serve")))

(use-package rg)
(use-package ripgrep)
(use-package projectile
  :init
  (projectile-mode +1)
  :bind
  (:map projectile-mode-map
	("C-c p" . projectile-command-map))
  :custom
  (projectile-project-search-path '(("~/ghq/github.com" . 2)))
  (projectile-globally-ignored-directories '(".git" "node_modules" "build" "cache" "logs")))

(use-package lsp-ui)

;(use-package corfu
;  :custom
;  (corfu-cycle t)
;  (corfu-auto t)
;  :init
;  (global-corfu-mode))

(use-package tempel
  :bind
  (("C-c TAB" . tempel-complete)
   ("M-*" . tempel-insert)))

(use-package gptel
  :init
  (gptel-make-anthropic "Claude"
			:models '(claude-4-5-20250929)
			:stream t
			:key (shell-command-to-string "echo -n `op item get claude-api-key --fields label=credential`"))
  (setq
   gptel-model 'claude-3-7-sonnet-20250219
   gptel-backend (gptel-make-anthropic "Claude"
		   :stream t
		   :key (shell-command-to-string "echo -n `op item get claude-api-key --fields label=credential`")))
  (require 'gptel-integrations))

(use-package shell-maker)
(use-package chatgpt-shell
  :hook
  ((chatgpt-shell-mode . (lambda ()
			   (if (not chatgpt-shell-anthropic-key)
			       (setq chatgpt-shell-anthropic-key
				     (shell-command-to-string "echo -n `op item get claude-api-key --fields label=credential`")
				     ))))))

(use-package vterm
  :bind
  (:map vterm-mode-map
	("C-g" . vterm--self-insert)
	("C-h" . vterm--self-insert)
	("C-x C-c" . vterm--self-insert)
        ("M-RET" . vterm-send-return))
  :hook
  ((vterm-mode . (lambda ()
        	   (display-line-numbers-mode 0)
		   (setq-local global-hl-line-mode nil)
		   (set (make-local-variable 'buffer-face-mode-face) '(:family "Hack Nerd Font"))
		   (buffer-face-mode t)))))

(use-package vterm-toggle
  :bind
  (("C-c v s" . vterm-toggle-show)
   ("C-c v h" . vterm-toggle-hide)
   ("C-c v n" . vterm-toggle-forward)
   ("C-c v p" . vterm-toggle-backward))
  :config
  ;; Show vterm buffer in the window located at bottom
  (add-to-list 'display-buffer-alist
               '((lambda(bufname _) (with-current-buffer bufname (equal major-mode 'vterm-mode)))
                 (display-buffer-reuse-window display-buffer-in-direction)
                 (direction . bottom)
                 (reusable-frames . visible)
                 (window-height . 0.4)))
  ;; Above display config affects all vterm command, not only vterm-toggle
  (defun my/vterm-new-buffer-in-current-window()
    (interactive)
    (let ((display-buffer-alist nil))
            (vterm)))
  )

(use-package elfeed
  :preface
  (defun my/elfeed-load-feed ()
    (interactive)
    (setq elfeed-feeds
        (with-temp-buffer
          (insert-file-contents "~/OneDrive/org/assets/elfeed-source.csv")
          (mapcar
           (lambda (line) 
             (let ((items (split-string line ",")))
               (string-trim (cadr items))))
           (split-string (buffer-string) "\n" t)))))
  :config
  (my/elfeed-load-feed))


