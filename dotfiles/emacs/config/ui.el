(use-package nerd-icons)
(use-package doom-themes
  :custom
  (doom-themes-enable-italic t)
  (doom-themes-enable-bold t)
  :config
  (load-theme 'doom-dracula t)
  (doom-themes-neotree-config)
  (doom-themes-org-config)
  (use-package doom-modeline
    :custom
    (doom-modeline-buffer-file-name-style 'truncate-with-project)
    (doom-modeline-icon t)
    (doom-modeline-major-mode-color-icon t)
    (doom-modeline-major-mode-icon t)
    (doom-modeline-minor-modes t)
    :hook
    (after-init . doom-modeline-mode)
    :config
    (set-cursor-color "cyan")
    (line-number-mode 0)
    (column-number-mode 0)
    (doom-modeline-def-modeline 'main
      '(bar window-number matches buffer-info remote-host buffer-position parrot selection-info)
      '(misc-info persp-name lsp github debug minor-modes input-method major-mode process vcs checker))))

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

(global-display-line-numbers-mode t)

