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

(global-display-line-numbers-mode t)

;; (use-package dimmer
;;   :custom
;;   (dimmer-fraction 0.3)
;;   (dimmer-adjustment-mode :background)
;;   (dimmer-exclusion-regexp "^\*helm.*\\|^ \*Minibuf-.*")
;;   :config
;;   (dimmer-configure-which-key)
;;   (dimmer-configure-magit)
;;   (dimmer-configure-org)
;;   (dimmer-mode t))
