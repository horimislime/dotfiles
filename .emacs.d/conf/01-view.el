;; color-theme
(when (require 'color-theme)
  (color-theme-initialize)
  (when (require 'color-theme-solarized)
    (color-theme-solarized-dark)))

;; Using Ricty font
(set-face-attribute 'default nil
                   :family "Ricty"
                   :height 140)

;; Enable git-gutter
(global-git-gutter-mode t)

;; Highlight current line
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

;; ELScreen
(setq elscreen-prefix-key "\C-t")
(elscreen-start)
(setq elscreen-display-tab 12) 
(setq elscreen-tab-display-kill-screen nil) 
(require 'elscreen-server nil t)

;; popwin.el
;(setq pop-up-windows nil)
;(require 'popwin nil t)
;(when (require 'popwin nil t)
;  (setq anything-samewindow nil)
;  (setq display-buffer-function 'popwin:display-buffer)
;  (push '("anything" :regexp t :height 0.5) popwin:special-display-config)
;  (push '("*Completions*" :height 0.4) popwin:special-display-config)
;  (push '("*compilation*" :height 0.4 :noselect t :stick t) popwin:special-display-config)
;)

(set-face-background 'highlight-indentation-face "#e3e3d3")
(set-face-background 'highlight-indentation-current-column-face "#c3b3b3")
