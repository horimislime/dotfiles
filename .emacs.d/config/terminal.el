;; term+
(use-package term+
  :config
  (progn
    (use-package term+key-intercept)
    (use-package term+mux)
    (require 'xterm-256color)))
(setq explicit-shell-file-name "/usr/local/bin/zsh")

;; editorconfig
(setq edconf-exec-path "/usr/local/bin/editorconfig")
(require 'editorconfig)
(editorconfig-mode 1)

;; auto-complete
(require 'auto-complete-config)
(ac-config-default)
(ac-set-trigger-key "TAB")
(setq ac-use-menu-map t)
(setq ac-use-fuzzy t)
