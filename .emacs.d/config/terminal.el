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
