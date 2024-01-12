(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(eglot treesit-auto ox-hugo elfeed rg ripgrep ag fira-code-mode browse-at-remote org-ac vterm pcre2el org-clock chatgpt-shell chatgpt slack pdf-view-restore pdf-tools nov terraform-mode org-modern all-the-icons minions solarized-theme nerd-icons org-journal moody nyan-mode corfu tempel-collection tempel org-preview-html org-pomodoro doom-modeline doom-themes persistent-scratch prettier-js hover company lsp-ui yasnippet projectile lsp-dart lsp-mode vscode-dark-plus-theme flutter dart-mode flycheck smart-mode-line-powerline-theme smart-mode-line powerline benchmark-init git-gutter auto-complete use-package js2-mode git-gutter-fringe git-modes magit exec-path-from-shell rainbow-delimiters yaml-mode ruby-end ruby-block helm-ghq helm helm-config neotree popwin smart-cursor-color-mode auto-save-buffers-enhanced ivy undo-tree markdown-mode helm-core darcula-theme nil smart-cursor-color))
 '(safe-local-variable-values
   '((eval dap-register-debug-template "Flutter :: Itoyokado Local"
	   (list :type "flutter" :args
		 '("--flavor" "itoyokadoDevelopment" "--dart-define" "ENVIRONMENT=local" "--dart-define" "SITE_TYPE=itoyokado")))
     (eval dap-register-debug-template "Flutter :: Itoyokado Development"
	   (list :type "flutter" :args
		 '("--flavor" "itoyokadoDevelopment" "--dart-define" "ENVIRONMENT=development" "--dart-define" "SITE_TYPE=itoyokado")))))
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(doom-modeline-bar ((t (:background "#6272a4"))))
 '(org-block-begin-line ((((background dark)) (:foreground "#669966" :weight bold)) (t (:foreground "#CC3333" :weight bold))))
 '(org-block-end-line ((((background dark)) (:foreground "#CC3333" :weight bold)) (t (:foreground "#669966" :weight bold)))))
