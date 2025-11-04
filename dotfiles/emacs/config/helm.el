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

