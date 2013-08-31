(require 'auto-complete-config)
(require 'python)
(require 'jedi)
(add-hook 'python-mode-hook 'jedi:ac-setup)
(define-key python-mode-map (kbd "<C-return>") 'jedi:complete)

;; auto-complete
;(add-to-list 'load-path "~/.emacs.d/elpa/auto-complete")
;(require 'auto-complete-config)
;(add-to-list 'ac-dictionary-directories "~/.emacs.d/elpa/auto-complete-20130503.2013/dict")
;(ac-config-default)

;(setq ac-auto-start nil)
;(ac-set-trigger-key "TAB")
;(dolist (m '(python-mode java-mode))
;  (add-to-list 'ac-modes m))
;(global-auto-complete-mode t)



;(el-get 'sync '(auto-complete))
;(add-hook 'auto-complete-mode-hook
;          (lambda ()
;            (define-key ac-completing-map (kbd "C-n") 'ac-next)
;            (define-key ac-completing-map (kbd "C-p") 'ac-previous)))

;(require 'ac-python)
;(add-to-list 'ac-modes 'python-2-mode)
