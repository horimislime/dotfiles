(require 'helm-config)
(helm-mode 1)

(defun custom-helm-sources ()
  (interactive)
  (helm :sources '(helm-source-recentf
                   helm-source-files-in-current-dir
                   helm-source-buffers-list)
        :buffer "*Custom helm sources*"))

(define-key global-map (kbd "C-x C-f") 'helm-find-files)
(define-key global-map (kbd "C-x C-r") 'custom-helm-sources)
(define-key global-map (kbd "M-x")     'helm-M-x)
(define-key global-map (kbd "M-y")     'helm-show-kill-ring)
(define-key global-map (kbd "C-x b")   'helm-buffers-list)
(define-key global-map (kbd "M-r")     'helm-resume)
(define-key global-map (kbd "C-M-h")   'helm-apropos)
(define-key helm-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)

;; helm-ghq
(add-to-list 'helm-for-files-preferred-list 'helm-source-ghq)
(add-to-list 'exec-path "/usr/local/bin/ghq")
(define-key global-map (kbd "C-c C-g") 'helm-ghq)