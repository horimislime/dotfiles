(use-package helm
  :init
  (setq helm-ff-file-name-history-use-recentf t)
  (setq helm-display-function #'display-buffer)
  :bind
  (("M-x" . 'helm-M-x)
   ("C-c C-f" . helm-find-files)
   ("C-c C-r" . helm-for-files)
   ("C-c C-y" . helm-show-kill-ring)
   ("C-c C-b" . helm-buffers-list)
   :map helm-map
   ("C-h" . delete-backward-char)
   :map helm-find-files-map
   ("C-h" . delete-backward-char)
   ("TAB" . helm-execute-persistent-action)
   :map helm-read-file-map
   ("TAB" . helm-execute-persistent-action))
  :config
  (helm-mode 1))

(use-package helm-ghq
  :bind
  (("C-c C-g" . helm-ghq)))
