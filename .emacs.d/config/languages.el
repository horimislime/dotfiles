;; Swift
(require 'swift-mode)

;; Markdown
(setq w3m-command "/usr/local/bin/w3m")
(require 'w3m)
(require 'markdown-mode)
;(use-package markdown-mode
;  :mode ("\\.md\\'" . gfm-mode))

(defun markdown-insert-link ()
  "Insert link tag template"
  (interactive)
  (insert "[](")
  (save-excursion
    (insert ")")))

(defun markdown-insert-image ()
  "Insert image link tag template"
  (interactive)
  (insert "![](")
  (save-excursion
    (insert ")")))

(defun w3m-browse-url-other-window (url &optional newwin)
  (let ((w3m-pop-up-windows t))
    (if (one-window-p) (split-window))
    (other-window 1)
    (w3m-browse-url url newwin)))

(defun markdown-render-w3m (n)
  (interactive "p")
  (message (buffer-file-name))
  (call-process "/usr/local/bin/grip" nil nil nil
                "--gfm" "--export"
                (buffer-file-name)
                "/tmp/grip.html")
  (w3m-browse-url-other-window "file:///tmp/grip.html")
  )
(define-key markdown-mode-map "\C-c p" 'markdown-render-w3m)


;; JavaScript
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-jsx-mode))

;; Others
(add-to-list 'auto-mode-alist '("\\Fastfile\\'" . ruby-mode))
