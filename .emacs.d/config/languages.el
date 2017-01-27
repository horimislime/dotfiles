
;; Markdown
(use-package markdown-mode
  :mode ("\\.md\\'" . gfm-mode))

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

(defun markdown-preview-file ()
  "run Marked on the current file and revert the buffer"
  (interactive)
  (shell-command
   (format "open -a /opt/homebrew-cask/Caskroom/marked/latest/Marked2.app %s"
       (shell-quote-argument (buffer-file-name))))
)
(global-set-key "\C-cm" 'markdown-preview-file)

;; JavaScript
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-jsx-mode))

;; Others
(add-to-list 'auto-mode-alist '("\\Fastfile\\'" . ruby-mode))
