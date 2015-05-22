(use-package markdown-mode
  :mode ("\\.md\\'" . gfm-mode))

(defun markdown-preview-file ()
  "run Marked on the current file and revert the buffer"
  (interactive)
  (shell-command
   (format "open -a \"/opt/homebrew-cask/Caskroom/marked/latest/Marked 2.app\" %s"
	   (shell-quote-argument (buffer-file-name))))
  )

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

(add-hook
 'markdown-mode-hook
 '(lambda ()
    (define-key markdown-mode-map (kbd "C-c C-m") 'markdown-preview-file)
    (define-key markdown-mode-map (kbd "C-c C-l") 'markdown-insert-link)
    (define-key markdown-mode-map (kbd "C-c C-i") 'markdown-insert-image)
    ))
