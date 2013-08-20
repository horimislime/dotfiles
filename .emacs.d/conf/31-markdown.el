;;marked
(defun markdown-preview-file ()
  "run Marked on the current file and revert the buffer"
  (interactive)
  (shell-command
   (format "open -a /Applications/Marked.app %s"
	   (shell-quote-argument (buffer-file-name))))
  )
(global-set-key "\C-cm" 'markdown-preview-file)

;; copy markdown as html using python-markdown2
(defun yank-md-in-html ()
  "Copy html source converted from markdown"
  (interactive)
  (shell-command
  (format "/usr/local/share/python/markdown2 %s | pbcopy"
	   (shell-quote-argument (buffer-file-name))))
  )
(global-set-key "\C-xy" 'yank-md-in-html)

;;markdown-mode
(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
(setq auto-mode-alist (cons '("\\.text" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.markdown" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))

(defun markdown-custom ()
  "markdown-mode-hook"
  (local-unset-key (kbd "M-<return>")))
(add-hook 'markdown-mode-hook '(lambda() (markdown-custom)))
