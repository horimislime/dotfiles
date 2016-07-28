;; Swift
(require 'swift-mode)
(require 'auto-complete-swift)
(push 'ac-source-swift-complete ac-sources)

;; Objective-C
(ffap-bindings)
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@implementation" . objc-mode))
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@interface" . objc-mode))
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@protocol" . objc-mode))

(setq ff-other-file-alist
      '(("\\.mm?$" (".h"))
        ("\\.cc$"  (".hh" ".h"))
        ("\\.hh$"  (".cc" ".C"))

        ("\\.c$"   (".h"))
        ("\\.h$"   (".c" ".cc" ".C" ".CC" ".cxx" ".cpp" ".m" ".mm"))

        ("\\.C$"   (".H"  ".hh" ".h"))
        ("\\.H$"   (".C"  ".CC"))

        ("\\.CC$"  (".HH" ".H"  ".hh" ".h"))
        ("\\.HH$"  (".CC"))

        ("\\.cxx$" (".hh" ".h"))
        ("\\.cpp$" (".hpp" ".hh" ".h"))

        ("\\.hpp$" (".cpp" ".c"))))
(add-hook 'objc-mode-hook
          (lambda ()
            (define-key c-mode-base-map (kbd "C-c o") 'ff-find-other-file)
            ))

;;Markdown
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

(setq w3m-command "/usr/local/bin/w3m")
(require 'w3m)
(require 'markdown-mode)

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
