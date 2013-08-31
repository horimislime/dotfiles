;;add load path
(setq load-path (cons "~/.emacs.d/site-lisp" load-path))
(add-to-list 'load-path "~/.emacs.d/site-lisp/helm")
(add-to-list 'load-path "~/dev/auto-complete")
(add-to-list 'load-path "~/.emacs.d/site-lisp/ensime_2.10.0-0.9.8.9/elisp/")

;; MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))
(el-get 'sync)

;; init loader
;; http://coderepos.org/share/browser/lang/elisp/init-loader/init-loader.el
(eval-when-compile (require 'cl))
(defvar init-loader-directory (expand-file-name "~/.emacs.d/conf"))
(defvar init-loader-regexp "\\(?:^[[:digit:]]\\{2\\}-.*\.elc?\\)$")
(defun init-loader-files (dir regexp)
  (let ((common-files (directory-files dir t))
        (window-files (directory-files (mapconcat 'identity (list init-loader-directory "window" (symbol-name window-system)) "/") t)))
    (loop for file in (append common-files window-files)
          when (and (string-match regexp (file-name-nondirectory file))
                    (or (not (locate-library (concat file "c")))
                        (string-match "\.elc$" file)))
          collect file into ret
          finally return (sort ret 'string<))))
(defun init-loader-load (dir regexp)
  (dolist (target (init-loader-files dir regexp))
    (condition-case e
        (load (file-name-sans-extension target) nil t)
      (error (message "load error: %s - %s" target e)))))
(init-loader-load init-loader-directory init-loader-regexp)

;; after init
(add-hook 'after-init-hook
  (lambda ()
    (message "init time: %.3f sec"
             (float-time (time-subtract after-init-time before-init-time)))))

;(require 'twittering-mode)
;(setq twittering-use-master-password t)
