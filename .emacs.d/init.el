(setq load-path (cons "~/.emacs.d/site-lisp" load-path))
(setq load-path (cons "~/.emacs.d/site-lisp/apel" load-path))
(setq load-path (cons "~/.emacs.d/auto-install" load-path))

(require 'quickrun)
(require 'zlc)
;;Anything settings
(require 'anything)
(require 'anything-startup)
(global-set-key (kbd "M-f") 'anything-recentf)
(global-set-key (kbd "M-y") 'anything-show-kill-ring)
(global-set-key (kbd "M-b") 'anything-for-files)

;;Set font Ricty
(set-face-attribute 'default nil
                   :family "Ricty"
                   :height 160)
;(set-fontset-font
; nil 'japanese-jisx0208
; (font-spec :family "Ricty"))

;;Show line numbers
(require 'linum)
(global-linum-mode t)
(setq linum-format "%5d")

;;scroll by 1 line
(setq scroll-step 1)

;;Default encoding
(set-language-environment 'utf-8)

;;;;Key bind settings
;;Use command key as Meta-key
(setq ns-command-modifier (quote meta))
(setq ns-alternate-modifier (quote super))

(global-set-key "\M-z" 'undo)
;(global-set-key "\C-j" 'scroll-up)
;(global-set-key "\C-k" 'scroll-down)

;;ELScreen key settings
(global-set-key "\M-t" 'elscreen-create)
(global-set-key "\M-T" 'elscreen-clone)
(global-set-key "\M-}" 'elscreen-next)
(global-set-key "\M-{" 'elscreen-previous)
(global-set-key "\M-w" 'elscreen-kill)
(require 'elscreen)

;;give edited file execution authority if the file begin with #!~ line
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;look up words in MacOS X Dictionary.app
(require 'thingatpt)
(defun macdict-lookup (word)
  "Lookup word with Dictionary.app"
  (call-process "open" nil 0 nil (concat "dict://" word)))
  
(defun macdict-lookup-word ()
  "Lookup the word at point with Dictionary.app."
  (interactive)
  (macdict-lookup (word-at-point)))

(global-set-key "\M-l" 'macdict-lookup-word)

;;truncate lines in separated windows
(setq truncate-partial-width-windows nil)


;;;;;;;;;;Transparent window setting
(when (eq window-system 'mac)
  (add-hook 'window-setup-hook
            (lambda ()
              (setq mac-autohide-menubar-on-maximize t)
              (set-frame-parameter nil 'fullscreen 'fullboth)
              )))

(defun mac-toggle-max-window ()
  (interactive)
  (if (frame-parameter nil 'fullscreen)
      (set-frame-parameter nil 'fullscreen nil)
    (set-frame-parameter nil 'fullscreen 'fullboth)))

;;Clipboard setting
(defvar prev-yanked-text nil "*previous yanked text")

(setq interprogram-cut-function
      (lambda (text &optional push)
        ; use pipe
        (let ((process-connection-type nil))
          (let ((proc (start-process "pbcopy" nil "pbcopy")))
            (process-send-string proc string)
            (process-send-eof proc)
            ))))

(setq interprogram-paste-function
      (lambda ()
        (let ((text (shell-command-to-string "pbpaste")))
          (if (string= prev-yanked-text text)
              nil
            (setq prev-yanked-text text)))))


;;Hide menu
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(display-time-mode t)
 '(make-backup-files nil)
 '(tool-bar-mode nil)
 '(transient-mark-mode t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

;;Window Color
(if window-system (progn
   (set-background-color "Black")
   (set-foreground-color "LightGray")
   (set-cursor-color "Gray")
   (set-frame-font "ricty-12")
   (set-frame-parameter nil 'alpha 80)
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;Keybind setting;;;;;;;;;;;;;;;;;;;;;;;;;
;;smartchr.el
(require 'smartchr)
(defun my-smartchr-setting ()
  (local-set-key (kbd "'") (smartchr '("'`!!''" "'")))
  (local-set-key (kbd "+") (smartchr '(" + " "++" " + =  " "+")))
  (local-set-key (kbd "-") (smartchr '(" - " "--" " - =  " "-")))
  (global-set-key (kbd "=") (smartchr '(" = " " == " " === ")))
  (local-set-key (kbd ">") (smartchr '(">" "->" ">>")))
  (global-set-key (kbd "{") (smartchr '(" { `!!' }" " {")))
  (local-set-key (kbd "(") (smartchr '("(`!!')" "(")))
  (local-set-key (kbd "[") (smartchr '("[`!!']" "[")))
)
(add-hook 'java-mode-hook 'my-smartchr-setting)
(add-hook 'javascript-mode-hook 'my-smartchr-setting)

;;cscope
(require 'xcscope)
;;pymacs
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)

(eval-after-load "pymacs"
  '(add-to-list 'pymacs-load-path "~/.pymacs"))

;; python-mode, pycomplete 
(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist (cons '("python" . python-mode)
                                   interpreter-mode-alist))
(autoload 'python-mode "python-mode" "Python editing mode." t)
(add-hook 'python-mode-hook '(lambda ()
                               (require 'pycomplete)
                               ))


;; python-mode
(autoload 'python-mode "python-mode" nil t)
(autoload 'py-shell "python-mode" "Python shell" t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-hook 'python-mode-hook
          '(lambda()
             (require 'pycomplete)
             (setq indent-tabs-mode nil)))
;;;;;;;;;;


;;Development environment
(require 'flymake)
;; redefine to remove "check-syntax" target
(defun flymake-get-make-cmdline (source base-dir)
  (list "make"
        (list "-s"
              "-C"
              base-dir
              (concat "CHK_SOURCES=" source)
               "SYNTAX_CHECK_MODE=1")))

;; specify that flymake use ant instead of make                                                                                                                
(setcdr (assoc "\\.java\\'" flymake-allowed-file-name-masks)
        '(flymake-simple-ant-java-init flymake-simple-java-cleanup))

;; redefine to remove "check-syntax" target
(defun flymake-get-ant-cmdline (source base-dir)
  (list "ant"
        (list "-buildfile"
              (concat base-dir "/" "build.xml"))))

(add-hook 'java-mode-hook
          '(lambda ()
             (flymake-mode)))



;;Auto install
(require 'auto-install)
(setq auto-install-directory "~/.emacs.d/auto-install/")
;(auto-install-update-emacswiki-package-name t)
(auto-install-compatibility-setup)

