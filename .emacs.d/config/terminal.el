;; term+
(use-package term+
  :config
  (progn
    (use-package term+key-intercept)
    (use-package term+mux)
    (require 'xterm-256color)))
