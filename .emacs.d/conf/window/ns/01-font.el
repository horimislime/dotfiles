;;; font
;; http://www.dafont.com/bitstream-vera-mono.font
;; http://www-old.gnome.org/fonts/
;; http://yumisuke.net/297/emacs/
;; http://sourceforge.jp/projects/macemacsjp/lists/archive/users/2011-January/001686.html
;(create-fontset-from-ascii-font "-apple-Bitstream_Vera_Sans_Mono-medium-normal-normal-*-14-*-*-*-m-0-iso10646-1" nil "BitstreamMarugo")
(set-frame-font "fontset-bitstreammarugo")
(set-fontset-font (frame-parameter nil 'font)
                  'unicode
                  (font-spec :family "Hiragino Maru Gothic ProN")
                  nil
                  'append)
(setq face-font-rescale-alist '(("Hiragino.*" . 1.2)))
