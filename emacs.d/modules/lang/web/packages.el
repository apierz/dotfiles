;; -*- no-byte-compile: t; -*-
;;; lang/web/packages.el

;; requires js-beautify stylelint stylelint-scss

(package! rainbow-mode)
(package! web-beautify)
(when (featurep! :completion ivy)
  (package! counsel-css :recipe (:fetcher github :repo "hlissner/emacs-counsel-css")))

;; +html.el
(package! emmet-mode)
(package! haml-mode)
(package! pug-mode)
(package! web-mode)
(when (featurep! :completion company)
  (package! company-web))

;; +css.el
(package! less-css-mode)
(package! sass-mode)
(package! stylus-mode)

