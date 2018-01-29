;;; lang/web/+css.el -*- lexical-binding: t; -*-

;; css-mode hooks apply to scss and less-css modes
(add-hook 'css-mode-hook #'rainbow-delimiters-mode)
(add-hook! (css-mode sass-mode)
  #'(yas-minor-mode-on flycheck-mode highlight-numbers-mode))

(after! smartparens
  (sp-with-modes '(css-mode scss-mode less-css-mode stylus-mode)
    (sp-local-pair "/*" "*/" :post-handlers '(("[d-3]||\n[i]" "RET") ("| " "SPC")))))

(map! :map* (css-mode-map scss-mode-map less-css-mode-map)
      :n "M-R" #'+css/web-refresh-browser
      (:localleader
        :n  "rb" #'+css/toggle-inline-or-block))


;;
;; Packages
;;

(def-package! counsel-css
  :when (featurep! :completion ivy)
  :commands (counsel-css counsel-css-imenu-setup)
  :hook (css-mode . counsel-css-imenu-setup)
  :init
  (map! :map* (css-mode-map scss-mode-map less-css-mode-map)
        :localleader :n ";" #'counsel-css))


(def-package! rainbow-mode
  :hook (css-mode sass-mode))


(def-package! css-mode
  :mode "\\.css$"
  :mode ("\\.scss$" . scss-mode)
  :config
  (set! :company-backend '(css-mode scss-mode) '(company-css company-yasnippet))
  (map! :map scss-mode-map :localleader "b" #'+css/scss-build))


(def-package! sass-mode
  :mode "\\.sass$"
  :config
  (set! :company-backend 'sass-mode '(company-css company-yasnippet))
  (map! :map scss-mode-map :localleader "b" #'+css/sass-build))


(def-package! less-css-mode
  :mode "\\.less$")


(def-package! stylus-mode
  :mode "\\.styl$"
  :init (add-hook! stylus-mode #'(yas-minor-mode-on flycheck-mode)))

