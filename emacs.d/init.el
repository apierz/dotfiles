
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'org)
(org-babel-load-file
 (expand-file-name "config.org"
                   user-emacs-directory))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-latex-default-packages-alist
   (quote
    (("AUTO" "inputenc" t)
     ("T1" "fontenc" t)
     ("" "fixltx2e" nil)
     ("" "graphicx" t)
     ("" "longtable" nil)
     ("" "float" nil)
     ("" "wrapfig" nil)
     ("" "rotating" nil)
     ("normalem" "ulem" t)
     ("" "amsmath" t)
     ("" "textcomp" t)
     ("" "marvosym" t)
     ("" "wasysym" t)
     ("" "amssymb" t)
     ("colorlinks=true,linkcolor=blue,urlcolor=blue" "hyperref" t)
     ("" "" nil))))
 '(package-selected-packages
   (quote
    (monokai-theme dracula-theme fm anaphora python-mode eldoc-eval doom-themes hl-line+ pydoc all-the-icons neotree evil-multiedit evil-anzu iedit anzu fountain-mode rainbow-mode coffee-mode yasnippet yaml-mode web-mode use-package unicode-fonts unbound smooth-scrolling robe rainbow-delimiters powerline-evil org-grep org-bullets markdown-mode linum-relative linum-off key-chord helm-projectile flymake-ruby flycheck fill-column-indicator evil-visual-mark-mode evil-surround evil-snipe evil-mu4e evil-magit evil-leader evil-commentary engine-mode dumb-jump color-theme-sanityinc-tomorrow auto-complete)))
 '(python-shell-completion-native-disabled-interpreters (quote ("pypy" "python3"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(doom-modeline-buffer-project ((t (:foreground "white" :weight normal))))
 '(highlight ((t nil)))
 '(org-block-background ((t (:background "#272822"))))
 '(org-checkbox ((t (:background "#272822" :foreground "#61AFEF" :box (:line-width 1 :style released-button)))))
 '(region ((t (:inherit highlight :background "#7b7962")))))
