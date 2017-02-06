
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
 '(git-gutter+-added-sign "***")
 '(git-gutter+-deleted-sign "***")
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
 '(org-tags-column -50)
 '(package-selected-packages
   (quote
    (leuven-theme tao-theme flatland-theme spacegray-theme spacemacs-theme peacock-theme git-gutter-fringe+ moe-theme material-theme gruvbox-theme monokai-theme dracula-theme fm anaphora python-mode eldoc-eval doom-themes hl-line+ pydoc all-the-icons neotree evil-multiedit evil-anzu iedit anzu fountain-mode rainbow-mode coffee-mode yasnippet yaml-mode web-mode use-package unicode-fonts unbound smooth-scrolling robe rainbow-delimiters powerline-evil org-grep org-bullets markdown-mode linum-relative linum-off key-chord helm-projectile flymake-ruby flycheck fill-column-indicator evil-visual-mark-mode evil-surround evil-snipe evil-mu4e evil-magit evil-leader evil-commentary engine-mode dumb-jump color-theme-sanityinc-tomorrow auto-complete)))
 '(python-shell-completion-native-disabled-interpreters (quote ("pypy" "python3")))
 '(safe-local-variable-values
   (quote
    ((eval when
           (fboundp
            (quote rainbow-mode))
           (rainbow-mode 1))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#Ffffff" :foreground "#333333" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "nil" :family "Fira Mono for Powerline"))))
 '(doom-modeline-buffer-modified ((t (:inherit highlight :background "#335EA8"))))
 '(doom-modeline-info ((t (:inherit warning))))
 '(org-tag ((t (:background "white" :foreground "#9A9FA4" :slant italic :weight normal :height 1.0)))))

