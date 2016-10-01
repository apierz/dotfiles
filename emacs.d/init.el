
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
 '(package-selected-packages
   (quote
    (anaphora python-mode eldoc-eval doom-themes hl-line+ pydoc all-the-icons neotree evil-multiedit evil-anzu iedit anzu fountain-mode rainbow-mode coffee-mode yasnippet yaml-mode web-mode use-package unicode-fonts unbound smooth-scrolling robe rainbow-delimiters powerline-evil org-grep org-bullets markdown-mode linum-relative linum-off key-chord helm-projectile flymake-ruby flycheck fill-column-indicator evil-visual-mark-mode evil-surround evil-snipe evil-mu4e evil-magit evil-leader evil-commentary engine-mode dumb-jump color-theme-sanityinc-tomorrow auto-complete)))
 '(python-shell-completion-native-disabled-interpreters (quote ("pypy" "python3"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
