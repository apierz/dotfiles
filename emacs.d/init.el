
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
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(byte-compile-delete-errors t)
 '(git-gutter:added-sign "▐")
 '(git-gutter:deleted-sign "▐")
 '(git-gutter:modified-sign "▐")
 '(grep-highlight-matches nil)
 '(hl-sexp-background-color "#efebe9")
 '(org-latex-default-packages-alist
   '(("AUTO" "inputenc" t)
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
     ("" "" nil)))
 '(org-tags-column -50)
 '(package-selected-packages
   '(dracula-theme dap-mode which-key lsp-mode evil-numbers ghub doom-modeline 0blayout plan9-theme nofrils-acme-theme diminish base16-theme avk-emacs-themes hexrgb ipython elpy ggtags helm-gtags company-dict company-emacs-eclim company company-go company-web helm-company go-mode git-gutter leuven-theme spacemacs-theme gruvbox-theme fm anaphora eldoc-eval doom-themes hl-line+ pydoc all-the-icons neotree evil-multiedit evil-anzu iedit anzu fountain-mode rainbow-mode coffee-mode yaml-mode web-mode use-package unicode-fonts unbound smooth-scrolling robe rainbow-delimiters powerline-evil org-grep org-bullets markdown-mode key-chord helm-projectile flymake-ruby flycheck fill-column-indicator evil-visual-mark-mode evil-surround evil-snipe evil-mu4e evil-magit evil-leader evil-commentary engine-mode auto-complete))
 '(python-shell-completion-native-disabled-interpreters '("pypy" "python3"))
 '(safe-local-variable-values '((eval when (fboundp 'rainbow-mode) (rainbow-mode 1)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(git-gutter:added ((t (:foreground "#40883F"))))
 '(git-gutter:deleted ((t (:foreground "#FF5555"))))
 '(git-gutter:modified ((t (:foreground "#AF8700"))))
 '(org-date ((t (:underline t))))
 '(org-tag ((t (:foreground nil :background "#98ece8" :slant italic)))))
