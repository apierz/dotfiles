;;; nord-theme.el --- An arctic, north-bluish color palette.
;; Package-Version: 20170225.824

;; Copyright (c) 2016 Artic Ice Studio
;; Copyright (c) 2017 Andy Pierz

;; Author Andy Pierz <andy@andypierz.com>
;; (current maintainer)

;; Version 1.0
;; License: Apache 2.0

;;; Commentary:

;; An unofficial port of the Nord Theme, created by Artic Ice Studio.

;; This theme attempts to mimic the color choices and syntax highlighting of
;; the Vim version of this theme whereever possible, but also includes a few
;; tweaks and support for many Emacs modes such as Mu4e, Gnus, Org, Helm,
;; Powerline and more.

;; This theme is designed to work in both GUI and terminal interfaces, however
;; it requires using Nord Colors in your terminal to display properply.

;;; Code:

(deftheme nord "An arctic, north-bluish color palette")

(let ((class '((class color) (min-colors 89)))
      ;;                                GUI     Term
      (nord0  (if (display-graphic-p) "#2e3440"  nil            ))
      (nord1  (if (display-graphic-p) "#3b4252"  "black"        ))
      (nord2  (if (display-graphic-p) "#434c5e"  "#434c5e"      ))
      (nord3  (if (display-graphic-p) "#4c566a"  "brightblack"  ))
      (nord4  (if (display-graphic-p) "#d8dee9"  "#d8dee9"      ))
      (nord5  (if (display-graphic-p) "#e5e9f0"  "#white"       ))
      (nord6  (if (display-graphic-p) "#eceff4"  "#brightwhite" ))
      (nord7  (if (display-graphic-p) "#8fbcbb"  "#cyan"        ))
      (nord8  (if (display-graphic-p) "#88c0d0"  "brightcyan"   ))
      (nord9  (if (display-graphic-p) "#81a1c1"  "blue"         ))
      (nord10 (if (display-graphic-p) "#5e81ac"  "brightblue"   ))
      (nord11 (if (display-graphic-p) "#bf616a"  "red"          ))
      (nord12 (if (display-graphic-p) "#d08770"  "brightyellow" ))
      (nord13 (if (display-graphic-p) "#ebcb8b"  "yellow"       ))
      (nord14 (if (display-graphic-p) "#a3be8c"  "green"        ))
      (nord15 (if (display-graphic-p) "#b48ead"  "magenta"      )))
      

  (custom-theme-set-faces
   'nord

   ;; Basic Faces
   `(default ((,class (:background ,nord0 :foreground ,nord4))))
   `(font-lock-builtin-face ((,class (:foreground ,nord9))))
   `(font-lock-comment-face ((,class (:foreground ,nord3))))
   `(font-lock-negation-char-face ((,class (:foreground ,nord4))))
   `(font-lock-reference-face ((,class (:foreground ,nord4))))
   `(font-lock-constant-face ((,class (:foreground ,nord4))))
   `(font-lock-doc-face ((,class (:foreground ,nord3))))
   `(font-lock-function-name-face ((,class (:foreground ,nord8 ))))
   `(font-lock-keyword-face ((,class (:bold ,class :foreground ,nord9))))
   `(font-lock-string-face ((,class (:foreground ,nord14))))
   `(font-lock-type-face ((,class (:foreground ,nord9 ))))
   `(font-lock-variable-name-face ((,class (:foreground ,nord4))))
   `(font-lock-warning-face ((,class (:foreground ,nord12 :background ,nord1))))
   `(region ((,class (:background ,nord14 :foreground ,nord0))))
   `(highlight ((,class (:foreground ,nord6 :background ,nord2))))
   `(hl-line ((,class (:background  ,nord2))))
   `(fringe ((,class (:background ,nord0 :foreground ,nord5))))
   `(cursor ((,class (:background ,nord6))))
   `(show-paren-match-face ((,class (:background ,nord12))))

   `(isearch ((,class (:bold t :foreground ,nord12 :background ,nord2))))
   `(isearch-fail ((,class (:foreground ,nord0 :background ,nord12))))
   `(mode-line ((,class (:foreground nil :background ,nord2 :box ,nord4))))
   `(mode-line-inactive ((,class (:foreground ,nord3 :background ,nord1 :box ,nord2))))
   `(vertical-border ((,class (:foreground ,nord1))))
   `(minibuffer-prompt ((,class (:bold t :foreground ,nord9))))
   `(default-italic ((,class (:italic t))))
   `(link ((,class (:foreground ,nord4 :underline t))))
   `(linum ((,class (:foreground ,nord3 :background ,nord0))))
   `(linum-relative-current-face ((,class (:foreground ,nord13 :background ,nord0))))

   ;; Org Mode
   `(outline-1 ((,class (:foreground ,nord14))))
   `(outline-2 ((,class (:foreground ,nord15))))
   `(outline-3 ((,class (:foreground ,nord8))))
   `(outline-4 ((,class (:foreground ,nord12))))
   `(outline-5 ((,class (:foreground ,nord13))))
   `(outline-6 ((,class (:foreground ,nord10))))
   `(org-agenda-structure ((,class (:foreground ,nord15))))
   `(org-agenda-date ((,class (:foreground ,nord8 :underline nil))))
   `(org-agenda-done ((,class (:foreground ,nord14))))
   `(org-agenda-dimmed-todo-face ((,class (:foreground ,nord3))))
   `(org-block ((,class (:foreground ,nord15))))
   `(org-block-background ((,class (:background ,nord1))))
   `(org-block-begin-line ((,class (:background ,nord3 :foreground ,nord13))))
   `(org-block-end-line ((,class (:background ,nord3 :foreground ,nord13))))
   `(org-checkbox ((,class (:foreground ,nord13))))
   `(org-checkbox-statistics-done ((,class (:foreground ,nord14))))
   `(org-checkbox-statistics-todo ((,class (:foreground ,nord13))))
   `(org-code ((,class (:foreground ,nord13))))
   `(org-column ((,class (:background ,nord3))))
   `(org-column-title ((,class (:inherit org-column :weight bold :underline t))))
   `(org-date ((,class (:foreground ,nord8 :underline t))))
   `(org-document-info ((,class (:foreground ,nord10))))
   `(org-document-info-keyword ((,class (:foreground ,nord14))))
   `(org-document-title ((,class (:weight bold :foreground ,nord15 :height 1.44))))
   `(org-done ((,class (:foreground ,nord14))))
   `(org-ellipsis ((,class (:foreground ,nord3))))
   `(org-footnote ((,class (:foreground ,nord10))))
   `(org-formula ((,class (:foreground ,nord3))))
   `(org-hide ((,class (:foreground ,nord0 :background ,nord0))))
   `(org-link ((,class (:foreground ,nord8 :underline t))))
   `(org-scheduled ((,class (:foreground ,nord14))))
   `(org-scheduled-previously ((,class (:foreground ,nord13))))
   `(org-scheduled-today ((,class (:foreground ,nord14))))
   `(org-special-keyword ((,class (:foreground ,nord13))))
   `(org-table ((,class (:foreground ,nord15))))
   `(org-todo ((,class (:foreground ,nord3))))
   `(org-upcoming-deadline ((,class (:foreground ,nord13))))
   `(org-warning ((,class (:weight bold :foreground ,nord3))))
   `(font-latex-bold-face ((,class (:foreground ,nord9))))
   `(font-latex-italic-face ((,class (:foreground ,nord4 :italic t))))
   `(font-latex-string-face ((,class (:foreground ,nord14))))
   `(font-latex-match-reference-keywords ((,class (:foreground ,nord4))))
   `(font-latex-match-variable-keywords ((,class (:foreground ,nord4))))
   `(ido-only-match ((,class (:foreground ,nord12))))
   `(org-sexp-date ((,class (:foreground ,nord5))))
   `(ido-first-match ((,class (:foreground ,nord9 :bold t))))
   `(ido-subdir ((,class (:foreground ,nord9))))

   ;;Markdown
   `(markdown-header-face-1 ((,class (:foreground ,nord14))))
   `(markdown-header-face-2 ((,class (:foreground ,nord15))))
   `(markdown-header-face-3 ((,class (:foreground ,nord8))))
   `(markdown-header-face-4 ((,class (:foreground ,nord12))))
   `(markdown-header-face-5 ((,class (:foreground ,nord13))))
   `(markdown-header-face-6 ((,class (:foreground ,nord10))))
   `(markdown-inline-code-face ((,class (:bold t :foreground ,nord5))))
   `(markdown-link-face ((,class (:foreground ,nord8))))
   
   ;; Git Gutter
   `(git-gutter:modified ((,class (:foreground ,nord13))))
   `(git-gutter:added ((,class (:foreground ,nord14))))
   `(git-gutter:deleted ((,class (:foreground ,nord11))))

   ;;Evil Mode Faces
   `(evil-ex-info ((,class (:foreground ,nord11 :italic t))))
   `(evil-ex-substitute-replacement ((,class (:foreground ,nord12 ))))
   `(evil-ex-substitute-matches ((,class (:background ,nord3 :foreground ,nord13))))

   ;;Gnus
   `(gnus-group-mail-1 ((,class (:foreground ,nord9 :bold t))))
   `(gnus-group-mail-1-empty ((,class (:inherit gnus-group-mail-1 :bold nil))))
   `(gnus-group-mail-2 ((,class (:foreground ,nord4 :bold t))))
   `(gnus-group-mail-2-empty ((,class (:inherit gnus-group-mail-2 :bold nil))))
   `(gnus-group-mail-3 ((,class (:foreground ,nord3 :bold t))))
   `(gnus-group-mail-3-empty ((,class (:inherit gnus-group-mail-3 :bold nil))))
   `(gnus-group-mail-low ((,class (:foreground ,nord0 :bold t))))
   `(gnus-group-mail-low-empty ((,class (:inherit gnus-group-mail-low :bold nil))))
   `(gnus-group-news-1 ((,class (:foreground ,nord9 :bold t))))
   `(gnus-group-news-1-empty ((,class (:inherit gnus-group-news-1 :bold nil))))
   `(gnus-group-news-2 ((,class (:foreground ,nord4 :bold t))))
   `(gnus-group-news-2-empty ((,class (:inherit gnus-group-news-2 :bold nil))))
   `(gnus-group-news-3 ((,class (:foreground ,nord3 :bold t))))
   `(gnus-group-news-3-empty ((,class (:inherit gnus-group-news-3 :bold nil))))
   `(gnus-group-news-low ((,class (:foreground ,nord0 :bold t))))
   `(gnus-group-news-low-empty ((,class (:inherit gnus-group-news-low :bold nil))))
   `(gnus-group-news-4 ((,class (:inherit gnus-group-news-low))))
   `(gnus-group-news-4-empty ((,class (:inherit gnus-group-news-low-empty))))
   `(gnus-group-news-5 ((,class (:inherit gnus-group-news-low))))
   `(gnus-group-news-5-empty ((,class (:inherit gnus-group-news-low-empty))))
   `(gnus-group-news-6 ((,class (:inherit gnus-group-news-low))))
   `(gnus-group-news-6-empty ((,class (:inherit gnus-group-news-low-empty))))
   `(gnus-header-content ((,class (:foreground ,nord9))))
   `(gnus-header-from ((,class (:foreground ,nord4))))
   `(gnus-header-name ((,class (:foreground ,nord9))))
   `(gnus-header-subject ((,class (:foreground ,nord8 :bold t))))
   `(gnus-summary-normal-unread ((,class (:foreground ,nord3 :weight normal))))
   `(gnus-summary-normal-read ((,class (:foreground ,nord0 :weight normal))))
   `(gnus-summary-normal-ticked ((,class (:foreground ,nord9 :weight light))))
   `(gnus-summary-selected ((,class (:inverse-video t))))
   `(gnus-summary-markup-face ((,class (:foreground ,nord4))))
   `(gnus-summary-normal-ancient ((,class (:inherit gnus-summary-normal-read))))
   `(spam ((,class (:inherit gnus-summary-normal-read :foreground ,nord12 :strike-through t :slant oblique))))

   ;;Mu4e
   `(mu4e-view-url-number-face ((,class (:foreground ,nord9))))
   `(mu4e-cited-1-face ((,class (:foreground ,nord5))))
   `(mu4e-cited-7-face ((,class (:foreground ,nord6))))
   `(mu4e-header-marks-face ((,class (:foreground ,nord9))))
   `(mu4e-title-face ((,class (:foreground ,nord14))))
   `(mu4e-header-key-face ((,class (:foreground ,nord13))))
   `(mu4e-highlight-face ((,class (:foreground ,nord10))))
   `(mu4e-flagged-face ((,class (:foreground ,nord12))))
   `(mu4e-unread-face ((,class (:foreground ,nord4))))
   `(mu4e-link-face ((,class (:foreground ,nord8))))
   
   `(ffap ((,class (:foreground ,nord5))))
   `(js2-private-function-call ((,class (:foreground ,nord4))))
   `(js2-jsdoc-html-tag-delimiter ((,class (:foreground ,nord12))))
   `(js2-jsdoc-html-tag-name ((,class (:foreground ,nord8))))
   `(js2-external-variable ((,class (:foreground ,nord9  ))))
   `(js2-function-param ((,class (:foreground ,nord4))))
   `(js2-jsdoc-value ((,class (:foreground ,nord14))))
   `(js2-private-member ((,class (:foreground ,nord6))))
   `(js3-warning-face ((,class (:underline ,nord9))))
   `(js3-error-face ((,class (:underline ,nord12))))
   `(js3-external-variable-face ((,class (:foreground ,nord4))))
   `(js3-function-param-face ((,class (:foreground ,nord4))))
   `(js3-jsdoc-tag-face ((,class (:foreground ,nord9))))
   `(js3-instance-member-face ((,class (:foreground ,nord4))))
   `(warning ((,class (:foreground ,nord12))))
   `(ac-completion-face ((,class (:underline t :foreground ,nord9))))
   `(info-quoted-name ((,class (:foreground ,nord9))))
   `(info-string ((,class (:foreground ,nord14))))
   `(icompletep-determined ((,class :foreground ,nord9)))
   `(undo-tree-visualizer-current-face ((,class :foreground ,nord9)))
   `(undo-tree-visualizer-default-face ((,class :foreground ,nord5)))
   `(undo-tree-visualizer-unmodified-face ((,class :foreground ,nord4)))
   `(undo-tree-visualizer-register-face ((,class :foreground ,nord9)))
   `(slime-repl-inputed-output-face ((,class (:foreground ,nord9))))

   ;; Whitespace
   `(whitespace-line ((,class :foreground ,nord15 )))
   `(whitespace-space ((,class :foreground ,nord13 :background ,nord0)))
   `(trailing-whitespace ((,class :foreground nil :background ,nord15)))
   `(whitespace-trailing ((,class :inherit trailing-whitespace)))

   ;;Rainbow Delimiters
   `(rainbow-delimiters-depth-1-face ((,class :foreground ,nord4)))
   `(rainbow-delimiters-depth-2-face ((,class :foreground ,nord10)))
   `(rainbow-delimiters-depth-3-face ((,class :foreground ,nord15)))
   `(rainbow-delimiters-depth-4-face ((,class :foreground ,nord7)))
   `(rainbow-delimiters-depth-5-face ((,class :foreground ,nord13)))
   `(rainbow-delimiters-depth-6-face ((,class :foreground ,nord14)))
   `(rainbow-delimiters-depth-7-face ((,class :foreground ,nord15)))
   `(rainbow-delimiters-depth-8-face ((,class :foreground ,nord10)))
   `(rainbow-delimiters-unmatched-face ((,class :foreground ,nord11)))

   ;;Magit
   `(magit-item-highlight ((,class :background ,nord2)))
   `(magit-section-heading        ((,class (:foreground ,nord9 :weight bold))))
   `(magit-hunk-heading           ((,class (:background ,nord2))))
   `(magit-section-highlight      ((,class (:background ,nord1))))
   `(magit-hunk-heading-highlight ((,class (:background ,nord2))))
   `(magit-diff-context-highlight ((,class (:background ,nord2 :foreground ,nord6))))
   `(magit-diffstat-added   ((,class (:foreground ,nord9))))
   `(magit-diffstat-removed ((,class (:foreground ,nord4))))
   `(magit-process-ok ((,class (:foreground ,nord8 :weight bold))))
   `(magit-process-ng ((,class (:foreground ,nord12 :weight bold))))
   `(magit-branch ((,class (:foreground ,nord4 :weight bold))))
   `(magit-log-author ((,class (:foreground ,nord6))))
   `(magit-hash ((,class (:foreground ,nord5))))
   `(magit-diff-file-header ((,class (:foreground ,nord5 :background ,nord2))))

   ;;Term
   `(lazy-highlight ((,class (:foreground ,nord5 :background ,nord2))))
   `(term ((,class (:foreground ,nord4 :background ,nord0))))
   `(term-color-black ((,class (:foreground ,nord2 :background ,nord2))))
   `(term-color-blue ((,class (:foreground ,nord8 :background ,nord8))))
   `(term-color-red ((,class (:foreground ,nord9 :background ,nord2))))
   `(term-color-green ((,class (:foreground ,nord9 :background ,nord2))))
   `(term-color-yellow ((,class (:foreground ,nord4 :background ,nord4))))
   `(term-color-magenta ((,class (:foreground ,nord9 :background ,nord9))))
   `(term-color-cyan ((,class (:foreground ,nord14 :background ,nord14))))
   `(term-color-white ((,class (:foreground ,nord5 :background ,nord5))))

   ;;Helm
   `(helm-header ((,class (:height 1.1 :foreground ,nord5 :background ,nord0 :underline nil :box nil))))
   `(helm-source-header ((,class (:height 1.44 :foreground ,nord9 :background ,nord2 :underline nil :weight bold))))
   `(helm-selection ((,class (:background ,nord1 :underline nil))))
   `(helm-selection-line ((,class (:background ,nord1))))
   `(helm-visible-mark ((,class (:foreground ,nord0 :background ,nord2))))
   `(helm-candidate-number ((,class (:foreground ,nord0 :background ,nord4))))
   `(helm-separator ((,class (:foreground ,nord9 :background ,nord0))))
   `(helm-time-zone-current ((,class (:foreground ,nord9 :background ,nord0))))
   `(helm-time-zone-home ((,class (:foreground ,nord9 :background ,nord0))))
   `(helm-buffer-not-saved ((,class (:foreground ,nord9 :background ,nord0))))
   `(helm-buffer-process ((,class (:foreground ,nord9 :background ,nord0))))
   `(helm-buffer-saved-out ((,class (:foreground ,nord4 :background ,nord0))))
   `(helm-buffer-size ((,class (:foreground ,nord4 :background ,nord0))))
   `(helm-ff-directory ((,class (:foreground ,nord8 :background ,nord0 :weight bold))))
   `(helm-ff-file ((,class (:foreground ,nord4 :background ,nord0 :weight normal))))
   `(helm-ff-executable ((,class (:foreground ,nord8 :background ,nord0 :weight normal))))
   `(helm-ff-invalid-symlink ((,class (:foreground ,nord4 :background ,nord0 :weight bold))))
   `(helm-ff-symlink ((,class (:foreground ,nord9 :background ,nord0 :weight bold))))
   `(helm-ff-prefix ((,class (:foreground ,nord0 :background ,nord9 :weight normal))))
   `(helm-grep-cmd-line ((,class (:foreground ,nord4 :background ,nord0))))
   `(helm-grep-file ((,class (:foreground ,nord4 :background ,nord0))))
   `(helm-grep-finish ((,class (:foreground ,nord5 :background ,nord0))))
   `(helm-grep-lineno ((,class (:foreground ,nord4 :background ,nord0))))
   `(helm-grep-match ((,class (:foreground nil :background nil :inherit helm-match))))
   `(helm-grep-running ((,class (:foreground ,nord8 :background ,nord0))))
   `(helm-moccur-buffer ((,class (:foreground ,nord8 :background ,nord0))))
   `(helm-source-go-package-godoc-description ((,class (:foreground ,nord14))))
   `(helm-bookmark-w3m ((,class (:foreground ,nord9))))

   ;;Company Mode
   `(company-echo-common ((,class (:foreground ,nord0 :background ,nord4))))
   `(company-preview ((,class (:background ,nord0 :foreground ,nord8))))
   `(company-preview-common ((,class (:foreground ,nord1 :foreground ,nord6))))
   `(company-preview-search ((,class (:foreground ,nord9 :background ,nord0))))
   `(company-scrollbar-bg ((,class (:background ,nord2))))
   `(company-scrollbar-fg ((,class (:foreground ,nord9))))
   `(company-tooltip ((,class (:foreground ,nord5 :background ,nord0 :bold t))))
   `(company-tooltip-annotation ((,class (:foreground ,nord4))))
   `(company-tooltip-common ((,class ( :foreground ,nord6))))
   `(company-tooltip-common-selection ((,class (:foreground ,nord14))))
   `(company-tooltip-mouse ((,class (:inherit highlight))))
   `(company-tooltip-selection ((,class (:background ,nord2 :foreground ,nord6))))
   `(company-template-field ((,class (:inherit region))))

   ;;Web Mode
   `(web-mode-builtin-face ((,class (:inherit ,font-lock-builtin-face))))
   `(web-mode-comment-face ((,class (:inherit ,font-lock-comment-face))))
   `(web-mode-constant-face ((,class (:inherit ,font-lock-constant-face))))
   `(web-mode-keyword-face ((,class (:foreground ,nord9))))
   `(web-mode-doctype-face ((,class (:inherit ,font-lock-comment-face))))
   `(web-mode-function-name-face ((,class (:inherit ,font-lock-function-name-face))))
   `(web-mode-string-face ((,class (:foreground ,nord14))))
   `(web-mode-type-face ((,class (:inherit ,font-lock-type-face))))
   `(web-mode-html-attr-name-face ((,class (:foreground ,nord8))))
   `(web-mode-html-attr-value-face ((,class (:foreground ,nord9))))
   `(web-mode-warning-face ((,class (:inherit ,font-lock-warning-face))))
   `(web-mode-html-tag-face ((,class (:foreground ,nord3))))
   `(web-mode-symbol-face ((,class (:foreground ,nord15))))

   
   `(jde-java-font-lock-package-face ((t (:foreground ,nord4))))
   `(jde-java-font-lock-public-face ((t (:foreground ,nord9))))
   `(jde-java-font-lock-private-face ((t (:foreground ,nord9))))
   `(jde-java-font-lock-constant-face ((t (:foreground ,nord4))))
   `(jde-java-font-lock-modifier-face ((t (:foreground ,nord4))))
   `(jde-java-font-lock-number-face ((t (:foreground ,nord4))))
   `(enh-ruby-op-face ((,class (:foreground ,nord9))))
   `(enh-ruby-heredoc-delimiter-face ((,class (:foreground ,nord14))))
   `(enh-ruby-string-delimiter-face ((,class (:foreground ,nord14))))
   `(enh-ruby-regexp-delimiter-face ((,class (:foreground ,nord14))))
   `(which-func ((,class (:inherit ,font-lock-function-name-face))))

   ;;Fountain Mode
   `(fountain-dialog ((, class (:foreground ,nord14))))
   `(fountain-trans ((, class (:foreground ,nord15))))

   ;;Powerline-Evil
   `(powerline-evil-base-face ((t (:foreground ,nord1))))
   `(powerline-evil-normal-face ((,class (:inherit powerline-evil-base-face :background ,nord14))))
   `(powerline-evil-insert-face ((,class (:inherit powerline-evil-base-face :background ,nord8))))
   `(powerline-evil-visual-face ((,class (:inherit powerline-evil-base-face :background ,nord15))))
   `(powerline-evil-operator-face ((,class (:inherit powerline-evil-base-face :background ,nord3))))
   `(powerline-evil-replace-face ((,class (:inherit powerline-evil-base-face :background ,nord11))))
   `(powerline-evil-motion-face ((,class (:inherit powerline-evil-base-face :background ,nord15))))
   `(powerline-evil-emacs-face ((,class (:inherit powerline-evil-base-face :background ,nord13))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'nord)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; nord-theme.el ends here
