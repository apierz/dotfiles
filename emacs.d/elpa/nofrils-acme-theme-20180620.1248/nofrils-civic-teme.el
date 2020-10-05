;;; nofrils-acme-theme.el --- Port of "No Frils Acme" Vim theme.

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(deftheme nofrils-civic
  "Port of No Frils Acme by Robert Melton with Civic Colors.")

(let ((background "#1f1f29")
      (foreground "#e1e2e7")
      (comment "#c7794a")
      (error "#d3222d")
      (fringe "#e1e2e7")
      (search "#32cf71")
      (selection "#736cb0")
      (status "#1da9a2"))

  (custom-theme-set-faces
   'nofrils-acme

   `(default ((t :background ,background :foreground ,foreground)))

   ;; Highlight only comments and errors.
   `(error ((t :background nil :foreground ,error)))
   `(font-lock-builtin-face ((t nil)))
   `(font-lock-comment-face ((t :foreground ,comment)))
   `(font-lock-constant-face ((t nil)))
   `(font-lock-function-name-face ((t nil)))
   `(font-lock-keyword-face ((t nil)))
   `(font-lock-negation-char-face ((t nil)))
   `(font-lock-regexp-grouping-backslash ((t nil)))
   `(font-lock-regexp-grouping-construct ((t nil)))
   `(font-lock-string-face ((t nil)))
   `(font-lock-type-face ((t nil)))
   `(font-lock-variable-name-face ((t nil)))

   ;; Show searches and selections.
   `(isearch ((t :background ,search :foreground ,foreground)))
   `(lazy-highlight ((t :background ,foreground :foreground ,background)))
   `(region ((t :background ,selection)))

   ;; Parenthesis matching is never wrong.
   `(show-paren-match ((t :weight bold)))
   `(show-paren-mismatch ((t :background ,error :weight bold)))

   ;; Decorate the frame to resemble Acme.
   `(fringe ((t :background ,fringe)))
   `(minibuffer-prompt ((t :foreground ,foreground)))
   `(mode-line ((t :background ,status)))
   `(mode-line-inactive ((t :background ,fringe)))

   ;; Org mode needs to chill.
   `(org-done ((t :foreground ,search :weight bold)))
   `(org-agenda-date ((t :foreground ,fringe :weight bold)))
   `(org-document-info ((t :foreground ,status :weight bold)))
   `(org-agenda-structure ((t :foreground ,selection :weight bold)))
   `(org-todo ((t :foreground ,error :weight bold)))))

;;; Footer:

;;;###autoload
(when load-file-name
  (add-to-list
   'custom-theme-load-path
   (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'nofrils-civic)

(provide 'nofrils-civic-theme)

;;; nofrils-civic-theme.el ends here
