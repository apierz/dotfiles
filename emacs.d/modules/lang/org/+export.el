;;; lang/org/+export.el -*- lexical-binding: t; -*-

;; I don't have any beef with org's built-in export system, but I do wish it
;; would export to a central directory, rather than `default-directory'. This is
;; because all my org files are usually in one place, and I want to be able to
;; refer back to old exports if needed.

(def-package! ox-pandoc
  :defer t
  :config
  (when (executable-find "pandoc")
    (push 'pandoc org-export-backends))
  (setq org-pandoc-options
        '((standalone . t)
          (mathjax . t)
          (parse-raw . t))))

;;
(after! org
  (add-transient-hook! #'org-export-dispatch (require 'ox-pandoc))

  (setq org-export-directory (expand-file-name ".export" +org-dir)
        org-export-backends '(ascii html latex md)
        org-export-with-toc t
        org-export-with-author t)

  ;; Always export to a central location
  (unless (file-directory-p org-export-directory)
    (make-directory org-export-directory t))
  (defun +org*export-output-file-name (args)
    "Return a centralized export location."
    (unless (nth 2 args)
      (setq args (append args (list org-export-directory))))
    args)
  (advice-add #'org-export-output-file-name
              :filter-args #'+org*export-output-file-name))
