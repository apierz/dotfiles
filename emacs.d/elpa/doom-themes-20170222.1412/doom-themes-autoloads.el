;;; doom-themes-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "doom-themes" "doom-themes.el" (22702 8286
;;;;;;  0 0))
;;; Generated autoloads from doom-themes.el

(autoload 'doom-brighten-minibuffer "doom-themes" "\


\(fn)" nil nil)

(autoload 'doom-buffer-mode "doom-themes" "\
Brighten source buffers by remapping common faces (like default, hl-line and
linum) to their doom-theme variants.

\(fn &optional ARG)" t nil)

(autoload 'doom-buffer-mode-maybe "doom-themes" "\
Enable `doom-buffer-mode' in the current buffer, if it isn't already and the
buffer represents a real file.

\(fn)" nil nil)

(when (and (boundp 'custom-theme-load-path) load-file-name) (add-to-list 'custom-theme-load-path (file-name-as-directory (file-name-directory load-file-name))))

;;;***

;;;### (autoloads nil nil ("doom-molokai-theme.el" "doom-neotree.el"
;;;;;;  "doom-nlinum.el" "doom-one-light-theme.el" "doom-one-theme.el"
;;;;;;  "doom-themes-pkg.el") (22702 8286 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; doom-themes-autoloads.el ends here
