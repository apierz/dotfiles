;;; evil-multiedit-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "evil-multiedit" "evil-multiedit.el" (22786
;;;;;;  31385 0 0))
;;; Generated autoloads from evil-multiedit.el

(autoload 'evil-multiedit-restore "evil-multiedit" "\
Restore the last group of multiedit regions.

\(fn)" t nil)

(autoload 'evil-multiedit-match-all "evil-multiedit" "\
Highlight all matches of the current selection (or symbol under pointer) as
multiedit regions.

\(fn)" t nil)
 (autoload 'evil-multiedit-match-symbol-and-next "evil-multiedit" nil t)
 (autoload 'evil-multiedit-match-symbol-and-prev "evil-multiedit" nil t)

(autoload 'evil-multiedit-toggle-marker-here "evil-multiedit" "\
Toggle an arbitrary multiedit region at point.

\(fn)" t nil)
 (autoload 'evil-multiedit-match-and-next "evil-multiedit" nil t)
 (autoload 'evil-multiedit-match-and-prev "evil-multiedit" nil t)

(autoload 'evil-multiedit-toggle-or-restrict-region "evil-multiedit" "\
If in visual mode, restrict the multiedit regions to the selected region.
i.e. disable all regions outside the selection. If in any other mode, toggle the
multiedit region beneath the cursor, if one exists.

\(fn &optional BEG END)" t nil)

(autoload 'evil-multiedit-next "evil-multiedit" "\
Jump to the next multiedit region.

\(fn)" t nil)

(autoload 'evil-multiedit-prev "evil-multiedit" "\
Jump to the previous multiedit region.

\(fn)" t nil)

(autoload 'evil-multiedit-abort "evil-multiedit" "\
Clear all multiedit regions, clean up and revert to normal state.

\(fn &optional INHIBIT-NORMAL)" t nil)
 (autoload 'evil-multiedit-ex-match "evil-multiedit" nil t)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; evil-multiedit-autoloads.el ends here
