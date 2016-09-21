;;; evil-surround-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "evil-surround" "evil-surround.el" (22489 41309
;;;;;;  0 0))
;;; Generated autoloads from evil-surround.el
 (autoload 'evil-surround-delete "evil-surround" nil t)
 (autoload 'evil-surround-change "evil-surround" nil t)

(autoload 'evil-surround-mode "evil-surround" "\
Buffer-local minor mode to emulate surround.vim.

\(fn &optional ARG)" t nil)

(autoload 'turn-on-evil-surround-mode "evil-surround" "\
Enable evil-surround-mode in the current buffer.

\(fn)" nil nil)

(autoload 'turn-off-evil-surround-mode "evil-surround" "\
Disable evil-surround-mode in the current buffer.

\(fn)" nil nil)

(defvar global-evil-surround-mode nil "\
Non-nil if Global Evil-Surround mode is enabled.
See the `global-evil-surround-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-evil-surround-mode'.")

(custom-autoload 'global-evil-surround-mode "evil-surround" nil)

(autoload 'global-evil-surround-mode "evil-surround" "\
Toggle Evil-Surround mode in all buffers.
With prefix ARG, enable Global Evil-Surround mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Evil-Surround mode is enabled in all buffers where
`turn-on-evil-surround-mode' would do it.
See `evil-surround-mode' for more information on Evil-Surround mode.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; evil-surround-autoloads.el ends here
