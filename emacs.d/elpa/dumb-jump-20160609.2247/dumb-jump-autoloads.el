;;; dumb-jump-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "dumb-jump" "dumb-jump.el" (22395 50631 0 0))
;;; Generated autoloads from dumb-jump.el

(defvar dumb-jump-mode-map (let ((map (make-sparse-keymap))) (define-key map (kbd "C-M-g") 'dumb-jump-go) (define-key map (kbd "C-M-p") 'dumb-jump-back) (define-key map (kbd "C-M-q") 'dumb-jump-quick-look) map))

(defvar dumb-jump-mode nil "\
Non-nil if Dumb-Jump mode is enabled.
See the command `dumb-jump-mode' for a description of this minor mode.")

(custom-autoload 'dumb-jump-mode "dumb-jump" nil)

(autoload 'dumb-jump-mode "dumb-jump" "\
Minor mode for jumping to variable and function definitions

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; dumb-jump-autoloads.el ends here
