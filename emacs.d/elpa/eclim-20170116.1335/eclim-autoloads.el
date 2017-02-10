;;; eclim-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "eclim" "eclim.el" (22680 63791 0 0))
;;; Generated autoloads from eclim.el

(autoload 'eclim/workspace-dir "eclim" "\


\(fn)" nil nil)

(autoload 'eclim-mode "eclim" "\
An interface to the Eclipse IDE.

\(fn &optional ARG)" t nil)

(defvar global-eclim-mode nil "\
Non-nil if Global Eclim mode is enabled.
See the `global-eclim-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-eclim-mode'.")

(custom-autoload 'global-eclim-mode "eclim" nil)

(autoload 'global-eclim-mode "eclim" "\
Toggle Eclim mode in all buffers.
With prefix ARG, enable Global Eclim mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Eclim mode is enabled in all buffers where
`eclim--enable-for-accepted-files-in-project' would do it.
See `eclim-mode' for more information on Eclim mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "eclim-common" "eclim-common.el" (22680 63791
;;;;;;  0 0))
;;; Generated autoloads from eclim-common.el

(defvar eclim-mode-map (let ((map (make-sparse-keymap))) (define-key map (kbd "M-TAB") 'eclim-complete) map) "\
The keymap used in `eclim-mode'.")

;;;***

;;;### (autoloads nil "eclim-project" "eclim-project.el" (22680 63791
;;;;;;  0 0))
;;; Generated autoloads from eclim-project.el

(autoload 'eclim-project-mode "eclim-project" "\
Manage all your eclim projects in one buffer.

\\{eclim-project-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads nil "eclimd" "eclimd.el" (22680 63791 0 0))
;;; Generated autoloads from eclimd.el

(autoload 'start-eclimd "eclimd" "\
Start the eclimd process and optionally wait for it to be ready.
This will ask for a workspace directory, and it will attempt to
start eclimd program with the entered workspace directory. The
configurable variable `eclimd-default-workspace' controls the
default value of this directory. After having started the deamon,
it will block until eclimd is ready to receive commands since
otherwise those would fail. You can modify
`eclimd-wait-for-process' to prevent this command from
blocking. To stop the started process and you should use
`stop-eclimd'.

\(fn WORKSPACE-DIR &optional CALLBACK)" t nil)

;;;***

;;;### (autoloads nil nil ("eclim-ant.el" "eclim-completion.el" "eclim-debug.el"
;;;;;;  "eclim-java-run.el" "eclim-java.el" "eclim-macros.el" "eclim-maven.el"
;;;;;;  "eclim-pkg.el" "eclim-problems.el" "eclim-scala.el") (22680
;;;;;;  63791 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; eclim-autoloads.el ends here
