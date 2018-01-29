;;; core.el --- the heart of the beast -*- lexical-binding: t; -*-

;;; Naming conventions:
;;
;;   doom-...   public variables or non-interactive functions
;;   doom--...  private anything (non-interactive), not safe for direct use
;;   doom/...   an interactive function; safe for M-x or keybinding
;;   doom//...  an interactive function for managing/maintaining Doom itself
;;   doom:...   an evil operator, motion or command
;;   doom|...   hook function
;;   doom*...   advising functions
;;   doom@...   a hydra command
;;   ...!       a macro or function that configures DOOM
;;   =...       an interactive command that starts an app module
;;   %...       functions used for in-snippet logic
;;   +...       Any of the above but part of a module, e.g. `+emacs-lisp|init-hook'
;;
;; Autoloaded functions are in core/autoload/*.el and modules/*/*/autoload.el or
;; modules/*/*/autoload/*.el.

(defvar doom-version "2.0.9"
  "Current version of DOOM emacs.")

(defvar doom-debug-mode (or (getenv "DEBUG") init-file-debug)
  "If non-nil, all doom functions will be verbose. Set DEBUG=1 in the command
line or use --debug-init to enable this.")

(defvar doom-emacs-dir (file-truename user-emacs-directory)
  "The path to this emacs.d directory.")

(defvar doom-core-dir (concat doom-emacs-dir "core/")
  "Where essential files are stored.")

(defvar doom-modules-dir (concat doom-emacs-dir "modules/")
  "Where configuration modules are stored.")

(defvar doom-local-dir (concat doom-emacs-dir ".local/")
  "Root directory for local Emacs files. Use this as permanent storage for files
that are safe to share across systems (if this config is symlinked across
several computers).")

(defvar doom-etc-dir (concat doom-local-dir "etc/")
  "Directory for non-volatile storage.

Use this for files that don't change much, like servers binaries, external
dependencies or long-term shared data.")

(defvar doom-cache-dir (concat doom-local-dir "cache/")
  "Directory for volatile storage.

Use this for files that change often, like cache files.")

(defvar doom-packages-dir (concat doom-local-dir "packages/")
  "Where package.el and quelpa plugins (and their caches) are stored.")

(defvar doom-autoload-file (concat doom-local-dir "autoloads.el")
  "Where `doom//reload-autoloads' will generate its autoloads file.")

(defgroup doom nil
  "DOOM Emacs, an Emacs configuration for a stubborn, shell-dwelling and
melodramatic ex-vimmer disappointed with the text-editor status quo."
  :group 'emacs)


;;;
;; UTF-8 as the default coding system
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))     ; pretty
(prefer-coding-system        'utf-8)   ; pretty
(set-terminal-coding-system  'utf-8)   ; pretty
(set-keyboard-coding-system  'utf-8)   ; pretty
(set-selection-coding-system 'utf-8)   ; perdy
(setq locale-coding-system   'utf-8)   ; please
(setq-default buffer-file-coding-system 'utf-8) ; with sugar on top

(setq-default
 ad-redefinition-action 'accept   ; silence advised function warnings
 apropos-do-all t                 ; make `apropos' more useful
 compilation-always-kill t        ; kill compilation process before starting another
 compilation-ask-about-save nil   ; save all buffers on `compile'
 compilation-scroll-output t
 confirm-nonexistent-file-or-buffer t
 enable-recursive-minibuffers nil
 debug-on-error (and (not noninteractive) doom-debug-mode)
 idle-update-delay 2              ; update ui less often
 load-prefer-newer (or noninteractive doom-debug-mode)
 ;; keep the point out of the minibuffer
 minibuffer-prompt-properties '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)
 ;; History & backup settings (save nothing, that's what git is for)
 auto-save-default nil
 create-lockfiles nil
 history-length 500
 make-backup-files nil
 ;; files
 abbrev-file-name             (concat doom-local-dir "abbrev.el")
 auto-save-list-file-name     (concat doom-cache-dir "autosave")
 backup-directory-alist       (list (cons "." (concat doom-cache-dir "backup/")))
 pcache-directory             (concat doom-cache-dir "pcache/")
 mc/list-file                 (concat doom-etc-dir "mc-lists.el")
 server-auth-dir              (concat doom-cache-dir "server/")
 shared-game-score-directory  (concat doom-etc-dir "shared-game-score/")
 tramp-auto-save-directory    (concat doom-cache-dir "tramp-auto-save/")
 tramp-backup-directory-alist backup-directory-alist
 tramp-persistency-file-name  (concat doom-cache-dir "tramp-persistency.el")
 url-cache-directory          (concat doom-cache-dir "url/")
 url-configuration-directory  (concat doom-etc-dir "url/"))

;; move custom defs out of init.el
(setq custom-file (concat doom-etc-dir "custom.el"))
(load custom-file t t)

;; be quiet at startup; don't load or display anything unnecessary
(unless noninteractive
  (advice-add #'display-startup-echo-area-message :override #'ignore)
  (setq inhibit-startup-message t
        inhibit-startup-echo-area-message user-login-name
        inhibit-default-init t
        initial-major-mode 'fundamental-mode
        initial-scratch-message nil
        mode-line-format nil))

;; Custom init hooks; clearer than `after-init-hook', `emacs-startup-hook', and
;; `window-setup-hook'.
(defvar doom-init-hook nil
  "A list of hooks run when DOOM is initialized, before `doom-post-init-hook'.")

(defvar doom-post-init-hook nil
  "A list of hooks run after DOOM initialization is complete, and after
`doom-init-hook'.")

(defun doom-try-run-hook (fn hook)
  "Runs a hook wrapped in a `condition-case-unless-debug' block; its objective
is to include more information in the error message, without sacrificing your
ability to invoke the debugger in debug mode."
  (condition-case-unless-debug ex
      (if noninteractive
          (quiet! (funcall fn))
        (funcall fn))
    ('error
     (lwarn hook :error
          "%s in '%s' -> %s"
          (car ex) fn (error-message-string ex))))
  nil)


;;;
;; Initialize
(eval-and-compile
  (defvar doom--file-name-handler-alist file-name-handler-alist)
  (unless (or after-init-time noninteractive)
    ;; One of the contributors to long startup times is the garbage collector,
    ;; so we up its memory threshold, temporarily. It is reset later in
    ;; `doom|finalize'.
    (setq gc-cons-threshold 402653184
          gc-cons-percentage 0.6
          file-name-handler-alist nil))

  (require 'cl-lib)
  (load (concat doom-core-dir "core-packages") nil t)
  (setq load-path (eval-when-compile (doom-initialize t)
                                     (doom-initialize-load-path t))
        doom--package-load-path (eval-when-compile doom--package-load-path))

  (load! core-lib)
  (load! core-os) ; consistent behavior across OSes
  (condition-case-unless-debug ex
      (require 'autoloads doom-autoload-file t)
    ('error
     (lwarn 'doom-autoloads :warning
            "%s in autoloads.el -> %s"
            (car ex) (error-message-string ex))))

  (unless noninteractive
    (load! core-ui)         ; draw me like one of your French editors
    (load! core-popups)     ; taming sudden yet inevitable windows
    (load! core-editor)     ; baseline configuration for text editing
    (load! core-projects)   ; making Emacs project-aware
    (load! core-keybinds))  ; centralized keybind system + which-key

  (defun doom|finalize ()
    "Run `doom-init-hook', `doom-post-init-hook' and reset `gc-cons-threshold',
`gc-cons-percentage' and `file-name-handler-alist'."
    (unless (or (not after-init-time) noninteractive)
      (dolist (hook '(doom-init-hook doom-post-init-hook))
        (run-hook-wrapped hook #'doom-try-run-hook hook)))

    ;; If you forget to reset this, you'll get stuttering and random freezes!
    (setq gc-cons-threshold 16777216
          gc-cons-percentage 0.1
          file-name-handler-alist doom--file-name-handler-alist)
    t)

  (add-hook! '(emacs-startup-hook doom-reload-hook)
    #'doom|finalize))


;;
;; Emacs fixes/hacks
;;

;; Automatic minor modes
(defvar doom-auto-minor-mode-alist '()
  "Alist mapping filename patterns to corresponding minor mode functions, like
`auto-mode-alist'. All elements of this alist are checked, meaning you can
enable multiple minor modes for the same regexp.")

(defun doom|enable-minor-mode-maybe ()
  "Check file name against `doom-auto-minor-mode-alist'."
  (when buffer-file-name
    (let ((name buffer-file-name)
          (remote-id (file-remote-p buffer-file-name))
          (alist doom-auto-minor-mode-alist))
      ;; Remove backup-suffixes from file name.
      (setq name (file-name-sans-versions name))
      ;; Remove remote file name identification.
      (when (and (stringp remote-id)
                 (string-match-p (regexp-quote remote-id) name))
        (setq name (substring name (match-end 0))))
      (while (and alist (caar alist) (cdar alist))
        (if (string-match-p (caar alist) name)
            (funcall (cdar alist) 1))
        (setq alist (cdr alist))))))
(add-hook 'find-file-hook #'doom|enable-minor-mode-maybe)

(defun doom*set-indirect-buffer-filename (orig-fn base-buffer name &optional clone)
  "In indirect buffers, `buffer-file-name' is nil, which can cause problems
with functions that require it (like modeline segments)."
  (let ((file-name (buffer-file-name base-buffer))
        (buffer (funcall orig-fn base-buffer name clone)))
    (when (and file-name buffer)
      (with-current-buffer buffer
        (unless buffer-file-name
          (setq buffer-file-name file-name
                buffer-file-truename (file-truename file-name)))))
    buffer))
(advice-add #'make-indirect-buffer :around #'doom*set-indirect-buffer-filename)

(defun doom*no-authinfo-for-tramp (orig-fn &rest args)
  "Don't look into .authinfo for local sudo TRAMP buffers."
  (let ((auth-sources (if (equal tramp-current-method "sudo") nil auth-sources)))
    (apply orig-fn args)))
(advice-add #'tramp-read-passwd :around #'doom*no-authinfo-for-tramp)

(provide 'core)
;;; core.el ends here
