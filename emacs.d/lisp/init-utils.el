;;; package --- Summary:

;;; Commentary:

;;; Code:

(provide 'init-utils)

;; Use abeiber's function to cycle the powerline seperators
(defun cycle-powerline-separators (&optional reverse)
  "Set Powerline separators in turn.  If REVERSE is not nil, go backwards."
 (interactive)
 (let* ((fn (if reverse 'reverse 'identity))
   (separators (funcall fn '("arrow" "arrow-fade" "slant"
                             "chamfer" "wave" "brace" "roundstub" "zigzag"
                             "butt" "rounded" "contour" "curve")))
   (found nil))
  (while (not found)
    (progn (setq separators (append (cdr separators) (list (car separators))))
    (when (string= (car separators) powerline-default-separator)
      (progn (setq powerline-default-separator (cadr separators))
         (setq found t)
          (redraw-display)))))))


(defun set-exec-path-from-shell-PATH ()
  "Set the 'exec-path' to the same value used by the user shell."
  (let ((path-from-shell
         (replace-regexp-in-string
          "[[:space:]\n]*$" ""
          (shell-command-to-string "$SHELL -l -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)


(defmacro diminish-minor-mode (filename mode &optional abbrev)
  "Supply a FILENAME, to hide a minor MODE or replace with an ABBREV."
  `(eval-after-load (symbol-name ,filename)
     '(diminish ,mode ,abbrev)))

(defmacro diminish-major-mode (mode-hook abbrev)
  "Supply a MODE-HOOK, to hide a major MODE or replace with an ABBREV."
  `(add-hook ,mode-hook
             (lambda () (setq mode-name ,abbrev))))


(defun search-my-notes (searchforthis)
  "Search for SEARCHFORTHIS."
  (interactive "sSearch Query: ")
  (rgrep searchforthis "*.txt"  "~/Dropbox/Notes"))

(eval-after-load "grep"
  '(grep-compute-defaults))

(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(defun andy-new-empty-buffer ()
  "Open a new empty buffer.
URL `http://ergoemacs.org/emacs/emacs_new_empty_buffer.html'
Version 2015-06-12"
  (interactive)
  (let ((ξbuf (generate-new-buffer "untitled")))
    (switch-to-buffer ξbuf)
    (funcall (and initial-major-mode))
    (setq buffer-offer-save t)))

(defun rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (if (get-buffer new-name)
        (message "A buffer named '%s' already exists!" new-name)
      (progn
        (when (file-exists-p filename)
         (rename-file filename new-name 1))
        (rename-buffer new-name)
(set-visited-file-name new-name)))))

(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (or (buffer-file-name) (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
(kill-this-buffer)))


(require 'htmlfontify)
(defun fontify-and-browse ()
  "Fontify the current buffer into HTML, write it to a temp file, and open it in a browser."
  (interactive)
  (let* ((fontified-buffer (hfy-fontify-buffer))
         (temp-file-name (make-temp-file "ff" nil ".html")))
    (with-current-buffer fontified-buffer
      (write-region (point-min) (point-max) temp-file-name))
    (browse-url (concat "file://" temp-file-name))))


(defun show-first-occurrence ()
  "Display the location of the word at point's first occurrence in the buffer."
  (interactive)
  (save-excursion
    (let ((search-word (thing-at-point 'symbol t)))
      (goto-char 1)
      (re-search-forward search-word)
      (message (concat
                "L" (number-to-string (line-number-at-pos)) ": "
                (replace-regexp-in-string
                 "[ \t\n]*\\'"
                 ""
                 (thing-at-point 'line t)
                 ))))))

(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun narrow-and-set-normal ()
  "Narrow to the region and, if in a visual mode, set normal mode."
  (interactive)
  (narrow-to-region (region-beginning) (region-end))
  (if (string= evil-state "visual")
      (progn (evil-normal-state nil)
(evil-goto-first-line))))

(provide 'init-utils)
;;; init-utils.el ends here
