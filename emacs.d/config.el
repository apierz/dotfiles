(setq user-full-name "Andy Pierz"
      user-mail-address "andy@andypierz.com")

(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" .
"http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)

(add-to-list 'load-path (expand-file-name "snippets" user-emacs-directory))
(add-to-list 'load-path "~/.emacs.d/plugins")
(add-to-list 'load-path "~/.emacs.d/plugins/evil-org-mode")
(add-to-list 'load-path "/usr/local/Cellar/mu/HEAD-1f232b6/bin/mu")
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'load-path "/Users/Andy/Documents/Programming_Projects/dracula-theme/emacs/")
(add-to-list 'load-path "/Users/Andy/Documents/Programming_Projects/nord-theme/")

(require 'use-package)

(setq inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(when (boundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(show-paren-mode nil)
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(setq-default left-fringe-width nil)
(setq-default indent-tabs-mode nil)
;; (eval-after-load "vc" '(setq vc-handled-backends nil))
;; (setq vc-follow-symlinks t)
(setq large-file-warning-threshold nil)
(setq split-width-threshold nil)
(setq custom-safe-themes t)
(put 'narrow-to-region 'disabled nil)
(setq global-visual-line-mode t)
(setq word-wrap t)
(setq initial-major-mode 'org-mode)
(setq initial-scratch-message "")
(setq-default fill-column 80)
(setq-default tab-width 2)
(put 'dired-find-alternate-file 'disabled nil)

(setq frame-title-format
  '("" invocation-name ": "(:eval (if (buffer-file-name)
                (abbreviate-file-name (buffer-file-name))
                  "%b"))))

(setq backup-directory-alist '(("." . "~/Dropbox/emacs_backups"))
      backup-by-copying      t  ; Don't de-link hard links
      version-control        t  ; Use version numbers on backups
      delete-old-versions    t  ; Automatically delete excess backups:
      kept-new-versions      5  ; how many of the newest versions to keep
      kept-old-versions      5) ; and how many of the old

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
      "Open a new empty buffer."
      (interactive)
      (let ((ξbuf (generate-new-buffer "untitled")))
        (switch-to-buffer ξbuf)
        (funcall (and initial-major-mode))
        (setq buffer-offer-save t)))

(defun region-to-hexcol ()
  (interactive)
  (let
      ((start (region-beginning))
       (end (region-end))
       (text))

    (setq text (buffer-substring-no-properties start end))

    (when (string-match "^[[:digit:]]+$" text)
      (setq text (format "%02x" (string-to-number text)))
      (delete-region start end)
      (insert text))))

(defun rgb-to-hex ()
  (interactive)

  (let
      ((start (region-beginning))
       (end (region-end)))

    (goto-char start)
    (set-mark start)
    (skip-chars-forward "0-9")
    (region-to-hexcol)

    (skip-chars-forward ", ")
    (set-mark (point))
    (skip-chars-forward "0-9")
    (region-to-hexcol)

    (skip-chars-forward ", ")
    (set-mark (point))
    (skip-chars-forward "0-9")
    (region-to-hexcol)

    (setq end (point))
    (goto-char start)

    (save-restriction
      (narrow-to-region start end)
      (while (re-search-forward "[, ]" nil t) (replace-match "" nil t)))))


;; recursively find .org files in provided directory
;; modified from an Emacs Lisp Intro example
(defun sa-find-org-file-recursively (&optional directory filext)
  "Return .org and .org_archive files recursively from DIRECTORY.
If FILEXT is provided, return files with extension FILEXT instead."
  (interactive "DDirectory: ")
  (let* (org-file-list
         (case-fold-search t)         ; filesystems are case sensitive
         (file-name-regex "^[^.#].*") ; exclude dot, autosave, and backupfiles
         (filext (or filext "org$\\\|org_archive"))
         (fileregex (format "%s\\.\\(%s$\\)" file-name-regex filext))
         (cur-dir-list (directory-files directory t file-name-regex)))
    ;; loop over directory listing
    (dolist (file-or-dir cur-dir-list org-file-list) ; returns org-file-list
      (cond
       ((file-regular-p file-or-dir)             ; regular files
        (if (string-match fileregex file-or-dir) ; org files
            (add-to-list 'org-file-list file-or-dir)))
       ((file-directory-p file-or-dir)
        (dolist (org-file (sa-find-org-file-recursively file-or-dir filext)
                          org-file-list) ; add files found to result
          (add-to-list 'org-file-list org-file)))))))

(require 'nofrils-acme-theme)
(load-theme 'nofrils-acme t)

      (set-face-attribute 'default nil
                      :family "SF Mono" :height 120)

      (mac-auto-operator-composition-mode)

      ;; (use-package doom-neotree
      ;;   :config
      ;;   (setq doom-neotree-enable-file-icons 'simple)
      ;;   (setq doom-neotree-enable-dir-icons t)
      ;;   (setq doom-neotree-enable-dir-chevrons t)
      ;;   (setq doom-neotree-line-spacing 2))

      ;; (use-package spacemacs-theme)
      ;; (load-theme 'spacemacs-light t)

      ;; (use-package civic-theme)
      ;; (load-theme 'civic t)

(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8)

(diminish-minor-mode 'auto-complete 'auto-complete-mode " ⓐ ")
(diminish-minor-mode 'flycheck 'flycheck-mode " ⓕ ")
(diminish-minor-mode 'projectile 'projectile-mode " ⓟ ")
(diminish-minor-mode 'robe 'robe-mode " ⓡ ")
(diminish-minor-mode 'flymake 'flymake-mode " ⓜ ")
(diminish-minor-mode 'server 'server-mode)
(diminish-minor-mode 'evil-snipe 'evil-snipe-local-mode)
(diminish-minor-mode 'evil-surround 'evil-surround-mode )
(diminish-minor-mode 'evil-commentary 'evil-commentary-mode)
(diminish-minor-mode 'yasnippet 'yas-minor-mode)
(diminish-minor-mode 'autorevert 'auto-revert-mode)
(diminish-minor-mode 'flyspell 'flyspell-mode)
(diminish-minor-mode 'undo-tree 'undo-tree-mode)
(diminish-minor-mode 'evil-org 'evil-org-mode)
(diminish-minor-mode 'git-gutter 'git-gutter-mode)
(diminish-minor-mode 'company 'company-mode)
(diminish-minor-mode 'doom-buffer 'doom-buffer-mode)
(diminish-minor-mode 'rainbow 'rainbow-mode)

(diminish-major-mode 'emacs-lisp-mode-hook ".el")
(diminish-major-mode 'haskell-mode-hook "?=")
(diminish-major-mode 'lisp-interaction-mode-hook "?")
(diminish-major-mode 'python-mode-hook ".py")
(diminish-major-mode 'ruby-mode-hook ".rb")
(diminish-major-mode 'sh-mode-hook ".sh")
(diminish-major-mode 'markdown-mode-hook ".md")

(use-package evil)
(evil-mode t)

(use-package evil-leader)
(use-package evil-surround
  :config
  (global-evil-surround-mode 1))
(use-package evil-commentary
  :config
  (evil-commentary-mode))
(use-package evil-snipe
  :config
  (evil-snipe-mode 1)
  (evil-snipe-override-mode 1))
(use-package evil-numbers
:config
(define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt))

(use-package key-chord
  :config
  (key-chord-mode 1))

(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-define evil-insert-state-map ",," "<")
(key-chord-define evil-insert-state-map ".." ">")
(key-chord-define evil-insert-state-map "[[" "[ ]")
(key-chord-define evil-replace-state-map "jk" 'evil-normal-state)
(key-chord-define evil-visual-state-map "jk" 'evil-normal-state)
(key-chord-define evil-motion-state-map "jk" 'evil-normal-state)

(key-chord-define-global "jk" 'evil-normal-state)

(evil-define-key 'normal dired-mode-map "h" 'dired-up-directory)
(evil-define-key 'normal dired-mode-map "l" 'dired-find-alternate-file)
(evil-define-key 'normal dired-mode-map "v" 'dired-toggle-marks)
(evil-define-key 'normal dired-mode-map "m" 'dired-mark)
(evil-define-key 'normal dired-mode-map "u" 'dired-unmark)
(evil-define-key 'normal dired-mode-map "U" 'dired-unmark-all-marks)
(evil-define-key 'normal dired-mode-map "c" 'dired-create-directory)
(evil-define-key 'normal dired-mode-map "n" 'evil-search-next)
(evil-define-key 'normal dired-mode-map "N" 'evil-search-previous)
(evil-define-key 'normal dired-mode-map "q" 'kill-this-buffer)
(setq evil-shift-width 2)

(use-package evil-org)
(evil-define-key 'normal evil-org-mode-map (kbd "M-k") 'org-metaup)
(evil-define-key 'normal evil-org-mode-map (kbd "M-h") 'org-metaleft)
(evil-define-key 'normal evil-org-mode-map (kbd "M-j") 'org-metadown)
(evil-define-key 'normal evil-org-mode-map (kbd "M-l") 'org-metaright)
(evil-define-key 'normal evil-org-mode-map (kbd "M-K") 'org-shiftmetaup)
(evil-define-key 'normal evil-org-mode-map (kbd "M-H") 'org-shiftmetaleft)
(evil-define-key 'normal evil-org-mode-map (kbd "M-J") 'org-shiftmetadown)
(evil-define-key 'normal evil-org-mode-map (kbd "M-L") 'org-shiftmetaright)
(evil-define-key 'normal evil-org-mode-map (kbd "K") 'org-shiftup)
(evil-define-key 'normal evil-org-mode-map (kbd "H") 'org-shiftleft)
(evil-define-key 'normal evil-org-mode-map (kbd "J") 'org-shiftdown)
(evil-define-key 'normal evil-org-mode-map (kbd "L") 'org-shiftright)

(defun andy--config-evil-leader ()
  "Configure evil leader mode."
  (evil-leader/set-leader ",")
  (setq evil-leader/in-all-states 1)
  (evil-leader/set-key
    "k"  'switch-to-previous-buffer
    "m"  'previous-buffer
    "."  'next-buffer
    ":"  'eval-expression
    "d"  'delete-window
    "e"  'find-file
    "f"  'fontify-and-browse
    "h"  'split-window-vertically
    "p"  'cycle-powerline-separators
    "b"  'helm-bookmarks
    "l"  'whitespace-mode       ;; Show invisible characters
    "nn" 'narrow-and-set-normal ;; Narrow to region and enter normal mode
    "nw" 'widen
    "1"  'delete-other-windows  ;; C-w o
    "r"  'rainbow-mode ;; show hexcodes in their actual color
    "S"  'delete-trailing-whitespace
    "t"  'gtags-reindex
    "T"  'gtags-find-tag
    "v"  'split-window-horizontally
    "w"  'save-buffer
    "x"  'helm-M-x))

(global-evil-leader-mode)
(andy--config-evil-leader)

(defun pbcopy ()
  "Use OSX' pasteboard for copying."
  (interactive)
  (call-process-region (point) (mark) "pbcopy")
  (setq deactivate-mark t))

(defun pbpaste ()
  "Use OSX' pasteboard for pasting."
  (interactive)
  (call-process-region (point) (if mark-active (mark) (point)) "pbpaste" t t))

(defun pbcut ()
  "Use OSX' pasteboard for cutting."
  (interactive)
  (pbcopy)
  (delete-region (region-beginning) (region-end)))

(global-set-key (kbd "M-c") 'pbcopy)
(global-set-key (kbd "C-c x") 'pbcut)
(global-set-key (kbd "M-v") 'pbpaste)

(defun mac-switch-meta nil
  "Switch meta between Option and Command."
  (interactive)
  (if (eq mac-option-modifier nil)
      (progn
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'hyper)
)
    (progn
      (setq mac-option-modifier nil)
      (setq mac-command-modifier 'meta))))

(setq evil-move-cursor-back nil)

(global-set-key [f1] 'mu4e)
(global-set-key [f2] 'andy-new-empty-buffer)

(global-set-key [f4] 'fci-mode)
(global-set-key [f5] 'search-my-notes)
(global-set-key [f6] 'display-line-numbers-mode)

(use-package neotree)
(global-set-key [f8] 'neotree-toggle)

(global-set-key (kbd "M-j") 'robe-jump)

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-motion-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key evil-emacs-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape]'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

(setq evil-normal-state-modes (append evil-motion-state-modes
  evil-normal-state-modes))

(add-hook 'neotree-mode-hook
           (lambda ()
             (define-key evil-normal-state-local-map (kbd "h") 'neotree-enter-horizontal-split)
             (define-key evil-normal-state-local-map (kbd "v") 'neotree-enter-vertical-split)
             (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
             (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
             (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
             (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))

(setq evil-default-cursor (face-attribute 'cursor :background nil t)
      evil-normal-state-cursor 'box
      evil-emacs-state-cursor  `(,(face-attribute 'warning :foreground nil nil) box)
      evil-insert-state-cursor 'bar
      evil-visual-state-cursor 'hollow)

(use-package helm)
(use-package helm-config)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-x r b") 'helm-bookmarks)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(setq helm-split-window-in-side-p t)

(with-eval-after-load
  'helm (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
     (define-key helm-map (kbd "ESC") 'helm-keyboard-quit)
)

(use-package org)
(use-package ox)
(use-package org-grep)
(use-package org-capture)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-export-coding-system 'utf-8)
(setq org-agenda-files '("~/Dropbox/Notes"))
(setq org-agenda-text-search-extra-files
    (append (sa-find-org-file-recursively "~/Dropbox/Notes" "txt")
            (sa-find-org-file-recursively "~/Dropbox/Notes" "org")))


(add-hook 'find-file-hooks
  (lambda ()
    (let ((file (buffer-file-name)))
    (when (and file (equal (file-name-directory file) "~/Dropbox/Notes"))
    (org-mode)))))

(add-to-list 'org-latex-classes
             '("article"
               "\\documentclass{article}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(setq org-todo-keywords
  '((sequence "TODO(t)" "ONDECK(o)" "WAITING(w)" "SOMEDAY(s)" "CURRENT(c)" "|" "DONE(d)")))

 ;; For Dracula Theme
 (setq org-todo-keyword-faces
   '(("ONDECK"  . (:foreground "#c77a4b" :weight bold))
     ("TODO"    . (:foreground "#746db0" :weight bold))
     ("WAITING" . (:foreground "#7f7f7f" :weight bold))
     ("CURRENT" . (:foreground "#d3232e" :weight bold))
     ("DONE"    . (:foreground "#32cf72" :weight bold))
     ("SOMEDAY" . (:foreground "#5124e3" :weight bold))))

(setq org-hide-leading-stars t)
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(defface org-checkbox-todo-text
    '((t (:inherit nil)))
    "Face for the text part of an unchecked org-mode checkbox.")

(font-lock-add-keywords
 'org-mode
 `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?: \\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)" 1 'org-checkbox-todo-text prepend))
 'append)

(defface org-checkbox-done-text
    '((t (:inherit 'shadow :bold nil)))
    "Face for the text part of a checked org-mode checkbox.")

(font-lock-add-keywords
 'org-mode
 `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)" 1 'org-checkbox-done-text prepend))
 'append)

;; (custom-set-faces '(org-checkbox ((t (:foreground nil :strike-through nil)))))

(setq org-ellipsis "…")
(setq
      ;; org-fontify-whole-heading-line t
      org-fontify-done-headline t
      org-fontify-quote-and-verse-blocks t)

(setq org-cycle-separator-lines 0)

(setq org-startup-truncated nil)

(defun capture-report-date-file (path)
  (let ((name (read-string "Name: ")))
    (expand-file-name (format "%s.txt" name) path)))

(setq org-capture-templates
  '(
    ("t" "TODO" entry (file+headline "~/Dropbox/Notes/todo.txt" "Inbox")
     "** TODO %^{prompt}\n%U\n")
    ("n" "New Note" entry (file (capture-report-date-file "~/Dropbox/Notes/"))
     "** %^{prompt}\n %a\n%U\n")
    ("k" "Kill Ring Note" entry (file (capture-report-date-file "~/Dropbox/Notes"))
     "** %c\n %? %a\n %U\n")))

(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-src-window-setup 'current-window)
(setq org-confirm-babel-evaluate nil)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (ruby . t)
   (dot . t)
   (gnuplot . t)))

;; Set to the location of your Org files on your local system
(setq org-directory "~/Dropbox/Notes")
;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull "~/Dropbox/Notes/flagged.org")
;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")

(use-package yasnippet
  :ensure t
  :defer t
  :config
  (yas-reload-all)
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"
                           "~/.emacs.d/remote-snippets"))
  (setq tab-always-indent 'complete)
  (setq yas-prompt-functions '(yas-completing-prompt
                               yas-ido-prompt
                               yas-dropdown-prompt))
(define-key yas-minor-mode-map (kbd "<escape>") 'yas-exit-snippet))

(setq path-to-ctags "/usr/bin/ctags")
(defun create-tags (dir-name)
    "Create tags file."
    (interactive "DDirectory: ")
    (shell-command
        (format "%s -f TAGS -e -R %s" path-to-ctags (directory-file-name dir-name)))
)


(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(require 'company-web-html)
(add-to-list 'company-backends 'company-elisp)
(add-to-list 'company-backends 'company-yasnippet)
(add-to-list 'company-backends 'company-dabbrev)
(add-to-list 'company-backends 'company-dabbrev-code)

(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)
(setq display-line-numbers "%4d \u2502 ")

(set-face-attribute 'line-number-current-line nil :weight 'bold)

(use-package smooth-scrolling
  :config
  (smooth-scrolling-mode 1))

(use-package fill-column-indicator)
(use-package unbound)
(use-package nnir)

;; (add-hook 'emacs-lisp-mode-hook
;;           (lambda ()
;;             (rainbow-delimiters-mode)))

(setq python-indent-offset 2)
(setq flycheck-python-pycompile-executable "python3")
(setq python-shell-interpreter "python3")
(setq python-shell-native-complete nil)

(add-hook 'python-mode-hook
 (lambda ()
   (flycheck-mode)
   (yas-minor-mode)))

(add-hook 'sh-mode-hook
          (lambda ()
            (setq sh-basic-offset 2
                  sh-indentation 2)))

(add-hook 'ruby-mode-hook
  (lambda ()
    (setq ruby-insert-encoding-magic-comment nil)
      (yas-minor-mode)
      (robe-mode)
      (local-set-key "\r" 'newline-and-indent)
      (flymake-mode)
      (flymake-ruby-load)
      (define-key ruby-mode-map (kbd "C-c C-c") 'xmp)
      (define-key ruby-mode-map (kbd "C-c C-s") 'inf-ruby)
      (define-key ruby-mode-map (kbd "C-c C-r") 'ruby-send-region)
      (define-key ruby-mode-map (kbd "C-c C-z") 'ruby-switch-to-inf)
      (define-key ruby-mode-map (kbd "C-c C-l") 'ruby-load-file)
      (define-key ruby-mode-map (kbd "C-c C-b") 'ruby-send-block)
))
(add-to-list 'auto-mode-alist
  '("\\.\\(?:erb\\)\\'" . web-mode))

(add-to-list 'auto-mode-alist
  '("\\.\\(?:cap\\|gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode))
(add-to-list 'auto-mode-alist
  '("\\(?:Brewfile\\|Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . ruby-mode))

(use-package web-mode
  :ensure t
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.twig$" . web-mode))
  (setq web-mode-attr-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-indent-style 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-sql-indent-offset 2))

;; (add-to-list 'load-path "~/gocode/src/github.com/dougm/goflymake")
;; (require 'go-flymake)

(use-package projectile)
(use-package helm-projectile)

(add-hook 'ruby-mode-hook 'projectile-mode)
(add-hook 'web-mode-hook 'projectile-mode)
(setq projectile-indexing-method 'alien)
(setq projectile-switch-project-action 'projectile-find-file)
(setq projectile-completion-system 'default)
(setq projectile-enable-caching nil)

(helm-projectile-on)

;; (set-face-attribute 'helm-source-header nil :foreground "#ffb86c" :height 1.66)

(use-package magit)
(use-package evil-magit)

(require 'epa-file)
(epa-file-enable)

(require 'doom-modeline)
(doom-modeline-mode 1)

;; How tall the mode-line should be (only respected in GUI Emacs).
(setq doom-modeline-height 25)

;; How wide the mode-line bar should be (only respected in GUI Emacs).
(setq doom-modeline-bar-width 3)

;; Determines the style used by `doom-modeline-buffer-file-name'.
;;
;; Given ~/Projects/FOSS/emacs/lisp/comint.el
;;   truncate-upto-project => ~/P/F/emacs/lisp/comint.el
;;   truncate-from-project => ~/Projects/FOSS/emacs/l/comint.el
;;   truncate-with-project => emacs/l/comint.el
;;   truncate-except-project => ~/P/F/emacs/l/comint.el
;;   truncate-upto-root => ~/P/F/e/lisp/comint.el
;;   truncate-all => ~/P/F/e/l/comint.el
;;   relative-from-project => emacs/lisp/comint.el
;;   relative-to-project => lisp/comint.el
;;   file-name => comint.el
;;   buffer-name => comint.el<2> (uniquify buffer name)
;;
;; If you are expereicing the laggy issue, especially while editing remote files
;; with tramp, please try `file-name' style.
;; Please refer to https://github.com/bbatsov/projectile/issues/657.
(setq doom-modeline-buffer-file-name-style 'truncate-upto-project)

;; Whether display icons in mode-line or not.
(setq doom-modeline-icon t)

;; Whether display the icon for major mode. It respects `doom-modeline-icon'.
(setq doom-modeline-major-mode-icon t)

;; Whether display color icons for `major-mode'. It respects
;; `doom-modeline-icon' and `all-the-icons-color-icons'.
(setq doom-modeline-major-mode-color-icon t)

;; Whether display icons for buffer states. It respects `doom-modeline-icon'.
(setq doom-modeline-buffer-state-icon t)

;; Whether display buffer modification icon. It respects `doom-modeline-icon'
;; and `doom-modeline-buffer-state-icon'.
(setq doom-modeline-buffer-modification-icon t)

;; Whether display minor modes in mode-line or not.
(setq doom-modeline-minor-modes nil)

;; If non-nil, a word count will be added to the selection-info modeline segment.
(setq doom-modeline-enable-word-count nil)

;; If non-nil, only display one number for checker information if applicable.
(setq doom-modeline-checker-simple-format t)


(setq doom-modeline-vcs t)

;; The maximum displayed length of the branch name of version control.
(setq doom-modeline-vcs-max-length 12)

;; Whether display perspective name or not. Non-nil to display in mode-line.
(setq doom-modeline-persp-name t)

;; Whether display `lsp' state or not. Non-nil to display in mode-line.
(setq doom-modeline-lsp t)

;; Whether display github notifications or not. Requires `ghub` package.
(setq doom-modeline-github nil)

;; The interval of checking github.
(setq doom-modeline-github-interval (* 30 60))

;; Whether display environment version or not
(setq doom-modeline-env-version t)
;; Or for individual languages
(setq doom-modeline-env-enable-python t)
(setq doom-modeline-env-enable-ruby t)
(setq doom-modeline-env-enable-perl t)
(setq doom-modeline-env-enable-go t)
(setq doom-modeline-env-enable-elixir t)
(setq doom-modeline-env-enable-rust t)

;; Change the executables to use for the language version string
(setq doom-modeline-env-python-executable "python")
(setq doom-modeline-env-ruby-executable "ruby")
(setq doom-modeline-env-perl-executable "perl")
(setq doom-modeline-env-go-executable "go")
(setq doom-modeline-env-elixir-executable "iex")
(setq doom-modeline-env-rust-executable "rustc")

;; Whether display mu4e notifications or not. Requires `mu4e-alert' package.
(setq doom-modeline-mu4e nil)

;; Whether display irc notifications or not. Requires `circe' package.
(setq doom-modeline-irc nil)

;; Function to stylize the irc buffer names.
;; (setq doom-modeline-irc-stylize 'identity)

;;Visual Customization
(set-face-attribute 'doom-modeline-buffer-modified nil :foreground "#FF5555" :background "#AEEEEE")
(set-face-attribute 'doom-modeline-evil-insert-state nil :foreground "#FF5555" :background "#AEEEEE")

(use-package git-gutter)

;; If you enable global minor mode
(global-git-gutter-mode +1)

;; If you would like to use git-gutter.el and linum-mode
;; (git-gutter:linum-setup)

(global-set-key (kbd "C-x C-g") 'git-gutter)
(global-set-key (kbd "C-x v =") 'git-gutter:popup-hunk)

;; Jump to next/previous hunk
(global-set-key (kbd "C-x p") 'git-gutter:previous-hunk)
(global-set-key (kbd "C-x n") 'git-gutter:next-hunk)

;; Stage current hunk
(global-set-key (kbd "C-x v s") 'git-gutter:stage-hunk)
;; Revert current hunk

(global-set-key (kbd "C-x v r") 'git-gutter:revert-hunk)

;; Mark current hunk
(global-set-key (kbd "C-x v SPC") #'git-gutter:mark-hunk)

(custom-set-variables
 '(git-gutter:modified-sign "▐") 
 '(git-gutter:added-sign "▐")    
 '(git-gutter:deleted-sign "▐"))
 '(git-gutter:update-interval 2)
 '(git-gutter:visual-line t)


(custom-set-faces
 '(git-gutter:added    ((t (:foreground "#40883F"))))
 '(git-gutter:modified ((t (:foreground "#AF8700"))))
 '(git-gutter:deleted  ((t (:foreground "#FF5555")))))
