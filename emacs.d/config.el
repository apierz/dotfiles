
(setq user-full-name "Andy Pierz"
      user-mail-address "andy@andypierz.com")

(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" .
"http://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)

(add-to-list 'load-path (expand-file-name "snippets" user-emacs-directory))
(add-to-list 'load-path "~/.emacs.d/plugins")
(add-to-list 'load-path "~/.emacs.d/plugins/evil-org-mode")
(add-to-list 'load-path "/usr/local/Cellar/mu/HEAD/bin/mu")
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'load-path "/Users/Andy/Documents/Programming_Projects/doom-theme") 
(add-to-list 'load-path "/Users/Andy/Documents/Programming_Projects/dracula-theme/emacs")
(add-to-list 'load-path "/Users/Andy/Documents/Programming_Projects/doom-theme")

(require 'use-package)

(setq inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t) (menu-bar-mode -1)
(tool-bar-mode -1)
(when (boundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(show-paren-mode nil)
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(setq-default left-fringe-width nil)
(setq-default indent-tabs-mode nil)
(eval-after-load "vc" '(setq vc-handled-backends nil))
(setq vc-follow-symlinks t)
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
      kept-new-versions      5 ; how many of the newest versions to keep
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

;;(use-package dracula-theme)
;;(load-theme 'dracula t)

(use-package doom-theme
  :config
  (global-hl-line-mode)
  (doom-init-neotree)
  (setq doom-enable-bold t)
  (setq doom-enable-bright-buffers t)
  (setq doom-enable-bright-minibuffer t)
  (setq doom-enable-italic t))

(load-theme 'doom-one t)

(set-face-attribute 'default nil
                     :family "Hack" :height 140)

 ;; (set-frame-parameter (selected-frame) 'alpha '(90 90))
 ;; (add-to-list 'default-frame-alist '(alpha 90 90))

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

(use-package key-chord
  :config
  (key-chord-mode 1))

(key-chord-define evil-insert-state-map "hh" 'evil-normal-state)
(key-chord-define evil-insert-state-map ",," "<")
(key-chord-define evil-insert-state-map ".." ">")
(key-chord-define evil-replace-state-map "hh" 'evil-normal-state)
(key-chord-define evil-visual-state-map "hh" 'evil-normal-state)
(key-chord-define evil-motion-state-map "hh" 'evil-normal-state)
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
    "b"  'helm-mini
    "d"  'kill-this-buffer
    "e"  'find-file
    "f"  'fontify-and-browse
    "p"  'cycle-powerline-separators
    "b"  'switch-to-buffer
    "l"  'whitespace-mode       ;; Show invisible characters
    "nn" 'narrow-and-set-normal ;; Narrow to region and enter normal mode
    "nw" 'widen
    "o"  'delete-other-windows  ;; C-w o
    "S"  'delete-trailing-whitespace
    "t"  'gtags-reindex
    "T"  'gtags-find-tag
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

(global-set-key [f1]  'mu4e)
(global-set-key [f2] 'andy-new-empty-buffer)

(global-set-key [f4] 'fci-mode)
(global-set-key [f5] 'search-my-notes)
(global-set-key [f6] 'linum-relative-mode)

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

(use-package helm)
(use-package helm-config)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-x r b") 'helm-bookmarks)
(global-set-key (kbd "C-X m") 'helm-M-x)
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
(setq org-agenda-files (list "~/Dropbox/Notes"))
(setq org-agenda-file-regexp "\\`[^.].*\\.txt\\|[0-9]\\{8\\}\\'")
(add-to-list 'auto-mode-alist '("\\.txt$" . org-mode))
(setq org-agenda-text-search-extra-files (list nil ))


(add-hook 'find-file-hooks 
  (lambda ()
    (let ((file (buffer-file-name)))
    (when (and file (equal (file-name-directory file) "~/Dropbox/Notes"))
    (org-mode)))))

(use-package linum-off
  :config
  (add-to-list 'linum-disabled-modes-list "org-mode"))

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
   '(("ONDECK" . (:foreground "#ecbe7b" :weight bold))   
     ("WAITING" . (:foreground "#9c91e4" :weight bold)) 
     ("CANCELED" . (:foreground "#dc79dc" :weight bold))
     ("CURRENT" . (:foreground "#7bc275" :weight bold))
     ("DONE" . (:foreground "#ff665c" :weight bold))
     ("SOMEDAY" . (:foreground "#525E6C" :weight bold))))

(setq org-hide-leading-stars t)
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(setq org-ellipsis "…")

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

;; (push "<path-to-this-file>" load-path)
;; (require 'org-toodledo)
;; (setq org-toodledo-userid "<toodledo-userid>")      << *NOT* your email!
;; (setq org-toodledo-password "<toodled-password>")

;; ;; Useful key bindings for org-mode
;; (add-hook 'org-mode-hook
;;        (lambda ()
;;          (local-unset-key "\C-o")
;;          (local-set-key "\C-od" 'org-toodledo-mark-task-deleted)
;;          (local-set-key "\C-os" 'org-toodledo-sync)
;;          )
;;        )
;; (add-hook 'org-agenda-mode-hook
;;        (lambda ()
;;          (local-unset-key "\C-o")
;;          (local-set-key "\C-od" 'org-toodledo-agenda-mark-task-deleted)
;;          )
       ;; )

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
(ac-config-default)

(require 'linum-relative)

(linum-mode)
(global-linum-mode)
(setq linum-format "%4d \u2502 ")
(set-face-attribute 'linum nil :slant 'normal)
(with-eval-after-load 'linum
(linum-relative-toggle))
(setq linum-relative-current-symbol "->")
(setq linum-relative-plusp-offset 0)

(use-package smooth-scrolling
  :config
  (smooth-scrolling-mode 1))

(use-package fill-column-indicator)
(use-package unbound)
(use-package nnir)
(use-package dumb-jump
  :config
  (dumb-jump-mode))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (rainbow-delimiters-mode)))

(setq python-indent-offset 2)

(add-hook 'python-mode-hook
 (lambda ()
   (flycheck-mode)
   (yas-minor-mode)))

(add-hook 'sh-mode-hook
          (lambda ()
            (rainbow-delimiters-mode)
            (setq sh-basic-offset 2
                  sh-indentation 2)))

(add-hook 'ruby-mode-hook
  (lambda ()
    (setq ruby-insert-encoding-magic-comment nil)
      (yas-minor-mode)
      (robe-mode)
      (rainbow-delimiters-mode)
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
  (rainbow-delimiters-mode)
  (setq web-mode-attr-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-indent-style 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-sql-indent-offset 2))

;; (setq display-time-format "%I:%M")
;; (setq display-time-mail-directory "~/.Maildir/Personal/INBOX/new")
;; (setq display-time-default-load-average nil)
;; (display-time-mode 1)

  (defgroup segments-group nil "My powerline line segments" :group 'segments)

(if window-system  (defface my-pl-segment1-active
    '((t (:foreground "#3d3d48" :background "#ecbe7b")))
    "Powerline first segment active face.")
  (defface my-pl-segment1-active
    '((t (:foreground "#525252" :background "#ecbe7b")))
    "Powerline first segment active face."))
  (defface my-pl-segment1-inactive
   '((t (:foreground "#b5babf" :background "#545565")))
    "Powerline first segment inactive face.")

  (defface my-pl-segment2-active
    '((t (:foreground "#eeeeee" :background "#00b3ef")))
    "Powerline second segment active face.")
  (defface my-pl-segment2-inactive
    '((t (:foreground "#b5babf" :background "#545565")))
    "Powerline second segment inactive face.")

  (if window-system (defface my-pl-segment3-active
    '((t (:foreground "#00b3ef" :background "#3d3d48")))
    "Powerline third segment active face.")
   (defface my-pl-segment3-active
    '((t (:foreground "#00b3ef" :background "#525252")))
    "Powerline third segment active face."))
  (defface my-pl-segment3-inactive
    '((t (:foreground "#b5babf" :background "#545565")))
    "Powerline third segment inactive face.")

  (defface my-pl-segment4-active
    '((t (:foreground "#ffffff" :background "#dc79dc")))
    "Powerline hud segment active face.")
  (defface my-pl-segment4-inactive
    '((t (:foreground "#ffffff" :background "#b5babf")))
    "Powerline hud segment inactive face.")


 (if window-system (defface my-pl-segment5-active
    '((t (:foreground "#dc79dc" :background "#3d3d48")))
    "Powerline buffersize segment active face.")
   (defface my-pl-segment5-active
    '((t (:foreground "#dc79dc" :background "#525252")))
    "Powerline buffersize segment active face."))

  (defface my-pl-segment5-inactive
    '((t (:foreground "#b5babf" :background "#545565")))
    "Powerline buffersize segment inactive face.")

  (if window-system (defface my-pl-segment6-active
   '((t (:foreground "#3d3d48" :background "#ecbe7b" :weight bold)))
    "Powerline buffer-id  segment active face.")
   (defface my-pl-segment6-active
   '((t (:foreground "#525252" :background "#ecbe7b" :weight bold)))
    "Powerline buffer-id  segment active face."))
  (defface my-pl-segment6-inactive
   '((t (:foreground "#b5babf" :background "#545565" :weight bold)))
    "Powerline buffer-id  segment inactive face.")

;;     (defun andy--powerline-default-theme ()
;;       "Set up my custom Powerline with Evil indicators."
;;       (interactive)
;;       (setq-default mode-line-format
;;         '("%e"
;;           (:eval
;;            (let* ((active (powerline-selected-window-active))
;;              (seg1 (if active 'my-pl-segment1-active 'my-pl-segment1-inactive))
;;              (seg2 (if active 'my-pl-segment2-active 'my-pl-segment2-inactive))
;;              (seg3 (if active 'my-pl-segment3-active 'my-pl-segment3-inactive))
;;              (seg4 (if active 'my-pl-segment4-active 'my-pl-segment4-inactive))
;;              (seg5 (if active 'my-pl-segment5-active 'my-pl-segment5-inactive))
;;              (seg6 (if active 'my-pl-segment6-active 'my-pl-segment6-inactive))
;;              (separator-left (intern (format "powerline-%s-%s"
;;                                    (powerline-current-separator)
;;                                    (car powerline-default-separator-dir))))
;;              (separator-right (intern (format "powerline-%s-%s"
;;                                     (powerline-current-separator)
;;                                     (cdr powerline-default-separator-dir))))
;;                   (lhs (list (let ((evil-face (powerline-evil-face)))
;;                                (if evil-mode
;;                                    (powerline-raw (powerline-evil-tag) evil-face)
;;                                  ))
;;                              (if evil-mode
;;                                  (funcall separator-left (powerline-evil-face) seg1))
;;                              (powerline-raw "[%*]" seg1 'l)
;;                              (powerline-buffer-path seg1 'l)
;;                              ;; (when powerline-display-buffer-size
;;                                ;; (powerline-buffer-size seg5 'l))
;;                              (powerline-vc seg5 'l)
;;                              (powerline-buffer-id seg6 'l)
;;                              (when (and (boundp 'which-func-mode) which-func-mode)
;;                                (powerline-raw which-func-format seg1 'l))
;;                              (powerline-raw " " seg1)
;;                              (funcall separator-left seg1 seg2)
;;                              (when (boundp 'erc-modified-channels-object)
;;                                (powerline-raw erc-modified-channels-object seg2 'l))
;;                              (powerline-major-mode seg2 'l)
;;                              (powerline-process seg2)
;;                              (powerline-narrow seg2 'l)
;;                              (powerline-raw " " seg2)
;;                              (funcall separator-left seg2 seg3)
;;                              (powerline-minor-modes seg3 'l)
;;                              ))
;;                              (rhs (list 
;;                              (funcall separator-right seg3 seg2)
;;                              (powerline-raw (char-to-string #xe0a1) seg2 'l)
;;                              (powerline-raw "%l" seg2 'l)
;;                              (powerline-raw ":" seg2 'r)
;;                              (powerline-raw "%c" seg2 'r)
;;                              (funcall separator-right seg2 seg1)
;;                              (powerline-raw " " seg1)
;;                              (powerline-raw "%6p" seg3 'r)
;;                              (when powerline-display-hud
;;                                (powerline-hud seg4 seg1))
;;                              (powerline-raw " " seg1 'r)
;;                              (funcall separator-right seg1 seg2)
;;                              (powerline-raw global-mode-string seg2 'r)
;; )))
;;              (concat (powerline-render lhs)
;;                      (powerline-fill seg3 (powerline-width rhs))
;;                      (powerline-render rhs)))))))

;;     (use-package powerline
;;       :ensure t
;;       :config
;;       (setq powerline-height 26)
;;       (setq powerline-default-separator (if (display-graphic-p) 'arrow-fade
;;                                           nil))
;;       (andy--powerline-default-theme))

;; (use-package powerline-evil
;;   :ensure t)

(use-package projectile)
(use-package helm-projectile)

(add-hook 'ruby-mode-hook 'projectile-mode)
(add-hook 'web-mode-hook 'projectile-mode)
(setq projectile-indexing-method 'alien)
(setq projectile-switch-project-action 'projectile-find-file)
(setq projectile-completion-system 'default)
(setq projectile-enable-caching nil)

(helm-projectile-on)

(set-face-attribute 'helm-source-header nil :foreground "#ffb86c" :height 1.66)

(use-package mu4e)
(require 'mu4e-multi)
(use-package evil-mu4e)

(setq mu4e-mu-binary "/usr/local/Cellar/mu/HEAD/bin/mu")
(setq mu4e-maildir "/Users/Andy/.Maildir")

(setq mu4e-multi-account-alist
  '(("personal"
     (user-mail-address .  "andy@andypierz.com")
     (user-full-name  .   "Andy Pierz")
     (mu4e-drafts-folder . "/personal/Drafts")
     (mu4e-trash-folder .  "/personal/Trash")
     (mu4e-refile-folder . "/personal/Archive"))
    ("work"
     (user-mail-address .  "andy@mutdut.com")
     (user-fullname . "Andy Pierz")
     (mu4e-drafts-folder . "/work/Drafts")
     (mu4e-trash-folder .  "/work/Trash")
     (mu4e-refile-folder . "/work/Archive"))))

(mu4e-multi-enable)

(setq mu4e-drafts-folder "/drafts")
(setq mu4e-sent-folder "/personal/Sent Items")


;;set attachment downloads directory
(setq mu4e-attachment-dir  "~/Downloads")

;; setup some handy shortcuts
;; you can quickly switch to your Inbox -- press ``ji''
;; then, when you want archive some messages, move them to
;; the 'All Mail' folder by pressing ``ma''.

(setq mu4e-maildir-shortcuts
  '( ("/personal/INBOX"              . ?i)
     ("/personal/Sent Items"   . ?s)
     ("/personal/Trash"       . ?t)
     ("/personal/Archive"    . ?a)
     ("/personal/Starred"    . ?p)
     ("/personal/Drafts"    . ?d)
       
     ("/work/INBOX"      . ?w)
     ("/work/Drafts"      . ?z)
     ("/work/Sent Items"       . ?f)
     ("/work/Archive"    . ?o)))


;; allow for updating mail using 'U' in the main view:
(setq mu4e-get-mail-command "offlineimap")
(setq mu4e-update-interval 300)

;; something about ourselves
(setq
  user-mail-address "andy@andypierz.com"
  user-full-name  "Andy Pierz"
  mu4e-compose-signature
  (concat
    ""
    ""))


(require 'smtpmail)

(setq message-send-mail-function 'smtpmail-send-it
  smtpmail-stream-type 'ssl
  smtpmail-auth-credentials
    (expand-file-name "~/.authinfo.gpg")
  smtpmail-default-smtp-server "mail.hover.com"
  smtpmail-smtp-server "mail.hover.com"
  smtpmail-smtp-service 465)

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)

(defvar my-mu4e-account-alist
  '(("personal"
  ;; about me
  (user-mail-address      "andy@andypierz.com")
  (user-full-name         "Andy Pierz")
  ;; smtp
  (smtpmail-stream-type ssl)
  (smtpmail-starttls-credentials '(("mail.hover.com" 587 nil nil)))
  (smtpmail-default-smtp-server "mail.hover.com")
  (smtpmail-smtp-server "mail.hover.com")
  (smtpmail-smtp-service 465))
  ("work"
  ;; about me
  (user-mail-address      "andy@mutdut.com")
  (user-full-name         "Andy Pierz")
  ;;(mu4e-compose-signature "0xAX")

  ;; smtp
  (smtpmail-stream-type ssl)
  (smtpmail-auth-credentials '(("mail.hover.com" 25 "andy@mutdut.com" nil)))
  (smtpmail-default-smtp-server "mail.hover.com")
  (smtpmail-smtp-service 465))))

(defun my-mu4e-set-account ()
  "Set the account for composing a message."
  (let* ((account
    (if mu4e-compose-parent-message
      (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
      (string-match "/\\(.*?\\)/" maildir)
      (match-string 1 maildir))
      (completing-read (format "Compose with account: (%s) "
        (mapconcat #'(lambda (var) (car var)) my-mu4e-account-alist "/"))
        (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
          nil t nil nil (car my-mu4e-account-alist))))
        (account-vars (cdr (assoc account my-mu4e-account-alist))))
    (if account-vars
      (mapc #'(lambda (var)
       (set (car var) (cadr var)))
          account-vars)
    (error "No email account found"))))

(add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)


(use-package evil-mu4e)

(define-key mu4e-headers-mode-map "p" 'mu4e-headers-mark-for-flag)

(add-hook 'mu4e-main-mode-hook 'evil-motion-state)
(add-hook 'mu4e-headers-mode-hook 'evil-motion-state)

(require 'gnus-dired)
;; make the `gnus-dired-mail-buffers' function also work on
;; message-mode derived modes, such as mu4e-compose-mode
(defun gnus-dired-mail-buffers ()
  "Return a list of active message buffers."
  (let (buffers)
    (save-current-buffer
      (dolist (buffer (buffer-list t))
  (set-buffer buffer)
  (when (and (derived-mode-p 'message-mode)
    (null message-sent-message-via))
    (push (buffer-name buffer) buffers))))
    (nreverse buffers)))

(setq gnus-dired-mail-mode 'mu4e-user-agent)
(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

(setq mu4e-view-show-images t)
(setq mu4e-view-show-image-max-width 800)
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))
(setq mu4e-view-prefer-html nil)

(setq mu4e-html2text-command 'mu4e-shr2text)

(use-package magit)
(use-package evil-magit)

(require 'epa-file)
(epa-file-enable)

;;;###autoload
(defun doom-fix-unicode (font &rest chars)
  "Display certain unicode characters in a specific font.
e.g. (doom-fix-unicode \"DejaVu Sans\" ?⚠ ?★ ?λ)"
  (declare (indent 1))
  (mapc (lambda (x) (set-fontset-font
                t (cons x x)
                (cond ((fontp font)
                       font)
                      ((listp font)
                       (font-spec :family (car font) :size (nth 1 font)))
                      ((stringp font)
                       (font-spec :family font))
                      (t (error "FONT is an invalid type: %s" font)))))
        chars))

;;;###autoload
(defun doom/project-root (&optional strict-p)
  "Get the path to the root of your project."
  (let (projectile-require-project-root strict-p)
    (projectile-project-root)))

  (defvar mode-line-height 30
    "How tall the mode-line should be. This is only respected in GUI emacs.")

  ;; Load powerline only when uncompiled, in order to generate the xpm bitmaps for
  ;; the mode-line. This is the tall blue bar on the left of the mode-line.
  ;; NOTE Compile this file for a faster startup!
(eval-when-compile (require 'powerline))
;; FIXME Don't hardcode colors in
(defvar mode-line-bar          (eval-when-compile (pl/percent-xpm mode-line-height 100 0 100 0 3 "#00B3EF" nil)))
(defvar mode-line-eldoc-bar    (eval-when-compile (pl/percent-xpm mode-line-height 100 0 100 0 3 "#B3EF00" nil)))
(defvar mode-line-inactive-bar (eval-when-compile (pl/percent-xpm mode-line-height 100 0 100 0 3 nil nil)))


  ;; Custom faces
  (defface mode-line-is-modified nil
    "Face for mode-line modified symbol")

  (defface mode-line-2 nil
    "The alternate color for mode-line text.")

  (defface mode-line-highlight nil
    "Face for bright segments of the mode-line.")

  (defface mode-line-count-face nil
    "Face for anzu/evil-substitute/evil-search number-of-matches display.")

  ;; Git/VCS segment faces
  (defface mode-line-vcs-info '((t (:inherit warning)))
    "")
  (defface mode-line-vcs-warning '((t (:inherit warning)))
    "")

  ;; Flycheck segment faces
  (defface doom-flycheck-error '((t (:inherit error)))
    "Face for flycheck error feedback in the modeline.")
  (defface doom-flycheck-warning '((t (:inherit warning)))
    "Face for flycheck warning feedback in the modeline.")


  ;;
  ;; Functions
  ;;

  (defun doom-ml-flycheck-count (state)
    "Return flycheck information for the given error type STATE."
    (when (flycheck-has-current-errors-p state)
      (if (eq 'running flycheck-last-status-change)
          "?"
        (cdr-safe (assq state (flycheck-count-errors flycheck-current-errors))))))

  ;; pyenv/rbenv version segment
  (defvar doom-ml-env-version-hook '()
    "Hook that runs whenever the environment version changes (e.g. rbenv/pyenv)")

  (defun doom-ml|env-update ()
    (when doom-ml--env-command
      (let ((default-directory (doom/project-root)))
        (let ((s (shell-command-to-string doom-ml--env-command)))
          (setq doom-ml--env-version (if (string-match "[ \t\n\r]+\\'" s)
                                      (replace-match "" t t s)
                                    s))
          (run-hook-with-args 'doom-ml-env-version-hook doom-ml--env-version)))))

  (defmacro def-version-cmd! (modes command)
    "Define a COMMAND for MODE that will set `doom-ml--env-command' when that mode is
  activated, which should return the version number of the current environment. It is used
  by `doom-ml|env-update' to display a version number in the modeline. For instance:
    (def-version-cmd! ruby-mode \"ruby --version | cut -d' ' -f2\")
  This will display the ruby version in the modeline in ruby-mode buffers. It is cached the
  first time."
    (add-hook! (focus-in find-file) 'doom-ml|env-update)
    `(add-hook! ,modes (setq doom-ml--env-command ,command)))


  ;;
  ;; Initialization
  ;;

  ;; Where (py|rb)env version strings will be stored
  (defvar-local doom-ml--env-version nil)
  (defvar-local doom-ml--env-command nil)

  ;; Make certain unicode glyphs bigger for the mode-line.
  ;; FIXME Replace with all-the-icons?
  (doom-fix-unicode '("DejaVu Sans Mono" 15) ?✱) ;; modified symbol
  (let ((font "DejaVu Sans Mono for Powerline"))
    (doom-fix-unicode (list font 12) ?)  ;; git symbol
    (doom-fix-unicode (list font 16) ?∄)  ;; non-existent-file symbol
    (doom-fix-unicode (list font 15) ?)) ;; read-only symbol

  ;; So the mode-line can keep track of "the current window"
  (defvar mode-line-selected-window nil)
  (defun doom|set-selected-window (&rest _)
    (let ((window (frame-selected-window)))
      (unless (minibuffer-window-active-p window)
        (setq mode-line-selected-window window))))
  (add-hook 'window-configuration-change-hook #'doom|set-selected-window)
  (add-hook 'focus-in-hook #'doom|set-selected-window)
  (advice-add 'select-window :after 'doom|set-selected-window)
  (advice-add 'select-frame  :after 'doom|set-selected-window)


  ;;
  ;; Mode-line segments
  ;;

  (defun *buffer-path ()
    "Displays the buffer's full path relative to the project root (includes the
  project root). Excludes the file basename. See `*buffer-name' for that."
    (when buffer-file-name
      (propertize
       (f-dirname
        (let ((buffer-path (file-relative-name buffer-file-name (doom/project-root)))
              (max-length (truncate (/ (window-body-width) 1.75))))
          (concat (projectile-project-name) "/"
                  (if (> (length buffer-path) max-length)
                      (let ((path (reverse (split-string buffer-path "/" t)))
                            (output ""))
                        (when (and path (equal "" (car path)))
                          (setq path (cdr path)))
                        (while (and path (<= (length output) (- max-length 4)))
                          (setq output (concat (car path) "/" output))
                          (setq path (cdr path)))
                        (when path
                          (setq output (concat "../" output)))
                        (when (string-suffix-p "/" output)
                          (setq output (substring output 0 -1)))
                        output)
                    buffer-path))))
       'face (if active 'mode-line-2))))

  (defun *buffer-name ()
    "The buffer's base name or id."
    ;; FIXME Don't show uniquify tags
    (s-trim-left (format-mode-line "%b")))

  (defun *buffer-pwd ()
    "Displays `default-directory', for special buffers like the scratch buffer."
    (propertize
     (concat "[" (abbreviate-file-name default-directory) "]")
     'face 'mode-line-2))

  (defun *buffer-state ()
    "Displays symbols representing the buffer's state
  (non-existent/modified/read-only)"
    (when buffer-file-name
      (propertize
       (concat (if (not (file-exists-p buffer-file-name))
                   "∄"
                 (if (buffer-modified-p) "✱"))
               (if buffer-read-only ""))
       'face 'mode-line-is-modified)))

  (defun *buffer-encoding-abbrev ()
    "The line ending convention used in the buffer."
    (if (memq buffer-file-coding-system '(utf-8 utf-8-unix))
        ""
      (symbol-name buffer-file-coding-system)))

  (defun *major-mode ()
    "The major mode, including process, environment and text-scale info."
    (concat (format-mode-line mode-name)
            (if (stringp mode-line-process) mode-line-process)
            (if doom-ml--env-version (concat " " doom-ml--env-version))
            (and (featurep 'face-remap)
                 (/= text-scale-mode-amount 0)
                 (format " (%+d)" text-scale-mode-amount))))

  (defun *vc ()
    "Displays the current branch, colored based on its state."
    (when vc-mode
      (let ((backend (concat " " (substring vc-mode (+ 2 (length (symbol-name (vc-backend buffer-file-name)))))))
            (face (let ((state (vc-state buffer-file-name)))
                    (cond ((memq state '(edited added))
                           'mode-line-vcs-info)
                          ((memq state '(removed needs-merge needs-update conflict removed unregistered))
                           'mode-line-vcs-warning)))))
        (if active
            (propertize backend 'face face)
          backend))))

  (defvar-local doom--flycheck-err-cache nil "")
  (defvar-local doom--flycheck-cache nil "")
  (defun *flycheck ()
    "Persistent and cached flycheck indicators in the mode-line."
    (when (and (featurep 'flycheck)
               flycheck-mode
               (or flycheck-current-errors
                   (eq 'running flycheck-last-status-change)))
      (or (and (or (eq doom--flycheck-err-cache doom--flycheck-cache)
                   (memq flycheck-last-status-change '(running not-checked)))
               doom--flycheck-cache)
          (and (setq doom--flycheck-err-cache flycheck-current-errors)
               (setq doom--flycheck-cache
                     (let ((fe (doom-ml-flycheck-count 'error))
                           (fw (doom-ml-flycheck-count 'warning)))
                       (concat
                        (if fe (propertize (format " •%d " fe)
                                           'face (if active
                                                     'doom-flycheck-error
                                                   'mode-line)))
                        (if fw (propertize (format " •%d " fw)
                                           'face (if active
                                                     'doom-flycheck-warning
                                                   'mode-line))))))))))

  (defun *selection-info ()
    "Information about the current selection, such as how many characters and
  lines are selected, or the NxM dimensions of a block selection."
    (when (and active (evil-visual-state-p))
      (propertize
       (let ((reg-beg (region-beginning))
             (reg-end (region-end))
             (evil (eq 'visual evil-state)))
         (let ((lines (count-lines reg-beg (min (1+ reg-end) (point-max))))
               (chars (- (1+ reg-end) reg-beg))
               (cols (1+ (abs (- (evil-column reg-end)
                                 (evil-column reg-beg))))))
           (cond
            ;; rectangle selection
            ((or (bound-and-true-p rectangle-mark-mode)
                 (and evil (eq 'block evil-visual-selection)))
             (format " %dx%dB " lines (if evil cols (1- cols))))
            ;; line selection
            ((or (> lines 1) (eq 'line evil-visual-selection))
             (if (and (eq evil-state 'visual) (eq evil-this-type 'line))
                 (format " %dL " lines)
               (format " %dC %dL " chars lines)))
            (t (format " %dC " (if evil chars (1- chars)))))))
       'face 'mode-line-highlight)))

  (defun *macro-recording ()
    "Display current macro being recorded."
    (when (and active defining-kbd-macro)
      (propertize
       (format " %s ▶ " (char-to-string evil-this-macro))
       'face 'mode-line-highlight)))

  (make-variable-buffer-local 'anzu--state)
  (defun *anzu ()
    "Show the current match number and the total number of matches. Requires anzu
  to be enabled."
    (when (and (featurep 'evil-anzu) (evil-ex-hl-active-p 'evil-ex-search))
      (propertize
       (format " %s/%d%s "
               anzu--current-position anzu--total-matched
               (if anzu--overflow-p "+" ""))
       'face (if active 'mode-line-count-face))))

  (defun *evil-substitute ()
    "Show number of :s matches in real time."
    (when (and (evil-ex-p) (evil-ex-hl-active-p 'evil-ex-substitute))
      (propertize
       (let ((range (if evil-ex-range
                        (cons (car evil-ex-range) (cadr evil-ex-range))
                      (cons (line-beginning-position) (line-end-position))))
             (pattern (car-safe (evil-delimited-arguments evil-ex-argument 2))))
         (if pattern
             (format " %s matches "
                     (count-matches pattern (car range) (cdr range))
                     evil-ex-argument)
           " ... "))
       'face (if active 'mode-line-count-face))))

  (defun *iedit ()
    "Show the number of iedit regions matches + what match you're on."
    (when (and (boundp 'iedit-mode) iedit-mode)
      (propertize
       (let ((this-oc (let (message-log-max) (iedit-find-current-occurrence-overlay)))
             (length (or (ignore-errors (length iedit-occurrences-overlays)) 0)))
         (format
          " %s/%s "
          (save-excursion
            (unless this-oc
              (iedit-prev-occurrence)
              (setq this-oc (iedit-find-current-occurrence-overlay)))
            (if this-oc
                ;; NOTE: Not terribly reliable
                (- length (-elem-index this-oc iedit-occurrences-overlays))
              "-"))
          length))
       'face (if active 'mode-line-count-face))))

  (defun *buffer-position ()
    "A more vim-like buffer position."
    (let ((start (window-start))
          (end (window-end))
          (pend (point-max)))
      (if (and (= start 1)
               (= end pend))
          ":All"
        (cond ((= start 1) ":Top")
              ((= end pend) ":Bot")
              (t (format ":%d%%%%" (/ end 0.01 pend)))))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defun doom-mode-line (&optional id)
    `(:eval
      (let* ((active (eq (selected-window) mode-line-selected-window))
             (lhs (list (propertize " " 'display (if active mode-line-bar mode-line-inactive-bar))
                        (*flycheck)
                        (*macro-recording)
                        (*selection-info)
                        (*anzu)
                        (*evil-substitute)
                        (*iedit)
                        " "
                        (*buffer-path)
                        (*buffer-name)
                        " "
                        (*buffer-state)
                        ,(if (eq id 'scratch) '(*buffer-pwd))))

             (rhs (list 
                       ;; (*buffer-encoding-abbrev)
                        (*vc) " "
                        (*major-mode) "  "
                        (propertize
                         (concat "(%l,%c) " (*buffer-position))
                         'face (if active 'mode-line-2))))
             (middle (propertize
                      " " 'display `((space :align-to (- (+ right right-fringe right-margin)
                                                         ,(1+ (string-width (format-mode-line rhs)))))))))
        (list lhs middle rhs))))

  (setq-default mode-line-format (doom-mode-line))
