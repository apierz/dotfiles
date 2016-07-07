
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
(add-to-list 'load-path "/Users/Andy/Documents/Programming_Projects/dracula-theme/emacs")

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
(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'default-frame-alist '(width . 80))
(setq-default fill-column 80)
(setq-default tab-width 2)
(put 'dired-find-alternate-file 'disabled nil)

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
  (interactive "Search Query: ")
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

(use-package dracula-theme)
(load-theme 'dracula t)

(load-theme 'dracula t)
(set-face-attribute 'default nil
                     :family "Hack" :height 140)

(set-frame-parameter (selected-frame) 'alpha '(90 90))
(add-to-list 'default-frame-alist '(alpha 90 90))

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

(use-package key-chord
  :config
  (key-chord-mode 1))

(define-key evil-normal-state-map "i" 'evil-previous-line)
(define-key evil-normal-state-map "j" 'evil-backward-char)
(define-key evil-normal-state-map "k" 'evil-next-line)
(define-key evil-normal-state-map "l" 'evil-forward-char)
(define-key evil-normal-state-map "h" 'evil-insert-state)
(key-chord-define evil-insert-state-map "hh" 'evil-normal-state)
(key-chord-define evil-replace-state-map "hh" 'evil-normal-state)
(key-chord-define evil-visual-state-map "hh" 'evil-normal-state)
(key-chord-define evil-motion-state-map "hh" 'evil-normal-state)
(evil-define-key 'normal dired-mode-map "k" 'dired-next-line)
(evil-define-key 'normal dired-mode-map "i" 'dired-previous-line)
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
(define-key evil-motion-state-map "i" 'evil-previous-line)
(define-key evil-motion-state-map "j" 'evil-backward-char)
(define-key evil-motion-state-map "k" 'evil-next-line)
(define-key evil-motion-state-map "l" 'evil-forward-char)
(define-key evil-motion-state-map "h" 'evil-insert-state)
(setq evil-shift-width 2)
(evil-define-key 'motion mu4e-headers-mode-map "k" 'mu4e-headers-next)

(use-package evil-org)
(evil-define-key 'normal evil-org-mode-map (kbd "M-i") 'org-metaup)
(evil-define-key 'normal evil-org-mode-map (kbd "M-j") 'org-metaleft)
(evil-define-key 'normal evil-org-mode-map (kbd "M-k") 'org-metadown)
(evil-define-key 'normal evil-org-mode-map (kbd "M-i") 'org-metaup)
(evil-define-key 'normal evil-org-mode-map (kbd "M-I") 'org-shiftmetaup)
(evil-define-key 'normal evil-org-mode-map (kbd "M-J") 'org-shiftmetaleft)
(evil-define-key 'normal evil-org-mode-map (kbd "M-K") 'org-shiftmetadown)
(evil-define-key 'normal evil-org-mode-map (kbd "M-L") 'org-shiftmetaright)
(evil-define-key 'normal evil-org-mode-map (kbd "I") 'org-shiftup)
(evil-define-key 'normal evil-org-mode-map (kbd "J") 'org-shiftleft)
(evil-define-key 'normal evil-org-mode-map (kbd "K") 'org-shiftdown)
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

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-todo-keywords
  '((sequence "TODO(t)" "ONDECK(o)" "WAITING(w)" "SOMEDAY(s)" "CURRENT(c)" "|" "DONE(d)")))

 ;; For Dracula Theme
 (setq org-todo-keyword-faces
   '(("ONDECK" . (:foreground "#f1fa8c" :weight bold))   
     ("WAITING" . (:foreground "#bd93f9" :weight bold)) 
     ("CANCELED" . (:foreground "#ff5555" :weight bold))
     ("CURRENT" . (:foreground "#50fa7b" :weight bold))
     ("SOMEDAY" . (:foreground "#6272a4" :weight bold))))

(setq org-hide-leading-stars t)
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(custom-set-faces
  '(org-level-1 ((t (:foreground "#8be9fd" :height 2.0 :weight bold))))
  '(org-level-2 ((t (:foreground "#ffb86c" :weight bold :height 1.0))))
  '(org-level-3 ((t (:foreground "#ff79c6" :height 1.0))))
  '(org-level-4 ((t (:foreground "#bd93f9" :height 1.0))))
  '(org-level-5 ((t (:foreground "#6272a4" :height 1.0))))
  '(org-level-6 ((t (:foreground "#6272a4" :height 1.0)))))

(setq org-ellipsis " …")

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
   (ruby . t)
   (dot . t)
   (gnuplot . t)))

(use-package yasnippet)
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

(defgroup segments-group nil "My powerline line segments" :group 'segments)

(defface my-pl-segment1-active
  '((t (:foreground "#f1fa8c" :background "#3a2e58")))
  "Powerline first segment active face.")
(defface my-pl-segment1-inactive
 '((t (:foreground "#f8f8f2" :background "#545565")))
  "Powerline first segment inactive face.")
(defface my-pl-segment2-active
  '((t (:foreground "#f8f8f2" :background "#bd93f9")))
  "Powerline second segment active face.")
(defface my-pl-segment2-inactive
  '((t (:foreground "#f8f8f2" :background "#545565")))
  "Powerline second segment inactive face.")
(defface my-pl-segment3-active
  '((t (:foreground "#bd93f9" :background "#3a2e58")))
  "Powerline third segment active face.")
(defface my-pl-segment3-inactive
  '((t (:foreground "#f8f8f2" :background "#545565")))
  "Powerline third segment inactive face.")
(defface my-pl-segment4-active
  '((t (:foreground "#ffffff" :background "#f1fa8c")))
  "Powerline hud segment active face.")
(defface my-pl-segment4-inactive
  '((t (:foreground "#ffffff" :background "#f8f8f2")))
  "Powerline hud segment inactive face.")
(defface my-pl-segment5-active
  '((t (:foreground "#ff79c6" :background "#3a2e58")))
  "Powerline buffersize segment active face.")
(defface my-pl-segment5-inactive
  '((t (:foreground "#f8f8f2" :background "#545565")))
  "Powerline buffersize segment inactive face.")
(defface my-pl-segment6-active
 '((t (:foreground "#f1fa8c" :background "#3a2e58" :weight bold)))
  "Powerline buffer-id  segment active face.")
(defface my-pl-segment6-inactive
 '((t (:foreground "#f8f8f2" :background "#545565" :weight bold)))
  "Powerline buffer-id  segment inactive face.")

(defun andy--powerline-default-theme ()
  "Set up my custom Powerline with Evil indicators."
  (interactive)
  (setq-default mode-line-format
    '("%e"
      (:eval
       (let* ((active (powerline-selected-window-active))
         (seg1 (if active 'my-pl-segment1-active 'my-pl-segment1-inactive))
         (seg2 (if active 'my-pl-segment2-active 'my-pl-segment2-inactive))
         (seg3 (if active 'my-pl-segment3-active 'my-pl-segment3-inactive))
         (seg4 (if active 'my-pl-segment4-active 'my-pl-segment4-inactive))
         (seg5 (if active 'my-pl-segment5-active 'my-pl-segment5-inactive))
         (seg6 (if active 'my-pl-segment6-active 'my-pl-segment6-inactive))
         (separator-left (intern (format "powerline-%s-%s"
                               (powerline-current-separator)
                               (car powerline-default-separator-dir))))
         (separator-right (intern (format "powerline-%s-%s"
                                (powerline-current-separator)
                                (cdr powerline-default-separator-dir))))
              (lhs (list (let ((evil-face (powerline-evil-face)))
                           (if evil-mode
                               (powerline-raw (powerline-evil-tag) evil-face)
                             ))
                         (if evil-mode
                             (funcall separator-right (powerline-evil-face) seg1))
                         (powerline-raw "[%*]" seg1 'l)
                         (when powerline-display-buffer-size
                           (powerline-buffer-size seg5 'l))
                         (powerline-buffer-id seg6 'l)
                         (when (and (boundp 'which-func-mode) which-func-mode)
                           (powerline-raw which-func-format seg1 'l))
                         (powerline-raw " " seg1)
                         (funcall separator-left seg1 seg2)
                         (when (boundp 'erc-modified-channels-object)
                           (powerline-raw erc-modified-channels-object seg2 'l))
                         (powerline-major-mode seg2 'l)
                         (powerline-process seg2)
                         (powerline-narrow seg2 'l)
                         (powerline-raw " " seg2)
                         (funcall separator-right seg2 seg3)
                         (powerline-minor-modes seg3 'l)
                         ))
                         (rhs (list (powerline-raw global-mode-string seg3 'r)
                         (funcall separator-left seg3 seg2)
                         (powerline-vc seg2 'r)
                         (powerline-raw "|" seg2 'r)
                         (unless window-system
                           (powerline-raw (char-to-string #xe0a1) seg2 'l))
                         (powerline-raw "%l" seg2 'l)
                         (powerline-raw ":" seg2 'r)
                         (powerline-raw "%c" seg2 'r)
                         (funcall separator-left seg2 seg1)
                         (powerline-raw " " seg1)
                         (powerline-raw "%6p" seg1 'r)
                         (when powerline-display-hud
                           (powerline-hud seg1 seg4)))))
         (concat (powerline-render lhs)
                 (powerline-fill seg3 (powerline-width rhs))
                 (powerline-render rhs)))))))

(use-package powerline
  :ensure t
  :config
  (setq powerline-height 26)
  (setq powerline-default-separator (if (display-graphic-p) 'slant
                                      nil))
  (andy--powerline-default-theme))

(use-package powerline-evil
  :ensure t)

(use-package helm)
(use-package helm-config)

(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-x r b") 'helm-bookmarks)
(global-set-key (kbd "C-X m") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

;; other helm configurations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq helm-split-window-in-side-p t)

(with-eval-after-load
  'helm (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
     (define-key helm-map (kbd "ESC") 'helm-keyboard-quit)
)

(use-package projectile)
(use-package helm-projectile)

(add-hook 'ruby-mode-hook 'projectile-mode)
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
     (mu4e-sent-folder .  "/personal/Sent\ Items")
     (mu4e-drafts-folder . "/personal/Drafts")
     (mu4e-trash-folder .  "/personal/Trash")
     (mu4e-refile-folder . "/personal/Archive"))
    ("work"
     (user-mail-address .  "andy@mutdut.com")
     (user-fullname . "Andy Pierz")
     (mu4e-sent-folder .  "/work/Sent\ Items")
     (mu4e-drafts-folder . "/work/Drafts")
     (mu4e-trash-folder .  "/work/Trash")
     (mu4e-refile-folder . "/work/Archive"))))

(mu4e-multi-enable)

(setq mu4e-drafts-folder "/drafts")

;; don't save message to Sent Messages, Gmail/IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)

;;set attachment downloads directory
(setq mu4e-attachment-dir  "~/Downloads")

;; setup some handy shortcuts
;; you can quickly switch to your Inbox -- press ``ji''
;; then, when you want archive some messages, move them to
;; the 'All Mail' folder by pressing ``ma''.

(setq mu4e-maildir-shortcuts
  '( ("/personal/INBOX"              . ?i)
     ("/personal/Sent\ Items"   . ?s)
     ("/personal/Trash"       . ?t)
     ("/personal/Archive"    . ?a)
     ("/personal/Starred"    . ?p)
       
     ("/work/INBOX"      . ?w)
     ("/work/Sent\ Items"       . ?f)
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
  (smtpmail-stream-type 'ssl)
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
  (smtpmail-stream-type 'ssl)
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

(evil-define-key 'motion mu4e-headers-mode-map "k" 'mu4e-headers-next)
(evil-define-key 'motion mu4e-main-mode-map    "k" 'evil-next-line)

(setq mu4e-html2text-command 'mu4e-shr2text)

(use-package magit)
(use-package evil-magit)
