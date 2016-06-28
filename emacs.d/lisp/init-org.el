(provide 'init-org)

;; The Basic Bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)


;; The Basic File Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; TODO Keywords
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-todo-keywords
  '((sequence "TODO(t)" "ONDECK(o)" "WAITING(w)" "SOMEDAY(s)" "CURRENT(c)" "|" "DONE(d)")))

 ;; For Dracula Theme
 (setq org-todo-keyword-faces
   '(("ONDECK" . (:foreground "#f1fa8c" :weight bold))   
     ("WAITING" . (:foreground "#bd93f9" :weight bold)) 
     ("CANCELED" . (:foreground "#ff5555" :weight bold))
     ("CURRENT" . (:foreground "#50fa7b" :weight bold))
     ("SOMEDAY" . (:foreground "#6272a4" :weight bold))))


;; Visual Stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-hide-leading-stars t)
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; For Dracula
(custom-set-faces
  '(org-level-1 ((t (:height 1.0 :foreground "#8be9fd" :height 2.0 :weight bold))))
  '(org-level-2 ((t (:foreground "#ffb86c" :weight bold :height 1.0))))
  '(org-level-3 ((t (:foreground "#ff79c6" :height 1.0))))
  '(org-level-4 ((t (:foreground "#bd93f9" :height 1.0))))
  '(org-level-5 ((t (:foreground "#6272a4" :height 1.0))))
  '(org-level-6 ((t (:foreground "#6272a4" :height 1.0)))))

(setq org-ellipsis " …")

(require 'linum-off)
(add-to-list 'linum-disabled-modes-list "org-mode")

(setq org-cycle-separator-lines 0)


;; org-capture stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


;; stuff for source code blocks in org docs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-src-window-setup 'current-window)
(setq org-confirm-babel-evaluate nil)

;; active Babel languages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(org-babel-do-load-languages
 'org-babel-load-languages
 '((ruby . t)
   (emacs-lisp . nil)
   ))


(defun mark-done-and-archive ()
  "Mark the state of an org-mode item as DONE and archive it."
  (interactive)
  (org-todo 'done)
  (org-archive-subtree))

(define-key global-map "\C-c\C-x\C-s" 'mark-done-and-archive)
;; (setq org-log-done 'time)

;; Exporting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ox-md)
(require 'ox-beamer)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (ruby . t)
   (dot . t)
   (gnuplot . t)))

(setq org-export-with-smart-quotes t)

;; Org-Grep Stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'org-grep "org-grep" nil t)
(define-key global-map "\C-cfg" 'org-grep-full)
(define-key global-map "\C-cg"  'org-grep)

(setq org-grep-directories (list "~/Dropbox/Notes"))

(provide 'init-org)
