;;; package --- Summary

;;; Commentary:

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Package and load path stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" .
"http://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)

(require 'use-package)
(require 'advice)
(require 'evil)
(require 'helm)
(require 'powerline)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'auto-mode-alist '("\\.org\\;" . org-mode))
(add-to-list 'load-path "~/.emacs.d/plugins/evil-org-mode")
(add-to-list 'load-path "/usr/local/Cellar/mu/HEAD/bin/mu")
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'load-path "/Users/Andy/Documents/Programming_Projects/dracula-theme/emacs")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Essential settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
;; (setq ns-use-srgb-colorspace t)
(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'default-frame-alist '(width . 80))
(setq-default fill-column 80)
(setq-default tab-width 2)
(put 'dired-find-alternate-file 'disabled nil)

;; Font Encoding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8)

(use-package unicode-fonts
  :config
  (unicode-fonts-setup))

(set-fontset-font "fontset-default"
    (cons (decode-char 'ucs #x222F)
          (decode-char 'ucs #x259F))
                           "Deja Vu Sans Mono")

;; Write backups to Dropbox
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq backup-directory-alist '(("." . "~/Dropbox/emacs_backups"))
      backup-by-copying      t  ; Don't de-link hard links
      version-control        t  ; Use version numbers on backups
      delete-old-versions    t  ; Automatically delete excess backups:
      kept-new-versions      5 ; how many of the newest versions to keep
      kept-old-versions      5) ; and how many of the old

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Require Nothing Further
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package magit)
(use-package flycheck
  :config
  (global-flycheck-mode))
(use-package evil-magit)
(use-package fill-column-indicator)
(use-package unbound)
(use-package diminish)
(use-package nnir)
(use-package dracula-theme)
(use-package dumb-jump
  :config
  (dumb-jump-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load my spooky theme and set my font
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-theme 'dracula t)
(set-face-attribute 'default nil
                     :family "Hack" :height 140)

;; set transparency
(set-frame-parameter (selected-frame) 'alpha '(90 90))
(add-to-list 'default-frame-alist '(alpha 90 90))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Other Dot Files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package init-utils)        ;; Various useful functions
(use-package init-powerline)    ;; My Awesome Bottom Bar
(use-package init-org)          ;; color/capture settings, etc.
(use-package init-helm)         ;; some minor keybindings
(use-package init-engine)       ;; search from within Emacs "C-c /"
(use-package init-projectile)   ;; project mgmt
(use-package init-evil)         ;; Hail Satan! plus all other keybindings
(use-package init-mu4e)         ;; my email setup


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hide some modes from the powerline
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Snippets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package yasnippet)

(ac-config-default)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Language Specifc Stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Emacs-Lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (rainbow-delimiters-mode)))

;; Python
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package python)
(setq python-indent-offset 2)

;; sh
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package sh-script)
(add-hook 'sh-mode-hook
          (lambda ()
            (rainbow-delimiters-mode)
            (setq sh-basic-offset 2
                  sh-indentation 2)))

;; Ruby
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ruby-mode)
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; line numbering settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'linum-relative)

(linum-mode)
(global-linum-mode)
(setq linum-format "%4d \u2502 ")
(set-face-attribute 'linum nil :slant 'normal)
(with-eval-after-load 'linum
(linum-relative-toggle))
(setq linum-relative-current-symbol "->")
(setq linum-relative-plusp-offset 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; smooth scrolling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package smooth-scrolling
  :config
  (smooth-scrolling-mode 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Encryption for .authinfo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package epa-file
  :config
  (epa-file-enable))

(provide 'init)
;;; init.el ends here
