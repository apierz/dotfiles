(provide 'init-evil)

(require 'key-chord)
(key-chord-mode 1)
(require 'dired)
(require 'evil)
(require 'evil-leader)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; I Solemenly Swear That I Am Up To No Good
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(evil-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Key Bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; My Fn Button Shortcuts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key [f1]  'mu4e)
(global-set-key [f2] 'andy-new-empty-buffer)

(global-set-key [f4] 'fci-mode)
(global-set-key [f5] 'search-my-notes)

;; Switch Mac Meta Key
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mac-switch-meta nil 
  "switch meta between Option and Command"
  (interactive)
  (if (eq mac-option-modifier nil)
      (progn
	(setq mac-option-modifier 'meta)
	(setq mac-command-modifier 'hyper)
	)
    (progn 
      (setq mac-option-modifier nil)
      (setq mac-command-modifier 'meta)
      )
    )
  )

;; Use Normal Mac Keys For Cut Copy Paste
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun pbcopy ()
  (interactive)
  (call-process-region (point) (mark) "pbcopy")
  (setq deactivate-mark t))

(defun pbpaste ()
  (interactive)
  (call-process-region (point) (if mark-active (mark) (point)) "pbpaste" t t))

(defun pbcut ()
  (interactive)
  (pbcopy)
  (delete-region (region-beginning) (region-end)))

(global-set-key (kbd "M-c") 'pbcopy)
(global-set-key (kbd "C-c x") 'pbcut)
(global-set-key (kbd "M-v") 'pbpaste)


;; Evil Leader Shortcuts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun air--config-evil-leader ()
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
    "."  'switch-to-previous-buffer
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
(air--config-evil-leader)

;; Robe Mode Commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "M-j") 'robe-jump)

;; Temporary Home for Magit Commands and Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

;; Custom Controls for Evil-Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; Mu4e Controls
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(evil-define-key 'motion mu4e-headers-mode-map "k" 'mu4e-headers-next)

;; esc quits for non-evil stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-motion-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key evil-emacs-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape]'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

;; buffer cycling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key [C-tab] 'next-buffer)
(global-set-key [C-S-iso-lefttab] 'previous-buffer);Linux
(global-set-key [C-S-tab] 'previous-buffer);Windows/Linux

;;Stop terrible cursor move back nonsense
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq evil-move-cursor-back nil)

;; Org Mode IJKL controls
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


;; Use Evil Controls in Dired mode / other motion states
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq evil-normal-state-modes (append evil-motion-state-modes
  evil-normal-state-modes))

;; Use tpope style surround plugin with emacs c/d/y ; s ; "/'/>/} etc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package evil-surround)
(global-evil-surround-mode 1)

;; Use tpope style commentry plugin with emacs gc_
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(evil-commentary-mode)


(provide 'init-evil)

