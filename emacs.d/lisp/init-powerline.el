(provide 'init.powerline)
;; For Dracula Theme
(defface my-pl-segment1-active
  '((t (:foreground "#f1fa8c" :background "#3a2e58")))
  "Powerline first segment active face.")
(defface my-pl-segment1-inactive
 '((t (:foreground "#f8f8f2" :background "#3a2e58")))
  "Powerline first segment inactive face.")
(defface my-pl-segment2-active
  '((t (:foreground "#f8f8f2" :background "#bd93f9")))
  "Powerline second segment active face.")
(defface my-pl-segment2-inactive
  '((t (:foreground "#f8f8f2" :background "#3a2e58")))
  "Powerline second segment inactive face.")
(defface my-pl-segment3-active
  '((t (:foreground "#bd93f9" :background "#3a2e58")))
  "Powerline third segment active face.")
(defface my-pl-segment3-inactive
  '((t (:foreground "#f8f8f2" :background "#3a2e58")))
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
  '((t (:foreground "#f8f8f2" :background "#3a2e58")))
  "Powerline buffersize segment inactive face.")


;; Alt Background color: #44475a

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
                         (powerline-buffer-id seg1 'l)
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
                         (when (bound-and-true-p nyan-mode)
                           (powerline-raw (list (nyan-create)) seg3 'l))
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

(require 'powerline)

(provide 'init-powerline)

