;;; package --- Summary
;;; Commentary:
;;; Code:


(provide 'init-mu4e)
(use-package mu4e)
(use-package mu4e-multi)
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


;;; Keybindings for mu4e
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package evil-mu4e)

(define-key mu4e-headers-mode-map "p" 'mu4e-headers-mark-for-flag)


;;; Use Dired for attachments
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


(provide 'init-mu4e)
;;; init-mu4e.el ends here
