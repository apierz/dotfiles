;; -*- no-byte-compile: t; -*-
;;; tools/password-store/test/autoload-pass.el

(load! ../autoload)

(defmacro with-passwords!! (buffer-args &rest body)
  (declare (indent defun))
  `(cl-letf
       (((symbol-function '+pass--get-entry)
         (lambda (entry)
           (when (equal entry "fake/source")
             '((secret . "defuse-account-gad")
               ("login" . "HL2532-GANDI")
               ("alt-login" . "hlissner")
               ("email" . "henrik@lissner.net")
               ("url" . "https://www.gandi.net/login"))))))
     ,@body))

;;
(def-test! get-field
  (with-passwords!!
   (should (equal (+pass-get-field "fake/source" "login")
                  "HL2532-GANDI"))
   (should (equal (+pass-get-field "fake/source" "email")
                  "henrik@lissner.net"))
   (should (equal (+pass-get-field "fake/source" '("alt-login" "email"))
                  "hlissner"))
   (should (equal (+pass-get-field "fake/source" '("username" "email"))
                  "henrik@lissner.net"))))

(def-test! missing-fields-return-nil
  (with-passwords!!
   (should-not (+pass-get-field "fake/source" '("x" "y" "z")))))

(def-test! missing-entries-throw-error
  (with-passwords!!
   (should-error (+pass-get-field "nonexistent/source" "login"))))

(def-test! get-login
  (with-passwords!!
    (should (equal (+pass-get-user   "fake/source") "HL2532-GANDI"))
    (should (equal (+pass-get-secret "fake/source") "defuse-account-gad"))))
