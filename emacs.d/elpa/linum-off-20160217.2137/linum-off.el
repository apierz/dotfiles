;;; linum-off.el --- Provides an interface for turning line-numbering off
;; 
;; Filename: linum-off.el
;; Description: 
;; Author: Matthew L. Fidler, Florian Adamsky (see wiki)
;; Maintainer: Matthew L. Fidler
;; Created: Mon Sep 20 08:50:07 2010 (-0500)
;; Version: 0.1
;; Package-Version: 20160217.2137
;; Last-Updated: Wed Jul 27 01:45:27 2011 (+0900)
;;           By: Nos
;;     Update #: 43
;; URL:  http://www.emacswiki.org/emacs/auto-indent-mode.el 
;; Keywords: Line Numbering
;; Compatibility: Unknown.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; Put this in your load path and then:
;; (require 'linum-off)
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; 27-Jul-2011
;;    Last-Updated: Wed Jul 27 01:45:27 2011 (+0900) #43 (Nos)
;;    fixed that linum-disable-starred-buffers didn't work
;; 29-Sep-2010    Matthew L. Fidler  
;;    Last-Updated: Wed Sep 29 09:35:10 2010 (-0500) #39 (Matthew L. Fidler)
;;    Added Dired mode
;; 20-Sep-2010    Matthew L. Fidler  
;;    Last-Updated: Mon Sep 20 09:29:06 2010 (-0500) #37 (Matthew L. Fidler)
;;    Took out starred buffers.
;; 20-Sep-2010    Matthew L. Fidler  
;;    Last-Updated: Mon Sep 20 09:24:49 2010 (-0500) #34 (Matthew L. Fidler)
;;    Changed advice to function change.  Couldn't get advice to work.
;; 20-Sep-2010    Matthew L. Fidler  
;;    Last-Updated: Mon Sep 20 09:11:13 2010 (-0500) #14 (Matthew L. Fidler)
;;    Added provide and some explanation of how to use.
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:
(require 'linum)

(defcustom linum-disabled-modes-list '(eshell-mode wl-summary-mode compilation-mode org-mode text-mode dired-mode pdf-view-mode)
  "* List of modes disabled when global linum mode is on"
  :type '(repeat (sexp :tag "Major mode"))
  :tag " Major modes where linum is disabled: "
  :group 'linum
  )
(defcustom linum-disable-starred-buffers 't
  "* Disable buffers that have stars in them like *Gnu Emacs*"
  :type 'boolean
  :group 'linum)

(defun linum-on ()
  "* When linum is running globally, disable line number in modes defined in `linum-disabled-modes-list'. Changed by linum-off. Also turns off numbering in starred modes like *scratch*"

  (unless (or (minibufferp) (member major-mode linum-disabled-modes-list)
              (and linum-disable-starred-buffers (string-match "*" (buffer-name)))
              )
    (linum-mode 1)))

(provide 'linum-off)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; linum-off.el ends here
