(in-package :ol-user)

(defpackage :simple-accounting-data
  (:nicknames :sad)
  (:use :cl :ol :clsql)
  (:export
   #:connect-simple-accounting
   #:account
   #:account-id
   #:abbrev
   #:account-name
   #:payments
   #:payment
   #:payment-id
   #:payment-date
   #:payment-account-id
   #:payment-account
   #:amount
   #:all-accounts))

(defpackage :simple-accounting-interface
  (:nicknames :sai)
  (:use :cl :ol :iterate
        :gtk :gdk :gdk-pixbuf :gobject :glib :gio :pango :cairo)
  (:export))

#|(eval-when (:load-toplevel :execute)
  (sb-int:set-floating-point-modes :traps nil))|#
