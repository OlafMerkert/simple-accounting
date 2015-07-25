(in-package :ol-user)

(defpackage :simple-accounting-data
  (:nicknames :sad)
  (:use :cl :ol :clsql
        :clsql-helpers)
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
   #:all-accounts
   #:account-by-id
   #:all-payments
   #:*simple-accounting-db*
   #:payment-by-id
   #:make-db-snapshot
   #:payments-per-month
   #:accounts-total
   #:accounts-per-month
   #:total-total
   #:total-per-month))

(defpackage :simple-accounting-interface
  (:nicknames :sai)
  (:use :cl :ol :iterate
        :gtk :gdk :gdk-pixbuf :gobject :glib :gio :pango :cairo)
  (:export
   #:simple-account))

#|(eval-when (:load-toplevel :execute)
  (sb-int:set-floating-point-modes :traps nil))|#
