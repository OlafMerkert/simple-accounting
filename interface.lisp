(in-package :simple-accounting-interface)

(defmacro setf3 (&rest args)
  "Transform (setf3 a1 a2 a3 b1 b2 b3) into (setf3 a1 a2 a2 a3 b1 b2 b2 b3)."
  (labels ((rec (list)
             (case (length list)
               (0 nil)
               (1 (error "invalid setf3"))
               (2 list)
               (t (list* (first list) (second list) (second list) (third list)
                         (rec (nthcdr 3 list)))))))
    `(setf ,@(rec args))))

(defun accounts-manager ()
  (let ((abbrev-entry (make-instance 'gtk-entry :max-length 5))
        (account-name-entry (make-instance 'gtk-entry :max-length 100))
        (accounts-table (make-list-view (sad:account-id "guint")
                                        (sad:abbrev "gchararray" "Abbrev")
                                        (sad:account-name "gchararray" "Account Name"))))
    (labels ((load-accounts-table ()
               (fill-model accounts-table (sad:all-accounts)))
             (selected-account ()
               (sad:account-by-id (selected-cell accounts-table 0)))
             (add (&rest args)
               (declare (ignore args))
               (with-entries (abbrev-entry account-name-entry)
                 (when (length>0 account-name-entry)
                   (clsql-sys:update-records-from-instance
                    (make-instance 'sad:account :abbrev abbrev-entry
                                   :account-name account-name-entry))
                   (setf abbrev-entry ""
                         account-name-entry "")
                   (load-accounts-table))))
             (read% (&rest args)
               (declare (ignore args))
               (awhen  (selected-account)
                 (with-entries (abbrev-entry account-name-entry)
                   (setf abbrev-entry (slot-value it 'sad:abbrev)
                         account-name-entry (slot-value it 'sad:account-name)))))
             (update (&rest args)
               (declare (ignore args))
               (awhen (selected-account)
                 (with-entries (abbrev-entry account-name-entry)
                   (when (length>0 account-name-entry)
                     (setf3 (slot-value it 'sad:abbrev) abbrev-entry ""
                            (slot-value it 'sad:account-name) account-name-entry "")
                     (clsql-sys:update-records-from-instance it)
                     (load-accounts-table)))))
             (delete% (&rest args)
               (declare (ignore args))
               (awhen (selected-account)
                 (clsql-sys:delete-instance-records it)
                 (load-accounts-table))))
      (load-accounts-table)
      (vertically
       (buttons-with-actions "add"    #'add
                             "read"   #'read%
                             "update" #'update
                             "delete" #'delete%)
       (horizontally abbrev-entry account-name-entry)
       :expand
       (view accounts-table)))))

(defun calendar->sql-date (calendar)
  (clsql-sys:make-date
   :year (gtk-calendar-year calendar)
   :month (gtk-calendar-month calendar)
   :day (gtk-calendar-day calendar)))

(defun load-date-in-calendar (date calendar)
  (mvbind (day month year) (clsql-sys:decode-date date)
    (setf (gtk-calendar-year calendar) year
          (gtk-calendar-month calendar) month
          (gtk-calendar-day calendar) day)))

(defun sql-date->string (date)
  (mvbind (day month year) (clsql-sys:decode-date date)
    (format nil "~4,'0D-~2,'0D-~2,'0D"
            year month day)))

(defun payments-recorder ()
  (let ((date-entry (make-instance 'gtk-calendar))
        (account-entry (make-combo-box (sad:account-id "guint")
                                       (sad:account-name "gchararray" t)))
        (amount-entry (make-instance 'gtk-spin-button
                                     :adjustment (make-instance 'gtk-adjustment :value 0
                                                                :lower 0
                                                                :upper 10000
                                                                :step-increment 1
                                                                :page-increment 10)
                                     :digits 2
                                     :numeric t
                                     :xalign 1))
        (payments-table (make-list-view (sad:payment-id "guint")
                                        ((lambda (p) (sql-date->string (sad:payment-date p)))
                                         "gchararray" "Date")
                                        ((lambda (p) (sad:account-name (sad:payment-account p))) "gchararray" "Account")
                                        (sad:amount "gfloat" "Amount"))))
    (labels ((load-account-entry ()
               (fill-model account-entry (sad:all-accounts)))
             (load-payments-table ()
               (fill-model payments-table (sad:all-payments)))
             (selected-account-id ()
               (selected-cell account-entry))
             #|(selected-account ()
               (sad:account-by-id (selected-cell account-entry)))|#
             (selected-payment ()
               (sad:payment-by-id (selected-cell payments-table)))
             (add (&rest args)
               (declare (ignore args))
               (let ((pmt (make-instance 'sad:payment :payment-account-id (selected-account-id)
                                         :payment-date (calendar->sql-date date-entry)
                                         :amount (gtk-spin-button-value amount-entry))))
                 (format t "~A~%" pmt)
                 (clsql-sys:update-records-from-instance
                  pmt))
               (load-payments-table))
             (read% (&rest args)
               (declare (ignore args))
               (awhen (selected-payment)
                 (load-date-in-calendar (sad:payment-date it) date-entry)
                 (setf (selected-cell account-entry) (sad:payment-account-id it)
                       (gtk-spin-button-value amount-entry) (sad:amount it))))
             (update (&rest args)
               (declare (ignore args))
               (awhen (selected-payment)
                 (setf (sad:payment-account-id it) (selected-account-id)
                       (sad:payment-date it) (calendar->sql-date date-entry)
                       (sad:amount it) (gtk-spin-button-value amount-entry))
                 (clsql-sys:update-records-from-instance it)
                 (load-payments-table)))
             (delete% (&rest args)
               (declare (ignore args))
               (awhen (selected-payment)
                 (clsql-sys:delete-instance-records it)
                 (load-payments-table))))
      (load-account-entry)
      (load-payments-table)
      (vertically
       (buttons-with-actions "add" #'add
                             "read" #'read%
                             "update" #'update
                             "delete" #'delete%)
       (horizontally date-entry
                     (aprog1 (make-instance 'gtk-alignment :yscale 0 :yalign 0.5)
                       (gtk-container-add it (box account-entry)))
                     amount-entry)
       :expand
       (view payments-table)))))

(defun simple-account-main ()
  (sb-int:with-float-traps-masked (:divide-by-zero)
    (within-main-loop
      (let ((window (make-instance 'gtk-window
                                   :title "Simple Accounting" :type :toplevel
                                   :default-width 800 :default-height 600)))
        (g-signal-connect window "destroy" (ilambda+ (format t "Leaving ..~%")
                                                (leave-gtk-main)))
        ;; (gtk-container-add window (accounts-manager))
        (gtk-container-add window (payments-recorder))
        (gtk-widget-show-all window)))))

(defun simple-account ()
  (unless sad:*simple-accounting-db*
    (sad:connect-simple-accounting))
  (simple-account-main))

;; (simple-account-main)
