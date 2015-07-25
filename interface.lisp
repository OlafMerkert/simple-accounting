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
                   (let ((act (make-instance 'sad:account :abbrev abbrev-entry
                                             :account-name account-name-entry)))
                     (clsql-sys:update-records-from-instance act)
                     (setf abbrev-entry ""
                           account-name-entry "")
                     (load-accounts-table)
                     (setf (selected-cell accounts-table) (sad:account-id act))))))
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
      (values (vertically
               (buttons-with-actions "add"    #'add
                                     "read"   #'read%
                                     "update" #'update
                                     "delete" #'delete%)
               (horizontally abbrev-entry account-name-entry)
               :expand
               (aprog1 (make-instance 'gtk-scrolled-window)
                 (gtk-scrolled-window-add-with-viewport it (view accounts-table))))
              ;; also return function for updating models
              (ilambda+ (load-accounts-table))))))

;; in the GTK Widget, months seem to go from 0-11
(defun calendar->sql-date (calendar)
  (clsql-sys:make-date
   :year (gtk-calendar-year calendar)
   :month (+ 1 (gtk-calendar-month calendar))
   :day (gtk-calendar-day calendar)))

(defun load-date-in-calendar (date calendar)
  (mvbind (day month year) (clsql-sys:decode-date date)
    (setf (gtk-calendar-year calendar) year
          (gtk-calendar-month calendar) (- month 1)
          (gtk-calendar-day calendar) day)))

(defun sql-date->string (date)
  (mvbind (day month year) (clsql-sys:decode-date date)
    (format nil "~4,'0D-~2,'0D-~2,'0D"
            year month day)))

(defun euro-format (euro)
  (substitute #\, #\. (format nil "~$ EUR" euro)))

(defun payments-recorder ()
  (let ((date-entry (make-instance 'gtk-calendar))
        (account-entry (make-combo-box (sad:account-id "guint")
                                       (sad:account-name "gchararray" t)))
        (amount-entry (make-spinner :min 0 :max 10000 :default 0 :digits 2))
        (payments-table (make-list-view (sad:payment-id "guint")
                                        ((lambda (p) (sql-date->string (sad:payment-date p)))
                                         "gchararray" "Date")
                                        ((lambda (p) (sad:account-name (sad:payment-account p))) "gchararray" "Account")
                                        ((lambda (p) (euro-format (sad:amount p))) "gchararray" "Amount")))
        (date-selector (make-instance 'month-year-input)))
    (update-disabled-state date-selector)
    (labels ((load-account-entry ()
               (fill-model account-entry (sad:all-accounts)))
             (load-payments-table ()
               (fill-model payments-table (if (selector-active-p date-selector)
                                              (sad:payments-per-month (month date-selector) (year date-selector))
                                              (sad:all-payments))))
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
                 (dbug "payment: ~A" pmt)
                 (clsql-sys:update-records-from-instance pmt)
                 (setf (gtk-spin-button-value amount-entry) 0)
                 (load-payments-table)
                 (setf (selected-cell payments-table) (sad:payment-id pmt))))
             (read% (&rest args)
               (declare (ignore args))
               (awhen (selected-payment)
                 (dbug "amount: ~A, type: ~A" (sad:amount it) (type-of (sad:amount it)))
                 (load-date-in-calendar (sad:payment-date it) date-entry)
                 (setf (selected-cell account-entry) (sad:payment-account-id it)
                       (gtk-spin-button-value amount-entry) (sad:amount it))))
             (update (&rest args)
               (declare (ignore args))
               (awhen (selected-payment)
                 (setf (sad:payment-account-id it) (selected-account-id)
                       (sad:payment-date it) (calendar->sql-date date-entry)
                       (sad:amount it) (gtk-spin-button-value amount-entry))
                 (setf (gtk-spin-button-value amount-entry) 0)
                 (clsql-sys:update-records-from-instance it)
                 (load-payments-table)))
             (delete% (&rest args)
               (declare (ignore args))
               (awhen (selected-payment)
                 (clsql-sys:delete-instance-records it)
                 (load-payments-table))))
      (load-account-entry)
      (load-payments-table)
      (advise-change-action date-selector #'load-payments-table)
      (values (vertically
               (buttons-with-actions "add" #'add
                                     "read" #'read%
                                     "update" #'update
                                     "delete" #'delete%)
               (horizontally (aprog1 (make-instance 'gtk-alignment :yscale 0 :yalign 0.5)
                               (gtk-container-add it (box account-entry)))
                             amount-entry
                             date-entry)
               :expand
               (aprog1 (make-instance 'gtk-scrolled-window)
                 (gtk-scrolled-window-add-with-viewport it (view payments-table)))
               (widget date-selector))
              ;; second return value: a function to be called for
              ;; updating the models
              (ilambda+ (load-account-entry) (load-payments-table))))))

;;; expenses per account
(defun accounts-expenses ()
  (let ((expenses-table (make-list-view ((lambda (p) (sad:account-name (first p))) "gchararray" "Account")
                                        ((lambda (p) (euro-format (second p))) "gchararray" "Amount")))
        (total-expenses (make-instance 'gtk-label :single-line-mode t))
        (date-selector (make-instance 'month-year-input)))
    (update-disabled-state date-selector)
    (labels ((load-expenses ()
               (fill-model expenses-table 
                           (if (selector-active-p date-selector)
                               (sad:accounts-per-month (month date-selector) (year date-selector))
                               (sad:accounts-total)))
               (setf (gtk-label-label total-expenses)
                     (format nil "Total sum of expenses displayed below: ~A"
                             (euro-format
                              (if (selector-active-p date-selector)
                                  (sad:total-per-month (month date-selector) (year date-selector))
                                  (sad:total-total)))))))
      (load-expenses)
      (advise-change-action date-selector #'load-expenses)
      (values (vertically
               total-expenses
               :expand
               (aprog1 (make-instance 'gtk-scrolled-window)
                 (gtk-scrolled-window-add-with-viewport it (view expenses-table)))
               (widget date-selector))
              #'load-expenses))))

(defun tools ()
  (values (vertically
           (buttons-with-actions "Make database snapshot" (swallow #'sad:make-db-snapshot)))
          (ilambda+ ())))

(defun simple-account-main ()
  (sb-int:with-float-traps-masked (:divide-by-zero)
    (within-main-loop
      (let ((window (make-instance 'gtk-window
                                   :title "Simple Accounting" :type :toplevel
                                   :default-width 700 :default-height 900)))
        (g-signal-connect window "destroy" (ilambda+ (format t "Leaving ..~%")
                                                (leave-gtk-main)))
        (gtk-container-add window
                           (notebook ("Accounts" accounts-manager)
                                     ("Payments" payments-recorder)
                                     ("Expenses by Accounts" accounts-expenses)
                                     ("Tools"    tools)))
        (gtk-widget-show-all window)))))

(defun simple-account ()
  (unless sad:*simple-accounting-db*
    (sad:connect-simple-accounting))
  (simple-account-main))

;; (simple-account-main)
