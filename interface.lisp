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

(defun simple-account-main ()
  (sb-int:with-float-traps-masked (:divide-by-zero)
    (within-main-loop
      (let ((window (make-instance 'gtk-window
                                   :title "Simple Accounting" :type :toplevel
                                   :default-width 800 :default-height 600))
            (abbrev-entry (make-instance 'gtk-entry :max-length 5))
            (account-name-entry (make-instance 'gtk-entry :max-length 100))
            (accounts-table (make-table (sad:account-id "guint")
                                        (sad:abbrev "gchararray" "Abbrev")
                                        (sad:account-name "gchararray" "Account Name"))))
        (g-signal-connect window "destroy" (ilambda+ (format t "Leaving ..~%")
                                                (leave-gtk-main)))
        (labels ((load-accounts-table ()
                   (fill-table accounts-table (sad:all-accounts)))
                 (selected-account ()
                   (sad:account-by-id (table-selected-cell accounts-table 0)))
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
          (gtk-container-add
           window
           (vertically
            (buttons-with-actions "add"    #'add
                                  "read"   #'read%
                                  "update" #'update
                                  "delete" #'delete%)
            (horizontally abbrev-entry account-name-entry)
            (view accounts-table))))
        (gtk-widget-show-all window)))))


;; (simple-account-main)
