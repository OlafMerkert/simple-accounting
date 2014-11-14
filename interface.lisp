(in-package :simple-accounting-interface)

(defun buttons-with-actions (&rest b+a)
  (let ((button-box (make-instance 'gtk-box
                                   :orientation :horizontal
                                   :homegeneous t)))
    (dolist (b+a (group b+a 2))
      (let ((button (make-instance 'gtk-button :label (first b+a))))
        (awhen (second b+a) (g-signal-connect button "clicked" it))
        (gtk-container-add button-box button)))
    button-box))

(defmacro with-entries (entries &body body)
  (let ((syms (list->gensyms :entry entries)))
    `(let ,(mapcar #'list syms entries)
       (symbol-macrolet ,(mapcar #2`(,a1 (gtk-entry-text ,a2)) entries syms)
         ,@body))))

(defmacro! fill-table (o!model list &optional (columns 1))
  (let ((syms (iter (for i from 1 to columns) (collect (gensym)))))
    `(progn
       ;; first remove everything
       (gtk-list-store-clear ,g!model)
       ;; then add one after the other
       (dolist (,g!item ,list)
         (dbind ,syms ,g!item
           ;; (format t "~A~%" ,g!item)
           (gtk-list-store-set ,g!model (gtk-list-store-append ,g!model) ,@syms))))))

(defun simple-account-main ()
  (sb-int:with-float-traps-masked (:divide-by-zero)
    (within-main-loop
      (let* ((window (make-instance 'gtk-window
                                    :type :toplevel
                                    :title "Simple Accounting"
                                    :default-width 800
                                    :default-height 600))
             (container (make-instance 'gtk-box :orientation :vertical))
             (entry-container (make-instance 'gtk-box :orientation :horizontal))
             (abbrev-entry (make-instance 'gtk-entry :max-length 5))
             (account-name-entry (make-instance 'gtk-entry :max-length 100))
             (accounts-model (make-instance 'gtk-list-store :column-types '("gchararray" "gchararray")))
             (accounts-table (make-instance 'gtk-tree-view :model accounts-model)))
        (g-signal-connect window "destroy" (ilambda+ (format t "Leaving ..~%")
                                                (leave-gtk-main)))
        (gtk-tree-view-append-column accounts-table
                                     (gtk-tree-view-column-new-with-attributes "Abbrev"
                                                                               (make-instance 'gtk-cell-renderer-text)
                                                                               "text" 0))
        (gtk-tree-view-append-column accounts-table
                                     (gtk-tree-view-column-new-with-attributes "Account name"
                                                                               (make-instance 'gtk-cell-renderer-text)
                                                                               "text" 0)) 
        (labels ((load-accounts-table ()
                   (fill-table accounts-model
                               (mapcar (lambda (account) (slot-value account 'sad:account-name))
                                       (sad:all-accounts))
                               2))
                 (add (&rest args) (declare (ignore args))
                      (with-entries (abbrev-entry account-name-entry)
                        (when (length>0 account-name-entry)
                          (clsql-sys:update-records-from-instance
                           (make-instance 'sad:account :abbrev abbrev-entry
                                          :account-name account-name-entry))
                          (setf abbrev-entry ""
                                account-name-entry "")
                          (load-accounts-table)))))
          (load-accounts-table)
          (gtk-container-add container
                             (buttons-with-actions "add" #'add
                                                   "read" nil
                                                   "update" nil
                                                   "delete" nil)))
        (dolist (entry (list abbrev-entry account-name-entry))
          (gtk-container-add entry-container entry))
        (gtk-container-add container entry-container)
        (gtk-container-add container accounts-table)
        (gtk-container-add window container)
        (gtk-widget-show-all window)))))

(simple-account-main)
