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

(defun fill-table (model list)
  (gtk-list-store-clear model)
  (dolist (item list)
    (apply #'gtk-list-store-set model (gtk-list-store-append model) item)))

(defun fill-table-map (model list &rest accessors)
  (gtk-list-store-clear model)
  (dolist (item list)
    (apply #'gtk-list-store-set model (gtk-list-store-append model)
           (mapcar (lambda (a) (funcall a item)) accessors))))

(defun table-columns-labels (table &rest args)
  (dolist (col+label (group args 2))
    (gtk-tree-view-append-column
     table
     (gtk-tree-view-column-new-with-attributes
      (second col+label)
      (make-instance 'gtk-cell-renderer-text)
      "text" (first col+label)))))

(bind-multi ((macroname vertically horizontally)
             (direction :vertical :horizontal))
  (defmacro! macroname (&rest widgets)
    ;; todo support for packing and expansion options
    `(let ((,g!box (make-instance 'gtk-box :orientation direction)))
       ,@(mapcar #`(gtk-container-add ,g!box ,a1) widgets)
       ,g!box)))

(defun simple-account-main ()
  (sb-int:with-float-traps-masked (:divide-by-zero)
    (within-main-loop
      (let* ((window (make-instance 'gtk-window
                                    :type :toplevel
                                    :title "Simple Accounting"
                                    :default-width 800
                                    :default-height 600)) 
             (abbrev-entry (make-instance 'gtk-entry :max-length 5))
             (account-name-entry (make-instance 'gtk-entry :max-length 100))
             (accounts-model (make-instance 'gtk-list-store :column-types '("guint" "gchararray" "gchararray")))
             (accounts-table (make-instance 'gtk-tree-view :model accounts-model)))
        (g-signal-connect window "destroy" (ilambda+ (format t "Leaving ..~%")
                                                (leave-gtk-main)))
        (table-columns-labels accounts-table 1 "Abbrev" 2 "Account name")
        (labels ((load-accounts-table ()
                   (fill-table-map accounts-model
                                   (sad:all-accounts)
                                   #'sad:account-id
                                   #'sad:abbrev
                                   #'sad:account-name))
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
                 (selected-account-id ()
                   (awhen (gtk-tree-selection-get-selected (gtk-tree-view-get-selection accounts-table))
                     (first (gtk-tree-model-get accounts-model it 0))))
                 (selected-account ()
                   (sad:account-by-id (selected-account-id)))
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
                         (setf (slot-value it 'sad:abbrev) abbrev-entry
                               (slot-value it 'sad:account-name) account-name-entry
                               abbrev-entry ""
                               account-name-entry "")
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
            (buttons-with-actions "add" #'add
                                  "read" #'read%
                                  "update" #'update
                                  "delete" #'delete%)
            (horizontally abbrev-entry account-name-entry)
            accounts-table))) 
        (gtk-widget-show-all window)))))

;; (simple-account-main)

