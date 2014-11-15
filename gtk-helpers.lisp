(in-package :simple-accounting-interface)

;;; packing helper macros
(bind-multi ((macroname vertically horizontally)
             (direction :vertical :horizontal))
  (defmacro! macroname (&rest widgets)
    ;; todo support for packing and expansion options
    `(let ((,g!box (make-instance 'gtk-box :orientation direction)))
       ,@(mapcar #`(gtk-container-add ,g!box ,a1) widgets)
       ,g!box)))

(defun buttons-with-actions (&rest labels+actions)
  "Return a horizontal box for the alternating lists of labels and actions"
  (let ((button-box (make-instance 'gtk-box
                                   :orientation :horizontal
                                   :homegeneous t)))
    (dolist (label+action (group labels+actions 2))
      (let ((button (make-instance 'gtk-button :label (first label+action))))
        (awhen (second label+action) (g-signal-connect button "clicked" it))
        (gtk-container-add button-box button)))
    button-box))

(defmacro with-entries (entries &body body)
  "Bind the symbols designating `entries' to their respective `text'
property."
  (let ((syms (list->gensyms :entry entries)))
    `(let ,(mapcar #'list syms entries)
       (symbol-macrolet ,(mapcar #2`(,a1 (gtk-entry-text ,a2)) entries syms)
         ,@body))))

;;; viewing table with `gtk-tree-view'
(defclass table ()
  ((store-class :initarg :store-class
                :reader store-class)
   (model :initarg :model
          :accessor model)
   (view :accessor view)
   (column-accessors :initarg :column-accessors
                     :initform nil
                     :accessor column-accessors))
  (:documentation "TODO"))

(create-standard-print-object table view "on" model "with" column-accessors (store-class))

(defun init-table% (column-types column-accessors column-labels)
  (let ((table (make-instance 'table
                              :store-class 'gtk-list-store
                              :model (make-instance 'gtk-list-store
                                                    :column-types column-types)
                              :column-accessors column-accessors)))
    (setf (view table)
          (make-instance 'gtk-tree-view :model (model table)))
    (dolist (column-label column-labels)
      (gtk-tree-view-append-column
       (view table)
       (gtk-tree-view-column-new-with-attributes
        (cdr column-label)
        (make-instance 'gtk-cell-renderer-text)
        "text" (car column-label))))
    table))

(defmacro make-table (&rest columns)
  "Every column should have the shape (accessor type &optional label).
  Only columns where `label' is present are displayed."
  `(init-table% (list ,@(mapcar #'second columns))
                (list ,@(mapcan #`(#',(first a1)) columns))
                (list ,@(filter* (lambda (n col) (awhen (third col) `(cons ,n ,it)))
                                 (lrange columns) columns))))

(defun fill-table (table list)
  (with-slots (model column-accessors) table
    (gtk-list-store-clear model)
    (dolist (item list)
      (apply #'gtk-list-store-set model (gtk-list-store-append model)
             (mapcar (lambda (a) (funcall a item)) column-accessors)))))

(defun table-selected-cell (table &optional (column 0))
  (awhen (gtk-tree-selection-get-selected (gtk-tree-view-get-selection (view table)))
    (first (gtk-tree-model-get (model table) it column))))
