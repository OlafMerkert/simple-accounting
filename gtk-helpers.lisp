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
(defclass tree-model-helper ()
  ((store-class :reader store-class)
   (model :initarg :model
          :accessor model)
   (column-accessors :initarg :column-accessors
                     :initform nil
                     :accessor column-accessors))
  (:documentation "TODO"))

(create-standard-print-object tree-model-helper store-class "for" column-accessors)

(defmethod initialize-instance :after ((x tree-model-helper) &key column-types)
  (with-slots (store-class model) x
    (setf store-class 'gtk-list-store
          model (make-instance 'gtk-list-store
                               :column-types column-types))))

(defclass table (tree-model-helper)
  ((view :accessor view))
  (:documentation "TODO"))

(defmethod initialize-instance :after ((x table) &key column-labels)
  (with-slots (model view) x
    (setf view 
          (make-instance 'gtk-tree-view :model model))
    (dolist (column-label column-labels)
      (gtk-tree-view-append-column
       view
       (gtk-tree-view-column-new-with-attributes
        (cdr column-label)
        (make-instance 'gtk-cell-renderer-text)
        "text" (car column-label))))))

(defmacro make-table (&rest columns)
  "Every column should have the shape (accessor type &optional label).
  Only columns where `label' is present are displayed."
  `(make-instance 'table
                  :column-accessors (list ,@(mapcan #`(#',(first a1)) columns))
                  :column-types (list ,@(mapcar #'second columns))
                  :column-labels (list ,@(filter* (lambda (n col) (awhen (third col) `(cons ,n ,it)))
                                                  (lrange columns) columns))))

(defmethod fill-table ((x tree-model-helper) (list list))
  (with-slots (model column-accessors) x
    (gtk-list-store-clear model)
    (dolist (item list)
      (apply #'gtk-list-store-set model (gtk-list-store-append model)
             (mapcar (lambda (a) (funcall a item)) column-accessors)))))

(defmethod selected-cell ((table table) &optional (column 0))
  (with-slots (model view) table
    (awhen (gtk-tree-selection-get-selected (gtk-tree-view-get-selection view))
      (first (gtk-tree-model-get model it column)))))
