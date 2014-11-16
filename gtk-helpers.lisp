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

;;; viewing list-view with `gtk-tree-view'
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

(defclass list-view (tree-model-helper)
  ((view :accessor view))
  (:documentation "TODO"))

(defmethod initialize-instance :after ((x list-view) &key column-labels)
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

(defmacro make-list-view (&rest columns)
  "Every column should have the shape (accessor type &optional label).
  Only columns where `label' is present are displayed."
  `(make-instance 'list-view
                  :column-accessors (list ,@(mapcan #`(#',(first a1)) columns))
                  :column-types (list ,@(mapcar #'second columns))
                  :column-labels (list ,@(filter* (lambda (n col) (awhen (third col) `(cons ,n ,it)))
                                                  (lrange columns) columns))))

(defmethod fill-model ((x tree-model-helper) (list list))
  (with-slots (model column-accessors) x
    (gtk-list-store-clear model)
    (dolist (item list)
      (apply #'gtk-list-store-set model (gtk-list-store-append model)
             (mapcar (lambda (a) (funcall a item)) column-accessors)))))

(defmethod selected-cell ((list-view list-view) &optional (column 0))
  (with-slots (model view) list-view
    (awhen (gtk-tree-selection-get-selected (gtk-tree-view-get-selection view))
      (first (gtk-tree-model-get model it column)))))

(defclass combo-box (tree-model-helper)
  ((box :accessor box))
  (:documentation "TODO"))

(defmethod initialize-instance :after ((combo-box combo-box) &key visible-columns)
  (with-slots (model box) combo-box
    (setf box (make-instance 'gtk-combo-box :model model))
    (dolist (col visible-columns)
      (let ((renderer (make-instance 'gtk-cell-renderer-text)))
        (gtk-cell-layout-pack-start box renderer)
        (gtk-cell-layout-add-attribute box renderer "text" col)))))

(defmacro make-combo-box (&rest columns)
  "Every column should have the shape (accessor type &optional visible).
  Only columns where `visible' is non-nil are displayed."
  `(make-instance 'combo-box
                  :column-accessors (list ,@(mapcan #`(#',(first a1)) columns))
                  :column-types (list ,@(mapcar #'second columns))
                  :visible-columns (list ,@(filter* (lambda (n col) (when (third col) n))
                                                    (lrange columns) columns))))

(defmethod selected-cell ((combo-box combo-box) &optional (column 0))
  (with-slots (model box) combo-box
    (awhen (gtk-combo-box-get-active-iter box)
      (first (gtk-tree-model-get model it column)))))
