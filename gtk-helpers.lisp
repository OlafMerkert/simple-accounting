(in-package :simple-accounting-interface)

;;; packing helper macros
(defpar *padding-horizontal* 3)
(defpar *padding-vertical* 2)

(ew
 (defun push-if (pred list)
   "Given a list like (:a :b c d :e f) produce the list of
  lists ((c :b :a) (d) (f :e)) if `pred' is `keywordp'."
   (labels ((rec (list acc matches)
              (if (null list)
                  (if matches (cons matches acc) acc)
                  (if (funcall pred (car list))
                      (rec (cdr list) acc (cons (car list) matches))
                      (rec (cdr list) (cons (cons (car list) matches) acc) nil)))))
     (nreverse (rec list nil nil)))))

(bind-multi ((macroname vertically horizontally)
             (direction :vertical :horizontal)
             (padding *padding-vertical* *padding-horizontal*))
  (defmacro! macroname (&rest widgets)
    `(let ((,g!box (make-instance 'gtk-box :orientation direction))
           (,g!padding padding))
       ,@(mapcar #`(gtk-box-pack-start ,g!box ,(car a1) :padding ,g!padding
                                       ,@(mapcan #`(,a1 t) (rest a1))
                                       :expand nil)
                 (push-if #'keywordp widgets))
       ,g!box)))

(defun buttons-with-actions (&rest labels+actions)
  "Return a horizontal box for the alternating lists of labels and actions"
  (let ((button-box (make-instance 'gtk-box
                                   :orientation :horizontal
                                   :homegeneous t)))
    (dolist (label+action (partition labels+actions 2))
      (let ((button (make-instance 'gtk-button :label (first label+action))))
        (awhen (second label+action) (g-signal-connect button "clicked" it))
        (gtk-box-pack-start button-box button :expand nil :padding *padding-horizontal*)))
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

(defmethod find-cell-by-value ((x tree-model-helper) value &optional (column 0))
  (with-slots (model) x
    (do ((iter (gtk-tree-model-get-iter-first model)
               (gtk-tree-model-iter-next model iter)))
        ((not iter))
      (when (eql value (first (gtk-tree-model-get model iter column)))
        (return iter)))))

(defmethod fill-model ((x tree-model-helper) (list list))
  (with-slots (model column-accessors) x
    (gtk-list-store-clear model)
    (dolist (item list)
      (apply #'gtk-list-store-set model (gtk-list-store-append model)
             (mapcar (lambda (a) (funcall a item)) column-accessors)))))

(defsetf selected-cell (obj &optional (column 0)) (value)
  `(set-selected-cell ,obj ,value ,column))

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

(defmethod selected-cell ((list-view list-view) &optional (column 0))
  (with-slots (model view) list-view
    (awhen (gtk-tree-selection-get-selected (gtk-tree-view-get-selection view))
      (first (gtk-tree-model-get model it column)))))

(defmethod set-selected-cell ((list-view list-view) value &optional column)
  (when value
    (with-slots (view) list-view
      (awhen (find-cell-by-value list-view value column)
        (gtk-tree-selection-select-iter (gtk-tree-view-get-selection view) it)
        value))))

(defmethod fill-model :around ((list-view list-view) (list list))
  (let ((selection (selected-cell list-view)))
    (call-next-method)
    (setf (selected-cell list-view) selection)))

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

(defmethod set-selected-cell ((combo-box combo-box) value &optional (column 0))
  (when value
    (with-slots (box) combo-box
      (awhen (find-cell-by-value combo-box value column)
        (gtk-combo-box-set-active-iter box it)
        value))))

(defmethod fill-model :around ((combo-box combo-box) (list list))
  (let ((selection (selected-cell combo-box)))
    (call-next-method)
    (unless (setf (selected-cell combo-box) selection)
      (gtk-combo-box-set-active (box combo-box) 0))))

;;; multipage apps with notebooks
(defmacro! notebook (&rest components)
  "Every component should have the shape (label . call) where `call'
ought to be a valid function invocation returning 2 values, a widget
and a function which will be called every time we switch to the
widget. "
  `(let ((,g!notebook (make-instance 'gtk-notebook :enable-popup t))
         (,g!updaters (make-array ,(length components))))
     ,@(mapcar #2`(mvbind (,g!component ,g!updater) ,(rest  a1)
                    (gtk-notebook-add-page ,g!notebook ,g!component
                                           (make-instance 'gtk-label :label ,(first a1)))
                    (setf (aref ,g!updaters ,a2) ,g!updater))
               components (lrange components))
     (g-signal-connect ,g!notebook "switch-page" (ilambda (,g!notebook ,g!page ,g!page-nr &rest ,g!data)
                                                   (funcall (aref ,g!updaters ,g!page-nr))))
     ,g!notebook))

;;; filtering tables by a given month and year
(defun current-month ()
  (local-time:timestamp-month (local-time:today)))

(defun current-year ()
  (local-time:timestamp-year (local-time:today)))

(defun make-spinner (&key (min most-negative-fixnum) (max most-positive-fixnum) (default 0) (digits 0))
  (make-instance 'gtk-spin-button
                 :adjustment (make-instance 'gtk-adjustment :value default
                                            :lower min
                                            :upper max 
                                            :step-increment 1
                                            :page-increment 10)
                 :digits digits
                 :numeric t
                 :xalign 1))

(defclass month-year-input ()
  ((month-widget :initform (make-spinner :min 1 :max 12 :default (current-month))
                 :reader month-widget)
   (year-widget :initarg :year-widget
                :initform (make-spinner :min 1900 :default (current-year))
                :reader year-widget)
   (mode-widgets :reader mode-widgets))
  (:documentation "TODO"))

(defmethod initialize-instance :after ((month-year-input month-year-input) &key)
  ;; select between no filtering, per year or per month
  (let* ((all (make-instance 'gtk-radio-button :label "All"))
         (year (make-instance 'gtk-radio-button :group all :label "Year" ))
         (month (make-instance 'gtk-radio-button :group all :label "Month")))
    (setf (slot-value month-year-input 'mode-widgets) (list all year month))))

(defmethod selector-active-p ((month-year-input month-year-input))
  (not (eq :all (current-mode month-year-input))))

(defmethod month ((month-year-input month-year-input))
  (when (eq :month (current-mode month-year-input))
    ;; need to make sure we return an integer
    (floor (gtk-spin-button-value (month-widget month-year-input)))))

(defmethod year ((month-year-input month-year-input))
  (floor (gtk-spin-button-value (year-widget month-year-input))))

(defmethod widget ((month-year-input month-year-input))
  (with-slots (mode-widgets month-widget year-widget) month-year-input
    (horizontally
     (elt mode-widgets 0) (elt mode-widgets 1) (elt mode-widgets 2)
     month-widget year-widget)))

(defmethod current-mode ((month-year-input month-year-input))
  (with-slots (mode-widgets) month-year-input
    (cond ((gtk-toggle-button-active (elt mode-widgets 0)) :all)
          ((gtk-toggle-button-active (elt mode-widgets 1)) :year)
          ((gtk-toggle-button-active (elt mode-widgets 2)) :month)
          (t :all))))

(defmethod update-disabled-state ((month-year-input month-year-input))
  (ecase (current-mode month-year-input)
    (:all (setf (gtk-widget-sensitive (month-widget month-year-input)) nil
                (gtk-widget-sensitive (year-widget month-year-input)) nil))
    (:year (setf (gtk-widget-sensitive (month-widget month-year-input)) nil
                 (gtk-widget-sensitive (year-widget month-year-input)) t))
    (:month (setf (gtk-widget-sensitive (month-widget month-year-input)) t
                (gtk-widget-sensitive (year-widget month-year-input)) t))))

(defmethod advise-change-action ((month-year-input month-year-input) function)
  ;; call the `function' every time we get a state change on the
  ;; selector
  (with-slots (mode-widgets) month-year-input
    (dolist (w mode-widgets)
      (g-signal-connect w  "toggled" (ilambda+ (update-disabled-state month-year-input)
                                          (funcall function)))))
  ;; same for the spinners
  (g-signal-connect (gtk-spin-button-adjustment (month-widget month-year-input)) "value-changed" (swallow function))
  (g-signal-connect (gtk-spin-button-adjustment (year-widget month-year-input)) "value-changed" (swallow function)))

