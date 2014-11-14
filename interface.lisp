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

(defun simple-account-main ()
  (sb-int:with-float-traps-masked (:divide-by-zero)
    (within-main-loop
      (let ((window (make-instance 'gtk-window
                                   :type :toplevel
                                   :title "Simple Accounting"
                                   :default-width 800
                                   :default-height 600))
            (container (make-instance 'gtk-box :orientation :vertical))
            (entry-container (make-instance 'gtk-box :orientation :horizontal))
            (abbrev-entry (make-instance 'gtk-entry :max-length 5))
            (account-name-entry (make-instance 'gtk-entry :max-length 100)))
        (g-signal-connect window "destroy" (ilambda+ (format t "Leaving ..~%")
                                                (leave-gtk-main)))
        (flet ((add (&rest args) (declare (ignore args))
                    (with-entries (abbrev-entry account-name-entry)
                      (when (length>0 account-name-entry)
                        (clsql-sys:update-records-from-instance
                         (make-instance 'sad:account :abbrev abbrev-entry
                                        :account-name account-name-entry))
                        (setf abbrev-entry ""
                              account-name-entry "")))))
          (gtk-container-add container
                             (buttons-with-actions "add" #'add
                                                   "read" nil
                                                   "update" nil
                                                   "delete" nil)))
        (dolist (entry (list abbrev-entry account-name-entry))
          (gtk-container-add entry-container entry))
        (gtk-container-add container entry-container)
        (gtk-container-add window container)
        (gtk-widget-show-all window)))))

(simple-account-main)
