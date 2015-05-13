(in-package :simple-accounting-data)

(in-readtable clsql-readtable)

(defun account-per-month (month)
  (select [payment-account-id] [sum [amount]] :all t :from [payment]))

(defun my-date-format (stamp)
  (format nil "~4,'0D-~2,'0D-~2,'0D"
          (local-time:timestamp-year stamp)
          (local-time:timestamp-month stamp)
          (local-time:timestamp-day stamp)))

(defmacro! with-month-filter ((month year) &body body)
  `(let* ((,g!stamp (ol-date-utils:encode-date 1 ,month ,year))
          (,g!next-stamp (local-time:timestamp+ ,g!stamp 1 :month))
          (month-condition [and [<= (my-date-format ,g!stamp) [payment-date]]
                           [< [payment-date] (my-date-format ,g!next-stamp)]]))
     ,@body))


(defun accounts-per-month (month year)
  (with-month-filter (month year)
        (sort (mapcar (lambda (row)
               (dbind (id sum) row
                 (list (account-by-id id) sum)))
             (select [payment-account-id] [sum [amount]]
                     :from [payment]
                     :group-by [payment-account-id]
                     :where month-condition))
              #'>= :key #'cadr)))

(defun total-per-month (month year)
  (with-month-filter (month year)
    (or (caar (select [sum [amount]]
                      :from [payment]
                      :where month-condition))
        0)))

(defun totals-per-month ()
  (mapcar (lambda (stamp) (list stamp
                           (total-per-month (local-time:timestamp-month stamp)
                                            (local-time:timestamp-year stamp))))
          (months-table [payment] [payment-date])))

;; (totals-per-month)

;; (account-per-month 4 2015)

;; (total-per-month 4 2015)
