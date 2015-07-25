(in-package :simple-accounting-data)

(in-readtable clsql-readtable)

(defun my-date-format (stamp)
  (format nil "~4,'0D-~2,'0D-~2,'0D"
          (local-time:timestamp-year stamp)
          (local-time:timestamp-month stamp)
          (local-time:timestamp-day stamp)))

(defmacro! with-month-filter ((month year) &body body)
  `(let* ((,g!stamp (ol-date-utils:encode-date 1 (or month 1) ,year))
          (,g!next-stamp (local-time:timestamp+ ,g!stamp 1 (if month :month :year)))
          (month-condition [and [<= (my-date-format ,g!stamp) [payment-date]]
                           [< [payment-date] (my-date-format ,g!next-stamp)]]))
     ,@body))

(defun payments-per-month (month year)
  (with-month-filter (month year)
    (select 'payment :where month-condition :order-by 'payment-date :flatp t)))

;; (payments-per-month nil 2015)

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

;; (accounts-per-month 4 2015)

;; (total-per-month 4 2015)
