(in-package :simple-accounting-data)

(file-enable-sql-reader-syntax)

;;; for starters, we just need two simple classes: accounts and
;;; payments
(def-view-class account ()
  ((account-id :accessor account-id
               :type integer
               :initform (sequence-next 'account-id)
               :db-kind :key
               :db-constraints :not-null)
   (abbrev :accessor abbrev
           :type (string 5)
           :initarg :abbrev
           :nulls-ok t)
   (account-name :accessor account-name
                 :type string
                 :initarg :account-name
                 :nulls-ok nil)
   (payments :reader payments
             :db-kind :join
             :db-info (:join-class payment
                                   :home-key account-id
                                   :foreign-key payment-account-id
                                   :set t))))

(create-standard-print-object account (account-id abbrev) account-name)

(def-view-class payment ()
  ((payment-id :accessor payment-id
               :type integer
               :initform (sequence-next 'payment-id)
               :db-kind :key :db-constraints :not-null)
   (payment-date :accessor payment-date
                 :type date
                 :initarg :payment-date
                 :nulls-ok nil)
   (payment-account-id :accessor payment-account-id
                       :type integer
                       :initarg :payment-account-id
                       :nulls-ok nil)
   (payment-account :accessor payment-account
                    :db-kind :join
                    :db-info (:join-class account
                                          :home-key payment-account-id
                                          :foreign-key account-id
                                          :set nil
                                          :retrieval :immediate))
   (amount :accessor amount
           :type float
           :initarg :amount
           :nulls-ok nil)))

(create-standard-print-object payment (payment-id) payment-date amount payment-account)

(define-sqlite3-database simple-accounting "/home/olaf/data/simple-accounting/haushaltsbuch.sqlite"
  :sequences '(account-id payment-id)
  :tables '((account account-name)
            (payment payment-date payment-account-id)))

;; (connect-simple-accounting)
;; (setup-simple-accounting)

(defun database-pathname (database)
  (pathname (first (slot-value database 'clsql-sys:connection-spec))))

(defun pathname-with-date-stamp (pathname)
  (make-pathname :defaults pathname :name (format nil "~A.~A"
                                              (pathname-name pathname)
                                              (ol-date-utils:print-date/reverse
                                               (local-time:today)))))

(defun make-db-snapshot (&optional (database *simple-accounting-db*))
  (when database
    (let* ((path (database-pathname database))
           (new-path (pathname-with-date-stamp path)))
      (uiop/stream:copy-file path new-path))))

(defun all-accounts () (select 'account :order-by 'account-name :flatp t))

(defun account-by-id (id)
  (when id (first (select 'account :where [= [account-id] id] :flatp t :limit 1))))

(defun all-payments () (select 'payment :order-by 'payment-date :flatp t))

(defun payment-by-id (id)
  (when id (first (select 'payment :where [= [payment-id] id] :flatp t :limit 1))))
