(in-package :simple-accounting-data)

(locally-enable-sql-reader-syntax)

;;; for starters, we just need two simple classes: accounts and
;;; payments
(def-view-class account ()
  ((account-id :type integer :initform (sequence-next 'account-id)
               :db-kind :key :db-constraints :not-null)
   (abbrev :type (string 5) :initarg :abbrev :nulls-ok t)
   (account-name :type string :initarg :account-name :nulls-ok nil)
   (payments :db-kind :join
             :db-info (:join-class payment
                                   :home-key account-id
                                   :foreign-key payment-account-id
                                   :set t))))

(def-view-class payment ()
  ((payment-id :type integer :initform (sequence-next 'payment-id)
               :db-kind :key :db-constraints :not-null)
   (payment-date :type date :initarg :payment-date :nulls-ok nil)
   (payment-account-id :type integer :initarg :payment-account-id :nulls-ok nil)
   (payment-account :db-kind :join
                    :db-info (:join-class account
                                          :home-key payment-account-id
                                          :foreign-key account-id
                                          :set nil))
))

(defun ensure-sequences (sequences)
  "Make sure the sequence identified by the given list of symbols
actually exist in the database. Return the symbols for which we
created sequences."
  (filter (lambda (seq)
            (unless (sequence-exists-p seq)
              (create-sequence seq)
              seq))
          sequences))

(defun ensure-tables (tables)
  "Create the tables identified by the given list of symbols if they
do not exist yet, and return the symbols of created tables."
  (filter (lambda (table)
            (unless (table-exists-p table)
              (create-view-from-class table)
              table))
          tables))

(defun ensure-tables-with-indices (tables-and-indices)
  "Extend `create-tables': if an element of `tables-and-indices' is a
`cons', treat the `car' as table names and make sure every column
mentioned in `cdr' has the appropriate (single) column index."
  (let (created)
    (mapc (lambda (table-and-indices)
            (dbind (table . indices) (mklist table-and-indices)
              (unless (table-exists-p table)
                (create-view-from-class table)
                (push table created))
              (dolist (index indices)
                (let ((index-id (symb table '- index '-index)))
                  (unless (index-exists-p index-id)
                    (create-index index-id :on table :attributes (list index))
                    (push index-id created))))))
          tables-and-indices)
    created))

(defmacro define-sqlite3-database (name path &key sequences tables)
  (let ((db-name (symb '* name '-db*)))
    `(progn
       (defvar ,db-name nil)

       (defun ,(symb 'setup- name) ()
         (append (ensure-sequences ,sequences)
                 (ensure-tables-with-indices ,tables)))

       (defun ,(symb 'connect- name) (&optional (path ,path))
         (setf ,db-name (connect (list path)
                                 :database-type :sqlite3))
         (execute-command "PRAGMA synchronous=OFF" :database ,db-name)
         (setf *default-caching* nil)
         (cons ',db-name (,(symb 'setup- name)))))))

(define-sqlite3-database simple-accounting "/home/olaf/tmp/haushaltsbuch.sqlite"
  :sequences '(account-id payment-id)
  :tables '((account account-name)
            (payment payment-date payment-account-id)))

(connect-simple-accounting)
