(defsystem simple-accounting
  :depends-on (ol-utils
               clsql
               clsql-sqlite3
               ol-sql-utils
               cl-cffi-gtk
               ol-data-utils)
  :serial t
  :components ((:file "packages")
               (:file "database")
               (:file "statistics")
               (:file "gtk-helpers")
               (:file "interface")))
