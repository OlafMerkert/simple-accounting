(defsystem simple-accounting
  :depends-on (ol-utils
               clsql
               clsql-sqlite3
               cl-cffi-gtk)
  :serial t
  :components ((:file "packages")
               (:file "database")
               (:file "gtk-helpers")
               (:file "interface")))
