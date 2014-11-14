(defsystem simple-accounting
  :depends-on (ol-utils
               clsql
               clsql-sqlite3)
  :serial t
  :components ((:file "packages")))
