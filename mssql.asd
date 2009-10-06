;; cl-mssql.asd

(defpackage :mysql-system
  (:use :cl :asdf))

(in-package :mysql-system)

(defsystem :mssql
  :depends-on (#:cffi #:iterate #:garbage-pools)
  :components
  ((:module :src
            :components
            ((:file "packages")
             (:file "mssql" :depends-on ("packages"))
             (:file "connection" :depends-on ("mssql"))
             (:file "query" :depends-on ("connection"))
             ))))