;; cl-mssql.asd

(defsystem :mssql
  :depends-on (#:cffi #:iterate #:garbage-pools #:bordeaux-threads)
  :components
  ((:module :src
            :components
            ((:file "package")
             (:file "mssql" :depends-on ("package"))
             (:file "connection" :depends-on ("mssql"))
             (:file "query" :depends-on ("connection"))
             ))))