;; packages.lisp

(defpackage :mssql
  (:use :cl :iter :cffi)
  (:export #:*database*
           #:database-connection
           #:connect
           #:connected-p
           #:disconnect
           #:connect-toplevel
           #:disconnect-toplevel
           #:with-connection
           #:query))