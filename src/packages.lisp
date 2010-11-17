;;;; packages.lisp
;;;;
;;;; This file is part of the cl-mssql library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(defpackage #:mssql
  (:use #:cl #:iter #:cffi)
  (:export #:*database*
           #:database-connection
           #:connect
           #:connected-p
           #:disconnect
           #:connect-toplevel
           #:disconnect-toplevel
           #:with-connection
           
           #:execute
           #:with-transaction
           #:rollback-transaction
           #:commit-transaction

           #:query

           #:call-remote-procedure))