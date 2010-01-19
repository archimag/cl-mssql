;;;; mssql.asd
;;;;
;;;; This file is part of the cl-mssql library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(defsystem mssql
  :depends-on (#:cffi #:iterate #:garbage-pools)
  :components
  ((:module :src
            :components
            ((:file "packages")
             (:file "mssql" :depends-on ("packages"))
             (:file "connection" :depends-on ("mssql"))
             (:file "query" :depends-on ("connection"))))))