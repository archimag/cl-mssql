;;;; mssql.asd
;;;;
;;;; This file is part of the cl-mssql library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(defsystem "mssql"
  :description "A Common Lisp library for interacting with MS SQL Server
  databases."
  :version "0.0.3"
  :author "Andrey Moskvitin <archimag@gmail.com>"
  :homepage "https://github.com/archimag/cl-mssql"
  :license "GPLv2"
  :depends-on ("cffi" "iterate" "garbage-pools" "parse-number")
  :components
  ((:module :src
            :components
            ((:file "packages")
             (:file "mssql" :depends-on ("packages"))
             (:file "connection" :depends-on ("mssql"))
             (:file "query" :depends-on ("connection"))
             (:file "execute" :depends-on ("connection"))
             (:file "remote" :depends-on ("query"))))))
