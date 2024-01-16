;;;; init.lisp
;;;;
;;;; This file is part of the cl-mssql library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>


(in-package :mssql)

;;; initialization

(define-sybdb-function ("dbinit" %dbinit :check-retcode) %RETCODE)

(define-sybdb-function ("dbexit" %dbexit) :void)

(defun init ()
  (%dbinit))

(defun exit ()
  (%dbexit))
