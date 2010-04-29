;;;; execute.lisp
;;;;
;;;; This file is part of the cl-mssql library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package :mssql)

(defun execute (query &key (connection *database*))
  (let ((%dbproc (slot-value connection 'dbproc))
        (cffi:*default-foreign-encoding* (slot-value connection 'external-format)))
    (with-foreign-string (%query query)
      (%dbcmd %dbproc %query))
    (%dbsqlexec %dbproc)
    (%dbcancel %dbproc)))

;;;; Transaction support

(defclass transaction-handle ()
  ((open-p :initform t :accessor transaction-open-p)
   (connection :initarg :connection :initform nil :reader transaction-connection))
  (:documentation "Simple box type for storing the status and the
associated database connection of a transaction. When open-p is nil,
the transaction has been aborted or committed."))

(defmacro with-transaction ((&key (connection '*database*) name) &body body)
  "Execute the body within a database transaction, committing when the
body exits normally, and aborting otherwise. An optional name can be
given to the transaction, which can be used to force a commit or abort
before the body unwinds."
  (let ((name (or name (gensym))))
    `(let ((,name (make-instance 'transaction-handle
                                 :connection ,connection)))
      (execute "BEGIN TRAN"
               :connection (transaction-connection ,name))
      (unwind-protect
           (prog1 (progn ,@body)
             (commit-transaction ,name))
        (rollback-transaction ,name)))))

(defun rollback-transaction (transaction)
  "Immediately rollback an open transaction."
  (when (transaction-open-p transaction)
    (let ((*database* (transaction-connection transaction)))
      (execute "ROLLBACK TRAN"
               :connection (transaction-connection transaction)))
    (setf (transaction-open-p transaction) nil)))

(defun commit-transaction (transaction)
  "Immediately commit an open transaction."
  (when (transaction-open-p transaction)
    (let ((*database* (transaction-connection transaction)))
      (execute "COMMIT TRAN"
               :connection (transaction-connection transaction)))
    (setf (transaction-open-p transaction) nil)))
