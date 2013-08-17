;;;; connection.lisp
;;;;
;;;; This file is part of the cl-mssql library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>


(in-package :mssql)

;;; database-connection

(defvar *database* nil)

(defclass database-connection ()
  ((dbproc :initarg :dbproc :initform nil)
   (external-format :initarg :external-format :initform :utf-8)))

;;; connect

(define-sybdb-function ("dblogin" %dblogin :check-null-pointer) :pointer)

(define-sybdb-function ("dbloginfree" %dbloginfree) :void
  (login :pointer))

(define-sybdb-function ("dbsetlname" %dbsetlname :check-retcode) %RETCODE
  (login %LOGINREC)
  (value :pointer)
  (which :int))

(define-sybdb-function ("tdsdbopen" %tdsdbopen :check-null-pointer "Connection to the database failed for an unknown reason.") %DBPROCESS
  (login %LOGINREC)
  (server :pointer)
  (msdblib :int))

(define-sybdb-function ("dbcmd" %dbcmd :check-retcode) %RETCODE
  (dbproc %DBPROCESS)
  (cmdstring :pointer))

(define-sybdb-function ("dbsqlexec" %dbsqlexec :check-retcode) %RETCODE
  (dbproc %DBPROCESS))

(define-sybdb-function ("dbcancel" %dbcancel :check-retcode) %RETCODE
  (dbproc %DBPROCESS))

(defun connect (database user password host &key (external-format :utf-8))
  "Connects to `database` on `host` using `user` and `password`.
The external format defaults to :utf-8.

A CLOS object of type 'database-connection is returned."
  (let ((cffi:*default-foreign-encoding* external-format))
    (garbage-pools:with-garbage-pool ()
      (let ((%login (garbage-pools:cleanup-register (%dblogin) #'%dbloginfree)))
        (iter (for (key . value) in (list (cons host 1)       ;; set host
                                          (cons user 2)       ;; set user
                                          (cons password 3)   ;; set password
                                          '("cl-mssql" . 5))) ;; set app
              (%dbsetlname %login (cffi-string key) value))

        (let ((%dbproc (garbage-pools:cleanup-register (%tdsdbopen %login (cffi-string host) 1)
                                                       #'%dbclose)))
          (%dbcmd %dbproc
                  (cffi-string (concatenate 'string
                                            "SET ARITHABORT ON;"
                                            "SET CONCAT_NULL_YIELDS_NULL ON;"
                                            "SET ANSI_NULLS ON;"
                                            "SET ANSI_NULL_DFLT_ON ON;"
                                            "SET ANSI_PADDING ON;"
                                            "SET ANSI_WARNINGS ON;"
                                            "SET ANSI_NULL_DFLT_ON ON;"
                                            "SET CURSOR_CLOSE_ON_COMMIT ON;"
                                            "SET QUOTED_IDENTIFIER ON;"
                                            (format nil "USE [~A]" database)
                                            )))
          (%dbsqlexec %dbproc)
          (%dbcancel %dbproc)

          (garbage-pools:cancel-object-cleanup %dbproc)
          (make-instance 'database-connection
                         :dbproc %dbproc
                         :external-format external-format))))))

;;; connect-p

(defun connected-p (connection)
  "Is `connection` available?"
  (let ((dbproc (slot-value connection 'dbproc)))
    (and dbproc (not (null-pointer-p dbproc)))))

;;; disconnect

(define-sybdb-function ("dbclose" %dbclose) :void
  (dbproc %DBPROCESS))

(defun disconnect (connection)
  "Disconnect from `connection`."
  (when (slot-value connection 'dbproc)
    (%dbclose (slot-value connection 'dbproc))
    (setf (slot-value connection 'dbproc) nil)))

;;; garbage-pool support

(garbage-pools:defcleanup database-connection #'disconnect)

;;; disconnnect-toplevel

(defun disconnect-toplevel ()
  "Disconnects from *database*"
  (when *database* (connected-p *database*)
        (disconnect *database*)
        (setf *database* nil)))

;;; connect-toplevel

(defun connect-toplevel (database user password host &key (external-format :utf-8))
  "Connects to `database` on `host` using `user` and `password`.
The connection is bound to the special variable *database*"
  (when (and *database* (connected-p *database*))
    (restart-case (error "Top-level database already connected.")
      (replace () :report "Replace it with a new connection." (disconnect-toplevel))
      (leave () :report "Leave it." (return-from connect-toplevel nil))))
  (setf *database*
        (connect database user password host :external-format external-format)))


;;; with-connection

(defmacro with-connection ((database user password host &key (external-format :utf-8)) &body body)
  "Connects to `database` on `host` using `user` and `password`.
Inside of `body`, the connection is bound to the special variable *database*"
  `(garbage-pools:with-garbage-pool ()
     (let ((*database* (garbage-pools:object-register (connect ,database ,user ,password ,host :external-format ,external-format))))
       ,@body)))
