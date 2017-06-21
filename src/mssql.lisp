;;;; mssql.lisp
;;;;
;;;; This file is part of the cl-mssql library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package :mssql)

(define-foreign-library sybdb
  (:darwin "libsybdb.dylib")
  (:linux (:or "libsybdb.so" "libsybdb.so.5"))
  (:unix "libsybdb.so")
  (t (:default "sybdb")))

(with-simple-restart (skip "Skip loading foreign library tds.")
  (use-foreign-library sybdb))

(defctype %DBPROCESS :pointer)
(defctype %CHARPTR :pointer)
(defctype %LOGINREC :pointer)
(defctype %RETCODE :int)
(defctype %BYTE :uchar)
(defctype %DBINT :int32)
(defctype %DBSMALLINT :int16)

(defconstant +FAIL+ 0)
(defconstant +INT_CANCEL+ 2)

(define-condition mssql-error (error)
  ((messages :initarg :messages :initform nil)))

(defmethod print-object ((err mssql-error) stream)
  (format stream "~{~A~^~%~}" (slot-value err 'messages)))

;; error-handler

(defcfun ("dberrhandle" %dberrhandle) :pointer
  (handler :pointer))

(defvar *error-message-list*)

(defcallback %error-handler :int
    ((%dbproc %DBPROCESS) (%severity :int) (%dberr :int) (%oserr :int) (%dberrstr :pointer) (%oserrstr :pointer))
  (declare (ignore %dbproc %severity %dberr %oserr))
  (if (boundp '*error-message-list*)
      (progn
        (unless (null-pointer-p %dberrstr)
          (push (foreign-string-to-lisp %dberrstr) *error-message-list*))
        (unless (null-pointer-p %oserrstr)
          (push (foreign-string-to-lisp %oserrstr) *error-message-list*)))
      (progn
        (unless (null-pointer-p %dberrstr)
          (warn (foreign-string-to-lisp %dberrstr)))
        (unless (null-pointer-p %oserrstr)
          (warn (foreign-string-to-lisp %oserrstr)))))
  +INT_CANCEL+)

(%dberrhandle (callback %error-handler))

;;; message-handler

(defcfun ("dbmsghandle" %dbmsghandle) :pointer
  (handler :pointer))

(defcallback %message-handler :int
    ((%dbproc %DBPROCESS) (%msgno :int) (%msgstate :int) (%severity :int)
     (%msgtext :pointer) (%svrname :pointer) (%proc :pointer) (%line :int))
  (declare (ignore %dbproc %severity %msgno %msgstate %svrname %proc %line))
  (unless (null-pointer-p %msgtext)
    (if (boundp '*error-message-list*)
        (push (foreign-string-to-lisp %msgtext) *error-message-list*)
        (warn (foreign-string-to-lisp %msgtext))))
  0)

(%dbmsghandle (callback %message-handler))

;; define-sybdb-function

(defmacro define-sybdb-function ((foreign-name lisp-name &optional check-mode error-message) return-type &body args)
  (if check-mode
      (let ((impl-name (intern (format nil "%~A" (symbol-name lisp-name))))
            (simplify-args (map 'list #'car args))
            (error-msg (or error-message
                           (format nil "~(~A fail~)" lisp-name)))
            (check-fun (case check-mode
                         (:check-retcode '(lambda (val)
                                           (= val +FAIL+)))
                         (:check-null-pointer '(lambda (val)
                                                (null-pointer-p val))))))
        `(progn 
           (defcfun (,foreign-name ,impl-name) ,return-type ,@args)
           (defun ,lisp-name ,simplify-args
             (let ((*error-message-list* nil))
               (let ((result (,impl-name ,@simplify-args)))
                 (if (funcall ,check-fun result)
                     (progn
                       (push ,error-msg *error-message-list*)
                       (error 'mssql-error :messages (reverse  *error-message-list*)))
                     (progn
                       (when *error-message-list*
                         (format t "~{~A~^~%~}~%" *error-message-list*))
                       result)))))))
      `(defcfun (,foreign-name ,lisp-name) ,return-type ,@args)))

;;; utility

(defun cffi-string (str &optional (pool garbage-pools::*pool*))
  (if str
      (garbage-pools:cleanup-register (foreign-string-alloc str)
                                      #'foreign-string-free
                                      pool)
      (null-pointer)))
