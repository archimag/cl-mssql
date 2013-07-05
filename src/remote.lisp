;;;; remote.lisp
;;;;
;;;; This file is part of the cl-mssql library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package :mssql)

(define-sybdb-function ("dbrpcinit" %dbrpcinit) %RETCODE
  (dbproc %DBPROCESS)
  (rpcname :pointer)
  (options %DBSMALLINT))

(define-sybdb-function ("dbrpcparam" %dbrpcparam) %RETCODE
  (dbproc %DBPROCESS)
  (paramname :pointer)
  (status %BYTE)
  (type %syb-value-type)
  (maxlen %DBINT)
  (datalen %DBINT)
  (value :pointer))

(define-sybdb-function ("dbrpcsend" %dbrpcsend) %RETCODE
  (dbproc %DBPROCESS))

(defun lisp-to-sysdb (val)
  (cond
    ((null val) (values (null-pointer) 0 :syb-char))
    ((typep val 'fixnum) (values (garbage-pools:cleanup-register (foreign-alloc :int :initial-element val)
                                                                 #'foreign-free)
                                 (foreign-type-size :int)
                                 :syb-int4))
    ((typep val 'bignum) (values (garbage-pools:cleanup-register (foreign-alloc :long-long :initial-element val)
                                                                 #'foreign-free)
                                 (foreign-type-size :long-long)
                                 :syb-int8))
    ((numberp val) (values (garbage-pools:cleanup-register (foreign-alloc :double :initial-element (coerce val 'double-float))
                                                           #'foreign-free)
                           (foreign-type-size :double)
                           :syb-flt8))
    (t (multiple-value-bind (ret length)
           (foreign-string-alloc (if (stringp val)
                                     val
                                     (write-to-string val))
                                 :null-terminated-p nil)
         (values (garbage-pools:cleanup-register ret #'foreign-string-free)
                 length
                 :syb-char)))))

(defun call-remote-procedure (name &rest params)
  (let ((%dbproc (slot-value *database* 'dbproc))
        (cffi:*default-foreign-encoding* (slot-value *database* 'external-format)))
    (with-foreign-string (%name name)
      (%dbrpcinit %dbproc %name 0))
    (garbage-pools:with-garbage-pool ()
      (iter (for param in params)
            (multiple-value-bind (%val %size type) (lisp-to-sysdb param)
              (%dbrpcparam %dbproc (null-pointer) 0 type -1 %size %val)))
      (%dbrpcsend %dbproc))
    (unwind-protect
         (get-results %dbproc :plists)
      (%dbcancel %dbproc))))


