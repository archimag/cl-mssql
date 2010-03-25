;;;; query.lisp
;;;;
;;;; This file is part of the cl-mssql library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>


(in-package :mssql)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant +no-more-results+ 2)
(defconstant +no-more-rows+ -2)

(define-sybdb-function ("dbresults" %dbresults) %RETCODE
  (dbproc %DBPROCESS))

(define-sybdb-function ("dbnumcols" %dbnumcols) :int
    (dbproc %DBPROCESS))

(define-sybdb-function ("dbcolname" %dbcolname) %CHARPTR
  (dbproc %DBPROCESS)
  (colnum :int))

(define-sybdb-function ("dbcoltype" %dbcoltype) :int
  (dbproc %DBPROCESS)
  (colnum :int))

(define-sybdb-function ("dbnextrow" %dbnextrow) %RETCODE
  (dbproc %DBPROCESS))

(define-sybdb-function ("dbdata" %dbdata) :pointer
  (dbproc %DBPROCESS)
  (column :int))


(define-sybdb-function ("dbdatlen" %dbdatlen) %DBINT
  (dbproc %DBPROCESS)
  (column :int))
  

(defcenum %syb-value-type
  (:syb-char  47)
  (:syb-varchar  39)
  (:syb-intn  38)
  (:syb-int1  48)
  (:syb-int2  52)
  (:syb-int4  56)
  (:syb-int8  127)
  (:syb-flt8  62)
  (:syb-datetime  61)
  (:syb-bit  50)
  (:syb-text  35)
  (:syb-image  34)
  (:syb-money4  122)
  (:syb-money  60)
  (:syb-datetime4  58)
  (:syb-real  59)
  (:syb-binary  45)
  (:syb-varbinary  37)
  (:syb-numeric  108)
  (:syb-decimal  106)
  (:syb-fltn  109)
  (:syb-moneyn  110)
  (:syb-datetimn  111))

(defun sysdb-data-to-lisp (data type len)
  (if (> len 0)
      (case (foreign-enum-keyword '%syb-value-type type)
        ((:syb-varchar :syb-text) (foreign-string-to-lisp data :count len))
        (:syb-char (string-trim #(#\Space) (foreign-string-to-lisp data :count len)))
        (:syb-int4 (mem-ref data :int))
        (:syb-int8 (mem-ref data :int8))
        (:syb-flt8 (mem-ref data :double))
        (otherwise (error "not supported type ~A" (foreign-enum-keyword '%syb-value-type type))))))

(defun field-name-s (str)
  (let ((name (string-upcase str)))
    (iter (for i from 0 below (length name))
          (when (char= (char str i)
                       #\_)
            (setf (char name i)
                  #\-)))
    (intern name :keyword)))

(defun query (query &key (connection *database*) (format :lists))
  (let ((%dbproc (slot-value connection 'dbproc))
        (cffi:*default-foreign-encoding* (slot-value connection 'external-format))
        (column-name-keyword-p (member format '(:alists :plists)))
        (field-plist-p (member format '(:plists))))
    (with-foreign-string (%query query)
      (%dbcmd %dbproc %query))
    (%dbsqlexec %dbproc)
    (unwind-protect
         (unless (= +no-more-results+ (%dbresults %dbproc))
           (let ((collumns (iter (for x from 1 to (%dbnumcols %dbproc))
                                 (collect (cons (let ((name (foreign-string-to-lisp (%dbcolname %dbproc x))e))
                                                  (if column-name-keyword-p
                                                      (field-name-s name)
                                                      name))
                                                (%dbcoltype %dbproc x))))))
             (iter (for rtc = (%dbnextrow %dbproc))
                   (while (not (= rtc +no-more-rows+)))
                   (collect (iter (for collumn in collumns)
                                  (for i from 1)
                                  (let ((value (sysdb-data-to-lisp (%dbdata %dbproc i)
                                                                   (%dbcoltype %dbproc i)
                                                                   (%dbdatlen %dbproc i))))
                                    (when value
                                      (cond
                                        (field-plist-p
                                         (collect (car collumn))
                                         (collect value))
                                        (t (collect (cons (car collumn)
                                                          value)))))))))))
      (%dbcancel %dbproc))))
