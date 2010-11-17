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

(define-sybdb-function ("dbconvert" %dbconvert) %DBINT
  (dbproc %DBPROCESS)
  (srctype %syb-value-type)
  (src :pointer)
  (srclen %DBINT)
  (desttype %syb-value-type)
  (dest :pointer)
  (destlen %DBINT))


(defconstant +numeric-buf-sz+ 45)

(defun sysdb-data-to-lisp (%dbproc data type len)
  (if (> len 0)
      (case (foreign-enum-keyword '%syb-value-type type)
        ((:syb-varchar :syb-text) (foreign-string-to-lisp data :count len))
        (:syb-char (string-trim #(#\Space) (foreign-string-to-lisp data :count len)))
        (:syb-int4 (mem-ref data :int))
        (:syb-int8 (mem-ref data :int8))
        (:syb-flt8 (mem-ref data :double))
        ((:syb-money :syb-money4 :syb-decimal :syb-numeric)
         (with-foreign-pointer (%buf +numeric-buf-sz+)
           (parse-number:parse-number
            (foreign-string-to-lisp %buf
                                    :count (%dbconvert %dbproc type data -1 :syb-char %buf +numeric-buf-sz+)))))
        (otherwise (error "not supported type ~A"
                          (foreign-enum-keyword '%syb-value-type type))))))

(defun field-name-s (str)
  (let ((name (string-upcase str)))
    (iter (for i from 0 below (length name))
          (when (char= (char str i)
                       #\_)
            (setf (char name i)
                  #\-)))
    (intern name :keyword)))

(defun one-row (%dbproc collumns get-row-fun)
  (unless (= (%dbnextrow %dbproc)
             +no-more-rows+)
    (funcall get-row-fun %dbproc collumns)))

(defun all-rows (%dbproc collumns get-row-fun)
  (iter (for rtc = (%dbnextrow %dbproc))
        (while (not (= rtc +no-more-rows+)))
        (collect (funcall get-row-fun %dbproc collumns))))

(defmacro define-row-reader (name (collumn value) &body body)
  (let ((%dbproc (gensym))
        (collumns (gensym))
        (i (gensym)))
    `(defun ,name (,%dbproc ,collumns)
       (iter (for ,collumn in ,collumns)
             (for ,i from 1)
             (for ,value = (sysdb-data-to-lisp ,%dbproc
                                               (%dbdata ,%dbproc ,i)
                                               (%dbcoltype ,%dbproc ,i)
                                               (%dbdatlen ,%dbproc ,i)))
             ,@body))))
  
(define-row-reader read-plist-row (collumn value)
  (collect collumn)
  (collect value))

(define-row-reader read-alist-row (collumn value)
  (collect (cons collumn
                 value)))

(define-row-reader read-list-row (collumn value)
  (collect value))

(defun read-single-value (%dbproc collumns)
  (declare (ignore collumns))
  (sysdb-data-to-lisp %dbproc
                      (%dbdata %dbproc 1)
                      (%dbcoltype %dbproc 1)
                      (%dbdatlen %dbproc 1)))

(defparameter *query-formats*
  `((:lists read-list-row all-rows)
    (:list read-list-row one-row)
    (:rows read-list-row all-rows)
    (:row read-list-row one-row)
    (:alists read-alist-row all-rows keyword-collumn)
    (:alist read-alist-row one-row keyword-collumn)
    (:str-alists read-alist-row all-rows)
    (:str-alist read-alist-row one-row)
    (:plists read-plist-row all-rows keyword-collumn)
    (:plist read-plist-row one-row keyword-collumn)
    (:single read-single-value one-row)))

(defun query (query &key (connection *database*) (format :lists))
  (let ((%dbproc (slot-value connection 'dbproc))
        (cffi:*default-foreign-encoding* (slot-value connection 'external-format))
        (format-info (cdr (assoc format *query-formats*))))
    (unless format-info
      (error "Unknow query format: ~A" format))
    (with-foreign-string (%query query)
      (%dbcmd %dbproc %query))
    (%dbsqlexec %dbproc)
    (unwind-protect
         (unless (= +no-more-results+ (%dbresults %dbproc))
           (let ((collumns (iter (for x from 1 to (%dbnumcols %dbproc))
                                 (collect (let ((name (foreign-string-to-lisp (%dbcolname %dbproc x))e))
                                                  (if (eql (third format-info) 'keyword-collumn)
                                                      (field-name-s name)
                                                      name))))))
             (funcall (second format-info)
                      %dbproc
                      collumns
                      (first format-info))))
      (%dbcancel %dbproc))))

