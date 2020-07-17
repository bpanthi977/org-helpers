(in-package :org-helpers)

(defparameter *table* nil)

(defparameter *table-float-digits* 3)

(defclass table ()
  ((list :initform nil :type list)
   (posi :initform 0 :type integer)
   (posj :initform 0 :type integer)))

(defun table-handle-floats (list)
  (cond ((listp list)
	 (mapcar #'table-handle-floats list))
	((typep list 'integer)
	 list)
	((typep list 'float)
	 (if *table-float-digits*
	     (format nil "~,vf" *table-float-digits* list)
	     list))
	((typep list 'number)
	 (if *table-float-digits*
	     (format nil "~,vf" *table-float-digits* (coerce list 'float))
	     list))
	(t list)))

(export 'table)
(defmacro table (&body body)
  "first paramter can be t, nil, number or anything else"
  `(let ((*table* (make-instance 'table))
	 (*table-float-digits* ,(if (numberp (first body)) (first body) nil)))
     ,@(append body
	       (unless (eql (first body) nil)
		 (list `(table-handle-floats (slot-value *table* 'list)))))))

(export 'table-data)
(defun table-data ()
  (slot-value *table* 'list))

(export 'row)
(defun row (&rest values)
  (with-slots (list posi posj) *table*
    (setf list (append list (list values))
	  posi (1+ posi)
	  posj 0)))



