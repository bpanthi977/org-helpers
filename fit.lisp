;;; nonlinear curve fitting (Backup of local project)

(defpackage #:fit
  (:use #:cl)
  (:export
   :fit
   :calculate-r^2))

(in-package #:fit)

(defun norm-f (fit)
  "Find the norm of the fit function f."
  (gsll:euclidean-norm (gsll:function-value fit)))

(defun derivative (f x j)
  "Compute partial derivative of f with respect to x_j at x"
  (let ((f1 (funcall f x))
	f2)
    (incf (grid:aref x j) 0.0001d0)
    (setf f2 (funcall f x))
    (decf (grid:aref x j) 0.0001d0)
    (/ (- f2 f1) 0.0001d0)))

(defun nonlinear-least-squares
    (residue-function
     parameters-initial-value
     &key
       residuef-derivative
       number-of-parameters
       number-of-observations
       (method gsll:+levenberg-marquardt+)
       (print-steps nil)
       (print-fit t)
       (absolute-error 1.0d-4)
       (relative-error 1.0d-4)
       (max-steps 50))
  "residue-function(parameters, residue_to_setf) :: with given
    parameters, set the residue for each observation. both arguments
    are grid arrays
parameters-initial-value :: (grid:make-foreign-array 'double-float
    :initial-contents initial-values))
absolute and relative error (measured wrt to previous and current
    fit) are tolerance to stop the fit. 
"
  (flet ((residual-derivative (parameters jacobian)
	   (let ((r1 (grid:make-foreign-array 'double-float :dimensions number-of-observations))
		 (r2 (grid:make-foreign-array 'double-float :dimensions number-of-observations)))
	     (dotimes (j number-of-parameters)
	       (funcall residue-function parameters r1)
	       (incf (grid:aref parameters j) 0.01)
	       (funcall residue-function  parameters r2)
	       (dotimes (i number-of-observations)
		 (setf (grid:aref jacobian i j)
		       (/ (- (grid:aref r2 i) (grid:aref r1 i))
			  0.01)))
	       (decf (grid:aref parameters j) 0.01)))))	
    (let* ((residue-derivative (or residuef-derivative #'residual-derivative))
	   covariance
	   (fit (gsll:make-nonlinear-fdffit
		 method
		 (list number-of-observations number-of-parameters)
		 (list residue-function residue-derivative)
		 parameters-initial-value nil))
	   ;; Making the jacobian here is optional -- it saves remaking on each iteration.
	   (jacobian (when print-fit (gsll:make-jacobian-matrix fit))))
      (macrolet ((fitx (i) `(grid:aref (gsll:solution fit) ,i))
		 (err (i) `(sqrt (grid:aref covariance ,i ,i))))
	(when print-steps
	  (format t "iter: ~d x = ~15,8f ~15,8f ~15,8f |f(x)|=~7,6g~&"
		  0 (fitx 0) (fitx 1) (fitx 2)
		  (norm-f fit)))
	(loop for iter from 0 below max-steps
	      until
	      (and (plusp iter)
		   (gsll:fit-test-delta fit absolute-error relative-error))
	      do
		 (gsll:iterate fit)
		 (when print-fit
		   (setf covariance (gsll:ls-covariance fit 0.0d0 covariance jacobian)))
		 (when print-steps
		   (format t "iter: ~d x = ~15,8f ~15,8f ~15,8f |f(x)|=~7,6g~&"
			   (1+ iter) (fitx 0) (fitx 1) (fitx 2)
			   (norm-f fit)))
	      finally
		 (if print-fit
		     (let* ((chi (norm-f fit))
			    (dof (- number-of-observations number-of-parameters))
			    (c (max 1.0d0 (if (= dof 0) 1 (/ chi (sqrt dof)))))
			    (table nil))
		       (format t "chisq/dof = ~g~&" (if (= dof 0) "infinity" (/ (expt chi 2) dof)))
		       ;; err(i) is standard deviation of ith prarameter given by sqrt of  diagonal element of
		       ;; covariance matrix 
		       (setf table
			     (loop for i from 0 below number-of-parameters
				   for valuei = (fitx i)
				   for confidencei = (* c (err i)) do 
				     (format t "P_~a         = ~,5f +/- ~,5f~&" i valuei confidencei)
				   collect (list valuei confidencei)))
		       (return (values (gsll:solution fit) (/ (expt chi 2) dof) table covariance)))
		     (return (gsll:solution fit))))))))

(defun nonlinear-least-squares-example ()
  (let* ((samples-count 5)
	 (data '((0 4) (1 8) (-2 2) (2 13) (-1 2)))
	 (residue-function (lambda (params residues)
			     (let ((a (grid:aref params 0))
				   (b (grid:aref params 1))
				   (c (grid:aref params 2)))
			       (loop for (x y) in data
				     for i from 0 do
				       (setf (grid:aref residues i)
					     (+ (* a x x) (* b x) c (- y))))))))
    (nonlinear-least-squares residue-function
			     (grid:make-foreign-array
			      'double-float :dimensions 3)
			     :number-of-parameters 3
			     :number-of-observations
			     samples-count
			     :print-steps nil
			     :print-fit t)))

(defun average (values)
  (loop for v in values
	for count from 1
	sum v into sum
	finally (return (/ sum count))))

(defun calculate-r^2 (function data &optional (dof 0))
  (loop with ybar = (average (mapcar #'second data))
	with n = (length data)
	for (x y) in data
	sum (expt (- y ybar) 2) into variance
	sum (expt (- y (funcall function x)) 2) into sse
	finally (return (values  (- 1 (/ sse variance)) (sqrt (/ sse (- n dof)))))))

(defun fit(function data initial-parameters &optional (calculate-r^2 nil) (table? nil))
  "function(x; parameters...)"
  (let* ((count (length data))
	 (pcount (length initial-parameters))
	 (residue-function (lambda (params residues)
			     (let ((parameters (loop for i from 0 to (1- pcount)
						     collect (grid:aref params i))))
			       (loop for (x y) in data
				     for i from 0 do
				       (setf (grid:aref residues i)
					     (- y (apply function x parameters))))))))
    (multiple-value-bind (fit chi table covariance)
	(nonlinear-least-squares residue-function
				 (grid:make-foreign-array 'double-float
							  :dimensions pcount
							  :initial-contents initial-parameters)
				 :number-of-parameters pcount
				 :number-of-observations count
				 :print-fit t)
      (let* ((parameters (loop for i from 0 to (1- pcount)
			       collect (grid:aref fit i))))
	(multiple-value-bind (r^2 rmse)  (if calculate-r^2
					     (calculate-r^2 (lambda (x)
							      (apply function x parameters))
							    data
							    pcount)
					     (values nil nil))
	  (when calculate-r^2
	    (format t "R^2~8t = ~a~%" r^2)
	    (format t "RMSE~8t = ~a~%" rmse))
	  (if table?
	      (values (append (list (list "Parameter" "Confidence +/-"))
			      table
			      (list (list "chisq/dof" chi)
				    (list "R^2" r^2 )
				    (list "rmse" rmse)))
		      parameters r^2 rmse covariance)
	      (values parameters r^2 rmse covariance)))))))

(defun partition (data)
  "data = {(x,y,z)}; partition into {(y,i)}"
  (let ((hashtable (make-hash-table :test #'equal))
	(i -1))
    (loop for (x y z) in data
	  for py = (gethash y hashtable) do
	    (unless py
	      (setf (gethash y hashtable) (incf i))))		
    (values hashtable (incf i))))

(defun partial-fit (data c f initial &optional (calculate-r^2 nil) (table? nil) (max-iters 100))
  "
fits z = c(f(x;[m]), u(y))
data = [(x y z)]
c(x &rest u_n)
initial = [m]
"
  (multiple-value-bind (P N) (partition data)
    (let* ((count (length data))
	   (f-parameters-count (length initial))
	   (total-parameters-count (+ N f-parameters-count))
	   (residue-function (lambda (params residues)
			       (let ((m (loop for i from 0 below f-parameters-count 
					      collect (grid:aref params i))))
				 (loop for (x y z) in data
				       for i from 0
				       for u_y = (grid:aref params (+ f-parameters-count (gethash y P)))
				       do 
					  (setf (grid:aref residues i)
						(- z (funcall c
							      (apply f x m)
							      u_y))))))))
      (multiple-value-bind (fit chi table)
	  (nonlinear-least-squares residue-function
				   (grid:make-foreign-array 'double-float
							    :dimensions total-parameters-count
							    :initial-contents (append initial
										      (loop for i from 0 below N
											    collect 1)))
				   :number-of-parameters total-parameters-count
				   :number-of-observations count
				   :print-fit t
				   :max-steps max-iters)
	(let* ((parameters (loop for i from 0 below f-parameters-count
				 collect (grid:aref fit i))))
	  (multiple-value-bind (r^2 rmse)
	      (if calculate-r^2
		  (calculate-r^2 (lambda (xy)
				   (funcall c
					    (apply f (first xy) parameters)
					    (grid:aref fit (+ f-parameters-count (gethash (second xy) P)))))
				 (mapcar (lambda (xyz) (list (list (first xyz) (second xyz))
							(third xyz)))
					 data))
		  (values nil nil))
	    (when calculate-r^2
	      (format t "R^2~8t = ~a~%" r^2)
	      (format t "RMSE~8t = ~a~%" rmse))
	    (if table?
		(append (list (list "Parameter" "Confidence +/-"))
			(loop for i from 0 below  f-parameters-count
			      collect (nth i table))
			(list (list "chisq/dof (for all parameters too)" chi)
			      (list "R^2" r^2)
			      (list "RMS" rmse)))
		(values parameters r^2 rmse))))))))
	
	


(defun r^2 (predicted actual)
  (loop with ybar = (average actual)
	for y in actual
	for ypred in predicted
	sum (expt (- y ybar) 2) into variance
	sum (expt (- y ypred) 2) into sse
	finally (return (- 1 (/ sse variance)))))
