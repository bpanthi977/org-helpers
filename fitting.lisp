(in-package :org-helpers)

(export '*bigdata*)
(defparameter *bigdata* nil)

(export 'linear-fit)
(defun linear-fit (&key (data (slot-value *table* 'list)))
  (multiple-value-bind (slope c r) 
      (cl-mathstats:linear-regression-brief (mapcar #'second  data)
					    (mapcar #'first data))
    (values (list (list "Slope" slope)
		  (list "Intercept" c)
		  (list "R^2 (Coeff. of Determination)" r))
	    slope c r)))

(export 'fit)
(defun fit (lambda initial &key (data (slot-value *table* 'list)))
   (fit::fit lambda data initial t t))
  

(export 'multilinear-fit)
(defun multilinear-fit (&key (data (slot-value *table* 'list)))
  (fit (lambda (x a b c)
	 (+ a (* b (first x)) (* c (second x))))
       '(1 1 1)
       :data data))

(export 'multilinear-origin-fit)
(defun multilinear-origin-fit (&key (data (slot-value *table* 'list)))
  (fit (lambda (x b c)
	 (+ (* b (first x)) (* c (second x))))
       '(1 1)
       :data data))

(export 'linear-origin-fit)
(defun linear-origin-fit (&key (data (slot-value *table* 'list)))
  (fit (lambda (x a) (* a x)) '(1) :data data))

(export 'power-fit)
(defun power-fit (&key (data (slot-value *table* 'list)) (initial '(1 1)))
  (append (list (list "Equation =" "a*x^b"))
	  (fit::fit (lambda (x a b) (* a (expt x b))) data initial t t)))

(export 'shifted-power-fit)
(defun shifted-power-fit (&key (data (slot-value *table* 'list)) (initial '(1 1 1)))
  (append (list (list "Equation =" "a*x^b + c"))
	  (fit::fit (lambda (x a b c) (+ c(* a (expt x b)))) data initial t t)))

(export 'almost-linear-fit)
(defun almost-linear-fit (&key (data (slot-value *table* 'list)))
  (multiple-value-bind (table slope c r) (linear-fit :data data)
    (declare (ignore table r))
    (shifted-power-fit :data data :initial (list slope 1 c))))

(export 'polynomial-fit2)
(defun polynomial-fit2 (&key (data (slot-value *table* 'list)) (initial '(1 1)))
  (append (list (list "Equation =" "a*x^2 + b*x"))
	  (fit::fit (lambda (x a b) (+ (* b x) (* a (expt x 2)))) data initial t t)))


(defun rmse (function data &optional (dof 0))
  (loop with n = (length data)
	for (x y) in data
	sum (expt (- y (funcall function x)) 2) into sse
	finally (return  (sqrt (/ sse (- n dof))))))

;; exponential fit  y = A*e^(Bx)
(export 'exponential-fit)
(defun exponential-fit (&optional (data (slot-value *table* 'list)))
  (multiple-value-bind (slope c r) 
      (cl-mathstats:linear-regression-brief (mapcar (lambda (d) (log (second d))) data) ;; ln(y)
					    (mapcar #'first data)) ;; x
    (list (list "A" (exp c))
	  (list "B" slope)
	  (list "R^2 (Coeff. of Determination)" r)
	  (list "rmse" (rmse (lambda (x)
			       (+ (exp c) (expt x slope)))
			     data 2)))))



(export 'polynomial2-fit)
(defun polynomial2-fit (&key (data (slot-value *table* 'list)) (initial '(1 1 1)))
  (append (list (list "Equation =" "a*x^2 + b*x + c "))
	  (fit::fit (lambda (x a b c) (+ c (* b x) (* a (expt x 2)))) data initial t t)))

(export 'statistical-summary)
(defun statistical-summary (data)
  (multiple-value-bind (length minimum maximum range median mode mean variance sd iqr skewness)
      (cl-mathstats:statistical-summary data)
    (list (list 'length length) (list 'minimum minimum)
	  (list 'range range)
	  (list 'maximum maximum)
	  (list 'median median)
	  (list 'mode mode)
	  (list 'mean mean)
	  (list 'variance variance)
	  (list 'sd sd)
	  (list 'iqr iqr)
	  (list 'skewness skewness))))

(export 'fit-params)
(defun fit-params (&optional (fit *lastfit*))
  (mapcar #'(lambda (n)
	      (cond ((numberp n) n)
		    (t (parse-float:parse-float n :type 'double-float :exponent-character #\d))))
	  (butlast (mapcar #'first (rest fit)) 3)))


(export 'max-diff)
(defun max-diff (fit &optional (data *bigdata*))
  (loop for (x tp) in data
	for fit-tp = (funcall fit x)
	maximizing (abs (- tp fit-tp))))

(export 'max-%diff)
(defun max-%diff (fit &optional (data *bigdata*))
  (loop for (x tp) in data
	for fit-tp = (funcall fit x)
	maximizing (abs (* 100 (/ (- tp fit-tp) fit-tp)))))

(export 'save-fit-func)
(defun save-fit-func (func params)
  (lambda (x)
    (apply func x params)))

(export 'max-fit-%diff)
(defun max-fit-%diff (func initial &key (data *bigdata*) (save nil))
  (multiple-value-bind (params r^2 rmse covariance) (fit::fit func data initial t nil)
    (if save
	(setf (symbol-value save) (save-fit-func func params)))
    (table 6
      (row "max %diff" "R^2" "rmse" "Params" "Covariance")
      (row (loop for (x tp) in data
		 for fit-tp = (apply func x params)
		 maximizing (abs (* 100 (/ (- tp fit-tp) fit-tp))))
	   r^2
	   rmse
	   params
	   (loop for i from 0 below (length initial) collect (grid:aref covariance i i))))))

(export 'map-data)
(defun map-data (func &optional (data *bigdata*))
  (loop for (x tp) in data do
    (funcall func x tp)))


(export 'fit-check-csv)
(defun fit-check-csv (fit-func x-func file &optional (data *bigdata*))
  (cl-csv:write-csv
   (sort (loop 
	   for (x tp) in data 
	   collect (list (funcall x-func x) tp (funcall fit-func x)))
	 #'< :key #'first)
   :stream (merge-pathnames file *img-path*)))

(export 'scatter-csv)
(defun scatter-csv (fit-func file &optional (data *bigdata*))
  (cl-csv:write-csv
   (loop for (x tp) in data 
	 for fitted-tp = (funcall fit-func x)
	 collect  (list tp fitted-tp))
   
   :stream (merge-pathnames file *img-path*)))

(export 'scatter-plot)
(defun scatter-plot (fit-func file &optional (data *bigdata*) (ylabel "Fitted T") (fitTitle "Fit"))
  (let ((tp-and-fit (loop for (x tp) in data 
			  for fitted-tp = (funcall fit-func x)
			  collect  (list tp fitted-tp))))
    (with-plot file
      (setup :xlabel "Exact Period (sec)" :ylabel ylabel
	     :terminal '(:png :size :|1500,1100| :font "Times New Roman, 30pt"))
      (ezplot tp-and-fit :title fitTitle :with '(:point))
      (eazy-gnuplot:plot "x" :with '(:lines) :title "Perfect Fit"))))

(export 'comparision-scatter-plot)
(defun comparision-scatter-plot (fit-func other-funcs titles file &optional (data *bigdata*))
  (let ((tp-and-fit (loop for (x tp) in data 
			  for fitted-tp = (funcall fit-func x)
			  collect  (list tp fitted-tp)))
	(sorteddata (sort data #'< :key #'second)))
    (with-plot file
      (setup :xlabel "Exact T" :ylabel "Fitted T" 
	     :terminal '(:png :size :|1500,1100| :font "Times New Roman, 30pt"))
      (ezplot tp-and-fit :title "Fit" :with '(:point))
      (eazy-gnuplot:plot "x" :with '(:lines) :title "Exact")
      (loop for o in other-funcs
	    for title in titles do
	(ezplot (loop for (x tp) in sorteddata
		      for fitted-tp = (funcall o x)
		      collect (list tp fitted-tp))
		:title title
		:with :point)))))
		

(export 'fit-plot)
(defun fit-plot (fit-func x-axis file &optional (data *bigdata*))
  (let ((x-tp-fit (sort (loop 
			  for (x tp) in data 
			  collect (list (funcall x-axis x) tp (funcall fit-func x)))
			#'< :key #'first)))
    (with-plot file
      (setup :xlabel "X-axis" :ylabel "Tp"
	     :terminal '(:png :size :|1500,1100| :font "Times New Roman, 30pt"))
      (ezplot x-tp-fit :title "Fit" :using '(1 3) :with '(:lines))
      (ezplot x-tp-fit :title "Exact" :using '(1 2) :with '(:point)))))


(export 'histogram)
(defun histogram (list &key (min (reduce #'min list)) (max (reduce #'max list)) (steps 1))
  (let* ((size (ceiling (- max min) steps))
	 (index (mapcar (lambda (n)
			  (min (max  0 (floor (- n min) steps)) size))
			list))
	 (histogram (make-array size :element-type 'fixnum :initial-element 0)))
    (loop for i in index do
      (incf (aref histogram i)))
    histogram))


(export 'histogram-list)
(defun histogram-list (list &key (min (reduce #'min list)) (max (reduce #'max list)) (steps 1))
  (let ((hist (histogram list :min  min :max  max :steps steps)))
    (loop for v across hist
	  for x from min by steps
	  collect (list x v))))


(export 'loglog-fit)
(defun loglog-fit (&key (data (slot-value *table* 'list)))
  (multiple-value-bind (slope c r) 
      (cl-mathstats:linear-regression-brief (mapcar #'(lambda (x) (log (second x)))  data)
					    (mapcar #'(lambda (x) (log (first x))) data))
    (values (list (list "Slope" slope)
		  (list "Intercept" c)
		  (list "R^2 (Coeff. of Determination)" r))
	    slope c r)))


(export 'log-fit)
(defun log-fit (&key (data (slot-value *table* 'list)))
  (fit (lambda (x a b)
	 (+ (* a (log x)) b))
       '(1 1)
       :data data))

(defparameter *default-funcs* (list (cons "Linear" #'(lambda (x a b)
						       (+ a (* x b))))
				     (cons "Exp" #'(lambda (x a b)
						     (* a (exp (* b x)))))
				    (cons "Log" #'(lambda (x a b)
						    (+ a (* b (log x)))))
				    (cons "Power (Log-Log)" #'(lambda (x a b)
								(* a (expt x b))))))					  


(export 'different-fits)
(defun different-fits (data &optional other-funcs (default-funcs *default-funcs*))
  (let ((funcs (append other-funcs default-funcs)))
    (if funcs
      (table 3
	(row "F" "RMSE" "r^2" "params" "covariance")
	(loop for (name . f) in funcs do
	  (multiple-value-bind (params r^2 rmse  covariance) (fit::fit f data '(1 1) t nil)
	    (row name rmse r^2 params (loop for i from 0 below (length params) collect (grid:aref covariance i i))))))
      "No functions supplied")))
	
	

	  
  
