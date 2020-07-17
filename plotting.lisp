(in-package :org-helpers)
;;(defparameter *img-path* (asdf:system-relative-pathname :moi "../workbook/img/"))
(export '*img-path*)
(defparameter *img-path* nil)

(export 'setup-img-path)
(defun setup-img-path (system &optional (relative-path "../workbook/img/"))
  (setf *img-path* (asdf:system-relative-pathname system relative-path)))

(export 'plot-printer)
(defun plot-printer (data &optional (type :xy) (format-string "~&~,10f ~,10f"))
  (if (typep type 'list)
      (lambda ()
	(loop for d in data
	      with xcol = (first type)
	      with ycol = (second type) do
		(format t format-string (nth xcol d) (nth ycol d))))
      (ecase type
	(:xy (lambda ()
	       (loop for (x y) in data do
		 (format t format-string x y))))
	(:xxyy (lambda ()
		 (loop for x in (first data)
		       for y in (second data) do
			 (format t format-string x y))))
	(:y (lambda ()
	      (loop for y in data
		    for i from 0 do
		      (format t format-string i y)))))))

(defparameter *ezy-file* nil)

(export 'with-plot)
(defmacro with-plot (filename &body body)
  `(let ((*ezy-file* (merge-pathnames ,filename *img-path*)))
     (eazy-gnuplot:with-plots (*standard-output* :debug t)
       ,@body)
     (format nil "./img/~a" ,filename)))

(export 'setup)
(defun setup (&rest args &key (xlabel "x") (ylabel "y")
			   (terminal "png")
			   (key '(:bottom :right :font "Times New Roman,20"))
			   &allow-other-keys)
  
  (apply #'eazy-gnuplot:gp-setup :output *ezy-file*
				 :xlabel xlabel :ylabel ylabel
				 :terminal terminal
				 :key key 
				 (uiop:remove-plist-keys '(:xlabel :ylabel :terminal :key) args)))

(export 'ezplot)
(defun ezplot (data &rest args &key (title "Plot") (using '(1 2)) (with '(:lines))
				 &allow-other-keys)
  (apply #'eazy-gnuplot:plot (plot-printer data (mapcar #'1- using))
	 :using using
	 :with with
	 :title title
	 (uiop:remove-plist-keys '(:using :with :title) args)))

(export 'plot-example)
(defun plot-example () 
  (eazy-gnuplot:with-plots (*standard-output* :debug nil)
    (eazy-gnuplot:gp-setup :xlabel "x-label"      
			   :ylabel "y-label"
			   :output file 
			   :terminal "png"
			   :key '(:bottom :right :font "Times New Roman, 20")
			   :pointsize "0.4px")
    (eazy-gnuplot:plot (plot-printer data :xy)
		       :using '(1 2)
		       :title "title"
		       :with '(:lines))))

(export 'plot)
(defun plot (data file &key (type :xy) (gui nil) (multiple nil) title)
  (let ((terminal (if gui :qt :png)))
    (eazy-gnuplot:with-plots (*standard-output* :debug t)
      (eazy-gnuplot:gp-setup :xlabel "x-label"      ; strings : "\"x-label\""
			     :ylabel "y-label"
			     :output file ; pathnames : "\"sample.png\""
			     :terminal terminal         ; keyword/symbols: "terminal png"
			     
			     ;; list contents are recursively quoted, then joined by a space
			     :key '(:bottom :right :font "Times New Roman, 20")
			     
			     :pointsize "0.4px"
			     
			     ;;:yrange :|[0:1]|
			     ;; currently, specifying these kinds of options requires to abuse
			     ;; keywords and symbols. Another example: comma separated list, e.g.,
			     ;; :terminal '(:png :size |10cm,6cm|)
			     ;;
			     ;; 2/4/2016 Major options are now covered. 
			     )

      ;; any unsupported commands are available by printing it to the stream
      ;;(format t "~%unset key")
      
      ;; We are extending its expressivity. For example, as of 39d60d, there is gp-unset and gp-set.
      ;; An equivalent of above is (gp-unset :keys) .
      ;; The list is growing!

      ;; Functions can be protted with func-plot
      ;;(plot "sin(x)" :title "super sin curve!")
      ;; Plot a lisp data directly
      (eazy-gnuplot:plot (plot-printer data type)
			 :using '(1 2)
			 :title (if multiple (first title) title)
			 :with '(:lines))
      (when multiple
	(loop for i from 3 to (length (first data)) do 
	  (eazy-gnuplot:plot (plot-printer data (list 0 (1- i)))
			     :using '(1 2)
			     :title (nth (- i 2) title)
			     :with '(:lines))))
      (if gui
	  (format t "~&pause mouse button2;~%")))))

(export 'plot-table)
(defun plot-table (&key file (type :xy) (gui nil) (title "line"))
  (plot (slot-value *table* 'list) (merge-pathnames file *img-path*) :type type :gui gui :multiple (listp title) :title title)
  (format nil "img/~a" file))

(export 'plot2)
(defun plot2 (data filename &optional (titles "line"))
  (plot data (merge-pathnames filename *img-path*) :type :xy :gui nil :multiple (listp titles) :title titles)
  (format nil "img/~a" filename))

