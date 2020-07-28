;;;; org-helpers.asd

(asdf:defsystem #:org-helpers
  :description "Describe org-helpers here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :depends-on  (:parse-float :cl-csv :cl-mathstats :gsll :eazy-gnuplot)
  :serial t
  :components ((:file "package")
	       (:file "table")
	       (:file "fit")
	       (:file "fitting")
	       (:file "plotting")
               (:file "org-helpers")))
