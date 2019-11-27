;;;; jmutil.asd

(asdf:defsystem #:jmutil
  :description "Describe jmutil here"
  :author "Jack Mudge <jmudge14@gmail.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria #:closer-mop)
  :components ((:file "package")
               (:file "jmutil")
               (:file "clos")))
