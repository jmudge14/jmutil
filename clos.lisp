;;; Utilities for CLOS and MOP
(in-package #:jmutil)

(defun all-direct-slots (class)
  "Returns list of all slots for a given class."
  ;Credits: This answer: https://stackoverflow.com/a/38473905
  ; https://stackoverflow.com/questions/38452350/is-there-a-way-to-gather-slot-definition-readers-from-all-the-inheritance-tree
  (append (closer-mop:class-direct-slots class)
          (alexandria:mappend #'all-direct-slots
                   (closer-mop:class-direct-superclasses class))))

(defun all-direct-slot-names (class)
  "Returns list of all slot names for a given object"
  (mapcar #'closer-mop:slot-definition-name
          (all-direct-slots class)))

(defmacro with-all-slots ((object class &optional suffix) &body body)
  "Assigns all slots from a given object to variables of the same name plus an optional suffix
   Example usage:
   (defclass tst () (x y z))
   (setf mx (make-instance 'tst))

   Example without suffix:
   (with-all-slots (mx tst) (setf x 1 y 2 z 3))

   Example with suffix (for nesting/handling of multiple objects):
   (with-all-slots (mx tst '-1') (setf x-1 1 y-1 2 z-1 3)) "
  (let* ((suffix-name (if (symbolp suffix)
                          (symbol-name suffix)
                          suffix))
         (slot-names (all-direct-slot-names (find-class class)))
         (new-slot-symbols (mapcar (lambda (sname)
                                     (intern (concatenate 'string
                                                          (symbol-name sname)
                                                          (string-upcase suffix-name))))
                                   slot-names)))
    `(with-slots ,(map 'list #'list new-slot-symbols slot-names)
                 ,object
                 ,@body)))

(defmacro with-object ((name object class &optional (separator ".")) &body body)
  "Binds symbols such that C++-style dot syntax can be used for slots.
   Example usage:
   (defclass tst () (x y z))
   (setf mx (make-instance 'tst))

   (with-object (var mx tst) (setf var.x 1 var.y 2 var.z 3))"
  (let* ((slot-names (all-direct-slot-names (find-class class)))
         (new-slot-symbols (mapcar (lambda (sname)
                                     (intern (concatenate 'string
                                                          (symbol-name name)
                                                          separator
                                                          (symbol-name sname))))
                                   slot-names)))
    `(with-slots ,(map 'list #'list new-slot-symbols slot-names)
                 ,object
                 ,@body)))
