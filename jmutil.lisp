;;;; jmutil.lisp

(in-package #:jmutil)


(defmacro asetf (arr &body sets)
  "Conveniently assign multiple items in an array in arbitrary order
   Example:
   (defvar some-array (make-array '(2 2)))
   (asetf some-array (0 0 t) (1 0 t))
   some-array -> #2A ((T 0)(T 0))"
  (let ((setfs '(setf)))
    (dolist (oper sets (reverse setfs))
      (push `(aref ,arr ,@(butlast oper)) setfs)
      (push (car (last oper)) setfs))))

(defun clampedp (val var1 var2)
  "T if and only if VAL is between (inclusive) VAR1 and VAR2
   That is, if (clampedp val var1 var2), then (= val (clamp val var1 var2))
   C.f., #'alexandria:clamp"
  (and (>= val (min var1 var2))
       (<= val (max var1 var2))))

(defmacro with-interned-symbols (symbol-list &body body)
  "Interns a set of symbols in the current package to variables of the same (symbol-name)."
  (let ((symbol-list (mapcar (lambda (s)
                               (list s `(intern (symbol-name ',s))))
                             symbol-list)))
    `(let ,symbol-list ,@body)))

