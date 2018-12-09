;;;; package.lisp

(defpackage #:jmutil
  (:nicknames #:jm)
  (:use #:cl)
  (:export ; From jmutil.lisp
           #:asetf
           #:clampedp
           #:with-interned-symbols
           ; From clos.lisp
           #:with-object
           ))
