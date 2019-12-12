(defpackage :utils
  (:use :common-lisp)
  (:export :do-lines))

(in-package :utils)

(defmacro do-lines ((var filename) &body body)
  (let ((stream (gensym)))
    `(with-open-file (,stream ,filename)
       (do ((,var (read-line ,stream nil) (read-line ,stream nil)))
           ((null ,var))
         ,@body))))
