(defpackage :utils
  (:use :cl :uiop :cl-ppcre)
  (:export :with-file-lines :with-file-string))

(in-package :utils)

(defmacro with-file-lines (varname filename &body body)
  `(let ((,varname (uiop:read-file-lines ,filename)))
     ,@body))

(defmacro with-file-string (varname filename &body body)
  `(let ((,varname (uiop:read-file-string ,filename)))
     ,@body))

