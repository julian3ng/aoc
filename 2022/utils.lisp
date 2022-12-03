(defpackage :utils
  (:use :cl :uiop :cl-ppcre)
  (:export
   :with-file-lines
   :with-file-string
   :dbind
   :sum
   :prod
   :group-list))

(in-package :utils)

(defmacro with-file-lines ((varname filename &optional line-processor)  &body body )
  `(let ((,varname
           ,(if (null line-processor)
                `(uiop:read-file-lines ,filename)
                `(mapcar ,line-processor (uiop:read-file-lines ,filename)))))
     ,@body))

(defmacro with-file-string (varname filename &body body)
  `(let ((,varname (uiop:read-file-string ,filename)))
     ,@body))

(defmacro dbind (lambda-list expression &body body)
  `(destructuring-bind ,lambda-list ,expression ,@body)
  )

(defun sum (num-list)
  (apply #'+ num-list))

(defun prod (num-list)
  (apply #'* num-list))


(defun group-list (l n)
  (let ((len (length l)))
    (loop for i from 0 to (truncate len n)
          for start = (* i n)
          for end = (* (1+ i) n)
          until (>= start len)
          collect
          (subseq l start (when (< end len) end)))))
