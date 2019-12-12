(defpackage :day2
  (:use :common-lisp)
  (:import-from :uiop/utility
                :split-string))

(in-package :day2)

(defmacro do-lines ((var filename) &body body)
  (let ((stream (gensym)))
    `(with-open-file (,stream ,filename)
       (do ((,var (read-line ,stream nil) (read-line ,stream nil)))
           ((null ,var))
         ,@body))))

(defun eval-intmachine (storage iptr)
  (let ((op (aref storage iptr))
        (a (aref storage (+ iptr 1)))
        (b (aref storage (+ iptr 2)))
        (tgt (aref storage (+ iptr 3))))
    (let ((v1 (aref storage a))
          (v2 (aref storage b))) 
      (cond
        ((= op 1) (progn
                    (setf (aref storage tgt) (+ v1 v2))
                    (+ iptr 4)))
        ((= op 2) (progn
                    (setf (aref storage tgt) (* v1 v2))
                    (+ iptr 4)))
        ((= op 99) -1)))))


(do-lines (line "input/day2.txt")
  (let ((code (mapcar (lambda (n) (parse-integer n)) (split-string line :separator  '(#\,)))))
    (let ((storage (make-array (length code) :initial-contents code)))
      (setf (aref storage 1) 12
            (aref storage 2) 2)
      (do ((iptr 0 (eval-intmachine storage iptr)))
          ((= -1 iptr))
          (print iptr)
        (print storage)))))

(do-lines (line "input/day2.txt")
  (let ((code (mapcar (lambda (n) (parse-integer n)) (split-string line :separator  '(#\,)))))
    (loop for i from 0 to (1- (length code)) do
         (loop for j from 0 to (1- (length code)) do 
              (let ((storage (make-array (length code) :initial-contents code)))
                (setf (aref storage 1) i
                      (aref storage 2) j)
                (do ((iptr 0 (eval-intmachine storage iptr)))
                    ((= -1 iptr))
                ;  (print iptr)
                 ; (print storage)
                  )
                (when (= 19690720 (aref storage 0))
                  (print (list i j (aref storage 0) (+ (* 100 i) j)))))))))
