(defpackage :day1
  (:use :common-lisp))

(in-package :day1)

(defun mass-to-fuel (mass)
  (- (floor mass 3) 2))

(defun total-fuel (mass)
  (if (<= mass 0)
      0
      (let ((f (max 0 (mass-to-fuel mass))))
        (+ f (total-fuel f)))))


(defun day-1-solution-1 (filename)
  (let ((result 0)) 
    (with-open-file (f filename)
      (do ((line (read-line f nil) (read-line f nil)))
          ((null line) result)
        (setf result (+ result (mass-to-fuel (parse-integer line :radix 10))))))))

(defun day-1-solution-2 (filename)
  (let ((result 0)) 
    (with-open-file (f filename)
      (do ((line (read-line f nil) (read-line f nil)))
          ((null line) result)
        (setf result (+ result (total-fuel (parse-integer line :radix 10))))))))

(day-1-solution-1 "./input/day1.txt") ; 3404722
(day-1-solution-2 "./input/day1.txt") ; 5104215
