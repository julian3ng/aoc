(defpackage :day4
  (:use :common-lisp))

(in-package :day4)


(defun digits (num &key (base 10))
  (loop with result = '()
     while (> num 0) do
       (multiple-value-bind (divisor remainder) (floor num base)
         (push remainder result)
         (setf num divisor))
     finally (return result)))

(defun nondecreasing-digits-p (digs)
  (apply #'<= digs))

(defun adjacent-pair-p (digs)
  (find-if-not #'null (loop for (a b) on digs while b collect (= a b))))

(defun any-single-adjacent-pair-p (digs)
  (let ((pairs (nconc (list nil) (loop for (a b) on digs while b collect (= a b)) (list nil))))
    (loop for (p1 p2 p3) on pairs 
       counting (and (null p1) p2 (null p3)) into hits
       finally (return (> hits 0)))))

(let ((day4-input '(353096 . 843212)))
  (let ((soln1 (loop for i from (car day4-input) to (cdr day4-input)
                  count
                    (let ((d (digits i))) 
                      (and (nondecreasing-digits-p d)
                           (adjacent-pair-p d)))))
        (soln2 (loop for i from (car day4-input) to (cdr day4-input)
                  count
                    (let ((d (digits i)))
                      (and (nondecreasing-digits-p d)
                           (any-single-adjacent-pair-p d))))))
    (format t "SOLN 1: ~A~%SOLN 2: ~A~%" soln1 soln2)))
