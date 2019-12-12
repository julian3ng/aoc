(defpackage :day7
  (:use :common-lisp
        :intcode
        :utils)
)

(in-package :day7)

(defun make-circular (l)
  (setf (cdr (last l)) l)
  l)

(defun all-permutations (list)
  (cond ((null list) '())
        ((null (cdr list)) (list list))
        (t (loop for elem in list append (mapcar (lambda (l) (cons elem l))
                                                 (all-permutations (remove elem list)))))))
(let ((code-str (with-open-file (f "./input/day7.txt") (read-line f)))
      (perms (all-permutations '(0 1 2 3 4))))
  (loop for perm in perms maximize
       (let ((res 0)) 
         (loop for amp in perm do 
              (with-input-from-string (in-str (format nil "~A ~A" amp res))
                (let ((out-str (make-array 0 :fill-pointer 0 :element-type 'character :adjustable t)))
                  (with-output-to-string (os out-str) 
                    (intcode:eval-program-string code-str :stream in-str :out-stream os))
                  (setf res (parse-integer out-str)))))
         res)))

(defun list-rotate (l)
  (append (cdr l) (list (car l))))

(defun make-big-input (perm)
  (let ((first-input (first perm))
        (tail (loop repeat 10 nconc (list-rotate perm))))
    (append (list first-input 0) tail)))

(let ((code-str (with-open-file (f "./input/day7.txt") (read-line f)))
      (perms (all-permutations '(5 6 7 8 9))))
  (loop for perm in perms collect
       (with-input-from-string (in-str (format nil "~{~A ~}" (make-big-input perm)))
         (let ((out-str (make-array 0 :fill-pointer 0 :element-type 'character :adjustable t)))
           (with-output-to-string (os out-str) 
             (intcode:eval-program-string code-str :stream in-str :out-stream os))
           (parse-integer out-str)))))


(let ((code-str (with-open-file (f "./input/day7.txt") (read-line f))))
  (intcode:eval-program-string code-str))




(format nil "~{~A ~}"  (loop repeat 10 nconc (list 4 5 6)))
