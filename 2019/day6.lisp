(defpackage :day6
  (:use :common-lisp
        :utils)
  (:import-from :uiop/utility
                :split-string))

(in-package :day6)

(defun path-length (graph start)
  (do ((cur start (gethash cur graph))
       (l 0 (1+ l)))
      ((null cur) (1- l))))

(defun path (graph start)
  (do ((cur start (gethash cur graph))
       (p nil (cons cur p)))
      ((null cur) p)))

(let ((graph (make-hash-table :test #'equal))
      (all-planets '()))
  (utils:do-lines (line "./input/day6.txt")
    (let* ((planets (split-string line :separator '(#\))))
           (parent (first planets))
           (child (second planets)))
      (pushnew parent all-planets :test #'equal)
      (pushnew child all-planets :test #'equal)
      (multiple-value-bind (value present) (gethash child graph)
        (setf (gethash child graph) parent))))
  (let ((soln1 (loop for planet in all-planets sum
                    (path-length graph planet)))
        (soln2 (let ((common (intersection (path graph "YOU")
                                           (path graph "SAN")
                                           :test #'equal)))
                 (loop for i in common minimize (- (+  (- (path-length graph "YOU") (path-length graph i))
                                                       (- (path-length graph "SAN") (path-length graph i)))
                                                   2)))))
    (list soln1 soln2)))
