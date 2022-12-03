(defpackage :problem-3
  (:use :cl :cl-ppcre :alexandria :split-sequence :utils))

(in-package :problem-3)

(defparameter *priorities*
  (alist-hash-table (loop for c across "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" for i from 1 collect (cons c i)))
  )

(defun get-priority (c)
  (gethash c *priorities* 0))


(defun problem-3 ()
  (let ((part1 (with-file-lines (lines "./input/3.txt" (lambda (line)
                                                          (let ((l (/ (length line) 2)))
                                                            (let ((sack1 (subseq line 0 l))
                                                                  (sack2 (subseq line l)))
                                                              (list sack1 sack2)))))
                  (apply #'+  (mapcar (lambda (line) (destructuring-bind (s1 s2) line
                                                  (let ((both (first (intersection (coerce  s1 'list) (coerce  s2 'list)))))
                                                    (get-priority both))))
                                      lines))))
        (part2 (with-file-lines (lines "./input/3.txt")
                 (let ((groups (loop for i from 0 to (/ (length lines) 3)
                                      collect
                                      (subseq lines (* i 3)
                                              (let ((next (* (1+ i) 3)))
                                                (unless (> next (length lines)) next))))))
                   (apply #'+  (mapcar (lambda (group)
                                         (get-priority  (when group
                                                          (first (reduce
                                                                  (lambda (g1 g2)
                                                                    (format t "~A ~A~%" g1 g2)
                                                                    (intersection (coerce g1 'list) (coerce g2 'list)))
                                                                  group))))) groups))))))
    part2))

(problem-3)

