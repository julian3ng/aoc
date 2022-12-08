(defpackage :problem-8
  (:use :cl :cl-ppcre :alexandria :utils))

(in-package :problem-8)

(with-file-lines (lines "./input/8.txt")
  (let ((trees (make-array (list
                            (length lines)
                            (length (first lines)))
                           :initial-contents
                           (mapcar (lambda (line) (loop for c across line collect (digit-char-p c))) lines))))
    (dbind (n m) (array-dimensions trees)
      
      (labels ((visible (y x)
                 (let ((target-tree (aref trees y x)))
                   (or 
                    (every #'identity (loop for i from 0 below y collect (< (aref trees i x) target-tree)))
                    (every #'identity (loop for i from (1+ y) below n collect (< (aref trees i x) target-tree)))
                    (every #'identity (loop for j from 0 below x collect (< (aref trees y j) target-tree)))
                    (every #'identity (loop for j from (1+ x) below m collect (< (aref trees y j) target-tree))))))
(scenic-score (y x)
                 (let ((target-tree (aref trees y x)))

                   (let ((up (loop for i from (1- y) downto 0
                                    for s from 1 
                                    do (when (>= (aref trees i x) target-tree) (return s))
                                    finally (return s)))
                         (down 
                           (loop for i from (1+ y) below n
                                 for s from 1 
                                 do (when (>= (aref trees i x) target-tree) (return s))
                                 finally (return s)))
                         (left 
                           (loop for j from (1- x) downto 0
                                 for s from 1 
                                 do (when (>= (aref trees y j) target-tree) (return s))
                                 finally (return s)))
                         (right 
                           (loop for j from (1+ x) below m
                                 for s from 1 
                                 do  (when (>= (aref trees y j) target-tree) (return s))
                                 finally (return s))))
                     (* up down left right)))))
        (let ((part1 (loop for i from 0 below n
                           summing
                           (loop for j from 0 below m
                                 summing (if  (visible i j) 1 0))))
              (part2 (loop for i from 0 below n
                           maximize
                           (loop for j from 0 below m
                                 maximize (scenic-score i j)))))
          (list part1 part2))))))
