(defpackage :problem-6
  (:use :cl :cl-ppcre :alexandria :utils))

(in-package :problem-6)

(with-file-string (s "./input/6.txt")
  (let ((part1 (
                loop for i from 0 to (1- (length s))
                     for section = (subseq s i (+ i 4))
                     do (when (= (length section) (length (remove-duplicates section)))
                          (return (+ i 4)))))
        (part2 (
                loop for i from 0 to (1- (length s))
                     for section = (subseq s i (+ i 14))
                     do (when (= (length section) (length (remove-duplicates section)))
                          (return (+ i 14))))))
    (print-parts 6 part1 part2)))

(with-file-string (s "./input/6.txt")
  (loop
    for i from 0
    for w in (window s 4)
    for ww = (remove-duplicates w)
    do (when (= (length w) (length ww)) (return (+ i 4))))
    (loop
    for i from 0
    for w in (window s 14)
    for ww = (remove-duplicates w)
    do (when (= (length w) (length ww)) (return (+ i 14)))))

