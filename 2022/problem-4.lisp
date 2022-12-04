(defpackage :problem-4
  (:use :cl :cl-ppcre :alexandria :utils))

(in-package :problem-4)

(defun problem-4 ()
  (with-file-lines (lines "./input/4.txt"
                          (lambda (line)
                            (mapcar (lambda (elf)
                                      (mapcar #'parse-integer
                                              (cl-ppcre:split "-" elf)))
                                    (cl-ppcre:split "," line))))
    (let ((part1 (loop for ((a b) (c d)) in lines
                       count
                       (or (and (<= a c) (<= d b))
                           (and (<= c a) (<= b d)))))
          (part2 (loop for ((a b) (c d)) in lines
                       count
                       (or (and (<= c b) (<= a d))
                           (and (<= a d) (<= c b)))))
          )
      (print-parts 4 part1 part2))))

(problem-4)
