(defpackage :problem-2
  (:use :cl :cl-ppcre :alexandria :split-sequence :utils))

(in-package :problem-2)


(defun problem-2 () 
  (with-file-lines (l "./input/2.txt")
    (let ((part1 (apply #'+ (mapcar (lambda (round)
                                      (cond
                                        ((equal round "A X") (+ 1 3))
                                        ((equal round "A Y") (+ 2 6))
                                        ((equal round "A Z") (+ 3 0))
                                        ((equal round "B X") (+ 1 0))
                                        ((equal round "B Y") (+ 2 3))
                                        ((equal round "B Z") (+ 3 6))
                                        ((equal round "C X") (+ 1 6))
                                        ((equal round "C Y") (+ 2 0))
                                        ((equal round "C Z") (+ 3 3))
                                        )) l)))
          (part2 (apply #'+ (mapcar (lambda (round)
                                      (cond ; A = rock = 1, B = paper = 2, C = scissor = 3
                                        ((equal round "A X") (+ 3 0))
                                        ((equal round "A Y") (+ 1 3))
                                        ((equal round "A Z") (+ 2 6))
                                        ((equal round "B X") (+ 1 0))
                                        ((equal round "B Y") (+ 2 3))
                                        ((equal round "B Z") (+ 3 6))
                                        ((equal round "C X") (+ 2 0))
                                        ((equal round "C Y") (+ 3 3))
                                        ((equal round "C Z") (+ 1 6))
                                        )) l))))
      (format t "~&2-1: ~A~&2-2: ~A" part1 part2))))

(problem-2)


