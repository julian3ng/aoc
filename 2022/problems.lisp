(defpackage :problems
  (:use :cl :uiop :cl-ppcre))

(in-package :problems)

(declaim (optimize (debug 0) (speed 3) (space 3)))

(defun problem-1 ()
  (let* ((l (uiop:read-file-string "./input/1.txt"))
         (elves (cl-ppcre:split "\\n\\n" l)) 
         (elf-calories
           (mapcar
            (lambda (calorie-list)
              (apply #'+ (mapcar #'parse-integer (cl-ppcre:split "\\n" calorie-list))))
            elves)))
    (let ((sorted-elves (sort elf-calories #'>)))
      (format t "~&Part 1: ~A" (first sorted-elves))
      (format t "~&Part 2: ~A" (apply #'+ (subseq sorted-elves 0 3))))))


(problem-1)

