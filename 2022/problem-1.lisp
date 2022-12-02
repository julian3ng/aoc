(defpackage :problem-1
  (:use :cl :uiop :cl-ppcre :utils))

(in-package :problem-1)

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
      (let ((part1 (first sorted-elves))
            (part2 (apply #'+ (subseq sorted-elves 0 3))))

        (format t "~&1-1: ~A~&1-2: ~A" part1 part2)))))


(problem-1)


