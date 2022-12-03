(defpackage :problem-3
  (:use :cl :cl-ppcre :alexandria :split-sequence :utils))

(in-package :problem-3)

(defparameter *priorities*
  (alist-hash-table (loop for c across "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" for i from 1 collect (cons c i)))
  )

(defun get-priority (c)
  (gethash c *priorities* 0))


(defun common-chars (s1 s2)
  (intersection (coerce s1 'list) (coerce s2 'list)))

(defun problem-3 ()
  (let ((part1
          (with-file-lines
              ;; Take each line and split it in half
              (lines "./input/3.txt" (lambda (line)
                                       (let ((l (/ (length line) 2)))
                                         (let ((sack1 (subseq line 0 l))
                                               (sack2 (subseq line l)))
                                           (list sack1 sack2)))))

            ;; Sum of priorities of the common char between halves
            (sum (mapcar
                  (lambda (line) (dbind (s1 s2) line (get-priority (first (common-chars s1 s2)))))
                  lines))))
        (part2 (with-file-lines (lines "./input/3.txt")
                 (let ((groups (group-list lines 3)))
                   (sum
                    ;; For each group
                    (mapcar
                     (lambda (group)
                       ;; Get the priority of the common char between all
                       ;; three elves in the group
                       (get-priority (first (reduce #'common-chars group))))
                     groups))))))
    (format t "~&3-1: ~A~&3-2: ~A" part1 part2)))

(problem-3)
