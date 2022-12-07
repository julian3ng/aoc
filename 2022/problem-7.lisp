(defpackage :problem-7
  (:use :cl :cl-ppcre :alexandria :utils))

(in-package :problem-7)


(with-file-lines (lines "./input/7.txt")
  (let ((path nil)
        (sizes (make-hash-table :test #'equal)))
    (loop for line in lines
          for cur-dir-match = (nth-value 1 (cl-ppcre:scan-to-strings "^\\$ cd (.*)$" line))
          for size-file-match = (nth-value 1 (cl-ppcre:scan-to-strings "^(\\d+) (.*)$" line))
          do
             (when cur-dir-match
               (let ((cur-dir (aref cur-dir-match 0))) 
                 (cond
                   ((equal cur-dir "..") (pop path))
                   (t (push cur-dir path)))))
             (when size-file-match
               ;; "for var on list" iterates on conses
               ;; (loop for x on '(1 2 3) collect x) => '((1 2 3) (2 3) (3))
               (loop for p on path do
                 (incf (gethash p sizes 0) (parse-integer (aref size-file-match 0))))))
    (let* ((part1 (loop for k being the hash-keys of sizes using (hash-value v)
                        when (<= v 100000) sum v))
           (initial-free-space (- 70000000 (gethash '("/") sizes)))
           (goal-space 30000000)
           (part2 (loop for k being the hash-keys of sizes using (hash-value v)
                        when (> (+ initial-free-space v) goal-space)
                          minimize v)))

      (print-parts 7 part1 part2))))
