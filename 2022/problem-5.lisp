(defpackage :problem-5
  (:use :cl :cl-ppcre :alexandria :utils))

(in-package :problem-5)

(defun problem-5 ()
  (let ((part1 (with-file-string (fstr "./input/5.txt")
                 (let ((moves (cl-ppcre:split "\\n" (second (cl-ppcre:split "\\n\\n" fstr))))
                       (stacks (alist-hash-table
                                '((1 P G R N)
                                  (2 C D G F L B T J)
                                  (3 V S M)
                                  (4 P Z C R S L)
                                  (5 Q D W C V L S P)
                                  (6 S M D W N T C)
                                  (7 P W G D H)
                                  (8 V M C S H P L Z)
                                  (9 Z G W L F P R)))))
                   (let ((parsed-moves (loop for move in moves collect
                                                               (mapcar #'parse-integer (remove-if (lambda (x) (equal x "")) (cl-ppcre:split "move | from | to " move))))))
                     (loop for (num src tgt) in parsed-moves do
                       (let ((stack-top (reverse (subseq (gethash src stacks) 0 num))))
                         (setf (gethash src stacks) (subseq (gethash src stacks) num))
                         (setf (gethash tgt stacks) (append stack-top (gethash tgt stacks)))))
                     (loop for k being the hash-keys of stacks collect (first (gethash k stacks)))))))
        (part2 (with-file-string (fstr "./input/5.txt")
                 (let ((moves (cl-ppcre:split "\\n" (second (cl-ppcre:split "\\n\\n" fstr))))
                       (stacks (alist-hash-table
                                '((1 P G R N)
                                  (2 C D G F L B T J)
                                  (3 V S M)
                                  (4 P Z C R S L)
                                  (5 Q D W C V L S P)
                                  (6 S M D W N T C)
                                  (7 P W G D H)
                                  (8 V M C S H P L Z)
                                  (9 Z G W L F P R)))))
                   (let ((parsed-moves (loop for move in moves collect
                                                               (mapcar #'parse-integer (remove-if (lambda (x) (equal x "")) (cl-ppcre:split "move | from | to " move))))))
                     (loop for (num src tgt) in parsed-moves do
                       (let ((stack-top (subseq (gethash src stacks) 0 num)))
                         (setf (gethash src stacks) (subseq (gethash src stacks) num))
                         (setf (gethash tgt stacks) (append stack-top (gethash tgt stacks)))))
                     (format t "================~%")
                     (loop for k being the hash-keys of stacks collect (first  (gethash k stacks))))))))
    
    (print-parts 5 part1 part2)))

(problem-5)
