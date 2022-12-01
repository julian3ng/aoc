(defpackage :problem25
  (:use :cl))

(in-package :problem25)

(declaim (optimize (speed 3) (safety 0) (space 0) (debug 0)))
(defun parse-input-25 (lines)
  (let ((n (length lines))
        (m (length (first lines))))
    (make-array (list n m) :initial-contents lines)))

(defun print-board (board)
  (destructuring-bind (n m) (array-dimensions board)
    (loop for i from 0 to (1- n) do
      (loop for j from 0 to (1- m) do
        (format t "~C" (aref board i j)))
      (format t "~%"))))

(defun first-no-move (board)
  (destructuring-bind (n m) (array-dimensions board)
    (loop for i = 0 then (1+ i)
          do
             ;; (format t "~%~%")
             ;; (print-board board)
             (let ((rights nil)
                   (downs nil))
               (loop for i from 0 to (1- n) do
                 (loop for j from 0 to (1- m) do
                   (when (and (char= #\> (aref board i j))
                              (char= #\. (aref board i (mod (1+ j) m))))
                     (push (cons i j) rights))))

               (loop for (i . j) in rights do
                 (rotatef (aref board i j)
                          (aref board i (mod (1+ j) m))))

               (loop for i from 0 to (1- n) do
                 (loop for j from 0 to (1- m) do
                   (when (and (char= #\v (aref board i j))
                              (char= #\. (aref board (mod (1+ i) n) j)))
                     (push (cons i j) downs))))
               
               (loop for (i . j) in downs do
                 (rotatef (aref board i j)
                          (aref board (mod (1+ i) n) j)))
               (when (and (null rights) (null downs))
                 (return-from first-no-move (1+ i)))))))


(time (first-no-move (parse-input-25 (uiop:read-file-lines "./input/problem25.txt"))))
