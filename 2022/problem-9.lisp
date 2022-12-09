(defpackage :problem-9
  (:use :cl :cl-ppcre :alexandria :utils))

(in-package :problem-9)

(with-file-lines (lines  "./input/9.txt" (lambda (line)
                                           (dbind (direction distance) (cl-ppcre:split " " line)
                                             (list direction (parse-integer distance)))))
  (let ((part1 (let ((tail-positions (make-hash-table :test #'equal))
                     ;; x y
                     (head '(0 0))
                     (tail '(0 0)))
                 (loop for (direction distance) in lines do
                   (loop for d from 0 below distance do
                     (dbind (hx hy) head 
                       (cond
                         ((equal direction "U") (setf head (list hx (1+ hy))))
                         ((equal direction "D") (setf head (list hx (1- hy))))
                         ((equal direction "L") (setf head (list (1- hx) hy)))
                         ((equal direction "R") (setf head (list (1+ hx) hy))))
                       
                       )
                     (dbind (hx hy) head
                       (dbind (tx ty) tail 
                         (let ((xdiff (- hx tx))
                               (ydiff (- hy ty)))
                           (cond
                             ((= 2 ydiff) (setf tail (list (+ tx xdiff) (1+ ty))))
                             ((= -2 ydiff) (setf tail (list (+ tx xdiff) (1- ty))))
                             ((= 2 xdiff) (setf tail (list (1+ tx) (+ ty ydiff))))
                             ((= -2 xdiff) (setf tail (list (1- tx) (+ ty ydiff)))))
                           (incf (gethash tail tail-positions 0)))))))
                 (loop for k being the hash-keys of tail-positions count k)))
        (part2   (let ((tail-positions (make-hash-table :test #'equal))
                       (rope (make-array 10 :initial-contents (loop repeat 10 collect (list 0 0)))))
                   (labels ((propogate ()
                              (loop for i from 0 to 8
                                    for j = (1+ i)
                                    do
                                       (dbind (hx hy) (aref rope i)
                                         (dbind (tx ty) (aref rope j)
                                           (let ((xdiff (- hx tx))
                                                 (ydiff (- hy ty)))
                                             (cond
                                               ((= 2 xdiff ydiff) (setf (aref rope j) (list (1+ tx) (1+ ty))))
                                               ((= 2 (- xdiff) ydiff) (setf (aref rope j) (list (1- tx) (1+ ty))))
                                               ((= 2 xdiff (- ydiff)) (setf (aref rope j) (list (1+ tx) (1- ty))))
                                               ((= -2 xdiff ydiff) (setf (aref rope j) (list (1- tx) (1- ty))))
                                               ((= 2 ydiff) (setf (aref rope j) (list (+ tx xdiff) (1+ ty))))
                                               ((= -2 ydiff) (setf (aref rope j) (list (+ tx xdiff) (1- ty))))
                                               ((= 2 xdiff) (setf (aref rope j) (list (1+ tx) (+ ty ydiff))))
                                               ((= -2 xdiff) (setf (aref rope j) (list (1- tx) (+ ty ydiff))))))))
                                       (when (= j 9) (incf (gethash (aref rope 9) tail-positions 0))))))
                     (loop for (direction distance) in lines do
                       (loop repeat distance do 
                         (progn
                           (dbind (hx hy) (aref rope 0) 
                             (cond
                               ((equal direction "U") (setf (aref rope 0) (list hx (1+ hy))))
                               ((equal direction "D") (setf (aref rope 0) (list hx (1- hy))))
                               ((equal direction "L") (setf (aref rope 0) (list (1- hx) hy)))
                               ((equal direction "R") (setf (aref rope 0) (list (1+ hx) hy)))))
                           (propogate)
                           )))
                     (loop for k being the hash-keys of tail-positions count k)))))
    (print-parts 9 part1 part2)
    (list part1 part2)))
