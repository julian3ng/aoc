(defpackage :problem-12
  (:use :cl :cl-ppcre :alexandria :utils))

(in-package :problem-12)

(declaim (optimize (debug 3)))
(with-file-lines (lines "./input/12.txt")
  (let* ((board (make-array (list (length lines) (length (first lines)))
                            :initial-contents lines))
         
         (start)
         (end))
    (dbind (n m) (array-dimensions board)
      (loop for i from 0 below n do
            (loop for j from 0 below m do
              (when (char= #\S (aref board i j))
                (setf start (list i j)))
              (when (char= #\E (aref board i j))
                (setf end (list i j)))))

      (let ((distances (make-array (list n m) :initial-element most-positive-double-float))
            (q (make-hash-table :test #'equal)))
        (loop for i from 0 below n
              do (loop for j from 0 below m
                       do (setf (gethash (list i j) q) t)))

        (setf (apply #'aref distances start) 0)
        (labels (
                 (distance (coord)
                   (apply #'aref distances coord))
                 (elevation (coord)
                   (let ((c (apply #'aref board coord)))
                     (char-code  (cond
                                   ((char= c #\S) #\a)
                                   ((char= c #\E) #\z)
                                   (t c)))))
                 (elevation-change (coord1 coord2)
                   (- (elevation coord2)
                      (elevation coord1)))
                 (neighbors (coord)
                   (dbind (y x) coord
                     (loop for (i j) in (list (list (1- y) x)
                                              (list (1+ y) x)
                                              (list y (1- x))
                                              (list y (1+ x)))
                           when (and
                                 (<= 0 i (1- n))
                                 (<= 0 j (1- m))
                                 (<= (elevation-change coord (list i j)) 1))
                             collect (list i j)
                           ))))
          (loop until (zerop (hash-table-count q)) do 
            (let ((closest (loop with min-coord = nil
                                 with min-dist = most-positive-double-float
                                 for coord being the hash-keys in q
                                 for dist = (apply #'aref distances coord)
                                 do
                                    (when (< dist min-dist)
                                      (setf min-coord coord)
                                      (setf min-dist dist))
                                 finally
                                    (return min-coord))))
              (remhash closest q)

              (when (char= (apply #'aref board closest) #\E)
                (return (distance end)))

              
              (format t "~A: ~A~%" closest (distance closest))
              
              (loop for neighbor in (neighbors closest)
                    when (gethash neighbor q)
                      do
                         (let ((alt (+
                                     (apply #'aref distances closest)
                                     1)))
                           (when (< alt (apply #'aref distances neighbor))
                             (setf (apply #'aref distances neighbor) alt)))))))))))


(with-file-lines (lines "./input/12.txt")
  (let* ((board (make-array (list (length lines) (length (first lines)))
                            :initial-contents lines))
         
         (start))
    (dbind (n m) (array-dimensions board)
      (loop for i from 0 below n do
        (loop for j from 0 below m do
          (when (char= #\E (aref board i j))
            (setf start (list i j)))))

      (let ((distances (make-array (list n m) :initial-element most-positive-double-float))
            (q (make-hash-table :test #'equal)))
        (loop for i from 0 below n
              do (loop for j from 0 below m
                       do (setf (gethash (list i j) q) t)))


        (setf (apply #'aref distances start) 0)
        (labels (
                 (distance (coord)
                   (apply #'aref distances coord))
                 (elevation (coord)
                   (let ((c (apply #'aref board coord)))
                     (char-code  (cond
                                   ((char= c #\S) #\a)
                                   ((char= c #\E) #\z)
                                   (t c)))))
                 (elevation-change (coord1 coord2)
                   (- 0  (- (elevation coord2)
                            (elevation coord1))))
                 (neighbors (coord)
                   (dbind (y x) coord
                     (loop for (i j) in (list (list (1- y) x)
                                              (list (1+ y) x)
                                              (list y (1- x))
                                              (list y (1+ x)))
                           when (and
                                 (<= 0 i (1- n))
                                 (<= 0 j (1- m))
                                 (<= (elevation-change coord (list i j)) 1))
                             collect (list i j)
                           ))))
          (loop until (zerop (hash-table-count q)) do
            (let ((closest (loop with min-coord = nil
                                 with min-dist = most-positive-double-float
                                 for coord being the hash-keys in q
                                 for dist =
                                 (if  (< (length coord) 2)
                                      (progn (print coord) (return)) 
                                      (apply #'aref distances coord))
                                 do
                                    (when (< dist min-dist)
                                      (setf min-coord coord)
                                      (setf min-dist dist))
                                 finally
                                    (return min-coord))))
              (when (null closest)
                (return ))
              (remhash closest q)
              (loop for neighbor in (neighbors closest)
                    when (gethash neighbor q)
                      do
                         (let ((alt (+
                                     (apply #'aref distances closest)
                                     1)))
                           (when (< alt (apply #'aref distances neighbor))
                             (setf (apply #'aref distances neighbor) alt))))))

          (loop
            with min-dist = most-positive-double-float
            for i from 0 below n
            do (loop for j from 0 below m
                         do
                            (when (and
                                   (< (distance (list i j)) min-dist)
                                   (= (elevation (list i j)) (char-code #\a)))
                              (setf min-dist (distance (list i j)))
                              (format t "~A ~A~%" (list i j) (distance (list i j)))
                              ))))))))


