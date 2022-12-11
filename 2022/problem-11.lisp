(defpackage :problem-11
  (:use :cl :cl-ppcre :alexandria :utils))

(in-package :problem-11)

(defclass monkey ()
  ((items :accessor items :initarg :items)
   (operation :accessor operation :initarg :operation)
   (test :accessor test :initarg :test)
   (true :accessor true :initarg :true)
   (false :accessor false :initarg :false)))

(let ((monkeys (make-array 8 :initial-contents  (list 
                                                 (make-instance 'monkey :items '(83 62 93)
                                                                        :operation (lambda (i) (* i 17))
                                                                        :test (lambda (i) (zerop (mod i 2)))
                                                                        :true 1
                                                                        :false 6)
                                                 (make-instance 'monkey :items '(90 55)
                                                                        :operation (lambda (i) (+ i 1))
                                                                        :test (lambda (i) (zerop (mod i 17)))
                                                                        :true 6
                                                                        :false 3)
                                                 (make-instance 'monkey :items '(91 78 80 97 79 88)
                                                                        :operation (lambda (i) (+ i 3))
                                                                        :test (lambda (i) (zerop (mod i 19)))
                                                                        :true 7
                                                                        :false 5)
                                                 (make-instance 'monkey :items '(64 80 83 89 59)
                                                                        :operation (lambda (i) (+ i 5))
                                                                        :test (lambda (i) (zerop (mod i 3)))
                                                                        :true 7
                                                                        :false 2)
                                                 (make-instance 'monkey :items '(98 92 99 51)
                                                                        :operation (lambda (i) (* i i))
                                                                        :test (lambda (i) (zerop (mod i 5)))
                                                                        :true 0
                                                                        :false 1)
                                                 (make-instance 'monkey :items '(68 57 95 85 98 75 98 75)
                                                                        :operation (lambda (i) (+ i 2))
                                                                        :test (lambda (i) (zerop (mod i 13)))
                                                                        :true 4
                                                                        :false 0)
                                                 (make-instance 'monkey :items '(74)
                                                                        :operation (lambda (i) (+ i 4))
                                                                        :test (lambda (i) (zerop (mod i 7)))
                                                                        :true 3
                                                                        :false 2)
                                                 (make-instance 'monkey :items '(68 64 60 68 87 80 82)
                                                                        :operation (lambda (i) (* i 19))
                                                                        :test (lambda (i) (zerop (mod i 11)))
                                                                        :true 4
                                                                        :false 5))))
      (inspections (make-hash-table)))
  (loop repeat 20 do
    (loop for monkey across monkeys
          for m from 0
          do
      (loop repeat (length (items monkey)) do
        (incf (gethash m inspections 0))
        (let ((new-item (truncate (funcall (operation monkey) (pop (items monkey))) 3)))
          (if (funcall (test monkey) new-item)
              (let ((true-monkey (aref monkeys (true monkey))))
                (setf (items true-monkey) (nconc (items true-monkey) (list new-item))))
              (let ((false-monkey (aref monkeys (false monkey))))
                (setf (items false-monkey) (nconc (items false-monkey) (list new-item))))
              )))))

  (apply #'*  (subseq (sort (loop for v being the hash-values of inspections collect v) #'>) 0 2)))



(let ((monkeys (make-array 8 :initial-contents  (list 
                                                 (make-instance 'monkey :items '(83 62 93)
                                                                        :operation (lambda (i) (* i 17))
                                                                        :test (lambda (i) (zerop (mod i 2)))
                                                                        :true 1
                                                                        :false 6)
                                                 (make-instance 'monkey :items '(90 55)
                                                                        :operation (lambda (i) (+ i 1))
                                                                        :test (lambda (i) (zerop (mod i 17)))
                                                                        :true 6
                                                                        :false 3)
                                                 (make-instance 'monkey :items '(91 78 80 97 79 88)
                                                                        :operation (lambda (i) (+ i 3))
                                                                        :test (lambda (i) (zerop (mod i 19)))
                                                                        :true 7
                                                                        :false 5)
                                                 (make-instance 'monkey :items '(64 80 83 89 59)
                                                                        :operation (lambda (i) (+ i 5))
                                                                        :test (lambda (i) (zerop (mod i 3)))
                                                                        :true 7
                                                                        :false 2)
                                                 (make-instance 'monkey :items '(98 92 99 51)
                                                                        :operation (lambda (i) (* i i))
                                                                        :test (lambda (i) (zerop (mod i 5)))
                                                                        :true 0
                                                                        :false 1)
                                                 (make-instance 'monkey :items '(68 57 95 85 98 75 98 75)
                                                                        :operation (lambda (i) (+ i 2))
                                                                        :test (lambda (i) (zerop (mod i 13)))
                                                                        :true 4
                                                                        :false 0)
                                                 (make-instance 'monkey :items '(74)
                                                                        :operation (lambda (i) (+ i 4))
                                                                        :test (lambda (i) (zerop (mod i 7)))
                                                                        :true 3
                                                                        :false 2)
                                                 (make-instance 'monkey :items '(68 64 60 68 87 80 82)
                                                                        :operation (lambda (i) (* i 19))
                                                                        :test (lambda (i) (zerop (mod i 11)))
                                                                        :true 4
                                                                        :false 5))))
      (inspections (make-hash-table)))
  (loop repeat 10000 do
    (loop for monkey across monkeys
          for m from 0
          do
      (loop repeat (length (items monkey)) do
        (incf (gethash m inspections 0))
        (let ((new-item (mod (funcall (operation monkey) (pop (items monkey)))
                             (* 2 17 19 3 5 13 7 11))))
          (if (funcall (test monkey) new-item)
              (let ((true-monkey (aref monkeys (true monkey))))
                (setf (items true-monkey) (nconc (items true-monkey) (list new-item))))
              (let ((false-monkey (aref monkeys (false monkey))))
                (setf (items false-monkey) (nconc (items false-monkey) (list new-item))))
              )))))

  (apply #'*  (subseq (sort (loop for v being the hash-values of inspections collect v) #'>) 0 2)))25738411485
