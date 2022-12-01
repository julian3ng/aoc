(defpackage :problem23
  (:use :cl :alexandria))

(in-package :problem23)

(defclass amphipod ()
  ((pod-type :accessor pod-type :initarg :pod-type)
   (multiplier :accessor multiplier)
   (moved-once-p :accessor moved-once-p :initform nil)
   (donep :accessor donep :initform nil)))

(defmethod initialize-instance :after ((pod amphipod) &key)
  (with-slots (pod-type multiplier) pod
    (setf multiplier (case pod-type
                       (a 1)
                       (b 10)
                       (c 100)
                       (d 1000)
                       (t (error "Bad pod type"))))))

(defun make-amphipod (pod-type)
  (make-instance 'amphipod :pod-type pod-type))

(defmethod print-object ((a amphipod) stream)
  (with-slots (pod-type) a
    (format stream " ~A " pod-type)))

(defclass board ()
  ((state :accessor state :initform (make-array '(3 11) :initial-element nil))))

(defmethod initialize-instance :after ((b board) &key pods)
  (with-slots (state) b
    (loop for (i j) in '((1 2) (2 2) (1 4) (2 4) (1 6) (2 6) (1 8) (2 8))
          for k = 0 then (1+ k) do
            (setf (aref state i j) (make-amphipod (elt pods k))))))


(defmethod board= ((b1 board) (b2 board))
  (with-slots ((state1 state)) b1
    (with-slots ((state2 state)) b2
      (destructuring-bind (n m) (array-dimensions state1)
        (loop for i from 0 to (1- n)
              always (loop for j from 0 to (1- m)
                           always (or (and (null (aref state1 i j)) (null (aref state2 i j)))
                                      (eq (pod-type (aref state1 i j)) (pod-type (aref state2 i j))))))))))

(defmethod goals ((b board) (a amphipod))
  (case (pod-type a)
    (a '((1 2) (2 2)))
    (b '((1 4) (2 4)))
    (c '((1 6) (2 6)))
    (d '((1 8) (2 8)))
    (t (error "bad amphipod"))))


(defmethod print-object ((b board) stream)
  (with-slots (state) b
    (format stream "~A" state)))

(board= (make-instance 'board :pods '(a a b b c c d d))
        (make-instance 'board :pods '(a a b b c c d d)))

(let ((best-scores (make-hash-table :test #'equal))
      (initial-board (make-instance 'board :pods '(b c a d b d c a))))
  (setf (gethash (format nil "~A"  initial-board) best-scores) 0)
  (gethash (format nil "~A"  initial-board) best-scores))
