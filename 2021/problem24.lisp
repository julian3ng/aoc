(defpackage :problem24
  (:use :cl :cl-ppcre)
  (:import-from :cl-utilities :split-sequence)
  (:shadow :mod :eql))

(in-package :problem24)


(defclass ALU ()
  ((w :accessor w :initarg :w :initform 0 :type integer)
   (x :accessor x :initarg :x :initform 0 :type integer)
   (y :accessor y :initarg :y :initform 0 :type integer)
   (z :accessor z :initarg :z :initform 0 :type integer)
   (trace-ops :accessor trace-ops :initarg :trace-ops :initform nil)))

(defmethod print-object ((alu ALU) stream)
  (with-slots (w x y z) alu
    (format stream "~4,4TALU: w=~A, x=~A, y=~A, z=~A" w x y z)))

(defmethod get-register ((alu ALU) (register symbol))
  (ecase register
    (w (w alu))
    (x (x alu))
    (y (y alu))
    (z (z alu))))

(defmethod (setf get-register) (value (alu ALU) (register symbol))
  (ecase register
    (w (setf (w alu) value))
    (x (setf (x alu) value))
    (y (setf (y alu) value))
    (z (setf (z alu) value)))
  (when (trace-ops alu)
    (format t "~A~%" alu)))

(defmethod inp ((alu ALU) register &optional (stream *standard-input*))
  (let ((digit (digit-char-p (read-char stream))))
    (when (trace-ops alu)
      (format t "inp ~A <-- ~A~%" register digit))
    (setf (get-register alu register) digit)))

(defmethod add ((alu ALU) (register-a symbol) (register-b symbol))
  (let ((a (get-register alu register-a))
        (b (get-register alu register-b)))
    (when (trace-ops alu)
      (format t "add ~A(=~A) ~A(=~A)~%" register-a a register-b b))
    (setf (get-register alu register-a)
          (+ a b))))

(defmethod add ((alu ALU) (register-a symbol) (b integer))
  (let ((a (get-register alu register-a)))
    (when (trace-ops alu)
      (format t "add ~A(=~A) ~A~%" register-a a b))
    (setf (get-register alu register-a)
          (+ a b))))

(defmethod mul ((alu ALU) (register-a symbol) (register-b symbol))
  (let ((a (get-register alu register-a))
        (b (get-register alu register-b)))
    (when (trace-ops alu)
      (format t "mul ~A(=~A) ~A(=~A)~%" register-a a register-b b))
    (setf (get-register alu register-a)
          (* a b))))

(defmethod mul ((alu ALU) (register-a symbol) (b integer))
  (let ((a (get-register alu register-a)))
    (when (trace-ops alu)
      (format t "mul ~A(=~A) ~A~%" register-a a b))
    (setf (get-register alu register-a)
          (* a b))))

(defmethod div ((alu ALU) (register-a symbol) (register-b symbol))
  (let ((a (get-register alu register-a))
        (b (get-register alu register-b)))
    (when (trace-ops alu)
      (format t "div ~A(=~A) ~A(=~A)~%" register-a a register-b b))
    (setf (get-register alu register-a)
          (truncate a b))))

(defmethod div ((alu ALU) (register-a symbol) (b integer))
  (let ((a (get-register alu register-a)))
    (when (trace-ops alu)
      (format t "div ~A(=~A) ~A~%" register-a a b))
    (setf (get-register alu register-a)
          (truncate a b))))

(defmethod mod ((alu ALU) (register-a symbol) (register-b symbol))
  (let ((a (get-register alu register-a))
        (b (get-register alu register-b)))
    (when (trace-ops alu)
      (format t "mod ~A(=~A) ~A(=~A)~%" register-a a register-b b))
    (setf (get-register alu register-a)
          (rem a b))))

(defmethod mod ((alu ALU) (register-a symbol) (b integer))
  (let ((a (get-register alu register-a)))
    (when (trace-ops alu)
      (format t "mod ~A(=~A) ~A~%" register-a a b))
    (setf (get-register alu register-a)
          (rem a b))))

(defmethod eql ((alu ALU) (register-a symbol) (register-b symbol))
  (let ((a (get-register alu register-a))
        (b (get-register alu register-b)))
    (when (trace-ops alu)
      (format t "eql ~A(=~A) ~A(=~A)~%" register-a a register-b b))
    (setf (get-register alu register-a) (if (= a b) 1 0))))

(defmethod eql ((alu ALU) (register-a symbol) (b integer))
  (let ((a (get-register alu register-a)))
    (when (trace-ops alu)
      (format t "eql ~A(=~A) ~A~%" register-a a b))
    (setf (get-register alu register-a) (if (= a b) 1 0))))

(defmethod valid-p ((alu ALU))
  (zerop (get-register alu 'z)))

(defun parse-instruction (line)
  (cl-ppcre:register-groups-bind (instruction a b) ("(\\w{3}) (\\w{1}) ?(-?\\d+|\\w)?" line)
    (read-from-string (format nil "(~A ~A~:[)~;~:* ~A)~]" instruction a b))))

(let* ((lines (uiop:read-file-lines "./input/problem24.txt"))
       (instructions (mapcar #'parse-instruction lines))
       (alu (make-instance 'ALU)))          
  (with-input-from-string (*standard-input* "29599469991739")
    (loop for raw-instruction in instructions do
      (destructuring-bind (instruction &rest args) raw-instruction
        (apply instruction alu args)
        (when (eq instruction 'inp)
          (format t "~26R~%" (get-register alu 'z))))))
  (get-register alu 'z)) ; 0



(let* ((lines (uiop:read-file-lines "./input/problem24.txt"))
       (instructions (mapcar #'parse-instruction lines))
       (alu (make-instance 'ALU)))          
  (with-input-from-string (*standard-input* "17153114691118")
    (loop for raw-instruction in instructions do
      (destructuring-bind (instruction &rest args) raw-instruction
        (apply instruction alu args)
        (when (eq instruction 'inp)
          (format t "~26R~%" (get-register alu 'z))))))
  (get-register alu 'z)) ; 0


