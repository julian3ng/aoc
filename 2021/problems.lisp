(ql:quickload :cl-ppcre)
(ql:quickload :alexandria)
(defpackage :problems
  (:use :cl :uiop :cl-ppcre :alexandria))

(in-package :problems)

(declaim (optimize (debug 3) (speed 1) (safety 3)))


;; Problem 1
(defun count-increases (l)
  (count t (loop for (a b) on l while b collect (> b a))))

(defun count-sliding-window (l)
  (count t (loop for (a b c d) on l while d collect (> (+ b c d) (+ a b c)))))


(let ((l (mapcar #'parse-integer (uiop:read-file-lines "./input/problem1.txt"))))
  (count-sliding-window l) ; 1523
  (count-increases l) ; 1477
  )

;; Problem 2
(defun calculate-position-1 (l)
  (let ((pos (list 0 0)))
    (dolist (line l pos)
      (destructuring-bind (direction distance) (cl-ppcre:split " " line)
        (let ((dist (parse-integer distance)))
          (cond
            ((string= direction "down") (incf (first pos) dist))
            ((string= direction "up") (decf (first pos) dist))
            ((string= direction "forward") (incf (second pos) dist))
            (t (error "unreachable"))))))))


(defun calculate-position-2 (l)
  (let ((pos (list 0 0 0))) ; y x aim
    (dolist (line l (subseq pos 0 2))
      (destructuring-bind (direction distance) (cl-ppcre:split " " line)
        (let ((dist (parse-integer distance)))
          (cond
            ((string= direction "down") (incf (third pos) dist))
            ((string= direction "up") (decf (third pos) dist))
            ((string= direction "forward") (progn  (incf (second pos) dist)
                                                   (incf (first pos) (* dist (third pos)))))
            (t (error "unreachable")))))
      )))

(let ((l (uiop:read-file-lines "./input/problem2.txt")))
  (apply #'* (calculate-position-1 l)) ; 2187380  
  (apply #'* (calculate-position-2 l)) ; 2086357770
  )

;; Problem 3
(defun get-gamma-epsilon-rates (l)
  ;; Get counts for each bit, then take most/least popular for gamma and epsilon
  (let* ((n (length l))
         (m (length (first l)))
         (counts (loop repeat m collect 0)))
    (dolist (bstring l)
      (loop for i from 0 to (- m 1) do
        (when (char-equal #\1 (char bstring i))
          (incf (nth i counts)))))
    (let ((gamma-string (coerce (mapcar #'(lambda (c) (if (> c (/ n 2)) #\1 #\0)) counts) 'string))
          (epsilon-string (coerce (mapcar #'(lambda (c) (if (< c (/ n 2)) #\1 #\0)) counts) 'string)))
      (let ((gamma-rate (parse-integer gamma-string :radix 2))
            (epsilon-rate (parse-integer epsilon-string :radix 2)))
        (list gamma-rate epsilon-rate)))))

(apply #'*  (get-gamma-epsilon-rates (uiop:read-file-lines "./input/problem3.txt"))) ; 2595824


(defun count-nth-bits (l n)
  (loop for bstring in l counting (char= #\1 (char bstring n))))

(defun get-o2-co2-rates (l)
  ;; For each bit, count them and remove the most/least popular bitted numbers
  (let ((m (length (first l)))
        (o2 l)
        (co2 l))
    (loop for i from 0 to (1- m) do
      (let ((o2-count (count-nth-bits o2 i))
            (co2-count (count-nth-bits co2 i)))
        ;(format t "~A ~A" co2-count co2)
        (when (> (length o2) 1) 
          (if (>= o2-count (/ (length o2) 2))
              (setf o2 (remove-if-not #'(lambda (bstring) (char= #\1 (char bstring i))) o2))
              (setf o2 (remove-if #'(lambda (bstring) (char= #\1 (char bstring i))) o2))))
        (when (> (length co2) 1)
          (if (< co2-count (/ (length co2) 2))
              (setf co2 (remove-if-not #'(lambda (bstring) (char= #\1 (char bstring i))) co2))
              (setf co2 (remove-if #'(lambda (bstring) (char= #\1 (char bstring i))) co2))))
        ;(format t " ---> ~A~%" co2)
        ))
    (list (parse-integer (first o2) :radix 2) (parse-integer (first co2) :radix 2))))

(apply #'*  (get-o2-co2-rates (uiop:read-file-lines "./input/problem3.txt"))) ; 2135254


;; Problem 4
(defclass board ()
  ((size :initarg :s
         :initform (error "Supply :size")
         :accessor size)
   (cells :accessor cells)
   (won :accessor won :initform nil)))

(defmethod initialize-instance :after ((b board) &key s)
  (with-slots (size cells) b
    (setf cells (make-array (list size size)))
    (loop for i from 0 to (1- size) do
      (loop for j from 0 to (1- size) do
        (setf (aref cells i j) (list (- 1) nil))))))

(defmethod check-win ((b board))
  (when  (loop for i from 0 to (1- (size b))
                 thereis
                 (or 
                  (loop for j from 0 to (1- (size b)) always (second (aref (cells b) i j)))
                  (loop for j from 0 to (1- (size b)) always (second (aref (cells b) j i)))))
    (setf (won b) t)
    (won b)))

(defmethod score ((b board))
  (loop for i from 0 to (1- (size b))
        sum (loop for j from 0 to (1- (size b))
                  sum (let ((cell (aref (cells b) i j)))
                        (if (null (second cell))
                            (first cell)
                            0)))))

(defmethod mark-number ((b board) (k number))
  (loop named top for i from 0 to (1- (size b)) do
    (loop for j from 0 to (1- (size b)) do
      (when (= k (first (aref (cells b) i j)))
        (setf (second (aref (cells b) i j)) t)
        (return-from top))))
  (when (check-win b)
    (list k (score b))))

(let ((b (make-instance 'board :s 3)))
  (setf (aref (cells b) 0 2) '(0 nil))  
  (setf (aref (cells b) 1 2) '(1 nil))
  (setf (aref (cells b) 2 2) '(2 nil))
  (mark-number b 0)
  (mark-number b 1)
  (prog1 
      (mark-number b 2)
    (print (cells b)))
  )


(defun parse-input-4 (s)
  (let* ((chunks (cl-ppcre:split "\\n\\n" s))
         (order (first chunks)))
    (list order (loop for chunk in (subseq chunks 1)
                      collect 
                      (let* ((n (length (cl-ppcre:split "\\n" chunk)))
                             (b (make-instance 'board :s n))
                             (cells (remove-if #'(lambda (s) (string= "" s)) (cl-ppcre:split "\\s" chunk))))
                        (loop for i from 0 to (1- n) do
                          (loop for j from 0 to (1- n) do
                            (setf (aref (cells b) i j)
                                  (list (parse-integer (elt cells (+ (* i n) j))) nil))))
                        b)))))


(defun bingo-1 (s)
  ;; Go through numbers, marking on each board
  ;; On win, return number and score
  (destructuring-bind (order boards) (parse-input-4 s)
    (let ((numbers (mapcar #'parse-integer (cl-ppcre:split "," order))))
      (loop named top for number in numbers do
        (loop for b in boards do
          (let ((result (mark-number b number)))
            (when result
              (return-from top result))))))))

(apply #'* (bingo-1 (uiop:read-file-string "./input/problem4.txt"))) ; 44736

(defun bingo-2 (s)
  ;; Go through numbers, removing boards when they win
  ;; On last board, continue until won, then return score
  (destructuring-bind (order boards) (parse-input-4 s)
    (let ((numbers (mapcar #'parse-integer (cl-ppcre:split "," order)))
          (bs boards)
          (last nil))
      
      (loop named top for number in numbers do
        (loop for i from 0 to (1- (length bs)) do
          (mark-number (elt bs i) number)
          (when (and last (won (elt bs i)))
            (return-from top (* number (score (elt bs i))))))
        (setf bs (remove-if #'(lambda (b) (won b)) bs))
        (when (= 1 (length bs))
          (setf last t))))))

(bingo-2 (uiop:read-file-string "./input/problem4.txt"))


;; Problem 5
;; Screw math, we're making the board
(defun problem-5-a (input)
  (let ((lines (mapcar #'(lambda (l) (mapcar #'parse-integer (cl-ppcre:split ",| -> " l))) input)))
    (destructuring-bind (maxy maxx) 
        (loop with maxx = 0 with maxy = 0 for l in lines do
          (progn
            (when (> (first l) maxy) (setf maxy (first l)))
            (when (> (second l) maxx) (setf maxx (second l)))
            (when (> (third l) maxy) (setf maxy (third l)))
            (when (> (fourth l) maxx) (setf maxx (fourth l))))
              finally
                 (return (list maxy maxx)))
      (let ((board (make-array (list (1+ maxy) (1+ maxx)) :element-type 'number :initial-element 0)))
        (loop for line in lines do
          (let ((y1 (first line))
                (x1 (second line))
                (y2 (third line))
                (x2 (fourth line)))
            (cond
              ((= y1 y2) (loop for k from (min x1 x2) to (max x1 x2) do (incf (aref board y1 k))))
              ((= x1 x2) (loop for k from (min y1 y2) to (max y1 y2) do (incf (aref board k x1)))))))
        (loop for i from 0 to maxy
              sum (loop for j from 0 to maxx
                        count (> (aref board i j) 1)))))))

(problem-5-a (uiop:read-file-lines "./input/problem5.txt"))

(defun problem-5-b (input)
  (let ((lines (mapcar #'(lambda (l) (mapcar #'parse-integer (cl-ppcre:split ",| -> " l))) input)))
    (destructuring-bind (maxy maxx) 
        (loop with maxx = 0 with maxy = 0 for l in lines do
          (progn
            (when (> (first l) maxy) (setf maxy (first l)))
            (when (> (second l) maxx) (setf maxx (second l)))
            (when (> (third l) maxy) (setf maxy (third l)))
            (when (> (fourth l) maxx) (setf maxx (fourth l))))
              finally
                 (return (list maxy maxx)))
      (let ((board (make-array (list (1+ maxy) (1+ maxx)) :element-type 'number :initial-element 0)))
        (loop for line in lines do
          (let ((y1 (first line))
                (x1 (second line))
                (y2 (third line))
                (x2 (fourth line)))
            (cond
              ((= y1 y2) (loop for k from (min x1 x2) to (max x1 x2) do (incf (aref board y1 k))))
              ((= x1 x2) (loop for k from (min y1 y2) to (max y1 y2) do (incf (aref board k x1))))
              (t
               ;(progn (format t "~&(~A ~A) -> (~A ~A)" y2 x2 y1 x1))
               (if (< x1 x2)
                   (if (< y1 y2)
                       (loop for k from 0 to (- x2 x1) do (incf (aref board (+ y1 k) (+ x1 k))))
                       (loop for k from 0 to (- x2 x1) do (incf (aref board (- y1 k) (+ x1 k)))))
                   (if (< y1 y2)
                       (loop for k from 0 to (- x1 x2) do (incf (aref board (- y2 k) (+ x2 k))))
                       (loop for k from 0 to (- x1 x2) do (incf (aref board (+ y2 k) (+ x2 k))))))))))
        (loop for i from 0 to maxy
              sum (loop for j from 0 to maxx
                        count (> (aref board i j) 1)))))))


(problem-5-b (uiop:read-file-lines "./input/problem5.txt"))


;; Problem 6

(defun brute-force-lanternfish (l k)
  (let ((fish (make-array (length l) :element-type 'number
                                     :initial-contents l)))
    (loop repeat k do
      (progn
        (loop for i from 0 to (1- (length fish)) do (decf (elt fish i)))
        (let ((new-fish (loop for i from 0 to (1- (length fish))
                              when (= (- 1) (elt fish i))
                                collect
                              (progn (setf (elt fish i) 6) 8))))
          (setf fish (concatenate 'vector fish new-fish))
          )))
    fish))

;; Had to peek for this algorithm :(
(defun lanternfish-b (l k)
  (let ((fishes (make-array 9 :element-type 'number :initial-element 0)))
    (loop for fish in l do
      (incf (aref fishes fish)))
    (loop repeat k do
      (let ((newfish (aref fishes 0)))
        (alexandria:rotate fishes (- 1))
        (incf (aref fishes 6) newfish)))
    fishes))

(let ((l (mapcar #'parse-integer (cl-ppcre:split "," (uiop:read-file-string "./input/problem6.txt")))))
  (loop for x across (lanternfish-b l 80) summing x) ; 358214
  (loop for x across (lanternfish-b l 256) summing x)) ; ;1622533344325



;; Problem 7

(defun brute-force-7-a (l)
  (loop for i from 0 to (apply #'max l) minimize
        (apply #'+ (mapcar #'(lambda (x) (abs (- x i))) l))))


(defun fuel-cost (a b)
  (let ((dist (abs (- a b))))
    (/ (* dist (1+ dist)) 2)))

(defun brute-force-7-b (l)
  (loop for i from 0 to (apply #'max l) minimize
        (apply #'+ (mapcar #'(lambda (x) (fuel-cost x i)) l))))


(let ((crabs (mapcar #'parse-integer (cl-ppcre:split "," (uiop:read-file-string "./input/problem7.txt")))))
  (brute-force-7-a crabs) ; 355521
  (brute-force-7-b crabs)) ; 100148777
