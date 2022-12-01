(defpackage :problems
  (:use :cl :uiop :cl-ppcre))

(in-package :problems)

(declaim (optimize (debug 0) (speed 3) (space 3)))


(defun foo (x y &key (a 1) (b 2))
  (list x y a b))

;; Macros!
(defmacro do-2d-array ((var arr) &body body)
  (let ((i (gensym))
        (j (gensym)))
    `(loop for ,i from 0 to (1- (array-dimension ,arr 0)) do
      (loop for ,j from 0 to (1- (array-dimension ,arr 1)) do
        (let ((,var (aref ,arr ,i ,j)))
          ,@body)))))



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
        (when (> (length o2) 1) 
          (if (>= o2-count (/ (length o2) 2))
              (setf o2 (remove-if-not #'(lambda (bstring) (char= #\1 (char bstring i))) o2))
              (setf o2 (remove-if #'(lambda (bstring) (char= #\1 (char bstring i))) o2))))
        (when (> (length co2) 1)
          (if (< co2-count (/ (length co2) 2))
              (setf co2 (remove-if-not #'(lambda (bstring) (char= #\1 (char bstring i))) co2))
              (setf co2 (remove-if #'(lambda (bstring) (char= #\1 (char bstring i))) co2))))
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


(defmethod initialize-instance :after ((b board) &key)
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

;; Note: Apparently this should be the geometric median
;; https://en.wikipedia.org/wiki/Geometric_median
(defun brute-force-7-a (l)
  (loop for i from 0 to (apply #'max l) minimize
        (apply #'+ (mapcar #'(lambda (x) (abs (- x i))) l))))

(defun better-7-a (l) 
  (let ((m (alexandria:median l)))
    (apply #'+ (mapcar #'(lambda (x) (abs (- x m))) l))))



(defun fuel-cost (a b)
  (let ((dist (abs (- a b))))
    (/ (* dist (1+ dist)) 2)))

;; Use weighted median for good soln here?
(defun brute-force-7-b (l)
  (loop for i from 0 to (apply #'max l) minimize
        (apply #'+ (mapcar #'(lambda (x) (fuel-cost x i)) l))))

(let ((crabs (mapcar #'parse-integer (cl-ppcre:split "," (uiop:read-file-string "./input/problem7.txt")))))
  (brute-force-7-a crabs) ; 355521
  (brute-force-7-b crabs)) ; 100148777


;; Problem 8

(defun count-output-1-4-7-8 (outputs)
  (loop for output in outputs
        sum (loop for digit in output count (or (= (length digit) 2)
                                                   (= (length digit) 3)
                                                   (= (length digit) 4)
                                                   (= (length digit) 7)))))

(defun deduce-segments (inputs)
  (let ((one (find 2 inputs :key #'length))
        (four (find 4 inputs :key #'length))
        (seven (find 3 inputs :key #'length))
        (eight (find 7 inputs :key #'length)))
    (labels ((chars-of (s)
               (map 'list #'identity s))) 
      (let* ((cf (chars-of one))
             (a (set-difference (chars-of seven) cf))
             (bd (set-difference (chars-of four) cf))
             (eg (reduce #'set-difference (list  (chars-of eight) (chars-of four) a)))
             (fivers (remove-if-not #'(lambda (s) (= 5 (length s))) inputs))
             (sixers (remove-if-not #'(lambda (s) (= 6 (length s))) inputs))
             (b (find 1 (loop for sixer in sixers
                              collect (reduce #'set-difference (list (chars-of sixer) a cf eg)))
                      :key #'length))
             (d (set-difference bd b))
             (f (find 1 (loop for sixer in sixers
                              collect (reduce #'set-difference (list (chars-of sixer) a bd eg)))
                      :key #'length))
             (c (set-difference cf f))
             (g (find 1 (loop for fiver in fivers
                              collect (reduce #'set-difference (list (chars-of fiver) a bd cf)))
                      :key #'length))
             (e (set-difference eg g)))

        (let ((h (make-hash-table)))
          (setf (gethash (first a) h) #\a)
          (setf (gethash (first b) h) #\b)
          (setf (gethash (first c) h) #\c)
          (setf (gethash (first d) h) #\d)
          (setf (gethash (first e) h) #\e)
          (setf (gethash (first f) h) #\f)
          (setf (gethash (first g) h) #\g)
          h)))))

(defun deduce-digits (output segments)
  (labels ((convert-digit (digit)
             (loop for c across digit collect (gethash c segments)))
           (digit-to-int (digit)
             (let ((digit-segments (convert-digit digit)))
               (cond
                 ((null (set-exclusive-or digit-segments '(#\a #\b #\c #\e #\f #\g))) #\0)
                 ((null (set-exclusive-or digit-segments '(#\c #\f))) #\1)
                 ((null (set-exclusive-or digit-segments '(#\a #\c #\d #\e #\g))) #\2)
                 ((null (set-exclusive-or digit-segments '(#\a #\c #\d #\f #\g))) #\3)
                 ((null (set-exclusive-or digit-segments '(#\b #\c #\d #\f))) #\4)
                 ((null (set-exclusive-or digit-segments '(#\a #\b #\d #\f #\g))) #\5)
                 ((null (set-exclusive-or digit-segments '(#\a #\b #\d #\e #\f #\g))) #\6)
                 ((null (set-exclusive-or digit-segments '(#\a #\c #\f))) #\7)
                 ((null (set-exclusive-or digit-segments '(#\a #\b #\c #\d #\e #\f #\g))) #\8)
                 ((null (set-exclusive-or digit-segments '(#\a #\b #\c #\d #\f #\g))) #\9)))))
    (parse-integer (map 'string #'digit-to-int output))))

(let ((seven-segments
        (mapcar #'(lambda (line) (cl-ppcre:split " \\| "  line)) (uiop:read-file-lines "./input/problem8.txt"))))
  (let ((outputs (mapcar #'(lambda (io) (cl-ppcre:split " " (second io))) seven-segments)))
    (count-output-1-4-7-8 outputs))
  (loop for io in seven-segments
        summing (let ((inputs (first io))
                      (outputs (second io)))
                  (let ((output (cl-ppcre:split " " outputs))
                        (input  (cl-ppcre:split " " inputs)))
                    (deduce-digits output (deduce-segments input))))))

;; Problem 9
(defun find-low-points (heightmap)
  (let ((n (array-dimension heightmap 0))
        (m (array-dimension heightmap 1)))
    (labels ((check-neighbors (i j)
               (let ((potential-neighbors (list (list (1- i) j)
                                                (list (1+ i) j)
                                                (list i (1- j))
                                                (list i (1+ j)))))
                 (loop for (y x) in potential-neighbors
                       always (or (< y 0) (< x 0)
                                  (>= y n) (>= x m)
                                  (char< (aref heightmap i j) (aref heightmap y x)))))))
      (loop for i from 0 to (1- n)
            nconc (loop for j from 0 to (1- m)
                     when (check-neighbors i j)
                       collect (list i j))))))

(defun risk-level (heightmap low-point)
  (destructuring-bind (y x) low-point
    (1+ (digit-char-p (aref heightmap y x)))))


(defun flood-fill (heightmap low-point)
  (let ((visited (make-hash-table :test #'equal))
        (fill-size 0)
        (n (array-dimension heightmap 0))
        (m (array-dimension heightmap 1)))
    (labels ((out-of-bounds (lp)
               (destructuring-bind (y x) lp
                 (or (< y 0) (< x 0) (>= y n) (>= x m))))
             (get-neighbors (lp)
               (destructuring-bind (y x) lp
                 (remove-if #'out-of-bounds 
                            (list (list (1- y) x) (list (1+ y) x) (list y (1- x)) (list y (1+ x))))))
             (dfs (lp)
               (when (not (gethash lp visited))
                 (setf (gethash lp visited) t)
                 (destructuring-bind (y x) lp
                   (when (char< (aref heightmap y x) #\9)
                     (incf fill-size)
                     (dolist (neighbor (get-neighbors lp))
                       (dfs neighbor)))))))
      (dfs low-point)
      fill-size)))


(let* ((lines (uiop:read-file-lines "./input/problem9.txt"))
       (n (length lines))
       (m (length (first lines)))
       (heightmap  (make-array (list n m) :initial-contents lines))
       (low-points (find-low-points heightmap)))

  (loop for lp in low-points summing (risk-level heightmap lp)) ; 452
  (apply #'* (subseq (sort (loop for lp in low-points collect (flood-fill heightmap lp)) #'>) 0 3))) ; 1263735

(flood-fill (make-array '(3 3) :initial-contents '((#\0 #\1 #\2) (#\1 #\2 #\9) (#\9 #\9 #\9))) '(0 0))


;; Problem 10


(defun find-first-illegal (line)
  (labels ((open-delim (c)
             (some  #'(lambda (opener) (char= opener c))  '(#\( #\[ #\{ #\<)))
           (match-delim (c opener)
             (or 
              (and (char= opener #\() (char= c #\)))
              (and (char= opener #\[) (char= c #\]))
              (and (char= opener #\{) (char= c #\}))
              (and (char= opener #\<) (char= c #\>)))))
    (let ((stack (list))) 
      (loop for c across line do
        (progn
          ; (format t "~{~A ~}~%" (reverse stack))
          (cond
            ((null stack) (push c stack))
            ((open-delim c) (push c stack))
            ((match-delim c (first stack)) (pop stack))
            (t (return-from find-first-illegal c))))))))

(defun score-delimiter (closer)
  (case closer
    (#\) 3)
    (#\] 57)
    (#\} 1197)
    (#\> 25137)))

(defun problem-10-a (lines)
  (loop for line in lines summing (let ((first-illegal (find-first-illegal line)))
                                    (if first-illegal
                                        (score-delimiter first-illegal)
                                        0))))


(defun complete-incomplete (line)
  (labels ((open-delim (c)
             (some  #'(lambda (opener) (char= opener c))  '(#\( #\[ #\{ #\<)))
           (match-delim (c opener)
             (or 
              (and (char= opener #\() (char= c #\)))
              (and (char= opener #\[) (char= c #\]))
              (and (char= opener #\{) (char= c #\}))
              (and (char= opener #\<) (char= c #\>))))
           (close-delim (opener)
             (case opener
               (#\( #\))
               (#\[ #\])
               (#\{ #\})
               (#\< #\>))))
    (let ((stack (list)))
      (loop for c across line do
        (progn
          (cond
            ((null stack) (push c stack))
            ((open-delim c) (push c stack))
            ((match-delim c (first stack)) (pop stack)))))
      (loop for c in stack collect (close-delim c)))))

(defun score-completion (completion)
  (let ((score 0))
    (loop for close-delim in completion do
      (setf score (* score 5))
      (incf score (case close-delim
                    (#\) 1)
                    (#\] 2)
                    (#\} 3)
                    (#\> 4)
                    (t 0))))
    score))


(defun problem-10-b (lines)
  (let ((scores (loop for line in lines when (null (find-first-illegal line))
                      collect (score-completion (complete-incomplete line)))))
    (setf scores (sort scores #'<)) ; destructive
    (alexandria:median scores)))


(let ((lines (uiop:read-file-lines "./input/problem10.txt")))
  (problem-10-a lines)  ; 436497
  (problem-10-b lines)  ; 2377613374
  )


;; Problem 11

(defun make-board-11 (lines)
  (let* ((n (length lines))
         (m (length (first lines)))
         (board (make-array (list n m) :element-type 'number)))
    (loop for i from 0 to (1- n) do
      (loop for j from 0 to (1- m) do
        (let ((c (char (elt lines i) j)))
          (setf (aref board i j) (digit-char-p c 16)))))
    board))


(defun print-board (board)
  (let ((n (array-dimension board 0))
        (m (array-dimension board 1)))
    (format t "~&")
    (loop for i from 0 to (1- n) do
      (loop for j from 0 to (1- m) do
        (format t "~36R" (aref board i j)))
      (format t "~&"))))

(defun coords-around (i j)
  (loop for y from (1- i) to (1+ i)
        nconc (loop for x from (1- j) to (1+ j) when (not (and (= i y) (= j x)))
                    collect
                    (list y x))))

(defun out-of-bounds (coords bounds)
  (not (and (<= 0 (first coords) (1- (first bounds)))
            (<= 0 (second coords) (1- (second bounds))))))

(defun flashable (board)
  (let ((n (array-dimension board 0))
        (m (array-dimension board 1)))
    (loop for i from 0 to (1- n)
            thereis (loop for j from 0 to (1- m)
                            thereis (> (aref board i j) 9)))))

(defun step-board (board)
  (let ((n (array-dimension board 0))
        (m (array-dimension board 1))
        (flashed (make-hash-table :test #'equal)))
    ;; Step 1: inc all 
    (loop for i from 0 to (1- n) do
      (loop for j from 0 to (1- m) do
        (incf (aref board i j))))
    ; (print "================")
    ;; (print "Inced")
    ;; (print-board board)

    ;; Flash    
    (labels ((flash? (y x)
               (when (and (not (gethash (list y x) flashed)) (> (aref board y x) 9)) 
                 (setf (gethash (list y x) flashed) t)
                 (let ((neighbors (remove-if #'(lambda (coord) (out-of-bounds coord (list n m)))
                                             (coords-around y x))))
                   (loop for (y x) in neighbors do
                     (incf (aref board y x)))
                   (loop for (y x) in neighbors when (not (gethash (list y x) flashed)) do (flash? y x))))))

      (loop for i from 0 to (1- n) do
        (loop for j from 0 to (1- m) do
          (flash? i j))))

    ;; (print "Flashed")
    ;; (print-board board)
          
    ;; Reset
    (loop for (i j) being the hash-keys of flashed do (setf (aref board i j) 0))
    ;; (print "Reset")
    ;(print-board board)
    (loop for k being the hash-keys of flashed count k)))


(defun step-until-synced (board)
  (let* ((size (* (array-dimension board 0) (array-dimension board 1))))
    (loop with i = 0 do
      (progn 
        (incf i)
        ;(format t "**************** ~A ****************~%" i)
        (when (= size (step-board board))
          (return-from step-until-synced i))))))

(let ((board (make-board-11  (uiop:read-file-lines "./input/problem11.txt"))))
  (loop repeat 100 sum (step-board board))) ; 1688


;; Have to separate this out because of state
(let ((board (make-board-11  (uiop:read-file-lines "./input/problem11.txt"))))
  (step-until-synced board)) ; 403


;; Problem 12

(defun make-problem-12-graph (lines)
  (let ((h (make-hash-table :test #'equal)))
    (loop for line in lines do
      (destructuring-bind (a b) (cl-ppcre:split "-" line)
        (unless (or (equal b "start") (equal a "end")) 
          (setf (gethash a h) (cons b (gethash a h))))

        ;; Shouldn't have anyone going back to start
        ;; Shouldn't have end going back to anyone
        (unless (or (equal a "start") (equal b "end")) 
          (setf (gethash b h) (cons a (gethash b h))))))
    h))

(defun print-graph (graph)
  (maphash #'(lambda (k v) (format t "~A: ~A~%" k v)) graph))

(defun small-cave-p (cave)
  (lower-case-p (char cave 0)))

(defun count-start-end-paths (graph)
  (let ((counter 0)) 
    (labels ((dfs (start-node end-node visited)
               ;(format t "~&================~%")
               ;(format t "DFS: ~A ~A ~A~%" start-node end-node visited)
               (if (equal start-node end-node)
                   (incf counter)
                   (progn 
                     (loop for v in (gethash start-node graph)
                           when (or (not (small-cave-p v)) (not (find v visited :test #'equal)))
                             do (dfs v end-node (cons start-node visited)))))))
      (dfs "start" "end" '()))
    counter))


(defun count-start-end-paths-double-smalls (graph)
  (let ((counter 0)) 
    (labels ((dfs (start-node end-node visited doubled?)
               (if (equal start-node end-node)
                   (progn 
                     (incf counter)
                     ;; (format t "~{~A~^,~}~%" (reverse (cons end-node visited)))
                     )
                   (if doubled?
                       (loop for v in (gethash start-node graph)
                             when (or (not (small-cave-p v))
                                      (not (find v visited :test #'equal)))
                               do (dfs v end-node (cons start-node visited) t))
                       (loop for v in (gethash start-node graph)
                             do (dfs v end-node (cons start-node visited)
                                     (if (small-cave-p v)
                                         (find v visited :test #'equal)
                                         nil)))))))
      (dfs "start" "end" '() nil))
    counter))

(let ((graph (make-problem-12-graph (uiop:read-file-lines "./input/problem12.txt"))))
  (count-start-end-paths graph) ; 3495
  (count-start-end-paths-double-smalls graph) ; 94849
  )

;; Problem 13

(defun make-board-13 (input)
  (let ((maxy 0)
        (maxx 0)
        (points nil)) 
    (destructuring-bind (dotstrings foldstrings) (cl-ppcre:split "\\n\\n" input)
       (let ((lines (cl-ppcre:split "\\n" dotstrings)))
         (loop for dotstring in lines do
           (progn
             (let ((point (mapcar #'parse-integer (cl-ppcre:split "," dotstring))))
               (setf points (cons point points))
               (when (> (first point) maxx)
                 (setf maxx (first point)))
               (when (> (second point) maxy)
                 (setf maxy (second point)))))))
       (let ((board (make-array (list (1+  maxy) (1+ maxx)) :initial-element #\.)))
         (loop for point in points do
           (setf (aref board (second point) (first point)) #\#))

         (let ((folds nil))
           (cl-ppcre:do-register-groups (y ycoord x xcoord) ("(y)=(\\d+)|(x)=(\\d+)" foldstrings)
             (when y (setf folds (cons (list :y (parse-integer ycoord)) folds)))
             (when x (setf folds (cons (list :x (parse-integer xcoord)) folds))))
           (values board (reverse folds)))))))


(defun fold-paper (board fold)
  (let ((n (array-dimension board 0))
        (m (array-dimension board 1)))
    (cond
      ((eql (first fold) :y) (let* ((new-n (second fold))
                                    (new-board (make-array (list new-n m))))
                               (loop for i from 0 to (1- new-n) do
                                 (loop for j from 0 to (1- m) do
                                   (setf (aref new-board i j) (aref board i j))))
                               (loop for i from (1+ new-n) to (1- n) do
                                 (loop for j from 0 to (1- m) do
                                   (let ((new-point (aref board i j)))
                                     (when (char= new-point #\#)
                                       (setf (aref new-board (- new-n (- i new-n)) j) new-point)))))
                               new-board))
      ((eql (first fold) :x) (let* ((new-m (second fold))
                                    (new-board (make-array (list n new-m))))
                               (loop for i from 0 to (1- n) do
                                 (loop for j from 0 to (1- new-m) do
                                   (setf (aref new-board i j) (aref board i j))))
                               (loop for i from 0 to (1- n) do
                                 (loop for j from (1+ new-m) to (1- m) do
                                   (let ((new-point (aref board i j)))
                                     (when (char= new-point #\#)  
                                       (setf (aref new-board i (- new-m (- j new-m))) new-point)))))
                               new-board))
      (t (error "bad fold")))))


(defun fold-paper-completely (board folds)
  (let ((cur-board board))
    (dolist (fold folds) 
      (setf cur-board (fold-paper cur-board fold)))
    cur-board))

(defun print-board-13 (board)
  (loop for i from 0 to (1- (array-dimension board 0)) do
    (loop for j from 0 to (1- (array-dimension board 1)) do
      (format t "~c" (aref board i j)))
    (format t "~%")))

(defun count-dots (board)
  (loop for i from 0 to (1- (array-dimension board 0)) sum
    (loop for j from 0 to (1- (array-dimension board 1)) count
          (char= (aref board i j) #\#))))


(multiple-value-bind (board folds) (make-board-13 "6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5")
  (print-board-13 (fold-paper-completely board folds)))


(multiple-value-bind (board folds) (make-board-13 (uiop:read-file-string "./input/problem13.txt"))
  (count-dots (fold-paper board (first folds))) ; 647
  (print-board (fold-paper-completely board folds)))


;; Problem 14

(defun parse-input-14 (input-str)
  (destructuring-bind (initial-polymer pair-insert-str) (cl-ppcre:split "\\n\\n" input-str)
    (let ((all-pair-rules (make-hash-table :test #'equal)))
      (cl-ppcre:do-register-groups (pair insertee) ("(.*) -> (.)" pair-insert-str)
        (setf (gethash pair all-pair-rules) insertee))
      (list initial-polymer all-pair-rules))))


(defun apply-rule-one-step (polymer rules)
  ;(format t "~&0 ~A: ~A" (length polymer) polymer)
  (let ((next-polymer (make-array (length polymer)
                                 :element-type 'character
                                 :fill-pointer 0
                                 :adjustable t)))
    (loop for i from 0 to (- (length polymer) 2)
          do
             (vector-push-extend (char polymer i) next-polymer)
             (let ((insertee (gethash (subseq polymer i (+ i 2)) rules)))
               (when insertee
                 (vector-push-extend (coerce insertee 'character) next-polymer))))
    (vector-push-extend (char polymer (1- (length polymer))) next-polymer)
    next-polymer))

(defun apply-n-steps (polymer rules n)
  (let ((current-polymer polymer))
    (loop repeat n do (setf current-polymer (apply-rule-one-step current-polymer rules)))
    current-polymer))

(defun problem-14-a (polymer rules)
  (let ((tenth-polymer (apply-n-steps polymer rules 10))
        (element-counts (make-hash-table)))
    (loop for element across tenth-polymer do
      (if (gethash element element-counts)
          (incf (gethash element element-counts))
          (setf (gethash element element-counts) 1)))
    (let ((max-count (loop for v being the hash-values of element-counts maximize v))
          (min-count (loop for v being the hash-values of element-counts minimize v)))
      (- max-count min-count))))

(defun print-hash-table (h)
  (maphash #'(lambda (k v) (format t "~A: ~A~%" k v)) h))

;; Brute force won't work this time :(
(defun problem-14-b (polymer rules)
  ;; Had to cheat :(
  ;; Key idea: aggregate things when you can to avoid the exponential

  
  (let ((char-counts (make-hash-table))
        (pair-counts (make-hash-table :test #'equal)))
    ;; Setup
    (loop for i from 0 to (1- (length polymer)) do
      (incf (gethash (char polymer i) char-counts 0))
      (when (<= (+ i 2) (length polymer))
        (incf (gethash (subseq polymer i (+ i 2)) pair-counts 0))))

    (loop repeat 40 do
      (let ((pair-counts-copy (alexandria:copy-hash-table pair-counts)))
        (loop for k being the hash-keys of pair-counts-copy using (hash-value v) do
          (let* ((insertee (coerce (gethash k rules) 'character))
                 (pair-a (format nil "~c~c" (char k 0) insertee))
                 (pair-b (format nil "~c~c" insertee (char k 1))))
            (decf (gethash k pair-counts) v)
            (incf (gethash pair-a pair-counts 0) v)
            (incf (gethash pair-b pair-counts 0) v)
            (incf (gethash insertee char-counts 0) v)))))
    (let ((max-count (apply #'max (alexandria:hash-table-values char-counts)))
          (min-count (apply #'min (alexandria:hash-table-values char-counts))))
      (- max-count min-count))))


(destructuring-bind (first-polymer rules) (parse-input-14 (uiop:read-file-string "./input/problem14.txt"))
  (problem-14-a first-polymer rules) ; 2967
  (problem-14-b first-polymer rules) ; 3692219987038
  )



;; Problem 15
(defun parse-input-15 (lines)
  (let* ((n (length lines))
         (m (length (first lines)))
         (board (make-array (list n m) :initial-contents lines)))
    (loop for i from 0 to (1- n) do
      (loop for j from 0 to (1- m) do
            (setf (aref board i j) (digit-char-p (aref board i j)))))

    board))

(defun create-input-15-b (board)
  (let* ((n (array-dimension board 0))
         (m (array-dimension board 1))
         (new-n (* 5 n))
         (new-m (* 5 m))
         (new-board (make-array `(,new-n ,new-m)))
         (combos (loop for i from 0 to 4 nconc (loop for j from 0 to 4 collect (list i j)))))
    (loop for (my mx) in combos do
      (loop for i from 0 to (1- n) do
        (loop for j from 0 to (1- m) do
          (setf (aref new-board (+ (* my n) i) (+ (* mx m) j))
                (multiple-value-bind (quotient remainder) (floor (+ (aref board i j) my mx) 10)
                  (if (>= quotient 1)
                      (1+ remainder)
                      remainder))))))
    new-board))

;; HEAP
(defclass heap ()
  ((backing-array :initform (make-array 16 :adjustable t :fill-pointer 1)
                  :accessor backing-array)
   (key :initform #'identity :initarg :key :accessor key)
   (elements :initform (make-hash-table :test #'equal) :accessor elements)))

(defmethod insert ((h heap) element)
  (with-slots (backing-array key elements) h
    (let* ((index (let ((possible-index (gethash element elements)))
                    (or possible-index (vector-push-extend element backing-array))))
           (parent (if (> index 1)
                       (floor index 2)
                       nil)))
      (when parent
        (loop until (or
                     (null parent)
                     (<
                      (funcall key (aref backing-array parent))
                      (funcall key (aref backing-array index))))
              do (rotatef (aref backing-array parent) (aref backing-array index))
                 (setf index parent)
                 (setf parent (if (> index 1) (floor index 2) nil))))
      (setf (gethash element elements) index))))

(defmethod initialize-instance :after ((h heap) &key initial-contents)
  (dolist (val (coerce initial-contents 'list))
    (insert h val)))

(defmethod extract ((h heap))
  (with-slots (backing-array key elements) h
    (let ((return-value (aref backing-array 1)))
      (setf (aref backing-array 1) (aref backing-array (1- (length backing-array))))
      (decf (fill-pointer backing-array))

      (let ((index 1)) 
        (loop until (or (>= (* index 2) (length backing-array))
                        (and 
                         (< (funcall key (aref backing-array index))
                            (funcall key (aref backing-array (* index 2))))
                         (< (funcall key (aref backing-array index))
                            (funcall key (aref backing-array (1+ (* index 2)))))))
              do (let ((best-child (if (< (funcall key (aref backing-array (* index 2)))
                                          (funcall key (aref backing-array (1+ (* index 2)))))
                                       (* index 2)
                                       (1+ (* index 2)))))
                   (rotatef (aref backing-array index) (aref backing-array best-child))
                   (setf index best-child))))
      (remhash return-value elements)
      return-value)))

(defmethod peek ((h heap))
  (with-slots (backing-array) h
    (aref backing-array 1)))

(defmethod empty ((h heap))
  (with-slots (backing-array) h
    (= 1 (length backing-array))))

(defmethod check-heap ((h heap))
  (with-slots (backing-array key) h 
    (loop for i from 2 to (1- (length backing-array)) do
      (assert (<= (funcall key (aref backing-array (floor i 2))) (funcall key (aref backing-array i)))))))

(defmethod print-heap ((h heap))
  (format t "~&HEAPHEAPHEAP~%")
  (with-slots (backing-array elements) h
    (labels ((do-print (index tabs)
               (unless (>= index (length backing-array)) 
                 (loop repeat tabs do (format t "|~t"))
                 (format t "~A~%" (aref backing-array index))
                 (do-print (* 2 index) (1+ tabs))
                 (do-print (1+ (* 2 index)) (1+ tabs)))))
      (do-print 1 0)
      (format t "Elements: ")
      (loop for k being the hash-keys of elements using (hash-value v) do
        (format t "~A: ~A," k v))
      (format t "~%"))))

;; oops this isn't dynamic programming
(defun solve-problem-15 (board)
  (let ((dists (make-array (array-dimensions board)))
        (unvisited (make-hash-table :test #'equal))
        (n (array-dimension board 0))
        (m (array-dimension board 1)))
    
    (loop for i from 0 to (1- n) do
      (loop for j from 0 to (1- m) do
        (setf (aref dists i j) most-positive-long-float)
        (setf (gethash (list i j) unvisited) t)))
    (setf (aref dists 0 0) 0)
    
    (loop until (zerop (hash-table-count unvisited)) do
      (let ((min-coords nil)
            (min-dist most-positive-long-float))
        ;; Get minimum distance coords
        (loop for (i j) being the hash-keys of unvisited do
          (let ((alt-dist (aref dists i j)))
            (when (< alt-dist min-dist)
              (setf min-coords (list i j))
              (setf min-dist alt-dist))))
        (remhash min-coords unvisited)

        (destructuring-bind (i j) min-coords
          (let ((neighbors (remove-if-not
                            #'(lambda (coord) (gethash coord unvisited))
                            `((,(1- i) ,j) (,(1+ i) ,j) (,i ,(1- j)) (,i ,(1+ j))))))
            (loop for (ni nj) in neighbors do
              (let ((alternative (+ (aref dists i j) (aref board ni nj))))
                (when (< alternative (aref dists ni nj))
                  (setf (aref dists ni nj) alternative))))))))
    (aref dists (1- n) (1- m))))

(defun solve-problem-15-with-heap (board)
  (let* ((dists (make-array (array-dimensions board)))
         (unvisited (make-hash-table :test #'equal))
         (n (array-dimension board 0))
         (m (array-dimension board 1))
         (minheap (make-instance 'heap :key #'(lambda (l) (apply #'aref dists l)))))
    
    (loop for i from 0 to (1- n) do
      (loop for j from 0 to (1- m) do
        (setf (aref dists i j) most-positive-long-float)
        (setf (gethash (list i j) unvisited) t)))
    (setf (aref dists 0 0) 0)
    (insert minheap (list 0 0))
    
    (loop until (zerop (hash-table-count unvisited)) do
      (let ((min-coords (extract minheap)))
        (remhash min-coords unvisited)
        (destructuring-bind (i j) min-coords
          (let ((neighbors (remove-if-not
                            #'(lambda (coord) (gethash coord unvisited))
                            `((,(1- i) ,j) (,(1+ i) ,j) (,i ,(1- j)) (,i ,(1+ j))))))
            (loop for (ni nj) in neighbors do
              (let ((alternative (+ (aref dists i j) (aref board ni nj))))
                (when (< alternative (aref dists ni nj))
                  (setf (aref dists ni nj) alternative)
                  (insert minheap (list ni nj)))))))))
    (aref dists (1- n) (1- m))))


(let ((board (parse-input-15 (uiop:read-file-lines "./input/problem15.txt"))))
  (time (solve-problem-15-with-heap board)) ; 621
  (time (solve-problem-15-with-heap (create-input-15-b board))) ; 2904
  )


;; Problem 16
(defun parse-header (transmission starting-position)
  (let* ((end-of-version (- starting-position 3))
         (end-of-type (- end-of-version 3)))
    (list (ldb (byte 3 end-of-version) transmission)
          (ldb (byte 3 end-of-type) transmission)
          end-of-type)))


(defun parse-literal (transmission starting-position)
  (let ((literal 0)) 
    (loop for chunk-start = starting-position then (- chunk-start 5)
          for maybe-end = (ldb (byte 1 (- chunk-start 1)) transmission)
            then (ldb (byte 1 (- chunk-start 1)) transmission)
          for chunk-literal = (ldb (byte 4 (- chunk-start 5)) transmission)
            then (ldb (byte 4 (- chunk-start 5)) transmission)
          do
             (incf literal chunk-literal)
             (when (= maybe-end 1) 
               (setf literal (* literal 16)))
             (when (= maybe-end 0)
               (let ((next-start (- chunk-start 5)))
                 (return-from parse-literal (list literal next-start)))))))

(defun parse-operator (transmission starting-position)
  (let ((length-type-id (ldb (byte 1 (- starting-position 1)) transmission)))
    (if (= length-type-id 0)
        ;; Next 15 bits represent LENGTH IN BITS of subpackets
        (let* ((current-position (- starting-position 16))
               (total-subpacket-bits (ldb (byte 15 current-position) transmission))
               (ending-position (- current-position total-subpacket-bits)))
          ;(format t  "LENGTH TYPE 0: total-subpacket-bits ~A~%" total-subpacket-bits)
          (list (loop until (<= current-position ending-position)
                       collect (destructuring-bind (pkt next-start) 
                                   (parse-packet transmission current-position)
                                 (setf current-position next-start)
                                 pkt))
                current-position))
        ;; Next 11 bits represent NUMBER OF SUBPACKETS 
        (let* ((current-position (- starting-position 12))
               (num-subpackets (ldb (byte 11 current-position) transmission)))
          ;(format t "LENGTH TYPE 1: num-subpackets~A~%" num-subpackets)
          (list (loop repeat num-subpackets
                      collect (destructuring-bind (pkt next-start)
                                  (parse-packet transmission current-position)
                                (setf current-position next-start)
                                pkt))
                current-position)))))

(defclass packet ()
  ((packet-version :initarg :packet-version
                   :initform (error "Supply :packet-version")
                   :accessor packet-version)))


(defclass literal-packet (packet)
  ((packet-literal :initarg :packet-literal
            :initform (error "Supply :packet-literal")
                   :accessor packet-literal)))


(defmethod print-object ((l literal-packet) stream)
  (format stream "<VERSION: ~A, LITERAL ~A>" (packet-version l) (packet-literal l)))

(defmethod version-sum ((l literal-packet))
  (packet-version l))

(defmethod evaluate ((l literal-packet))
  (packet-literal l))

(defclass operator-packet (packet)
  ((packet-operation :initarg :packet-operation
                     :initform (error "Supply :packet-operation")
                     :accessor packet-operation)
   (subpackets :initarg :subpackets
               :initform (error "Supply :subpackets")
               :accessor subpackets)))

(defmethod print-object ((o operator-packet) stream)
  (format stream "<VERSION: ~A, OPERATION: ~A, SUBPACKETS: [~{~A~^, ~}]"
          (packet-version o) (packet-operation o) (subpackets o)))

(defmethod version-sum ((o operator-packet))
  (+ (packet-version o) (loop for pkt in (subpackets o) summing (version-sum pkt))))

(defmethod evaluate ((o operator-packet))
  (let ((op (case (packet-operation o)
              (0 #'+)
              (1 #'*)
              (2 #'min)
              (3 #'max)
              (5 #'(lambda (a b) (if (> a b) 1 0)))
              (6 #'(lambda (a b) (if (< a b) 1 0)))
              (7 #'(lambda (a b) (if (= a b) 1 0))))))
    (apply op (mapcar #'evaluate (subpackets o)))))

(defun parse-packet (transmission starting-position)
  (destructuring-bind (packet-version packet-type body-start)
      (parse-header transmission starting-position)
;    (format t "version: ~A, type: ~A~%" packet-version packet-type)
    (if (= packet-type 4)
        (destructuring-bind (packet-literal next-start)
             (parse-literal transmission body-start)
           (list (make-instance 'literal-packet
                                 :packet-version packet-version
                                 :packet-literal packet-literal)
                 next-start))
        (destructuring-bind (subpackets next-start)
            (parse-operator transmission body-start)
          (list (make-instance 'operator-packet
                               :packet-version packet-version
                               :packet-operation packet-type
                               :subpackets subpackets)
                next-start)))))

(defun get-start-position (transmission)
  (* (ceiling (integer-length transmission) 4) 4))

(let ((input-16 (parse-integer (uiop:read-file-string "./input/problem16.txt") :radix 16)))
  (destructuring-bind (pkt next-start) (parse-packet input-16 (get-start-position input-16))
    (version-sum pkt) ; 860
    (evaluate pkt))) ; 470949537659

;; Problem 17

(defun parse-problem-17 (input-str)
  (cl-ppcre:register-groups-bind (x1 x2 y1 y2)
      (".*x=(-?\\d+)\\.\\.(-?\\d+), y=(-?\\d+)\\.\\.(-?\\d+)" input-str)
    (mapcar #'parse-integer (list x1 x2 y1 y2))))


(defun triangle-number (n)
  (* n (1+ n) (/ 1 2)))

(defun calculate-min-vx (min-x max-x)
  (loop for i = 0 then (1+ i)
        for tri = (triangle-number i) then (triangle-number i)
        when (> tri min-x)
          do
             (return i)))

;; Part A
;; KEY IDEA: highest we can get is if we let vx drag to 0
;; Then we have as much time as we need (provided we have such a vx) to shoot up
;; The highest we can get is determined by how far the lower zone border is.
;; If we set vy to greater than (abs min-y) then we'll overshoot
;; The best we can get is vy = (1- (abs min-y))
(destructuring-bind (min-x max-x min-y max-y)
    (parse-problem-17 (uiop:read-file-string "./input/problem17.txt"))
  (let ((min-time-to-hit (calculate-min-vx min-x max-x)))
    (if (< min-time-to-hit (abs (1+ min-y)))
        (triangle-number (1- (- min-y)))
        (error "who knows"))))


;; Simulate a shot with vx vy, collecting what intersects with the zone
(defun check-if-valid (vx vy min-x max-x min-y max-y)
  (loop for time = 0 then (1+ time)
        for x = 0 then (+ x cur-vx)
        for y = 0 then (+ y cur-vy)
        for cur-vx = vx then (cond
                               ((> cur-vx 0) (1- cur-vx))
                               ((< cur-vx 0) (1+ cur-vx))
                               ((= cur-vx 0) 0))
        for cur-vy = vy then (1- cur-vy)
        until (< y min-y)
        when (and (<= min-x x max-x)
                  (<= min-y y max-y))
          collect (list vx vy x y)))

;; Boundary for vx is determined by minimum vx that hits when dragged
;; to 0 and vx = max-x (shoot directly at zone). Boudnary for vy is
;; determined by min-y (shoot directly at zone since it's below) and
;; (1- (abs min-y)), as determined by part A.
;; Then, we just simulate everything and count the hits.
(defun find-valid-starts (min-x max-x min-y max-y)
  (loop for vx from (calculate-min-vx min-x max-x) to max-x
        nconc (loop for vy from min-y to (1- (abs min-y))
                      when (check-if-valid vx vy min-x max-x min-y max-y)
                        collect (list vx vy))))

(destructuring-bind (min-x max-x min-y max-y)
    (parse-problem-17 (uiop:read-file-string "./input/problem17.txt"))
  (loop for start in (find-valid-starts min-x max-x min-y max-y) count start))

;; Problem 18

(defclass node ()
  ((left :initarg :left :initform nil :accessor left)
   (right :initarg :right :initform nil :accessor right)
   (parent :initarg :parent :initform nil :accessor parent)
   (direction :initarg :direction :initform nil :accessor direction)))

(defmethod print-object ((n node) stream)
  (with-slots (left right) n
    (format stream "[~A,~A]" left right)))

(defclass leaf (node)
  ((value :initarg :value :initform (error "Supply :value") :accessor value)))

(defmethod print-object ((l leaf) stream)
  (with-slots (value) l
    (format stream "~A" value)))

(defun tree-to-node (tree)
  (cond
    ((atom tree) (make-instance 'leaf :value tree))
    ((consp tree) (let ((parent (make-instance 'node))
                        (left (tree-to-node (car tree)))
                        (right (tree-to-node (cdr tree))))
                    (setf (parent left) parent)
                    (setf (parent right) parent)
                    (setf (left parent) left)
                    (setf (right parent) right)
                    (setf (direction left) 'left)
                    (setf (direction right) 'right)
                    parent))))

(defun make-readable (input)
  (cl-ppcre:regex-replace-all "," 
                              (cl-ppcre:regex-replace-all "\\]"
                                                          (cl-ppcre:regex-replace-all "\\[" input "(")
                                                          ")")
                              " . "))

(defun string-to-node (str)
  (tree-to-node (read-from-string (make-readable str))))

(defmethod rightmost ((l leaf))
  (value l))

(defmethod (setf rightmost) (value (l leaf))
  (setf (value l) value))

(defmethod leftmost ((l leaf))
  (value l))

(defmethod (setf leftmost) (value (l leaf))
  (setf (value l) value))

(defmethod rightmost ((n node))
  (rightmost (right n)))

(defmethod (setf rightmost) (value (n node))
  (setf (rightmost (right n)) value))

(defmethod leftmost ((n node))
  (leftmost (left n)))

(defmethod (setf leftmost) (value (n node))
  (setf (leftmost (left n)) value))

(defmethod left-neighbor ((n node))
  (if (null (parent n))
      nil
      (if (eq (direction n) 'right)
          (rightmost (left (parent n)))
          (left-neighbor (parent n)))))

(defmethod (setf left-neighbor) (value (n node))
  (if (null (parent n))
      nil
      (if (eq (direction n) 'right)
          (setf (rightmost (left (parent n))) value)
          (setf (left-neighbor (parent n)) value))))

(defmethod right-neighbor ((n node))
  (if (null (parent n))
      nil
      (if (eq (direction n) 'left)
          (leftmost (right (parent n)))
          (right-neighbor (parent n)))))

(defmethod (setf right-neighbor) (value (n node))
  (if (null (parent n))
      nil
      (if (eq (direction n) 'left)
          (setf (leftmost (right (parent n))) value)
          (setf (right-neighbor (parent n)) value))))

(defmethod find-first-level-n ((l leaf) level)
  nil)

(defmethod find-first-level-n ((n node) level)
  (if (zerop level)
      n
      (or (find-first-level-n (left n) (1- level))
          (find-first-level-n (right n) (1- level)))))

(defmethod explodable ((n node))
  (not (null (find-first-level-n n 4))))

(defmethod try-explode ((n node))
  (let ((explodable (find-first-level-n n 4)))
    ;(format t "EXPLODING ~A~%" explodable)
    (when explodable
      (when (left-neighbor explodable)
        (setf (left-neighbor explodable) (+ (left-neighbor explodable) (value (left explodable)))))
      (when (right-neighbor explodable)
        (setf (right-neighbor explodable) (+ (right-neighbor explodable) (value (right explodable)))))

      (let ((zero-leaf (make-instance 'leaf :value 0 :parent (parent explodable)
                                      :direction (direction explodable))))
        (if (eq (direction explodable) 'left)
            (setf (left (parent explodable)) zero-leaf)
            (setf (right (parent explodable)) zero-leaf)))))
  n)

(defmethod splittable ((l leaf))
  (> (value l) 9))

(defmethod splittable ((n node))
  (or (splittable (left n))
      (splittable (right n))))

(defmethod try-split ((l leaf))
  (let ((v (value l))) 
    (when (> v 9)
      ;(format t "SPLITTING ~A~%" v)
      (let* ((half (/ v 2))
             (new-left (floor half))
             (new-right (ceiling half)))
        (let ((new-node (tree-to-node (cons new-left new-right))))
          (setf (direction new-node) (direction l))
          (setf (parent new-node) (parent l))
          (if (eq (direction l) 'left)
              (setf (left (parent l)) new-node)
              (setf (right (parent l)) new-node))))
      t)))

(defmethod try-split ((n node))
  (or (try-split (left n))
      (try-split (right n))))


(defmethod snail-reduce-1 ((n node))
  (loop while (explodable n) do (try-explode n))
  (when (splittable n) (try-split n))
  n)


(defmethod snail-reduce ((n node) &key (debug nil))
  (loop while (or (explodable n) (splittable n)) do
    (loop while (explodable n) do
      (when debug
        (format t "================EXPLODE================~%")
        (format t "~A~%" n))
      (try-explode n)
      (when debug
        (format t "~A~%" n)))
    (when (splittable n)
      (when debug
        (format t "================SPLIT================~%")
        (format t "~A~%" n))
      (try-split n)
      (when debug
        (format t "~A~%" n))))
  n)

(defmethod join-snails ((n node) (m node))
  (let ((p (make-instance 'node :left n :right m)))
    (setf (parent n) p)
    (setf (direction n) 'left)
    (setf (parent m) p)
    (setf (direction m) 'right)
    p))

(defmethod add-snails ((n node) (m node))
  (let ((p (join-snails n m)))
    (snail-reduce p :debug nil)
    p))

(defun snailsum-list (node-list)
  (let ((sum (first node-list)))
    (loop for n in (cdr node-list) do
      (setf sum (add-snails sum n)))
    sum))

(defmethod magnitude ((l leaf))
  (value l))

(defmethod magnitude ((n node))
  (+ (* 3 (magnitude (left n)))
     (* 2 (magnitude (right n)))))


(defmethod copy-node ((l leaf))
  (make-instance 'leaf
                 :value (value l)
                 :direction (direction l)))

(defmethod copy-node ((n node))
  (let ((c (make-instance 'node
                         :left (copy-node (left n))
                         :right (copy-node (right n))
                         :direction (direction n))))
    (setf (parent (left c)) c)
    (setf (parent (right c)) c)))

(let ((l (mapcar #'string-to-node (uiop:read-file-lines "./input/problem18.txt"))))
  (magnitude (snailsum-list l))) ;4184

(let ((l (mapcar #'string-to-node (uiop:read-file-lines "./input/problem18.txt")))) 
  (loop for i from 0 to (1- (length l))
        maximize (loop for j from 0 to (1- (length l))  
                       when (/= i j)
                         maximize
                       (magnitude (add-snails (copy-node (elt l i)) (copy-node (elt l j))))))) ; 4731


;; Problem 19


;; Problem 20

(defun parse-input-20 (input-str)
  (destructuring-bind (io-map-str initial-board-str) (cl-ppcre:split "\\n\\n" input-str)
    (let ((initial-board-list (cl-ppcre:split "\\n" initial-board-str))
          (io-map 0))
      (loop for i from 0 to 511 do
        (setf (ldb (byte 1 i) io-map)
              (if (char= (char io-map-str i) #\#)
                  1
                  0)))
      
      (let ((initial-board (make-array (list (length initial-board-list)
                                             (length (first initial-board-list)))
                                       :initial-contents initial-board-list)))

        (list io-map initial-board)))))

(defparameter *board-growth* 6) ; evens only

(defun enhance-once (io-map board fill-char)
  (destructuring-bind (n m) (array-dimensions board)
    (let* ((new-n (+ *board-growth* n))
           (new-m (+ *board-growth* m))
           (this-board (make-array (list new-n new-m) :initial-element fill-char))
           (next-board (make-array (list new-n new-m) :initial-element fill-char)))

      (loop for i from 0 to (1- n) do
        (loop for j from 0 to (1- m) do
          (setf (aref this-board (+ i (/ *board-growth* 2)) (+ j (/ *board-growth* 2))) (aref board i j))))
      (labels ((get-next-char (i j)
                 (let ((ix 0)) 
                   (loop for y from (1- i) to (1+ i) do
                     (loop for x from (1- j) to (1+ j) do
                       (if (or (< y 0)
                               (< x 0)
                               (>= y new-n)
                               (>= x new-n))
                           (when (char= fill-char #\#) ;; out of bounds 
                             (incf ix))
                           (when (char= (aref this-board y x) #\#) ;; in bounds
                             (incf ix)))

                       (when (not (equal (list y x) (list (1+ i) (1+ j))))
                         (setf ix (ash ix 1)))))

                   (if (zerop (ldb (byte 1 ix) io-map))
                       #\.
                       #\#))))
        (loop for i from 0 to (1- new-n) do
          (loop for j from 0 to (1- new-m) do
            (setf (aref next-board i j) (get-next-char i j)))))
      next-board)))



(defun enhance-n (io-map board n)
  (let ((cur-board board))
    (loop for i from 1 to n  do
      (setf cur-board (enhance-once io-map cur-board (if (evenp i) #\# #\.))))
    cur-board))

(first  (parse-input-20 (uiop:read-file-string "./input/problem20.txt")))


(print-board (destructuring-bind (io-map board) (parse-input-20 
                                                 "..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#. .#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#..... .#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.. ...####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#..... ..##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#

#..#.
#....
##..#
..#..
..###")
               (enhance-n io-map board 2)))

(destructuring-bind (io-map board) (parse-input-20 (uiop:read-file-string "./input/problem20.txt"))
  (let ((new-board (enhance-n io-map board 2)))
    (loop for i from 0 to (1- (array-dimension new-board 0))
          summing (loop for j from 0 to (1- (array-dimension new-board 1))
                        counting (char= #\# (aref new-board i j))))) ; 5179
  (let ((new-board (enhance-n io-map board 50)))
    (loop for i from 0 to (1- (array-dimension new-board 0))
          summing (loop for j from 0 to (1- (array-dimension new-board 1))
                        counting (char= #\# (aref new-board i j)))))) ; 16112





;; Problem 21

(defun run-game (input-lines)
  ;; Zero index first
  (let ((p1-pos (1- (parse-integer (cl-ppcre:scan-to-strings "\\d+$" (first input-lines)))))
        (p2-pos (1- (parse-integer (cl-ppcre:scan-to-strings "\\d+$" (second input-lines)))))
        (p1-score 0)
        (p2-score 0)
        (turn 'p1)
        (board (make-array 10 :initial-contents '(1 2 3 4 5 6 7 8 9 10)))
        (die-counter 0)
        (rolls 0))
    (labels ((roll-die ()
               (let ((sum 0))
                 (loop repeat 3 do
                   (incf rolls)
                   (incf die-counter)
                   (when (> die-counter 100)
                     (setf die-counter (- die-counter 100)))
                   (setf sum (+ sum die-counter)))
                 sum)))
      ;; (format t "p1-pos: ~A p2-pos: ~A p1-score: ~A p2-score: ~A turn: ~A die-counter: ~A~%"
      ;;         (1+  p1-pos) (1+ p2-pos) p1-score p2-score turn die-counter)
      
      (loop until (or (>= p1-score 1000)
                      (>= p2-score 1000))
            do (if (eq turn 'p1)
                   (progn
                     (setf p1-pos (mod (+ p1-pos (roll-die)) 10))
                     (setf p1-score (+ p1-score (aref board p1-pos)))
                     (setf turn 'p2))
                   (progn
                     (setf p2-pos (mod (+ p2-pos (roll-die)) 10))
                     (setf p2-score (+ p2-score (aref board p2-pos)))
                     (setf turn 'p1)))
               ;; (format t "p1-pos: ~A p2-pos: ~A p1-score: ~A p2-score: ~A turn: ~A die-counter: ~A~%"
               ;;         (1+ p1-pos) (1+  p2-pos) p1-score p2-score turn die-counter)
            )

      (if (>= p1-score 1000)
          (* p2-score rolls)
          (* p1-score rolls)))))



(defun run-dirac-game (input-lines)
  (let ((p1-wins 0)
        (p2-wins 0)
        (p1-pos (1- (parse-integer (cl-ppcre:scan-to-strings "\\d+$" (first input-lines)))))
        (p2-pos (1- (parse-integer (cl-ppcre:scan-to-strings "\\d+$" (second input-lines)))))
        (step-multipliers '((3 1)
                            (4 3)
                            (5 6)
                            (6 7)
                            (7 6)
                            (8 3)
                            (9 1))))
    (labels ((run-turn (p1-score p2-score p1-pos p2-pos p1-turn? multiplier)
               ;; (format t "~A ~A ~A ~A ~A ~A~%"
               ;;         p1-score p2-score (1+ p1-pos) (1+ p2-pos) p1-turn? multiplier)
               (cond
                 ((>= p1-score 21) (incf p1-wins multiplier))
                 ((>= p2-score 21) (incf p2-wins multiplier))
                 (p1-turn?
                  (loop for (step m) in step-multipliers do
                    (let* ((next-pos (mod (+ p1-pos step) 10))
                           (next-score (1+ next-pos)))
                      (run-turn (+ p1-score next-score) p2-score next-pos p2-pos nil (* multiplier m)))))
                 (t (loop for (step m) in step-multipliers do
                    (let* ((next-pos (mod (+ p2-pos step) 10))
                           (next-score (1+ next-pos)))
                      (run-turn p1-score (+ p2-score next-score) p1-pos next-pos t (* multiplier m))))))))
      
      (run-turn 0 0 p1-pos p2-pos t 1)
      (list  p1-wins p2-wins))))

(run-game (uiop:read-file-lines "./input/problem21.txt")) ; 797160
(run-dirac-game (uiop:read-file-lines "./input/problem21.txt")) ; (27464148626406 22909380722959)


;; Problem 22

(defclass point ()
  ((x :accessor x :initarg :x :type number)
   (y :accessor y :initarg :y :type number)
   (z :accessor z :initarg :z :type number)))

(defmethod print-object ((p point) stream)
  (with-slots (x y z) p
    (format stream "(x: ~A y: ~A z: ~A)" x y z)))


(defun list->point (l)
  (make-instance 'point :x (first l) :y (second l) :z (third l)))


(defclass cube ()
  ((p1 :accessor p1 :initarg :p1 :type point)
   (p2 :accessor p2 :initarg :p2 :type point)
   (on :accessor on :initarg :on)))

(defmethod bounds ((c cube))
  (with-slots (p1 p2) c
    (with-slots ((x1 x) (y1 y) (z1 z)) p1
      (with-slots ((x2 x) (y2 y) (z2 z)) p2
        (list (min x1 x2) (max x1 x2)
              (min y1 y2) (max y1 y2)
              (min z1 z2) (max z1 z2))))))

(defmethod print-object ((c cube) stream)
  (with-slots (on p1 p2) c
    (format stream "[~A ~A ~A]" on p1 p2)))

(defmethod volume ((c cube))
  (with-slots (p1 p2) c
    (let ((side-length (abs (- (x p2) (x p1)))))
      (expt side-length 3))))

(defmethod point-in-cube ((p point) (c cube))
  (destructuring-bind (minx maxx miny maxy minz maxz) (bounds c)
    (and (<= minx (x p) maxx)
         (<= miny (y p) maxy)
         (<= minz (z p) maxz))))

(defun bounding-box (list-of-cubes)
  (let ((bb-minx most-positive-fixnum)
        (bb-maxx most-negative-fixnum)
        (bb-miny most-positive-fixnum)
        (bb-maxy most-negative-fixnum)
        (bb-minz most-positive-fixnum)
        (bb-maxz most-negative-fixnum))
    (loop for c in list-of-cubes do
      (destructuring-bind (minx maxx miny maxy minz maxz) (bounds c)
        (when (< minx bb-minx)
          (setf bb-minx minx))
        (when (< miny bb-miny)
          (setf bb-miny miny))
        (when (< minz bb-minz)
          (setf bb-minz minz))
        (when (> maxx bb-maxx)
          (setf bb-maxx maxx))
        (when (> maxy bb-maxy)
          (setf bb-maxy maxy))
        (when (> maxz bb-maxz)
          (setf bb-maxz maxz))))
    (make-instance 'cube
                   :on 'off
                   :p1 (list->point (list bb-minx bb-miny bb-minz))
                   :p2 (list->point (list bb-maxx bb-maxy bb-maxz)))))

(defun parse-input-22 (input-str)
  (let ((rval nil)) 
    (cl-ppcre:do-register-groups (onoff x1 x2 y1 y2 z1 z2)
        ("(on|off) x=(-?\\d+)\\.\\.(-?\\d+),y=(-?\\d+)\\.\\.(-?\\d+),z=(-?\\d+)\\.\\.(-?\\d+)" input-str)
      (let ((p1 (mapcar #'parse-integer (list x1 y1 z1)))
            (p2 (mapcar #'parse-integer (list x2 y2 z2)))
            (onoff-s (intern (string-upcase onoff))))
        (push (make-instance 'cube :on onoff-s :p1 (list->point p1) :p2 (list->point p2)) rval)))
    rval))

(defun count-points-in-bbox (cube-list bbox)
  (let ((on-count 0)
        (new-cube-list (append cube-list (list bbox))))

    (destructuring-bind (minx maxx miny maxy minz maxz) (bounds bbox)
      (loop for x from minx to maxx do
        (loop for y from miny to maxy do
          (loop for z from minz to maxz do
            (let ((p (list->point (list x y z))))
              (loop for c in new-cube-list do
                (when (point-in-cube p c)
                  (when (eq (on c) 'on)
                    (incf on-count))
                  (return))))))))
    on-count))
  
  (let ((bbox (make-instance 'cube :on 'off
                                   :p1 (list->point '(-50 -50 -50))
                                   :p2 (list->point '(50 50 50)))))
    (count-points-in-bbox l bbox)))

(defmethod cube-intersects-p ((c1 cube) (c2 cube))
  (destructuring-bind (minx1 maxx1 miny1 maxy1 minz1 maxz1) (bounds c1)
    (destructuring-bind (minx2 maxx2 miny2 maxy2 minz2 maxz2) (bounds c2)
      (not (or 
            (or (< maxx1 minx2) (< maxx2 minx1))
            (or (< maxy1 miny2) (< maxy2 miny1))
            (or (< maxz1 minz2) (< maxz2 minz1)))))))

(let* ((l (parse-input-22 (uiop:read-file-string "./input/problem22.txt")))
       (bbox (make-instance 'cube :on 'off
                                  :p1 (list->point '(-50 -50 -50))
                                  :p2 (list->point '(50 50 50))))
       (cubes-we-care-about (loop for c in l when (cube-intersects-p c bbox) collect c)))
  (count-points-in-bbox cubes-we-care-about bbox)) ; 596989


;; Algorithm taken from:
;; https://www.reddit.com/r/adventofcode/comments/rlxhmg/2021_day_22_solutions/hpizza8/
(defun problem-22-part-2 (input-str)
  
  (let ((signed-volumes (make-hash-table :test #'equal))) 
    (cl-ppcre:do-register-groups (on? minx maxx miny maxy minz maxz)
        ("(on|off) x=(-?\\d+)\\.\\.(-?\\d+),y=(-?\\d+)\\.\\.(-?\\d+),z=(-?\\d+)\\.\\.(-?\\d+)" input-str)

      (destructuring-bind (minx maxx miny maxy minz maxz)
          (mapcar #'parse-integer (list minx maxx miny maxy minz maxz))
        (let ((update-volumes (make-hash-table :test #'equal)))
          (loop for (eminx emaxx eminy emaxy eminz emaxz) being the hash-keys of signed-volumes
                  using (hash-value sign) do
                    (let ((iminx (max minx eminx))
                          (imaxx (min maxx emaxx))
                          (iminy (max miny eminy))
                          (imaxy (min maxy emaxy))
                          (iminz (max minz eminz))
                          (imaxz (min maxz emaxz)))
                      (when (and (<= iminx imaxx) (<= iminy imaxy) (<= iminz imaxz))
                        (let* ((prism (list iminx imaxx iminy imaxy iminz imaxz))
                               (old-value (alexandria:ensure-gethash prism update-volumes 0)))
                          (setf (gethash prism update-volumes) (- old-value sign))))))

          (when (string= on? "on")
            (let* ((prism (list minx maxx miny maxy minz maxz))
                   (old-value (alexandria:ensure-gethash prism update-volumes 0)))
              (setf (gethash prism update-volumes) (1+ old-value))))
          
          (loop for k being the hash-keys of update-volumes using (hash-value v) do
            (let ((old-value (alexandria:ensure-gethash k signed-volumes 0)))
              (setf (gethash k signed-volumes) (+ old-value v)))))))

    ;(maphash #'(lambda (k v) (format t "~A: ~A~%" k v)) signed-volumes)
    (loop for (minx maxx miny maxy minz maxz) being the hash-keys of signed-volumes
            using (hash-value sign) summing
          (* (- maxx minx -1) (- maxy miny -1) (- maxz minz -1) sign))))



(problem-22-part-2 (uiop:read-file-string "./input/problem22.txt")) ; 1160011199157381



