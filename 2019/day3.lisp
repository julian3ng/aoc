(defpackage :day3
  (:use :common-lisp)
  (:import-from :uiop/utility
                :split-string))

(in-package :day3)

(defun seg-xs (seg)
  (sort (list (car (first seg)) (car (second seg))) #'<))

(defun seg-ys (seg)
  (sort (list (cdr (first seg)) (cdr (second seg))) #'<))

(defun path-points (line)
  (let ((points (list (cons 0 0))))
    (loop for instruction in line do
         (let ((direction (aref instruction 0))
               (distance (parse-integer (subseq instruction 1)))
               (cur (first points)))
           (cond
             ((char= direction #\R) (push (cons (+ (car cur) distance) (cdr cur)) points))
             ((char= direction #\L) (push (cons (- (car cur) distance) (cdr cur)) points))
             ((char= direction #\U) (push (cons (car cur) (+ (cdr cur) distance)) points))
             ((char= direction #\D) (push (cons (car cur) (- (cdr cur) distance)) points)))))
    (nreverse points)))

(defun manhattan-distance (p1 p2)
  (+ (abs (- (car p1) (car p2))) (abs (- (cdr p1) (cdr p2)))))

(defun distance-segment-point (seg point)
  (let ((xs (seg-xs seg))
        (ys (seg-ys seg)))
    (when (and (<= (first xs) (car point) (second xs))
               (<= (first ys) (cdr point) (second ys)))
      (manhattan-distance (first seg) point))))

(defun intersect-segments (seg1 seg2)
  (let ((seg1-xs (seg-xs seg1))
        (seg1-ys (seg-ys seg1))
        (seg2-xs (seg-xs seg2))
        (seg2-ys (seg-ys seg2)))
    (cond
      ;; both horizontal
      ((and (apply #'= seg1-ys) (apply #'= seg2-ys)) nil)
      ;; both vertical
      ((and (apply #'= seg1-xs) (apply #'= seg2-xs))  nil)
      ;; first horizontal, second vertical
      ((and (apply #'= seg1-ys) (apply #'= seg2-xs))
       (when (and
              (<= (first seg1-xs) (first seg2-xs) (second seg1-xs))
              (<= (first seg2-ys) (first seg1-ys) (second seg2-ys)))
         (cons (first seg2-xs) (first seg1-ys))))
      ;; first vertical, second horizontal
      ((and (apply #'= seg1-xs) (apply #'= seg2-ys))
       (when (and
              (<= (first seg1-ys) (first seg2-ys) (second seg1-ys))
              (<= (first seg2-xs) (first seg1-xs) (second seg2-xs)))
         (cons (first seg1-xs) (first seg2-ys)))))))

(defun calculate-distance (path point)
  (let ((dist 0)) 
    (loop for seg in path
       do
         (let ((last-dist (distance-segment-point seg point)))
           (if (null last-dist)
               (setf dist (+ dist (distance-segment-point seg (second seg))))
               (return (+ dist last-dist)))))))

(with-open-file (stream "./input/day3.txt")
  (let ((line1 (read-line stream nil))
        (line2 (read-line stream nil)))
    (let ((ins1 (split-string line1 :separator '(#\,)))
          (ins2 (split-string line2 :separator '(#\,))))
      (let ((path1 (path-points ins1))
            (path2 (path-points ins2)))
        (let ((seg1s (loop for (p1 p2) on path1 while p2 collect (list p1 p2)))
              (seg2s (loop for (p1 p2) on path2 while p2 collect (list p1 p2))))
          (let ((intersections (remove-if #'null
                                          (loop for seg1 in seg1s nconc
                                               (loop for seg2 in seg2s collect
                                                    (intersect-segments seg1 seg2))))))
            (let ((soln1 (sort (mapcar (lambda (point) (+ (abs (car point)) (abs (cdr point)))) intersections) #'<)))

              (sort (mapcar (lambda (intersection)
                              (+ (calculate-distance seg1s intersection)
                                 (calculate-distance seg2s intersection)))
                            intersections)
                    #'<)
              )))))))



