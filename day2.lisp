(in-package :day2)

(defmethod check-double-triple ((s string))
  (let ((double 0)
        (triple 0))
    (loop for c across s do
         (when (= 2 (count c s))
           (setf double 1))
         (when (= 3 (count c s))
           (setf triple 1)))
    (list double triple)))

(test-all (equal (check-double-triple "abcdef") '(0 0))
          (equal (check-double-triple "bababc") '(1 1))
          (equal (check-double-triple "abbcde") '(1 0))
          (equal (check-double-triple "abcccd") '(0 1))
          (equal (check-double-triple "aabcdd") '(1 0))
          (equal (check-double-triple "abcdee") '(1 0))
          (equal (check-double-triple "ababab") '(0 1))
          (equal (check-double-triple "aaaaaa") '(0 0)))

(defmethod sol1 (filename)
  (with-open-file (f filename)
    (let ((strings (loop for line = (read-line f nil)
                      while line collecting line))
          (doubles 0)
          (triples 0))
      (mapcar (lambda (p)
                (setf doubles (+ doubles (first p)))
                (setf triples (+ triples (second p))))
              (mapcar #'check-double-triple strings))
      (* doubles triples))))


(defmethod char-diff ((c1 character) (c2 character))
  (- (char-int c1) (char-int c2)))

(defmethod string-diff ((s1 string) (s2 string))
  (assert (= (length s1) (length s2)))
  (map 'vector #'char-diff s1 s2))

(defun nonzeros (v)
  (count 0 v :test #'/=))

(defmethod sol2 (filename)
  (with-open-file (f filename)
    (let ((strings (loop for line = (read-line f nil)
                      while line collecting line)))
      (let ((proto-fabrics (mapcon (lambda (l)
                                     (let ((target (first l))
                                           (others (rest l)))
                                       (mapcan (lambda (other)
                                                 (if (= 1 (nonzeros (string-diff target other)))
                                                     (list target other))) others)))
                                   strings)))
        (let ((common-chars 
               (remove-if #'null
                          (map 'list (lambda (c1 c2) (if (char-equal c1 c2) c1))
                               (first proto-fabrics) (second proto-fabrics)))))
          (coerce (make-array (length common-chars) :initial-contents common-chars) 'string))))))


(time (sol1 "input/day2.txt"))
(time (sol2 "input/day2.txt"))
