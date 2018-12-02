(in-package :day1)

(defun sol1 (filename)
  (with-open-file (f filename)
    (loop for num = (read f nil)
       while num summing num)))

(defun sol2 (filename)
  (let ((nums (with-open-file (f filename)
                 (loop for num = (read f nil)
                    while num collecting num)))
        (freq 0)
        (seen (make-hash-table)))
    (loop named outer-loop do
         (loop for num in nums do
              (setf freq (+ freq num))
              (if (nth-value 1 (gethash freq seen))
                  (return-from outer-loop freq)
                  (setf (gethash freq seen) 1))))))

(sol1 "input/day1.txt")
(sol2 "input/day1.txt")



