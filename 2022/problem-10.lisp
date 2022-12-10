(defpackage :problem-10
  (:use :cl :cl-ppcre :alexandria :utils))

(in-package :problem-10)

(with-file-lines (lines "./input/10.txt")
  (let ((part1 (let ((x 1)
                     (cycles 0)
                     (signal-strength (make-hash-table)))
                 (labels ((inc-cycles (num)
                            (loop repeat num do
                              (incf cycles)
                              (when (zerop (mod cycles 20))
                                (setf (gethash cycles signal-strength) (* x cycles)))))) 
                   (loop for line in lines do
                     (let ((command (subseq line 0 4)))
                       (cond
                         ((equal command "noop") (inc-cycles 1))
                         ((equal command "addx")
                          (let ((addend (parse-integer (subseq line 5))))
                            (inc-cycles 2)
                            (incf x addend))))))
                   (apply #'+ (mapcar (lambda (x) (gethash x signal-strength)) '(20 60 100 140 180 220))))))
        (part2 (with-file-lines (lines "./input/10.txt")
                 (with-output-to-string (s) 
                   (let ((x 1)
                         (cycles 0)
                         (signal-strength (make-hash-table)))
                     (labels ((inc-cycles (num)
                                (loop repeat num do
                                  (if (<= (1- x) (mod cycles 40) (1+ x))
                                      (format s "#")
                                      (format s "."))

                                  (incf cycles)
                                  
                                  (when (zerop (mod cycles 20))
                                    (setf (gethash cycles signal-strength) (* x cycles)))
                                  (when (zerop (mod cycles 40))
                                    (format s "~%"))
                                      ))) 
                       (loop for line in lines do
                         (let ((command (subseq line 0 4)))
                           (cond
                             ((equal command "noop") (inc-cycles 1))
                             ((equal command "addx")
                              (let ((addend (parse-integer (subseq line 5))))
                                (inc-cycles 2)
                                (incf x addend))))))
                       ))
                   s)))
        )

    (print-parts 10 part1 (format nil "~%~A" part2))))

