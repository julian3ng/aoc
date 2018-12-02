(in-package :macros)


(defmacro test (expr)
  `(format t "~:[FAIL~;PASS~]: ~S~%" ,expr ',expr))


(defmacro test-all (&body body)
  (cons 'progn  (loop for expr in body collect `(test ,expr))))

 
