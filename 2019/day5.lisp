(defpackage :day5
  (:use :common-lisp
        :intcode
        :utils))

(in-package :day5)

(utils:do-lines (line "input/day5.txt")
  (intcode:eval-program-string line))
