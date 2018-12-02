(defpackage :macros
  (:use :cl)
  (:export :test-all))

(defpackage :common
  (:use :cl :macros))

(defpackage :day1
  (:use :cl :macros :common))

(defpackage :day2
  (:use :cl :macros :common))


(defpackage :day3
  (:use :cl :macros :common))

