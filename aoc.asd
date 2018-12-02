(defpackage :aoc-asd
  (:use :cl :asdf))

(in-package :aoc-asd)

(defsystem aoc
  :name "aoc2018"
  :version "0.0.0"
  :author "Julian Eng"
  :description "Advent of Code 2018"
  :components ((:file "packages")
               (:file "macros")
               (:file "common")
               (:file "day1")
               (:file "day2")
               (:file "day3"))) 
