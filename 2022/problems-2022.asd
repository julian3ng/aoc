(defsystem :problems-2022
  :version "0.0.1"
  :description "AOC 2022"
  :components ((:file "utils")
               (:file "problem-1" :depends-on ("utils"))
               (:file "problem-2" :depends-on ("utils"))
               (:file "problem-3" :depends-on ("utils"))
               (:file "problem-4" :depends-on ("utils")))
  :depends-on ("cl-ppcre" "alexandria" "split-sequence"))


;; HOW TO USE THIS:
;; (asdf:load-asd "/home/julian/common-lisp/aoc/2022/problems-2022.asd")
;; (asdf:make :problems-2022)
;;
;; No, I don't really have a clue why this tries to clash with 2021 problems if
;; I name it "problems"...
