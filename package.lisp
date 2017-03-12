(defpackage #:labs
  (:use #:cl
        #:utils
        #:my-tests)
  (:export #:test-lab3
           #:test-lab4
           #:test-lab5
           #:test-lab6
           #:test-lab7
           #:test-lab8
           #:test-lab9
           #:test-all))

(in-package :labs)

(defun test-all ()
  (test-lab3)
  (test-lab4)
  (test-lab5)
  (test-lab6)
  (test-lab7)
  (test-lab8)
  (test-lab9))
