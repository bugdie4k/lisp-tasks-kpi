;;;; lab 9 for lisp course at KPI

(in-package :labs)

(named-readtables:in-readtable cmu-infix:syntax)

(defun calculate (w)
  (let* ((r1 (complex 10 0)) (r2 (complex 200 0)) (xl1 (complex 0 #i(w * 100))) (xl2 (complex 0 #i(w * 10)))
         (z1 #i(r1 * xl1 / (r1 + xl1))) (z2 #i(r2 * xl2 / (r2 + xl2)))
         (z #i(z1 + z2)))
    (* z 1.0)))

;; tests

(deftestlab test-lab9 ()
  (w0.01 (calculate 0.01) #C(0.099059895 1.0900989))
  (w0.1 (calculate 0.1) #C(5.0049996 5.999975))
  (w10 (calculate 10) #C(49.999 80.09999))
  (w100 (calculate 100) #C(202.30768 38.47154))) 
