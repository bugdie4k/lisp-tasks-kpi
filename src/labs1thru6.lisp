;;;; labs 3 thru 6 for lisp course at KPI

(in-package #:labs)

;; lab3

(defun lab3-task-11 (lst)
  `(,(caddr lst)
    ,(car lst)
    ,(cadr lst)
    ,(car (reverse lst))))

(deftestlab test-lab3 ()
  (1 (cons 1 (cons 'a (cons 2 (cons 'b nil)))) '(1 a 2 b))
  (2 (list 1 'a 2 'b) '(1 a 2 b))
  (3 (car '(a b c)) 'a)
  (4 (cdr '(a b c)) '(b c))
  (5 (car (cdr (cdr '(a b c d e)))) 'c)
  (6 (caddr '(a b c d e)) 'c)
  (7.1 (eq 'x 'x) t)
  (7.2 (eq '(a b) '(a b)) t)
  (7.3 (eql 3 3) t)
  (7.4 (eql 2.0 2) nil)
  (7.5 (equal 'x 'x) t)
  (7.6 (equal '(a b c) '(a b c)) t)
  (8 (null '()) t)
  (9.1 (atom 'x) t)
  (9.2 (atom nil) t)
  (10 (car (cdr (cons 'a '(b c)))) 'b)
  (11 (lab3-task-11 '(a b c d))
      '(c a b d)))

;; lab4

(defun lab4-task-1 (lst)
  (aif (cdr lst)
       (cons (lab4-task-1 it) (cons (car lst) nil))
       (list (car lst))))

(defun lab4-task-2 (lst)
  (labels ((%lab4-task-2-aux (acc lst)
             (if lst
                 (%lab4-task-2-aux (if acc (append `(,acc) `(,(car lst))) `(,(car lst))) (cdr lst))
                 acc)))
    (%lab4-task-2-aux nil lst)))

(defun lab4-task-34 (lst &key do-task)
  (labels ((%lab4-task-34-aux (acc to-collect? lst)
             (if lst
                 (%lab4-task-34-aux (if to-collect? (append acc `(,(car lst))) acc) (not to-collect?) (cdr lst))
                 acc)))
    (%lab4-task-34-aux nil (equal do-task 3) lst)))

(defun lab4-task-5 (lst)
  (labels ((%lab4-task-5-aux (acc count lst)
             (if lst
                 (%lab4-task-5-aux (if (equal count 3) (append acc `(,(car lst))) acc)
                                   (if (equal count 3) 1 (1+ count))
                                   (cdr lst))
                 acc)))
    (%lab4-task-5-aux nil 3 lst)))

;; how a sane person would do this
(defun lab4-task-20.0 (lst)
  (let ((half-length (/ (list-length lst) 2)))
    (reverse (map 'list #'list (nthcdr half-length (reverse lst)) (nthcdr half-length lst)))))

;; how i'm probably supposed to do it 
(defun lab4-task-20.1 (lst)
  (let ((half-length (/ (list-length lst) 2)))
    (labels ((%my-reverse (lst)
               (when lst
                 (append (%my-reverse (cdr lst)) `(,(car lst)))))
             (%lab4-task-20.1-aux (acc lst rev-lst count)
               (if (<= count half-length)
                   (%lab4-task-20.1-aux (append acc `((,(car lst) ,(car rev-lst))))
                                        (cdr lst) (cdr rev-lst) (1+ count))
                   acc)))
      (%lab4-task-20.1-aux nil lst (%my-reverse lst) 1))))


(deftestlab test-lab4 ()
  (1 (lab4-task-1 '(a b c))
       '(((c) b) a))
  (2 (lab4-task-2 '(a b c))
     '(((a) b) c))
  (3 (lab4-task-34 '(a b c d e f g) :do-task 3)
     '(a c e g))
  (4 (lab4-task-34 '(a b c d e f g) :do-task 4)
     '(b d f))
  (5 (lab4-task-5 '(a b c d e f g))
     '(a d g))
  (20.0 (lab4-task-20.0 '(a b c d e f g h))
        '((a h) (b g) (c f) (d e)))
  (20.1 (lab4-task-20.1 '(a b c d e f g h))
        '((a h) (b g) (c f) (d e))))

;; lab 5 - shitft a list by certain number of elements

(defun lab5-task-20 (lst shift-n &key shift-left?)
  (labels ((%lab5-task-20-right (acc count lst)
             (if (> count shift-n)
                 (%lab5-task-20-right (append acc `(,(car lst)))
                                (1- count)
                                (cdr lst))
                 (append lst acc)))
           (%lab5-task-20-left (acc count lst)
             (if (< count shift-n)
                 (%lab5-task-20-left (append acc `(,(car lst)))
                                     (1+ count)
                                     (cdr lst))
                 (append lst acc))))
    (if shift-left?
        (%lab5-task-20-left nil 0 lst)
        (%lab5-task-20-right nil (list-length lst) lst))))

(deftestlab test-lab5 ((lst-in-sh-2 '(a b c d e f g h))
                       (lst-out-sh-2-right '(g h a b c d e f)) (lst-out-sh-2-left '(c d e f g h a b))
                       (lst-in-sh-6 '(a b c d e f g h i j k))
                       (lst-out-sh-6-right '(f g h i j k a b c d e)) (lst-out-sh-6-left '(g h i j k a b c d e f)))
  (20-right-1 (lab5-task-20 lst-in-sh-2 2) lst-out-sh-2-right)
  (20-right-2 (lab5-task-20 lst-in-sh-6 6) lst-out-sh-6-right)
  (20-left-1 (lab5-task-20 lst-in-sh-2 2 :shift-left? t) lst-out-sh-2-left)
  (20-left-2 (lab5-task-20 lst-in-sh-6 6 :shift-left? t) lst-out-sh-6-left))


;; lab 6 - find all subsets of a set represented with lst

(defun lab6-task-20v1 (lst)  
  (when lst
    (let ((prev-lst (lab6-task-20v1 (cdr lst))))
      (append prev-lst (aif prev-lst
                            (mapcar (lambda (el) (append `(,(car lst)) el)) (copy-tree it))
                            (cons nil (cons `(,(car lst)) nil)))))))

(defun lab6-task-20v2 (lst)
  (aif (cdr lst)
       (let ((prev-lst (lab6-task-20v2 it)))
         (mapcar (lambda (el) (append `(,(car lst)) el)) (copy-tree prev-lst)))
       (append '(nil) `(,(car lst)))))

(defun set-of-sets-eq (lst1 lst2)
  (null (set-difference lst1 lst2
                        :test (lambda (el1 el2)
                                (if (and (listp el1) (listp el2))
                                    (null (set-difference el1 el1))
                                    (equalp el1 el2))))))

(deftestlab test-lab6 ((lst-in-1 '(a b c))
                       (lst-out-1 '(nil (a) (b) (c) (a b) (a c) (b c) (a b c)))
                       (lst-in-2 '(a b c d e))
                       (lst-out-2 '(nil (a) (b) (c) (d) (e)
                                    (a b) (a c) (a d) (a e) (b c) (b d) (b e) (c d) (c e) (d e)
                                    (a b c) (a b d) (a b e) (a c d) (a c e) (a d e) (b c d) (b c e) (c d e)
                                    (a b c d) (a b c e) (b c d e) (a b c d e))))
  (20-version-1.1 (lab6-task-20v1 lst-in-1) lst-out-1 #'set-of-sets-eq)
  (20-version-1.2 (lab6-task-20v1 lst-in-2) lst-out-2 #'set-of-sets-eq)
  (20-version-2.1 (lab6-task-20v2 lst-in-1) lst-out-1 #'set-of-sets-eq)
  (20-version-2.2 (lab6-task-20v2 lst-in-2) lst-out-2 #'set-of-sets-eq))

;; test all

(defun test-all ()
  (test-lab3)
  (test-lab4)
  (test-lab5)
  (test-lab6))
