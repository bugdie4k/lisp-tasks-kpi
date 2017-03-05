;;;; testing framework for lisp course at KPI

(defpackage #:my-tests
  (:use #:cl #:utils)
  (:export #:deftest
           #:deftestlab))

(in-package #:my-tests)

(defmacro deftest (test-name form-tested form-expected test-fn)
  "Defines a function with name 'test-name' which s set to use
'test-fn' to compare evaluation results from 'form-tested' and 'form-expected'.
If 'test-gn' returnes nil - signals simple condition.
If 'test-gn' doesn't return nil - returnes that value."
  `(defun ,test-name ()
     (let ((result ,form-tested)
           (expected ,form-expected))
       (aif (funcall ,test-fn result expected)
            it 
            (error "error in test ~A:~%expected:~%~A~%result:~%~A~%"
                   ',test-name expected result)))))

(defmacro deftestlab (lab-test-name let-list &rest test-lists-list)
  "Takes let-list like ((a 1) (b 2)) and then 
lists like (test-task1 (cons 1 (cons 'a (cons 2 (cons 'b nil)))) '(1 a 2 b) #'test-func)
and produces test functions with 'deftest' and then calls them."
  (let ((test-func-calls)
        (global-let-list (mapcar (lambda (let-form) `(,(gensym) ,(second let-form))) let-list)))
    (labels ((%with-expanding-local-let-list (form-tested)
               `(let ,(map 'list (lambda (orig-let-form gen-global-let-form)
                                   `(,(first orig-let-form) ,(first gen-global-let-form)))
                       let-list global-let-list)
                  ,form-tested))
             (%expand-test-lists-list (test-lists-list)
               (when test-lists-list
                 (let* ((test-list (first test-lists-list))
                        (test-name (first test-list))
                        (form-tested (second test-list))
                        (form-expected (third test-list))
                        (test-fn (fourth test-list))
                        (test-full-name (intern (concatenate 'string (write-to-string lab-test-name) "-" (write-to-string test-name)))))
                   ;; while generating 'deftests' i also collect a form in which test function should be called to
                   ;; 'test-func-calls'.
                   ;; i know that 'setf' is not functional, but i didn't want to mess with multiple-value-bind
                   (setf test-func-calls (cons `(format t " ~A ~A~%"
                                                        (handler-case (,test-full-name)
                                                          (error (se) (declare (ignore se)) (push ',test-full-name failed-test-func-names) "-")
                                                          (:no-error (ret-val) (declare (ignore ret-val)) "+"))
                                                        ',test-full-name)
                                               test-func-calls)) 
                   (cons `(deftest ,test-full-name ,(%with-expanding-local-let-list form-tested) ,form-expected ,(if test-fn test-fn '(function equalp)))
                         (%expand-test-lists-list (cdr test-lists-list)))))))      
      `(let* ,global-let-list
         ,@(append
            ;; definition of test functions
            (%expand-test-lists-list test-lists-list)
            ;; call test functions
            `((defun ,lab-test-name ()
                (let ((failed-test-func-names))
                  (format t "====== ~A ======~%" ',lab-test-name)
                  ;; call functions of tests
                  ,@(reverse test-func-calls)
                  ;; report failed tests
                  (format t " * FAILED:~%~A~%" 
                          (if failed-test-func-names
                              (format nil "~{   * ~A~%~}" failed-test-func-names)
                              "   * NONE"))
                  ;; return nil if failed and t if succeed
                  (null failed-test-func-names)))))))))
