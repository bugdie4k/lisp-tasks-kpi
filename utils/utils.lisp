;;;; utils for lisp course at KPI

(defpackage #:utils
  (:use #:cl)
  (:export #:aif
           #:awhen
           #:acond
           #:it ;; ask somebody if this is a good idea at all
           #:traverse-slots
           #:pretty-print-object
           #:print-as-row))

(in-package #:utils)

;; anaphoric macros

(defmacro aif (expr t-form &optional nil-form)
  (let ((tmp-sym (gensym)))
    `(let ((,tmp-sym ,expr))
       (if ,tmp-sym
           (let ((it ,tmp-sym))
             ,t-form)
           ,nil-form))))

(defmacro awhen (expr &body body)
  `(let  ((it ,expr))
     (when it
       ,@body)))

(defmacro acond (&rest rest)
  (labels ((%acond-expander (rest)
             (if rest
                 (if (eq (caar rest) 't)
                     `(eval (progn ,@(cdr (car rest))))
                     `(let ((it ,(car (car rest))))
                        (if it
                            (progn ,@(cdr (car rest)))
                            ,(%acond-expander (cdr rest)))))
                 `nil)))
    (%acond-expander rest)))

;; defmethods for aaaall objects


(defgeneric traverse-slots (obj fn)
  (:documentation "Use fn function on each slot. 'fn' takes two parameters: slot-name and slot-value."))

(defmethod traverse-slots (obj fn)
  (labels ((%traverse-slots (slots-lst)
             (when slots-lst
               (let* ((slot (car slots-lst))
                      (name (sb-mop:slot-definition-name slot))
                      (value (slot-value obj name)))
                 (funcall fn name value)
                 (%traverse-slots (cdr slots-lst))))))
    (%traverse-slots (sb-mop:class-slots (class-of obj)))))

(defgeneric pretty-print-object (obj stream)
  (:documentation "Pretty printer for objects. Prints all slots with format 'SLOT-NAME: SLOT-VALUE'"))

(defmethod pretty-print-object (obj stream)
  (traverse-slots obj (lambda (name val) (format stream "~A: ~A~%" name val))))

(defgeneric print-as-row (obj width stream &key key-fn)
  (:documentation "Print object slots as a row in a table. Prints '|' char and then prints all slots with format 'SLOT_VALUE |'"))

(defmethod print-as-row (obj width stream &key key-fn)
  (format stream "│")
  (traverse-slots obj (lambda (name val)
                        (format stream "~{~VA~}│" `(,width ,(aif key-fn (funcall it name val) val)))))
  (format stream "~%"))


