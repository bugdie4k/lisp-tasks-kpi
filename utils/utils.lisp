;;;; utils for lisp course at KPI

(defpackage #:utils
  (:use #:cl)
  (:export #:aif
           #:awhen
           #:acond
           ;; TODO ask somebody if exporting 'it' is a good idea at all.
           ;; also this is kinda cool how all the tests made with 'deftest' and 'deftestlab'
           ;; are automatically broken if i don't export 'it'
           #:it 
           #:traverse-slots
           #:has-slot?
           #:has-slot?/with-wildcard-string
           #:pretty-print-object
           #:print-as-row
           #:skip-el
           #:string-equal-with-wildcards
           #:copy-instance))

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
  (:documentation "Use fn function on each slot. 'fn' must take two parameters: slot-name and slot-value."))

(defmethod traverse-slots (obj fn)
  (labels ((%traverse-slots (slots-lst)
             (when slots-lst
               (let* ((slot (car slots-lst))
                      (name (sb-mop:slot-definition-name slot))
                      (value (slot-value obj name)))
                 (funcall fn name value)
                 (%traverse-slots (cdr slots-lst))))))
    (%traverse-slots (sb-mop:class-slots (class-of obj)))))

(defgeneric has-slot? (obj name value test)
  (:documentation "Checkes if object has a slot of a certain value"))

(defmethod has-slot? (obj name value test)
  (funcall test value (slot-value obj (intern (symbol-name name)))))

(defgeneric has-slot?/with-wildcard-string (obj name value wildcard)
  (:documentation "obj's slot by name 'name' is converted to string and compared to 'value'.
'value' can have a wildcard 'wildcard'.
If slot value is already a string it is not converted."))

(defmethod has-slot?/with-wildcard-string (obj name value wildcard)
  (let ((val (slot-value obj (intern (symbol-name name)))))
    (string-equal-with-wildcards :string-with-wildcards value :string (if (typep val 'string) val (write-to-string val)) :wildcard wildcard)))

(defgeneric pretty-print-object (obj stream)
  (:documentation "Pretty printer for objects. Prints all slots with format 'SLOT-NAME: SLOT-VALUE'"))

(defmethod pretty-print-object (obj stream)
  (traverse-slots obj (lambda (name val) (format stream "~S: ~S~%" name val))))

(defgeneric print-as-row (obj width-or-widths-list stream &key key-fn)
  (:documentation "Print object slots as a row in a table. Prints '|' char and then prints all slots with format 'SLOT_VALUE |'"))

(defmethod print-as-row (obj width-or-widths-list stream &key key-fn)
  "width is either number or list of widths for each column (slot)"
  (format stream "│")  
  (traverse-slots obj (if (numberp width-or-widths-list)
                          (lambda (name val)
                            (format stream "~{~VA~}│" `(,width-or-widths-list ,(aif key-fn (funcall it name val) val))))
                          (let ((i 0))
                            (lambda (name val)
                              (format stream "~{~VA~}│" `(,(nth i width-or-widths-list) ,(aif key-fn (funcall it name val) val)))
                              (incf i)))))
  (format stream "~%"))

;; others 

(defun skip-el (el lst &key (test #'equalp))
  "Skips 'el' in 'lst' and returnes the part where a sequence of elements 'el' is over."
  (when lst
    (if (funcall test el (car lst))
        (skip-el el (cdr lst) :test test)
        lst)))

(defun string-equal-with-wildcards (&key string-with-wildcards string (wildcard #\*))
  "Compares two strings with consideration of wildcards in the first one."
  (labels ((%skip-wildcard (str-lst)
             (skip-el wildcard str-lst :test #'char=))
           (%compare (swl sl)
             (let ((swl-1st (first swl)))
               (cond ((and swl sl)
                      (if (char= swl-1st wildcard)
                          (let* ((swl-rest (%skip-wildcard (rest swl)))
                                 (swl-2nd (first swl-rest)))                            
                            (if swl-2nd
                                (do ((sl-rest sl (rest sl-rest)))
                                    ((null sl-rest))
                                  (when (char= (first sl-rest) swl-2nd)
                                    (return (%compare (rest swl-rest) (rest sl-rest)))))
                                t))
                          (when (char= swl-1st (first sl))
                            (%compare (rest swl) (rest sl)))))
                     ((and swl (null sl))
                      (when (and (char= swl-1st wildcard)
                                 (not (first (%skip-wildcard (rest swl)))))
                        t))
                     ((and (null swl) sl) nil)
                     ((and (null swl) (null sl)) t)))))
    (if (and string-with-wildcards string)
        (let ((swl (coerce string-with-wildcards 'list))
              (sl (coerce string 'list)))
          (%compare swl sl))
        (error "STRING-EQUAL-WITH-WILDCARDS NEEDS ARGUMENTS THAT ARE NON-NIL"))))

;; http://stackoverflow.com/questions/11067899/is-there-a-generic-method-for-cloning-clos-objects
(defgeneric copy-instance (object &rest initargs &key &allow-other-keys)
  (:documentation "Makes and returns a shallow copy of OBJECT.

  An uninitialized object of the same class as OBJECT is allocated by
  calling ALLOCATE-INSTANCE.  For all slots returned by
  CLASS-SLOTS, the returned object has the
  same slot values and slot-unbound status as OBJECT.

  REINITIALIZE-INSTANCE is called to update the copy with INITARGS.")
  (:method ((object standard-object) &rest initargs &key &allow-other-keys)
    (let* ((class (class-of object))
           (copy (allocate-instance class)))
      (dolist (slot-name (mapcar #'sb-mop:slot-definition-name (sb-mop:class-slots class)))
        (when (slot-boundp object slot-name)
          (setf (slot-value copy slot-name)
            (slot-value object slot-name))))
      (apply #'reinitialize-instance copy initargs))))
