;;;; lab 7 for lisp cource at KPI

(in-package #:labs)

(defclass table ()
  ((name :accessor table-name :initarg :name :initform nil)
   (row-type :accessor table-row-type :initarg :row-type :initform (error "You must supply the type of a row for 'table object"))
   (rows :accessor table-rows :initarg :rows :initform nil)))

(defmethod add-row ((tbl table) obj)
  (if (typep obj (table-row-type tbl))
      (push obj (table-rows tbl))
      (error "~A is not of desired type ~A" obj (table-row-type tbl))))

(defmethod find-row ((tbl table) col-name value &key (test #'equalp))
  (member-if (lambda (row-obj) (funcall test value (slot-value row-obj col-name))) (table-rows tbl)))

(defmethod delete-row ((tbl table) col-name value &key (test #'equalp) count)
  (setf (table-rows tbl) (remove-if (lambda (row-obj) (funcall test value (slot-value row-obj col-name))) (table-rows tbl) :count count)))

(defmethod print-table ((tbl table) width stream)
  (let* ((rows (table-rows tbl))
         (row1 (car rows))
         (width width)             ; TODO determine max width
         (col-count (list-length (sb-mop:class-slots (class-of row1))))
         (horizontal-line (make-string width :initial-element #\BOX_DRAWINGS_LIGHT_HORIZONTAL))
         (list-of-horizontal-lines (make-list col-count :initial-element horizontal-line))
         (line-upper (format nil "┌~{~A~^┬~}┐~%" list-of-horizontal-lines))
         (line-middle (format nil "├~{~A~^┼~}┤~%" list-of-horizontal-lines))
         (line-lower (format nil "└~{~A~^┴~}┘~%" list-of-horizontal-lines)))
    (format t "~A" line-upper)
    (print-as-row (car rows) width stream :key-fn (lambda (name value) (declare (ignore value)) name)) ; print hat
    (format t "~A" line-middle)
    (map nil (lambda (row) (print-as-row row width stream)) rows)
    (format t "~A" line-lower)))

;; (defmethod find-row-by-string ((tbl table) col-name string-value &key (test #'string=))
;;   ;; (let ((word-parts-list (split-sequence:split-sequence #\* string-value))))
;;   (map nil (lambda ())
;;        (split-sequence:split-sequence #\* string-value)))

(defmacro create-table (name columns-list)
  (let* ((table-name (concatenate 'string (write-to-string name) "-TABLE"))
         (row-class-name-string (concatenate 'string "ROW-OF-" table-name))
         (row-class-name-symbol (intern row-class-name-string)))
    `(block ,(gensym)
       (defclass ,row-class-name-symbol ()
         ,(mapcar (lambda (column-name-symbol)
                    (let ((column-name-string (write-to-string column-name-symbol)))
                      `(,column-name-symbol :accessor ,(intern (concatenate 'string row-class-name-string "-" column-name-string))
                                            :initarg ,(intern column-name-string "KEYWORD") 
                                            :initform nil)))
           columns-list))))) 

;;;; test

(defclass hm ()
  ((slt1 :accessor hm-slt1 :initarg :slt1 :initform 123)
   (slt2 :accessor hm-slt2 :initarg :slt2 :initform 345)))

(defclass hm2 ()
  ((hmhm :accessor hm2-hmhm :initarg :hmhm :initform 'opana)))

(defun test ()
  (let ((row-obj1 (make-instance 'hm :slt1 123 :slt2 345))
        (row-obj2 (make-instance 'hm :slt1 567 :slt2 789))
        (row-obj3 (make-instance 'hm :slt1 111 :slt2 222))
        (invalid-row-obj (make-instance 'hm2 :hmhm 999)))
    (let ((table-obj (make-instance 'table :name "table-table" :row-type 'hm)))
      (add-row table-obj row-obj1)
      (add-row table-obj row-obj2)
      (add-row table-obj row-obj3)
      (handler-case (add-row table-obj invalid-row-obj)
        (condition (c) (declare (ignore c))))
      (print-table table-obj 10 t)
      (format t "------> (delete-row table-obj 'slt1 123)~%")
      (delete-row table-obj 'slt1 123)
      (print-table table-obj 10 t))))
