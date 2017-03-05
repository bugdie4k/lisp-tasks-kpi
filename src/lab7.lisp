;;;; lab 7 for lisp course at KPI

(in-package #:labs)

(defclass table ()
  ((name :accessor table-name :initarg :name :initform nil)
   (row-type :accessor table-row-type :initarg :row-type :initform (error "You must supply the type of a row for 'table object"))
   (rows :accessor table-rows :initform nil)
   ))

(defmethod add-row-obj ((tbl table) obj)
  (if (typep obj (table-row-type tbl))
      (push obj (table-rows tbl))
      (error "~A is not of desired type ~A" obj (table-row-type tbl))))

(defmethod add-row ((tbl table) &rest args-for-making-row-instance)
  (add-row-obj tbl (apply #'make-instance (table-row-type tbl) args-for-making-row-instance)))

(defmethod find-row-by-column ((tbl table) col-name value &key (test #'equalp))
  (find-if (lambda (row-obj) (has-slot? row-obj col-name value test)) (table-rows tbl)))

(defmethod find-row ((tbl table) find-descriptors)
  "'find-descriptors' must be formatted as follows: ((column-name value-in-this-column-to-find test-function-for-this-column) ... ).
If test function is left out or is 'nil', 'equalp' is used."
  (find-if (lambda (row-obj)
             (dolist (fd find-descriptors t)
               (unless (has-slot? row-obj (first fd) (second fd) (aif (third fd) it #'equalp)) (return nil))))
           (table-rows tbl)))

(defmethod find-row-by-column/string-with-wildcard ((tbl table) col-name string-value &key (wildcard #\*))
  "The 'string-value' is going to be compared with 'write-to-string'ed values of a specified (with 'col-name') column.
If value is already a string it is not converted to a string."
  (find-if (lambda (row-obj) (has-slot?/with-wildcard-string row-obj col-name string-value wildcard)) (table-rows tbl)))

(defmethod find-row/string-with-wildcard ((tbl table) find-descriptors &key (wildcard #\*))
  "'find-descriptors' must be formatted as follows: ((column-name string-value-in-this-column-to-find) ... ).
The 'string-value-in-this-column-to-find' is going to be compared with 'write-to-string'ed values of a specified (with 'column-name') column.
If value is already a string it is not converted to a string."  
  (find-if (lambda (row-obj)
             (dolist (fd find-descriptors t)
               (unless (has-slot?/with-wildcard-string row-obj (first fd) (second fd) wildcard) (return nil))))
           (table-rows tbl)))

(defmethod make-same-table ((tbl table))
  (make-instance 'table :name (table-name tbl) :row-type (table-row-type tbl)))

;; this method is literally just me being tired of this "project"
(defmethod filter-table ((tbl table) find-descriptors &key (wildcard #\*))
  "'find-descriptors' must be formatted as follows: ((column-name string-value-in-this-column-to-find) ... )."
  (let ((res-table (make-same-table tbl)))
    (map nil (lambda (row-obj)
               (when (dolist (fd find-descriptors t)
                       (unless (has-slot?/with-wildcard-string row-obj (first fd) (second fd) wildcard) (return nil)))
                 (add-row-obj res-table row-obj)))
         (table-rows tbl))
    res-table))

(defmethod delete-row ((tbl table) col-name value &key (test #'equalp) count)
  (let ((col-sym-name (intern (symbol-name col-name))))
    (setf (table-rows tbl) (remove-if (lambda (row-obj) (funcall test value (slot-value row-obj col-sym-name))) (table-rows tbl) :count count))))

(defmethod print-table ((tbl table) stream &key width)
  (let* ((rows (table-rows tbl))
         (row1 (car rows))
         (width width)             ; TODO determine max width         
         (col-count (list-length (sb-mop:class-slots (class-of row1))))
         (horizontal-line (make-string width :initial-element #\BOX_DRAWINGS_LIGHT_HORIZONTAL))
         (list-of-horizontal-lines (make-list col-count :initial-element horizontal-line))
         (line-upper (format nil "┌~{~A~^┬~}┐~%" list-of-horizontal-lines))
         (line-middle (format nil "├~{~A~^┼~}┤~%" list-of-horizontal-lines))
         (line-lower (format nil "└~{~A~^┴~}┘~%" list-of-horizontal-lines)))
    (format stream "~A" line-upper)
    (print-as-row (car rows) width stream :key-fn (lambda (name value) (declare (ignore value)) name)) ; print hat
    (format stream "~A" line-middle)
    (map nil (lambda (row) (print-as-row row width stream)) rows)
    (format stream "~A" line-lower)))
 
(defmacro create-table (name &rest columns-list)
  (let* ((table-name (concatenate 'string (write-to-string name)))
         (row-class-name-string (concatenate 'string "ROW-OF-" table-name))
         (row-class-name-symbol (intern row-class-name-string)))
    `(progn
       (defclass ,row-class-name-symbol ()
         ,(mapcar (lambda (column-name-symbol)
                    (let ((column-name-string (write-to-string column-name-symbol)))
                      `(,column-name-symbol :accessor ,(intern (concatenate 'string row-class-name-string "-" column-name-string))
                                            :initarg ,(intern column-name-string "KEYWORD") 
                                            :initform nil)))
           columns-list))
       (make-instance 'table :name ,table-name :row-type ',row-class-name-symbol)))) 

;;;; test

(deftestlab test-lab7
    ((my-table-raw (create-table movies-table name director year rating))
     (my-table (progn (add-row my-table-raw :name "The Shawshenk Redemption" :director "Frank Darabont" :year 1994 :rating 9.3)
                      (add-row my-table-raw :name "The Godfather" :director "Francis Ford Coppola" :year 1972 :rating 9.2)
                      (add-row my-table-raw :name "The Dark Knight" :director "Christopher Nolan" :year 2008 :rating 9.0)
                      (add-row my-table-raw :name "12 Angry Men" :director "Sidney Lumet" :year 1957 :rating 8.9)
                      (add-row my-table-raw :name "Schindler's List" :director "Steven Spielberg" :year 1993 :rating 8.9)
                      my-table-raw)))
  (printing (with-output-to-string (stream)
              (print-table my-table stream :width 25))
            "")
  (adding (with-output-to-string (stream)
            (add-row my-table :name "Pulp Fiction" :director "Quentin Tarantino" :year 1994 :rating 8.9)
            (add-row my-table :name "In the Name of the Father" :director "Jim Sheridan" :year 1993 :rating 8.1)
            (print-table my-table stream :width 25))
          "")
  (deleting1 (with-output-to-string (stream)
               (delete-row my-table :year 1994 :test #'=)
               (print-table my-table stream :width 25))
             "")
  (deleting2 (with-output-to-string (stream)
               (delete-row my-table :year 1993 :test #'= :count 1)
               (print-table my-table stream :width 25))
             "")
  (deleting3 (with-output-to-string (stream)
               (delete-row my-table :name "Pulp Fiction" :test #'string=)
               (print-table my-table stream :width 25))
             ""))

(defun quicktest ()
  (let ((tbl (create-table tbl col1 col2 col3)))
    (add-row tbl :col1 11 :col2 12 :col3 "13")
    (add-row tbl :col1 21 :col2 22 :col3 "23")
    (add-row tbl :col1 31 :col2 32 :col3 "33")
    ;;;
    (pretty-print-object (find-row-by-column tbl :col2 22) t)
    (format t "----~%")
    (pretty-print-object (find-row-by-column/string-with-wildcard tbl :col3 "***3*") t)
    (format t "----------~%")
    (pretty-print-object (find-row tbl '((:col1 11 =) (:col2 12) (:col3 "13" string=))) t)
    (format t "----~%")
    (pretty-print-object (find-row/string-with-wildcard tbl '((:col1 "1*1") (:col2 "*12*") (:col3 "1*"))) t)
    (format t "----------~%")
    (print-table (filter-table tbl '((:col1 "3*") (:col2 "*2"))) t :width 25)))
