;;;; lab 7 for lisp course at KPI

(in-package #:labs)

(defclass table ()
  ((name :accessor table-name :initarg :name :initform nil)
   (row-type :accessor table-row-type :initarg :row-type :initform (error "You must supply the type of a row for 'table object"))
   (rows :accessor table-rows :initarg :rows :initform nil)))

(defmethod add-row-obj ((tbl table) obj)
  (if (typep obj (table-row-type tbl))
      (push obj (table-rows tbl))
      (error "~A is not of desired type ~A" obj (table-row-type tbl))))

(defmethod add-row ((tbl table) &rest args-for-making-row-instance)
  (add-row-obj tbl (apply #'make-instance (table-row-type tbl) args-for-making-row-instance)))

(defmethod find-row ((tbl table) col-name value &key (test #'equalp))
  (find-if (lambda (row-obj) (funcall test value (slot-value row-obj col-name))) (table-rows tbl)))

(defmethod delete-row ((tbl table) col-name value &key (test #'equalp) count)
  (setf (table-rows tbl) (remove-if (lambda (row-obj) (funcall test value (slot-value row-obj (intern (symbol-name col-name))))) (table-rows tbl) :count count)))

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
    (format stream "~A" line-upper)
    (print-as-row (car rows) width stream :key-fn (lambda (name value) (declare (ignore value)) name)) ; print hat
    (format stream "~A" line-middle)
    (map nil (lambda (row) (print-as-row row width stream)) rows)
    (format stream "~A" line-lower)))

(defun string-equal-with-wildcards (&key string-with-wildcards string (wildcard #\*))
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
        (error "STRING-EQUAL-WITH-WILDCARDS NEEDS ARGUMENTS"))))
 
(defmethod find-row-by-string ((tbl table) col-name string-value)
  (find-if (lambda (string-name)
               (string-equal-with-wildcards :string-with-wildcards string-value :string string-name :wildcard #\*))
             (table-rows tbl)
             :key (lambda (row-obj) (write-to-string (slot-value row-obj col-name)))))

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
  (1 (with-output-to-string (stream)
       (print-table my-table 24 stream))
     "┌────────────────────────┬────────────────────────┬────────────────────────┬────────────────────────┐
│NAME                    │DIRECTOR                │YEAR                    │RATING                  │
├────────────────────────┼────────────────────────┼────────────────────────┼────────────────────────┤
│Schindler's List        │Steven Spielberg        │1993                    │8.9                     │
│12 Angry Men            │Sidney Lumet            │1957                    │8.9                     │
│The Dark Knight         │Christopher Nolan       │2008                    │9.0                     │
│The Godfather           │Francis Ford Coppola    │1972                    │9.2                     │
│The Shawshenk Redemption│Frank Darabont          │1994                    │9.3                     │
└────────────────────────┴────────────────────────┴────────────────────────┴────────────────────────┘
")
  (2 (with-output-to-string (stream)
       (add-row my-table :name "Pulp Fiction" :director "Quentin Tarantino" :year 1994 :rating 8.9)
       (print-table my-table 24 stream))
     "┌────────────────────────┬────────────────────────┬────────────────────────┬────────────────────────┐
│NAME                    │DIRECTOR                │YEAR                    │RATING                  │
├────────────────────────┼────────────────────────┼────────────────────────┼────────────────────────┤
│Pulp Fiction            │Quentin Tarantino       │1994                    │8.9                     │
│Schindler's List        │Steven Spielberg        │1993                    │8.9                     │
│12 Angry Men            │Sidney Lumet            │1957                    │8.9                     │
│The Dark Knight         │Christopher Nolan       │2008                    │9.0                     │
│The Godfather           │Francis Ford Coppola    │1972                    │9.2                     │
│The Shawshenk Redemption│Frank Darabont          │1994                    │9.3                     │
└────────────────────────┴────────────────────────┴────────────────────────┴────────────────────────┘
")
  (3 (with-output-to-string (stream)
       (delete-row my-table :year 1994 :test #'=)
       (print-table my-table 24 stream))
     "2"))
