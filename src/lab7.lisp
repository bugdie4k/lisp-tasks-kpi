;;;; lab 7 for lisp course at KPI

(in-package #:labs)

(defclass table ()
  ((name :accessor table-name :initarg :name :initform nil)
   (row-type :accessor table-row-type :initarg :row-type :initform (error "You must supply the type of a row for 'table object"))
   (rows :accessor table-rows :initarg :rows :initform nil)
   (max-widths :accessor table-max-widths :initarg :max-widths :initform nil)))

(defmethod add-row-obj ((tbl table) obj)
  (if (typep obj (table-row-type tbl))
      (progn (push obj (table-rows tbl))
             (setf (table-max-widths tbl)
                   (if (table-max-widths tbl)
                       (map 'list
                            (lambda (width slot)
                              (let ((slot-length (length (format nil "~A" (slot-value obj (sb-mop:slot-definition-name slot))))))
                                (if (< width slot-length)
                                    slot-length
                                    width)))
                            (table-max-widths tbl) (sb-mop:class-slots (class-of obj)))
                       (mapcar (lambda (slot) (length (format nil "~A" (slot-value obj (sb-mop:slot-definition-name slot)))))
                               (sb-mop:class-slots (class-of obj))))))
      (error "~A is not of desired type. Desired type is ~A" obj (table-row-type tbl))))

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
  (make-instance 'table :name (table-name tbl)
                        :row-type (table-row-type tbl)
                        :max-widths (mapcar (lambda (slt)
                                              (length (symbol-name (sb-mop:slot-definition-name slt))))
                                            (sb-mop:class-slots (class-of
                                                                 (make-instance (table-row-type tbl)) ;; SEVERE KLUDGE
                                                                 )))))

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
         (list-of-horizontal-lines
           (if width
               (make-list (list-length (sb-mop:class-slots (class-of row1)))
                          :initial-element (make-string width :initial-element #\BOX_DRAWINGS_LIGHT_HORIZONTAL))
               (mapcar (lambda (width)
                         (make-string width :initial-element #\BOX_DRAWINGS_LIGHT_HORIZONTAL))
                       (table-max-widths tbl))))
         (line-upper (format nil "┌~{~A~^┬~}┐~%" list-of-horizontal-lines))
         (line-middle (format nil "├~{~A~^┼~}┤~%" list-of-horizontal-lines))
         (line-lower (format nil "└~{~A~^┴~}┘~%" list-of-horizontal-lines)))
    (format stream "~A" line-upper)
    (print-as-row (car rows) (aif width it (table-max-widths tbl)) stream :key-fn (lambda (name value) (declare (ignore value)) name)) ; print hat
    (format stream "~A" line-middle)
    (map nil
         (if width
             (lambda (row) (print-as-row row width stream))
             (lambda (row) (print-as-row row (table-max-widths tbl) stream)))
         rows)
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
       (make-instance 'table :name ,table-name :row-type ',row-class-name-symbol :max-widths ',(mapcar (lambda (col) (length (symbol-name col))) columns-list)))))

;;;; test

(deftestlab test-lab7
    ((my-table (let ((my-table-raw (create-table movies-table name director year rating)))
                 (add-row my-table-raw :name "The Shawshenk Redemption" :director "Frank Darabont" :year 1994 :rating 9.3)
                 (add-row my-table-raw :name "The Godfather" :director "Francis Ford Coppola" :year 1972 :rating 9.2)
                 (add-row my-table-raw :name "The Dark Knight" :director "Christopher Nolan" :year 2008 :rating 9.0)
                 (add-row my-table-raw :name "12 Angry Men" :director "Sidney Lumet" :year 1957 :rating 8.9)
                 (add-row my-table-raw :name "Schindler's List" :director "Steven Spielberg" :year 1993 :rating 8.9)
                 (add-row my-table-raw :name "Logan" :director "James Mangold" :year 2017 :rating 8.8)
                 (add-row my-table-raw :name "Fight Club" :director "David Fincher" :year 1999 :rating 8.8)
                 (add-row my-table-raw :name "Pulp Fiction" :director "Quentin Tarantino" :year 1994 :rating 8.9)
                 (add-row my-table-raw :name "The Matrix" :director "The Wachowski Brothers" :year 1999 :rating 8.7)
                 my-table-raw)))
  (printing (let ((my-table (copy-instance my-table)))
              (with-output-to-string (stream)
                (print-table my-table stream)))
            "┌────────────────────────┬──────────────────────┬────┬──────┐
│NAME                    │DIRECTOR              │YEAR│RATING│
├────────────────────────┼──────────────────────┼────┼──────┤
│The Matrix              │The Wachowski Brothers│1999│8.7   │
│Pulp Fiction            │Quentin Tarantino     │1994│8.9   │
│Fight Club              │David Fincher         │1999│8.8   │
│Logan                   │James Mangold         │2017│8.8   │
│Schindler's List        │Steven Spielberg      │1993│8.9   │
│12 Angry Men            │Sidney Lumet          │1957│8.9   │
│The Dark Knight         │Christopher Nolan     │2008│9.0   │
│The Godfather           │Francis Ford Coppola  │1972│9.2   │
│The Shawshenk Redemption│Frank Darabont        │1994│9.3   │
└────────────────────────┴──────────────────────┴────┴──────┘
")
  (adding (let ((my-table (copy-instance my-table)))
            (with-output-to-string (stream)
              (add-row my-table :name "In the Name of the Father" :director "Jim Sheridan" :year 1993 :rating 8.1)
              (print-table my-table stream)))
          "┌─────────────────────────┬──────────────────────┬────┬──────┐
│NAME                     │DIRECTOR              │YEAR│RATING│
├─────────────────────────┼──────────────────────┼────┼──────┤
│In the Name of the Father│Jim Sheridan          │1993│8.1   │
│The Matrix               │The Wachowski Brothers│1999│8.7   │
│Pulp Fiction             │Quentin Tarantino     │1994│8.9   │
│Fight Club               │David Fincher         │1999│8.8   │
│Logan                    │James Mangold         │2017│8.8   │
│Schindler's List         │Steven Spielberg      │1993│8.9   │
│12 Angry Men             │Sidney Lumet          │1957│8.9   │
│The Dark Knight          │Christopher Nolan     │2008│9.0   │
│The Godfather            │Francis Ford Coppola  │1972│9.2   │
│The Shawshenk Redemption │Frank Darabont        │1994│9.3   │
└─────────────────────────┴──────────────────────┴────┴──────┘
")
  (deleting1 (let ((my-table (copy-instance my-table)))
               (with-output-to-string (stream)
                 (delete-row my-table :year 1994 :test #'=)
                 (print-table my-table stream)))
             "┌────────────────────────┬──────────────────────┬────┬──────┐
│NAME                    │DIRECTOR              │YEAR│RATING│
├────────────────────────┼──────────────────────┼────┼──────┤
│The Matrix              │The Wachowski Brothers│1999│8.7   │
│Fight Club              │David Fincher         │1999│8.8   │
│Logan                   │James Mangold         │2017│8.8   │
│Schindler's List        │Steven Spielberg      │1993│8.9   │
│12 Angry Men            │Sidney Lumet          │1957│8.9   │
│The Dark Knight         │Christopher Nolan     │2008│9.0   │
│The Godfather           │Francis Ford Coppola  │1972│9.2   │
└────────────────────────┴──────────────────────┴────┴──────┘
")
  (deleting2 (let ((my-table (copy-instance my-table)))
               (with-output-to-string (stream)
                 (delete-row my-table :year 1999 :test #'= :count 1)
                 (print-table my-table stream)))
             "┌────────────────────────┬──────────────────────┬────┬──────┐
│NAME                    │DIRECTOR              │YEAR│RATING│
├────────────────────────┼──────────────────────┼────┼──────┤
│The Matrix              │The Wachowski Brothers│1999│8.7   │
│Pulp Fiction            │Quentin Tarantino     │1994│8.9   │
│Fight Club              │David Fincher         │1999│8.8   │
│Logan                   │James Mangold         │2017│8.8   │
│12 Angry Men            │Sidney Lumet          │1957│8.9   │
│The Dark Knight         │Christopher Nolan     │2008│9.0   │
│The Godfather           │Francis Ford Coppola  │1972│9.2   │
│The Shawshenk Redemption│Frank Darabont        │1994│9.3   │
└────────────────────────┴──────────────────────┴────┴──────┘
")
  (deleting3 (let ((my-table (copy-instance my-table)))
               (with-output-to-string (stream)
                 (delete-row my-table :name "Pulp Fiction" :test #'string=)
                 (print-table my-table stream)))
             "┌────────────────────────┬──────────────────────┬────┬──────┐
│NAME                    │DIRECTOR              │YEAR│RATING│
├────────────────────────┼──────────────────────┼────┼──────┤
│The Matrix              │The Wachowski Brothers│1999│8.7   │
│Fight Club              │David Fincher         │1999│8.8   │
│Logan                   │James Mangold         │2017│8.8   │
│Schindler's List        │Steven Spielberg      │1993│8.9   │
│12 Angry Men            │Sidney Lumet          │1957│8.9   │
│The Dark Knight         │Christopher Nolan     │2008│9.0   │
│The Godfather           │Francis Ford Coppola  │1972│9.2   │
│The Shawshenk Redemption│Frank Darabont        │1994│9.3   │
└────────────────────────┴──────────────────────┴────┴──────┘
")
  (filtering (let ((my-table (copy-instance my-table)))
               (with-output-to-string (stream)
                 (print-table (filter-table my-table '((:name "The*") (:year "*1*9*") (:rating "*"))) stream)))
             "┌────────────────────────┬──────────────────────┬────┬──────┐
│NAME                    │DIRECTOR              │YEAR│RATING│
├────────────────────────┼──────────────────────┼────┼──────┤
│The Shawshenk Redemption│Frank Darabont        │1994│9.3   │
│The Godfather           │Francis Ford Coppola  │1972│9.2   │
│The Matrix              │The Wachowski Brothers│1999│8.7   │
└────────────────────────┴──────────────────────┴────┴──────┘
")
  (find1 (let ((my-table (copy-instance my-table)))
           (with-output-to-string (stream)
             (pretty-print-object (find-row/string-with-wildcard my-table '((:director "*ncis*Coppola"))) stream)))
         "NAME: \"The Godfather\"
DIRECTOR: \"Francis Ford Coppola\"
YEAR: 1972
RATING: 9.2
"))
