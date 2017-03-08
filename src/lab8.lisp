;;;; lab 8 for lisp course at KPI

(in-package :labs)

(defvar *graph* nil)

(defmacro with-graph (graph &body body)
  `(let ((*graph* ,graph))
     ,@body))

(defun @get-list (vn)
  (find vn *graph* :key #'first))

(defun @get-children (vn)
  (second (@get-list vn)))

(defun @get-children-with-lengths (vn)
  (let ((v-list (@get-list vn)))
    (map 'list (lambda (ch len) (cons ch len)) (second v-list) (third v-list))))

(defun @get-lngth (vn1 vn2)
  (reduce (lambda (acc el) (if (eq (car el) vn2) (cdr el) acc)) (@get-children-with-lengths vn1) :initial-value nil))

(defun @find-path (vn1 vn2 traverse-type &key return-ht?) ; traverse-type is :depth-first or :breadth first
  (let ((paths-ht (make-hash-table))
        (bf-visited (fset:set vn1))
        (bf-queue (queues:make-queue :simple-queue)))
    (queues:qpush bf-queue (list vn1 nil 0)) ; -> ( v path len)
    (labels ((%traverse-df (vn len path visited)
               (if (eq vn vn2)
                   (setf (gethash len paths-ht) (aif (gethash len paths-ht) (cons (reverse (cons vn2 path)) it) (list (reverse (cons vn2 path)))))
                   (awhen (@get-children-with-lengths vn)
                     (dolist (chwl it) (unless (find (car chwl) visited) (%traverse-df (car chwl) (+ len (cdr chwl)) (cons vn path) (cons vn visited)))))))
             (%traverse-bf ()
               (let* ((current (queues:qpop bf-queue))
                      (cur-vn (first current))
                      (cur-path (second current))
                      (cur-len (third current)))
                 (when cur-vn
                   (if (eq cur-vn vn2)
                       (setf (gethash cur-len paths-ht)
                             (aif (gethash cur-len paths-ht)
                                  (cons (reverse (cons vn2 cur-path)) it)
                                  (list (reverse (cons vn2 cur-path)))))
                       (awhen (@get-children-with-lengths cur-vn)
                         (dolist (chwl it) (unless (fset:lookup bf-visited (car chwl))
                                             (fset:adjoinf bf-visited (car chwl))
                                             (queues:qpush bf-queue (list (car chwl) (cons cur-vn cur-path) (+ cur-len (cdr chwl))))))))
                   (unless (= 0 (queues:qsize bf-queue)) (%traverse-bf))))))
      (if (eq traverse-type :depth-first) (%traverse-df vn1 0 nil nil) (when (eq traverse-type :breadth-first) (%traverse-bf)))
      (if return-ht?
          paths-ht
          (let ((m))
            (maphash (lambda (k v) (declare (ignore v)) (if m (when (< k m) (setf m k)) (setf m k))) paths-ht)
            (cons m (gethash m paths-ht)))))))

(defun @degree (vn)
  (reduce (lambda (acc el) (if (eq vn (car el)) (+ acc (list-length (second el)))
                          (+ acc (reduce (lambda (acc el2) (if (eq (car el2) vn) (+ acc 1) acc)) (@get-children-with-lengths (car el)) :initial-value 0))))
          *graph* :initial-value 0))

(defun @sort-by-degrees ()
  (zsort:quicksort (mapcar (lambda (v) (cons (@degree (car v)) (car v))) *graph*) #'> :key #'car))

;;;; tests

(deftestlab test-lab8 ((test-graph1 '((a (e l d) (3 14 1)) ; a -> d -> a loop
                                      (b (i) (4))
                                      (c (d e i) (3 1 3))
                                      (d (i a) (5 1))
                                      (e (f g h) (1 1 5))
                                      (f (i) (1))
                                      (g (l) (10))
                                      (h (j) (2))
                                      (i (h) (1))
                                      (j (k) (3))
                                      (k (l) (3))
                                      (l () ()))))
  (degree (with-graph test-graph1 (@degree 'e)) 5)
  (sort-by-degrees (with-graph test-graph1 (@sort-by-degrees)) '((5 . E) (5 . I) (4 . D) (4 . A) (3 . C) (3 . H) (3 . L) (2 . F) (2 . J) (2 . K) (2 . G) (1 . B)))
  (breadth-first (with-graph test-graph1 (@find-path 'a 'l :breadth-first)) '(14 (a l)))
  (depth-first (with-graph test-graph1 (@find-path 'a 'l :depth-first)) '(14 (A L) (A E G L) (A E F I H J K L))))

