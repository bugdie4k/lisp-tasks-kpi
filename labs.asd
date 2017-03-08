;; ':serial t' says that each sub-component in ':components' list depends on the previous components
;; and otherwise this file is pretty srlf-explanatory.
;;
;; So the the sequence of loading files must be like this:
;;
;; ------------------------
;; utils {
;;        utils.lisp ->
;;        my-tests.lisp
;;       } ->
;; package ->
;; src {
;;      labs1thru6.lisp
;;      lab7.lisp
;;     }
;; ------------------------
;;
;; (this notation is made up by me)

(defsystem :labs
  :description
  "A system that consists of tasks for my Lisp course at KPI.
It also represents my attempts at understanding Lisp production basics.
By production basics I mean trying out ASDF and packages-related stuff."
  :version "0.0.0"
  :author "Danylo Fedorov <fedorough@gmail.com>"
  :depends-on ("queues" "queues.simple-queue" "fset" "zsort")
  :serial t
  :components ((:module "utils"
                :serial t
                :components ((:file "utils")
                             (:file "my-tests")))
               (:file "package")
               (:module "src"
                :components ((:file "labs1thru6")
                             (:file "lab7")
                             (:file "lab8")))))


