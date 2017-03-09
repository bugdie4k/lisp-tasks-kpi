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
                             (:file "lab8")
                             (:file "lab9")))))


