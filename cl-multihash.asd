;;;; cl-multihash.asd
(cl:in-package #:asdf-user)

(defsystem #:cl-multihash
  :description "Multihash utility for Common Lisp"
  :version "0.0.1"
  :author "Cayman Nava <caymannava@gmail.com>"
  :license "MIT"
  :depends-on (#:alexandria #:babel #:cl-base58 #:ironclad)
  :serial t
  :components ((:file "package")
               (:file "cl-multihash")))

(defsystem #:cl-multihash-test
  :name "cl-multihash tests"
  :description "Unit tests for cl-multihash"
  :depends-on (:cl-multihash :fiveam)
  :components ((:file "cl-multihash-test")))

(defmethod perform ((op asdf:test-op) (system (eql (find-system :cl-multihash))))
  (format t "Loading tests.~%")
  (oos 'load-op :cl-multihash-test)
  (oos 'test-op :cl-multihash-test))
