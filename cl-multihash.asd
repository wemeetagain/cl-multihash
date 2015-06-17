;;;; cl-multihash.asd

(asdf:defsystem #:cl-multihash
  :description "Multihash implementation for Common Lisp"
  :version "0.0.1"
  :author "Cayman Nava <caymannava@gmail.com>"
  :license "MIT"
  :depends-on (:ironclad)
  :serial t
  :components ((:file "package")
               (:file "cl-multihash")))

(asdf:defsystem #:cl-multihash-tests
  :name "cl-multihash tests"
  :description "Unit tests for cl-multihash"
  :depends-on (:cl-multihash :fiveam)
  :components ((:file "cl-multihash-test")))

(defmethod asdf:perform ((op asdf:test-op) (system (eql (asdf:find-system :cl-multihash))))
  (format t "Loading tests.~%")
  (asdf:oos 'asdf:load-op :cl-multihash-tests)
  (asdf:oos 'asdf:test-op :cl-multihash-tests))
