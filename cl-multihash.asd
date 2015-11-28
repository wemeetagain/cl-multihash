(cl:in-package #:asdf-user)

(defsystem #:cl-multihash
  :description "Multihash utility for Common Lisp"
  :version "0.1.0"
  :author "Cayman Nava <caymannava@gmail.com>"
  :license "MIT"
  :depends-on (#:alexandria #:babel #:cl-base58 #:ironclad)
  :serial t
  :components ((:file "package")
               (:file "cl-multihash"))
  :long-description #.(uiop:read-file-string
		       (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op cl-multihash-test))))

(defsystem #:cl-multihash-test
  :name "cl-multihash tests"
  :description "Unit tests for cl-multihash"
  :depends-on (:cl-multihash :fiveam)
  :components ((:file "cl-multihash-test")))
