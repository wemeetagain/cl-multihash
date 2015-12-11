(cl:in-package #:asdf-user)

(defsystem #:multihash
  :description "Multihash utility for Common Lisp"
  :version "0.1.0"
  :author "Cayman Nava <caymannava@gmail.com>"
  :license "MIT"
  :depends-on (#:alexandria #:babel #:cl-base58 #:ironclad)
  :serial t
  :components ((:module "src"
                :serial t
                :components
                ((:file "definitions")
                 (:file "core")
                 (:file "hashing")
                 (:file "util")
                 (:file "multihash"))))
  :long-description #.(uiop:read-file-string
		       (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op multihash-test))))
