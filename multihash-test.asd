(cl:in-package #:asdf-user)

(defsystem #:multihash-test
  :name "multihash tests"
  :description "Unit tests for multihash"
  :depends-on (:multihash :fiveam)
  :components ((:module "t"
                :serial t
                :components
                ((:file "multihash-test")))))
