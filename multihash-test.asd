(cl:in-package #:asdf-user)

(defsystem #:multihash-test
  :name "multihash tests"
  :description "Unit tests for multihash"
  :depends-on (#:multihash #:prove)
  :defsystem-depends-on (#:prove-asdf)
  :components ((:module "t"
                :serial t
                :components
                ((:test-file "multihash-test"))))
  :perform (test-op :after (op c)
		    (funcall (intern #.(string :run) :prove) c)))
