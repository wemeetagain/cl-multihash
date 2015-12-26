(cl:in-package #:cl-user)

(defpackage #:multihash-test
  (:use #:cl)
  (:import-from #:ironclad
		#:hex-string-to-byte-array)
  (:import-from #:prove
		#:diag
		#:finalize
		#:is
		#:isnt
		#:plan
		#:subtest)
  (:export))

(in-package #:multihash-test)

;;; utility functions

(defun vector-equal (vector1 vector2)
  "Compares two vectors for equality."
  (declare
    (type (vector (unsigned-byte 8)) vector1)
    (type (vector (unsigned-byte 8)) vector2))
  (if (= (length vector1) (length vector2))
    (loop for index below (length vector1)
          always (= (aref vector1 index) (aref vector2 index)))
    nil))

;;; tests begin here

(plan nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MULTIHASH.DEFINITIONS tests

(diag "DEFINITIONS")

(defparameter test-definitions
  '((#x11 :sha1 20)
    (#x12 :sha256 32)
    (#x13 :sha512 64)
    (#x14 :sha3 64)
    (#x40 :blake2b 64)
    (#x41 :blake2s 32)))

(subtest "*DEFINITIONS*"
  (loop for (code name length) in test-definitions
	do
	   (is code
	       (multihash.definitions:definition-code
		(find name multihash.definitions:*definitions*
		      :key #'multihash.definitions:definition-name)))
	do
	   (is name
	       (multihash.definitions:definition-name
		(find code multihash.definitions:*definitions*
		      :key #'multihash.definitions:definition-code)))
	   (is length
	       (multihash.definitions:definition-length
		(find name multihash.definitions:*definitions*
		      :key #'multihash.definitions:definition-name)))))

(subtest "APP-CODE-P"
  (loop for code below #xff
	do (is (multihash.definitions:app-code-p code)
	       (and (>= code 0) (< code #x10)))))

(subtest "VALID-CODE-P"
  (loop for code below #xff
	for test-code-exists = (member code test-definitions
				       :key #'car
				       :test #'=)
	for test-code-valid = (or
			       (multihash.definitions:app-code-p
				code)
			       test-code-exists)
	do
	   (is (not (null
		     (multihash.definitions:valid-code-p code)))
	       (not (null test-code-valid)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MULTIHASH.CORE tests

(diag "CORE")

;;; symbol macros so we get fresh arrays

(define-symbol-macro bad-test-octets
  (list
   #(9001 1 0)
   (make-array 2 :element-type '(unsigned-byte 8)
	       :initial-contents '(0 0))
   (make-array 500 :element-type '(unsigned-byte 8))
   (make-array 5 :element-type '(unsigned-byte 8)
	       :initial-contents '(18 32 1 2 3))))

(define-symbol-macro good-test-octets
  (list (make-array 34 :element-type '(unsigned-byte 8)
		       :initial-contents
		       '(18 32 74 122 92 42 235 224 113 100 23 4 120 84 38 115 151 226 74 68 208 204
  224 150 18 116 17 233 206 156 207 235 44 23))))

(defparameter good-test-octets-parts
  ;;; code name length digest hex-string b58-string
  (list
   '(18 :sha256 32
     (74 122 92 42 235 224 113 100 23 4 120 84 38 115 151 226 74 68 208 204 224 150
      18 116 17 233 206 156 207 235 44 23)
     "12204a7a5c2aebe0716417047854267397e24a44d0cce096127411e9ce9ccfeb2c17"
     "QmTMP6TqrfpSAeo4nbZ4BSRkcHZDs4hUwnbWH6hMJiikR4")))

(subtest "MULTIHASH-OCTETS-P"
  (loop for o in bad-test-octets
	do (is (multihash.core:multihash-octets-p o) nil))

  (loop for o in good-test-octets
	do (isnt (multihash.core:multihash-octets-p o) nil)))

;(subtest "%CODE")
;(subtest "%LENGTH")
;(subtest "%DIGEST")
;(subtest "(SETF %CODE)")
;(subtest "(SETF %LENGTH)")
;(subtest "(SETF %DIGEST)")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MULTIHASH.HASHING tests

;(diag "MULTIHASH.HASHING")

;(subtest "MULTIHASH-SEQUENCE")
;(subtest "MULTIHASH-STREAM")
;(subtest "MULTIHASH-FILE")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MULTIHASH tests

(diag "MULTIHASH")

(subtest "HASH-CODE"
  (loop for o in good-test-octets
	for mhash = (make-instance 'multihash:simple-multihash
				   :octets o)
	for (code) in good-test-octets-parts
	do (is (multihash:hash-code mhash) code)))

(subtest "HASH-NAME"
  (loop for o in good-test-octets
	for mhash = (make-instance 'multihash:simple-multihash
				   :octets o)
	for (c name) in good-test-octets-parts
	do (is (multihash:hash-name mhash) name)))

(subtest "DIGEST"
  (loop for o in good-test-octets
	for mhash = (make-instance 'multihash:simple-multihash
				   :octets o)
	for (c n l _digest) in good-test-octets-parts
	for digest = (make-array 32 :element-type '(unsigned-byte 8)
				    :initial-contents _digest)
	do (is (vector-equal (multihash:digest mhash) digest) t)))

(subtest "HEX-STRING"
  (loop for o in good-test-octets
	for mhash = (make-instance 'multihash:simple-multihash
				   :octets o)
	for (c n l d hex-string) in good-test-octets-parts
	do (is (multihash:hex-string mhash) hex-string)))

(subtest "B58-STRING"
  (loop for o in good-test-octets
	for mhash = (make-instance 'multihash:simple-multihash
				   :octets o)
	for (c n l d hs b58-string) in good-test-octets-parts
	do (is (multihash:b58-string mhash) b58-string)))

(defparameter b58-hex-string-pairs
  ;; generated with multihash tool
  '(("Qmexf4GiBrD46cEFjQBNoUz6iKSGHNeJWmD88gLNYohgqS"
     "1220f6f44b0636ea0fd0753339fc58379035c40a9efc356fd6ddf4841df4f4a85cb5")
    ("QmfHUBdEnB6YXqxCZbmAduW8V5GZC6aTtGptWRhNPrpNKZ"
     "1220fbc608b82bf6e18d22e717e091b5a79abb3e0e9e280e0ec25831170f52e04690")
    ("QmdRYiSeNbR9cDei4mmUMc7RAQZ2CPQev6yTBEoJEqQxuC"
     "1220e0206e2689ecaa75bdf76ac2b48124e09bb0d19dcb2744699b5d6a605a853a87")))

(subtest "B58-STRING"
  (loop for (expected-b58-string hex-string) in b58-hex-string-pairs
	for o = (hex-string-to-byte-array hex-string)
	for mhash = (make-instance 'multihash:simple-multihash
				     :octets o)
	do (is (multihash:b58-string mhash) expected-b58-string)))

(subtest "HEX-STRING"
  (loop for (bs expected-hex-string) in b58-hex-string-pairs
	for o = (hex-string-to-byte-array expected-hex-string)
	for mhash = (make-instance 'multihash:simple-multihash
				   :octets o)
	do (is (multihash:hex-string mhash)
	       expected-hex-string)))


;(subtest "(SETF B58-STRING)")
;(subtest "(SETF HEX-STRING)")

(defparameter multihash-object-cases
  `((:sha256 "lol" "QmNpFhQ4bJgKDrm8nh211ayWvNL21s7r1y63BRHqfgp7s9")))

;(subtest "%TO-OCTETS")

(subtest "MULTIHASH-OBJECT"
  (loop for (digest object expected-b58-string) in multihash-object-cases
	for mhash = (multihash:multihash-object digest object)
	do (is (multihash:b58-string mhash)
	       expected-b58-string)))

(finalize)
