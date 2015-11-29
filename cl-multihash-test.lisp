;;;; cl-multihash-test.lisp
;;;;
;;;; multihash test suite

(cl:in-package #:cl-user)

(defpackage cl-multihash-test
  (:use :cl :multihash)
  (:import-from :5am def-suite fail in-suite is run! is-false signals test)
  (:shadowing-import-from :multihash multihash-definition-code multihash-definition-name)
  (:shadowing-import-from :ironclad byte-array-to-hex-string hex-string-to-byte-array)
  (:export run-all-tests))

(in-package :cl-multihash-test)

(def-suite cl-multihash)

(defun run-all-tests ()
  (run! 'cl-multihash))

(in-suite cl-multihash)

(defmethod asdf:perform ((o asdf:test-op) (c (eql (asdf:find-system :cl-multihash-test))))
  (format t "Starting tests.~%")
  (run-all-tests)
  (format t "Tests finished.~%"))

;; unit test variables

(defvar test-codes
  '((#x11 :sha1)
    (#x12 :sha256)
    (#x13 :sha512)
    (#x14 :sha3)
    (#x40 :blake2b)
    (#x41 :blake2s)))

(defvar test-cases
  '(("0beec7b5ea3f0fdbc95d0dd47f3c5bc275da8a33" #x11 :sha1)
    ("0beec7b5" #x11 :sha1)
    ("2c26b46b68ffc68ff99b453c1d30413413422d706483bfa0f98a5e886266e7ae" #x12 :sha256)
    ("2c26b46b" #x12 :sha256)
    ("0beec7b5ea3f0fdbc9" #x40 :blake2b)))

(defvar fail-multihashes
  '(;; invalid multihash code
    "7520d67100b7daece098c7e4fa1a360f1a2d8ec88ef6a2e6d168e6788e19f2806f72"
    ;; inconsistent length
    "1220d67100b7daece098c7e4fa1a360f1a2d8ec88ef6a2e6d168e6788e19f2806f72767770"
    ;; too short
    "12"
    ;; too long
    "12121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212"))

(defvar fail-encode
  `((#(1 2 3) ,(hex-string-to-byte-array "badcab")) ; bad digest-name type
    (:shaLOL ,(hex-string-to-byte-array "badcab")) ; unsupported digest
    (:sha256 ,(hex-string-to-byte-array "12121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212")))) ; too long

(defvar equal-encode
  ;; these are three ways of saying the same thing
  `((:sha256 ,(hex-string-to-byte-array "badcab"))
    ("sha256" ,(hex-string-to-byte-array "badcab"))
    (#x12 ,(hex-string-to-byte-array "badcab"))))

(defvar multihash-object-cases
  `((:sha256 "lol" "QmNpFhQ4bJgKDrm8nh211ayWvNL21s7r1y63BRHqfgp7s9")))

;;; utility functions

(defun create-test-multihash (hex code)
  (let ((bytes (hex-string-to-byte-array hex))
        (new-bytes (make-array 2 :element-type '(unsigned-byte 8))))
    (setf (aref new-bytes 0)  (coerce code '(unsigned-byte 8)))
    (setf (aref new-bytes 1) (coerce (length bytes) '(unsigned-byte 8)))
    (concatenate '(vector (unsigned-byte 8) *)
                 new-bytes bytes)))

(defun vector-equal (vector1 vector2)
  "Compares two vectors for equality."
  (declare
    (type (vector (unsigned-byte 8)) vector1)
    (type (vector (unsigned-byte 8)) vector2))
  (if (= (length vector1) (length vector2))
    (loop for index below (length vector1)
          always (= (aref vector1 index) (aref vector2 index)))
    nil))

;; Unit tests begin here

(test encode
  (loop for (hex code name) in test-cases
        do (tagbody
             (handler-bind ((error #'(lambda (condition)
                                       (fail condition)
                                       (go end))))
               (let ((bytes (hex-string-to-byte-array hex))
                     (new-bytes (make-array 2 :element-type '(unsigned-byte 8))))
                 (setf (aref new-bytes 0) (coerce code '(unsigned-byte 8)))
                 (setf (aref new-bytes 1) (coerce (length bytes) '(unsigned-byte 8)))
                 (let ((new-bytes (concatenate '(vector (unsigned-byte 8) *)
                                               new-bytes bytes))
                       (encoded (encode name bytes)))
                   (is (vector-equal encoded new-bytes)
                       "encoded byte mismatch: ~S ~S"
                       (byte-array-to-hex-string encoded)
                       (byte-array-to-hex-string new-bytes))
                   (let ((multihash (create-test-multihash hex code)))
                     (is (vector-equal multihash new-bytes)
                         "multihash func mismatch")))))
             end)))

(test decode
  (loop for (hex code name) in test-cases
        do (tagbody
             (handler-bind ((error #'(lambda (condition)
                                       (fail condition)
                                       (go end))))
               (let ((bytes (hex-string-to-byte-array hex))
                     (new-bytes (make-array 2 :element-type '(unsigned-byte 8))))
                 (setf (aref new-bytes 0) (coerce code '(unsigned-byte 8)))
                 (setf (aref new-bytes 1) (coerce (length bytes) '(unsigned-byte 8)))
                 (let* ((new-bytes (concatenate '(vector (unsigned-byte 8) *)
                                                new-bytes bytes))
                        (decoded (decode new-bytes)))
                   (is (= (decoded-multihash-code decoded) code)
                       "decoded code mismatch: ~X ~X"
                       (decoded-multihash-code decoded)
                       code)
                   (is (equal (decoded-multihash-name decoded) name)
                       "decoded name mismatch: ~S ~S"
                       (symbol-name (decoded-multihash-name decoded))
                       (symbol-name name))
                   (is (= (decoded-multihash-length decoded) (length bytes))
                       "decoded length mismatch: ~D ~D"
                       (decoded-multihash-length decoded)
                       (length bytes))
                   (is (vector-equal (decoded-multihash-digest decoded) bytes)
                       "decoded byte mismatch: ~S ~S"
                       (byte-array-to-hex-string (decoded-multihash-digest decoded))
                       (byte-array-to-hex-string bytes)))))
             end)))

(test table
  (loop for (code name) in test-codes
     do
       (is (eq (multihash-definition-name
		(find code *multihash-definitions* :key #'multihash-definition-code))
	       name)
	   "Table mismatch: ~S ~S"
	   (multihash-definition-name
	    (find code *multihash-definitions* :key #'multihash-definition-code))
	   name)
       (is (= (multihash-definition-code
	       (find name *multihash-definitions* :key #'multihash-definition-name))
	      code)
	   "Table mismatch: ~X ~X"
	   (multihash-definition-code
	    (find name *multihash-definitions* :key #'multihash-definition-name))
	   code)))

(test valid-code
  (loop for code below #xff
        do (let* ((test-code-exists ((lambda (code)
                                       (loop for (test-code _) in test-codes
                                             if (= code test-code)
                                               return t))
                                     code))
                  (test-code-valid (or (app-code-p code) test-code-exists)))
             (is (eql (valid-code-p code) test-code-valid)
                 "VALID-CODE-P incorrect for ~X"
                 code))))

(test app-code
  (loop for code below #xff
        do (is (eql (app-code-p code) (and (>= code 0) (< code #x10)))
               "APP-CODE-P incorrect for ~X"
               code)))

(defparameter b58-hex-equiv
  ;; generated with multihash tool
  '(("Qmexf4GiBrD46cEFjQBNoUz6iKSGHNeJWmD88gLNYohgqS"
     "1220f6f44b0636ea0fd0753339fc58379035c40a9efc356fd6ddf4841df4f4a85cb5")
    ("QmfHUBdEnB6YXqxCZbmAduW8V5GZC6aTtGptWRhNPrpNKZ"
     "1220fbc608b82bf6e18d22e717e091b5a79abb3e0e9e280e0ec25831170f52e04690")
    ("QmdRYiSeNbR9cDei4mmUMc7RAQZ2CPQev6yTBEoJEqQxuC"
     "1220e0206e2689ecaa75bdf76ac2b48124e09bb0d19dcb2744699b5d6a605a853a87")))

(test from-base58-and-from-hex-string
  (loop for (b58 hex) in b58-hex-equiv
        do (is (vector-equal (from-base58 b58)
                             (from-hex-string hex)))))
(test to-base58
  (loop for (b58 hex) in b58-hex-equiv
        do (is (string= b58
                        (to-base58 (from-hex-string hex))))))

(test to-hex-string
  (loop for (b58 hex) in b58-hex-equiv
        do (is (string= (to-hex-string (from-base58 b58))
                        hex))))

(test bad-hashes
  (loop for badhex in fail-multihashes
    do (is-false (multihash-p (hex-string-to-byte-array badhex)))))

(test fail-to-encode
  (loop for (digest seq) in fail-encode
    do (signals error (encode digest seq))))

(test equal-to-encode
  (let ((m1 (apply #'encode (first equal-encode)))
        (m2 (apply #'encode (second equal-encode)))
        (m3 (apply #'encode (third equal-encode))))
    (is (vector-equal m1 m2))
    (is (vector-equal m2 m3))
    (is (vector-equal m1 m3))))

(test multihash-object
  (loop for (digest obj result) in multihash-object-cases
        do (is (equal result (to-base58 (multihash-object digest obj))))))
