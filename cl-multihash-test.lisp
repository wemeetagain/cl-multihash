;;;; cl-multihash-test.lisp
;;;;
;;;; multihash test suite

(in-package :cl-multihash)

;; Set up tests to include 5AM test functions
(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(5am:def-suite 5am:fail 5am:in-suite 5am:is 5am:run! 5am:test))
  (export 'run-all-tests))

(def-suite cl-multihash)

(defun run-all-tests ()
  (run! 'cl-multihash))

(in-suite cl-multihash)

(defmethod asdf:perform ((o asdf:test-op) (c (eql (asdf:find-system :cl-multihash-tests))))
  (format t "Starting tests.~%")
  (run-all-tests)
  (format t "Tests finished.~%"))

;; unit test variables

(defvar test-codes
  '((#x11 sha1)
    (#x12 sha256)
    (#x13 sha512)
    (#x14 sha3)
    (#x40 blake2b)
    (#x41 blake2s)))

(defvar test-cases
  '(("0beec7b5ea3f0fdbc95d0dd47f3c5bc275da8a33" #x11 sha1)
    ("0beec7b5" #x11 sha1)
    ("2c26b46b68ffc68ff99b453c1d30413413422d706483bfa0f98a5e886266e7ae" #x12 sha256)
    ("2c26b46b" #x12 sha256)
    ("0beec7b5ea3f0fdbc9" #x40 blake2b)))

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
                   (is (equal (decoded-multihash-code decoded) code)
                       "decoded code mismatch: ~X ~X"
                       (decoded-multihash-code decoded)
                       code)
                   (is (equal (decoded-multihash-name decoded) name)
                       "decoded name mismatch: ~S ~S"
                       (decoded-multihash-name decoded)
                       name)
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
        (is (eq (multihash-definition-name (gethash code *multihash-definitions*)) name)
            "Table mismatch: ~S ~S"
            (multihash-definition-name (gethash code *multihash-definitions*))
            name)
        (is (= (multihash-definition-code (gethash name *multihash-definitions*)) code)
            "Table mismatch: ~X ~X"
            (multihash-definition-code (gethash name *multihash-definitions*))
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
