;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
;;;; multihash.lisp

;;; Multihash is a protocol for differentiating outputs from various
;;; well-established cryptographic hash functions, addressing size + encoding
;;; considerations.
;;;
;;; It is useful to write applications that future-proof their use of hashes,
;;; and allow multiple hash functions to coexist. See
;;; https://github.com/jbenet/random-ideas/issues/1 for a longer discussion.
;;;
;;; Format:
;;;
;;;   <1B hash function code><1B digest size in bytes><hash function output>
;;;
;;; Binary example (only 4 bytes for simplicity):
;;;
;;; fn code  dig size hash digest
;;; -------- -------- ------------------------------------
;;; 00010001 00000100 101101100 11111000 01011100 10110101
;;; sha1     4 bytes  4 byte sha1 digest
;;;
;;; For more information, see: https://github.com/jbenet/multihash
;;; -- Copied from https://github.com/jbenet/multihash

(in-package #:multihash)

(defparameter *definitions*
  '((sha1 #x11 20)
    (sha256 #x12 32)
    (sha512 #x13 64)
    (sha3 #x14 64)
    (blake2b #x40 64)
    (blake2s #x41 32)))

(defstruct multihash-definition
  name code length)

(defparameter *multihash-definitions*
  (loop for (name code length) in *definitions*
     collect (make-multihash-definition :name name :code code :length length))
  "multihash definitions mapped by digest symbol and multihash code")

(deftype multihash ()
  '(satisfies multihashp))

(defstruct decoded-multihash
  code name length digest)

(defun app-code-p (code)
  "Checks whether a multihash code is part of the valid app range."
  (and
   (integerp code)
   (>= code 0)
   (< code #x10)))

(defun valid-code-p (code)
  "Checks whether a multihash code is valid."
  (cond
    ((app-code-p code) t)
    ((member code *multihash-definitions* :key #'multihash-definition-code) t)
    (t nil)))

(defun multihashp (sequence)
  "Return T if SEQUENCE is a valid multihash, otherwise, return NIL.

SEQUENCE must be a (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*))"
  (declare (type (simple-array (unsigned-byte 8)) sequence))
  (block nil
    (handler-bind ((error (lambda (condition)
                            (declare (ignore condition))
                            (return nil))))
      (let ((decoded (decode sequence)))
        (if (valid-code-p (decoded-multihash-code decoded))
            t)))))

(defun encode (digest-name sequence)
  "Encode a hash digest along with the specified function code. Note: the
length is derived from SEQUENCE, rather than by the multihash definition.

SEQUENCE must be a (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*))"
  (declare (type (simple-array (unsigned-byte 8)) sequence))
  (typecase digest-name
    (symbol
     (let ((multihash-definition (find digest-name *multihash-definitions* :key #'multihash-definition-name)))
       (cond
         ((null multihash-definition) (error 'unsupported-digest digest-name))
         ((> (length sequence) 127) (error "Length Not Supported: ~S" sequence))
         (t (concatenate '(vector (unsigned-byte 8) *)
                         (vector (multihash-definition-code multihash-definition) (length sequence))
                         sequence)))))
    (t
     (error 'type-error :datum digest-name :expected-type 'symbol))))

(defun decode (sequence)
  "Decode a hash from a given multihash. Returns a DECODED-MULTIHASH struct or
 errors.

SEQUENCE must be a (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*))"
  (declare (type (simple-array (unsigned-byte 8)) sequence))
  (cond
    ((< (length sequence) 3) (error "Too Short: ~S" sequence))
    ((> (length sequence) 129) (error "Too Long: ~S" sequence))
    (t
     (let ((decoded
	    (make-decoded-multihash
	     :code (aref sequence 0)
	     :name (multihash-definition-name (find (aref sequence 0) *multihash-definitions* :key #'multihash-definition-code))
	     :length (aref sequence 1)
	     :digest (subseq sequence 2))))
       (cond
         ((not (= (length (decoded-multihash-digest decoded)) (decoded-multihash-length decoded)))
          (error "Inconsistent Length: ~S, length ~D should equal ~D" decoded (length (decoded-multihash-digest decoded)) (decoded-multihash-length decoded)))
         ((not (valid-code-p (decoded-multihash-code decoded)))
          (error "Invalid Multihash Code: ~S" decoded))
         (t
          decoded))))))

;;; multihash high-level functions

(defun multihash-file (digest-name pathname)
  "Return the multihash of the contents of the file named by PATHNAME using
the algorithm DIGEST-NAME."
  (encode digest-name (digest-file digest-name pathname)))

(defun multihash-stream (digest-name stream)
  "Return the multihash of the contents of STREAM using the algorithm
DIGEST-NAME.  STREAM-ELEMENT-TYPE of STREAM should be (UNSIGNED-BYTE 8)."
  (encode digest-name (digest-stream digest-name stream)))

(defun multihash-sequence (digest-name sequence)
  "Return the multihash of the subsequence of SEQUENCE
specified by START and END using the algorithm DIGEST-NAME.  For CMUCL
and SBCL, SEQUENCE can be any vector with an element-type
of (UNSIGNED-BYTE 8); for other implementations, SEQUENCE must be a
(SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*))."
  (encode digest-name (digest-sequence digest-name sequence)))

;;; extra utilities

(defun chars-to-byte-array (chars)
  (declare (type string chars))
  (loop with vector = (make-sequence '(vector (unsigned-byte 8)) (length chars))
     for index below (length chars)
     do (setf (aref vector index) (char-code (aref chars index)))
     finally (return vector)))

(defun byte-array-to-chars (bytes)
  (declare (type (vector (unsigned-byte 8)) bytes))
  (loop with chars = (make-sequence 'string (length bytes))
     for index below (length bytes)
     do (setf (aref chars index) (code-char (aref bytes index)))
     finally (return chars)))

(defun to-base58 (octets)
  (declare (type multihash octets))
  (base58:encode (byte-array-to-chars octets)))

(defun from-base58 (string)
  (declare (type string string))
  (let ((multihash (chars-to-byte-array (base58:decode string))))
    (declare (type multihash multihash))
    multihash))

(defun to-hex-string (octets)
  (declare (type multihash octets))
  (ironclad:byte-array-to-hex-string octets))

(defun from-hex-string (string)
  (declare (type string string))
  (let ((multihash (ironclad:hex-string-to-byte-array string)))
    (declare (type multihash multihash))
    multihash))
