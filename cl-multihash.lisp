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

(defvar *multihash-definitions* (make-hash-table)
  "multihash definitions mapped by digest symbol and multihash code")

(defstruct multihash-definition
  name code length)

(defun defmultihash (name code length)
  (let ((multihash-definition (make-multihash-definition :name name :code code :length length)))
    (setf (gethash name *multihash-definitions*) multihash-definition)
    (setf (gethash code *multihash-definitions*) multihash-definition)))

(defmultihash 'sha1 #x11 20)
(defmultihash 'sha256 #x12 32)
(defmultihash 'sha512 #x13 64)
(defmultihash 'sha3 #x14 64)
(defmultihash 'blake2b #x40 64)
(defmultihash 'blake2s #x41 32)

(defstruct decoded-multihash
  code name length digest)

(defun app-code-p (code)
  "Checks whether a multihash code is part of the valid app range."
  (and
   (numberp code)
   (>= code 0)
   (< code #x10)))

(defun valid-code-p (code)
  "Checks whether a multihash code is valid."
  (cond
    ((app-code-p code) t)
    ((gethash code *multihash-definitions*) t)
    (t nil)))

(defun encode (digest-name sequence)
  "Encode a hash digest along with the specified function code. Note: the
length is derived from SEQUENCE, rather than by the multihash definition.

SEQUENCE must be a (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*))"
  (declare (type (simple-array (unsigned-byte 8)) sequence))
  (typecase digest-name
    (symbol
     (let ((multihash-definition (gethash digest-name *multihash-definitions*)))
       (cond
         ((cl:null multihash-definition) (error 'unsupported-digest digest-name))
         ((> (length sequence) 127) (error "Length Not Supported: ~S" sequence))
         (t (concatenate '(vector (unsigned-byte 8) *)
                         (vector (multihash-definition-code multihash-definition) (length sequence))
                         sequence)))))
    (t
     (error 'type-error :datum digest-name :expected-type 'symbol))))

;;; multihash high-level functions

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
	     :name (multihash-definition-name (gethash (aref sequence 0) *multihash-definitions*))
	     :length (aref sequence 1)
	     :digest (subseq sequence 2))))
       (cond
         ((not (= (length (decoded-multihash-digest decoded)) (decoded-multihash-length decoded)))
          (error "Inconsistent Length: ~S" decoded))
         ((not (valid-code-p (decoded-multihash-code decoded)))
          (error "Invalid Multihash Code: ~S" decoded))
         (t
          decoded))))))

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
