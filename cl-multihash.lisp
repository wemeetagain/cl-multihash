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

;;; tl;dr: A multihash is a hash digest with 5 bytes of metadata prepended

(in-package #:multihash)

;;; We store the lookup list in these structs
(defstruct multihash-definition
  "A multihash definition."
  ;;; name is an IRONCLAD symbol digest name which is used behind the scenes for
  ;;; the multihash-* functions
  (name nil :type symbol :read-only t)
  ;;; code is the multihash function code
  (code nil :type (unsigned-byte 8) :read-only t)
  ;;; length is the typical length of the digest
  (length nil :type fixnum :read-only t))

;;; replicating table here to:
;;; 1. avoid parsing the csv
;;; 2. ensuring errors in the csv don't screw up code.
;;; 3. changing a number has to happen in two places.
(defparameter *definitions*
  ;;; name function-code length
  '((sha1 #x11 20)
    (sha256 #x12 32)
    (sha512 #x13 64)
    (sha3 #x14 64)
    (blake2b #x40 64)
    (blake2s #x41 32)))

;;; *MULTIHASH-DEFINITIONS* is a list of all multihash-definitions
;;; It is used for all lookup purposes
(defparameter *multihash-definitions*
  (loop for (name code length) in *definitions*
     collect (make-multihash-definition :name (make-keyword name) :code code :length length))
  "List of supported multihash definitions")

(deftype multihash ()
  '(satisfies multihash-p))

;;; DECODE returns a DECODED-MULTIHASH
(defstruct decoded-multihash
  "A multihash deconstructed into its parts."
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

(defun multihash-p (sequence)
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
  (flet ((%encode (symbol sequence)
           (let ((multihash-definition (find (make-keyword symbol) *multihash-definitions*
                                             :key #'multihash-definition-name)))
             (cond
               ((null multihash-definition)
                (error "Unsupported Digest: ~S" digest-name))
               ((> (length sequence) 127)
                (error "Length Not Supported: ~S" sequence))
               (t (concatenate '(vector (unsigned-byte 8) *)
                               (vector (multihash-definition-code multihash-definition) (length sequence))
                               sequence))))))
    (declare (inline %encode))
    (typecase digest-name
      (symbol (%encode (symbolicate digest-name) sequence))
      (string (%encode (symbolicate (string-upcase digest-name)) sequence))
      (integer (%encode (multihash-definition-name
                         (find digest-name *multihash-definitions*
                               :key #'multihash-definition-code))
                        sequence))
      (t
       (error 'type-error :datum digest-name :expected-type 'symbol)))))

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

;;; multihash mid-level functions

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

;;; multihash high-level functions

;;; %to-octets returns the serialized form of an object
;;; this is used to extend multihash-object
;;; it is recommended that any methods return a multicodec rather than just
;;; raw bytes in some unknown format
(defgeneric %to-octets (object)
  (:documentation "Returns a representation of the object as (SIMPLE-ARRAY (OCTET 8) *)"))

(defgeneric multihash-object (digest object)
  (:documentation "Returns a multihash of OBJECT."))

(defmethod multihash-object (digest object)
  (multihash-sequence digest (%to-octets object)))

(defmethod multihash-object (digest (path pathname))
  (multihash-file digest path))

(defmethod multihash-object (digest (stream stream))
  (multihash-stream digest stream))

(defmethod %to-octets ((string string))
  (string-to-octets string))

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
  "Encode a multihash to a base58-encoded string."
  (declare (type multihash octets))
  (base58:encode (byte-array-to-chars octets)))

(defun from-base58 (string)
  "Decode a base58-encoded string into a multihash."
  (declare (type string string))
  (let ((multihash (chars-to-byte-array (base58:decode string))))
    (declare (type multihash multihash))
    multihash))

(defun to-hex-string (octets)
  "Encode a multihash to a hex string."
  (declare (type multihash octets))
  (ironclad:byte-array-to-hex-string octets))

(defun from-hex-string (string)
  "Decode a hex string into a multihash."
  (declare (type string string))
  (let ((multihash (ironclad:hex-string-to-byte-array string)))
    (declare (type multihash multihash))
    multihash))
