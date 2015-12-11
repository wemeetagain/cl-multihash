;;;; -*- mode: lisp; indent-tabs-mode: nil -*-

(cl:in-package #:cl-user)
(defpackage #:multihash.octets
  (:use #:cl #:multihash.core)
  (:import-from #:babel
                #:string-to-octets)
  (:export
    #:multihash-octets
    #:multihash-octets-p
    ;; generic
    #:%to-octets
    ;; utilities
    #:from-base58
    #:to-base58
    #:from-hex-string
    #:to-hex-string))
(in-package #:multihash.octets)

(deftype multihash-octets ()
  '(satisfies multihash-octets-p))

(defun multihash-octets-p (sequence)
  "Return T if SEQUENCE is a valid multihash octet array, otherwise return NIL.

SEQUENCE must be a (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*))"
  (and (typep (simple-array (unsigned-byte 8)) sequence)
       ;;; check length between 3 and 128, inclusively
       (>= (length sequence) 3)
       (<= (length sequence) 128)
       ;;; check digest length matches multihash length byte
       (= (- (length sequence) 2) (aref sequence 1))
       ;;; check multihash code byte validity
       (valid-code-p (aref sequence 0))))

;;;

;;; %to-octets returns the serialized form of an object
;;; this is used to extend multihash-object
;;; it is recommended that any methods return a multicodec rather than just
;;; raw bytes in some unknown format
(defgeneric %to-octets (object)
    (:documentation "Returns a representation of the object as (SIMPLE-ARRAY (OCTET 8) *)"))

(defmethod %to-octets ((string string))
    (string-to-octets string))

;;; multihash-octets utilities

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
  (declare (type multihash-octets octets))
  (base58:encode (byte-array-to-chars octets)))

(defun from-base58 (string)
  "Decode a base58-encoded string into a multihash."
  (declare (type string string))
  (let ((multihash (chars-to-byte-array (base58:decode string))))
    (declare (type multihash-octets multihash))
    multihash))

(defun to-hex-string (octets)
  "Encode a multihash to a hex string."
  (declare (type multihash-octets octets))
  (ironclad:byte-array-to-hex-string octets))

(defun from-hex-string (string)
  "Decode a hex string into a multihash."
  (declare (type string string))
  (let ((multihash (ironclad:hex-string-to-byte-array string)))
    (declare (type multihash-octets multihash))
    multihash))
