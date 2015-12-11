;;;; -*- mode: lisp; indent-tabs-mode: nil -*-

(cl:in-package #:cl-user)
(defpackage #:multihash.util
  (:use #:cl #:multihash.core)
  (:import-from #:babel
                #:string-to-octets)
  (:export
    ;; generic
    #:%to-octets
    ;; utilities
    #:from-base58
    #:to-base58
    #:from-hex-string
    #:to-hex-string))
(in-package #:multihash.util)

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
