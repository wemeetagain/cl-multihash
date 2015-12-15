;;;; -*- mode: lisp; indent-tabs-mode: nil -*-

(cl:in-package #:cl-user)
(defpackage #:multihash.definitions
  (:use #:cl)
  (:import-from #:alexandria
                #:make-keyword)
  (:export
    #:*definitions*
    #:definition
    #:definition-code
    #:definition-name
    #:definition-length
    #:app-code-p
    #:valid-code-p))
(in-package #:multihash.definitions)

;;; We store the lookup list in these structs
(defstruct definition
  "A multihash definition, a single hash algorithm."
  ;;; name is an IRONCLAD symbol digest name which is used behind the scenes
  ;;; the multihash-* functions
  (name nil :type symbol :read-only t)
  ;;; code is the multihash function code
  (code nil :type (unsigned-byte 8) :read-only t)
  ;;; length is the typical length of the digest
  (length nil :type fixnum :read-only t))

(setf (documentation 'definition-name 'function)
      "Returns the name of the hash algorithm.")

(setf (documentation 'definition-code 'function)
      "Returns the multihash-allocated code of the hash algorithm.")

(setf (documentation 'definition-length 'function)
      "Returns the multihash-allocated length of digest of the hash algorithm.")

;;; replicating table here to:
;;; 1. avoid parsing the csv
;;; 2. ensuring errors in the csv don't screw up code.
;;; 3. changing a number has to happen in two places.
(defparameter *definition-list*
  ;;; name function-code length
  '((sha1 #x11 20)
    (sha256 #x12 32)
    (sha512 #x13 64)
    (sha3 #x14 64)
    (blake2b #x40 64)
    (blake2s #x41 32)))

;;; *DEFINITIONS* is a list of all known multihash definitions
;;; It is used for all lookup purposes
(defparameter *definitions*
  (loop for (name code length) in *definition-list*
     collect (make-definition
               :name (make-keyword name)
               :code code
               :length length))
  "List of supported multihash definitions")

(defun app-code-p (code)
  "Checks whether a multihash code is part of the valid app range."
  (and
   (integerp code)
   (>= code 0)
   (< code #x10)))

(defun valid-code-p (code)
  "Checks whether a multihash code is valid."
  (or
    (app-code-p code)
    (member code *definitions*
            :key #'definition-code)))
