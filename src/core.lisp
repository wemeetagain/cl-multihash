;;;; -*- mode: lisp; indent-tabs-mode: nil -*-

(cl:in-package #:cl-user)
(defpackage #:multihash.core
  (:use #:cl #:multihash.definitions)
  (:import-from #:alexandria
                #:make-keyword
                #:symbolicate)
  (:export
    ;; core functions
    #:encode #:decode
    ;; decoded-multihash struct and accessors
    #:decoded-multihash
    #:decoded-multihash-code
    #:decoded-multihash-name
    #:decoded-multihash-length
    #:decoded-multihash-digest
    ;; core utility functions
    #:app-code-p
    #:valid-code-p))
(in-package #:multihash.core)

;;; DECODE returns a DECODED-MULTIHASH
(defstruct decoded-multihash
  "A multihash deconstructed into its parts."
  (code nil
    :type (unsigned-byte 8)
    :read-only t)
  (name nil
    :type symbol
    :read-only t)
  (length nil
    :type fixnum
    :read-only t)
  (digest nil
    :type (simple-array (unsigned-byte 8) *)
    :read-only t))

(setf (documentation 'decoded-multihash-code 'function)
      "Returns the integer code of the underlying hash algorithm.")

(setf (documentation 'decoded-multihash-name 'function)
      "Returns the name of the underlying hash algorithm.")

(setf (documentation 'decoded-multihash-length 'function)
      "Returns the length of the underlying digest.")

(setf (documentation 'decoded-multihash-digest 'function)
      "Returns the underlying digest.")

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
       (error 'type-error
              :datum digest-name
              :expected-type '(symbol string integer))))))

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
