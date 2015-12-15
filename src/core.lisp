;;;; -*- mode: lisp; indent-tabs-mode: nil -*-

(cl:in-package #:cl-user)
(defpackage #:multihash.core
  (:use #:cl #:multihash.definitions)
  (:import-from #:alexandria
                #:make-keyword
                #:symbolicate)
  (:export
    ;; core type
    #:multihash-octets
    #:multihash-octets-p
    ;; core functions
    #:%code #:%name #:%length #:%digest
    #:encode #:decode
    ;; core utility functions
    #:app-code-p
    #:valid-code-p))
(in-package #:multihash.core)

(deftype multihash-octets ()
  '(satisfies multihash-octets-p))

(defun multihash-octets-p (sequence)
  "Return T if SEQUENCE is a valid multihash octet array, otherwise return NIL.

SEQUENCE must be a (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*))"
  (and (typep sequence '(simple-array (unsigned-byte 8)))
       ;;; check length between 3 and 128, inclusively
       (>= (length sequence) 3)
       (<= (length sequence) 128)
       ;;; check digest length matches multihash length byte
       (= (- (length sequence) 2) (%length sequence))
       ;;; check multihash code byte validity
       (valid-code-p (%code sequence))))

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

(defun %code (mhash-octets)
  (aref mhash-octets 0))

(defun (setf %code) (code mhash-octets)
  (setf (aref mhash-octets 0) (the (unsigned-byte 8) code)))

(defun %name (mhash-octets)
  (multihash-definition-name
    (find (%code mhash-octets) *multihash-definitions*
          :key #'multihash-definition-code)))

(defun (setf %name) (name mhash-octets)
  (let ((definition (find name *multihash-definitions*
                          :key #'multihash-definition-name)))
    (if (null definition)
      (error "No multihash definition found: ~S" name)
      (setf (%code mhash-octets)
             (multihash-definition-code definition)))))

(defun %length (mhash-octets)
  (aref mhash-octets 1))

(defun (setf %length) (length mhash-octets)
  (if (or (< length 3)
          (> length 129))
    (error "Invalid length: ~D" length)
    (progn
      (setf (aref mhash-octets 1) (the (unsigned-byte 8) length))
      (unless (= (- (length mhash-octets) 2) length)
        (adjust-array mhash-octets (+ length 2))))))

(defun %digest (mhash-octets)
  (subseq mhash-octets 2))

(defun (setf %digest) (digest mhash-octets)
  (setf (%length mhash-octets) (length digest))
  (setf (subseq mhash-octets 2 (1- (length mhash-octets)))
        digest))

(defun encode (digest-name sequence)
  "Encode a hash digest along with the specified function code. Note: the
length is derived from SEQUENCE, rather than by the multihash definition.

SEQUENCE must be a (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*))"
  (declare (type (simple-array (unsigned-byte 8)) sequence))
  (flet ((%encode (symbol sequence)
           (let ((mhash-octets (make-array
                                 (+ (length sequence) 2)
                                 :element-type '(unsigned-byte 8))))
             (setf (%code mhash-octets)
                   (multihash-definition-code
                     (find
                       (make-keyword symbol) *multihash-definitions*
                       :key #'multihash-definition-name)))
             (setf (%digest mhash-octets)
                   sequence)
             mhash-octets)))
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
  "Decode a hash from a given multihash. Returns values:
   code, name, length, digest
   or errors.

SEQUENCE must be a (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*))"
  (declare (type (simple-array (unsigned-byte 8)) sequence))
  (if (multihash-octets-p sequence)
    (values
      (%code sequence)
	  (%name sequence)
	  (%length sequence)
	  (%digest sequence))
    (error "Invalid multihash octets: ~S" sequence)))
