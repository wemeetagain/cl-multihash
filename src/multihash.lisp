(cl:in-package #:cl-user)

(defpackage #:multihash
  (:use #:cl
        #:multihash.core
        #:multihash.hashing
        #:multihash.util)
  (:import-from #:babel
                #:string-to-octets)
  (:export #:multihash
           ;; multihash interface
           #:hash-code
           #:hash-name
           #:digest
           ;; formatting accessors
           #:octets
           #:hex-string
           #:b58-string
           ;; high level multihash creation
           #:multihash-object
           #:%to-octets))

(in-package #:multihash)

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

;;; tl;dr: A multihash is a hash digest with 2 bytes of metadata prepended, a code and length

;;; multihash interface

(defgeneric hash-code (object)
  (:documentation "Returns the hash algorithm code of the multihash."))

(defgeneric hash-name (object)
  (:documentation "Returns the hash algorithm name of the multihash."))

(defgeneric digest (object)
  (:documentation "Return the digest of the multihash."))

;;; input/output common representations

(defgeneric octets (object)
  (:documentation "Returns the multihash as a (SIMPLE-ARRAY (UNSIGNED-BYTE 8) *)."))

(defgeneric (setf octets) (octets multihash)
  (:documentation "Set a multihash as a (SIMPLE-ARRAY (UNSIGNED-BYTE 8) *)."))

(defgeneric hex-string (object)
  (:documentation "Returns a hex-string representation of the multihash."))

(defgeneric (setf hex-string) (string multihash)
  (:documentation "Set a multihash as a hex-string."))

(defgeneric b58-string (object)
  (:documentation "Returns a base-58 string representation of the multihash."))

(defgeneric (setf b58-string) (string multihash)
  (:documentation "Set a multihash as a base-58 string."))

;;;

(defclass multihash ()
  ((%octets
     :initarg :octets
     :accessor octets
     :type multihash-octets)))

(defmethod print-object ((object multihash) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "~S" (b58-string object))))

(defmethod hash-code ((object multihash))
  (%code (octets object)))

(defmethod hash-name ((object multihash))
  (%name (octets object)))

(defmethod digest ((object multihash))
  (%digest (octets object)))

;;; formatting methods

(defmethod hex-string ((object multihash))
  (to-hex-string (octets object)))

(defmethod (setf hex-string) ((hex-string string) (object multihash))
  (setf (octets object)
        (from-hex-string hex-string)))

(defmethod b58-string ((object multihash))
  (to-base58 (octets object)))

(defmethod (setf b58-string) ((b58-string string) (object multihash))
  (setf (octets object)
        (from-base58 b58-string)))

;;; multihash creation

;;; %to-octets returns the serialized form of an object
;;; this is used to extend multihash-object
;;; it is recommended that any methods return a multicodec rather than just
;;; raw bytes in some unknown format
(defgeneric %to-octets (object)
    (:documentation "Returns a representation of the object as (SIMPLE-ARRAY (OCTET 8) *)"))

(defmethod %to-octets ((string string))
    (string-to-octets string))

(defgeneric multihash-object (digest object)
    (:documentation "Returns a multihash of OBJECT."))

(defmethod multihash-object (digest object)
    (multihash-sequence
      digest
      (if (typep object '(simple-array (unsigned-byte 8) *))
          object
          (%to-octets object))))

(defmethod multihash-object (digest (path pathname))
    (multihash-file digest path))

(defmethod multihash-object (digest (stream stream))
    (multihash-stream digest stream))

(defmethod multihash-object :around (digest stream)
  (make-instance 'multihash
                 :octets (call-next-method)))
