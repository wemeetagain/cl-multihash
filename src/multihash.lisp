(cl:in-package #:cl-user)

(defpackage #:multihash
  (:use #:cl
        #:multihash.core
        #:multihash.hashing
        #:multihash.util)
  (:export #:multihash
           #:simple-multihash
           ;; accessors
           #:octets
           #:hex-string
           #:b58-string
           ;; readers
           #:hash-code
           #:hash-name
           #:hash-length
           #:digest
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

;;; tl;dr: A multihash is a hash digest with 5 bytes of metadata prepended

(defgeneric octets (object)
  (:documentation "Returns the octets of the multihash."))
(defgeneric (setf octets) (object octets))

(defgeneric hex-string (object)
  (:documentation "Returns a hex-string representation of the multihash."))
(defgeneric (setf hex-string) (object string))

(defgeneric b58-string (object)
  (:documentation "Returns a base-58 string representation of the multihash.")
  )
(defgeneric (setf b58-string) (object string))

(defgeneric hash-code (object)
  (:documentation "Returns the digest algorithm code of the multihash."))
(defgeneric hash-name (object)
  (:documentation "Returns the digest algorithm name of the multihash."))
(defgeneric hash-length (object)
  (:documentation "Returns the digest length of the multihash."))
(defgeneric digest (object)
  (:documentation "Return the digest of the multihash."))

;;;

(defclass multihash () ())

(defclass simple-multihash (multihash)
  ((%decoded
     :initform nil
     :accessor decoded
     :type decoded-multihash)
   (%octets
     :initarg :octets
     :accessor octets
     :type multihash-octets)))

(defmethod print-object ((object simple-multihash) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (b58-string object))))

(defmethod (setf octets) :after ((object simple-multihash) octets)
  (setf (decoded object)
        (decode (octets object))))

(defmethod hex-string ((object simple-multihash))
  (to-hex-string (octets object)))

(defmethod (setf hex-string) ((object simple-multihash) (hex-string string))
  (setf (octets object)
        (from-hex-string hex-string)))

(defmethod (setf b58-string) :after ((object simple-multihash) hex-string)
  (setf (decoded object)
        (decode (octets object))))

(defmethod b58-string ((object simple-multihash))
  (to-base58 (octets object)))

(defmethod (setf b58-string) ((object simple-multihash) (b58-string string))
  (setf (octets object)
        (from-base58 b58-string)))

(defmethod (setf b58-string) :after ((object simple-multihash) b58-string)
  (setf (decoded object)
        (decode (octets object))))

(defmethod hash-code ((object simple-multihash))
  (decoded-multihash-code (decoded object)))

(defmethod hash-name ((object simple-multihash))
  (decoded-multihash-name (decoded object)))

(defmethod hash-length ((object simple-multihash))
  (decoded-multihash-length (decoded object)))

(defmethod digest ((object simple-multihash))
  (decoded-multihash-digest (decoded object)))

(defmethod hash-code :before ((object simple-multihash))
  (when (null (decoded object))
    (setf (decoded object)
          (decode (octets object)))))

(defmethod hash-name :before ((object simple-multihash))
  (when (null (decoded object))
    (setf (decoded object)
          (decode (octets object)))))

(defmethod hash-length :before ((object simple-multihash))
  (when (null (decoded object))
    (setf (decoded object)
          (decode (octets object)))))

(defmethod digest :before ((object simple-multihash))
  (when (null (decoded object))
    (setf (decoded object)
          (decode (octets object)))))

;;;

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
  (make-instance 'simple-multihash
                 :octets (call-next-method)))
