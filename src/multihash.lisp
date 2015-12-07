(cl:in-package #:cl-user)

(defpackage #:multihash
  (:use #:cl
        #:multihash.core
        #:multihash.hashing
        #:multihash.octets)
  (:export #:multihash
           #:simple-multihash
           ;; accessors
           #:multihash-octets
           #:multihash-hex-string
           #:multihash-b58-string
           ;; readers
           #:multihash-hash-code
           #:multihash-hash-name
           #:multihash-hash-length
           #:multihash-digest
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

(defgeneric multihash-octets (object))
(defgeneric (setf multihash-octets) (object octets))

(defgeneric multihash-hex-string (object))
(defgeneric (setf multihash-hex-string) (object string))

(defgeneric multihash-b58-string (object))
(defgeneric (setf multihash-b58-string) (object string))

(defgeneric multihash-hash-code (object))
(defgeneric multihash-hash-name (object))
(defgeneric multihash-hash-length (object))
(defgeneric multihash-digest (object))

;;;

(defclass multihash () ())

(defclass simple-multihash (multihash)
  ((%decoded
     :initform nil
     :accessor multihash-decoded
     :type decoded-multihash)
   (%octets
     :initarg :octets
     :accessor multihash-octets
     :type multihash-octets)))

(defmethod print-object ((object simple-multihash) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (multihash-b58-string object))))

(defmethod (setf multihash-octets) :after ((object simple-multihash) octets)
  (setf (multihash-decode object) (decode (multihash-octets))))

(defmethod multihash-hex-string ((object simple-multihash))
  (to-hex-string (multihash-octets object)))

(defmethod (setf multihash-hex-string) ((object simple-multihash) (hex-string string))
  (setf (multihash-octets object) (from-hex-string hex-string)))

(defmethod (setf multihash-b58-string) :after ((object simple-multihash) hex-string)
  (setf (multihash-decode object) (decode (multihash-octets))))

(defmethod multihash-b58-string ((object simple-multihash))
  (to-base58 (multihash-octets object)))

(defmethod (setf multihash-b58-string) ((object simple-multihash) (b58-string string))
  (setf (multihash-octets object) (from-base58 b58-string)))

(defmethod (setf multihash-b58-string) :after ((object simple-multihash) b58-string)
  (setf (multihash-decode object) (decode (multihash-octets))))

(defmethod multihash-hash-code ((object simple-multihash))
  (decoded-multihash-code (multihash-decoded object)))

(defmethod multihash-hash-name ((object simple-multihash))
  (decoded-multihash-name (multihash-decoded object)))

(defmethod multihash-hash-length ((object simple-multihash))
  (decoded-multihash-length (multihash-decoded object)))

(defmethod multihash-digest ((object simple-multihash))
  (decoded-multihash-digest (multihash-decoded object)))

(defmethod multihash-hash-code :before ((object simple-multihash))
  (when (null (multihash-decoded object))
    (setf (multihash-decoded object) (decode (multihash-octets object)))))

(defmethod multihash-hash-name :before ((object simple-multihash))
  (when (null (multihash-decoded object))
    (setf (multihash-decoded object) (decode (multihash-octets object)))))

(defmethod multihash-hash-length :before ((object simple-multihash))
  (when (null (multihash-decoded object))
    (setf (multihash-decoded object) (decode (multihash-octets object)))))

(defmethod multihash-digest :before ((object simple-multihash))
  (when (null (multihash-decoded object))
    (setf (multihash-decoded object) (decode (multihash-octets object)))))

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
