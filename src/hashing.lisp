;;;; -*- mode: lisp; indent-tabs-mode: nil -*-

(cl:in-package #:cl-user)
(defpackage #:multihash.hashing
  (:use #:cl #:multihash.core)
  (:import-from #:ironclad
                #:digest-file
                #:digest-stream
                #:digest-sequence
                #:sha1 #:sha256 #:sha512)
  (:export
    #:multihash-file
    #:multihash-stream
    #:multihash-sequence))
(in-package #:multihash.hashing)

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
