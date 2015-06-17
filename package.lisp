;;;; package.lisp

(defpackage #:cl-multihash
  (:nicknames :multihash)
  (:use #:cl #:ironclad)
  (:shadow null)
  (:export
   #:*multihash-definitions*
   ;; hash name symbols
   #:sha1 #:sha256 #:sha512 #:sha3 #:blake2b #:blake2s
   ;; decoded-multihash slot accessors
   #:decoded-multihash-code #:decoded-multihash-name
   #:decoded-multihash-length #:decoded-multihash-digest
   ;; core functions
   #:encode #:decode
   ;; high level functions
   #:multihash-file #:multihash-stream #:multihash-sequence
   ;; utility functions
   #:app-code-p #:valid-code-p))
