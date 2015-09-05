;;;; package.lisp

(defpackage #:cl-multihash
  (:nicknames :multihash)
  (:use #:cl)
  (:import-from #:ironclad
		#:digest-file
		#:digest-stream
		#:digest-sequence
		#:unsupported-digest
		#:sha1 #:sha256 #:sha512)
  (:export
   #:*multihash-definitions*
   ;; hash name symbols
   #:sha1 #:sha256 #:sha512 #:sha3 #:blake2b #:blake2s
   ;; decoded-multihash slot accessors
   #:decoded-multihash
   #:decoded-multihash-code
   #:decoded-multihash-name
   #:decoded-multihash-length
   #:decoded-multihash-digest
   ;; core functions
   #:encode #:decode
   ;; high level functions
   #:multihash-file
   #:multihash-stream
   #:multihash-sequence
   ;; utility functions
   #:app-code-p
   #:valid-code-p
   #:multihash-p
   #:from-base58
   #:to-base58
   #:from-hex-string
   #:to-hex-string))
