# cl-multihash

[![Build Status](https://api.travis-ci.org/wemeetagain/cl-multihash.svg?branch=master)](https://travis-ci.org/wemeetagain/cl-multihash)
[![Coverage Status](https://coveralls.io/repos/wemeetagain/cl-multihash/badge.svg?branch=master&service=github)](https://coveralls.io/github/wemeetagain/cl-multihash?branch=master)

[Multihash](https://github.com/jbenet/multihash) utility for Common Lisp

## Examples

```lisp
;; multihash something

(multihash:multihash-object :sha256 #P"quicklisp.lisp")
;; => #<MULTIHASH:MULTIHASH "QmTMP6TqrfpSAeo4nbZ4BSRkcHZDs4hUwnbWH6hMJiikQf">

(defclass person ()
  ((name :initarg :name :accessor name)))

(defmethod multihash:%to-octets ((p person))
    (babel:string-to-octets (name p)))

(multihash:multihash-object (make-instance 'person :name "Juan"))

;; => #<MULTIHASH:MULTIHASH "Qmc33LKvyxLP2F164KjKpvT62J4RJrn4LMF4NcqH971JFH">

```

## Package Index

### `MULTIHASH`

#### [class] `MULTIHASH`

#### [generic function] `HASH-NAME (OBJECT)`

    Returns the hash algorithm name of the multihash.

#### [generic function] `DIGEST (OBJECT)`

    Return the digest of the multihash.

#### [generic function] `OCTETS (OBJECT)`

    Returns the multihash as a (SIMPLE-ARRAY (UNSIGNED-BYTE 8) *).

#### [generic function] `HEX-STRING (OBJECT)`

    Returns a hex-string representation of the multihash.

#### [generic function] `B58-STRING (OBJECT)`

    Returns a base-58 string representation of the multihash.

#### [generic function] `MULTIHASH-OBJECT (HASH-NAME OBJECT)`

    Returns a multihash of OBJECT.

#### [generic function] `%TO-OCTETS (OBJECT)`

    Returns a representation of the object as (SIMPLE-ARRAY (OCTET 8) *)

### `MULTIHASH.DEFINITIONS`

#### [special] `\*DEFINITIONS\*`

    List of supported multihash definitions

#### [structure] `DEFINITION`

    A multihash definition, a single hash algorithm.

#### [function] `MAKE-DEFINITION (&KEY NAME CODE LENGTH)`

#### [function] `DEFINITION-CODE (INSTANCE)`

    Returns the multihash-allocated code of the hash algorithm.

#### [function] `DEFINITION-NAME (INSTANCE)`

    Returns the name of the hash algorithm.

#### [function] `DEFINITION-LENGTH (INSTANCE)`

    Returns the multihash-allocated length of digest of the hash algorithm.

#### [function] `APP-CODE-P (CODE)`

    Checks whether a multihash code is part of the valid app range.

#### [function] `VALID-CODE-P (CODE)`

    Checks whether a multihash code is valid.

### `MULTIHASH.CORE`

#### [function] `MULTIHASH-OCTETS-P (SEQUENCE)`

    Return T if SEQUENCE is a valid multihash octet array, otherwise return NIL.
    
    SEQUENCE must be a (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*))

#### [function] `%CODE (MHASH-OCTETS)`

#### [function] `%NAME (MHASH-OCTETS)`

#### [function] `%LENGTH (MHASH-OCTETS)`

#### [function] `%DIGEST (MHASH-OCTETS)`

#### [function] `ENCODE (DIGEST-NAME SEQUENCE)`

    Encode a hash digest along with the specified function code. Note: the
    length is derived from SEQUENCE, rather than by the multihash definition.
    
    SEQUENCE must be a (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*))

#### [function] `DECODE (SEQUENCE)`

    Decode a hash from a given multihash. Returns values:
    code, name, length, digest
    or errors.
    
    SEQUENCE must be a (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*))

### `MULTIHASH.HASHING`

#### [function] `MULTIHASH-SEQUENCE (DIGEST-NAME SEQUENCE)`

    Return the multihash of the subsequence of SEQUENCE
    specified by START and END using the algorithm DIGEST-NAME.  For CMUCL
    and SBCL, SEQUENCE can be any vector with an element-type
    of (UNSIGNED-BYTE 8); for other implementations, SEQUENCE must be a
    (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*)).

#### [function] `MULTIHASH-FILE (DIGEST-NAME PATHNAME)`

    Return the multihash of the contents of the file named by PATHNAME using
    the algorithm DIGEST-NAME.

#### [function] `MULTIHASH-STREAM (DIGEST-NAME STREAM)`

    Return the multihash of the contents of STREAM using the algorithm
    DIGEST-NAME.  STREAM-ELEMENT-TYPE of STREAM should be (UNSIGNED-BYTE 8).
