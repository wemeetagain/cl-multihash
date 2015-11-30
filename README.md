# cl-multihash

[![Build Status](https://api.travis-ci.org/WeMeetAgain/cl-multihash.svg?branch=master)](https://travis-ci.org/WeMeetAgain/cl-multihash)
[![Coverage Status](https://coveralls.io/repos/WeMeetAgain/cl-multihash/badge.svg?branch=master&service=github)](https://coveralls.io/github/WeMeetAgain/cl-multihash?branch=master)

[Multihash](https://github.com/jbenet/multihash) utility for Common Lisp

## Examples

```lisp
;; multihash something
;; uses sha256, outputs as (SIMPLE-ARRAY (UNSIGNED-BYTE 8) *) by default

(multihash:multihash "lolol")

;; => #(18 32 175 205 122 202 160 121 95 158 35 104 99 157 232 184 153 178 239
;; 186 89 25 87 25 241 68 228 215 255 22 13 147 233 250)

;; multihash something using different digest names and output formats

(multihash:multihash "lolol" :digest-name :sha1 :output :base58)

;; => "5dr9AEn4Vf2yxsf4nCoBvZPUg3TpdV"

;; extend for your own classes

(defclass person ()
  ((name :initarg :name :accessor person-name)))
(defmethod multihash:%to-octets ((p person))
    (multihash:%to-octets (format nil "person ~S" (person-name p))))
(multihash:multihash (make-instance 'person :name "Cran"))

;; => #(18 32 21 24 253 11 89 89 88 250 54 127 195 194 58 224 222 234 24 229 47
;; 87 60 247 52 194 189 36 217 59 18 16 102 103)

;; decode multihash into component parts

(multihash:decode (multihash:from-hex-string "11140beec7b5ea3f0fdbc95d0dd47f3c5bc275da8a33"))

;; => #S(CL-MULTIHASH::DECODED-MULTIHASH
;;   :CODE 17
;;   :NAME IRONCLAD:SHA1
;;   :LENGTH 20
;;   :DIGEST #(11 238 199 181 234 63 15 219 201 93 13 212 127 60 91 194 117 218
;;             138 51))

;; return the multihash of a (SIMPLE-ARRAY (UNSIGNED-BYTE 8) *)

(multihash:multihash-sequence :sha256 (babel:string-to-octets "hello world"))

;; => #(18 32 185 77 39 185 147 77 62 8 165 46 82 215 218 125 171 250 196 132
;; 239 227 122 83 128 238 144 136 247 172 226 239 205 233)

;; return the multihash of a file

(multihash:multihash-file :sha256 "test.txt")

;; => #(18 32 170 220 25 85 192 48 247 35 233 216 158 217 212 134 180 238 245
;; 176 209 198 148 91 224 221 107 123 52 13 66 146 142 201)

```

## Package Index

### [CL-MULTIHASH](#CL-MULTIHASH) (MULTIHASH)

#### [special] [\***MULTIHASH-DEFINITIONS**\*](#CL-MULTIHASH:*MULTIHASH-DEFINITIONS*)

    List of supported multihash definitions

#### [generic function] [**MULTIHASH-OBJECT**](#CL-MULTIHASH:MULTIHASH-OBJECT) (DIGEST OBJECT)

    Returns a multihash of OBJECT

#### [generic function] [**%TO-OCTETS**](#CL-MULTIHASH:%TO-OCTETS) (OBJECT)

    Returns a representation of the object as (SIMPLE-ARRAY (OCTET 8) *)

#### [structure] [**DECODED-MULTIHASH**](#CL-MULTIHASH:DECODED-MULTIHASH)

    A multihash deconstructed into its parts.

#### [accessor] [**DECODED-MULTIHASH-CODE**](#CL-MULTIHASH:DECODED-MULTIHASH-CODE) (INSTANCE)

#### [accessor] [**DECODED-MULTIHASH-NAME**](#CL-MULTIHASH:DECODED-MULTIHASH-NAME) (INSTANCE)

#### [accessor] [**DECODED-MULTIHASH-LENGTH**](#CL-MULTIHASH:DECODED-MULTIHASH-LENGTH) (INSTANCE)

#### [accessor] [**DECODED-MULTIHASH-DIGEST**](#CL-MULTIHASH:DECODED-MULTIHASH-DIGEST) (INSTANCE)

#### [function] [**MULTIHASH**](#CL-MULTIHASH:MULTIHASH) (OBJECT &KEY (DIGEST-NAME :SHA256) (OUTPUT :OCTETS))

    Returns the multihash of OBJECT, using DIGEST-NAME as the hash function.
    
    The output format can be set with OUTPUT.
    
    Output can be one of the following: OCTETS, :BASE58, or :HEX

#### [function] [**MULTIHASH-P**](#CL-MULTIHASH:MULTIHASH-P) (SEQUENCE)

    Return T if SEQUENCE is a valid multihash, otherwise, return NIL.
    
    SEQUENCE must be a (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*))

#### [function] [**MULTIHASH-STREAM**](#CL-MULTIHASH:MULTIHASH-STREAM) (DIGEST-NAME STREAM)

    Return the multihash of the contents of STREAM using the algorithm
    DIGEST-NAME.  STREAM-ELEMENT-TYPE of STREAM should be (UNSIGNED-BYTE 8).

#### [function] [**MULTIHASH-FILE**](#CL-MULTIHASH:MULTIHASH-FILE) (DIGEST-NAME PATHNAME)

    Return the multihash of the contents of the file named by PATHNAME using
    the algorithm DIGEST-NAME.

#### [function] [**MULTIHASH-SEQUENCE**](#CL-MULTIHASH:MULTIHASH-SEQUENCE) (DIGEST-NAME SEQUENCE)

    Return the multihash of the subsequence of SEQUENCE
    specified by START and END using the algorithm DIGEST-NAME.  For CMUCL
    and SBCL, SEQUENCE can be any vector with an element-type
    of (UNSIGNED-BYTE 8); for other implementations, SEQUENCE must be a
    (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*)).

#### [function] [**FROM-BASE58**](#CL-MULTIHASH:FROM-BASE58) (STRING)

    Decode a base58-encoded string into a multihash.

#### [function] [**TO-BASE58**](#CL-MULTIHASH:TO-BASE58) (OCTETS)

    Encode a multihash to a base58-encoded string.

#### [function] [**FROM-HEX-STRING**](#CL-MULTIHASH:FROM-HEX-STRING) (STRING)

    Decode a hex string into a multihash.

#### [function] [**TO-HEX-STRING**](#CL-MULTIHASH:TO-HEX-STRING) (OCTETS)

    Encode a multihash to a hex string.

#### [function] [**ENCODE**](#CL-MULTIHASH:ENCODE) (DIGEST-NAME SEQUENCE)

    Encode a hash digest along with the specified function code. Note: the
    length is derived from SEQUENCE, rather than by the multihash definition.
    
    SEQUENCE must be a (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*))

#### [function] [**DECODE**](#CL-MULTIHASH:DECODE) (SEQUENCE)

    Decode a hash from a given multihash. Returns a DECODED-MULTIHASH struct or
    errors.
    
    SEQUENCE must be a (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*))

#### [function] [**VALID-CODE-P**](#CL-MULTIHASH:VALID-CODE-P) (CODE)

    Checks whether a multihash code is valid.

#### [function] [**APP-CODE-P**](#CL-MULTIHASH:APP-CODE-P) (CODE)

    Checks whether a multihash code is part of the valid app range.

## Testing

To run unit tests, run the following command:

```lisp
(asdf:test-system :cl-multihash)
```

## License

MIT
