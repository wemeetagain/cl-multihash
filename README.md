# cl-multihash

[![Build Status](https://api.travis-ci.org/WeMeetAgain/cl-multihash.svg?branch=master)](https://travis-ci.org/WeMeetAgain/cl-multihash)
[![Coverage Status](https://coveralls.io/repos/WeMeetAgain/cl-multihash/badge.svg?branch=master&service=github)](https://coveralls.io/github/WeMeetAgain/cl-multihash?branch=master)

[Multihash](https://github.com/jbenet/multihash) utility for Common Lisp

## Examples

```lisp
;; return the multihash of a sequence

(multihash:multihash-sequence 'multihash:sha256 (babel:string-to-octets "hello world"))

;; => #(18 32 185 77 39 185 147 77 62 8 165 46 82 215 218 125 171 250 196 132
;; 239 227 122 83 128 238 144 136 247 172 226 239 205 233)

;; return the multihash of a file

(multihash:multihash-file 'multihash:sha256 "test.txt")

;; => #(18 32 170 220 25 85 192 48 247 35 233 216 158 217 212 134 180 238 245
;; 176 209 198 148 91 224 221 107 123 52 13 66 146 142 201)

;; decode multihash into component parts

(multihash:decode (multihash:from-hex-string "11140beec7b5ea3f0fdbc95d0dd47f3c5bc275da8a33"))

;; => #S(CL-MULTIHASH::DECODED-MULTIHASH
;;   :CODE 17
;;   :NAME IRONCLAD:SHA1
;;   :LENGTH 20
;;   :DIGEST #(11 238 199 181 234 63 15 219 201 93 13 212 127 60 91 194 117 218
;;             138 51))

```

## [Documentation](https://wemeetagain.github.io/cl-multihash/overview.html)

## Testing

To run unit tests, run the following command:

```lisp
(asdf:test-system :cl-multihash)
```

## License

MIT
