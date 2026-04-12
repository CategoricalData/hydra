;; Compatibility shim: (srfi 151) for implementations that lack it.
;; Re-exports from (srfi 60) which most implementations provide.
(define-library (srfi 151)
  (import (srfi 60))
  (export bitwise-and bitwise-ior bitwise-xor bitwise-not
          arithmetic-shift bit-count integer-length))
