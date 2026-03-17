;; Guile compatibility: (srfi srfi-151) via (srfi srfi-60)
(define-module (srfi srfi-151)
  #:use-module (srfi srfi-60)
  #:re-export (bitwise-and bitwise-ior bitwise-xor bitwise-not
               arithmetic-shift bit-count integer-length))
