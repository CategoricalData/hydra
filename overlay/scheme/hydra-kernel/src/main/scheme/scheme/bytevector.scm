;; Guile compatibility shim: (scheme bytevector)
;; Guile doesn't provide this R7RS module natively; re-export from (rnrs bytevectors).
(define-module (scheme bytevector)
  #:use-module (rnrs bytevectors)
  #:use-module ((rnrs bytevectors) #:select (bytevector-ieee-single-set!
                                              bytevector-ieee-single-ref
                                              endianness))
  #:re-export (bytevector? bytevector-length bytevector-u8-ref
               bytevector-u8-set! make-bytevector bytevector-copy
               bytevector-copy!
               utf8->string string->utf8)
  #:export (snap-to-float32))

(define (snap-to-float32 x)
  "Round-trip a double through IEEE 754 float32."
  (if (zero? x) 0.0
      (let ((bv (make-bytevector 4)))
        (bytevector-ieee-single-set! bv 0 x (endianness little))
        (bytevector-ieee-single-ref bv 0 (endianness little)))))
