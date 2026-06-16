;; Guile-specific float32 helper using (rnrs bytevectors)
(define-module (hydra float32)
  #:use-module (rnrs bytevectors)
  #:export (snap-to-float32))

(define (snap-to-float32 x)
  "Round-trip a double through IEEE 754 float32."
  (if (zero? x) 0.0
      (let ((bv (make-bytevector 4)))
        (bytevector-ieee-single-set! bv 0 x (endianness little))
        (bytevector-ieee-single-ref bv 0 (endianness little)))))
