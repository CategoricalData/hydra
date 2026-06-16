;; Compatibility shim: (scheme bytevector) for implementations that lack it.
;; Re-exports from (rnrs bytevectors) which Guile provides.
(define-library (scheme bytevector)
  (import (rnrs bytevectors))
  (export bytevector? bytevector-length bytevector-u8-ref
          bytevector-u8-set! make-bytevector bytevector-copy
          bytevector-copy! bytevector-append
          snap-to-float32
          utf8->string string->utf8)
  (begin
    ;; Ensure rnrs bytevectors is available (the bootstrap loader strips
    ;; define-library and its import clause, so we need this explicitly).
    (use-modules (rnrs bytevectors))

    ;; Round-trip a double through IEEE 754 float32 using bytevector operations.
    ;; This gives the exact float32 representation, not a decimal approximation.
    (define (snap-to-float32 x)
      (if (= x 0.0) 0.0
          (let ((bv (make-bytevector 4)))
            (bytevector-ieee-single-set! bv 0 x (endianness little))
            (bytevector-ieee-single-ref bv 0 (endianness little)))))))
