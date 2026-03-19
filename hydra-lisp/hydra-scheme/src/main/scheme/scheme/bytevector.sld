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
    ;; Portable float32 snap: round to 7 significant digits (24-bit mantissa)
    (define (snap-to-float32 x)
      (if (= x 0.0) 0.0
          (let* ((e (exact (floor (/ (log (abs x)) (log 10)))))
                 (scale (expt 10 (- 7 e 1)))
                 (rounded (/ (round (* (inexact x) scale)) scale)))
            (inexact rounded))))))
