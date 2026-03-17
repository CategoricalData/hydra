;; Compatibility shim: (scheme bytevector) for implementations that lack it.
;; Re-exports from (rnrs bytevectors) which Guile provides.
(define-library (scheme bytevector)
  (import (rnrs bytevectors))
  (export bytevector? bytevector-length bytevector-u8-ref
          bytevector-u8-set! make-bytevector bytevector-copy
          bytevector-copy! bytevector-append
          utf8->string string->utf8))
