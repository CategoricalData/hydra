;; Guile compatibility shim: (scheme bytevector)
;; Guile doesn't provide this R7RS module natively; re-export from (rnrs bytevectors).
(define-module (scheme bytevector)
  #:use-module (rnrs bytevectors)
  #:re-export (bytevector? bytevector-length bytevector-u8-ref
               bytevector-u8-set! make-bytevector bytevector-copy
               bytevector-copy! bytevector-append
               utf8->string string->utf8))
