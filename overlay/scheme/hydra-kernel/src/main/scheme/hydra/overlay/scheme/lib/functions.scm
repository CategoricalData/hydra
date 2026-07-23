(define-library (hydra overlay scheme lib functions)
  (import (scheme base))
  (export hydra_lib_functions_identity)
  (begin

    ;; Return a value unchanged.
    (define hydra_lib_functions_identity
      (lambda (x) x))))
