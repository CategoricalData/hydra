(define-library (hydra overlay scheme lib equality)
  (import (scheme base))
  (export hydra_overlay_scheme_lib_equality_equal)
  (begin

    ;; Check if two values are equal.
    (define hydra_overlay_scheme_lib_equality_equal
      (lambda (a)
        (lambda (b)
          (equal? a b))))))
