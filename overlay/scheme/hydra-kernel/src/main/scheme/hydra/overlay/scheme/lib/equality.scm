(define-library (hydra overlay scheme lib equality)
  (import (scheme base))
  (export hydra_lib_equality_equal)
  (begin

    ;; Check if two values are equal.
    (define hydra_lib_equality_equal
      (lambda (a)
        (lambda (b)
          (equal? a b))))))
