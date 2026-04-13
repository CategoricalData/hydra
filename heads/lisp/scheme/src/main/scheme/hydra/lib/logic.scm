(define-library (hydra lib logic)
  (import (scheme base))
  (export hydra_lib_logic_and
          hydra_lib_logic_if_else
          hydra_lib_logic_not
          hydra_lib_logic_or)
  (begin

    ;; Compute the logical AND of two boolean values.
    (define hydra_lib_logic_and
      (lambda (a)
        (lambda (b)
          (and a b))))

    ;; Compute a conditional expression.
    (define hydra_lib_logic_if_else
      (lambda (cond)
        (lambda (then-val)
          (lambda (else-val)
            (if cond then-val else-val)))))

    ;; Compute the logical NOT of a boolean value.
    (define hydra_lib_logic_not
      (lambda (x)
        (not x)))

    ;; Compute the logical OR of two boolean values.
    (define hydra_lib_logic_or
      (lambda (a)
        (lambda (b)
          (or a b))))))
