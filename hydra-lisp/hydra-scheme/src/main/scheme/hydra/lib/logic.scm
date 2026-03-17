(define-library (hydra lib logic)
  (import (scheme base))
  (export hydra_lib_logic_and
          hydra_lib_logic_if_else
          hydra_lib_logic_not
          hydra_lib_logic_or)
  (begin

    ;; and :: Bool -> Bool -> Bool
    (define hydra_lib_logic_and
      (lambda (a)
        (lambda (b)
          (and a b))))

    ;; if_else :: Bool -> a -> a -> a
    (define hydra_lib_logic_if_else
      (lambda (cond)
        (lambda (then-val)
          (lambda (else-val)
            (if cond then-val else-val)))))

    ;; not :: Bool -> Bool
    (define hydra_lib_logic_not
      (lambda (x)
        (not x)))

    ;; or :: Bool -> Bool -> Bool
    (define hydra_lib_logic_or
      (lambda (a)
        (lambda (b)
          (or a b))))))
