(define-library (hydra lib pairs)
  (import (scheme base))
  (export hydra_lib_pairs_bimap
          hydra_lib_pairs_first
          hydra_lib_pairs_second)
  (begin

    ;; Pairs are 2-element lists: (list a b)

    ;; bimap :: (a -> c) -> (b -> d) -> Pair a b -> Pair c d
    (define hydra_lib_pairs_bimap
      (lambda (f)
        (lambda (g)
          (lambda (p)
            (list (f (car p)) (g (cadr p)))))))

    ;; first :: Pair a b -> a
    (define hydra_lib_pairs_first
      (lambda (p)
        (car p)))

    ;; second :: Pair a b -> b
    (define hydra_lib_pairs_second
      (lambda (p)
        (cadr p)))))
