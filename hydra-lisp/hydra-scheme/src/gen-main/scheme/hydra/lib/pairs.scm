(define-library (hydra lib pairs)
  (import (scheme base))
  (export hydra_lib_pairs_bimap
          hydra_lib_pairs_first
          hydra_lib_pairs_second)
  (begin

    ;; Pairs are 2-element lists: (list a b)

    ;; Map over both elements of a pair.
    (define hydra_lib_pairs_bimap
      (lambda (f)
        (lambda (g)
          (lambda (p)
            (list (f (car p)) (g (cadr p)))))))

    ;; Get the first element of a pair.
    (define hydra_lib_pairs_first
      (lambda (p)
        (car p)))

    ;; Get the second element of a pair.
    (define hydra_lib_pairs_second
      (lambda (p)
        (cadr p)))))
