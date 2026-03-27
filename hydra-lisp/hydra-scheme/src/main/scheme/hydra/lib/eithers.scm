(define-library (hydra lib eithers)
  (import (scheme base)
          (hydra lib equality))
  (export hydra_lib_eithers_bimap
          hydra_lib_eithers_bind
          hydra_lib_eithers_either
          hydra_lib_eithers_foldl
          hydra_lib_eithers_from_left
          hydra_lib_eithers_from_right
          hydra_lib_eithers_is_left
          hydra_lib_eithers_is_right
          hydra_lib_eithers_lefts
          hydra_lib_eithers_map
          hydra_lib_eithers_map_list
          hydra_lib_eithers_map_maybe
          hydra_lib_eithers_map_set
          hydra_lib_eithers_partition_eithers
          hydra_lib_eithers_rights)
  (begin

    ;; Either representation: (list 'left val) or (list 'right val)

    (define (either-tag e) (car e))
    (define (either-val e) (cadr e))

    (define (set-insert x s)
      (cond
        ((null? s) (list x))
        ((= (generic-compare x (car s)) 0) s)
        ((< (generic-compare x (car s)) 0) (cons x s))
        (else (cons (car s) (set-insert x (cdr s))))))

    ;; Map over both sides of an Either value.
    (define hydra_lib_eithers_bimap
      (lambda (f)
        (lambda (g)
          (lambda (e)
            (if (eq? (either-tag e) 'left)
                (list 'left (f (either-val e)))
                (list 'right (g (either-val e))))))))

    ;; Bind (flatMap) for Either: if Right, apply the function; if Left, return unchanged.
    (define hydra_lib_eithers_bind
      (lambda (e)
        (lambda (f)
          (if (eq? (either-tag e) 'left)
              e
              (f (either-val e))))))

    ;; Eliminate an Either value by applying one of two functions.
    (define hydra_lib_eithers_either
      (lambda (f)
        (lambda (g)
          (lambda (e)
            (if (eq? (either-tag e) 'left)
                (f (either-val e))
                (g (either-val e)))))))

    ;; Left-fold over a list with an Either-returning function, short-circuiting on Left.
    (define hydra_lib_eithers_foldl
      (lambda (f)
        (lambda (init)
          (lambda (xs)
            (let loop ((rest xs) (acc init))
              (if (null? rest)
                  (list 'right acc)
                  (let ((result ((f acc) (car rest))))
                    (if (eq? (either-tag result) 'left)
                        result
                        (loop (cdr rest) (either-val result))))))))))

    ;; Extract the Left value, or return a default.
    ;; Thunk-aware: if def is a zero-arg procedure (thunk), only called when Either is Right
    (define hydra_lib_eithers_from_left
      (lambda (def)
        (lambda (e)
          (if (eq? (either-tag e) 'left)
              (either-val e)
              (if (procedure? def) (def) def)))))

    ;; Extract the Right value, or return a default.
    ;; Thunk-aware: if def is a zero-arg procedure (thunk), only called when Either is Left
    (define hydra_lib_eithers_from_right
      (lambda (def)
        (lambda (e)
          (if (eq? (either-tag e) 'right)
              (either-val e)
              (if (procedure? def) (def) def)))))

    ;; Check if an Either is a Left value.
    (define hydra_lib_eithers_is_left
      (lambda (e)
        (eq? (either-tag e) 'left)))

    ;; Check if an Either is a Right value.
    (define hydra_lib_eithers_is_right
      (lambda (e)
        (eq? (either-tag e) 'right)))

    ;; Extract all Left values from a list of Eithers.
    (define hydra_lib_eithers_lefts
      (lambda (es)
        (let loop ((rest es) (acc '()))
          (if (null? rest)
              (reverse acc)
              (if (eq? (either-tag (car rest)) 'left)
                  (loop (cdr rest) (cons (either-val (car rest)) acc))
                  (loop (cdr rest) acc))))))

    ;; Map a function over the Right side of an Either (standard functor map).
    (define hydra_lib_eithers_map
      (lambda (f)
        (lambda (e)
          (if (eq? (either-tag e) 'left)
              e
              (list 'right (f (either-val e)))))))

    ;; Map a function returning Either over a list, collecting results or short-circuiting on Left.
    (define hydra_lib_eithers_map_list
      (lambda (f)
        (lambda (xs)
          (let loop ((rest xs) (acc '()))
            (if (null? rest)
                (list 'right (reverse acc))
                (let ((result (f (car rest))))
                  (if (eq? (either-tag result) 'left)
                      result
                      (loop (cdr rest) (cons (either-val result) acc)))))))))

    ;; Map a function returning Either over a Maybe, or return Right Nothing if Nothing.
    (define hydra_lib_eithers_map_maybe
      (lambda (f)
        (lambda (m)
          (if (or (null? m) (and (pair? m) (eq? (car m) 'nothing)))
              (list 'right (list 'nothing))
              (let ((val (if (and (pair? m) (or (eq? (car m) 'just) (eq? (car m) 'maybe)))
                             (cadr m) m)))
                (let ((result (f val)))
                  (if (eq? (either-tag result) 'left)
                      result
                      (list 'right (list 'just (either-val result))))))))))

    ;; Map a function returning Either over a Set, collecting results or short-circuiting on Left.
    (define hydra_lib_eithers_map_set
      (lambda (f)
        (lambda (s)
          (let loop ((rest s) (acc '()))
            (if (null? rest)
                (list 'right (let sort-loop ((remaining (reverse acc)) (result '()))
                               (if (null? remaining)
                                   result
                                   (sort-loop (cdr remaining) (set-insert (car remaining) result)))))
                (let ((result (f (car rest))))
                  (if (eq? (either-tag result) 'left)
                      result
                      (loop (cdr rest) (cons (either-val result) acc)))))))))

    ;; Partition a list of Eithers into lefts and rights.
    (define hydra_lib_eithers_partition_eithers
      (lambda (es)
        (let loop ((rest es) (lefts '()) (rights '()))
          (if (null? rest)
              (list (reverse lefts) (reverse rights))
              (let ((e (car rest)))
                (if (eq? (either-tag e) 'left)
                    (loop (cdr rest) (cons (either-val e) lefts) rights)
                    (loop (cdr rest) lefts (cons (either-val e) rights))))))))

    ;; Extract all Right values from a list of Eithers.
    (define hydra_lib_eithers_rights
      (lambda (es)
        (let loop ((rest es) (acc '()))
          (if (null? rest)
              (reverse acc)
              (if (eq? (either-tag (car rest)) 'right)
                  (loop (cdr rest) (cons (either-val (car rest)) acc))
                  (loop (cdr rest) acc))))))))
