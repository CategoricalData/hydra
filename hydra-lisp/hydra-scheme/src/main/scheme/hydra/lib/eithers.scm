(define-library (hydra lib eithers)
  (import (scheme base)
          (hydra lib equality))
  (export hydra_lib_eithers_bimap
          hydra_lib_eithers_bind
          hydra_lib_eithers_either
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
          hydra_lib_eithers_foldl
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

    ;; bimap :: (a -> c) -> (b -> d) -> Either a b -> Either c d
    (define hydra_lib_eithers_bimap
      (lambda (f)
        (lambda (g)
          (lambda (e)
            (if (eq? (either-tag e) 'left)
                (list 'left (f (either-val e)))
                (list 'right (g (either-val e))))))))

    ;; bind :: Either a b -> (b -> Either a c) -> Either a c
    (define hydra_lib_eithers_bind
      (lambda (e)
        (lambda (f)
          (if (eq? (either-tag e) 'left)
              e
              (f (either-val e))))))

    ;; either :: (a -> c) -> (b -> c) -> Either a b -> c
    (define hydra_lib_eithers_either
      (lambda (f)
        (lambda (g)
          (lambda (e)
            (if (eq? (either-tag e) 'left)
                (f (either-val e))
                (g (either-val e)))))))

    ;; fromLeft :: a -> Either a b -> a
    (define hydra_lib_eithers_from_left
      (lambda (def)
        (lambda (e)
          (if (eq? (either-tag e) 'left)
              (either-val e)
              def))))

    ;; fromRight :: b -> Either a b -> b
    (define hydra_lib_eithers_from_right
      (lambda (def)
        (lambda (e)
          (if (eq? (either-tag e) 'right)
              (either-val e)
              def))))

    ;; isLeft :: Either a b -> Bool
    (define hydra_lib_eithers_is_left
      (lambda (e)
        (eq? (either-tag e) 'left)))

    ;; isRight :: Either a b -> Bool
    (define hydra_lib_eithers_is_right
      (lambda (e)
        (eq? (either-tag e) 'right)))

    ;; lefts :: [Either a b] -> [a]
    (define hydra_lib_eithers_lefts
      (lambda (es)
        (let loop ((rest es) (acc '()))
          (if (null? rest)
              (reverse acc)
              (if (eq? (either-tag (car rest)) 'left)
                  (loop (cdr rest) (cons (either-val (car rest)) acc))
                  (loop (cdr rest) acc))))))

    ;; map :: (b -> c) -> Either a b -> Either a c
    (define hydra_lib_eithers_map
      (lambda (f)
        (lambda (e)
          (if (eq? (either-tag e) 'left)
              e
              (list 'right (f (either-val e)))))))

    ;; map_list :: (a -> Either e b) -> [a] -> Either e [b]
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

    ;; map_maybe :: (a -> Either e b) -> Maybe a -> Either e (Maybe b)
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

    ;; map_set :: (a -> Either e b) -> Set a -> Either e (Set b)
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

    ;; partitionEithers :: [Either a b] -> Pair [a] [b]
    (define hydra_lib_eithers_partition_eithers
      (lambda (es)
        (let loop ((rest es) (lefts '()) (rights '()))
          (if (null? rest)
              (list (reverse lefts) (reverse rights))
              (let ((e (car rest)))
                (if (eq? (either-tag e) 'left)
                    (loop (cdr rest) (cons (either-val e) lefts) rights)
                    (loop (cdr rest) lefts (cons (either-val e) rights))))))))

    ;; foldl :: (a -> b -> Either c a) -> a -> [b] -> Either c a
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

    ;; rights :: [Either a b] -> [b]
    (define hydra_lib_eithers_rights
      (lambda (es)
        (let loop ((rest es) (acc '()))
          (if (null? rest)
              (reverse acc)
              (if (eq? (either-tag (car rest)) 'right)
                  (loop (cdr rest) (cons (either-val (car rest)) acc))
                  (loop (cdr rest) acc))))))))
