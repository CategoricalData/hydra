(define-library (hydra lib maybes)
  (import (scheme base))
  (export hydra_lib_maybes_apply
          hydra_lib_maybes_bind
          hydra_lib_maybes_cases
          hydra_lib_maybes_cat
          hydra_lib_maybes_compose
          hydra_lib_maybes_from_maybe
          hydra_lib_maybes_is_just
          hydra_lib_maybes_is_nothing
          hydra_lib_maybes_map
          hydra_lib_maybes_map_maybe
          hydra_lib_maybes_maybe
          hydra_lib_maybes_pure
          hydra_lib_maybes_to_list
          maybe-nothing?
          maybe-value)
  (begin

    ;; Maybe representation: (list 'just val) or (list 'nothing)
    ;; Also handles: bare value for Just, '() for Nothing, (maybe val)/(maybe '())

    (define (maybe-nothing? m)
      (or (null? m)
          (eq? m #f)
          (and (pair? m) (eq? (car m) 'nothing))))

    (define (maybe-value m)
      (cond
        ((and (pair? m) (eq? (car m) 'just)) (cadr m))
        (else m)))

    ;; apply :: Maybe (a -> b) -> Maybe a -> Maybe b
    (define hydra_lib_maybes_apply
      (lambda (mf)
        (lambda (mx)
          (if (maybe-nothing? mf)
              (list 'nothing)
              (if (maybe-nothing? mx)
                  (list 'nothing)
                  (list 'just ((maybe-value mf) (maybe-value mx))))))))

    ;; bind :: Maybe a -> (a -> Maybe b) -> Maybe b
    (define hydra_lib_maybes_bind
      (lambda (m)
        (lambda (f)
          (if (maybe-nothing? m)
              (list 'nothing)
              (f (maybe-value m))))))

    ;; cases :: Maybe a -> b -> (a -> b) -> b
    ;; Thunk-aware: if def is a zero-arg procedure (thunk), only called when Maybe is Nothing
    (define hydra_lib_maybes_cases
      (lambda (m)
        (lambda (def)
          (lambda (f)
            (if (maybe-nothing? m)
                (if (procedure? def) (def) def)
                (f (maybe-value m)))))))

    ;; cat :: [Maybe a] -> [a]
    (define hydra_lib_maybes_cat
      (lambda (ms)
        (let loop ((rest ms) (acc '()))
          (if (null? rest)
              (reverse acc)
              (if (not (maybe-nothing? (car rest)))
                  (loop (cdr rest) (cons (maybe-value (car rest)) acc))
                  (loop (cdr rest) acc))))))

    ;; compose :: (a -> Maybe b) -> (b -> Maybe c) -> a -> Maybe c
    (define hydra_lib_maybes_compose
      (lambda (g)
        (lambda (f)
          (lambda (x)
            (let ((result (g x)))
              (if (maybe-nothing? result)
                  (list 'nothing)
                  (f (maybe-value result))))))))

    ;; from_maybe :: a -> Maybe a -> a
    ;; Thunk-aware: if def is a zero-arg procedure (thunk), only called when Maybe is Nothing
    (define hydra_lib_maybes_from_maybe
      (lambda (def)
        (lambda (m)
          (if (maybe-nothing? m)
              (if (procedure? def) (def) def)
              (maybe-value m)))))

    ;; is_just :: Maybe a -> Bool
    (define hydra_lib_maybes_is_just
      (lambda (m)
        (not (maybe-nothing? m))))

    ;; is_nothing :: Maybe a -> Bool
    (define hydra_lib_maybes_is_nothing
      (lambda (m)
        (maybe-nothing? m)))

    ;; map :: (a -> b) -> Maybe a -> Maybe b
    (define hydra_lib_maybes_map
      (lambda (f)
        (lambda (m)
          (if (maybe-nothing? m)
              (list 'nothing)
              (list 'just (f (maybe-value m)))))))

    ;; map_maybe :: (a -> Maybe b) -> [a] -> [b]
    (define hydra_lib_maybes_map_maybe
      (lambda (f)
        (lambda (xs)
          (let loop ((rest xs) (acc '()))
            (if (null? rest)
                (reverse acc)
                (let ((result (f (car rest))))
                  (if (not (maybe-nothing? result))
                      (loop (cdr rest) (cons (maybe-value result) acc))
                      (loop (cdr rest) acc))))))))

    ;; maybe :: b -> (a -> b) -> Maybe a -> b
    ;; Thunk-aware: if def is a zero-arg procedure (thunk), only called when Maybe is Nothing
    (define hydra_lib_maybes_maybe
      (lambda (def)
        (lambda (f)
          (lambda (m)
            (if (maybe-nothing? m)
                (if (procedure? def) (def) def)
                (f (maybe-value m)))))))

    ;; pure :: a -> Maybe a
    (define hydra_lib_maybes_pure
      (lambda (x)
        (list 'just x)))

    ;; to_list :: Maybe a -> [a]
    (define hydra_lib_maybes_to_list
      (lambda (m)
        (if (maybe-nothing? m)
            '()
            (list (maybe-value m)))))))
