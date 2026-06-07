(define-library (hydra lib optionals)
  (import (scheme base))
  (export hydra_lib_optionals_apply
          hydra_lib_optionals_bind
          hydra_lib_optionals_cases
          hydra_lib_optionals_cat
          hydra_lib_optionals_compose
          hydra_lib_optionals_from_optional
          hydra_lib_optionals_is_given
          hydra_lib_optionals_is_none
          hydra_lib_optionals_map
          hydra_lib_optionals_map_optional
          hydra_lib_optionals_pure
          hydra_lib_optionals_to_list
          maybe-nothing?
          maybe-value)
  (begin

    ;; Maybe representation: (list 'given val) or (list 'none)
    ;; Also handles: bare value for Just, '() for Nothing, (maybe val)/(maybe '())

    (define (maybe-nothing? m)
      (or (null? m)
          (eq? m #f)
          (and (pair? m) (eq? (car m) 'none))))

    (define (maybe-value m)
      (cond
        ((and (pair? m) (eq? (car m) 'given)) (cadr m))
        (else m)))

    ;; apply :: Maybe (a -> b) -> Maybe a -> Maybe b
    (define hydra_lib_optionals_apply
      (lambda (mf)
        (lambda (mx)
          (if (maybe-nothing? mf)
              (list 'none)
              (if (maybe-nothing? mx)
                  (list 'none)
                  (list 'given ((maybe-value mf) (maybe-value mx))))))))

    ;; bind :: Maybe a -> (a -> Maybe b) -> Maybe b
    (define hydra_lib_optionals_bind
      (lambda (m)
        (lambda (f)
          (if (maybe-nothing? m)
              (list 'none)
              (f (maybe-value m))))))

    ;; cases :: Maybe a -> b -> (a -> b) -> b
    ;; Thunk-aware: if def is a zero-arg procedure (thunk), only called when Maybe is Nothing
    (define hydra_lib_optionals_cases
      (lambda (m)
        (lambda (def)
          (lambda (f)
            (if (maybe-nothing? m)
                (if (procedure? def) (def) def)
                (f (maybe-value m)))))))

    ;; cat :: [Maybe a] -> [a]
    (define hydra_lib_optionals_cat
      (lambda (ms)
        (let loop ((rest ms) (acc '()))
          (if (null? rest)
              (reverse acc)
              (if (not (maybe-nothing? (car rest)))
                  (loop (cdr rest) (cons (maybe-value (car rest)) acc))
                  (loop (cdr rest) acc))))))

    ;; compose :: (a -> Maybe b) -> (b -> Maybe c) -> a -> Maybe c
    (define hydra_lib_optionals_compose
      (lambda (g)
        (lambda (f)
          (lambda (x)
            (let ((result (g x)))
              (if (maybe-nothing? result)
                  (list 'none)
                  (f (maybe-value result))))))))

    ;; from_optional :: a -> Maybe a -> a
    ;; Thunk-aware: if def is a zero-arg procedure (thunk), only called when Maybe is Nothing
    (define hydra_lib_optionals_from_optional
      (lambda (def)
        (lambda (m)
          (if (maybe-nothing? m)
              (if (procedure? def) (def) def)
              (maybe-value m)))))

    ;; is_given :: Maybe a -> Bool
    (define hydra_lib_optionals_is_given
      (lambda (m)
        (not (maybe-nothing? m))))

    ;; is_none :: Maybe a -> Bool
    (define hydra_lib_optionals_is_none
      (lambda (m)
        (maybe-nothing? m)))

    ;; map :: (a -> b) -> Maybe a -> Maybe b
    (define hydra_lib_optionals_map
      (lambda (f)
        (lambda (m)
          (if (maybe-nothing? m)
              (list 'none)
              (list 'given (f (maybe-value m)))))))

    ;; map_optional :: (a -> Maybe b) -> [a] -> [b]
    (define hydra_lib_optionals_map_optional
      (lambda (f)
        (lambda (xs)
          (let loop ((rest xs) (acc '()))
            (if (null? rest)
                (reverse acc)
                (let ((result (f (car rest))))
                  (if (not (maybe-nothing? result))
                      (loop (cdr rest) (cons (maybe-value result) acc))
                      (loop (cdr rest) acc))))))))

    ;; pure :: a -> Maybe a
    (define hydra_lib_optionals_pure
      (lambda (x)
        (list 'given x)))

    ;; to_list :: Maybe a -> [a]
    (define hydra_lib_optionals_to_list
      (lambda (m)
        (if (maybe-nothing? m)
            '()
            (list (maybe-value m)))))))
