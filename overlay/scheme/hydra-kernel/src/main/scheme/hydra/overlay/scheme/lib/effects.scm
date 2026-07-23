(define-library (hydra overlay scheme lib effects)
  (import (scheme base))
  (export hydra_lib_effects_apply
          hydra_lib_effects_bind
          hydra_lib_effects_compose
          hydra_lib_effects_fold_list
          hydra_lib_effects_map
          hydra_lib_effects_map_list
          hydra_lib_effects_map_optional
          hydra_lib_effects_pure)
  (begin

    ;; Scheme (R7RS) implementations of hydra.lib.effects primitives (#494).
    ;;
    ;; The Hydra type effect<t> is transparent in the Lisp dialects (effect<t> = t): there is no
    ;; TypeVariantEffect in the target, so effectful programs are ordinary eager native code and
    ;; "running the effect" simply means forcing the value. These primitives therefore reduce to
    ;; ordinary applications. See the Haskell reference implementation in Hydra.Haskell.Lib.Effects
    ;; (where effect<t> = IO t). All functions are curried, matching the Scheme prim runtime style.
    ;;
    ;; Optional representation: (list 'given val) or (list 'none) (also: bare value / '() for none).

    ;; apply :: effect<x -> y> -> effect<x> -> effect<y>
    ;; Applicative apply for effects. Since effects are transparent, this just applies ef to ex.
    (define hydra_lib_effects_apply
      (lambda (ef)
        (lambda (ex)
          (ef ex))))

    ;; bind :: effect<x> -> (x -> effect<y>) -> effect<y>
    ;; Sequence two effectful computations. Since effects are transparent, this just applies f to a.
    (define hydra_lib_effects_bind
      (lambda (a)
        (lambda (f)
          (f a))))

    ;; compose :: (x -> effect<y>) -> (y -> effect<z>) -> x -> effect<z>
    ;; Kleisli composition for effects: run f, then g on its result.
    (define hydra_lib_effects_compose
      (lambda (f)
        (lambda (g)
          (lambda (x)
            (g (f x))))))

    ;; foldl :: (x -> y -> effect<x>) -> x -> [y] -> effect<x>
    ;; Left-fold over a list with an effect-returning function.
    (define hydra_lib_effects_fold_list
      (lambda (f)
        (lambda (acc)
          (lambda (xs)
            (let loop ((a acc) (rest xs))
              (if (null? rest)
                  a
                  (loop ((f a) (car rest)) (cdr rest))))))))

    ;; map :: (x -> y) -> effect<x> -> effect<y>
    ;; Map a pure function over the result of an effect. Since effects are transparent, just apply f.
    (define hydra_lib_effects_map
      (lambda (f)
        (lambda (a)
          (f a))))

    ;; mapList :: (x -> effect<y>) -> [x] -> effect<[y]>
    ;; Map an effect-returning function over a list, collecting the results.
    (define hydra_lib_effects_map_list
      (lambda (f)
        (lambda (xs)
          (map f xs))))

    ;; mapOptional :: (x -> effect<y>) -> Maybe x -> effect<Maybe y>
    ;; Map an effect-returning function over an optional.
    (define hydra_lib_effects_map_optional
      (lambda (f)
        (lambda (m)
          (cond
            ((or (null? m)
                 (and (pair? m) (eq? (car m) 'none)))
             (list 'none))
            ((and (pair? m) (eq? (car m) 'given))
             (list 'given (f (cadr m))))
            ;; bare value (term-level maybe)
            (else
             (list 'given (f m)))))))

    ;; pure :: x -> effect<x>
    ;; Lift a pure value into an effect. Since effects are transparent, this is the identity.
    (define hydra_lib_effects_pure
      (lambda (x) x))))
