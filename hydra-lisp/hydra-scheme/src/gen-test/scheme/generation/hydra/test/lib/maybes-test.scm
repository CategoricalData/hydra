;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.maybes primitives

(import (scheme base))

;; apply

(define (test-apply-negboth-just)

  (assert (equal? 8 ((hydra_lib_maybes_apply (hydra_lib_math_add 3)) 5))))

(define (test-apply-negnothing-function)

  (assert (equal? nil ((hydra_lib_maybes_apply nil) 5))))

(define (test-apply-negnothing-value)

  (assert (equal? nil ((hydra_lib_maybes_apply (hydra_lib_math_add 3)) nil))))

;; bind

(define (test-bind-negjust-to-just)

  (assert (equal? 10 ((hydra_lib_maybes_bind 5) (lambda (x) ((hydra_lib_math_mul x) 2))))))

(define (test-bind-negnothing-to-nothing)

  (assert (equal? nil ((hydra_lib_maybes_bind nil) (lambda (x) ((hydra_lib_math_mul x) 2))))))

;; cases

(define (test-cases-negjust-applies-function)

  (assert (equal? 10 (((hydra_lib_maybes_cases 5) 0) (lambda (x) ((hydra_lib_math_mul x) 2))))))

(define (test-cases-negnothing-returns-default)

  (assert (equal? 99 (((hydra_lib_maybes_cases nil) 99) (lambda (x) ((hydra_lib_math_mul x) 2))))))

;; cat

(define (test-cat-negfilters-nothings)

  (assert (equal? (list 1 2) (hydra_lib_maybes_cat (list 1 nil 2)))))

(define (test-cat-negall-justs)

  (assert (equal? (list 1 2) (hydra_lib_maybes_cat (list 1 2)))))

(define (test-cat-negall-nothings)

  (assert (equal? (list ) (hydra_lib_maybes_cat (list nil nil)))))

(define (test-cat-negempty-list)

  (assert (equal? (list ) (hydra_lib_maybes_cat (list )))))

;; compose

(define (test-compose-negboth-succeed)

  (assert (equal? 12 (((hydra_lib_maybes_compose (lambda (x) (if ((hydra_lib_equality_lte x) 5) ((hydra_lib_math_add x) 1) '()))) (lambda (y) (if ((hydra_lib_equality_gte y) 5) ((hydra_lib_math_mul y) 2) '()))) 5))))

(define (test-compose-negfirst-fails)

  (assert (equal? nil (((hydra_lib_maybes_compose (lambda (x) (if ((hydra_lib_equality_lte x) 5) ((hydra_lib_math_add x) 1) '()))) (lambda (y) (if ((hydra_lib_equality_gte y) 5) ((hydra_lib_math_mul y) 2) '()))) 10))))

(define (test-compose-negsecond-fails)

  (assert (equal? nil (((hydra_lib_maybes_compose (lambda (x) (if ((hydra_lib_equality_lte x) 5) ((hydra_lib_math_add x) 1) '()))) (lambda (y) (if ((hydra_lib_equality_gte y) 5) ((hydra_lib_math_mul y) 2) '()))) 3))))

;; fromJust

(define (test-fromjust-negextract-from-just)

  (assert (equal? 42 (hydra_lib_maybes_from_just 42))))

;; fromMaybe

(define (test-frommaybe-negjust-value)

  (assert (equal? 42 ((hydra_lib_maybes_from_maybe 0) 42))))

(define (test-frommaybe-negnothing-with-default)

  (assert (equal? 99 ((hydra_lib_maybes_from_maybe 99) nil))))

;; isJust

(define (test-isjust-negjust-value)

  (assert (equal? #t (hydra_lib_maybes_is_just 42))))

(define (test-isjust-negnothing)

  (assert (equal? #f (hydra_lib_maybes_is_just nil))))

;; isNothing

(define (test-isnothing-negjust-value)

  (assert (equal? #f (hydra_lib_maybes_is_nothing 42))))

(define (test-isnothing-negnothing)

  (assert (equal? #t (hydra_lib_maybes_is_nothing nil))))

;; map

(define (test-map-negmaps-just-value)

  (assert (equal? 10 ((hydra_lib_maybes_map (lambda (x) ((hydra_lib_math_mul x) 2))) 5))))

(define (test-map-negnothing-unchanged)

  (assert (equal? nil ((hydra_lib_maybes_map (lambda (x) ((hydra_lib_math_mul x) 2))) nil))))

;; mapMaybe

(define (test-mapmaybe-negfilter-and-transform)

  (assert (equal? (list 6 8 10) ((hydra_lib_maybes_map_maybe (lambda (x) (if ((hydra_lib_equality_gt x) 2) ((hydra_lib_math_mul x) 2) '()))) (list 1 2 3 4 5)))))

(define (test-mapmaybe-negempty-result)

  (assert (equal? (list ) ((hydra_lib_maybes_map_maybe (lambda (x) (if ((hydra_lib_equality_gt x) 2) ((hydra_lib_math_mul x) 2) '()))) (list 1 2)))))

(define (test-mapmaybe-negempty-input)

  (assert (equal? (list ) ((hydra_lib_maybes_map_maybe (lambda (x) (if ((hydra_lib_equality_gt x) 2) ((hydra_lib_math_mul x) 2) '()))) (list )))))

;; maybe

(define (test-maybe-negjust-value-applies-function)

  (assert (equal? 10 (((hydra_lib_maybes_maybe 0) (lambda (x) ((hydra_lib_math_mul x) 2))) 5))))

(define (test-maybe-negnothing-returns-default)

  (assert (equal? 99 (((hydra_lib_maybes_maybe 99) (lambda (x) ((hydra_lib_math_mul x) 2))) nil))))

;; pure

(define (test-pure-negwraps-integer)

  (assert (equal? 42 (hydra_lib_maybes_pure 42))))

(define (test-pure-negwraps-string)

  (assert (equal? "hello" (hydra_lib_maybes_pure "hello"))))

;; toList

(define (test-tolist-negjust-value)

  (assert (equal? (list 42) (hydra_lib_maybes_to_list 42))))

(define (test-tolist-negnothing)

  (assert (equal? (list ) (hydra_lib_maybes_to_list nil))))
