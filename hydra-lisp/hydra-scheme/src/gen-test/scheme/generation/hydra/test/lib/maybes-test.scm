;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.maybes primitives

(import (scheme base))

;; apply

(define (test-apply-negboth-just)

  (assert (equal? (list :just 8) ((hydra_lib_maybes_apply (list :just (hydra_lib_math_add 3))) (list :just 5)))))

(define (test-apply-negnothing-function)

  (assert (equal? (list :nothing) ((hydra_lib_maybes_apply (list :nothing)) (list :just 5)))))

(define (test-apply-negnothing-value)

  (assert (equal? (list :nothing) ((hydra_lib_maybes_apply (list :just (hydra_lib_math_add 3))) (list :nothing)))))

;; bind

(define (test-bind-negjust-to-just)

  (assert (equal? (list :just 10) ((hydra_lib_maybes_bind (list :just 5)) (lambda (x) (list 'just ((hydra_lib_math_mul x) 2)))))))

(define (test-bind-negnothing-to-nothing)

  (assert (equal? (list :nothing) ((hydra_lib_maybes_bind (list :nothing)) (lambda (x) (list 'just ((hydra_lib_math_mul x) 2)))))))

;; cases

(define (test-cases-negjust-applies-function)

  (assert (equal? 10 (((hydra_lib_maybes_cases (list :just 5)) 0) (lambda (x) ((hydra_lib_math_mul x) 2))))))

(define (test-cases-negnothing-returns-default)

  (assert (equal? 99 (((hydra_lib_maybes_cases (list :nothing)) 99) (lambda (x) ((hydra_lib_math_mul x) 2))))))

;; cat

(define (test-cat-negfilters-nothings)

  (assert (equal? (list 1 2) (hydra_lib_maybes_cat (list (list :just 1) (list :nothing) (list :just 2))))))

(define (test-cat-negall-justs)

  (assert (equal? (list 1 2) (hydra_lib_maybes_cat (list (list :just 1) (list :just 2))))))

(define (test-cat-negall-nothings)

  (assert (equal? (list ) (hydra_lib_maybes_cat (list (list :nothing) (list :nothing))))))

(define (test-cat-negempty-list)

  (assert (equal? (list ) (hydra_lib_maybes_cat (list )))))

;; compose

(define (test-compose-negboth-succeed)

  (assert (equal? (list :just 12) (((hydra_lib_maybes_compose (lambda (x) (if ((hydra_lib_equality_lte x) 5) (list 'just ((hydra_lib_math_add x) 1)) (list 'nothing)))) (lambda (y) (if ((hydra_lib_equality_gte y) 5) (list 'just ((hydra_lib_math_mul y) 2)) (list 'nothing)))) 5))))

(define (test-compose-negfirst-fails)

  (assert (equal? (list :nothing) (((hydra_lib_maybes_compose (lambda (x) (if ((hydra_lib_equality_lte x) 5) (list 'just ((hydra_lib_math_add x) 1)) (list 'nothing)))) (lambda (y) (if ((hydra_lib_equality_gte y) 5) (list 'just ((hydra_lib_math_mul y) 2)) (list 'nothing)))) 10))))

(define (test-compose-negsecond-fails)

  (assert (equal? (list :nothing) (((hydra_lib_maybes_compose (lambda (x) (if ((hydra_lib_equality_lte x) 5) (list 'just ((hydra_lib_math_add x) 1)) (list 'nothing)))) (lambda (y) (if ((hydra_lib_equality_gte y) 5) (list 'just ((hydra_lib_math_mul y) 2)) (list 'nothing)))) 3))))

;; fromJust

(define (test-fromjust-negextract-from-just)

  (assert (equal? 42 (hydra_lib_maybes_from_just (list :just 42)))))

;; fromMaybe

(define (test-frommaybe-negjust-value)

  (assert (equal? 42 ((hydra_lib_maybes_from_maybe 0) (list :just 42)))))

(define (test-frommaybe-negnothing-with-default)

  (assert (equal? 99 ((hydra_lib_maybes_from_maybe 99) (list :nothing)))))

;; isJust

(define (test-isjust-negjust-value)

  (assert (equal? #t (hydra_lib_maybes_is_just (list :just 42)))))

(define (test-isjust-negnothing)

  (assert (equal? #f (hydra_lib_maybes_is_just (list :nothing)))))

;; isNothing

(define (test-isnothing-negjust-value)

  (assert (equal? #f (hydra_lib_maybes_is_nothing (list :just 42)))))

(define (test-isnothing-negnothing)

  (assert (equal? #t (hydra_lib_maybes_is_nothing (list :nothing)))))

;; map

(define (test-map-negmaps-just-value)

  (assert (equal? (list :just 10) ((hydra_lib_maybes_map (lambda (x) ((hydra_lib_math_mul x) 2))) (list :just 5)))))

(define (test-map-negnothing-unchanged)

  (assert (equal? (list :nothing) ((hydra_lib_maybes_map (lambda (x) ((hydra_lib_math_mul x) 2))) (list :nothing)))))

;; mapMaybe

(define (test-mapmaybe-negfilter-and-transform)

  (assert (equal? (list 6 8 10) ((hydra_lib_maybes_map_maybe (lambda (x) (if ((hydra_lib_equality_gt x) 2) (list 'just ((hydra_lib_math_mul x) 2)) (list 'nothing)))) (list 1 2 3 4 5)))))

(define (test-mapmaybe-negempty-result)

  (assert (equal? (list ) ((hydra_lib_maybes_map_maybe (lambda (x) (if ((hydra_lib_equality_gt x) 2) (list 'just ((hydra_lib_math_mul x) 2)) (list 'nothing)))) (list 1 2)))))

(define (test-mapmaybe-negempty-input)

  (assert (equal? (list ) ((hydra_lib_maybes_map_maybe (lambda (x) (if ((hydra_lib_equality_gt x) 2) (list 'just ((hydra_lib_math_mul x) 2)) (list 'nothing)))) (list )))))

;; maybe

(define (test-maybe-negjust-value-applies-function)

  (assert (equal? 10 (((hydra_lib_maybes_maybe 0) (lambda (x) ((hydra_lib_math_mul x) 2))) (list :just 5)))))

(define (test-maybe-negnothing-returns-default)

  (assert (equal? 99 (((hydra_lib_maybes_maybe 99) (lambda (x) ((hydra_lib_math_mul x) 2))) (list :nothing)))))

;; pure

(define (test-pure-negwraps-integer)

  (assert (equal? (list :just 42) (hydra_lib_maybes_pure 42))))

(define (test-pure-negwraps-string)

  (assert (equal? (list :just "hello") (hydra_lib_maybes_pure "hello"))))

;; toList

(define (test-tolist-negjust-value)

  (assert (equal? (list 42) (hydra_lib_maybes_to_list (list :just 42)))))

(define (test-tolist-negnothing)

  (assert (equal? (list ) (hydra_lib_maybes_to_list (list :nothing)))))
