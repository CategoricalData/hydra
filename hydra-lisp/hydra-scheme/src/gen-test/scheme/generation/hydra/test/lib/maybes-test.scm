;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.maybes primitives

(import (scheme base))

;; apply

(define (test-maybes-negapply-negboth-just)

  (assert (equal? (list 'just 8) ((hydra_lib_maybes_apply (list 'just (hydra_lib_math_add 3))) (list 'just 5)))))

(define (test-maybes-negapply-negnothing-function)

  (assert (equal? (list 'nothing) ((hydra_lib_maybes_apply (list 'nothing)) (list 'just 5)))))

(define (test-maybes-negapply-negnothing-value)

  (assert (equal? (list 'nothing) ((hydra_lib_maybes_apply (list 'just (hydra_lib_math_add 3))) (list 'nothing)))))

;; bind

(define (test-maybes-negbind-negjust-to-just)

  (assert (equal? (list 'just 10) ((hydra_lib_maybes_bind (list 'just 5)) (lambda (x) (list 'just ((hydra_lib_math_mul x) 2)))))))

(define (test-maybes-negbind-negnothing-to-nothing)

  (assert (equal? (list 'nothing) ((hydra_lib_maybes_bind (list 'nothing)) (lambda (x) (list 'just ((hydra_lib_math_mul x) 2)))))))

;; cases

(define (test-maybes-negcases-negjust-applies-function)

  (assert (equal? 10 (((hydra_lib_maybes_cases (list 'just 5)) 0) (lambda (x) ((hydra_lib_math_mul x) 2))))))

(define (test-maybes-negcases-negnothing-returns-default)

  (assert (equal? 99 (((hydra_lib_maybes_cases (list 'nothing)) 99) (lambda (x) ((hydra_lib_math_mul x) 2))))))

;; cat

(define (test-maybes-negcat-negfilters-nothings)

  (assert (equal? (list 1 2) (hydra_lib_maybes_cat (list (list 'just 1) (list 'nothing) (list 'just 2))))))

(define (test-maybes-negcat-negall-justs)

  (assert (equal? (list 1 2) (hydra_lib_maybes_cat (list (list 'just 1) (list 'just 2))))))

(define (test-maybes-negcat-negall-nothings)

  (assert (equal? (list ) (hydra_lib_maybes_cat (list (list 'nothing) (list 'nothing))))))

(define (test-maybes-negcat-negempty-list)

  (assert (equal? (list ) (hydra_lib_maybes_cat (list )))))

;; compose

(define (test-maybes-negcompose-negboth-succeed)

  (assert (equal? (list 'just 12) (((hydra_lib_maybes_compose (lambda (x) (if ((hydra_lib_equality_lte x) 5) (list 'just ((hydra_lib_math_add x) 1)) (list 'nothing)))) (lambda (y) (if ((hydra_lib_equality_gte y) 5) (list 'just ((hydra_lib_math_mul y) 2)) (list 'nothing)))) 5))))

(define (test-maybes-negcompose-negfirst-fails)

  (assert (equal? (list 'nothing) (((hydra_lib_maybes_compose (lambda (x) (if ((hydra_lib_equality_lte x) 5) (list 'just ((hydra_lib_math_add x) 1)) (list 'nothing)))) (lambda (y) (if ((hydra_lib_equality_gte y) 5) (list 'just ((hydra_lib_math_mul y) 2)) (list 'nothing)))) 10))))

(define (test-maybes-negcompose-negsecond-fails)

  (assert (equal? (list 'nothing) (((hydra_lib_maybes_compose (lambda (x) (if ((hydra_lib_equality_lte x) 5) (list 'just ((hydra_lib_math_add x) 1)) (list 'nothing)))) (lambda (y) (if ((hydra_lib_equality_gte y) 5) (list 'just ((hydra_lib_math_mul y) 2)) (list 'nothing)))) 3))))

;; fromJust

(define (test-maybes-negfromjust-negextract-from-just)

  (assert (equal? 42 (hydra_lib_maybes_from_just (list 'just 42)))))

;; fromMaybe

(define (test-maybes-negfrommaybe-negjust-value)

  (assert (equal? 42 ((hydra_lib_maybes_from_maybe 0) (list 'just 42)))))

(define (test-maybes-negfrommaybe-negnothing-with-default)

  (assert (equal? 99 ((hydra_lib_maybes_from_maybe 99) (list 'nothing)))))

;; isJust

(define (test-maybes-negisjust-negjust-value)

  (assert (equal? #t (hydra_lib_maybes_is_just (list 'just 42)))))

(define (test-maybes-negisjust-negnothing)

  (assert (equal? #f (hydra_lib_maybes_is_just (list 'nothing)))))

;; isNothing

(define (test-maybes-negisnothing-negjust-value)

  (assert (equal? #f (hydra_lib_maybes_is_nothing (list 'just 42)))))

(define (test-maybes-negisnothing-negnothing)

  (assert (equal? #t (hydra_lib_maybes_is_nothing (list 'nothing)))))

;; map

(define (test-maybes-negmap-negmaps-just-value)

  (assert (equal? (list 'just 10) ((hydra_lib_maybes_map (lambda (x) ((hydra_lib_math_mul x) 2))) (list 'just 5)))))

(define (test-maybes-negmap-negnothing-unchanged)

  (assert (equal? (list 'nothing) ((hydra_lib_maybes_map (lambda (x) ((hydra_lib_math_mul x) 2))) (list 'nothing)))))

;; mapMaybe

(define (test-maybes-negmapmaybe-negfilter-and-transform)

  (assert (equal? (list 6 8 10) ((hydra_lib_maybes_map_maybe (lambda (x) (if ((hydra_lib_equality_gt x) 2) (list 'just ((hydra_lib_math_mul x) 2)) (list 'nothing)))) (list 1 2 3 4 5)))))

(define (test-maybes-negmapmaybe-negempty-result)

  (assert (equal? (list ) ((hydra_lib_maybes_map_maybe (lambda (x) (if ((hydra_lib_equality_gt x) 2) (list 'just ((hydra_lib_math_mul x) 2)) (list 'nothing)))) (list 1 2)))))

(define (test-maybes-negmapmaybe-negempty-input)

  (assert (equal? (list ) ((hydra_lib_maybes_map_maybe (lambda (x) (if ((hydra_lib_equality_gt x) 2) (list 'just ((hydra_lib_math_mul x) 2)) (list 'nothing)))) (list )))))

;; maybe

(define (test-maybes-negmaybe-negjust-value-applies-function)

  (assert (equal? 10 (((hydra_lib_maybes_maybe 0) (lambda (x) ((hydra_lib_math_mul x) 2))) (list 'just 5)))))

(define (test-maybes-negmaybe-negnothing-returns-default)

  (assert (equal? 99 (((hydra_lib_maybes_maybe 99) (lambda (x) ((hydra_lib_math_mul x) 2))) (list 'nothing)))))

;; pure

(define (test-maybes-negpure-negwraps-integer)

  (assert (equal? (list 'just 42) (hydra_lib_maybes_pure 42))))

(define (test-maybes-negpure-negwraps-string)

  (assert (equal? (list 'just "hello") (hydra_lib_maybes_pure "hello"))))

;; toList

(define (test-maybes-negtolist-negjust-value)

  (assert (equal? (list 42) (hydra_lib_maybes_to_list (list 'just 42)))))

(define (test-maybes-negtolist-negnothing)

  (assert (equal? (list ) (hydra_lib_maybes_to_list (list 'nothing)))))
