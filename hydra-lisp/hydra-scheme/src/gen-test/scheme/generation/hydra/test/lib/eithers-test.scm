;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.eithers primitives

(import (scheme base))

;; bind

(define (test-bind-negbind-right-with-success)

  (assert (equal? (list :right 2) ((hydra_lib_eithers_bind (list :right "ab")) (lambda (s) (if (hydra_lib_strings_null s) (list 'left 0) (list 'right (hydra_lib_strings_length s))))))))

(define (test-bind-negbind-right-with-failure)

  (assert (equal? (list :left 0) ((hydra_lib_eithers_bind (list :right "")) (lambda (s) (if (hydra_lib_strings_null s) (list 'left 0) (list 'right (hydra_lib_strings_length s))))))))

(define (test-bind-negbind-left-returns-left-unchanged)

  (assert (equal? (list :left 42) ((hydra_lib_eithers_bind (list :left 42)) (lambda (s) (if (hydra_lib_strings_null s) (list 'left 0) (list 'right (hydra_lib_strings_length s))))))))

;; bimap

(define (test-bimap-negmap-left-value)

  (assert (equal? (list :left 10) (((hydra_lib_eithers_bimap (lambda (x) ((hydra_lib_math_mul x) 2))) (lambda (s) (hydra_lib_strings_length s))) (list :left 5)))))

(define (test-bimap-negmap-right-value)

  (assert (equal? (list :right 2) (((hydra_lib_eithers_bimap (lambda (x) ((hydra_lib_math_mul x) 2))) (lambda (s) (hydra_lib_strings_length s))) (list :right "ab")))))

;; isLeft

(define (test-isleft-negleft-value)

  (assert (equal? #t (hydra_lib_eithers_is_left (list :left 42)))))

(define (test-isleft-negright-value)

  (assert (equal? #f (hydra_lib_eithers_is_left (list :right "test")))))

;; isRight

(define (test-isright-negright-value)

  (assert (equal? #t (hydra_lib_eithers_is_right (list :right "test")))))

(define (test-isright-negleft-value)

  (assert (equal? #f (hydra_lib_eithers_is_right (list :left 42)))))

;; fromLeft

(define (test-fromleft-negextract-left)

  (assert (equal? 42 ((hydra_lib_eithers_from_left 99) (list :left 42)))))

(define (test-fromleft-neguse-default-for-right)

  (assert (equal? 99 ((hydra_lib_eithers_from_left 99) (list :right "test")))))

;; fromRight

(define (test-fromright-negextract-right)

  (assert (equal? "test" ((hydra_lib_eithers_from_right "default") (list :right "test")))))

(define (test-fromright-neguse-default-for-left)

  (assert (equal? "default" ((hydra_lib_eithers_from_right "default") (list :left 42)))))

;; either

(define (test-either-negapply-left-function)

  (assert (equal? 10 (((hydra_lib_eithers_either (lambda (x) ((hydra_lib_math_mul x) 2))) (lambda (s) (hydra_lib_strings_length s))) (list :left 5)))))

(define (test-either-negapply-right-function)

  (assert (equal? 2 (((hydra_lib_eithers_either (lambda (x) ((hydra_lib_math_mul x) 2))) (lambda (s) (hydra_lib_strings_length s))) (list :right "ab")))))

;; lefts

(define (test-lefts-negfilter-left-values)

  (assert (equal? (list 1 2) (hydra_lib_eithers_lefts (list (list :left 1) (list :right "a") (list :left 2) (list :right "b"))))))

(define (test-lefts-negall-lefts)

  (assert (equal? (list 1 2) (hydra_lib_eithers_lefts (list (list :left 1) (list :left 2))))))

(define (test-lefts-negall-rights)

  (assert (equal? (list ) (hydra_lib_eithers_lefts (list (list :right "a") (list :right "b"))))))

(define (test-lefts-negempty-list)

  (assert (equal? (list ) (hydra_lib_eithers_lefts (list )))))

;; rights

(define (test-rights-negfilter-right-values)

  (assert (equal? (list "a" "b") (hydra_lib_eithers_rights (list (list :left 1) (list :right "a") (list :left 2) (list :right "b"))))))

(define (test-rights-negall-rights)

  (assert (equal? (list "a" "b") (hydra_lib_eithers_rights (list (list :right "a") (list :right "b"))))))

(define (test-rights-negall-lefts)

  (assert (equal? (list ) (hydra_lib_eithers_rights (list (list :left 1) (list :left 2))))))

(define (test-rights-negempty-list)

  (assert (equal? (list ) (hydra_lib_eithers_rights (list )))))

;; partitionEithers

(define (test-partitioneithers-negpartition-mixed)

  (assert (equal? (list (list 1 2) (list "a" "b")) (hydra_lib_eithers_partition_eithers (list (list :left 1) (list :right "a") (list :left 2) (list :right "b"))))))

(define (test-partitioneithers-negall-lefts)

  (assert (equal? (list (list 1 2) (list )) (hydra_lib_eithers_partition_eithers (list (list :left 1) (list :left 2))))))

(define (test-partitioneithers-negall-rights)

  (assert (equal? (list (list ) (list "a" "b")) (hydra_lib_eithers_partition_eithers (list (list :right "a") (list :right "b"))))))

(define (test-partitioneithers-negempty-list)

  (assert (equal? (list (list ) (list )) (hydra_lib_eithers_partition_eithers (list )))))

;; map

(define (test-map-negmap-right-value)

  (assert (equal? (list :right 10) ((hydra_lib_eithers_map (lambda (x) ((hydra_lib_math_mul x) 2))) (list :right 5)))))

(define (test-map-negpreserve-left)

  (assert (equal? (list :left 99) ((hydra_lib_eithers_map (lambda (x) ((hydra_lib_math_mul x) 2))) (list :left 99)))))

;; mapList

(define (test-maplist-negall-succeed)

  (assert (equal? (list :right (list 2 4 6)) ((hydra_lib_eithers_map_list (lambda (x) (if ((hydra_lib_equality_equal x) 0) (list 'left "zero") (list 'right ((hydra_lib_math_mul x) 2))))) (list 1 2 3)))))

(define (test-maplist-negfirst-fails)

  (assert (equal? (list :left "zero") ((hydra_lib_eithers_map_list (lambda (x) (if ((hydra_lib_equality_equal x) 0) (list 'left "zero") (list 'right ((hydra_lib_math_mul x) 2))))) (list 1 0 3)))))

(define (test-maplist-negempty-list)

  (assert (equal? (list :right (list )) ((hydra_lib_eithers_map_list (lambda (x) (if ((hydra_lib_equality_equal x) 0) (list 'left "zero") (list 'right ((hydra_lib_math_mul x) 2))))) (list )))))

;; mapMaybe

(define (test-mapmaybe-negjust-succeeds)

  (assert (equal? (list :right 10) ((hydra_lib_eithers_map_maybe (lambda (x) (if ((hydra_lib_equality_equal x) 0) (list 'left "zero") (list 'right ((hydra_lib_math_mul x) 2))))) 5))))

(define (test-mapmaybe-negjust-fails)

  (assert (equal? (list :left "zero") ((hydra_lib_eithers_map_maybe (lambda (x) (if ((hydra_lib_equality_equal x) 0) (list 'left "zero") (list 'right ((hydra_lib_math_mul x) 2))))) 0))))

(define (test-mapmaybe-negnothing)

  (assert (equal? (list :right nil) ((hydra_lib_eithers_map_maybe (lambda (x) (if ((hydra_lib_equality_equal x) 0) (list 'left "zero") (list 'right ((hydra_lib_math_mul x) 2))))) nil))))
