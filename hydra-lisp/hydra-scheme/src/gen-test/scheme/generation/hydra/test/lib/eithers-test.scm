;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.eithers primitives

(import (scheme base))

;; bind

(define (test-eithers-negbind-negbind-right-with-success)

  (assert (equal? (list 'right 2) ((hydra_lib_eithers_bind (list 'right "ab")) (lambda (s) (if (hydra_lib_strings_null s) (list 'left 0) (list 'right (hydra_lib_strings_length s))))))))

(define (test-eithers-negbind-negbind-right-with-failure)

  (assert (equal? (list 'left 0) ((hydra_lib_eithers_bind (list 'right "")) (lambda (s) (if (hydra_lib_strings_null s) (list 'left 0) (list 'right (hydra_lib_strings_length s))))))))

(define (test-eithers-negbind-negbind-left-returns-left-unchanged)

  (assert (equal? (list 'left 42) ((hydra_lib_eithers_bind (list 'left 42)) (lambda (s) (if (hydra_lib_strings_null s) (list 'left 0) (list 'right (hydra_lib_strings_length s))))))))

;; bimap

(define (test-eithers-negbimap-negmap-left-value)

  (assert (equal? (list 'left 10) (((hydra_lib_eithers_bimap (lambda (x) ((hydra_lib_math_mul x) 2))) (lambda (s) (hydra_lib_strings_length s))) (list 'left 5)))))

(define (test-eithers-negbimap-negmap-right-value)

  (assert (equal? (list 'right 2) (((hydra_lib_eithers_bimap (lambda (x) ((hydra_lib_math_mul x) 2))) (lambda (s) (hydra_lib_strings_length s))) (list 'right "ab")))))

;; isLeft

(define (test-eithers-negisleft-negleft-value)

  (assert (equal? #t (hydra_lib_eithers_is_left (list 'left 42)))))

(define (test-eithers-negisleft-negright-value)

  (assert (equal? #f (hydra_lib_eithers_is_left (list 'right "test")))))

;; isRight

(define (test-eithers-negisright-negright-value)

  (assert (equal? #t (hydra_lib_eithers_is_right (list 'right "test")))))

(define (test-eithers-negisright-negleft-value)

  (assert (equal? #f (hydra_lib_eithers_is_right (list 'left 42)))))

;; fromLeft

(define (test-eithers-negfromleft-negextract-left)

  (assert (equal? 42 ((hydra_lib_eithers_from_left 99) (list 'left 42)))))

(define (test-eithers-negfromleft-neguse-default-for-right)

  (assert (equal? 99 ((hydra_lib_eithers_from_left 99) (list 'right "test")))))

;; fromRight

(define (test-eithers-negfromright-negextract-right)

  (assert (equal? "test" ((hydra_lib_eithers_from_right "default") (list 'right "test")))))

(define (test-eithers-negfromright-neguse-default-for-left)

  (assert (equal? "default" ((hydra_lib_eithers_from_right "default") (list 'left 42)))))

;; either

(define (test-eithers-negeither-negapply-left-function)

  (assert (equal? 10 (((hydra_lib_eithers_either (lambda (x) ((hydra_lib_math_mul x) 2))) (lambda (s) (hydra_lib_strings_length s))) (list 'left 5)))))

(define (test-eithers-negeither-negapply-right-function)

  (assert (equal? 2 (((hydra_lib_eithers_either (lambda (x) ((hydra_lib_math_mul x) 2))) (lambda (s) (hydra_lib_strings_length s))) (list 'right "ab")))))

;; lefts

(define (test-eithers-neglefts-negfilter-left-values)

  (assert (equal? (list 1 2) (hydra_lib_eithers_lefts (list (list 'left 1) (list 'right "a") (list 'left 2) (list 'right "b"))))))

(define (test-eithers-neglefts-negall-lefts)

  (assert (equal? (list 1 2) (hydra_lib_eithers_lefts (list (list 'left 1) (list 'left 2))))))

(define (test-eithers-neglefts-negall-rights)

  (assert (equal? (list ) (hydra_lib_eithers_lefts (list (list 'right "a") (list 'right "b"))))))

(define (test-eithers-neglefts-negempty-list)

  (assert (equal? (list ) (hydra_lib_eithers_lefts (list )))))

;; rights

(define (test-eithers-negrights-negfilter-right-values)

  (assert (equal? (list "a" "b") (hydra_lib_eithers_rights (list (list 'left 1) (list 'right "a") (list 'left 2) (list 'right "b"))))))

(define (test-eithers-negrights-negall-rights)

  (assert (equal? (list "a" "b") (hydra_lib_eithers_rights (list (list 'right "a") (list 'right "b"))))))

(define (test-eithers-negrights-negall-lefts)

  (assert (equal? (list ) (hydra_lib_eithers_rights (list (list 'left 1) (list 'left 2))))))

(define (test-eithers-negrights-negempty-list)

  (assert (equal? (list ) (hydra_lib_eithers_rights (list )))))

;; partitionEithers

(define (test-eithers-negpartitioneithers-negpartition-mixed)

  (assert (equal? (list (list 1 2) (list "a" "b")) (hydra_lib_eithers_partition_eithers (list (list 'left 1) (list 'right "a") (list 'left 2) (list 'right "b"))))))

(define (test-eithers-negpartitioneithers-negall-lefts)

  (assert (equal? (list (list 1 2) (list )) (hydra_lib_eithers_partition_eithers (list (list 'left 1) (list 'left 2))))))

(define (test-eithers-negpartitioneithers-negall-rights)

  (assert (equal? (list (list ) (list "a" "b")) (hydra_lib_eithers_partition_eithers (list (list 'right "a") (list 'right "b"))))))

(define (test-eithers-negpartitioneithers-negempty-list)

  (assert (equal? (list (list ) (list )) (hydra_lib_eithers_partition_eithers (list )))))

;; map

(define (test-eithers-negmap-negmap-right-value)

  (assert (equal? (list 'right 10) ((hydra_lib_eithers_map (lambda (x) ((hydra_lib_math_mul x) 2))) (list 'right 5)))))

(define (test-eithers-negmap-negpreserve-left)

  (assert (equal? (list 'left 99) ((hydra_lib_eithers_map (lambda (x) ((hydra_lib_math_mul x) 2))) (list 'left 99)))))

;; mapList

(define (test-eithers-negmaplist-negall-succeed)

  (assert (equal? (list 'right (list 2 4 6)) ((hydra_lib_eithers_map_list (lambda (x) (if ((hydra_lib_equality_equal x) 0) (list 'left "zero") (list 'right ((hydra_lib_math_mul x) 2))))) (list 1 2 3)))))

(define (test-eithers-negmaplist-negfirst-fails)

  (assert (equal? (list 'left "zero") ((hydra_lib_eithers_map_list (lambda (x) (if ((hydra_lib_equality_equal x) 0) (list 'left "zero") (list 'right ((hydra_lib_math_mul x) 2))))) (list 1 0 3)))))

(define (test-eithers-negmaplist-negempty-list)

  (assert (equal? (list 'right (list )) ((hydra_lib_eithers_map_list (lambda (x) (if ((hydra_lib_equality_equal x) 0) (list 'left "zero") (list 'right ((hydra_lib_math_mul x) 2))))) (list )))))

;; mapMaybe

(define (test-eithers-negmapmaybe-negjust-succeeds)

  (assert (equal? (list 'right (list 'just 10)) ((hydra_lib_eithers_map_maybe (lambda (x) (if ((hydra_lib_equality_equal x) 0) (list 'left "zero") (list 'right ((hydra_lib_math_mul x) 2))))) (list 'just 5)))))

(define (test-eithers-negmapmaybe-negjust-fails)

  (assert (equal? (list 'left "zero") ((hydra_lib_eithers_map_maybe (lambda (x) (if ((hydra_lib_equality_equal x) 0) (list 'left "zero") (list 'right ((hydra_lib_math_mul x) 2))))) (list 'just 0)))))

(define (test-eithers-negmapmaybe-negnothing)

  (assert (equal? (list 'right (list 'nothing)) ((hydra_lib_eithers_map_maybe (lambda (x) (if ((hydra_lib_equality_equal x) 0) (list 'left "zero") (list 'right ((hydra_lib_math_mul x) 2))))) (list 'nothing)))))
