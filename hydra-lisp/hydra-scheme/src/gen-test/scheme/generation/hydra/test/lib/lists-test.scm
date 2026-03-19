;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.lists primitives

(import (scheme base))

;; apply

;; string transformations

(define (test-lists-negapply-negstring-transformations-negstring-transformations)

  (assert (equal? (list "ONE" "TWO" "THREE" "one" "two" "three") ((hydra_lib_lists_apply (list hydra_lib_strings_to_upper hydra_lib_strings_to_lower)) (list "One" "Two" "Three")))))

;; edge cases

(define (test-lists-negapply-negedge-cases-negempty-function-list)

  (assert (equal? (list ) ((hydra_lib_lists_apply (list )) (list "a" "b")))))

(define (test-lists-negapply-negedge-cases-negempty-input-list)

  (assert (equal? (list ) ((hydra_lib_lists_apply (list hydra_lib_strings_to_upper)) (list )))))

(define (test-lists-negapply-negedge-cases-negsingle-function)

  (assert (equal? (list "HELLO") ((hydra_lib_lists_apply (list hydra_lib_strings_to_upper)) (list "hello")))))

(define (test-lists-negapply-negedge-cases-negsingle-input)

  (assert (equal? (list "TEST" "test") ((hydra_lib_lists_apply (list hydra_lib_strings_to_upper hydra_lib_strings_to_lower)) (list "Test")))))

;; at

(define (test-lists-negat-negfirst-element)

  (assert (equal? 1 ((hydra_lib_lists_at 0) (list 1 2 3)))))

(define (test-lists-negat-negmiddle-element)

  (assert (equal? 2 ((hydra_lib_lists_at 1) (list 1 2 3)))))

(define (test-lists-negat-neglast-element)

  (assert (equal? 3 ((hydra_lib_lists_at 2) (list 1 2 3)))))

(define (test-lists-negat-negsingle-element-list)

  (assert (equal? 42 ((hydra_lib_lists_at 0) (list 42)))))

(define (test-lists-negat-negstring-list-access)

  (assert (equal? "world" ((hydra_lib_lists_at 1) (list "hello" "world")))))

;; bind

(define (test-lists-negbind-negnegation-function)

  (assert (equal? (list -1 -2 -3 -4) ((hydra_lib_lists_bind (list 1 2 3 4)) (lambda (x) (hydra_lib_lists_pure (hydra_lib_math_negate x)))))))

(define (test-lists-negbind-negempty-list)

  (assert (equal? (list ) ((hydra_lib_lists_bind (list )) (lambda (x) (hydra_lib_lists_pure (hydra_lib_math_negate x)))))))

(define (test-lists-negbind-negsingle-element)

  (assert (equal? (list -5) ((hydra_lib_lists_bind (list 5)) (lambda (x) (hydra_lib_lists_pure (hydra_lib_math_negate x)))))))

(define (test-lists-negbind-negduplicate-elements)

  (assert (equal? (list -1 -1 -2) ((hydra_lib_lists_bind (list 1 1 2)) (lambda (x) (hydra_lib_lists_pure (hydra_lib_math_negate x)))))))

;; concat

(define (test-lists-negconcat-negmultiple-non-negempty-lists)

  (assert (equal? (list 1 2 3 4 5 6 7 8) (hydra_lib_lists_concat (list (list 1 2 3) (list 4 5) (list 6 7 8))))))

(define (test-lists-negconcat-negempty-lists-included)

  (assert (equal? (list 1 2 3) (hydra_lib_lists_concat (list (list ) (list 1 2) (list ) (list 3))))))

(define (test-lists-negconcat-negsingle-list)

  (assert (equal? (list 1 2 3) (hydra_lib_lists_concat (list (list 1 2 3))))))

(define (test-lists-negconcat-negall-empty-lists)

  (assert (equal? (list ) (hydra_lib_lists_concat (list (list ) (list ) (list ))))))

(define (test-lists-negconcat-negempty-list-of-lists)

  (assert (equal? (list ) (hydra_lib_lists_concat (list )))))

;; concat2

(define (test-lists-negconcat2-negtwo-non-negempty-lists)

  (assert (equal? (list 1 2 3 4) ((hydra_lib_lists_concat2 (list 1 2)) (list 3 4)))))

(define (test-lists-negconcat2-negfirst-list-empty)

  (assert (equal? (list 1 2) ((hydra_lib_lists_concat2 (list )) (list 1 2)))))

(define (test-lists-negconcat2-negsecond-list-empty)

  (assert (equal? (list 1 2) ((hydra_lib_lists_concat2 (list 1 2)) (list )))))

(define (test-lists-negconcat2-negboth-lists-empty)

  (assert (equal? (list ) ((hydra_lib_lists_concat2 (list )) (list )))))

(define (test-lists-negconcat2-negsingle-elements)

  (assert (equal? (list 1 2) ((hydra_lib_lists_concat2 (list 1)) (list 2)))))

(define (test-lists-negconcat2-negstring-lists)

  (assert (equal? (list "a" "b" "c" "d") ((hydra_lib_lists_concat2 (list "a" "b")) (list "c" "d")))))

;; cons

(define (test-lists-negcons-negcons-to-non-negempty-list)

  (assert (equal? (list 1 2 3) ((hydra_lib_lists_cons 1) (list 2 3)))))

(define (test-lists-negcons-negcons-to-empty-list)

  (assert (equal? (list 1) ((hydra_lib_lists_cons 1) (list )))))

(define (test-lists-negcons-negcons-negative-number)

  (assert (equal? (list -1 2 3) ((hydra_lib_lists_cons -1) (list 2 3)))))

(define (test-lists-negcons-negcons-string)

  (assert (equal? (list "hello" "world") ((hydra_lib_lists_cons "hello") (list "world")))))

;; drop

(define (test-lists-negdrop-negdrop-from-beginning)

  (assert (equal? (list 3 4 5) ((hydra_lib_lists_drop 2) (list 1 2 3 4 5)))))

(define (test-lists-negdrop-negdrop-zero-elements)

  (assert (equal? (list 1 2 3) ((hydra_lib_lists_drop 0) (list 1 2 3)))))

(define (test-lists-negdrop-negdrop-all-elements)

  (assert (equal? (list ) ((hydra_lib_lists_drop 3) (list 1 2 3)))))

(define (test-lists-negdrop-negdrop-more-than-length)

  (assert (equal? (list ) ((hydra_lib_lists_drop 5) (list 1 2)))))

(define (test-lists-negdrop-negdrop-from-empty-list)

  (assert (equal? (list ) ((hydra_lib_lists_drop 3) (list )))))

(define (test-lists-negdrop-negdrop-negative-amount)

  (assert (equal? (list 1 2 3) ((hydra_lib_lists_drop -1) (list 1 2 3)))))

;; dropWhile

(define (test-lists-negdropwhile-negdrop-while-less-than-3)

  (assert (equal? (list 3 2 1) ((hydra_lib_lists_drop_while (lambda (x) ((hydra_lib_equality_lt x) 3))) (list 1 2 3 2 1)))))

(define (test-lists-negdropwhile-negdrop-all-elements)

  (assert (equal? (list ) ((hydra_lib_lists_drop_while (lambda (x) ((hydra_lib_equality_lt x) 10))) (list 1 2 3)))))

(define (test-lists-negdropwhile-negdrop-no-elements)

  (assert (equal? (list 1 2 3) ((hydra_lib_lists_drop_while (lambda (x) ((hydra_lib_equality_lt x) 0))) (list 1 2 3)))))

(define (test-lists-negdropwhile-negempty-list)

  (assert (equal? (list ) ((hydra_lib_lists_drop_while (lambda (x) ((hydra_lib_equality_lt x) 5))) (list )))))

;; elem

(define (test-lists-negelem-negelement-present)

  (assert (equal? #t ((hydra_lib_lists_elem 2) (list 1 2 3)))))

(define (test-lists-negelem-negelement-not-present)

  (assert (equal? #f ((hydra_lib_lists_elem 4) (list 1 2 3)))))

(define (test-lists-negelem-negempty-list)

  (assert (equal? #f ((hydra_lib_lists_elem 1) (list )))))

(define (test-lists-negelem-negsingle-element-present)

  (assert (equal? #t ((hydra_lib_lists_elem 1) (list 1)))))

(define (test-lists-negelem-negsingle-element-not-present)

  (assert (equal? #f ((hydra_lib_lists_elem 2) (list 1)))))

(define (test-lists-negelem-negduplicate-elements)

  (assert (equal? #t ((hydra_lib_lists_elem 2) (list 1 2 2 3)))))

(define (test-lists-negelem-negstring-element-present)

  (assert (equal? #t ((hydra_lib_lists_elem "hello") (list "world" "hello" "test")))))

(define (test-lists-negelem-negstring-element-not-present)

  (assert (equal? #f ((hydra_lib_lists_elem "missing") (list "world" "hello")))))

;; filter

(define (test-lists-negfilter-negfilter-positive-numbers)

  (assert (equal? (list 2 4 5) ((hydra_lib_lists_filter (lambda (x) ((hydra_lib_equality_gt x) 0))) (list -1 2 -3 4 5)))))

(define (test-lists-negfilter-negfilter-all-elements)

  (assert (equal? (list 1 2 3) ((hydra_lib_lists_filter (lambda (x) ((hydra_lib_equality_lt x) 10))) (list 1 2 3)))))

(define (test-lists-negfilter-negfilter-no-elements)

  (assert (equal? (list ) ((hydra_lib_lists_filter (lambda (x) ((hydra_lib_equality_gt x) 10))) (list 1 2 3)))))

(define (test-lists-negfilter-negempty-list)

  (assert (equal? (list ) ((hydra_lib_lists_filter (lambda (x) ((hydra_lib_equality_gt x) 0))) (list )))))

;; find

(define (test-lists-negfind-negfind-existing-element)

  (assert (equal? (list 'just 4) ((hydra_lib_lists_find (lambda (x) ((hydra_lib_equality_gt x) 3))) (list 1 2 4 5)))))

(define (test-lists-negfind-negfind-first-matching)

  (assert (equal? (list 'just 1) ((hydra_lib_lists_find (lambda (x) ((hydra_lib_equality_gt x) 0))) (list 1 2 3)))))

(define (test-lists-negfind-negfind-no-match)

  (assert (equal? (list 'nothing) ((hydra_lib_lists_find (lambda (x) ((hydra_lib_equality_gt x) 10))) (list 1 2 3)))))

(define (test-lists-negfind-negfind-in-empty-list)

  (assert (equal? (list 'nothing) ((hydra_lib_lists_find (lambda (x) ((hydra_lib_equality_gt x) 0))) (list )))))

(define (test-lists-negfind-negfind-single-element)

  (assert (equal? (list 'just 42) ((hydra_lib_lists_find (lambda (x) ((hydra_lib_equality_equal x) 42))) (list 42)))))

;; foldl

(define (test-lists-negfoldl-negsum-with-addition)

  (assert (equal? 10 (((hydra_lib_lists_foldl hydra_lib_math_add) 0) (list 1 2 3 4)))))

(define (test-lists-negfoldl-negproduct-with-multiplication)

  (assert (equal? 24 (((hydra_lib_lists_foldl hydra_lib_math_mul) 1) (list 2 3 4)))))

(define (test-lists-negfoldl-negempty-list)

  (assert (equal? 5 (((hydra_lib_lists_foldl hydra_lib_math_add) 5) (list )))))

(define (test-lists-negfoldl-negsingle-element)

  (assert (equal? 15 (((hydra_lib_lists_foldl hydra_lib_math_add) 10) (list 5)))))

(define (test-lists-negfoldl-negsubtraction-fold)

  (assert (equal? 4 (((hydra_lib_lists_foldl hydra_lib_math_sub) 10) (list 1 2 3)))))

;; foldr

(define (test-lists-negfoldr-negsubtraction-fold-right)

  (assert (equal? 2 (((hydra_lib_lists_foldr hydra_lib_math_sub) 0) (list 1 2 3)))))

(define (test-lists-negfoldr-negempty-list)

  (assert (equal? 5 (((hydra_lib_lists_foldr hydra_lib_math_add) 5) (list )))))

(define (test-lists-negfoldr-negsingle-element)

  (assert (equal? 15 (((hydra_lib_lists_foldr hydra_lib_math_add) 10) (list 5)))))

(define (test-lists-negfoldr-negsum-with-addition)

  (assert (equal? 10 (((hydra_lib_lists_foldr hydra_lib_math_add) 0) (list 1 2 3 4)))))

(define (test-lists-negfoldr-negsubtraction-vs-foldl)

  (assert (equal? -8 (((hydra_lib_lists_foldr hydra_lib_math_sub) 10) (list 1 2 3)))))

;; group

(define (test-lists-neggroup-negconsecutive-duplicates)

  (assert (equal? (list (list 1 1) (list 2 2 2) (list 3) (list 1)) (hydra_lib_lists_group (list 1 1 2 2 2 3 1)))))

(define (test-lists-neggroup-negno-duplicates)

  (assert (equal? (list (list 1) (list 2) (list 3)) (hydra_lib_lists_group (list 1 2 3)))))

(define (test-lists-neggroup-negall-same)

  (assert (equal? (list (list 1 1 1)) (hydra_lib_lists_group (list 1 1 1)))))

(define (test-lists-neggroup-negempty-list)

  (assert (equal? (list ) (hydra_lib_lists_group (list )))))

(define (test-lists-neggroup-negsingle-element)

  (assert (equal? (list (list 1)) (hydra_lib_lists_group (list 1)))))

;; head

(define (test-lists-neghead-negthree-element-list)

  (assert (equal? 1 (hydra_lib_lists_head (list 1 2 3)))))

(define (test-lists-neghead-negsingle-element-list)

  (assert (equal? 42 (hydra_lib_lists_head (list 42)))))

(define (test-lists-neghead-negnegative-numbers)

  (assert (equal? -1 (hydra_lib_lists_head (list -1 -2 -3)))))

(define (test-lists-neghead-negstring-list)

  (assert (equal? "hello" (hydra_lib_lists_head (list "hello" "world")))))

;; init

(define (test-lists-neginit-negmultiple-elements)

  (assert (equal? (list 1 2 3) (hydra_lib_lists_init (list 1 2 3 4)))))

(define (test-lists-neginit-negtwo-elements)

  (assert (equal? (list 1) (hydra_lib_lists_init (list 1 2)))))

(define (test-lists-neginit-negsingle-element)

  (assert (equal? (list ) (hydra_lib_lists_init (list 1)))))

(define (test-lists-neginit-negstring-list)

  (assert (equal? (list "a" "b") (hydra_lib_lists_init (list "a" "b" "c")))))

;; intercalate

(define (test-lists-negintercalate-negdouble-zero-separator)

  (assert (equal? (list 1 2 3 0 0 4 5 0 0 6 7 8) ((hydra_lib_lists_intercalate (list 0 0)) (list (list 1 2 3) (list 4 5) (list 6 7 8))))))

(define (test-lists-negintercalate-negempty-separator)

  (assert (equal? (list 1 2 3 4) ((hydra_lib_lists_intercalate (list )) (list (list 1 2) (list 3 4))))))

(define (test-lists-negintercalate-negsingle-element-separator)

  (assert (equal? (list 1 99 2 99 3) ((hydra_lib_lists_intercalate (list 99)) (list (list 1) (list 2) (list 3))))))

(define (test-lists-negintercalate-negempty-list-of-lists)

  (assert (equal? (list ) ((hydra_lib_lists_intercalate (list 0)) (list )))))

(define (test-lists-negintercalate-negsingle-list)

  (assert (equal? (list 1 2 3) ((hydra_lib_lists_intercalate (list 0)) (list (list 1 2 3))))))

(define (test-lists-negintercalate-neglists-with-empty-lists)

  (assert (equal? (list 0 1 0) ((hydra_lib_lists_intercalate (list 0)) (list (list ) (list 1) (list ))))))

;; intersperse

(define (test-lists-negintersperse-negstring-interspersion)

  (assert (equal? (list "one" "and" "two" "and" "three") ((hydra_lib_lists_intersperse "and") (list "one" "two" "three")))))

(define (test-lists-negintersperse-negsingle-element)

  (assert (equal? (list "only") ((hydra_lib_lists_intersperse "x") (list "only")))))

(define (test-lists-negintersperse-negempty-list)

  (assert (equal? (list ) ((hydra_lib_lists_intersperse "x") (list )))))

(define (test-lists-negintersperse-negtwo-elements)

  (assert (equal? (list "a" "+" "b") ((hydra_lib_lists_intersperse "+") (list "a" "b")))))

(define (test-lists-negintersperse-negnumber-interspersion)

  (assert (equal? (list 1 0 2 0 3) ((hydra_lib_lists_intersperse 0) (list 1 2 3)))))

;; last

(define (test-lists-neglast-negthree-element-list)

  (assert (equal? 3 (hydra_lib_lists_last (list 1 2 3)))))

(define (test-lists-neglast-negsingle-element-list)

  (assert (equal? 42 (hydra_lib_lists_last (list 42)))))

(define (test-lists-neglast-negnegative-numbers)

  (assert (equal? -3 (hydra_lib_lists_last (list -1 -2 -3)))))

(define (test-lists-neglast-negstring-list)

  (assert (equal? "world" (hydra_lib_lists_last (list "hello" "world")))))

;; length

(define (test-lists-neglength-negthree-elements)

  (assert (equal? 3 (hydra_lib_lists_length (list 1 2 3)))))

(define (test-lists-neglength-negempty-list)

  (assert (equal? 0 (hydra_lib_lists_length (list )))))

(define (test-lists-neglength-negsingle-element)

  (assert (equal? 1 (hydra_lib_lists_length (list 42)))))

(define (test-lists-neglength-negmany-elements)

  (assert (equal? 10 (hydra_lib_lists_length (list 1 2 3 4 5 6 7 8 9 10)))))

(define (test-lists-neglength-negstring-list)

  (assert (equal? 3 (hydra_lib_lists_length (list "a" "b" "c")))))

;; map

(define (test-lists-negmap-negstring-to-uppercase)

  (assert (equal? (list "ONE" "TWO") ((hydra_lib_lists_map hydra_lib_strings_to_upper) (list "one" "two")))))

(define (test-lists-negmap-negempty-list)

  (assert (equal? (list ) ((hydra_lib_lists_map hydra_lib_strings_to_upper) (list )))))

(define (test-lists-negmap-negsingle-element)

  (assert (equal? (list "HELLO") ((hydra_lib_lists_map hydra_lib_strings_to_upper) (list "hello")))))

(define (test-lists-negmap-negnumber-negation)

  (assert (equal? (list -1 -2 -3) ((hydra_lib_lists_map hydra_lib_math_negate) (list 1 2 3)))))

(define (test-lists-negmap-negidentity-function)

  (assert (equal? (list 1 2 3) ((hydra_lib_lists_map hydra_lib_equality_identity) (list 1 2 3)))))

;; nub

(define (test-lists-negnub-negremove-duplicates)

  (assert (equal? (list 1 2 3 4) (hydra_lib_lists_nub (list 1 2 1 3 2 4)))))

(define (test-lists-negnub-negno-duplicates)

  (assert (equal? (list 1 2 3) (hydra_lib_lists_nub (list 1 2 3)))))

(define (test-lists-negnub-negall-duplicates)

  (assert (equal? (list 1) (hydra_lib_lists_nub (list 1 1 1)))))

(define (test-lists-negnub-negempty-list)

  (assert (equal? (list ) (hydra_lib_lists_nub (list )))))

(define (test-lists-negnub-negsingle-element)

  (assert (equal? (list 1) (hydra_lib_lists_nub (list 1)))))

(define (test-lists-negnub-negstring-duplicates)

  (assert (equal? (list "a" "b" "c") (hydra_lib_lists_nub (list "a" "b" "a" "c")))))

;; null

(define (test-lists-negnull-negempty-int-list)

  (assert (equal? #t (hydra_lib_lists_null (list )))))

(define (test-lists-negnull-negsingle-element)

  (assert (equal? #f (hydra_lib_lists_null (list 1)))))

(define (test-lists-negnull-negmultiple-elements)

  (assert (equal? #f (hydra_lib_lists_null (list 1 2 3)))))

(define (test-lists-negnull-negempty-string-list)

  (assert (equal? #t (hydra_lib_lists_null (list )))))

(define (test-lists-negnull-negnon-negempty-string-list)

  (assert (equal? #f (hydra_lib_lists_null (list "a")))))

;; partition

(define (test-lists-negpartition-negpartition-greater-than-3)

  (assert (equal? (list (list 4 5 6) (list 1 2 3)) ((hydra_lib_lists_partition (lambda (x) ((hydra_lib_equality_gt x) 3))) (list 1 2 3 4 5 6)))))

(define (test-lists-negpartition-negpartition-all-elements)

  (assert (equal? (list (list 1 2 3) (list )) ((hydra_lib_lists_partition (lambda (x) ((hydra_lib_equality_lt x) 10))) (list 1 2 3)))))

(define (test-lists-negpartition-negpartition-no-elements)

  (assert (equal? (list (list ) (list 1 2 3)) ((hydra_lib_lists_partition (lambda (x) ((hydra_lib_equality_gt x) 10))) (list 1 2 3)))))

(define (test-lists-negpartition-negpartition-even-numbers)

  (assert (equal? (list (list 2 4 6) (list 1 3 5)) ((hydra_lib_lists_partition (lambda (x) (hydra_lib_math_even x))) (list 1 2 3 4 5 6)))))

(define (test-lists-negpartition-negempty-list)

  (assert (equal? (list (list ) (list )) ((hydra_lib_lists_partition (lambda (x) ((hydra_lib_equality_lt x) 5))) (list )))))

;; pure

(define (test-lists-negpure-negstring-element)

  (assert (equal? (list "one") (hydra_lib_lists_pure "one"))))

(define (test-lists-negpure-negempty-string)

  (assert (equal? (list "") (hydra_lib_lists_pure ""))))

(define (test-lists-negpure-negnumber-element)

  (assert (equal? (list 42) (hydra_lib_lists_pure 42))))

(define (test-lists-negpure-negnegative-number)

  (assert (equal? (list -5) (hydra_lib_lists_pure -5))))

;; replicate

(define (test-lists-negreplicate-negreplicate-three-times)

  (assert (equal? (list 42 42 42) ((hydra_lib_lists_replicate 3) 42))))

(define (test-lists-negreplicate-negreplicate-zero-times)

  (assert (equal? (list ) ((hydra_lib_lists_replicate 0) 1))))

(define (test-lists-negreplicate-negreplicate-once)

  (assert (equal? (list 99) ((hydra_lib_lists_replicate 1) 99))))

(define (test-lists-negreplicate-negreplicate-string)

  (assert (equal? (list "hello" "hello") ((hydra_lib_lists_replicate 2) "hello"))))

;; reverse

(define (test-lists-negreverse-negmultiple-elements)

  (assert (equal? (list 4 3 2 1) (hydra_lib_lists_reverse (list 1 2 3 4)))))

(define (test-lists-negreverse-negsingle-element)

  (assert (equal? (list 1) (hydra_lib_lists_reverse (list 1)))))

(define (test-lists-negreverse-negempty-list)

  (assert (equal? (list ) (hydra_lib_lists_reverse (list )))))

(define (test-lists-negreverse-negtwo-elements)

  (assert (equal? (list 2 1) (hydra_lib_lists_reverse (list 1 2)))))

(define (test-lists-negreverse-negstring-list)

  (assert (equal? (list "c" "b" "a") (hydra_lib_lists_reverse (list "a" "b" "c")))))

;; safeHead

(define (test-lists-negsafehead-negnon-negempty-int-list)

  (assert (equal? (list 'just 1) (hydra_lib_lists_safe_head (list 1 2 3)))))

(define (test-lists-negsafehead-negempty-int-list)

  (assert (equal? (list 'nothing) (hydra_lib_lists_safe_head (list )))))

(define (test-lists-negsafehead-negsingle-element)

  (assert (equal? (list 'just 42) (hydra_lib_lists_safe_head (list 42)))))

(define (test-lists-negsafehead-negnon-negempty-string-list)

  (assert (equal? (list 'just "hello") (hydra_lib_lists_safe_head (list "hello" "world")))))

(define (test-lists-negsafehead-negempty-string-list)

  (assert (equal? (list 'nothing) (hydra_lib_lists_safe_head (list )))))

;; singleton

(define (test-lists-negsingleton-negnumber-element)

  (assert (equal? (list 42) (hydra_lib_lists_singleton 42))))

(define (test-lists-negsingleton-negnegative-number)

  (assert (equal? (list -1) (hydra_lib_lists_singleton -1))))

(define (test-lists-negsingleton-negzero)

  (assert (equal? (list 0) (hydra_lib_lists_singleton 0))))

(define (test-lists-negsingleton-negstring-element)

  (assert (equal? (list "hello") (hydra_lib_lists_singleton "hello"))))

;; sort

(define (test-lists-negsort-negunsorted-numbers)

  (assert (equal? (list 1 1 3 4 5) (hydra_lib_lists_sort (list 3 1 4 1 5)))))

(define (test-lists-negsort-negalready-sorted)

  (assert (equal? (list 1 2 3) (hydra_lib_lists_sort (list 1 2 3)))))

(define (test-lists-negsort-negreverse-sorted)

  (assert (equal? (list 1 2 3) (hydra_lib_lists_sort (list 3 2 1)))))

(define (test-lists-negsort-negsingle-element)

  (assert (equal? (list 1) (hydra_lib_lists_sort (list 1)))))

(define (test-lists-negsort-negempty-list)

  (assert (equal? (list ) (hydra_lib_lists_sort (list )))))

(define (test-lists-negsort-negduplicates)

  (assert (equal? (list 1 1 2 2 3) (hydra_lib_lists_sort (list 2 1 2 3 1)))))

(define (test-lists-negsort-negstring-sort)

  (assert (equal? (list "apple" "banana" "zebra") (hydra_lib_lists_sort (list "zebra" "apple" "banana")))))

;; sortOn

(define (test-lists-negsorton-negsort-by-string-length)

  (assert (equal? (list "hi" "hello" "world") ((hydra_lib_lists_sort_on hydra_lib_strings_length) (list "hello" "hi" "world")))))

(define (test-lists-negsorton-negempty-string-list)

  (assert (equal? (list ) ((hydra_lib_lists_sort_on hydra_lib_strings_length) (list )))))

(define (test-lists-negsorton-negsingle-string-element)

  (assert (equal? (list "test") ((hydra_lib_lists_sort_on hydra_lib_strings_length) (list "test")))))

(define (test-lists-negsorton-negsort-by-negation)

  (assert (equal? (list 3 2 1) ((hydra_lib_lists_sort_on hydra_lib_math_negate) (list 1 3 2)))))

(define (test-lists-negsorton-negsort-by-absolute-value)

  (assert (equal? (list -1 2 -3) ((hydra_lib_lists_sort_on hydra_lib_math_abs) (list -1 -3 2)))))

;; span

(define (test-lists-negspan-negspan-less-than-3)

  (assert (equal? (list (list 1 2) (list 3 1 2)) ((hydra_lib_lists_span (lambda (x) ((hydra_lib_equality_lt x) 3))) (list 1 2 3 1 2)))))

(define (test-lists-negspan-negspan-all-elements)

  (assert (equal? (list (list 1 2 3) (list )) ((hydra_lib_lists_span (lambda (x) ((hydra_lib_equality_lt x) 10))) (list 1 2 3)))))

(define (test-lists-negspan-negspan-no-elements)

  (assert (equal? (list (list ) (list 1 2 3)) ((hydra_lib_lists_span (lambda (x) ((hydra_lib_equality_gt x) 10))) (list 1 2 3)))))

(define (test-lists-negspan-negempty-list)

  (assert (equal? (list (list ) (list )) ((hydra_lib_lists_span (lambda (x) ((hydra_lib_equality_lt x) 5))) (list )))))

;; tail

(define (test-lists-negtail-negmultiple-elements)

  (assert (equal? (list 2 3 4) (hydra_lib_lists_tail (list 1 2 3 4)))))

(define (test-lists-negtail-negtwo-elements)

  (assert (equal? (list 2) (hydra_lib_lists_tail (list 1 2)))))

(define (test-lists-negtail-negsingle-element)

  (assert (equal? (list ) (hydra_lib_lists_tail (list 1)))))

(define (test-lists-negtail-negstring-list)

  (assert (equal? (list "b" "c") (hydra_lib_lists_tail (list "a" "b" "c")))))

;; take

(define (test-lists-negtake-negtake-from-beginning)

  (assert (equal? (list 1 2) ((hydra_lib_lists_take 2) (list 1 2 3 4 5)))))

(define (test-lists-negtake-negtake-zero-elements)

  (assert (equal? (list ) ((hydra_lib_lists_take 0) (list 1 2 3)))))

(define (test-lists-negtake-negtake-all-elements)

  (assert (equal? (list 1 2 3) ((hydra_lib_lists_take 3) (list 1 2 3)))))

(define (test-lists-negtake-negtake-more-than-length)

  (assert (equal? (list 1 2) ((hydra_lib_lists_take 5) (list 1 2)))))

(define (test-lists-negtake-negtake-from-empty-list)

  (assert (equal? (list ) ((hydra_lib_lists_take 3) (list )))))

(define (test-lists-negtake-negtake-negative-amount)

  (assert (equal? (list ) ((hydra_lib_lists_take -1) (list 1 2 3)))))

;; transpose

(define (test-lists-negtranspose-negsquare-matrix)

  (assert (equal? (list (list 1 4) (list 2 5) (list 3 6)) (hydra_lib_lists_transpose (list (list 1 2 3) (list 4 5 6))))))

(define (test-lists-negtranspose-negempty-lists)

  (assert (equal? (list ) (hydra_lib_lists_transpose (list )))))

(define (test-lists-negtranspose-negsingle-row)

  (assert (equal? (list (list 1) (list 2) (list 3)) (hydra_lib_lists_transpose (list (list 1 2 3))))))

(define (test-lists-negtranspose-negsingle-column)

  (assert (equal? (list (list 1 2 3)) (hydra_lib_lists_transpose (list (list 1) (list 2) (list 3))))))

(define (test-lists-negtranspose-negragged-matrix)

  (assert (equal? (list (list 1 3 4) (list 2 5) (list 6)) (hydra_lib_lists_transpose (list (list 1 2) (list 3) (list 4 5 6))))))

;; zip

(define (test-lists-negzip-negequal-length-lists)

  (assert (equal? (list (list 1 "a") (list 2 "b") (list 3 "c")) ((hydra_lib_lists_zip (list 1 2 3)) (list "a" "b" "c")))))

(define (test-lists-negzip-negfirst-list-shorter)

  (assert (equal? (list (list 1 "a") (list 2 "b")) ((hydra_lib_lists_zip (list 1 2)) (list "a" "b" "c")))))

(define (test-lists-negzip-negsecond-list-shorter)

  (assert (equal? (list (list 1 "a") (list 2 "b")) ((hydra_lib_lists_zip (list 1 2 3)) (list "a" "b")))))

(define (test-lists-negzip-negempty-first-list)

  (assert (equal? (list ) ((hydra_lib_lists_zip (list )) (list "a" "b")))))

(define (test-lists-negzip-negempty-second-list)

  (assert (equal? (list ) ((hydra_lib_lists_zip (list 1 2)) (list )))))

(define (test-lists-negzip-negboth-empty-lists)

  (assert (equal? (list ) ((hydra_lib_lists_zip (list )) (list )))))

;; zipWith

(define (test-lists-negzipwith-negaddition)

  (assert (equal? (list 5 7 9) (((hydra_lib_lists_zip_with hydra_lib_math_add) (list 1 2 3)) (list 4 5 6)))))

(define (test-lists-negzipwith-negfirst-list-shorter)

  (assert (equal? (list 5 7) (((hydra_lib_lists_zip_with hydra_lib_math_add) (list 1 2)) (list 4 5 6)))))

(define (test-lists-negzipwith-negsecond-list-shorter)

  (assert (equal? (list 5 7) (((hydra_lib_lists_zip_with hydra_lib_math_add) (list 1 2 3)) (list 4 5)))))

(define (test-lists-negzipwith-negempty-first-list)

  (assert (equal? (list ) (((hydra_lib_lists_zip_with hydra_lib_math_add) (list )) (list 1 2 3)))))

(define (test-lists-negzipwith-negempty-second-list)

  (assert (equal? (list ) (((hydra_lib_lists_zip_with hydra_lib_math_add) (list 1 2 3)) (list )))))

(define (test-lists-negzipwith-negstring-concatenation)

  (assert (equal? (list "a1" "b2") (((hydra_lib_lists_zip_with hydra_lib_strings_cat2) (list "a" "b")) (list "1" "2")))))
