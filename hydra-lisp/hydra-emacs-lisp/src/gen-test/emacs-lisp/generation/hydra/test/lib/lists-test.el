;;; Note: this is an automatically generated file. Do not edit.
;;; hydra.lib.lists primitives

(require 'ert)

;; apply

;; string transformations

(ert-deftest test-apply-negstring-transformations-negstring-transformations ()

  (should (equal (list "ONE" "TWO" "THREE" "one" "two" "three") ((hydra_lib_lists_apply (list hydra_lib_strings_to_upper hydra_lib_strings_to_lower)) (list "One" "Two" "Three")))))

;; edge cases

(ert-deftest test-apply-negedge-cases-negempty-function-list ()

  (should (equal (list ) ((hydra_lib_lists_apply (list )) (list "a" "b")))))

(ert-deftest test-apply-negedge-cases-negempty-input-list ()

  (should (equal (list ) ((hydra_lib_lists_apply (list hydra_lib_strings_to_upper)) (list )))))

(ert-deftest test-apply-negedge-cases-negsingle-function ()

  (should (equal (list "HELLO") ((hydra_lib_lists_apply (list hydra_lib_strings_to_upper)) (list "hello")))))

(ert-deftest test-apply-negedge-cases-negsingle-input ()

  (should (equal (list "TEST" "test") ((hydra_lib_lists_apply (list hydra_lib_strings_to_upper hydra_lib_strings_to_lower)) (list "Test")))))

;; at

(ert-deftest test-at-negfirst-element ()

  (should (equal 1 ((hydra_lib_lists_at 0) (list 1 2 3)))))

(ert-deftest test-at-negmiddle-element ()

  (should (equal 2 ((hydra_lib_lists_at 1) (list 1 2 3)))))

(ert-deftest test-at-neglast-element ()

  (should (equal 3 ((hydra_lib_lists_at 2) (list 1 2 3)))))

(ert-deftest test-at-negsingle-element-list ()

  (should (equal 42 ((hydra_lib_lists_at 0) (list 42)))))

(ert-deftest test-at-negstring-list-access ()

  (should (equal "world" ((hydra_lib_lists_at 1) (list "hello" "world")))))

;; bind

(ert-deftest test-bind-negnegation-function ()

  (should (equal (list -1 -2 -3 -4) ((hydra_lib_lists_bind (list 1 2 3 4)) (lambda (x) (hydra_lib_lists_pure (hydra_lib_math_negate x)))))))

(ert-deftest test-bind-negempty-list ()

  (should (equal (list ) ((hydra_lib_lists_bind (list )) (lambda (x) (hydra_lib_lists_pure (hydra_lib_math_negate x)))))))

(ert-deftest test-bind-negsingle-element ()

  (should (equal (list -5) ((hydra_lib_lists_bind (list 5)) (lambda (x) (hydra_lib_lists_pure (hydra_lib_math_negate x)))))))

(ert-deftest test-bind-negduplicate-elements ()

  (should (equal (list -1 -1 -2) ((hydra_lib_lists_bind (list 1 1 2)) (lambda (x) (hydra_lib_lists_pure (hydra_lib_math_negate x)))))))

;; concat

(ert-deftest test-concat-negmultiple-non-negempty-lists ()

  (should (equal (list 1 2 3 4 5 6 7 8) (hydra_lib_lists_concat (list (list 1 2 3) (list 4 5) (list 6 7 8))))))

(ert-deftest test-concat-negempty-lists-included ()

  (should (equal (list 1 2 3) (hydra_lib_lists_concat (list (list ) (list 1 2) (list ) (list 3))))))

(ert-deftest test-concat-negsingle-list ()

  (should (equal (list 1 2 3) (hydra_lib_lists_concat (list (list 1 2 3))))))

(ert-deftest test-concat-negall-empty-lists ()

  (should (equal (list ) (hydra_lib_lists_concat (list (list ) (list ) (list ))))))

(ert-deftest test-concat-negempty-list-of-lists ()

  (should (equal (list ) (hydra_lib_lists_concat (list )))))

;; concat2

(ert-deftest test-concat2-negtwo-non-negempty-lists ()

  (should (equal (list 1 2 3 4) ((hydra_lib_lists_concat2 (list 1 2)) (list 3 4)))))

(ert-deftest test-concat2-negfirst-list-empty ()

  (should (equal (list 1 2) ((hydra_lib_lists_concat2 (list )) (list 1 2)))))

(ert-deftest test-concat2-negsecond-list-empty ()

  (should (equal (list 1 2) ((hydra_lib_lists_concat2 (list 1 2)) (list )))))

(ert-deftest test-concat2-negboth-lists-empty ()

  (should (equal (list ) ((hydra_lib_lists_concat2 (list )) (list )))))

(ert-deftest test-concat2-negsingle-elements ()

  (should (equal (list 1 2) ((hydra_lib_lists_concat2 (list 1)) (list 2)))))

(ert-deftest test-concat2-negstring-lists ()

  (should (equal (list "a" "b" "c" "d") ((hydra_lib_lists_concat2 (list "a" "b")) (list "c" "d")))))

;; cons

(ert-deftest test-cons-negcons-to-non-negempty-list ()

  (should (equal (list 1 2 3) ((hydra_lib_lists_cons 1) (list 2 3)))))

(ert-deftest test-cons-negcons-to-empty-list ()

  (should (equal (list 1) ((hydra_lib_lists_cons 1) (list )))))

(ert-deftest test-cons-negcons-negative-number ()

  (should (equal (list -1 2 3) ((hydra_lib_lists_cons -1) (list 2 3)))))

(ert-deftest test-cons-negcons-string ()

  (should (equal (list "hello" "world") ((hydra_lib_lists_cons "hello") (list "world")))))

;; drop

(ert-deftest test-drop-negdrop-from-beginning ()

  (should (equal (list 3 4 5) ((hydra_lib_lists_drop 2) (list 1 2 3 4 5)))))

(ert-deftest test-drop-negdrop-zero-elements ()

  (should (equal (list 1 2 3) ((hydra_lib_lists_drop 0) (list 1 2 3)))))

(ert-deftest test-drop-negdrop-all-elements ()

  (should (equal (list ) ((hydra_lib_lists_drop 3) (list 1 2 3)))))

(ert-deftest test-drop-negdrop-more-than-length ()

  (should (equal (list ) ((hydra_lib_lists_drop 5) (list 1 2)))))

(ert-deftest test-drop-negdrop-from-empty-list ()

  (should (equal (list ) ((hydra_lib_lists_drop 3) (list )))))

(ert-deftest test-drop-negdrop-negative-amount ()

  (should (equal (list 1 2 3) ((hydra_lib_lists_drop -1) (list 1 2 3)))))

;; dropWhile

(ert-deftest test-dropwhile-negdrop-while-less-than-3 ()

  (should (equal (list 3 2 1) ((hydra_lib_lists_drop_while (lambda (x) ((hydra_lib_equality_lt x) 3))) (list 1 2 3 2 1)))))

(ert-deftest test-dropwhile-negdrop-all-elements ()

  (should (equal (list ) ((hydra_lib_lists_drop_while (lambda (x) ((hydra_lib_equality_lt x) 10))) (list 1 2 3)))))

(ert-deftest test-dropwhile-negdrop-no-elements ()

  (should (equal (list 1 2 3) ((hydra_lib_lists_drop_while (lambda (x) ((hydra_lib_equality_lt x) 0))) (list 1 2 3)))))

(ert-deftest test-dropwhile-negempty-list ()

  (should (equal (list ) ((hydra_lib_lists_drop_while (lambda (x) ((hydra_lib_equality_lt x) 5))) (list )))))

;; elem

(ert-deftest test-elem-negelement-present ()

  (should (equal t ((hydra_lib_lists_elem 2) (list 1 2 3)))))

(ert-deftest test-elem-negelement-not-present ()

  (should (equal nil ((hydra_lib_lists_elem 4) (list 1 2 3)))))

(ert-deftest test-elem-negempty-list ()

  (should (equal nil ((hydra_lib_lists_elem 1) (list )))))

(ert-deftest test-elem-negsingle-element-present ()

  (should (equal t ((hydra_lib_lists_elem 1) (list 1)))))

(ert-deftest test-elem-negsingle-element-not-present ()

  (should (equal nil ((hydra_lib_lists_elem 2) (list 1)))))

(ert-deftest test-elem-negduplicate-elements ()

  (should (equal t ((hydra_lib_lists_elem 2) (list 1 2 2 3)))))

(ert-deftest test-elem-negstring-element-present ()

  (should (equal t ((hydra_lib_lists_elem "hello") (list "world" "hello" "test")))))

(ert-deftest test-elem-negstring-element-not-present ()

  (should (equal nil ((hydra_lib_lists_elem "missing") (list "world" "hello")))))

;; filter

(ert-deftest test-filter-negfilter-positive-numbers ()

  (should (equal (list 2 4 5) ((hydra_lib_lists_filter (lambda (x) ((hydra_lib_equality_gt x) 0))) (list -1 2 -3 4 5)))))

(ert-deftest test-filter-negfilter-all-elements ()

  (should (equal (list 1 2 3) ((hydra_lib_lists_filter (lambda (x) ((hydra_lib_equality_lt x) 10))) (list 1 2 3)))))

(ert-deftest test-filter-negfilter-no-elements ()

  (should (equal (list ) ((hydra_lib_lists_filter (lambda (x) ((hydra_lib_equality_gt x) 10))) (list 1 2 3)))))

(ert-deftest test-filter-negempty-list ()

  (should (equal (list ) ((hydra_lib_lists_filter (lambda (x) ((hydra_lib_equality_gt x) 0))) (list )))))

;; find

(ert-deftest test-find-negfind-existing-element ()

  (should (equal (list :just 4) ((hydra_lib_lists_find (lambda (x) ((hydra_lib_equality_gt x) 3))) (list 1 2 4 5)))))

(ert-deftest test-find-negfind-first-matching ()

  (should (equal (list :just 1) ((hydra_lib_lists_find (lambda (x) ((hydra_lib_equality_gt x) 0))) (list 1 2 3)))))

(ert-deftest test-find-negfind-no-match ()

  (should (equal (list :nothing) ((hydra_lib_lists_find (lambda (x) ((hydra_lib_equality_gt x) 10))) (list 1 2 3)))))

(ert-deftest test-find-negfind-in-empty-list ()

  (should (equal (list :nothing) ((hydra_lib_lists_find (lambda (x) ((hydra_lib_equality_gt x) 0))) (list )))))

(ert-deftest test-find-negfind-single-element ()

  (should (equal (list :just 42) ((hydra_lib_lists_find (lambda (x) ((hydra_lib_equality_equal x) 42))) (list 42)))))

;; foldl

(ert-deftest test-foldl-negsum-with-addition ()

  (should (equal 10 (((hydra_lib_lists_foldl hydra_lib_math_add) 0) (list 1 2 3 4)))))

(ert-deftest test-foldl-negproduct-with-multiplication ()

  (should (equal 24 (((hydra_lib_lists_foldl hydra_lib_math_mul) 1) (list 2 3 4)))))

(ert-deftest test-foldl-negempty-list ()

  (should (equal 5 (((hydra_lib_lists_foldl hydra_lib_math_add) 5) (list )))))

(ert-deftest test-foldl-negsingle-element ()

  (should (equal 15 (((hydra_lib_lists_foldl hydra_lib_math_add) 10) (list 5)))))

(ert-deftest test-foldl-negsubtraction-fold ()

  (should (equal 4 (((hydra_lib_lists_foldl hydra_lib_math_sub) 10) (list 1 2 3)))))

;; foldr

(ert-deftest test-foldr-negsubtraction-fold-right ()

  (should (equal 2 (((hydra_lib_lists_foldr hydra_lib_math_sub) 0) (list 1 2 3)))))

(ert-deftest test-foldr-negempty-list ()

  (should (equal 5 (((hydra_lib_lists_foldr hydra_lib_math_add) 5) (list )))))

(ert-deftest test-foldr-negsingle-element ()

  (should (equal 15 (((hydra_lib_lists_foldr hydra_lib_math_add) 10) (list 5)))))

(ert-deftest test-foldr-negsum-with-addition ()

  (should (equal 10 (((hydra_lib_lists_foldr hydra_lib_math_add) 0) (list 1 2 3 4)))))

(ert-deftest test-foldr-negsubtraction-vs-foldl ()

  (should (equal -8 (((hydra_lib_lists_foldr hydra_lib_math_sub) 10) (list 1 2 3)))))

;; group

(ert-deftest test-group-negconsecutive-duplicates ()

  (should (equal (list (list 1 1) (list 2 2 2) (list 3) (list 1)) (hydra_lib_lists_group (list 1 1 2 2 2 3 1)))))

(ert-deftest test-group-negno-duplicates ()

  (should (equal (list (list 1) (list 2) (list 3)) (hydra_lib_lists_group (list 1 2 3)))))

(ert-deftest test-group-negall-same ()

  (should (equal (list (list 1 1 1)) (hydra_lib_lists_group (list 1 1 1)))))

(ert-deftest test-group-negempty-list ()

  (should (equal (list ) (hydra_lib_lists_group (list )))))

(ert-deftest test-group-negsingle-element ()

  (should (equal (list (list 1)) (hydra_lib_lists_group (list 1)))))

;; head

(ert-deftest test-head-negthree-element-list ()

  (should (equal 1 (hydra_lib_lists_head (list 1 2 3)))))

(ert-deftest test-head-negsingle-element-list ()

  (should (equal 42 (hydra_lib_lists_head (list 42)))))

(ert-deftest test-head-negnegative-numbers ()

  (should (equal -1 (hydra_lib_lists_head (list -1 -2 -3)))))

(ert-deftest test-head-negstring-list ()

  (should (equal "hello" (hydra_lib_lists_head (list "hello" "world")))))

;; init

(ert-deftest test-init-negmultiple-elements ()

  (should (equal (list 1 2 3) (hydra_lib_lists_init (list 1 2 3 4)))))

(ert-deftest test-init-negtwo-elements ()

  (should (equal (list 1) (hydra_lib_lists_init (list 1 2)))))

(ert-deftest test-init-negsingle-element ()

  (should (equal (list ) (hydra_lib_lists_init (list 1)))))

(ert-deftest test-init-negstring-list ()

  (should (equal (list "a" "b") (hydra_lib_lists_init (list "a" "b" "c")))))

;; intercalate

(ert-deftest test-intercalate-negdouble-zero-separator ()

  (should (equal (list 1 2 3 0 0 4 5 0 0 6 7 8) ((hydra_lib_lists_intercalate (list 0 0)) (list (list 1 2 3) (list 4 5) (list 6 7 8))))))

(ert-deftest test-intercalate-negempty-separator ()

  (should (equal (list 1 2 3 4) ((hydra_lib_lists_intercalate (list )) (list (list 1 2) (list 3 4))))))

(ert-deftest test-intercalate-negsingle-element-separator ()

  (should (equal (list 1 99 2 99 3) ((hydra_lib_lists_intercalate (list 99)) (list (list 1) (list 2) (list 3))))))

(ert-deftest test-intercalate-negempty-list-of-lists ()

  (should (equal (list ) ((hydra_lib_lists_intercalate (list 0)) (list )))))

(ert-deftest test-intercalate-negsingle-list ()

  (should (equal (list 1 2 3) ((hydra_lib_lists_intercalate (list 0)) (list (list 1 2 3))))))

(ert-deftest test-intercalate-neglists-with-empty-lists ()

  (should (equal (list 0 1 0) ((hydra_lib_lists_intercalate (list 0)) (list (list ) (list 1) (list ))))))

;; intersperse

(ert-deftest test-intersperse-negstring-interspersion ()

  (should (equal (list "one" "and" "two" "and" "three") ((hydra_lib_lists_intersperse "and") (list "one" "two" "three")))))

(ert-deftest test-intersperse-negsingle-element ()

  (should (equal (list "only") ((hydra_lib_lists_intersperse "x") (list "only")))))

(ert-deftest test-intersperse-negempty-list ()

  (should (equal (list ) ((hydra_lib_lists_intersperse "x") (list )))))

(ert-deftest test-intersperse-negtwo-elements ()

  (should (equal (list "a" "+" "b") ((hydra_lib_lists_intersperse "+") (list "a" "b")))))

(ert-deftest test-intersperse-negnumber-interspersion ()

  (should (equal (list 1 0 2 0 3) ((hydra_lib_lists_intersperse 0) (list 1 2 3)))))

;; last

(ert-deftest test-last-negthree-element-list ()

  (should (equal 3 (hydra_lib_lists_last (list 1 2 3)))))

(ert-deftest test-last-negsingle-element-list ()

  (should (equal 42 (hydra_lib_lists_last (list 42)))))

(ert-deftest test-last-negnegative-numbers ()

  (should (equal -3 (hydra_lib_lists_last (list -1 -2 -3)))))

(ert-deftest test-last-negstring-list ()

  (should (equal "world" (hydra_lib_lists_last (list "hello" "world")))))

;; length

(ert-deftest test-length-negthree-elements ()

  (should (equal 3 (hydra_lib_lists_length (list 1 2 3)))))

(ert-deftest test-length-negempty-list ()

  (should (equal 0 (hydra_lib_lists_length (list )))))

(ert-deftest test-length-negsingle-element ()

  (should (equal 1 (hydra_lib_lists_length (list 42)))))

(ert-deftest test-length-negmany-elements ()

  (should (equal 10 (hydra_lib_lists_length (list 1 2 3 4 5 6 7 8 9 10)))))

(ert-deftest test-length-negstring-list ()

  (should (equal 3 (hydra_lib_lists_length (list "a" "b" "c")))))

;; map

(ert-deftest test-map-negstring-to-uppercase ()

  (should (equal (list "ONE" "TWO") ((hydra_lib_lists_map hydra_lib_strings_to_upper) (list "one" "two")))))

(ert-deftest test-map-negempty-list ()

  (should (equal (list ) ((hydra_lib_lists_map hydra_lib_strings_to_upper) (list )))))

(ert-deftest test-map-negsingle-element ()

  (should (equal (list "HELLO") ((hydra_lib_lists_map hydra_lib_strings_to_upper) (list "hello")))))

(ert-deftest test-map-negnumber-negation ()

  (should (equal (list -1 -2 -3) ((hydra_lib_lists_map hydra_lib_math_negate) (list 1 2 3)))))

(ert-deftest test-map-negidentity-function ()

  (should (equal (list 1 2 3) ((hydra_lib_lists_map hydra_lib_equality_identity) (list 1 2 3)))))

;; nub

(ert-deftest test-nub-negremove-duplicates ()

  (should (equal (list 1 2 3 4) (hydra_lib_lists_nub (list 1 2 1 3 2 4)))))

(ert-deftest test-nub-negno-duplicates ()

  (should (equal (list 1 2 3) (hydra_lib_lists_nub (list 1 2 3)))))

(ert-deftest test-nub-negall-duplicates ()

  (should (equal (list 1) (hydra_lib_lists_nub (list 1 1 1)))))

(ert-deftest test-nub-negempty-list ()

  (should (equal (list ) (hydra_lib_lists_nub (list )))))

(ert-deftest test-nub-negsingle-element ()

  (should (equal (list 1) (hydra_lib_lists_nub (list 1)))))

(ert-deftest test-nub-negstring-duplicates ()

  (should (equal (list "a" "b" "c") (hydra_lib_lists_nub (list "a" "b" "a" "c")))))

;; null

(ert-deftest test-null-negempty-int-list ()

  (should (equal t (hydra_lib_lists_null (list )))))

(ert-deftest test-null-negsingle-element ()

  (should (equal nil (hydra_lib_lists_null (list 1)))))

(ert-deftest test-null-negmultiple-elements ()

  (should (equal nil (hydra_lib_lists_null (list 1 2 3)))))

(ert-deftest test-null-negempty-string-list ()

  (should (equal t (hydra_lib_lists_null (list )))))

(ert-deftest test-null-negnon-negempty-string-list ()

  (should (equal nil (hydra_lib_lists_null (list "a")))))

;; partition

(ert-deftest test-partition-negpartition-greater-than-3 ()

  (should (equal (list (list 4 5 6) (list 1 2 3)) ((hydra_lib_lists_partition (lambda (x) ((hydra_lib_equality_gt x) 3))) (list 1 2 3 4 5 6)))))

(ert-deftest test-partition-negpartition-all-elements ()

  (should (equal (list (list 1 2 3) (list )) ((hydra_lib_lists_partition (lambda (x) ((hydra_lib_equality_lt x) 10))) (list 1 2 3)))))

(ert-deftest test-partition-negpartition-no-elements ()

  (should (equal (list (list ) (list 1 2 3)) ((hydra_lib_lists_partition (lambda (x) ((hydra_lib_equality_gt x) 10))) (list 1 2 3)))))

(ert-deftest test-partition-negpartition-even-numbers ()

  (should (equal (list (list 2 4 6) (list 1 3 5)) ((hydra_lib_lists_partition (lambda (x) (hydra_lib_math_even x))) (list 1 2 3 4 5 6)))))

(ert-deftest test-partition-negempty-list ()

  (should (equal (list (list ) (list )) ((hydra_lib_lists_partition (lambda (x) ((hydra_lib_equality_lt x) 5))) (list )))))

;; pure

(ert-deftest test-pure-negstring-element ()

  (should (equal (list "one") (hydra_lib_lists_pure "one"))))

(ert-deftest test-pure-negempty-string ()

  (should (equal (list "") (hydra_lib_lists_pure ""))))

(ert-deftest test-pure-negnumber-element ()

  (should (equal (list 42) (hydra_lib_lists_pure 42))))

(ert-deftest test-pure-negnegative-number ()

  (should (equal (list -5) (hydra_lib_lists_pure -5))))

;; replicate

(ert-deftest test-replicate-negreplicate-three-times ()

  (should (equal (list 42 42 42) ((hydra_lib_lists_replicate 3) 42))))

(ert-deftest test-replicate-negreplicate-zero-times ()

  (should (equal (list ) ((hydra_lib_lists_replicate 0) 1))))

(ert-deftest test-replicate-negreplicate-once ()

  (should (equal (list 99) ((hydra_lib_lists_replicate 1) 99))))

(ert-deftest test-replicate-negreplicate-string ()

  (should (equal (list "hello" "hello") ((hydra_lib_lists_replicate 2) "hello"))))

;; reverse

(ert-deftest test-reverse-negmultiple-elements ()

  (should (equal (list 4 3 2 1) (hydra_lib_lists_reverse (list 1 2 3 4)))))

(ert-deftest test-reverse-negsingle-element ()

  (should (equal (list 1) (hydra_lib_lists_reverse (list 1)))))

(ert-deftest test-reverse-negempty-list ()

  (should (equal (list ) (hydra_lib_lists_reverse (list )))))

(ert-deftest test-reverse-negtwo-elements ()

  (should (equal (list 2 1) (hydra_lib_lists_reverse (list 1 2)))))

(ert-deftest test-reverse-negstring-list ()

  (should (equal (list "c" "b" "a") (hydra_lib_lists_reverse (list "a" "b" "c")))))

;; safeHead

(ert-deftest test-safehead-negnon-negempty-int-list ()

  (should (equal (list :just 1) (hydra_lib_lists_safe_head (list 1 2 3)))))

(ert-deftest test-safehead-negempty-int-list ()

  (should (equal (list :nothing) (hydra_lib_lists_safe_head (list )))))

(ert-deftest test-safehead-negsingle-element ()

  (should (equal (list :just 42) (hydra_lib_lists_safe_head (list 42)))))

(ert-deftest test-safehead-negnon-negempty-string-list ()

  (should (equal (list :just "hello") (hydra_lib_lists_safe_head (list "hello" "world")))))

(ert-deftest test-safehead-negempty-string-list ()

  (should (equal (list :nothing) (hydra_lib_lists_safe_head (list )))))

;; singleton

(ert-deftest test-singleton-negnumber-element ()

  (should (equal (list 42) (hydra_lib_lists_singleton 42))))

(ert-deftest test-singleton-negnegative-number ()

  (should (equal (list -1) (hydra_lib_lists_singleton -1))))

(ert-deftest test-singleton-negzero ()

  (should (equal (list 0) (hydra_lib_lists_singleton 0))))

(ert-deftest test-singleton-negstring-element ()

  (should (equal (list "hello") (hydra_lib_lists_singleton "hello"))))

;; sort

(ert-deftest test-sort-negunsorted-numbers ()

  (should (equal (list 1 1 3 4 5) (hydra_lib_lists_sort (list 3 1 4 1 5)))))

(ert-deftest test-sort-negalready-sorted ()

  (should (equal (list 1 2 3) (hydra_lib_lists_sort (list 1 2 3)))))

(ert-deftest test-sort-negreverse-sorted ()

  (should (equal (list 1 2 3) (hydra_lib_lists_sort (list 3 2 1)))))

(ert-deftest test-sort-negsingle-element ()

  (should (equal (list 1) (hydra_lib_lists_sort (list 1)))))

(ert-deftest test-sort-negempty-list ()

  (should (equal (list ) (hydra_lib_lists_sort (list )))))

(ert-deftest test-sort-negduplicates ()

  (should (equal (list 1 1 2 2 3) (hydra_lib_lists_sort (list 2 1 2 3 1)))))

(ert-deftest test-sort-negstring-sort ()

  (should (equal (list "apple" "banana" "zebra") (hydra_lib_lists_sort (list "zebra" "apple" "banana")))))

;; sortOn

(ert-deftest test-sorton-negsort-by-string-length ()

  (should (equal (list "hi" "hello" "world") ((hydra_lib_lists_sort_on hydra_lib_strings_length) (list "hello" "hi" "world")))))

(ert-deftest test-sorton-negempty-string-list ()

  (should (equal (list ) ((hydra_lib_lists_sort_on hydra_lib_strings_length) (list )))))

(ert-deftest test-sorton-negsingle-string-element ()

  (should (equal (list "test") ((hydra_lib_lists_sort_on hydra_lib_strings_length) (list "test")))))

(ert-deftest test-sorton-negsort-by-negation ()

  (should (equal (list 3 2 1) ((hydra_lib_lists_sort_on hydra_lib_math_negate) (list 1 3 2)))))

(ert-deftest test-sorton-negsort-by-absolute-value ()

  (should (equal (list -1 2 -3) ((hydra_lib_lists_sort_on hydra_lib_math_abs) (list -1 -3 2)))))

;; span

(ert-deftest test-span-negspan-less-than-3 ()

  (should (equal (list (list 1 2) (list 3 1 2)) ((hydra_lib_lists_span (lambda (x) ((hydra_lib_equality_lt x) 3))) (list 1 2 3 1 2)))))

(ert-deftest test-span-negspan-all-elements ()

  (should (equal (list (list 1 2 3) (list )) ((hydra_lib_lists_span (lambda (x) ((hydra_lib_equality_lt x) 10))) (list 1 2 3)))))

(ert-deftest test-span-negspan-no-elements ()

  (should (equal (list (list ) (list 1 2 3)) ((hydra_lib_lists_span (lambda (x) ((hydra_lib_equality_gt x) 10))) (list 1 2 3)))))

(ert-deftest test-span-negempty-list ()

  (should (equal (list (list ) (list )) ((hydra_lib_lists_span (lambda (x) ((hydra_lib_equality_lt x) 5))) (list )))))

;; tail

(ert-deftest test-tail-negmultiple-elements ()

  (should (equal (list 2 3 4) (hydra_lib_lists_tail (list 1 2 3 4)))))

(ert-deftest test-tail-negtwo-elements ()

  (should (equal (list 2) (hydra_lib_lists_tail (list 1 2)))))

(ert-deftest test-tail-negsingle-element ()

  (should (equal (list ) (hydra_lib_lists_tail (list 1)))))

(ert-deftest test-tail-negstring-list ()

  (should (equal (list "b" "c") (hydra_lib_lists_tail (list "a" "b" "c")))))

;; take

(ert-deftest test-take-negtake-from-beginning ()

  (should (equal (list 1 2) ((hydra_lib_lists_take 2) (list 1 2 3 4 5)))))

(ert-deftest test-take-negtake-zero-elements ()

  (should (equal (list ) ((hydra_lib_lists_take 0) (list 1 2 3)))))

(ert-deftest test-take-negtake-all-elements ()

  (should (equal (list 1 2 3) ((hydra_lib_lists_take 3) (list 1 2 3)))))

(ert-deftest test-take-negtake-more-than-length ()

  (should (equal (list 1 2) ((hydra_lib_lists_take 5) (list 1 2)))))

(ert-deftest test-take-negtake-from-empty-list ()

  (should (equal (list ) ((hydra_lib_lists_take 3) (list )))))

(ert-deftest test-take-negtake-negative-amount ()

  (should (equal (list ) ((hydra_lib_lists_take -1) (list 1 2 3)))))

;; transpose

(ert-deftest test-transpose-negsquare-matrix ()

  (should (equal (list (list 1 4) (list 2 5) (list 3 6)) (hydra_lib_lists_transpose (list (list 1 2 3) (list 4 5 6))))))

(ert-deftest test-transpose-negempty-lists ()

  (should (equal (list ) (hydra_lib_lists_transpose (list )))))

(ert-deftest test-transpose-negsingle-row ()

  (should (equal (list (list 1) (list 2) (list 3)) (hydra_lib_lists_transpose (list (list 1 2 3))))))

(ert-deftest test-transpose-negsingle-column ()

  (should (equal (list (list 1 2 3)) (hydra_lib_lists_transpose (list (list 1) (list 2) (list 3))))))

(ert-deftest test-transpose-negragged-matrix ()

  (should (equal (list (list 1 3 4) (list 2 5) (list 6)) (hydra_lib_lists_transpose (list (list 1 2) (list 3) (list 4 5 6))))))

;; zip

(ert-deftest test-zip-negequal-length-lists ()

  (should (equal (list (list 1 "a") (list 2 "b") (list 3 "c")) ((hydra_lib_lists_zip (list 1 2 3)) (list "a" "b" "c")))))

(ert-deftest test-zip-negfirst-list-shorter ()

  (should (equal (list (list 1 "a") (list 2 "b")) ((hydra_lib_lists_zip (list 1 2)) (list "a" "b" "c")))))

(ert-deftest test-zip-negsecond-list-shorter ()

  (should (equal (list (list 1 "a") (list 2 "b")) ((hydra_lib_lists_zip (list 1 2 3)) (list "a" "b")))))

(ert-deftest test-zip-negempty-first-list ()

  (should (equal (list ) ((hydra_lib_lists_zip (list )) (list "a" "b")))))

(ert-deftest test-zip-negempty-second-list ()

  (should (equal (list ) ((hydra_lib_lists_zip (list 1 2)) (list )))))

(ert-deftest test-zip-negboth-empty-lists ()

  (should (equal (list ) ((hydra_lib_lists_zip (list )) (list )))))

;; zipWith

(ert-deftest test-zipwith-negaddition ()

  (should (equal (list 5 7 9) (((hydra_lib_lists_zip_with hydra_lib_math_add) (list 1 2 3)) (list 4 5 6)))))

(ert-deftest test-zipwith-negfirst-list-shorter ()

  (should (equal (list 5 7) (((hydra_lib_lists_zip_with hydra_lib_math_add) (list 1 2)) (list 4 5 6)))))

(ert-deftest test-zipwith-negsecond-list-shorter ()

  (should (equal (list 5 7) (((hydra_lib_lists_zip_with hydra_lib_math_add) (list 1 2 3)) (list 4 5)))))

(ert-deftest test-zipwith-negempty-first-list ()

  (should (equal (list ) (((hydra_lib_lists_zip_with hydra_lib_math_add) (list )) (list 1 2 3)))))

(ert-deftest test-zipwith-negempty-second-list ()

  (should (equal (list ) (((hydra_lib_lists_zip_with hydra_lib_math_add) (list 1 2 3)) (list )))))

(ert-deftest test-zipwith-negstring-concatenation ()

  (should (equal (list "a1" "b2") (((hydra_lib_lists_zip_with hydra_lib_strings_cat2) (list "a" "b")) (list "1" "2")))))
