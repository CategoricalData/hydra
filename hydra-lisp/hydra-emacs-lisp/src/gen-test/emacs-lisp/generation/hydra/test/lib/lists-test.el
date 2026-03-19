;;; Note: this is an automatically generated file. Do not edit. -*- lexical-binding: t; coding: utf-8 -*-
;;; hydra.lib.lists primitives

(require 'ert)

;; apply

;; string transformations

(ert-deftest test-lists-negapply-negstring-transformations-negstring-transformations ()

  (should (equal (list "ONE" "TWO" "THREE" "one" "two" "three") (funcall (funcall hydra_lib_lists_apply (list hydra_lib_strings_to_upper hydra_lib_strings_to_lower)) (list "One" "Two" "Three")))))

;; edge cases

(ert-deftest test-lists-negapply-negedge-cases-negempty-function-list ()

  (should (equal (list ) (funcall (funcall hydra_lib_lists_apply (list )) (list "a" "b")))))

(ert-deftest test-lists-negapply-negedge-cases-negempty-input-list ()

  (should (equal (list ) (funcall (funcall hydra_lib_lists_apply (list hydra_lib_strings_to_upper)) (list )))))

(ert-deftest test-lists-negapply-negedge-cases-negsingle-function ()

  (should (equal (list "HELLO") (funcall (funcall hydra_lib_lists_apply (list hydra_lib_strings_to_upper)) (list "hello")))))

(ert-deftest test-lists-negapply-negedge-cases-negsingle-input ()

  (should (equal (list "TEST" "test") (funcall (funcall hydra_lib_lists_apply (list hydra_lib_strings_to_upper hydra_lib_strings_to_lower)) (list "Test")))))

;; at

(ert-deftest test-lists-negat-negfirst-element ()

  (should (equal 1 (funcall (funcall hydra_lib_lists_at 0) (list 1 2 3)))))

(ert-deftest test-lists-negat-negmiddle-element ()

  (should (equal 2 (funcall (funcall hydra_lib_lists_at 1) (list 1 2 3)))))

(ert-deftest test-lists-negat-neglast-element ()

  (should (equal 3 (funcall (funcall hydra_lib_lists_at 2) (list 1 2 3)))))

(ert-deftest test-lists-negat-negsingle-element-list ()

  (should (equal 42 (funcall (funcall hydra_lib_lists_at 0) (list 42)))))

(ert-deftest test-lists-negat-negstring-list-access ()

  (should (equal "world" (funcall (funcall hydra_lib_lists_at 1) (list "hello" "world")))))

;; bind

(ert-deftest test-lists-negbind-negnegation-function ()

  (should (equal (list -1 -2 -3 -4) (funcall (funcall hydra_lib_lists_bind (list 1 2 3 4)) (lambda (x) (hydra_lib_lists_pure (hydra_lib_math_negate x)))))))

(ert-deftest test-lists-negbind-negempty-list ()

  (should (equal (list ) (funcall (funcall hydra_lib_lists_bind (list )) (lambda (x) (hydra_lib_lists_pure (hydra_lib_math_negate x)))))))

(ert-deftest test-lists-negbind-negsingle-element ()

  (should (equal (list -5) (funcall (funcall hydra_lib_lists_bind (list 5)) (lambda (x) (hydra_lib_lists_pure (hydra_lib_math_negate x)))))))

(ert-deftest test-lists-negbind-negduplicate-elements ()

  (should (equal (list -1 -1 -2) (funcall (funcall hydra_lib_lists_bind (list 1 1 2)) (lambda (x) (hydra_lib_lists_pure (hydra_lib_math_negate x)))))))

;; concat

(ert-deftest test-lists-negconcat-negmultiple-non-negempty-lists ()

  (should (equal (list 1 2 3 4 5 6 7 8) (funcall hydra_lib_lists_concat (list (list 1 2 3) (list 4 5) (list 6 7 8))))))

(ert-deftest test-lists-negconcat-negempty-lists-included ()

  (should (equal (list 1 2 3) (funcall hydra_lib_lists_concat (list (list ) (list 1 2) (list ) (list 3))))))

(ert-deftest test-lists-negconcat-negsingle-list ()

  (should (equal (list 1 2 3) (funcall hydra_lib_lists_concat (list (list 1 2 3))))))

(ert-deftest test-lists-negconcat-negall-empty-lists ()

  (should (equal (list ) (funcall hydra_lib_lists_concat (list (list ) (list ) (list ))))))

(ert-deftest test-lists-negconcat-negempty-list-of-lists ()

  (should (equal (list ) (funcall hydra_lib_lists_concat (list )))))

;; concat2

(ert-deftest test-lists-negconcat2-negtwo-non-negempty-lists ()

  (should (equal (list 1 2 3 4) (funcall (funcall hydra_lib_lists_concat2 (list 1 2)) (list 3 4)))))

(ert-deftest test-lists-negconcat2-negfirst-list-empty ()

  (should (equal (list 1 2) (funcall (funcall hydra_lib_lists_concat2 (list )) (list 1 2)))))

(ert-deftest test-lists-negconcat2-negsecond-list-empty ()

  (should (equal (list 1 2) (funcall (funcall hydra_lib_lists_concat2 (list 1 2)) (list )))))

(ert-deftest test-lists-negconcat2-negboth-lists-empty ()

  (should (equal (list ) (funcall (funcall hydra_lib_lists_concat2 (list )) (list )))))

(ert-deftest test-lists-negconcat2-negsingle-elements ()

  (should (equal (list 1 2) (funcall (funcall hydra_lib_lists_concat2 (list 1)) (list 2)))))

(ert-deftest test-lists-negconcat2-negstring-lists ()

  (should (equal (list "a" "b" "c" "d") (funcall (funcall hydra_lib_lists_concat2 (list "a" "b")) (list "c" "d")))))

;; cons

(ert-deftest test-lists-negcons-negcons-to-non-negempty-list ()

  (should (equal (list 1 2 3) (funcall (funcall hydra_lib_lists_cons 1) (list 2 3)))))

(ert-deftest test-lists-negcons-negcons-to-empty-list ()

  (should (equal (list 1) (funcall (funcall hydra_lib_lists_cons 1) (list )))))

(ert-deftest test-lists-negcons-negcons-negative-number ()

  (should (equal (list -1 2 3) (funcall (funcall hydra_lib_lists_cons -1) (list 2 3)))))

(ert-deftest test-lists-negcons-negcons-string ()

  (should (equal (list "hello" "world") (funcall (funcall hydra_lib_lists_cons "hello") (list "world")))))

;; drop

(ert-deftest test-lists-negdrop-negdrop-from-beginning ()

  (should (equal (list 3 4 5) (funcall (funcall hydra_lib_lists_drop 2) (list 1 2 3 4 5)))))

(ert-deftest test-lists-negdrop-negdrop-zero-elements ()

  (should (equal (list 1 2 3) (funcall (funcall hydra_lib_lists_drop 0) (list 1 2 3)))))

(ert-deftest test-lists-negdrop-negdrop-all-elements ()

  (should (equal (list ) (funcall (funcall hydra_lib_lists_drop 3) (list 1 2 3)))))

(ert-deftest test-lists-negdrop-negdrop-more-than-length ()

  (should (equal (list ) (funcall (funcall hydra_lib_lists_drop 5) (list 1 2)))))

(ert-deftest test-lists-negdrop-negdrop-from-empty-list ()

  (should (equal (list ) (funcall (funcall hydra_lib_lists_drop 3) (list )))))

(ert-deftest test-lists-negdrop-negdrop-negative-amount ()

  (should (equal (list 1 2 3) (funcall (funcall hydra_lib_lists_drop -1) (list 1 2 3)))))

;; dropWhile

(ert-deftest test-lists-negdropwhile-negdrop-while-less-than-3 ()

  (should (equal (list 3 2 1) (funcall (funcall hydra_lib_lists_drop_while (lambda (x) (funcall (hydra_lib_equality_lt x) 3))) (list 1 2 3 2 1)))))

(ert-deftest test-lists-negdropwhile-negdrop-all-elements ()

  (should (equal (list ) (funcall (funcall hydra_lib_lists_drop_while (lambda (x) (funcall (hydra_lib_equality_lt x) 10))) (list 1 2 3)))))

(ert-deftest test-lists-negdropwhile-negdrop-no-elements ()

  (should (equal (list 1 2 3) (funcall (funcall hydra_lib_lists_drop_while (lambda (x) (funcall (hydra_lib_equality_lt x) 0))) (list 1 2 3)))))

(ert-deftest test-lists-negdropwhile-negempty-list ()

  (should (equal (list ) (funcall (funcall hydra_lib_lists_drop_while (lambda (x) (funcall (hydra_lib_equality_lt x) 5))) (list )))))

;; elem

(ert-deftest test-lists-negelem-negelement-present ()

  (should (equal t (funcall (funcall hydra_lib_lists_elem 2) (list 1 2 3)))))

(ert-deftest test-lists-negelem-negelement-not-present ()

  (should (equal nil (funcall (funcall hydra_lib_lists_elem 4) (list 1 2 3)))))

(ert-deftest test-lists-negelem-negempty-list ()

  (should (equal nil (funcall (funcall hydra_lib_lists_elem 1) (list )))))

(ert-deftest test-lists-negelem-negsingle-element-present ()

  (should (equal t (funcall (funcall hydra_lib_lists_elem 1) (list 1)))))

(ert-deftest test-lists-negelem-negsingle-element-not-present ()

  (should (equal nil (funcall (funcall hydra_lib_lists_elem 2) (list 1)))))

(ert-deftest test-lists-negelem-negduplicate-elements ()

  (should (equal t (funcall (funcall hydra_lib_lists_elem 2) (list 1 2 2 3)))))

(ert-deftest test-lists-negelem-negstring-element-present ()

  (should (equal t (funcall (funcall hydra_lib_lists_elem "hello") (list "world" "hello" "test")))))

(ert-deftest test-lists-negelem-negstring-element-not-present ()

  (should (equal nil (funcall (funcall hydra_lib_lists_elem "missing") (list "world" "hello")))))

;; filter

(ert-deftest test-lists-negfilter-negfilter-positive-numbers ()

  (should (equal (list 2 4 5) (funcall (funcall hydra_lib_lists_filter (lambda (x) (funcall (hydra_lib_equality_gt x) 0))) (list -1 2 -3 4 5)))))

(ert-deftest test-lists-negfilter-negfilter-all-elements ()

  (should (equal (list 1 2 3) (funcall (funcall hydra_lib_lists_filter (lambda (x) (funcall (hydra_lib_equality_lt x) 10))) (list 1 2 3)))))

(ert-deftest test-lists-negfilter-negfilter-no-elements ()

  (should (equal (list ) (funcall (funcall hydra_lib_lists_filter (lambda (x) (funcall (hydra_lib_equality_gt x) 10))) (list 1 2 3)))))

(ert-deftest test-lists-negfilter-negempty-list ()

  (should (equal (list ) (funcall (funcall hydra_lib_lists_filter (lambda (x) (funcall (hydra_lib_equality_gt x) 0))) (list )))))

;; find

(ert-deftest test-lists-negfind-negfind-existing-element ()

  (should (equal (list :just 4) (funcall (funcall hydra_lib_lists_find (lambda (x) (funcall (hydra_lib_equality_gt x) 3))) (list 1 2 4 5)))))

(ert-deftest test-lists-negfind-negfind-first-matching ()

  (should (equal (list :just 1) (funcall (funcall hydra_lib_lists_find (lambda (x) (funcall (hydra_lib_equality_gt x) 0))) (list 1 2 3)))))

(ert-deftest test-lists-negfind-negfind-no-match ()

  (should (equal (list :nothing) (funcall (funcall hydra_lib_lists_find (lambda (x) (funcall (hydra_lib_equality_gt x) 10))) (list 1 2 3)))))

(ert-deftest test-lists-negfind-negfind-in-empty-list ()

  (should (equal (list :nothing) (funcall (funcall hydra_lib_lists_find (lambda (x) (funcall (hydra_lib_equality_gt x) 0))) (list )))))

(ert-deftest test-lists-negfind-negfind-single-element ()

  (should (equal (list :just 42) (funcall (funcall hydra_lib_lists_find (lambda (x) (funcall (hydra_lib_equality_equal x) 42))) (list 42)))))

;; foldl

(ert-deftest test-lists-negfoldl-negsum-with-addition ()

  (should (equal 10 (funcall (funcall (funcall hydra_lib_lists_foldl hydra_lib_math_add) 0) (list 1 2 3 4)))))

(ert-deftest test-lists-negfoldl-negproduct-with-multiplication ()

  (should (equal 24 (funcall (funcall (funcall hydra_lib_lists_foldl hydra_lib_math_mul) 1) (list 2 3 4)))))

(ert-deftest test-lists-negfoldl-negempty-list ()

  (should (equal 5 (funcall (funcall (funcall hydra_lib_lists_foldl hydra_lib_math_add) 5) (list )))))

(ert-deftest test-lists-negfoldl-negsingle-element ()

  (should (equal 15 (funcall (funcall (funcall hydra_lib_lists_foldl hydra_lib_math_add) 10) (list 5)))))

(ert-deftest test-lists-negfoldl-negsubtraction-fold ()

  (should (equal 4 (funcall (funcall (funcall hydra_lib_lists_foldl hydra_lib_math_sub) 10) (list 1 2 3)))))

;; foldr

(ert-deftest test-lists-negfoldr-negsubtraction-fold-right ()

  (should (equal 2 (funcall (funcall (funcall hydra_lib_lists_foldr hydra_lib_math_sub) 0) (list 1 2 3)))))

(ert-deftest test-lists-negfoldr-negempty-list ()

  (should (equal 5 (funcall (funcall (funcall hydra_lib_lists_foldr hydra_lib_math_add) 5) (list )))))

(ert-deftest test-lists-negfoldr-negsingle-element ()

  (should (equal 15 (funcall (funcall (funcall hydra_lib_lists_foldr hydra_lib_math_add) 10) (list 5)))))

(ert-deftest test-lists-negfoldr-negsum-with-addition ()

  (should (equal 10 (funcall (funcall (funcall hydra_lib_lists_foldr hydra_lib_math_add) 0) (list 1 2 3 4)))))

(ert-deftest test-lists-negfoldr-negsubtraction-vs-foldl ()

  (should (equal -8 (funcall (funcall (funcall hydra_lib_lists_foldr hydra_lib_math_sub) 10) (list 1 2 3)))))

;; group

(ert-deftest test-lists-neggroup-negconsecutive-duplicates ()

  (should (equal (list (list 1 1) (list 2 2 2) (list 3) (list 1)) (funcall hydra_lib_lists_group (list 1 1 2 2 2 3 1)))))

(ert-deftest test-lists-neggroup-negno-duplicates ()

  (should (equal (list (list 1) (list 2) (list 3)) (funcall hydra_lib_lists_group (list 1 2 3)))))

(ert-deftest test-lists-neggroup-negall-same ()

  (should (equal (list (list 1 1 1)) (funcall hydra_lib_lists_group (list 1 1 1)))))

(ert-deftest test-lists-neggroup-negempty-list ()

  (should (equal (list ) (funcall hydra_lib_lists_group (list )))))

(ert-deftest test-lists-neggroup-negsingle-element ()

  (should (equal (list (list 1)) (funcall hydra_lib_lists_group (list 1)))))

;; head

(ert-deftest test-lists-neghead-negthree-element-list ()

  (should (equal 1 (funcall hydra_lib_lists_head (list 1 2 3)))))

(ert-deftest test-lists-neghead-negsingle-element-list ()

  (should (equal 42 (funcall hydra_lib_lists_head (list 42)))))

(ert-deftest test-lists-neghead-negnegative-numbers ()

  (should (equal -1 (funcall hydra_lib_lists_head (list -1 -2 -3)))))

(ert-deftest test-lists-neghead-negstring-list ()

  (should (equal "hello" (funcall hydra_lib_lists_head (list "hello" "world")))))

;; init

(ert-deftest test-lists-neginit-negmultiple-elements ()

  (should (equal (list 1 2 3) (funcall hydra_lib_lists_init (list 1 2 3 4)))))

(ert-deftest test-lists-neginit-negtwo-elements ()

  (should (equal (list 1) (funcall hydra_lib_lists_init (list 1 2)))))

(ert-deftest test-lists-neginit-negsingle-element ()

  (should (equal (list ) (funcall hydra_lib_lists_init (list 1)))))

(ert-deftest test-lists-neginit-negstring-list ()

  (should (equal (list "a" "b") (funcall hydra_lib_lists_init (list "a" "b" "c")))))

;; intercalate

(ert-deftest test-lists-negintercalate-negdouble-zero-separator ()

  (should (equal (list 1 2 3 0 0 4 5 0 0 6 7 8) (funcall (funcall hydra_lib_lists_intercalate (list 0 0)) (list (list 1 2 3) (list 4 5) (list 6 7 8))))))

(ert-deftest test-lists-negintercalate-negempty-separator ()

  (should (equal (list 1 2 3 4) (funcall (funcall hydra_lib_lists_intercalate (list )) (list (list 1 2) (list 3 4))))))

(ert-deftest test-lists-negintercalate-negsingle-element-separator ()

  (should (equal (list 1 99 2 99 3) (funcall (funcall hydra_lib_lists_intercalate (list 99)) (list (list 1) (list 2) (list 3))))))

(ert-deftest test-lists-negintercalate-negempty-list-of-lists ()

  (should (equal (list ) (funcall (funcall hydra_lib_lists_intercalate (list 0)) (list )))))

(ert-deftest test-lists-negintercalate-negsingle-list ()

  (should (equal (list 1 2 3) (funcall (funcall hydra_lib_lists_intercalate (list 0)) (list (list 1 2 3))))))

(ert-deftest test-lists-negintercalate-neglists-with-empty-lists ()

  (should (equal (list 0 1 0) (funcall (funcall hydra_lib_lists_intercalate (list 0)) (list (list ) (list 1) (list ))))))

;; intersperse

(ert-deftest test-lists-negintersperse-negstring-interspersion ()

  (should (equal (list "one" "and" "two" "and" "three") (funcall (funcall hydra_lib_lists_intersperse "and") (list "one" "two" "three")))))

(ert-deftest test-lists-negintersperse-negsingle-element ()

  (should (equal (list "only") (funcall (funcall hydra_lib_lists_intersperse "x") (list "only")))))

(ert-deftest test-lists-negintersperse-negempty-list ()

  (should (equal (list ) (funcall (funcall hydra_lib_lists_intersperse "x") (list )))))

(ert-deftest test-lists-negintersperse-negtwo-elements ()

  (should (equal (list "a" "+" "b") (funcall (funcall hydra_lib_lists_intersperse "+") (list "a" "b")))))

(ert-deftest test-lists-negintersperse-negnumber-interspersion ()

  (should (equal (list 1 0 2 0 3) (funcall (funcall hydra_lib_lists_intersperse 0) (list 1 2 3)))))

;; last

(ert-deftest test-lists-neglast-negthree-element-list ()

  (should (equal 3 (funcall hydra_lib_lists_last (list 1 2 3)))))

(ert-deftest test-lists-neglast-negsingle-element-list ()

  (should (equal 42 (funcall hydra_lib_lists_last (list 42)))))

(ert-deftest test-lists-neglast-negnegative-numbers ()

  (should (equal -3 (funcall hydra_lib_lists_last (list -1 -2 -3)))))

(ert-deftest test-lists-neglast-negstring-list ()

  (should (equal "world" (funcall hydra_lib_lists_last (list "hello" "world")))))

;; length

(ert-deftest test-lists-neglength-negthree-elements ()

  (should (equal 3 (funcall hydra_lib_lists_length (list 1 2 3)))))

(ert-deftest test-lists-neglength-negempty-list ()

  (should (equal 0 (funcall hydra_lib_lists_length (list )))))

(ert-deftest test-lists-neglength-negsingle-element ()

  (should (equal 1 (funcall hydra_lib_lists_length (list 42)))))

(ert-deftest test-lists-neglength-negmany-elements ()

  (should (equal 10 (funcall hydra_lib_lists_length (list 1 2 3 4 5 6 7 8 9 10)))))

(ert-deftest test-lists-neglength-negstring-list ()

  (should (equal 3 (funcall hydra_lib_lists_length (list "a" "b" "c")))))

;; map

(ert-deftest test-lists-negmap-negstring-to-uppercase ()

  (should (equal (list "ONE" "TWO") (funcall (funcall hydra_lib_lists_map hydra_lib_strings_to_upper) (list "one" "two")))))

(ert-deftest test-lists-negmap-negempty-list ()

  (should (equal (list ) (funcall (funcall hydra_lib_lists_map hydra_lib_strings_to_upper) (list )))))

(ert-deftest test-lists-negmap-negsingle-element ()

  (should (equal (list "HELLO") (funcall (funcall hydra_lib_lists_map hydra_lib_strings_to_upper) (list "hello")))))

(ert-deftest test-lists-negmap-negnumber-negation ()

  (should (equal (list -1 -2 -3) (funcall (funcall hydra_lib_lists_map hydra_lib_math_negate) (list 1 2 3)))))

(ert-deftest test-lists-negmap-negidentity-function ()

  (should (equal (list 1 2 3) (funcall (funcall hydra_lib_lists_map hydra_lib_equality_identity) (list 1 2 3)))))

;; nub

(ert-deftest test-lists-negnub-negremove-duplicates ()

  (should (equal (list 1 2 3 4) (funcall hydra_lib_lists_nub (list 1 2 1 3 2 4)))))

(ert-deftest test-lists-negnub-negno-duplicates ()

  (should (equal (list 1 2 3) (funcall hydra_lib_lists_nub (list 1 2 3)))))

(ert-deftest test-lists-negnub-negall-duplicates ()

  (should (equal (list 1) (funcall hydra_lib_lists_nub (list 1 1 1)))))

(ert-deftest test-lists-negnub-negempty-list ()

  (should (equal (list ) (funcall hydra_lib_lists_nub (list )))))

(ert-deftest test-lists-negnub-negsingle-element ()

  (should (equal (list 1) (funcall hydra_lib_lists_nub (list 1)))))

(ert-deftest test-lists-negnub-negstring-duplicates ()

  (should (equal (list "a" "b" "c") (funcall hydra_lib_lists_nub (list "a" "b" "a" "c")))))

;; null

(ert-deftest test-lists-negnull-negempty-int-list ()

  (should (equal t (funcall hydra_lib_lists_null (list )))))

(ert-deftest test-lists-negnull-negsingle-element ()

  (should (equal nil (funcall hydra_lib_lists_null (list 1)))))

(ert-deftest test-lists-negnull-negmultiple-elements ()

  (should (equal nil (funcall hydra_lib_lists_null (list 1 2 3)))))

(ert-deftest test-lists-negnull-negempty-string-list ()

  (should (equal t (funcall hydra_lib_lists_null (list )))))

(ert-deftest test-lists-negnull-negnon-negempty-string-list ()

  (should (equal nil (funcall hydra_lib_lists_null (list "a")))))

;; partition

(ert-deftest test-lists-negpartition-negpartition-greater-than-3 ()

  (should (equal (list (list 4 5 6) (list 1 2 3)) (funcall (funcall hydra_lib_lists_partition (lambda (x) (funcall (hydra_lib_equality_gt x) 3))) (list 1 2 3 4 5 6)))))

(ert-deftest test-lists-negpartition-negpartition-all-elements ()

  (should (equal (list (list 1 2 3) (list )) (funcall (funcall hydra_lib_lists_partition (lambda (x) (funcall (hydra_lib_equality_lt x) 10))) (list 1 2 3)))))

(ert-deftest test-lists-negpartition-negpartition-no-elements ()

  (should (equal (list (list ) (list 1 2 3)) (funcall (funcall hydra_lib_lists_partition (lambda (x) (funcall (hydra_lib_equality_gt x) 10))) (list 1 2 3)))))

(ert-deftest test-lists-negpartition-negpartition-even-numbers ()

  (should (equal (list (list 2 4 6) (list 1 3 5)) (funcall (funcall hydra_lib_lists_partition (lambda (x) (hydra_lib_math_even x))) (list 1 2 3 4 5 6)))))

(ert-deftest test-lists-negpartition-negempty-list ()

  (should (equal (list (list ) (list )) (funcall (funcall hydra_lib_lists_partition (lambda (x) (funcall (hydra_lib_equality_lt x) 5))) (list )))))

;; pure

(ert-deftest test-lists-negpure-negstring-element ()

  (should (equal (list "one") (funcall hydra_lib_lists_pure "one"))))

(ert-deftest test-lists-negpure-negempty-string ()

  (should (equal (list "") (funcall hydra_lib_lists_pure ""))))

(ert-deftest test-lists-negpure-negnumber-element ()

  (should (equal (list 42) (funcall hydra_lib_lists_pure 42))))

(ert-deftest test-lists-negpure-negnegative-number ()

  (should (equal (list -5) (funcall hydra_lib_lists_pure -5))))

;; replicate

(ert-deftest test-lists-negreplicate-negreplicate-three-times ()

  (should (equal (list 42 42 42) (funcall (funcall hydra_lib_lists_replicate 3) 42))))

(ert-deftest test-lists-negreplicate-negreplicate-zero-times ()

  (should (equal (list ) (funcall (funcall hydra_lib_lists_replicate 0) 1))))

(ert-deftest test-lists-negreplicate-negreplicate-once ()

  (should (equal (list 99) (funcall (funcall hydra_lib_lists_replicate 1) 99))))

(ert-deftest test-lists-negreplicate-negreplicate-string ()

  (should (equal (list "hello" "hello") (funcall (funcall hydra_lib_lists_replicate 2) "hello"))))

;; reverse

(ert-deftest test-lists-negreverse-negmultiple-elements ()

  (should (equal (list 4 3 2 1) (funcall hydra_lib_lists_reverse (list 1 2 3 4)))))

(ert-deftest test-lists-negreverse-negsingle-element ()

  (should (equal (list 1) (funcall hydra_lib_lists_reverse (list 1)))))

(ert-deftest test-lists-negreverse-negempty-list ()

  (should (equal (list ) (funcall hydra_lib_lists_reverse (list )))))

(ert-deftest test-lists-negreverse-negtwo-elements ()

  (should (equal (list 2 1) (funcall hydra_lib_lists_reverse (list 1 2)))))

(ert-deftest test-lists-negreverse-negstring-list ()

  (should (equal (list "c" "b" "a") (funcall hydra_lib_lists_reverse (list "a" "b" "c")))))

;; safeHead

(ert-deftest test-lists-negsafehead-negnon-negempty-int-list ()

  (should (equal (list :just 1) (funcall hydra_lib_lists_safe_head (list 1 2 3)))))

(ert-deftest test-lists-negsafehead-negempty-int-list ()

  (should (equal (list :nothing) (funcall hydra_lib_lists_safe_head (list )))))

(ert-deftest test-lists-negsafehead-negsingle-element ()

  (should (equal (list :just 42) (funcall hydra_lib_lists_safe_head (list 42)))))

(ert-deftest test-lists-negsafehead-negnon-negempty-string-list ()

  (should (equal (list :just "hello") (funcall hydra_lib_lists_safe_head (list "hello" "world")))))

(ert-deftest test-lists-negsafehead-negempty-string-list ()

  (should (equal (list :nothing) (funcall hydra_lib_lists_safe_head (list )))))

;; singleton

(ert-deftest test-lists-negsingleton-negnumber-element ()

  (should (equal (list 42) (funcall hydra_lib_lists_singleton 42))))

(ert-deftest test-lists-negsingleton-negnegative-number ()

  (should (equal (list -1) (funcall hydra_lib_lists_singleton -1))))

(ert-deftest test-lists-negsingleton-negzero ()

  (should (equal (list 0) (funcall hydra_lib_lists_singleton 0))))

(ert-deftest test-lists-negsingleton-negstring-element ()

  (should (equal (list "hello") (funcall hydra_lib_lists_singleton "hello"))))

;; sort

(ert-deftest test-lists-negsort-negunsorted-numbers ()

  (should (equal (list 1 1 3 4 5) (funcall hydra_lib_lists_sort (list 3 1 4 1 5)))))

(ert-deftest test-lists-negsort-negalready-sorted ()

  (should (equal (list 1 2 3) (funcall hydra_lib_lists_sort (list 1 2 3)))))

(ert-deftest test-lists-negsort-negreverse-sorted ()

  (should (equal (list 1 2 3) (funcall hydra_lib_lists_sort (list 3 2 1)))))

(ert-deftest test-lists-negsort-negsingle-element ()

  (should (equal (list 1) (funcall hydra_lib_lists_sort (list 1)))))

(ert-deftest test-lists-negsort-negempty-list ()

  (should (equal (list ) (funcall hydra_lib_lists_sort (list )))))

(ert-deftest test-lists-negsort-negduplicates ()

  (should (equal (list 1 1 2 2 3) (funcall hydra_lib_lists_sort (list 2 1 2 3 1)))))

(ert-deftest test-lists-negsort-negstring-sort ()

  (should (equal (list "apple" "banana" "zebra") (funcall hydra_lib_lists_sort (list "zebra" "apple" "banana")))))

;; sortOn

(ert-deftest test-lists-negsorton-negsort-by-string-length ()

  (should (equal (list "hi" "hello" "world") (funcall (funcall hydra_lib_lists_sort_on hydra_lib_strings_length) (list "hello" "hi" "world")))))

(ert-deftest test-lists-negsorton-negempty-string-list ()

  (should (equal (list ) (funcall (funcall hydra_lib_lists_sort_on hydra_lib_strings_length) (list )))))

(ert-deftest test-lists-negsorton-negsingle-string-element ()

  (should (equal (list "test") (funcall (funcall hydra_lib_lists_sort_on hydra_lib_strings_length) (list "test")))))

(ert-deftest test-lists-negsorton-negsort-by-negation ()

  (should (equal (list 3 2 1) (funcall (funcall hydra_lib_lists_sort_on hydra_lib_math_negate) (list 1 3 2)))))

(ert-deftest test-lists-negsorton-negsort-by-absolute-value ()

  (should (equal (list -1 2 -3) (funcall (funcall hydra_lib_lists_sort_on hydra_lib_math_abs) (list -1 -3 2)))))

;; span

(ert-deftest test-lists-negspan-negspan-less-than-3 ()

  (should (equal (list (list 1 2) (list 3 1 2)) (funcall (funcall hydra_lib_lists_span (lambda (x) (funcall (hydra_lib_equality_lt x) 3))) (list 1 2 3 1 2)))))

(ert-deftest test-lists-negspan-negspan-all-elements ()

  (should (equal (list (list 1 2 3) (list )) (funcall (funcall hydra_lib_lists_span (lambda (x) (funcall (hydra_lib_equality_lt x) 10))) (list 1 2 3)))))

(ert-deftest test-lists-negspan-negspan-no-elements ()

  (should (equal (list (list ) (list 1 2 3)) (funcall (funcall hydra_lib_lists_span (lambda (x) (funcall (hydra_lib_equality_gt x) 10))) (list 1 2 3)))))

(ert-deftest test-lists-negspan-negempty-list ()

  (should (equal (list (list ) (list )) (funcall (funcall hydra_lib_lists_span (lambda (x) (funcall (hydra_lib_equality_lt x) 5))) (list )))))

;; tail

(ert-deftest test-lists-negtail-negmultiple-elements ()

  (should (equal (list 2 3 4) (funcall hydra_lib_lists_tail (list 1 2 3 4)))))

(ert-deftest test-lists-negtail-negtwo-elements ()

  (should (equal (list 2) (funcall hydra_lib_lists_tail (list 1 2)))))

(ert-deftest test-lists-negtail-negsingle-element ()

  (should (equal (list ) (funcall hydra_lib_lists_tail (list 1)))))

(ert-deftest test-lists-negtail-negstring-list ()

  (should (equal (list "b" "c") (funcall hydra_lib_lists_tail (list "a" "b" "c")))))

;; take

(ert-deftest test-lists-negtake-negtake-from-beginning ()

  (should (equal (list 1 2) (funcall (funcall hydra_lib_lists_take 2) (list 1 2 3 4 5)))))

(ert-deftest test-lists-negtake-negtake-zero-elements ()

  (should (equal (list ) (funcall (funcall hydra_lib_lists_take 0) (list 1 2 3)))))

(ert-deftest test-lists-negtake-negtake-all-elements ()

  (should (equal (list 1 2 3) (funcall (funcall hydra_lib_lists_take 3) (list 1 2 3)))))

(ert-deftest test-lists-negtake-negtake-more-than-length ()

  (should (equal (list 1 2) (funcall (funcall hydra_lib_lists_take 5) (list 1 2)))))

(ert-deftest test-lists-negtake-negtake-from-empty-list ()

  (should (equal (list ) (funcall (funcall hydra_lib_lists_take 3) (list )))))

(ert-deftest test-lists-negtake-negtake-negative-amount ()

  (should (equal (list ) (funcall (funcall hydra_lib_lists_take -1) (list 1 2 3)))))

;; transpose

(ert-deftest test-lists-negtranspose-negsquare-matrix ()

  (should (equal (list (list 1 4) (list 2 5) (list 3 6)) (funcall hydra_lib_lists_transpose (list (list 1 2 3) (list 4 5 6))))))

(ert-deftest test-lists-negtranspose-negempty-lists ()

  (should (equal (list ) (funcall hydra_lib_lists_transpose (list )))))

(ert-deftest test-lists-negtranspose-negsingle-row ()

  (should (equal (list (list 1) (list 2) (list 3)) (funcall hydra_lib_lists_transpose (list (list 1 2 3))))))

(ert-deftest test-lists-negtranspose-negsingle-column ()

  (should (equal (list (list 1 2 3)) (funcall hydra_lib_lists_transpose (list (list 1) (list 2) (list 3))))))

(ert-deftest test-lists-negtranspose-negragged-matrix ()

  (should (equal (list (list 1 3 4) (list 2 5) (list 6)) (funcall hydra_lib_lists_transpose (list (list 1 2) (list 3) (list 4 5 6))))))

;; zip

(ert-deftest test-lists-negzip-negequal-length-lists ()

  (should (equal (list (list 1 "a") (list 2 "b") (list 3 "c")) (funcall (funcall hydra_lib_lists_zip (list 1 2 3)) (list "a" "b" "c")))))

(ert-deftest test-lists-negzip-negfirst-list-shorter ()

  (should (equal (list (list 1 "a") (list 2 "b")) (funcall (funcall hydra_lib_lists_zip (list 1 2)) (list "a" "b" "c")))))

(ert-deftest test-lists-negzip-negsecond-list-shorter ()

  (should (equal (list (list 1 "a") (list 2 "b")) (funcall (funcall hydra_lib_lists_zip (list 1 2 3)) (list "a" "b")))))

(ert-deftest test-lists-negzip-negempty-first-list ()

  (should (equal (list ) (funcall (funcall hydra_lib_lists_zip (list )) (list "a" "b")))))

(ert-deftest test-lists-negzip-negempty-second-list ()

  (should (equal (list ) (funcall (funcall hydra_lib_lists_zip (list 1 2)) (list )))))

(ert-deftest test-lists-negzip-negboth-empty-lists ()

  (should (equal (list ) (funcall (funcall hydra_lib_lists_zip (list )) (list )))))

;; zipWith

(ert-deftest test-lists-negzipwith-negaddition ()

  (should (equal (list 5 7 9) (funcall (funcall (funcall hydra_lib_lists_zip_with hydra_lib_math_add) (list 1 2 3)) (list 4 5 6)))))

(ert-deftest test-lists-negzipwith-negfirst-list-shorter ()

  (should (equal (list 5 7) (funcall (funcall (funcall hydra_lib_lists_zip_with hydra_lib_math_add) (list 1 2)) (list 4 5 6)))))

(ert-deftest test-lists-negzipwith-negsecond-list-shorter ()

  (should (equal (list 5 7) (funcall (funcall (funcall hydra_lib_lists_zip_with hydra_lib_math_add) (list 1 2 3)) (list 4 5)))))

(ert-deftest test-lists-negzipwith-negempty-first-list ()

  (should (equal (list ) (funcall (funcall (funcall hydra_lib_lists_zip_with hydra_lib_math_add) (list )) (list 1 2 3)))))

(ert-deftest test-lists-negzipwith-negempty-second-list ()

  (should (equal (list ) (funcall (funcall (funcall hydra_lib_lists_zip_with hydra_lib_math_add) (list 1 2 3)) (list )))))

(ert-deftest test-lists-negzipwith-negstring-concatenation ()

  (should (equal (list "a1" "b2") (funcall (funcall (funcall hydra_lib_lists_zip_with hydra_lib_strings_cat2) (list "a" "b")) (list "1" "2")))))
