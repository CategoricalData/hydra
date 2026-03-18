;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.lists primitives

;; apply

;; string transformations

(defun test-apply-negstring-transformations-negstring-transformations ()

  (assert (equal (list "ONE" "TWO" "THREE" "one" "two" "three") ((hydra_lib_lists_apply (list hydra_lib_strings_to_upper hydra_lib_strings_to_lower)) (list "One" "Two" "Three")))))

;; edge cases

(defun test-apply-negedge-cases-negempty-function-list ()

  (assert (equal (list ) ((hydra_lib_lists_apply (list )) (list "a" "b")))))

(defun test-apply-negedge-cases-negempty-input-list ()

  (assert (equal (list ) ((hydra_lib_lists_apply (list hydra_lib_strings_to_upper)) (list )))))

(defun test-apply-negedge-cases-negsingle-function ()

  (assert (equal (list "HELLO") ((hydra_lib_lists_apply (list hydra_lib_strings_to_upper)) (list "hello")))))

(defun test-apply-negedge-cases-negsingle-input ()

  (assert (equal (list "TEST" "test") ((hydra_lib_lists_apply (list hydra_lib_strings_to_upper hydra_lib_strings_to_lower)) (list "Test")))))

;; at

(defun test-at-negfirst-element ()

  (assert (equal 1 ((hydra_lib_lists_at 0) (list 1 2 3)))))

(defun test-at-negmiddle-element ()

  (assert (equal 2 ((hydra_lib_lists_at 1) (list 1 2 3)))))

(defun test-at-neglast-element ()

  (assert (equal 3 ((hydra_lib_lists_at 2) (list 1 2 3)))))

(defun test-at-negsingle-element-list ()

  (assert (equal 42 ((hydra_lib_lists_at 0) (list 42)))))

(defun test-at-negstring-list-access ()

  (assert (equal "world" ((hydra_lib_lists_at 1) (list "hello" "world")))))

;; bind

(defun test-bind-negnegation-function ()

  (assert (equal (list -1 -2 -3 -4) ((hydra_lib_lists_bind (list 1 2 3 4)) (cl:lambda (x) (hydra_lib_lists_pure (hydra_lib_math_negate x)))))))

(defun test-bind-negempty-list ()

  (assert (equal (list ) ((hydra_lib_lists_bind (list )) (cl:lambda (x) (hydra_lib_lists_pure (hydra_lib_math_negate x)))))))

(defun test-bind-negsingle-element ()

  (assert (equal (list -5) ((hydra_lib_lists_bind (list 5)) (cl:lambda (x) (hydra_lib_lists_pure (hydra_lib_math_negate x)))))))

(defun test-bind-negduplicate-elements ()

  (assert (equal (list -1 -1 -2) ((hydra_lib_lists_bind (list 1 1 2)) (cl:lambda (x) (hydra_lib_lists_pure (hydra_lib_math_negate x)))))))

;; concat

(defun test-concat-negmultiple-non-negempty-lists ()

  (assert (equal (list 1 2 3 4 5 6 7 8) (hydra_lib_lists_concat (list (list 1 2 3) (list 4 5) (list 6 7 8))))))

(defun test-concat-negempty-lists-included ()

  (assert (equal (list 1 2 3) (hydra_lib_lists_concat (list (list ) (list 1 2) (list ) (list 3))))))

(defun test-concat-negsingle-list ()

  (assert (equal (list 1 2 3) (hydra_lib_lists_concat (list (list 1 2 3))))))

(defun test-concat-negall-empty-lists ()

  (assert (equal (list ) (hydra_lib_lists_concat (list (list ) (list ) (list ))))))

(defun test-concat-negempty-list-of-lists ()

  (assert (equal (list ) (hydra_lib_lists_concat (list )))))

;; concat2

(defun test-concat2-negtwo-non-negempty-lists ()

  (assert (equal (list 1 2 3 4) ((hydra_lib_lists_concat2 (list 1 2)) (list 3 4)))))

(defun test-concat2-negfirst-list-empty ()

  (assert (equal (list 1 2) ((hydra_lib_lists_concat2 (list )) (list 1 2)))))

(defun test-concat2-negsecond-list-empty ()

  (assert (equal (list 1 2) ((hydra_lib_lists_concat2 (list 1 2)) (list )))))

(defun test-concat2-negboth-lists-empty ()

  (assert (equal (list ) ((hydra_lib_lists_concat2 (list )) (list )))))

(defun test-concat2-negsingle-elements ()

  (assert (equal (list 1 2) ((hydra_lib_lists_concat2 (list 1)) (list 2)))))

(defun test-concat2-negstring-lists ()

  (assert (equal (list "a" "b" "c" "d") ((hydra_lib_lists_concat2 (list "a" "b")) (list "c" "d")))))

;; cons

(defun test-cons-negcons-to-non-negempty-list ()

  (assert (equal (list 1 2 3) ((hydra_lib_lists_cons 1) (list 2 3)))))

(defun test-cons-negcons-to-empty-list ()

  (assert (equal (list 1) ((hydra_lib_lists_cons 1) (list )))))

(defun test-cons-negcons-negative-number ()

  (assert (equal (list -1 2 3) ((hydra_lib_lists_cons -1) (list 2 3)))))

(defun test-cons-negcons-string ()

  (assert (equal (list "hello" "world") ((hydra_lib_lists_cons "hello") (list "world")))))

;; drop

(defun test-drop-negdrop-from-beginning ()

  (assert (equal (list 3 4 5) ((hydra_lib_lists_drop 2) (list 1 2 3 4 5)))))

(defun test-drop-negdrop-zero-elements ()

  (assert (equal (list 1 2 3) ((hydra_lib_lists_drop 0) (list 1 2 3)))))

(defun test-drop-negdrop-all-elements ()

  (assert (equal (list ) ((hydra_lib_lists_drop 3) (list 1 2 3)))))

(defun test-drop-negdrop-more-than-length ()

  (assert (equal (list ) ((hydra_lib_lists_drop 5) (list 1 2)))))

(defun test-drop-negdrop-from-empty-list ()

  (assert (equal (list ) ((hydra_lib_lists_drop 3) (list )))))

(defun test-drop-negdrop-negative-amount ()

  (assert (equal (list 1 2 3) ((hydra_lib_lists_drop -1) (list 1 2 3)))))

;; dropWhile

(defun test-dropwhile-negdrop-while-less-than-3 ()

  (assert (equal (list 3 2 1) ((hydra_lib_lists_drop_while (cl:lambda (x) ((hydra_lib_equality_lt x) 3))) (list 1 2 3 2 1)))))

(defun test-dropwhile-negdrop-all-elements ()

  (assert (equal (list ) ((hydra_lib_lists_drop_while (cl:lambda (x) ((hydra_lib_equality_lt x) 10))) (list 1 2 3)))))

(defun test-dropwhile-negdrop-no-elements ()

  (assert (equal (list 1 2 3) ((hydra_lib_lists_drop_while (cl:lambda (x) ((hydra_lib_equality_lt x) 0))) (list 1 2 3)))))

(defun test-dropwhile-negempty-list ()

  (assert (equal (list ) ((hydra_lib_lists_drop_while (cl:lambda (x) ((hydra_lib_equality_lt x) 5))) (list )))))

;; elem

(defun test-elem-negelement-present ()

  (assert (equal cl:t ((hydra_lib_lists_elem 2) (list 1 2 3)))))

(defun test-elem-negelement-not-present ()

  (assert (equal cl:nil ((hydra_lib_lists_elem 4) (list 1 2 3)))))

(defun test-elem-negempty-list ()

  (assert (equal cl:nil ((hydra_lib_lists_elem 1) (list )))))

(defun test-elem-negsingle-element-present ()

  (assert (equal cl:t ((hydra_lib_lists_elem 1) (list 1)))))

(defun test-elem-negsingle-element-not-present ()

  (assert (equal cl:nil ((hydra_lib_lists_elem 2) (list 1)))))

(defun test-elem-negduplicate-elements ()

  (assert (equal cl:t ((hydra_lib_lists_elem 2) (list 1 2 2 3)))))

(defun test-elem-negstring-element-present ()

  (assert (equal cl:t ((hydra_lib_lists_elem "hello") (list "world" "hello" "test")))))

(defun test-elem-negstring-element-not-present ()

  (assert (equal cl:nil ((hydra_lib_lists_elem "missing") (list "world" "hello")))))

;; filter

(defun test-filter-negfilter-positive-numbers ()

  (assert (equal (list 2 4 5) ((hydra_lib_lists_filter (cl:lambda (x) ((hydra_lib_equality_gt x) 0))) (list -1 2 -3 4 5)))))

(defun test-filter-negfilter-all-elements ()

  (assert (equal (list 1 2 3) ((hydra_lib_lists_filter (cl:lambda (x) ((hydra_lib_equality_lt x) 10))) (list 1 2 3)))))

(defun test-filter-negfilter-no-elements ()

  (assert (equal (list ) ((hydra_lib_lists_filter (cl:lambda (x) ((hydra_lib_equality_gt x) 10))) (list 1 2 3)))))

(defun test-filter-negempty-list ()

  (assert (equal (list ) ((hydra_lib_lists_filter (cl:lambda (x) ((hydra_lib_equality_gt x) 0))) (list )))))

;; find

(defun test-find-negfind-existing-element ()

  (assert (equal 4 ((hydra_lib_lists_find (cl:lambda (x) ((hydra_lib_equality_gt x) 3))) (list 1 2 4 5)))))

(defun test-find-negfind-first-matching ()

  (assert (equal 1 ((hydra_lib_lists_find (cl:lambda (x) ((hydra_lib_equality_gt x) 0))) (list 1 2 3)))))

(defun test-find-negfind-no-match ()

  (assert (equal nil ((hydra_lib_lists_find (cl:lambda (x) ((hydra_lib_equality_gt x) 10))) (list 1 2 3)))))

(defun test-find-negfind-in-empty-list ()

  (assert (equal nil ((hydra_lib_lists_find (cl:lambda (x) ((hydra_lib_equality_gt x) 0))) (list )))))

(defun test-find-negfind-single-element ()

  (assert (equal 42 ((hydra_lib_lists_find (cl:lambda (x) ((hydra_lib_equality_equal x) 42))) (list 42)))))

;; foldl

(defun test-foldl-negsum-with-addition ()

  (assert (equal 10 (((hydra_lib_lists_foldl hydra_lib_math_add) 0) (list 1 2 3 4)))))

(defun test-foldl-negproduct-with-multiplication ()

  (assert (equal 24 (((hydra_lib_lists_foldl hydra_lib_math_mul) 1) (list 2 3 4)))))

(defun test-foldl-negempty-list ()

  (assert (equal 5 (((hydra_lib_lists_foldl hydra_lib_math_add) 5) (list )))))

(defun test-foldl-negsingle-element ()

  (assert (equal 15 (((hydra_lib_lists_foldl hydra_lib_math_add) 10) (list 5)))))

(defun test-foldl-negsubtraction-fold ()

  (assert (equal 4 (((hydra_lib_lists_foldl hydra_lib_math_sub) 10) (list 1 2 3)))))

;; foldr

(defun test-foldr-negsubtraction-fold-right ()

  (assert (equal 2 (((hydra_lib_lists_foldr hydra_lib_math_sub) 0) (list 1 2 3)))))

(defun test-foldr-negempty-list ()

  (assert (equal 5 (((hydra_lib_lists_foldr hydra_lib_math_add) 5) (list )))))

(defun test-foldr-negsingle-element ()

  (assert (equal 15 (((hydra_lib_lists_foldr hydra_lib_math_add) 10) (list 5)))))

(defun test-foldr-negsum-with-addition ()

  (assert (equal 10 (((hydra_lib_lists_foldr hydra_lib_math_add) 0) (list 1 2 3 4)))))

(defun test-foldr-negsubtraction-vs-foldl ()

  (assert (equal -8 (((hydra_lib_lists_foldr hydra_lib_math_sub) 10) (list 1 2 3)))))

;; group

(defun test-group-negconsecutive-duplicates ()

  (assert (equal (list (list 1 1) (list 2 2 2) (list 3) (list 1)) (hydra_lib_lists_group (list 1 1 2 2 2 3 1)))))

(defun test-group-negno-duplicates ()

  (assert (equal (list (list 1) (list 2) (list 3)) (hydra_lib_lists_group (list 1 2 3)))))

(defun test-group-negall-same ()

  (assert (equal (list (list 1 1 1)) (hydra_lib_lists_group (list 1 1 1)))))

(defun test-group-negempty-list ()

  (assert (equal (list ) (hydra_lib_lists_group (list )))))

(defun test-group-negsingle-element ()

  (assert (equal (list (list 1)) (hydra_lib_lists_group (list 1)))))

;; head

(defun test-head-negthree-element-list ()

  (assert (equal 1 (hydra_lib_lists_head (list 1 2 3)))))

(defun test-head-negsingle-element-list ()

  (assert (equal 42 (hydra_lib_lists_head (list 42)))))

(defun test-head-negnegative-numbers ()

  (assert (equal -1 (hydra_lib_lists_head (list -1 -2 -3)))))

(defun test-head-negstring-list ()

  (assert (equal "hello" (hydra_lib_lists_head (list "hello" "world")))))

;; init

(defun test-init-negmultiple-elements ()

  (assert (equal (list 1 2 3) (hydra_lib_lists_init (list 1 2 3 4)))))

(defun test-init-negtwo-elements ()

  (assert (equal (list 1) (hydra_lib_lists_init (list 1 2)))))

(defun test-init-negsingle-element ()

  (assert (equal (list ) (hydra_lib_lists_init (list 1)))))

(defun test-init-negstring-list ()

  (assert (equal (list "a" "b") (hydra_lib_lists_init (list "a" "b" "c")))))

;; intercalate

(defun test-intercalate-negdouble-zero-separator ()

  (assert (equal (list 1 2 3 0 0 4 5 0 0 6 7 8) ((hydra_lib_lists_intercalate (list 0 0)) (list (list 1 2 3) (list 4 5) (list 6 7 8))))))

(defun test-intercalate-negempty-separator ()

  (assert (equal (list 1 2 3 4) ((hydra_lib_lists_intercalate (list )) (list (list 1 2) (list 3 4))))))

(defun test-intercalate-negsingle-element-separator ()

  (assert (equal (list 1 99 2 99 3) ((hydra_lib_lists_intercalate (list 99)) (list (list 1) (list 2) (list 3))))))

(defun test-intercalate-negempty-list-of-lists ()

  (assert (equal (list ) ((hydra_lib_lists_intercalate (list 0)) (list )))))

(defun test-intercalate-negsingle-list ()

  (assert (equal (list 1 2 3) ((hydra_lib_lists_intercalate (list 0)) (list (list 1 2 3))))))

(defun test-intercalate-neglists-with-empty-lists ()

  (assert (equal (list 0 1 0) ((hydra_lib_lists_intercalate (list 0)) (list (list ) (list 1) (list ))))))

;; intersperse

(defun test-intersperse-negstring-interspersion ()

  (assert (equal (list "one" "and" "two" "and" "three") ((hydra_lib_lists_intersperse "and") (list "one" "two" "three")))))

(defun test-intersperse-negsingle-element ()

  (assert (equal (list "only") ((hydra_lib_lists_intersperse "x") (list "only")))))

(defun test-intersperse-negempty-list ()

  (assert (equal (list ) ((hydra_lib_lists_intersperse "x") (list )))))

(defun test-intersperse-negtwo-elements ()

  (assert (equal (list "a" "+" "b") ((hydra_lib_lists_intersperse "+") (list "a" "b")))))

(defun test-intersperse-negnumber-interspersion ()

  (assert (equal (list 1 0 2 0 3) ((hydra_lib_lists_intersperse 0) (list 1 2 3)))))

;; last

(defun test-last-negthree-element-list ()

  (assert (equal 3 (hydra_lib_lists_last (list 1 2 3)))))

(defun test-last-negsingle-element-list ()

  (assert (equal 42 (hydra_lib_lists_last (list 42)))))

(defun test-last-negnegative-numbers ()

  (assert (equal -3 (hydra_lib_lists_last (list -1 -2 -3)))))

(defun test-last-negstring-list ()

  (assert (equal "world" (hydra_lib_lists_last (list "hello" "world")))))

;; length

(defun test-length-negthree-elements ()

  (assert (equal 3 (hydra_lib_lists_length (list 1 2 3)))))

(defun test-length-negempty-list ()

  (assert (equal 0 (hydra_lib_lists_length (list )))))

(defun test-length-negsingle-element ()

  (assert (equal 1 (hydra_lib_lists_length (list 42)))))

(defun test-length-negmany-elements ()

  (assert (equal 10 (hydra_lib_lists_length (list 1 2 3 4 5 6 7 8 9 10)))))

(defun test-length-negstring-list ()

  (assert (equal 3 (hydra_lib_lists_length (list "a" "b" "c")))))

;; map

(defun test-map-negstring-to-uppercase ()

  (assert (equal (list "ONE" "TWO") ((hydra_lib_lists_map hydra_lib_strings_to_upper) (list "one" "two")))))

(defun test-map-negempty-list ()

  (assert (equal (list ) ((hydra_lib_lists_map hydra_lib_strings_to_upper) (list )))))

(defun test-map-negsingle-element ()

  (assert (equal (list "HELLO") ((hydra_lib_lists_map hydra_lib_strings_to_upper) (list "hello")))))

(defun test-map-negnumber-negation ()

  (assert (equal (list -1 -2 -3) ((hydra_lib_lists_map hydra_lib_math_negate) (list 1 2 3)))))

(defun test-map-negidentity-function ()

  (assert (equal (list 1 2 3) ((hydra_lib_lists_map hydra_lib_equality_identity) (list 1 2 3)))))

;; nub

(defun test-nub-negremove-duplicates ()

  (assert (equal (list 1 2 3 4) (hydra_lib_lists_nub (list 1 2 1 3 2 4)))))

(defun test-nub-negno-duplicates ()

  (assert (equal (list 1 2 3) (hydra_lib_lists_nub (list 1 2 3)))))

(defun test-nub-negall-duplicates ()

  (assert (equal (list 1) (hydra_lib_lists_nub (list 1 1 1)))))

(defun test-nub-negempty-list ()

  (assert (equal (list ) (hydra_lib_lists_nub (list )))))

(defun test-nub-negsingle-element ()

  (assert (equal (list 1) (hydra_lib_lists_nub (list 1)))))

(defun test-nub-negstring-duplicates ()

  (assert (equal (list "a" "b" "c") (hydra_lib_lists_nub (list "a" "b" "a" "c")))))

;; null

(defun test-null-negempty-int-list ()

  (assert (equal cl:t (hydra_lib_lists_null (list )))))

(defun test-null-negsingle-element ()

  (assert (equal cl:nil (hydra_lib_lists_null (list 1)))))

(defun test-null-negmultiple-elements ()

  (assert (equal cl:nil (hydra_lib_lists_null (list 1 2 3)))))

(defun test-null-negempty-string-list ()

  (assert (equal cl:t (hydra_lib_lists_null (list )))))

(defun test-null-negnon-negempty-string-list ()

  (assert (equal cl:nil (hydra_lib_lists_null (list "a")))))

;; partition

(defun test-partition-negpartition-greater-than-3 ()

  (assert (equal (list (list 4 5 6) (list 1 2 3)) ((hydra_lib_lists_partition (cl:lambda (x) ((hydra_lib_equality_gt x) 3))) (list 1 2 3 4 5 6)))))

(defun test-partition-negpartition-all-elements ()

  (assert (equal (list (list 1 2 3) (list )) ((hydra_lib_lists_partition (cl:lambda (x) ((hydra_lib_equality_lt x) 10))) (list 1 2 3)))))

(defun test-partition-negpartition-no-elements ()

  (assert (equal (list (list ) (list 1 2 3)) ((hydra_lib_lists_partition (cl:lambda (x) ((hydra_lib_equality_gt x) 10))) (list 1 2 3)))))

(defun test-partition-negpartition-even-numbers ()

  (assert (equal (list (list 2 4 6) (list 1 3 5)) ((hydra_lib_lists_partition (cl:lambda (x) (hydra_lib_math_even x))) (list 1 2 3 4 5 6)))))

(defun test-partition-negempty-list ()

  (assert (equal (list (list ) (list )) ((hydra_lib_lists_partition (cl:lambda (x) ((hydra_lib_equality_lt x) 5))) (list )))))

;; pure

(defun test-pure-negstring-element ()

  (assert (equal (list "one") (hydra_lib_lists_pure "one"))))

(defun test-pure-negempty-string ()

  (assert (equal (list "") (hydra_lib_lists_pure ""))))

(defun test-pure-negnumber-element ()

  (assert (equal (list 42) (hydra_lib_lists_pure 42))))

(defun test-pure-negnegative-number ()

  (assert (equal (list -5) (hydra_lib_lists_pure -5))))

;; replicate

(defun test-replicate-negreplicate-three-times ()

  (assert (equal (list 42 42 42) ((hydra_lib_lists_replicate 3) 42))))

(defun test-replicate-negreplicate-zero-times ()

  (assert (equal (list ) ((hydra_lib_lists_replicate 0) 1))))

(defun test-replicate-negreplicate-once ()

  (assert (equal (list 99) ((hydra_lib_lists_replicate 1) 99))))

(defun test-replicate-negreplicate-string ()

  (assert (equal (list "hello" "hello") ((hydra_lib_lists_replicate 2) "hello"))))

;; reverse

(defun test-reverse-negmultiple-elements ()

  (assert (equal (list 4 3 2 1) (hydra_lib_lists_reverse (list 1 2 3 4)))))

(defun test-reverse-negsingle-element ()

  (assert (equal (list 1) (hydra_lib_lists_reverse (list 1)))))

(defun test-reverse-negempty-list ()

  (assert (equal (list ) (hydra_lib_lists_reverse (list )))))

(defun test-reverse-negtwo-elements ()

  (assert (equal (list 2 1) (hydra_lib_lists_reverse (list 1 2)))))

(defun test-reverse-negstring-list ()

  (assert (equal (list "c" "b" "a") (hydra_lib_lists_reverse (list "a" "b" "c")))))

;; safeHead

(defun test-safehead-negnon-negempty-int-list ()

  (assert (equal 1 (hydra_lib_lists_safe_head (list 1 2 3)))))

(defun test-safehead-negempty-int-list ()

  (assert (equal nil (hydra_lib_lists_safe_head (list )))))

(defun test-safehead-negsingle-element ()

  (assert (equal 42 (hydra_lib_lists_safe_head (list 42)))))

(defun test-safehead-negnon-negempty-string-list ()

  (assert (equal "hello" (hydra_lib_lists_safe_head (list "hello" "world")))))

(defun test-safehead-negempty-string-list ()

  (assert (equal nil (hydra_lib_lists_safe_head (list )))))

;; singleton

(defun test-singleton-negnumber-element ()

  (assert (equal (list 42) (hydra_lib_lists_singleton 42))))

(defun test-singleton-negnegative-number ()

  (assert (equal (list -1) (hydra_lib_lists_singleton -1))))

(defun test-singleton-negzero ()

  (assert (equal (list 0) (hydra_lib_lists_singleton 0))))

(defun test-singleton-negstring-element ()

  (assert (equal (list "hello") (hydra_lib_lists_singleton "hello"))))

;; sort

(defun test-sort-negunsorted-numbers ()

  (assert (equal (list 1 1 3 4 5) (hydra_lib_lists_sort (list 3 1 4 1 5)))))

(defun test-sort-negalready-sorted ()

  (assert (equal (list 1 2 3) (hydra_lib_lists_sort (list 1 2 3)))))

(defun test-sort-negreverse-sorted ()

  (assert (equal (list 1 2 3) (hydra_lib_lists_sort (list 3 2 1)))))

(defun test-sort-negsingle-element ()

  (assert (equal (list 1) (hydra_lib_lists_sort (list 1)))))

(defun test-sort-negempty-list ()

  (assert (equal (list ) (hydra_lib_lists_sort (list )))))

(defun test-sort-negduplicates ()

  (assert (equal (list 1 1 2 2 3) (hydra_lib_lists_sort (list 2 1 2 3 1)))))

(defun test-sort-negstring-sort ()

  (assert (equal (list "apple" "banana" "zebra") (hydra_lib_lists_sort (list "zebra" "apple" "banana")))))

;; sortOn

(defun test-sorton-negsort-by-string-length ()

  (assert (equal (list "hi" "hello" "world") ((hydra_lib_lists_sort_on hydra_lib_strings_length) (list "hello" "hi" "world")))))

(defun test-sorton-negempty-string-list ()

  (assert (equal (list ) ((hydra_lib_lists_sort_on hydra_lib_strings_length) (list )))))

(defun test-sorton-negsingle-string-element ()

  (assert (equal (list "test") ((hydra_lib_lists_sort_on hydra_lib_strings_length) (list "test")))))

(defun test-sorton-negsort-by-negation ()

  (assert (equal (list 3 2 1) ((hydra_lib_lists_sort_on hydra_lib_math_negate) (list 1 3 2)))))

(defun test-sorton-negsort-by-absolute-value ()

  (assert (equal (list -1 2 -3) ((hydra_lib_lists_sort_on hydra_lib_math_abs) (list -1 -3 2)))))

;; span

(defun test-span-negspan-less-than-3 ()

  (assert (equal (list (list 1 2) (list 3 1 2)) ((hydra_lib_lists_span (cl:lambda (x) ((hydra_lib_equality_lt x) 3))) (list 1 2 3 1 2)))))

(defun test-span-negspan-all-elements ()

  (assert (equal (list (list 1 2 3) (list )) ((hydra_lib_lists_span (cl:lambda (x) ((hydra_lib_equality_lt x) 10))) (list 1 2 3)))))

(defun test-span-negspan-no-elements ()

  (assert (equal (list (list ) (list 1 2 3)) ((hydra_lib_lists_span (cl:lambda (x) ((hydra_lib_equality_gt x) 10))) (list 1 2 3)))))

(defun test-span-negempty-list ()

  (assert (equal (list (list ) (list )) ((hydra_lib_lists_span (cl:lambda (x) ((hydra_lib_equality_lt x) 5))) (list )))))

;; tail

(defun test-tail-negmultiple-elements ()

  (assert (equal (list 2 3 4) (hydra_lib_lists_tail (list 1 2 3 4)))))

(defun test-tail-negtwo-elements ()

  (assert (equal (list 2) (hydra_lib_lists_tail (list 1 2)))))

(defun test-tail-negsingle-element ()

  (assert (equal (list ) (hydra_lib_lists_tail (list 1)))))

(defun test-tail-negstring-list ()

  (assert (equal (list "b" "c") (hydra_lib_lists_tail (list "a" "b" "c")))))

;; take

(defun test-take-negtake-from-beginning ()

  (assert (equal (list 1 2) ((hydra_lib_lists_take 2) (list 1 2 3 4 5)))))

(defun test-take-negtake-zero-elements ()

  (assert (equal (list ) ((hydra_lib_lists_take 0) (list 1 2 3)))))

(defun test-take-negtake-all-elements ()

  (assert (equal (list 1 2 3) ((hydra_lib_lists_take 3) (list 1 2 3)))))

(defun test-take-negtake-more-than-length ()

  (assert (equal (list 1 2) ((hydra_lib_lists_take 5) (list 1 2)))))

(defun test-take-negtake-from-empty-list ()

  (assert (equal (list ) ((hydra_lib_lists_take 3) (list )))))

(defun test-take-negtake-negative-amount ()

  (assert (equal (list ) ((hydra_lib_lists_take -1) (list 1 2 3)))))

;; transpose

(defun test-transpose-negsquare-matrix ()

  (assert (equal (list (list 1 4) (list 2 5) (list 3 6)) (hydra_lib_lists_transpose (list (list 1 2 3) (list 4 5 6))))))

(defun test-transpose-negempty-lists ()

  (assert (equal (list ) (hydra_lib_lists_transpose (list )))))

(defun test-transpose-negsingle-row ()

  (assert (equal (list (list 1) (list 2) (list 3)) (hydra_lib_lists_transpose (list (list 1 2 3))))))

(defun test-transpose-negsingle-column ()

  (assert (equal (list (list 1 2 3)) (hydra_lib_lists_transpose (list (list 1) (list 2) (list 3))))))

(defun test-transpose-negragged-matrix ()

  (assert (equal (list (list 1 3 4) (list 2 5) (list 6)) (hydra_lib_lists_transpose (list (list 1 2) (list 3) (list 4 5 6))))))

;; zip

(defun test-zip-negequal-length-lists ()

  (assert (equal (list (list 1 "a") (list 2 "b") (list 3 "c")) ((hydra_lib_lists_zip (list 1 2 3)) (list "a" "b" "c")))))

(defun test-zip-negfirst-list-shorter ()

  (assert (equal (list (list 1 "a") (list 2 "b")) ((hydra_lib_lists_zip (list 1 2)) (list "a" "b" "c")))))

(defun test-zip-negsecond-list-shorter ()

  (assert (equal (list (list 1 "a") (list 2 "b")) ((hydra_lib_lists_zip (list 1 2 3)) (list "a" "b")))))

(defun test-zip-negempty-first-list ()

  (assert (equal (list ) ((hydra_lib_lists_zip (list )) (list "a" "b")))))

(defun test-zip-negempty-second-list ()

  (assert (equal (list ) ((hydra_lib_lists_zip (list 1 2)) (list )))))

(defun test-zip-negboth-empty-lists ()

  (assert (equal (list ) ((hydra_lib_lists_zip (list )) (list )))))

;; zipWith

(defun test-zipwith-negaddition ()

  (assert (equal (list 5 7 9) (((hydra_lib_lists_zip_with hydra_lib_math_add) (list 1 2 3)) (list 4 5 6)))))

(defun test-zipwith-negfirst-list-shorter ()

  (assert (equal (list 5 7) (((hydra_lib_lists_zip_with hydra_lib_math_add) (list 1 2)) (list 4 5 6)))))

(defun test-zipwith-negsecond-list-shorter ()

  (assert (equal (list 5 7) (((hydra_lib_lists_zip_with hydra_lib_math_add) (list 1 2 3)) (list 4 5)))))

(defun test-zipwith-negempty-first-list ()

  (assert (equal (list ) (((hydra_lib_lists_zip_with hydra_lib_math_add) (list )) (list 1 2 3)))))

(defun test-zipwith-negempty-second-list ()

  (assert (equal (list ) (((hydra_lib_lists_zip_with hydra_lib_math_add) (list 1 2 3)) (list )))))

(defun test-zipwith-negstring-concatenation ()

  (assert (equal (list "a1" "b2") (((hydra_lib_lists_zip_with hydra_lib_strings_cat2) (list "a" "b")) (list "1" "2")))))
