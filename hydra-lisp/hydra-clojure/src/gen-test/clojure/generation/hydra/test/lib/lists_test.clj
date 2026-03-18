;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.lists primitives

(ns generation.hydra.test.lib.lists-test
  (:require [clojure.test :refer :all]))

;; apply

;; string transformations

(deftest test-apply-negstring-transformations-negstring-transformations

  (is (= (list "ONE" "TWO" "THREE" "one" "two" "three")

         ((hydra_lib_lists_apply (list hydra_lib_strings_to_upper hydra_lib_strings_to_lower)) (list "One" "Two" "Three")))))

;; edge cases

(deftest test-apply-negedge-cases-negempty-function-list

  (is (= (list )

         ((hydra_lib_lists_apply (list )) (list "a" "b")))))

(deftest test-apply-negedge-cases-negempty-input-list

  (is (= (list )

         ((hydra_lib_lists_apply (list hydra_lib_strings_to_upper)) (list )))))

(deftest test-apply-negedge-cases-negsingle-function

  (is (= (list "HELLO")

         ((hydra_lib_lists_apply (list hydra_lib_strings_to_upper)) (list "hello")))))

(deftest test-apply-negedge-cases-negsingle-input

  (is (= (list "TEST" "test")

         ((hydra_lib_lists_apply (list hydra_lib_strings_to_upper hydra_lib_strings_to_lower)) (list "Test")))))

;; at

(deftest test-at-negfirst-element

  (is (= 1

         ((hydra_lib_lists_at 0) (list 1 2 3)))))

(deftest test-at-negmiddle-element

  (is (= 2

         ((hydra_lib_lists_at 1) (list 1 2 3)))))

(deftest test-at-neglast-element

  (is (= 3

         ((hydra_lib_lists_at 2) (list 1 2 3)))))

(deftest test-at-negsingle-element-list

  (is (= 42

         ((hydra_lib_lists_at 0) (list 42)))))

(deftest test-at-negstring-list-access

  (is (= "world"

         ((hydra_lib_lists_at 1) (list "hello" "world")))))

;; bind

(deftest test-bind-negnegation-function

  (is (= (list -1 -2 -3 -4)

         ((hydra_lib_lists_bind (list 1 2 3 4)) (fn [x] (hydra_lib_lists_pure (hydra_lib_math_negate x)))))))

(deftest test-bind-negempty-list

  (is (= (list )

         ((hydra_lib_lists_bind (list )) (fn [x] (hydra_lib_lists_pure (hydra_lib_math_negate x)))))))

(deftest test-bind-negsingle-element

  (is (= (list -5)

         ((hydra_lib_lists_bind (list 5)) (fn [x] (hydra_lib_lists_pure (hydra_lib_math_negate x)))))))

(deftest test-bind-negduplicate-elements

  (is (= (list -1 -1 -2)

         ((hydra_lib_lists_bind (list 1 1 2)) (fn [x] (hydra_lib_lists_pure (hydra_lib_math_negate x)))))))

;; concat

(deftest test-concat-negmultiple-non-negempty-lists

  (is (= (list 1 2 3 4 5 6 7 8)

         (hydra_lib_lists_concat (list (list 1 2 3) (list 4 5) (list 6 7 8))))))

(deftest test-concat-negempty-lists-included

  (is (= (list 1 2 3)

         (hydra_lib_lists_concat (list (list ) (list 1 2) (list ) (list 3))))))

(deftest test-concat-negsingle-list

  (is (= (list 1 2 3)

         (hydra_lib_lists_concat (list (list 1 2 3))))))

(deftest test-concat-negall-empty-lists

  (is (= (list )

         (hydra_lib_lists_concat (list (list ) (list ) (list ))))))

(deftest test-concat-negempty-list-of-lists

  (is (= (list )

         (hydra_lib_lists_concat (list )))))

;; concat2

(deftest test-concat2-negtwo-non-negempty-lists

  (is (= (list 1 2 3 4)

         ((hydra_lib_lists_concat2 (list 1 2)) (list 3 4)))))

(deftest test-concat2-negfirst-list-empty

  (is (= (list 1 2)

         ((hydra_lib_lists_concat2 (list )) (list 1 2)))))

(deftest test-concat2-negsecond-list-empty

  (is (= (list 1 2)

         ((hydra_lib_lists_concat2 (list 1 2)) (list )))))

(deftest test-concat2-negboth-lists-empty

  (is (= (list )

         ((hydra_lib_lists_concat2 (list )) (list )))))

(deftest test-concat2-negsingle-elements

  (is (= (list 1 2)

         ((hydra_lib_lists_concat2 (list 1)) (list 2)))))

(deftest test-concat2-negstring-lists

  (is (= (list "a" "b" "c" "d")

         ((hydra_lib_lists_concat2 (list "a" "b")) (list "c" "d")))))

;; cons

(deftest test-cons-negcons-to-non-negempty-list

  (is (= (list 1 2 3)

         ((hydra_lib_lists_cons 1) (list 2 3)))))

(deftest test-cons-negcons-to-empty-list

  (is (= (list 1)

         ((hydra_lib_lists_cons 1) (list )))))

(deftest test-cons-negcons-negative-number

  (is (= (list -1 2 3)

         ((hydra_lib_lists_cons -1) (list 2 3)))))

(deftest test-cons-negcons-string

  (is (= (list "hello" "world")

         ((hydra_lib_lists_cons "hello") (list "world")))))

;; drop

(deftest test-drop-negdrop-from-beginning

  (is (= (list 3 4 5)

         ((hydra_lib_lists_drop 2) (list 1 2 3 4 5)))))

(deftest test-drop-negdrop-zero-elements

  (is (= (list 1 2 3)

         ((hydra_lib_lists_drop 0) (list 1 2 3)))))

(deftest test-drop-negdrop-all-elements

  (is (= (list )

         ((hydra_lib_lists_drop 3) (list 1 2 3)))))

(deftest test-drop-negdrop-more-than-length

  (is (= (list )

         ((hydra_lib_lists_drop 5) (list 1 2)))))

(deftest test-drop-negdrop-from-empty-list

  (is (= (list )

         ((hydra_lib_lists_drop 3) (list )))))

(deftest test-drop-negdrop-negative-amount

  (is (= (list 1 2 3)

         ((hydra_lib_lists_drop -1) (list 1 2 3)))))

;; dropWhile

(deftest test-dropwhile-negdrop-while-less-than-3

  (is (= (list 3 2 1)

         ((hydra_lib_lists_drop_while (fn [x] ((hydra_lib_equality_lt x) 3))) (list 1 2 3 2 1)))))

(deftest test-dropwhile-negdrop-all-elements

  (is (= (list )

         ((hydra_lib_lists_drop_while (fn [x] ((hydra_lib_equality_lt x) 10))) (list 1 2 3)))))

(deftest test-dropwhile-negdrop-no-elements

  (is (= (list 1 2 3)

         ((hydra_lib_lists_drop_while (fn [x] ((hydra_lib_equality_lt x) 0))) (list 1 2 3)))))

(deftest test-dropwhile-negempty-list

  (is (= (list )

         ((hydra_lib_lists_drop_while (fn [x] ((hydra_lib_equality_lt x) 5))) (list )))))

;; elem

(deftest test-elem-negelement-present

  (is (= true

         ((hydra_lib_lists_elem 2) (list 1 2 3)))))

(deftest test-elem-negelement-not-present

  (is (= false

         ((hydra_lib_lists_elem 4) (list 1 2 3)))))

(deftest test-elem-negempty-list

  (is (= false

         ((hydra_lib_lists_elem 1) (list )))))

(deftest test-elem-negsingle-element-present

  (is (= true

         ((hydra_lib_lists_elem 1) (list 1)))))

(deftest test-elem-negsingle-element-not-present

  (is (= false

         ((hydra_lib_lists_elem 2) (list 1)))))

(deftest test-elem-negduplicate-elements

  (is (= true

         ((hydra_lib_lists_elem 2) (list 1 2 2 3)))))

(deftest test-elem-negstring-element-present

  (is (= true

         ((hydra_lib_lists_elem "hello") (list "world" "hello" "test")))))

(deftest test-elem-negstring-element-not-present

  (is (= false

         ((hydra_lib_lists_elem "missing") (list "world" "hello")))))

;; filter

(deftest test-filter-negfilter-positive-numbers

  (is (= (list 2 4 5)

         ((hydra_lib_lists_filter (fn [x] ((hydra_lib_equality_gt x) 0))) (list -1 2 -3 4 5)))))

(deftest test-filter-negfilter-all-elements

  (is (= (list 1 2 3)

         ((hydra_lib_lists_filter (fn [x] ((hydra_lib_equality_lt x) 10))) (list 1 2 3)))))

(deftest test-filter-negfilter-no-elements

  (is (= (list )

         ((hydra_lib_lists_filter (fn [x] ((hydra_lib_equality_gt x) 10))) (list 1 2 3)))))

(deftest test-filter-negempty-list

  (is (= (list )

         ((hydra_lib_lists_filter (fn [x] ((hydra_lib_equality_gt x) 0))) (list )))))

;; find

(deftest test-find-negfind-existing-element

  (is (= 4

         ((hydra_lib_lists_find (fn [x] ((hydra_lib_equality_gt x) 3))) (list 1 2 4 5)))))

(deftest test-find-negfind-first-matching

  (is (= 1

         ((hydra_lib_lists_find (fn [x] ((hydra_lib_equality_gt x) 0))) (list 1 2 3)))))

(deftest test-find-negfind-no-match

  (is (= nil

         ((hydra_lib_lists_find (fn [x] ((hydra_lib_equality_gt x) 10))) (list 1 2 3)))))

(deftest test-find-negfind-in-empty-list

  (is (= nil

         ((hydra_lib_lists_find (fn [x] ((hydra_lib_equality_gt x) 0))) (list )))))

(deftest test-find-negfind-single-element

  (is (= 42

         ((hydra_lib_lists_find (fn [x] ((hydra_lib_equality_equal x) 42))) (list 42)))))

;; foldl

(deftest test-foldl-negsum-with-addition

  (is (= 10

         (((hydra_lib_lists_foldl hydra_lib_math_add) 0) (list 1 2 3 4)))))

(deftest test-foldl-negproduct-with-multiplication

  (is (= 24

         (((hydra_lib_lists_foldl hydra_lib_math_mul) 1) (list 2 3 4)))))

(deftest test-foldl-negempty-list

  (is (= 5

         (((hydra_lib_lists_foldl hydra_lib_math_add) 5) (list )))))

(deftest test-foldl-negsingle-element

  (is (= 15

         (((hydra_lib_lists_foldl hydra_lib_math_add) 10) (list 5)))))

(deftest test-foldl-negsubtraction-fold

  (is (= 4

         (((hydra_lib_lists_foldl hydra_lib_math_sub) 10) (list 1 2 3)))))

;; foldr

(deftest test-foldr-negsubtraction-fold-right

  (is (= 2

         (((hydra_lib_lists_foldr hydra_lib_math_sub) 0) (list 1 2 3)))))

(deftest test-foldr-negempty-list

  (is (= 5

         (((hydra_lib_lists_foldr hydra_lib_math_add) 5) (list )))))

(deftest test-foldr-negsingle-element

  (is (= 15

         (((hydra_lib_lists_foldr hydra_lib_math_add) 10) (list 5)))))

(deftest test-foldr-negsum-with-addition

  (is (= 10

         (((hydra_lib_lists_foldr hydra_lib_math_add) 0) (list 1 2 3 4)))))

(deftest test-foldr-negsubtraction-vs-foldl

  (is (= -8

         (((hydra_lib_lists_foldr hydra_lib_math_sub) 10) (list 1 2 3)))))

;; group

(deftest test-group-negconsecutive-duplicates

  (is (= (list (list 1 1) (list 2 2 2) (list 3) (list 1))

         (hydra_lib_lists_group (list 1 1 2 2 2 3 1)))))

(deftest test-group-negno-duplicates

  (is (= (list (list 1) (list 2) (list 3))

         (hydra_lib_lists_group (list 1 2 3)))))

(deftest test-group-negall-same

  (is (= (list (list 1 1 1))

         (hydra_lib_lists_group (list 1 1 1)))))

(deftest test-group-negempty-list

  (is (= (list )

         (hydra_lib_lists_group (list )))))

(deftest test-group-negsingle-element

  (is (= (list (list 1))

         (hydra_lib_lists_group (list 1)))))

;; head

(deftest test-head-negthree-element-list

  (is (= 1

         (hydra_lib_lists_head (list 1 2 3)))))

(deftest test-head-negsingle-element-list

  (is (= 42

         (hydra_lib_lists_head (list 42)))))

(deftest test-head-negnegative-numbers

  (is (= -1

         (hydra_lib_lists_head (list -1 -2 -3)))))

(deftest test-head-negstring-list

  (is (= "hello"

         (hydra_lib_lists_head (list "hello" "world")))))

;; init

(deftest test-init-negmultiple-elements

  (is (= (list 1 2 3)

         (hydra_lib_lists_init (list 1 2 3 4)))))

(deftest test-init-negtwo-elements

  (is (= (list 1)

         (hydra_lib_lists_init (list 1 2)))))

(deftest test-init-negsingle-element

  (is (= (list )

         (hydra_lib_lists_init (list 1)))))

(deftest test-init-negstring-list

  (is (= (list "a" "b")

         (hydra_lib_lists_init (list "a" "b" "c")))))

;; intercalate

(deftest test-intercalate-negdouble-zero-separator

  (is (= (list 1 2 3 0 0 4 5 0 0 6 7 8)

         ((hydra_lib_lists_intercalate (list 0 0)) (list (list 1 2 3) (list 4 5) (list 6 7 8))))))

(deftest test-intercalate-negempty-separator

  (is (= (list 1 2 3 4)

         ((hydra_lib_lists_intercalate (list )) (list (list 1 2) (list 3 4))))))

(deftest test-intercalate-negsingle-element-separator

  (is (= (list 1 99 2 99 3)

         ((hydra_lib_lists_intercalate (list 99)) (list (list 1) (list 2) (list 3))))))

(deftest test-intercalate-negempty-list-of-lists

  (is (= (list )

         ((hydra_lib_lists_intercalate (list 0)) (list )))))

(deftest test-intercalate-negsingle-list

  (is (= (list 1 2 3)

         ((hydra_lib_lists_intercalate (list 0)) (list (list 1 2 3))))))

(deftest test-intercalate-neglists-with-empty-lists

  (is (= (list 0 1 0)

         ((hydra_lib_lists_intercalate (list 0)) (list (list ) (list 1) (list ))))))

;; intersperse

(deftest test-intersperse-negstring-interspersion

  (is (= (list "one" "and" "two" "and" "three")

         ((hydra_lib_lists_intersperse "and") (list "one" "two" "three")))))

(deftest test-intersperse-negsingle-element

  (is (= (list "only")

         ((hydra_lib_lists_intersperse "x") (list "only")))))

(deftest test-intersperse-negempty-list

  (is (= (list )

         ((hydra_lib_lists_intersperse "x") (list )))))

(deftest test-intersperse-negtwo-elements

  (is (= (list "a" "+" "b")

         ((hydra_lib_lists_intersperse "+") (list "a" "b")))))

(deftest test-intersperse-negnumber-interspersion

  (is (= (list 1 0 2 0 3)

         ((hydra_lib_lists_intersperse 0) (list 1 2 3)))))

;; last

(deftest test-last-negthree-element-list

  (is (= 3

         (hydra_lib_lists_last (list 1 2 3)))))

(deftest test-last-negsingle-element-list

  (is (= 42

         (hydra_lib_lists_last (list 42)))))

(deftest test-last-negnegative-numbers

  (is (= -3

         (hydra_lib_lists_last (list -1 -2 -3)))))

(deftest test-last-negstring-list

  (is (= "world"

         (hydra_lib_lists_last (list "hello" "world")))))

;; length

(deftest test-length-negthree-elements

  (is (= 3

         (hydra_lib_lists_length (list 1 2 3)))))

(deftest test-length-negempty-list

  (is (= 0

         (hydra_lib_lists_length (list )))))

(deftest test-length-negsingle-element

  (is (= 1

         (hydra_lib_lists_length (list 42)))))

(deftest test-length-negmany-elements

  (is (= 10

         (hydra_lib_lists_length (list 1 2 3 4 5 6 7 8 9 10)))))

(deftest test-length-negstring-list

  (is (= 3

         (hydra_lib_lists_length (list "a" "b" "c")))))

;; map

(deftest test-map-negstring-to-uppercase

  (is (= (list "ONE" "TWO")

         ((hydra_lib_lists_map hydra_lib_strings_to_upper) (list "one" "two")))))

(deftest test-map-negempty-list

  (is (= (list )

         ((hydra_lib_lists_map hydra_lib_strings_to_upper) (list )))))

(deftest test-map-negsingle-element

  (is (= (list "HELLO")

         ((hydra_lib_lists_map hydra_lib_strings_to_upper) (list "hello")))))

(deftest test-map-negnumber-negation

  (is (= (list -1 -2 -3)

         ((hydra_lib_lists_map hydra_lib_math_negate) (list 1 2 3)))))

(deftest test-map-negidentity-function

  (is (= (list 1 2 3)

         ((hydra_lib_lists_map hydra_lib_equality_identity) (list 1 2 3)))))

;; nub

(deftest test-nub-negremove-duplicates

  (is (= (list 1 2 3 4)

         (hydra_lib_lists_nub (list 1 2 1 3 2 4)))))

(deftest test-nub-negno-duplicates

  (is (= (list 1 2 3)

         (hydra_lib_lists_nub (list 1 2 3)))))

(deftest test-nub-negall-duplicates

  (is (= (list 1)

         (hydra_lib_lists_nub (list 1 1 1)))))

(deftest test-nub-negempty-list

  (is (= (list )

         (hydra_lib_lists_nub (list )))))

(deftest test-nub-negsingle-element

  (is (= (list 1)

         (hydra_lib_lists_nub (list 1)))))

(deftest test-nub-negstring-duplicates

  (is (= (list "a" "b" "c")

         (hydra_lib_lists_nub (list "a" "b" "a" "c")))))

;; null

(deftest test-null-negempty-int-list

  (is (= true

         (hydra_lib_lists_null (list )))))

(deftest test-null-negsingle-element

  (is (= false

         (hydra_lib_lists_null (list 1)))))

(deftest test-null-negmultiple-elements

  (is (= false

         (hydra_lib_lists_null (list 1 2 3)))))

(deftest test-null-negempty-string-list

  (is (= true

         (hydra_lib_lists_null (list )))))

(deftest test-null-negnon-negempty-string-list

  (is (= false

         (hydra_lib_lists_null (list "a")))))

;; partition

(deftest test-partition-negpartition-greater-than-3

  (is (= (list (list 4 5 6) (list 1 2 3))

         ((hydra_lib_lists_partition (fn [x] ((hydra_lib_equality_gt x) 3))) (list 1 2 3 4 5 6)))))

(deftest test-partition-negpartition-all-elements

  (is (= (list (list 1 2 3) (list ))

         ((hydra_lib_lists_partition (fn [x] ((hydra_lib_equality_lt x) 10))) (list 1 2 3)))))

(deftest test-partition-negpartition-no-elements

  (is (= (list (list ) (list 1 2 3))

         ((hydra_lib_lists_partition (fn [x] ((hydra_lib_equality_gt x) 10))) (list 1 2 3)))))

(deftest test-partition-negpartition-even-numbers

  (is (= (list (list 2 4 6) (list 1 3 5))

         ((hydra_lib_lists_partition (fn [x] (hydra_lib_math_even x))) (list 1 2 3 4 5 6)))))

(deftest test-partition-negempty-list

  (is (= (list (list ) (list ))

         ((hydra_lib_lists_partition (fn [x] ((hydra_lib_equality_lt x) 5))) (list )))))

;; pure

(deftest test-pure-negstring-element

  (is (= (list "one")

         (hydra_lib_lists_pure "one"))))

(deftest test-pure-negempty-string

  (is (= (list "")

         (hydra_lib_lists_pure ""))))

(deftest test-pure-negnumber-element

  (is (= (list 42)

         (hydra_lib_lists_pure 42))))

(deftest test-pure-negnegative-number

  (is (= (list -5)

         (hydra_lib_lists_pure -5))))

;; replicate

(deftest test-replicate-negreplicate-three-times

  (is (= (list 42 42 42)

         ((hydra_lib_lists_replicate 3) 42))))

(deftest test-replicate-negreplicate-zero-times

  (is (= (list )

         ((hydra_lib_lists_replicate 0) 1))))

(deftest test-replicate-negreplicate-once

  (is (= (list 99)

         ((hydra_lib_lists_replicate 1) 99))))

(deftest test-replicate-negreplicate-string

  (is (= (list "hello" "hello")

         ((hydra_lib_lists_replicate 2) "hello"))))

;; reverse

(deftest test-reverse-negmultiple-elements

  (is (= (list 4 3 2 1)

         (hydra_lib_lists_reverse (list 1 2 3 4)))))

(deftest test-reverse-negsingle-element

  (is (= (list 1)

         (hydra_lib_lists_reverse (list 1)))))

(deftest test-reverse-negempty-list

  (is (= (list )

         (hydra_lib_lists_reverse (list )))))

(deftest test-reverse-negtwo-elements

  (is (= (list 2 1)

         (hydra_lib_lists_reverse (list 1 2)))))

(deftest test-reverse-negstring-list

  (is (= (list "c" "b" "a")

         (hydra_lib_lists_reverse (list "a" "b" "c")))))

;; safeHead

(deftest test-safehead-negnon-negempty-int-list

  (is (= 1

         (hydra_lib_lists_safe_head (list 1 2 3)))))

(deftest test-safehead-negempty-int-list

  (is (= nil

         (hydra_lib_lists_safe_head (list )))))

(deftest test-safehead-negsingle-element

  (is (= 42

         (hydra_lib_lists_safe_head (list 42)))))

(deftest test-safehead-negnon-negempty-string-list

  (is (= "hello"

         (hydra_lib_lists_safe_head (list "hello" "world")))))

(deftest test-safehead-negempty-string-list

  (is (= nil

         (hydra_lib_lists_safe_head (list )))))

;; singleton

(deftest test-singleton-negnumber-element

  (is (= (list 42)

         (hydra_lib_lists_singleton 42))))

(deftest test-singleton-negnegative-number

  (is (= (list -1)

         (hydra_lib_lists_singleton -1))))

(deftest test-singleton-negzero

  (is (= (list 0)

         (hydra_lib_lists_singleton 0))))

(deftest test-singleton-negstring-element

  (is (= (list "hello")

         (hydra_lib_lists_singleton "hello"))))

;; sort

(deftest test-sort-negunsorted-numbers

  (is (= (list 1 1 3 4 5)

         (hydra_lib_lists_sort (list 3 1 4 1 5)))))

(deftest test-sort-negalready-sorted

  (is (= (list 1 2 3)

         (hydra_lib_lists_sort (list 1 2 3)))))

(deftest test-sort-negreverse-sorted

  (is (= (list 1 2 3)

         (hydra_lib_lists_sort (list 3 2 1)))))

(deftest test-sort-negsingle-element

  (is (= (list 1)

         (hydra_lib_lists_sort (list 1)))))

(deftest test-sort-negempty-list

  (is (= (list )

         (hydra_lib_lists_sort (list )))))

(deftest test-sort-negduplicates

  (is (= (list 1 1 2 2 3)

         (hydra_lib_lists_sort (list 2 1 2 3 1)))))

(deftest test-sort-negstring-sort

  (is (= (list "apple" "banana" "zebra")

         (hydra_lib_lists_sort (list "zebra" "apple" "banana")))))

;; sortOn

(deftest test-sorton-negsort-by-string-length

  (is (= (list "hi" "hello" "world")

         ((hydra_lib_lists_sort_on hydra_lib_strings_length) (list "hello" "hi" "world")))))

(deftest test-sorton-negempty-string-list

  (is (= (list )

         ((hydra_lib_lists_sort_on hydra_lib_strings_length) (list )))))

(deftest test-sorton-negsingle-string-element

  (is (= (list "test")

         ((hydra_lib_lists_sort_on hydra_lib_strings_length) (list "test")))))

(deftest test-sorton-negsort-by-negation

  (is (= (list 3 2 1)

         ((hydra_lib_lists_sort_on hydra_lib_math_negate) (list 1 3 2)))))

(deftest test-sorton-negsort-by-absolute-value

  (is (= (list -1 2 -3)

         ((hydra_lib_lists_sort_on hydra_lib_math_abs) (list -1 -3 2)))))

;; span

(deftest test-span-negspan-less-than-3

  (is (= (list (list 1 2) (list 3 1 2))

         ((hydra_lib_lists_span (fn [x] ((hydra_lib_equality_lt x) 3))) (list 1 2 3 1 2)))))

(deftest test-span-negspan-all-elements

  (is (= (list (list 1 2 3) (list ))

         ((hydra_lib_lists_span (fn [x] ((hydra_lib_equality_lt x) 10))) (list 1 2 3)))))

(deftest test-span-negspan-no-elements

  (is (= (list (list ) (list 1 2 3))

         ((hydra_lib_lists_span (fn [x] ((hydra_lib_equality_gt x) 10))) (list 1 2 3)))))

(deftest test-span-negempty-list

  (is (= (list (list ) (list ))

         ((hydra_lib_lists_span (fn [x] ((hydra_lib_equality_lt x) 5))) (list )))))

;; tail

(deftest test-tail-negmultiple-elements

  (is (= (list 2 3 4)

         (hydra_lib_lists_tail (list 1 2 3 4)))))

(deftest test-tail-negtwo-elements

  (is (= (list 2)

         (hydra_lib_lists_tail (list 1 2)))))

(deftest test-tail-negsingle-element

  (is (= (list )

         (hydra_lib_lists_tail (list 1)))))

(deftest test-tail-negstring-list

  (is (= (list "b" "c")

         (hydra_lib_lists_tail (list "a" "b" "c")))))

;; take

(deftest test-take-negtake-from-beginning

  (is (= (list 1 2)

         ((hydra_lib_lists_take 2) (list 1 2 3 4 5)))))

(deftest test-take-negtake-zero-elements

  (is (= (list )

         ((hydra_lib_lists_take 0) (list 1 2 3)))))

(deftest test-take-negtake-all-elements

  (is (= (list 1 2 3)

         ((hydra_lib_lists_take 3) (list 1 2 3)))))

(deftest test-take-negtake-more-than-length

  (is (= (list 1 2)

         ((hydra_lib_lists_take 5) (list 1 2)))))

(deftest test-take-negtake-from-empty-list

  (is (= (list )

         ((hydra_lib_lists_take 3) (list )))))

(deftest test-take-negtake-negative-amount

  (is (= (list )

         ((hydra_lib_lists_take -1) (list 1 2 3)))))

;; transpose

(deftest test-transpose-negsquare-matrix

  (is (= (list (list 1 4) (list 2 5) (list 3 6))

         (hydra_lib_lists_transpose (list (list 1 2 3) (list 4 5 6))))))

(deftest test-transpose-negempty-lists

  (is (= (list )

         (hydra_lib_lists_transpose (list )))))

(deftest test-transpose-negsingle-row

  (is (= (list (list 1) (list 2) (list 3))

         (hydra_lib_lists_transpose (list (list 1 2 3))))))

(deftest test-transpose-negsingle-column

  (is (= (list (list 1 2 3))

         (hydra_lib_lists_transpose (list (list 1) (list 2) (list 3))))))

(deftest test-transpose-negragged-matrix

  (is (= (list (list 1 3 4) (list 2 5) (list 6))

         (hydra_lib_lists_transpose (list (list 1 2) (list 3) (list 4 5 6))))))

;; zip

(deftest test-zip-negequal-length-lists

  (is (= (list (list 1 "a") (list 2 "b") (list 3 "c"))

         ((hydra_lib_lists_zip (list 1 2 3)) (list "a" "b" "c")))))

(deftest test-zip-negfirst-list-shorter

  (is (= (list (list 1 "a") (list 2 "b"))

         ((hydra_lib_lists_zip (list 1 2)) (list "a" "b" "c")))))

(deftest test-zip-negsecond-list-shorter

  (is (= (list (list 1 "a") (list 2 "b"))

         ((hydra_lib_lists_zip (list 1 2 3)) (list "a" "b")))))

(deftest test-zip-negempty-first-list

  (is (= (list )

         ((hydra_lib_lists_zip (list )) (list "a" "b")))))

(deftest test-zip-negempty-second-list

  (is (= (list )

         ((hydra_lib_lists_zip (list 1 2)) (list )))))

(deftest test-zip-negboth-empty-lists

  (is (= (list )

         ((hydra_lib_lists_zip (list )) (list )))))

;; zipWith

(deftest test-zipwith-negaddition

  (is (= (list 5 7 9)

         (((hydra_lib_lists_zip_with hydra_lib_math_add) (list 1 2 3)) (list 4 5 6)))))

(deftest test-zipwith-negfirst-list-shorter

  (is (= (list 5 7)

         (((hydra_lib_lists_zip_with hydra_lib_math_add) (list 1 2)) (list 4 5 6)))))

(deftest test-zipwith-negsecond-list-shorter

  (is (= (list 5 7)

         (((hydra_lib_lists_zip_with hydra_lib_math_add) (list 1 2 3)) (list 4 5)))))

(deftest test-zipwith-negempty-first-list

  (is (= (list )

         (((hydra_lib_lists_zip_with hydra_lib_math_add) (list )) (list 1 2 3)))))

(deftest test-zipwith-negempty-second-list

  (is (= (list )

         (((hydra_lib_lists_zip_with hydra_lib_math_add) (list 1 2 3)) (list )))))

(deftest test-zipwith-negstring-concatenation

  (is (= (list "a1" "b2")

         (((hydra_lib_lists_zip_with hydra_lib_strings_cat2) (list "a" "b")) (list "1" "2")))))
