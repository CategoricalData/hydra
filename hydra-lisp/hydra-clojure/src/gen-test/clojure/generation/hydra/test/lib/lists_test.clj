;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.lists primitives

(ns generation.hydra.test.lib.lists-test
  (:require [clojure.test :refer :all]))

;; apply

;; string transformations

(deftest test-lists-negapply-negstring-transformations-negstring-transformations

  (is (= (list "ONE" "TWO" "THREE" "one" "two" "three")

         ((hydra_lib_lists_apply (list hydra_lib_strings_to_upper hydra_lib_strings_to_lower)) (list "One" "Two" "Three")))))

;; edge cases

(deftest test-lists-negapply-negedge-cases-negempty-function-list

  (is (= (list )

         ((hydra_lib_lists_apply (list )) (list "a" "b")))))

(deftest test-lists-negapply-negedge-cases-negempty-input-list

  (is (= (list )

         ((hydra_lib_lists_apply (list hydra_lib_strings_to_upper)) (list )))))

(deftest test-lists-negapply-negedge-cases-negsingle-function

  (is (= (list "HELLO")

         ((hydra_lib_lists_apply (list hydra_lib_strings_to_upper)) (list "hello")))))

(deftest test-lists-negapply-negedge-cases-negsingle-input

  (is (= (list "TEST" "test")

         ((hydra_lib_lists_apply (list hydra_lib_strings_to_upper hydra_lib_strings_to_lower)) (list "Test")))))

;; at

(deftest test-lists-negat-negfirst-element

  (is (= 1

         ((hydra_lib_lists_at 0) (list 1 2 3)))))

(deftest test-lists-negat-negmiddle-element

  (is (= 2

         ((hydra_lib_lists_at 1) (list 1 2 3)))))

(deftest test-lists-negat-neglast-element

  (is (= 3

         ((hydra_lib_lists_at 2) (list 1 2 3)))))

(deftest test-lists-negat-negsingle-element-list

  (is (= 42

         ((hydra_lib_lists_at 0) (list 42)))))

(deftest test-lists-negat-negstring-list-access

  (is (= "world"

         ((hydra_lib_lists_at 1) (list "hello" "world")))))

;; bind

(deftest test-lists-negbind-negnegation-function

  (is (= (list -1 -2 -3 -4)

         ((hydra_lib_lists_bind (list 1 2 3 4)) (fn [x] (hydra_lib_lists_pure (hydra_lib_math_negate x)))))))

(deftest test-lists-negbind-negempty-list

  (is (= (list )

         ((hydra_lib_lists_bind (list )) (fn [x] (hydra_lib_lists_pure (hydra_lib_math_negate x)))))))

(deftest test-lists-negbind-negsingle-element

  (is (= (list -5)

         ((hydra_lib_lists_bind (list 5)) (fn [x] (hydra_lib_lists_pure (hydra_lib_math_negate x)))))))

(deftest test-lists-negbind-negduplicate-elements

  (is (= (list -1 -1 -2)

         ((hydra_lib_lists_bind (list 1 1 2)) (fn [x] (hydra_lib_lists_pure (hydra_lib_math_negate x)))))))

;; concat

(deftest test-lists-negconcat-negmultiple-non-negempty-lists

  (is (= (list 1 2 3 4 5 6 7 8)

         (hydra_lib_lists_concat (list (list 1 2 3) (list 4 5) (list 6 7 8))))))

(deftest test-lists-negconcat-negempty-lists-included

  (is (= (list 1 2 3)

         (hydra_lib_lists_concat (list (list ) (list 1 2) (list ) (list 3))))))

(deftest test-lists-negconcat-negsingle-list

  (is (= (list 1 2 3)

         (hydra_lib_lists_concat (list (list 1 2 3))))))

(deftest test-lists-negconcat-negall-empty-lists

  (is (= (list )

         (hydra_lib_lists_concat (list (list ) (list ) (list ))))))

(deftest test-lists-negconcat-negempty-list-of-lists

  (is (= (list )

         (hydra_lib_lists_concat (list )))))

;; concat2

(deftest test-lists-negconcat2-negtwo-non-negempty-lists

  (is (= (list 1 2 3 4)

         ((hydra_lib_lists_concat2 (list 1 2)) (list 3 4)))))

(deftest test-lists-negconcat2-negfirst-list-empty

  (is (= (list 1 2)

         ((hydra_lib_lists_concat2 (list )) (list 1 2)))))

(deftest test-lists-negconcat2-negsecond-list-empty

  (is (= (list 1 2)

         ((hydra_lib_lists_concat2 (list 1 2)) (list )))))

(deftest test-lists-negconcat2-negboth-lists-empty

  (is (= (list )

         ((hydra_lib_lists_concat2 (list )) (list )))))

(deftest test-lists-negconcat2-negsingle-elements

  (is (= (list 1 2)

         ((hydra_lib_lists_concat2 (list 1)) (list 2)))))

(deftest test-lists-negconcat2-negstring-lists

  (is (= (list "a" "b" "c" "d")

         ((hydra_lib_lists_concat2 (list "a" "b")) (list "c" "d")))))

;; cons

(deftest test-lists-negcons-negcons-to-non-negempty-list

  (is (= (list 1 2 3)

         ((hydra_lib_lists_cons 1) (list 2 3)))))

(deftest test-lists-negcons-negcons-to-empty-list

  (is (= (list 1)

         ((hydra_lib_lists_cons 1) (list )))))

(deftest test-lists-negcons-negcons-negative-number

  (is (= (list -1 2 3)

         ((hydra_lib_lists_cons -1) (list 2 3)))))

(deftest test-lists-negcons-negcons-string

  (is (= (list "hello" "world")

         ((hydra_lib_lists_cons "hello") (list "world")))))

;; drop

(deftest test-lists-negdrop-negdrop-from-beginning

  (is (= (list 3 4 5)

         ((hydra_lib_lists_drop 2) (list 1 2 3 4 5)))))

(deftest test-lists-negdrop-negdrop-zero-elements

  (is (= (list 1 2 3)

         ((hydra_lib_lists_drop 0) (list 1 2 3)))))

(deftest test-lists-negdrop-negdrop-all-elements

  (is (= (list )

         ((hydra_lib_lists_drop 3) (list 1 2 3)))))

(deftest test-lists-negdrop-negdrop-more-than-length

  (is (= (list )

         ((hydra_lib_lists_drop 5) (list 1 2)))))

(deftest test-lists-negdrop-negdrop-from-empty-list

  (is (= (list )

         ((hydra_lib_lists_drop 3) (list )))))

(deftest test-lists-negdrop-negdrop-negative-amount

  (is (= (list 1 2 3)

         ((hydra_lib_lists_drop -1) (list 1 2 3)))))

;; dropWhile

(deftest test-lists-negdropwhile-negdrop-while-less-than-3

  (is (= (list 3 2 1)

         ((hydra_lib_lists_drop_while (fn [x] ((hydra_lib_equality_lt x) 3))) (list 1 2 3 2 1)))))

(deftest test-lists-negdropwhile-negdrop-all-elements

  (is (= (list )

         ((hydra_lib_lists_drop_while (fn [x] ((hydra_lib_equality_lt x) 10))) (list 1 2 3)))))

(deftest test-lists-negdropwhile-negdrop-no-elements

  (is (= (list 1 2 3)

         ((hydra_lib_lists_drop_while (fn [x] ((hydra_lib_equality_lt x) 0))) (list 1 2 3)))))

(deftest test-lists-negdropwhile-negempty-list

  (is (= (list )

         ((hydra_lib_lists_drop_while (fn [x] ((hydra_lib_equality_lt x) 5))) (list )))))

;; elem

(deftest test-lists-negelem-negelement-present

  (is (= true

         ((hydra_lib_lists_elem 2) (list 1 2 3)))))

(deftest test-lists-negelem-negelement-not-present

  (is (= false

         ((hydra_lib_lists_elem 4) (list 1 2 3)))))

(deftest test-lists-negelem-negempty-list

  (is (= false

         ((hydra_lib_lists_elem 1) (list )))))

(deftest test-lists-negelem-negsingle-element-present

  (is (= true

         ((hydra_lib_lists_elem 1) (list 1)))))

(deftest test-lists-negelem-negsingle-element-not-present

  (is (= false

         ((hydra_lib_lists_elem 2) (list 1)))))

(deftest test-lists-negelem-negduplicate-elements

  (is (= true

         ((hydra_lib_lists_elem 2) (list 1 2 2 3)))))

(deftest test-lists-negelem-negstring-element-present

  (is (= true

         ((hydra_lib_lists_elem "hello") (list "world" "hello" "test")))))

(deftest test-lists-negelem-negstring-element-not-present

  (is (= false

         ((hydra_lib_lists_elem "missing") (list "world" "hello")))))

;; filter

(deftest test-lists-negfilter-negfilter-positive-numbers

  (is (= (list 2 4 5)

         ((hydra_lib_lists_filter (fn [x] ((hydra_lib_equality_gt x) 0))) (list -1 2 -3 4 5)))))

(deftest test-lists-negfilter-negfilter-all-elements

  (is (= (list 1 2 3)

         ((hydra_lib_lists_filter (fn [x] ((hydra_lib_equality_lt x) 10))) (list 1 2 3)))))

(deftest test-lists-negfilter-negfilter-no-elements

  (is (= (list )

         ((hydra_lib_lists_filter (fn [x] ((hydra_lib_equality_gt x) 10))) (list 1 2 3)))))

(deftest test-lists-negfilter-negempty-list

  (is (= (list )

         ((hydra_lib_lists_filter (fn [x] ((hydra_lib_equality_gt x) 0))) (list )))))

;; find

(deftest test-lists-negfind-negfind-existing-element

  (is (= (list :just 4)

         ((hydra_lib_lists_find (fn [x] ((hydra_lib_equality_gt x) 3))) (list 1 2 4 5)))))

(deftest test-lists-negfind-negfind-first-matching

  (is (= (list :just 1)

         ((hydra_lib_lists_find (fn [x] ((hydra_lib_equality_gt x) 0))) (list 1 2 3)))))

(deftest test-lists-negfind-negfind-no-match

  (is (= (list :nothing)

         ((hydra_lib_lists_find (fn [x] ((hydra_lib_equality_gt x) 10))) (list 1 2 3)))))

(deftest test-lists-negfind-negfind-in-empty-list

  (is (= (list :nothing)

         ((hydra_lib_lists_find (fn [x] ((hydra_lib_equality_gt x) 0))) (list )))))

(deftest test-lists-negfind-negfind-single-element

  (is (= (list :just 42)

         ((hydra_lib_lists_find (fn [x] ((hydra_lib_equality_equal x) 42))) (list 42)))))

;; foldl

(deftest test-lists-negfoldl-negsum-with-addition

  (is (= 10

         (((hydra_lib_lists_foldl hydra_lib_math_add) 0) (list 1 2 3 4)))))

(deftest test-lists-negfoldl-negproduct-with-multiplication

  (is (= 24

         (((hydra_lib_lists_foldl hydra_lib_math_mul) 1) (list 2 3 4)))))

(deftest test-lists-negfoldl-negempty-list

  (is (= 5

         (((hydra_lib_lists_foldl hydra_lib_math_add) 5) (list )))))

(deftest test-lists-negfoldl-negsingle-element

  (is (= 15

         (((hydra_lib_lists_foldl hydra_lib_math_add) 10) (list 5)))))

(deftest test-lists-negfoldl-negsubtraction-fold

  (is (= 4

         (((hydra_lib_lists_foldl hydra_lib_math_sub) 10) (list 1 2 3)))))

;; foldr

(deftest test-lists-negfoldr-negsubtraction-fold-right

  (is (= 2

         (((hydra_lib_lists_foldr hydra_lib_math_sub) 0) (list 1 2 3)))))

(deftest test-lists-negfoldr-negempty-list

  (is (= 5

         (((hydra_lib_lists_foldr hydra_lib_math_add) 5) (list )))))

(deftest test-lists-negfoldr-negsingle-element

  (is (= 15

         (((hydra_lib_lists_foldr hydra_lib_math_add) 10) (list 5)))))

(deftest test-lists-negfoldr-negsum-with-addition

  (is (= 10

         (((hydra_lib_lists_foldr hydra_lib_math_add) 0) (list 1 2 3 4)))))

(deftest test-lists-negfoldr-negsubtraction-vs-foldl

  (is (= -8

         (((hydra_lib_lists_foldr hydra_lib_math_sub) 10) (list 1 2 3)))))

;; group

(deftest test-lists-neggroup-negconsecutive-duplicates

  (is (= (list (list 1 1) (list 2 2 2) (list 3) (list 1))

         (hydra_lib_lists_group (list 1 1 2 2 2 3 1)))))

(deftest test-lists-neggroup-negno-duplicates

  (is (= (list (list 1) (list 2) (list 3))

         (hydra_lib_lists_group (list 1 2 3)))))

(deftest test-lists-neggroup-negall-same

  (is (= (list (list 1 1 1))

         (hydra_lib_lists_group (list 1 1 1)))))

(deftest test-lists-neggroup-negempty-list

  (is (= (list )

         (hydra_lib_lists_group (list )))))

(deftest test-lists-neggroup-negsingle-element

  (is (= (list (list 1))

         (hydra_lib_lists_group (list 1)))))

;; head

(deftest test-lists-neghead-negthree-element-list

  (is (= 1

         (hydra_lib_lists_head (list 1 2 3)))))

(deftest test-lists-neghead-negsingle-element-list

  (is (= 42

         (hydra_lib_lists_head (list 42)))))

(deftest test-lists-neghead-negnegative-numbers

  (is (= -1

         (hydra_lib_lists_head (list -1 -2 -3)))))

(deftest test-lists-neghead-negstring-list

  (is (= "hello"

         (hydra_lib_lists_head (list "hello" "world")))))

;; init

(deftest test-lists-neginit-negmultiple-elements

  (is (= (list 1 2 3)

         (hydra_lib_lists_init (list 1 2 3 4)))))

(deftest test-lists-neginit-negtwo-elements

  (is (= (list 1)

         (hydra_lib_lists_init (list 1 2)))))

(deftest test-lists-neginit-negsingle-element

  (is (= (list )

         (hydra_lib_lists_init (list 1)))))

(deftest test-lists-neginit-negstring-list

  (is (= (list "a" "b")

         (hydra_lib_lists_init (list "a" "b" "c")))))

;; intercalate

(deftest test-lists-negintercalate-negdouble-zero-separator

  (is (= (list 1 2 3 0 0 4 5 0 0 6 7 8)

         ((hydra_lib_lists_intercalate (list 0 0)) (list (list 1 2 3) (list 4 5) (list 6 7 8))))))

(deftest test-lists-negintercalate-negempty-separator

  (is (= (list 1 2 3 4)

         ((hydra_lib_lists_intercalate (list )) (list (list 1 2) (list 3 4))))))

(deftest test-lists-negintercalate-negsingle-element-separator

  (is (= (list 1 99 2 99 3)

         ((hydra_lib_lists_intercalate (list 99)) (list (list 1) (list 2) (list 3))))))

(deftest test-lists-negintercalate-negempty-list-of-lists

  (is (= (list )

         ((hydra_lib_lists_intercalate (list 0)) (list )))))

(deftest test-lists-negintercalate-negsingle-list

  (is (= (list 1 2 3)

         ((hydra_lib_lists_intercalate (list 0)) (list (list 1 2 3))))))

(deftest test-lists-negintercalate-neglists-with-empty-lists

  (is (= (list 0 1 0)

         ((hydra_lib_lists_intercalate (list 0)) (list (list ) (list 1) (list ))))))

;; intersperse

(deftest test-lists-negintersperse-negstring-interspersion

  (is (= (list "one" "and" "two" "and" "three")

         ((hydra_lib_lists_intersperse "and") (list "one" "two" "three")))))

(deftest test-lists-negintersperse-negsingle-element

  (is (= (list "only")

         ((hydra_lib_lists_intersperse "x") (list "only")))))

(deftest test-lists-negintersperse-negempty-list

  (is (= (list )

         ((hydra_lib_lists_intersperse "x") (list )))))

(deftest test-lists-negintersperse-negtwo-elements

  (is (= (list "a" "+" "b")

         ((hydra_lib_lists_intersperse "+") (list "a" "b")))))

(deftest test-lists-negintersperse-negnumber-interspersion

  (is (= (list 1 0 2 0 3)

         ((hydra_lib_lists_intersperse 0) (list 1 2 3)))))

;; last

(deftest test-lists-neglast-negthree-element-list

  (is (= 3

         (hydra_lib_lists_last (list 1 2 3)))))

(deftest test-lists-neglast-negsingle-element-list

  (is (= 42

         (hydra_lib_lists_last (list 42)))))

(deftest test-lists-neglast-negnegative-numbers

  (is (= -3

         (hydra_lib_lists_last (list -1 -2 -3)))))

(deftest test-lists-neglast-negstring-list

  (is (= "world"

         (hydra_lib_lists_last (list "hello" "world")))))

;; length

(deftest test-lists-neglength-negthree-elements

  (is (= 3

         (hydra_lib_lists_length (list 1 2 3)))))

(deftest test-lists-neglength-negempty-list

  (is (= 0

         (hydra_lib_lists_length (list )))))

(deftest test-lists-neglength-negsingle-element

  (is (= 1

         (hydra_lib_lists_length (list 42)))))

(deftest test-lists-neglength-negmany-elements

  (is (= 10

         (hydra_lib_lists_length (list 1 2 3 4 5 6 7 8 9 10)))))

(deftest test-lists-neglength-negstring-list

  (is (= 3

         (hydra_lib_lists_length (list "a" "b" "c")))))

;; map

(deftest test-lists-negmap-negstring-to-uppercase

  (is (= (list "ONE" "TWO")

         ((hydra_lib_lists_map hydra_lib_strings_to_upper) (list "one" "two")))))

(deftest test-lists-negmap-negempty-list

  (is (= (list )

         ((hydra_lib_lists_map hydra_lib_strings_to_upper) (list )))))

(deftest test-lists-negmap-negsingle-element

  (is (= (list "HELLO")

         ((hydra_lib_lists_map hydra_lib_strings_to_upper) (list "hello")))))

(deftest test-lists-negmap-negnumber-negation

  (is (= (list -1 -2 -3)

         ((hydra_lib_lists_map hydra_lib_math_negate) (list 1 2 3)))))

(deftest test-lists-negmap-negidentity-function

  (is (= (list 1 2 3)

         ((hydra_lib_lists_map hydra_lib_equality_identity) (list 1 2 3)))))

;; nub

(deftest test-lists-negnub-negremove-duplicates

  (is (= (list 1 2 3 4)

         (hydra_lib_lists_nub (list 1 2 1 3 2 4)))))

(deftest test-lists-negnub-negno-duplicates

  (is (= (list 1 2 3)

         (hydra_lib_lists_nub (list 1 2 3)))))

(deftest test-lists-negnub-negall-duplicates

  (is (= (list 1)

         (hydra_lib_lists_nub (list 1 1 1)))))

(deftest test-lists-negnub-negempty-list

  (is (= (list )

         (hydra_lib_lists_nub (list )))))

(deftest test-lists-negnub-negsingle-element

  (is (= (list 1)

         (hydra_lib_lists_nub (list 1)))))

(deftest test-lists-negnub-negstring-duplicates

  (is (= (list "a" "b" "c")

         (hydra_lib_lists_nub (list "a" "b" "a" "c")))))

;; null

(deftest test-lists-negnull-negempty-int-list

  (is (= true

         (hydra_lib_lists_null (list )))))

(deftest test-lists-negnull-negsingle-element

  (is (= false

         (hydra_lib_lists_null (list 1)))))

(deftest test-lists-negnull-negmultiple-elements

  (is (= false

         (hydra_lib_lists_null (list 1 2 3)))))

(deftest test-lists-negnull-negempty-string-list

  (is (= true

         (hydra_lib_lists_null (list )))))

(deftest test-lists-negnull-negnon-negempty-string-list

  (is (= false

         (hydra_lib_lists_null (list "a")))))

;; partition

(deftest test-lists-negpartition-negpartition-greater-than-3

  (is (= (list (list 4 5 6) (list 1 2 3))

         ((hydra_lib_lists_partition (fn [x] ((hydra_lib_equality_gt x) 3))) (list 1 2 3 4 5 6)))))

(deftest test-lists-negpartition-negpartition-all-elements

  (is (= (list (list 1 2 3) (list ))

         ((hydra_lib_lists_partition (fn [x] ((hydra_lib_equality_lt x) 10))) (list 1 2 3)))))

(deftest test-lists-negpartition-negpartition-no-elements

  (is (= (list (list ) (list 1 2 3))

         ((hydra_lib_lists_partition (fn [x] ((hydra_lib_equality_gt x) 10))) (list 1 2 3)))))

(deftest test-lists-negpartition-negpartition-even-numbers

  (is (= (list (list 2 4 6) (list 1 3 5))

         ((hydra_lib_lists_partition (fn [x] (hydra_lib_math_even x))) (list 1 2 3 4 5 6)))))

(deftest test-lists-negpartition-negempty-list

  (is (= (list (list ) (list ))

         ((hydra_lib_lists_partition (fn [x] ((hydra_lib_equality_lt x) 5))) (list )))))

;; pure

(deftest test-lists-negpure-negstring-element

  (is (= (list "one")

         (hydra_lib_lists_pure "one"))))

(deftest test-lists-negpure-negempty-string

  (is (= (list "")

         (hydra_lib_lists_pure ""))))

(deftest test-lists-negpure-negnumber-element

  (is (= (list 42)

         (hydra_lib_lists_pure 42))))

(deftest test-lists-negpure-negnegative-number

  (is (= (list -5)

         (hydra_lib_lists_pure -5))))

;; replicate

(deftest test-lists-negreplicate-negreplicate-three-times

  (is (= (list 42 42 42)

         ((hydra_lib_lists_replicate 3) 42))))

(deftest test-lists-negreplicate-negreplicate-zero-times

  (is (= (list )

         ((hydra_lib_lists_replicate 0) 1))))

(deftest test-lists-negreplicate-negreplicate-once

  (is (= (list 99)

         ((hydra_lib_lists_replicate 1) 99))))

(deftest test-lists-negreplicate-negreplicate-string

  (is (= (list "hello" "hello")

         ((hydra_lib_lists_replicate 2) "hello"))))

;; reverse

(deftest test-lists-negreverse-negmultiple-elements

  (is (= (list 4 3 2 1)

         (hydra_lib_lists_reverse (list 1 2 3 4)))))

(deftest test-lists-negreverse-negsingle-element

  (is (= (list 1)

         (hydra_lib_lists_reverse (list 1)))))

(deftest test-lists-negreverse-negempty-list

  (is (= (list )

         (hydra_lib_lists_reverse (list )))))

(deftest test-lists-negreverse-negtwo-elements

  (is (= (list 2 1)

         (hydra_lib_lists_reverse (list 1 2)))))

(deftest test-lists-negreverse-negstring-list

  (is (= (list "c" "b" "a")

         (hydra_lib_lists_reverse (list "a" "b" "c")))))

;; safeHead

(deftest test-lists-negsafehead-negnon-negempty-int-list

  (is (= (list :just 1)

         (hydra_lib_lists_safe_head (list 1 2 3)))))

(deftest test-lists-negsafehead-negempty-int-list

  (is (= (list :nothing)

         (hydra_lib_lists_safe_head (list )))))

(deftest test-lists-negsafehead-negsingle-element

  (is (= (list :just 42)

         (hydra_lib_lists_safe_head (list 42)))))

(deftest test-lists-negsafehead-negnon-negempty-string-list

  (is (= (list :just "hello")

         (hydra_lib_lists_safe_head (list "hello" "world")))))

(deftest test-lists-negsafehead-negempty-string-list

  (is (= (list :nothing)

         (hydra_lib_lists_safe_head (list )))))

;; singleton

(deftest test-lists-negsingleton-negnumber-element

  (is (= (list 42)

         (hydra_lib_lists_singleton 42))))

(deftest test-lists-negsingleton-negnegative-number

  (is (= (list -1)

         (hydra_lib_lists_singleton -1))))

(deftest test-lists-negsingleton-negzero

  (is (= (list 0)

         (hydra_lib_lists_singleton 0))))

(deftest test-lists-negsingleton-negstring-element

  (is (= (list "hello")

         (hydra_lib_lists_singleton "hello"))))

;; sort

(deftest test-lists-negsort-negunsorted-numbers

  (is (= (list 1 1 3 4 5)

         (hydra_lib_lists_sort (list 3 1 4 1 5)))))

(deftest test-lists-negsort-negalready-sorted

  (is (= (list 1 2 3)

         (hydra_lib_lists_sort (list 1 2 3)))))

(deftest test-lists-negsort-negreverse-sorted

  (is (= (list 1 2 3)

         (hydra_lib_lists_sort (list 3 2 1)))))

(deftest test-lists-negsort-negsingle-element

  (is (= (list 1)

         (hydra_lib_lists_sort (list 1)))))

(deftest test-lists-negsort-negempty-list

  (is (= (list )

         (hydra_lib_lists_sort (list )))))

(deftest test-lists-negsort-negduplicates

  (is (= (list 1 1 2 2 3)

         (hydra_lib_lists_sort (list 2 1 2 3 1)))))

(deftest test-lists-negsort-negstring-sort

  (is (= (list "apple" "banana" "zebra")

         (hydra_lib_lists_sort (list "zebra" "apple" "banana")))))

;; sortOn

(deftest test-lists-negsorton-negsort-by-string-length

  (is (= (list "hi" "hello" "world")

         ((hydra_lib_lists_sort_on hydra_lib_strings_length) (list "hello" "hi" "world")))))

(deftest test-lists-negsorton-negempty-string-list

  (is (= (list )

         ((hydra_lib_lists_sort_on hydra_lib_strings_length) (list )))))

(deftest test-lists-negsorton-negsingle-string-element

  (is (= (list "test")

         ((hydra_lib_lists_sort_on hydra_lib_strings_length) (list "test")))))

(deftest test-lists-negsorton-negsort-by-negation

  (is (= (list 3 2 1)

         ((hydra_lib_lists_sort_on hydra_lib_math_negate) (list 1 3 2)))))

(deftest test-lists-negsorton-negsort-by-absolute-value

  (is (= (list -1 2 -3)

         ((hydra_lib_lists_sort_on hydra_lib_math_abs) (list -1 -3 2)))))

;; span

(deftest test-lists-negspan-negspan-less-than-3

  (is (= (list (list 1 2) (list 3 1 2))

         ((hydra_lib_lists_span (fn [x] ((hydra_lib_equality_lt x) 3))) (list 1 2 3 1 2)))))

(deftest test-lists-negspan-negspan-all-elements

  (is (= (list (list 1 2 3) (list ))

         ((hydra_lib_lists_span (fn [x] ((hydra_lib_equality_lt x) 10))) (list 1 2 3)))))

(deftest test-lists-negspan-negspan-no-elements

  (is (= (list (list ) (list 1 2 3))

         ((hydra_lib_lists_span (fn [x] ((hydra_lib_equality_gt x) 10))) (list 1 2 3)))))

(deftest test-lists-negspan-negempty-list

  (is (= (list (list ) (list ))

         ((hydra_lib_lists_span (fn [x] ((hydra_lib_equality_lt x) 5))) (list )))))

;; tail

(deftest test-lists-negtail-negmultiple-elements

  (is (= (list 2 3 4)

         (hydra_lib_lists_tail (list 1 2 3 4)))))

(deftest test-lists-negtail-negtwo-elements

  (is (= (list 2)

         (hydra_lib_lists_tail (list 1 2)))))

(deftest test-lists-negtail-negsingle-element

  (is (= (list )

         (hydra_lib_lists_tail (list 1)))))

(deftest test-lists-negtail-negstring-list

  (is (= (list "b" "c")

         (hydra_lib_lists_tail (list "a" "b" "c")))))

;; take

(deftest test-lists-negtake-negtake-from-beginning

  (is (= (list 1 2)

         ((hydra_lib_lists_take 2) (list 1 2 3 4 5)))))

(deftest test-lists-negtake-negtake-zero-elements

  (is (= (list )

         ((hydra_lib_lists_take 0) (list 1 2 3)))))

(deftest test-lists-negtake-negtake-all-elements

  (is (= (list 1 2 3)

         ((hydra_lib_lists_take 3) (list 1 2 3)))))

(deftest test-lists-negtake-negtake-more-than-length

  (is (= (list 1 2)

         ((hydra_lib_lists_take 5) (list 1 2)))))

(deftest test-lists-negtake-negtake-from-empty-list

  (is (= (list )

         ((hydra_lib_lists_take 3) (list )))))

(deftest test-lists-negtake-negtake-negative-amount

  (is (= (list )

         ((hydra_lib_lists_take -1) (list 1 2 3)))))

;; transpose

(deftest test-lists-negtranspose-negsquare-matrix

  (is (= (list (list 1 4) (list 2 5) (list 3 6))

         (hydra_lib_lists_transpose (list (list 1 2 3) (list 4 5 6))))))

(deftest test-lists-negtranspose-negempty-lists

  (is (= (list )

         (hydra_lib_lists_transpose (list )))))

(deftest test-lists-negtranspose-negsingle-row

  (is (= (list (list 1) (list 2) (list 3))

         (hydra_lib_lists_transpose (list (list 1 2 3))))))

(deftest test-lists-negtranspose-negsingle-column

  (is (= (list (list 1 2 3))

         (hydra_lib_lists_transpose (list (list 1) (list 2) (list 3))))))

(deftest test-lists-negtranspose-negragged-matrix

  (is (= (list (list 1 3 4) (list 2 5) (list 6))

         (hydra_lib_lists_transpose (list (list 1 2) (list 3) (list 4 5 6))))))

;; zip

(deftest test-lists-negzip-negequal-length-lists

  (is (= (list (list 1 "a") (list 2 "b") (list 3 "c"))

         ((hydra_lib_lists_zip (list 1 2 3)) (list "a" "b" "c")))))

(deftest test-lists-negzip-negfirst-list-shorter

  (is (= (list (list 1 "a") (list 2 "b"))

         ((hydra_lib_lists_zip (list 1 2)) (list "a" "b" "c")))))

(deftest test-lists-negzip-negsecond-list-shorter

  (is (= (list (list 1 "a") (list 2 "b"))

         ((hydra_lib_lists_zip (list 1 2 3)) (list "a" "b")))))

(deftest test-lists-negzip-negempty-first-list

  (is (= (list )

         ((hydra_lib_lists_zip (list )) (list "a" "b")))))

(deftest test-lists-negzip-negempty-second-list

  (is (= (list )

         ((hydra_lib_lists_zip (list 1 2)) (list )))))

(deftest test-lists-negzip-negboth-empty-lists

  (is (= (list )

         ((hydra_lib_lists_zip (list )) (list )))))

;; zipWith

(deftest test-lists-negzipwith-negaddition

  (is (= (list 5 7 9)

         (((hydra_lib_lists_zip_with hydra_lib_math_add) (list 1 2 3)) (list 4 5 6)))))

(deftest test-lists-negzipwith-negfirst-list-shorter

  (is (= (list 5 7)

         (((hydra_lib_lists_zip_with hydra_lib_math_add) (list 1 2)) (list 4 5 6)))))

(deftest test-lists-negzipwith-negsecond-list-shorter

  (is (= (list 5 7)

         (((hydra_lib_lists_zip_with hydra_lib_math_add) (list 1 2 3)) (list 4 5)))))

(deftest test-lists-negzipwith-negempty-first-list

  (is (= (list )

         (((hydra_lib_lists_zip_with hydra_lib_math_add) (list )) (list 1 2 3)))))

(deftest test-lists-negzipwith-negempty-second-list

  (is (= (list )

         (((hydra_lib_lists_zip_with hydra_lib_math_add) (list 1 2 3)) (list )))))

(deftest test-lists-negzipwith-negstring-concatenation

  (is (= (list "a1" "b2")

         (((hydra_lib_lists_zip_with hydra_lib_strings_cat2) (list "a" "b")) (list "1" "2")))))
