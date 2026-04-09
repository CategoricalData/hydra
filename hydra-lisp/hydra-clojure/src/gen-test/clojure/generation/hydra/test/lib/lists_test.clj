;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.lists primitives

(ns test-ns
  (:require [clojure.test :refer :all]))

;; apply

;; string transformations

(deftest test-lists-negapply-negstring-transformations-negstring-transformations

  (is (= ["ONE", "TWO", "THREE", "one", "two", "three"]

         ["ONE", "TWO", "THREE", "one", "two", "three"])))

;; edge cases

(deftest test-lists-negapply-negedge-cases-negempty-function-list

  (is (= []

         [])))

(deftest test-lists-negapply-negedge-cases-negempty-input-list

  (is (= []

         [])))

(deftest test-lists-negapply-negedge-cases-negsingle-function

  (is (= ["HELLO"]

         ["HELLO"])))

(deftest test-lists-negapply-negedge-cases-negsingle-input

  (is (= ["TEST", "test"]

         ["TEST", "test"])))

;; at

(deftest test-lists-negat-negfirst-element

  (is (= 1:int32

         1:int32)))

(deftest test-lists-negat-negmiddle-element

  (is (= 2:int32

         2:int32)))

(deftest test-lists-negat-neglast-element

  (is (= 3:int32

         3:int32)))

(deftest test-lists-negat-negsingle-element-list

  (is (= 42:int32

         42:int32)))

(deftest test-lists-negat-negstring-list-access

  (is (= "world"

         "world")))

;; bind

(deftest test-lists-negbind-negnegation-function

  (is (= [-1:int32, -2:int32, -3:int32, -4:int32]

         [-1:int32, -2:int32, -3:int32, -4:int32])))

(deftest test-lists-negbind-negempty-list

  (is (= []

         [])))

(deftest test-lists-negbind-negsingle-element

  (is (= [-5:int32]

         [-5:int32])))

(deftest test-lists-negbind-negduplicate-elements

  (is (= [-1:int32, -1:int32, -2:int32]

         [-1:int32, -1:int32, -2:int32])))

;; concat

(deftest test-lists-negconcat-negmultiple-non-negempty-lists

  (is (= [1:int32, 2:int32, 3:int32, 4:int32, 5:int32, 6:int32, 7:int32, 8:int32]

         [1:int32, 2:int32, 3:int32, 4:int32, 5:int32, 6:int32, 7:int32, 8:int32])))

(deftest test-lists-negconcat-negempty-lists-included

  (is (= [1:int32, 2:int32, 3:int32]

         [1:int32, 2:int32, 3:int32])))

(deftest test-lists-negconcat-negsingle-list

  (is (= [1:int32, 2:int32, 3:int32]

         [1:int32, 2:int32, 3:int32])))

(deftest test-lists-negconcat-negall-empty-lists

  (is (= []

         [])))

(deftest test-lists-negconcat-negempty-list-of-lists

  (is (= []

         [])))

;; concat2

(deftest test-lists-negconcat2-negtwo-non-negempty-lists

  (is (= [1:int32, 2:int32, 3:int32, 4:int32]

         [1:int32, 2:int32, 3:int32, 4:int32])))

(deftest test-lists-negconcat2-negfirst-list-empty

  (is (= [1:int32, 2:int32]

         [1:int32, 2:int32])))

(deftest test-lists-negconcat2-negsecond-list-empty

  (is (= [1:int32, 2:int32]

         [1:int32, 2:int32])))

(deftest test-lists-negconcat2-negboth-lists-empty

  (is (= []

         [])))

(deftest test-lists-negconcat2-negsingle-elements

  (is (= [1:int32, 2:int32]

         [1:int32, 2:int32])))

(deftest test-lists-negconcat2-negstring-lists

  (is (= ["a", "b", "c", "d"]

         ["a", "b", "c", "d"])))

;; cons

(deftest test-lists-negcons-negcons-to-non-negempty-list

  (is (= [1:int32, 2:int32, 3:int32]

         [1:int32, 2:int32, 3:int32])))

(deftest test-lists-negcons-negcons-to-empty-list

  (is (= [1:int32]

         [1:int32])))

(deftest test-lists-negcons-negcons-negative-number

  (is (= [-1:int32, 2:int32, 3:int32]

         [-1:int32, 2:int32, 3:int32])))

(deftest test-lists-negcons-negcons-string

  (is (= ["hello", "world"]

         ["hello", "world"])))

;; drop

(deftest test-lists-negdrop-negdrop-from-beginning

  (is (= [3:int32, 4:int32, 5:int32]

         [3:int32, 4:int32, 5:int32])))

(deftest test-lists-negdrop-negdrop-zero-elements

  (is (= [1:int32, 2:int32, 3:int32]

         [1:int32, 2:int32, 3:int32])))

(deftest test-lists-negdrop-negdrop-all-elements

  (is (= []

         [])))

(deftest test-lists-negdrop-negdrop-more-than-length

  (is (= []

         [])))

(deftest test-lists-negdrop-negdrop-from-empty-list

  (is (= []

         [])))

(deftest test-lists-negdrop-negdrop-negative-amount

  (is (= [1:int32, 2:int32, 3:int32]

         [1:int32, 2:int32, 3:int32])))

;; dropWhile

(deftest test-lists-negdropwhile-negdrop-while-less-than-3

  (is (= [3:int32, 2:int32, 1:int32]

         [3:int32, 2:int32, 1:int32])))

(deftest test-lists-negdropwhile-negdrop-all-elements

  (is (= []

         [])))

(deftest test-lists-negdropwhile-negdrop-no-elements

  (is (= [1:int32, 2:int32, 3:int32]

         [1:int32, 2:int32, 3:int32])))

(deftest test-lists-negdropwhile-negempty-list

  (is (= []

         [])))

;; elem

(deftest test-lists-negelem-negelement-present

  (is (= true

         true)))

(deftest test-lists-negelem-negelement-not-present

  (is (= false

         false)))

(deftest test-lists-negelem-negempty-list

  (is (= false

         false)))

(deftest test-lists-negelem-negsingle-element-present

  (is (= true

         true)))

(deftest test-lists-negelem-negsingle-element-not-present

  (is (= false

         false)))

(deftest test-lists-negelem-negduplicate-elements

  (is (= true

         true)))

(deftest test-lists-negelem-negstring-element-present

  (is (= true

         true)))

(deftest test-lists-negelem-negstring-element-not-present

  (is (= false

         false)))

;; filter

(deftest test-lists-negfilter-negfilter-positive-numbers

  (is (= [2:int32, 4:int32, 5:int32]

         [2:int32, 4:int32, 5:int32])))

(deftest test-lists-negfilter-negfilter-all-elements

  (is (= [1:int32, 2:int32, 3:int32]

         [1:int32, 2:int32, 3:int32])))

(deftest test-lists-negfilter-negfilter-no-elements

  (is (= []

         [])))

(deftest test-lists-negfilter-negempty-list

  (is (= []

         [])))

;; find

(deftest test-lists-negfind-negfind-existing-element

  (is (= just(4:int32)

         just(4:int32))))

(deftest test-lists-negfind-negfind-first-matching

  (is (= just(1:int32)

         just(1:int32))))

(deftest test-lists-negfind-negfind-no-match

  (is (= nothing

         nothing)))

(deftest test-lists-negfind-negfind-in-empty-list

  (is (= nothing

         nothing)))

(deftest test-lists-negfind-negfind-single-element

  (is (= just(42:int32)

         just(42:int32))))

;; foldl

(deftest test-lists-negfoldl-negsum-with-addition

  (is (= 10:int32

         10:int32)))

(deftest test-lists-negfoldl-negproduct-with-multiplication

  (is (= 24:int32

         24:int32)))

(deftest test-lists-negfoldl-negempty-list

  (is (= 5:int32

         5:int32)))

(deftest test-lists-negfoldl-negsingle-element

  (is (= 15:int32

         15:int32)))

(deftest test-lists-negfoldl-negsubtraction-fold

  (is (= 4:int32

         4:int32)))

;; foldr

(deftest test-lists-negfoldr-negsubtraction-fold-right

  (is (= 2:int32

         2:int32)))

(deftest test-lists-negfoldr-negempty-list

  (is (= 5:int32

         5:int32)))

(deftest test-lists-negfoldr-negsingle-element

  (is (= 15:int32

         15:int32)))

(deftest test-lists-negfoldr-negsum-with-addition

  (is (= 10:int32

         10:int32)))

(deftest test-lists-negfoldr-negsubtraction-vs-foldl

  (is (= -8:int32

         -8:int32)))

;; group

(deftest test-lists-neggroup-negconsecutive-duplicates

  (is (= [[1:int32, 1:int32], [2:int32, 2:int32, 2:int32], [3:int32], [1:int32]]

         [[1:int32, 1:int32], [2:int32, 2:int32, 2:int32], [3:int32], [1:int32]])))

(deftest test-lists-neggroup-negno-duplicates

  (is (= [[1:int32], [2:int32], [3:int32]]

         [[1:int32], [2:int32], [3:int32]])))

(deftest test-lists-neggroup-negall-same

  (is (= [[1:int32, 1:int32, 1:int32]]

         [[1:int32, 1:int32, 1:int32]])))

(deftest test-lists-neggroup-negempty-list

  (is (= []

         [])))

(deftest test-lists-neggroup-negsingle-element

  (is (= [[1:int32]]

         [[1:int32]])))

;; head

(deftest test-lists-neghead-negthree-element-list

  (is (= 1:int32

         1:int32)))

(deftest test-lists-neghead-negsingle-element-list

  (is (= 42:int32

         42:int32)))

(deftest test-lists-neghead-negnegative-numbers

  (is (= -1:int32

         -1:int32)))

(deftest test-lists-neghead-negstring-list

  (is (= "hello"

         "hello")))

;; init

(deftest test-lists-neginit-negmultiple-elements

  (is (= [1:int32, 2:int32, 3:int32]

         [1:int32, 2:int32, 3:int32])))

(deftest test-lists-neginit-negtwo-elements

  (is (= [1:int32]

         [1:int32])))

(deftest test-lists-neginit-negsingle-element

  (is (= []

         [])))

(deftest test-lists-neginit-negstring-list

  (is (= ["a", "b"]

         ["a", "b"])))

;; intercalate

(deftest test-lists-negintercalate-negdouble-zero-separator

  (is (= [1:int32, 2:int32, 3:int32, 0:int32, 0:int32, 4:int32, 5:int32, 0:int32, 0:int32, 6:int32, 7:int32, 8:int32]

         [1:int32, 2:int32, 3:int32, 0:int32, 0:int32, 4:int32, 5:int32, 0:int32, 0:int32, 6:int32, 7:int32, 8:int32])))

(deftest test-lists-negintercalate-negempty-separator

  (is (= [1:int32, 2:int32, 3:int32, 4:int32]

         [1:int32, 2:int32, 3:int32, 4:int32])))

(deftest test-lists-negintercalate-negsingle-element-separator

  (is (= [1:int32, 99:int32, 2:int32, 99:int32, 3:int32]

         [1:int32, 99:int32, 2:int32, 99:int32, 3:int32])))

(deftest test-lists-negintercalate-negempty-list-of-lists

  (is (= []

         [])))

(deftest test-lists-negintercalate-negsingle-list

  (is (= [1:int32, 2:int32, 3:int32]

         [1:int32, 2:int32, 3:int32])))

(deftest test-lists-negintercalate-neglists-with-empty-lists

  (is (= [0:int32, 1:int32, 0:int32]

         [0:int32, 1:int32, 0:int32])))

;; intersperse

(deftest test-lists-negintersperse-negstring-interspersion

  (is (= ["one", "and", "two", "and", "three"]

         ["one", "and", "two", "and", "three"])))

(deftest test-lists-negintersperse-negsingle-element

  (is (= ["only"]

         ["only"])))

(deftest test-lists-negintersperse-negempty-list

  (is (= []

         [])))

(deftest test-lists-negintersperse-negtwo-elements

  (is (= ["a", "+", "b"]

         ["a", "+", "b"])))

(deftest test-lists-negintersperse-negnumber-interspersion

  (is (= [1:int32, 0:int32, 2:int32, 0:int32, 3:int32]

         [1:int32, 0:int32, 2:int32, 0:int32, 3:int32])))

;; last

(deftest test-lists-neglast-negthree-element-list

  (is (= 3:int32

         3:int32)))

(deftest test-lists-neglast-negsingle-element-list

  (is (= 42:int32

         42:int32)))

(deftest test-lists-neglast-negnegative-numbers

  (is (= -3:int32

         -3:int32)))

(deftest test-lists-neglast-negstring-list

  (is (= "world"

         "world")))

;; length

(deftest test-lists-neglength-negthree-elements

  (is (= 3:int32

         3:int32)))

(deftest test-lists-neglength-negempty-list

  (is (= 0:int32

         0:int32)))

(deftest test-lists-neglength-negsingle-element

  (is (= 1:int32

         1:int32)))

(deftest test-lists-neglength-negmany-elements

  (is (= 10:int32

         10:int32)))

(deftest test-lists-neglength-negstring-list

  (is (= 3:int32

         3:int32)))

;; map

(deftest test-lists-negmap-negstring-to-uppercase

  (is (= ["ONE", "TWO"]

         ["ONE", "TWO"])))

(deftest test-lists-negmap-negempty-list

  (is (= []

         [])))

(deftest test-lists-negmap-negsingle-element

  (is (= ["HELLO"]

         ["HELLO"])))

(deftest test-lists-negmap-negnumber-negation

  (is (= [-1:int32, -2:int32, -3:int32]

         [-1:int32, -2:int32, -3:int32])))

(deftest test-lists-negmap-negidentity-function

  (is (= [1:int32, 2:int32, 3:int32]

         [1:int32, 2:int32, 3:int32])))

;; maybeAt

(deftest test-lists-negmaybeat-negvalid-index

  (is (= just(20:int32)

         just(20:int32))))

(deftest test-lists-negmaybeat-negfirst-element

  (is (= just(10:int32)

         just(10:int32))))

(deftest test-lists-negmaybeat-neglast-element

  (is (= just(30:int32)

         just(30:int32))))

(deftest test-lists-negmaybeat-negout-of-bounds

  (is (= nothing

         nothing)))

(deftest test-lists-negmaybeat-negnegative-index

  (is (= nothing

         nothing)))

(deftest test-lists-negmaybeat-negempty-list

  (is (= nothing

         nothing)))

;; maybeHead

(deftest test-lists-negmaybehead-negnon-negempty-int-list

  (is (= just(1:int32)

         just(1:int32))))

(deftest test-lists-negmaybehead-negempty-int-list

  (is (= nothing

         nothing)))

(deftest test-lists-negmaybehead-negsingle-element

  (is (= just(42:int32)

         just(42:int32))))

(deftest test-lists-negmaybehead-negnon-negempty-string-list

  (is (= just("hello")

         just("hello"))))

(deftest test-lists-negmaybehead-negempty-string-list

  (is (= nothing

         nothing)))

;; maybeInit

(deftest test-lists-negmaybeinit-negthree-elements

  (is (= just([1:int32, 2:int32])

         just([1:int32, 2:int32]))))

(deftest test-lists-negmaybeinit-negsingle-element

  (is (= just([])

         just([]))))

(deftest test-lists-negmaybeinit-negempty-list

  (is (= nothing

         nothing)))

;; maybeLast

(deftest test-lists-negmaybelast-negthree-elements

  (is (= just(3:int32)

         just(3:int32))))

(deftest test-lists-negmaybelast-negsingle-element

  (is (= just(42:int32)

         just(42:int32))))

(deftest test-lists-negmaybelast-negempty-list

  (is (= nothing

         nothing)))

;; maybeTail

(deftest test-lists-negmaybetail-negthree-elements

  (is (= just([2:int32, 3:int32])

         just([2:int32, 3:int32]))))

(deftest test-lists-negmaybetail-negsingle-element

  (is (= just([])

         just([]))))

(deftest test-lists-negmaybetail-negempty-list

  (is (= nothing

         nothing)))

;; nub

(deftest test-lists-negnub-negremove-duplicates

  (is (= [1:int32, 2:int32, 3:int32, 4:int32]

         [1:int32, 2:int32, 3:int32, 4:int32])))

(deftest test-lists-negnub-negno-duplicates

  (is (= [1:int32, 2:int32, 3:int32]

         [1:int32, 2:int32, 3:int32])))

(deftest test-lists-negnub-negall-duplicates

  (is (= [1:int32]

         [1:int32])))

(deftest test-lists-negnub-negempty-list

  (is (= []

         [])))

(deftest test-lists-negnub-negsingle-element

  (is (= [1:int32]

         [1:int32])))

(deftest test-lists-negnub-negstring-duplicates

  (is (= ["a", "b", "c"]

         ["a", "b", "c"])))

;; null

(deftest test-lists-negnull-negempty-int-list

  (is (= true

         true)))

(deftest test-lists-negnull-negsingle-element

  (is (= false

         false)))

(deftest test-lists-negnull-negmultiple-elements

  (is (= false

         false)))

(deftest test-lists-negnull-negempty-string-list

  (is (= true

         true)))

(deftest test-lists-negnull-negnon-negempty-string-list

  (is (= false

         false)))

;; partition

(deftest test-lists-negpartition-negpartition-greater-than-3

  (is (= ([4:int32, 5:int32, 6:int32], [1:int32, 2:int32, 3:int32])

         ([4:int32, 5:int32, 6:int32], [1:int32, 2:int32, 3:int32]))))

(deftest test-lists-negpartition-negpartition-all-elements

  (is (= ([1:int32, 2:int32, 3:int32], [])

         ([1:int32, 2:int32, 3:int32], []))))

(deftest test-lists-negpartition-negpartition-no-elements

  (is (= ([], [1:int32, 2:int32, 3:int32])

         ([], [1:int32, 2:int32, 3:int32]))))

(deftest test-lists-negpartition-negpartition-even-numbers

  (is (= ([2:int32, 4:int32, 6:int32], [1:int32, 3:int32, 5:int32])

         ([2:int32, 4:int32, 6:int32], [1:int32, 3:int32, 5:int32]))))

(deftest test-lists-negpartition-negempty-list

  (is (= ([], [])

         ([], []))))

;; pure

(deftest test-lists-negpure-negstring-element

  (is (= ["one"]

         ["one"])))

(deftest test-lists-negpure-negempty-string

  (is (= [""]

         [""])))

(deftest test-lists-negpure-negnumber-element

  (is (= [42:int32]

         [42:int32])))

(deftest test-lists-negpure-negnegative-number

  (is (= [-5:int32]

         [-5:int32])))

;; replicate

(deftest test-lists-negreplicate-negreplicate-three-times

  (is (= [42:int32, 42:int32, 42:int32]

         [42:int32, 42:int32, 42:int32])))

(deftest test-lists-negreplicate-negreplicate-zero-times

  (is (= []

         [])))

(deftest test-lists-negreplicate-negreplicate-once

  (is (= [99:int32]

         [99:int32])))

(deftest test-lists-negreplicate-negreplicate-string

  (is (= ["hello", "hello"]

         ["hello", "hello"])))

;; reverse

(deftest test-lists-negreverse-negmultiple-elements

  (is (= [4:int32, 3:int32, 2:int32, 1:int32]

         [4:int32, 3:int32, 2:int32, 1:int32])))

(deftest test-lists-negreverse-negsingle-element

  (is (= [1:int32]

         [1:int32])))

(deftest test-lists-negreverse-negempty-list

  (is (= []

         [])))

(deftest test-lists-negreverse-negtwo-elements

  (is (= [2:int32, 1:int32]

         [2:int32, 1:int32])))

(deftest test-lists-negreverse-negstring-list

  (is (= ["c", "b", "a"]

         ["c", "b", "a"])))

;; safeHead

(deftest test-lists-negsafehead-negnon-negempty-int-list

  (is (= just(1:int32)

         just(1:int32))))

(deftest test-lists-negsafehead-negempty-int-list

  (is (= nothing

         nothing)))

(deftest test-lists-negsafehead-negsingle-element

  (is (= just(42:int32)

         just(42:int32))))

(deftest test-lists-negsafehead-negnon-negempty-string-list

  (is (= just("hello")

         just("hello"))))

(deftest test-lists-negsafehead-negempty-string-list

  (is (= nothing

         nothing)))

;; singleton

(deftest test-lists-negsingleton-negnumber-element

  (is (= [42:int32]

         [42:int32])))

(deftest test-lists-negsingleton-negnegative-number

  (is (= [-1:int32]

         [-1:int32])))

(deftest test-lists-negsingleton-negzero

  (is (= [0:int32]

         [0:int32])))

(deftest test-lists-negsingleton-negstring-element

  (is (= ["hello"]

         ["hello"])))

;; sort

(deftest test-lists-negsort-negunsorted-numbers

  (is (= [1:int32, 1:int32, 3:int32, 4:int32, 5:int32]

         [1:int32, 1:int32, 3:int32, 4:int32, 5:int32])))

(deftest test-lists-negsort-negalready-sorted

  (is (= [1:int32, 2:int32, 3:int32]

         [1:int32, 2:int32, 3:int32])))

(deftest test-lists-negsort-negreverse-sorted

  (is (= [1:int32, 2:int32, 3:int32]

         [1:int32, 2:int32, 3:int32])))

(deftest test-lists-negsort-negsingle-element

  (is (= [1:int32]

         [1:int32])))

(deftest test-lists-negsort-negempty-list

  (is (= []

         [])))

(deftest test-lists-negsort-negduplicates

  (is (= [1:int32, 1:int32, 2:int32, 2:int32, 3:int32]

         [1:int32, 1:int32, 2:int32, 2:int32, 3:int32])))

(deftest test-lists-negsort-negstring-sort

  (is (= ["apple", "banana", "zebra"]

         ["apple", "banana", "zebra"])))

;; sortOn

(deftest test-lists-negsorton-negsort-by-string-length

  (is (= ["hi", "hello", "world"]

         ["hi", "hello", "world"])))

(deftest test-lists-negsorton-negempty-string-list

  (is (= []

         [])))

(deftest test-lists-negsorton-negsingle-string-element

  (is (= ["test"]

         ["test"])))

(deftest test-lists-negsorton-negsort-by-negation

  (is (= [3:int32, 2:int32, 1:int32]

         [3:int32, 2:int32, 1:int32])))

(deftest test-lists-negsorton-negsort-by-absolute-value

  (is (= [-1:int32, 2:int32, -3:int32]

         [-1:int32, 2:int32, -3:int32])))

;; span

(deftest test-lists-negspan-negspan-less-than-3

  (is (= ([1:int32, 2:int32], [3:int32, 1:int32, 2:int32])

         ([1:int32, 2:int32], [3:int32, 1:int32, 2:int32]))))

(deftest test-lists-negspan-negspan-all-elements

  (is (= ([1:int32, 2:int32, 3:int32], [])

         ([1:int32, 2:int32, 3:int32], []))))

(deftest test-lists-negspan-negspan-no-elements

  (is (= ([], [1:int32, 2:int32, 3:int32])

         ([], [1:int32, 2:int32, 3:int32]))))

(deftest test-lists-negspan-negempty-list

  (is (= ([], [])

         ([], []))))

;; tail

(deftest test-lists-negtail-negmultiple-elements

  (is (= [2:int32, 3:int32, 4:int32]

         [2:int32, 3:int32, 4:int32])))

(deftest test-lists-negtail-negtwo-elements

  (is (= [2:int32]

         [2:int32])))

(deftest test-lists-negtail-negsingle-element

  (is (= []

         [])))

(deftest test-lists-negtail-negstring-list

  (is (= ["b", "c"]

         ["b", "c"])))

;; take

(deftest test-lists-negtake-negtake-from-beginning

  (is (= [1:int32, 2:int32]

         [1:int32, 2:int32])))

(deftest test-lists-negtake-negtake-zero-elements

  (is (= []

         [])))

(deftest test-lists-negtake-negtake-all-elements

  (is (= [1:int32, 2:int32, 3:int32]

         [1:int32, 2:int32, 3:int32])))

(deftest test-lists-negtake-negtake-more-than-length

  (is (= [1:int32, 2:int32]

         [1:int32, 2:int32])))

(deftest test-lists-negtake-negtake-from-empty-list

  (is (= []

         [])))

(deftest test-lists-negtake-negtake-negative-amount

  (is (= []

         [])))

;; transpose

(deftest test-lists-negtranspose-negsquare-matrix

  (is (= [[1:int32, 4:int32], [2:int32, 5:int32], [3:int32, 6:int32]]

         [[1:int32, 4:int32], [2:int32, 5:int32], [3:int32, 6:int32]])))

(deftest test-lists-negtranspose-negempty-lists

  (is (= []

         [])))

(deftest test-lists-negtranspose-negsingle-row

  (is (= [[1:int32], [2:int32], [3:int32]]

         [[1:int32], [2:int32], [3:int32]])))

(deftest test-lists-negtranspose-negsingle-column

  (is (= [[1:int32, 2:int32, 3:int32]]

         [[1:int32, 2:int32, 3:int32]])))

(deftest test-lists-negtranspose-negragged-matrix

  (is (= [[1:int32, 3:int32, 4:int32], [2:int32, 5:int32], [6:int32]]

         [[1:int32, 3:int32, 4:int32], [2:int32, 5:int32], [6:int32]])))

;; zip

(deftest test-lists-negzip-negequal-length-lists

  (is (= [(1:int32, "a"), (2:int32, "b"), (3:int32, "c")]

         [(1:int32, "a"), (2:int32, "b"), (3:int32, "c")])))

(deftest test-lists-negzip-negfirst-list-shorter

  (is (= [(1:int32, "a"), (2:int32, "b")]

         [(1:int32, "a"), (2:int32, "b")])))

(deftest test-lists-negzip-negsecond-list-shorter

  (is (= [(1:int32, "a"), (2:int32, "b")]

         [(1:int32, "a"), (2:int32, "b")])))

(deftest test-lists-negzip-negempty-first-list

  (is (= []

         [])))

(deftest test-lists-negzip-negempty-second-list

  (is (= []

         [])))

(deftest test-lists-negzip-negboth-empty-lists

  (is (= []

         [])))

;; zipWith

(deftest test-lists-negzipwith-negaddition

  (is (= [5:int32, 7:int32, 9:int32]

         [5:int32, 7:int32, 9:int32])))

(deftest test-lists-negzipwith-negfirst-list-shorter

  (is (= [5:int32, 7:int32]

         [5:int32, 7:int32])))

(deftest test-lists-negzipwith-negsecond-list-shorter

  (is (= [5:int32, 7:int32]

         [5:int32, 7:int32])))

(deftest test-lists-negzipwith-negempty-first-list

  (is (= []

         [])))

(deftest test-lists-negzipwith-negempty-second-list

  (is (= []

         [])))

(deftest test-lists-negzipwith-negstring-concatenation

  (is (= ["a1", "b2"]

         ["a1", "b2"])))
