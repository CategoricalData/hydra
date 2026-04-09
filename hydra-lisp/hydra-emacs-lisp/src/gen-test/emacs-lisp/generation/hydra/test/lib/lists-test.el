;;; Note: this is an automatically generated file. Do not edit. -*- lexical-binding: t; coding: utf-8 -*-
;;; hydra.lib.lists primitives

(require 'ert)

;; apply

;; string transformations

(ert-deftest test-lists-negapply-negstring-transformations-negstring-transformations ()

  (should (equal ["ONE", "TWO", "THREE", "one", "two", "three"] ["ONE", "TWO", "THREE", "one", "two", "three"])))

;; edge cases

(ert-deftest test-lists-negapply-negedge-cases-negempty-function-list ()

  (should (equal [] [])))

(ert-deftest test-lists-negapply-negedge-cases-negempty-input-list ()

  (should (equal [] [])))

(ert-deftest test-lists-negapply-negedge-cases-negsingle-function ()

  (should (equal ["HELLO"] ["HELLO"])))

(ert-deftest test-lists-negapply-negedge-cases-negsingle-input ()

  (should (equal ["TEST", "test"] ["TEST", "test"])))

;; at

(ert-deftest test-lists-negat-negfirst-element ()

  (should (equal 1:int32 1:int32)))

(ert-deftest test-lists-negat-negmiddle-element ()

  (should (equal 2:int32 2:int32)))

(ert-deftest test-lists-negat-neglast-element ()

  (should (equal 3:int32 3:int32)))

(ert-deftest test-lists-negat-negsingle-element-list ()

  (should (equal 42:int32 42:int32)))

(ert-deftest test-lists-negat-negstring-list-access ()

  (should (equal "world" "world")))

;; bind

(ert-deftest test-lists-negbind-negnegation-function ()

  (should (equal [-1:int32, -2:int32, -3:int32, -4:int32] [-1:int32, -2:int32, -3:int32, -4:int32])))

(ert-deftest test-lists-negbind-negempty-list ()

  (should (equal [] [])))

(ert-deftest test-lists-negbind-negsingle-element ()

  (should (equal [-5:int32] [-5:int32])))

(ert-deftest test-lists-negbind-negduplicate-elements ()

  (should (equal [-1:int32, -1:int32, -2:int32] [-1:int32, -1:int32, -2:int32])))

;; concat

(ert-deftest test-lists-negconcat-negmultiple-non-negempty-lists ()

  (should (equal [1:int32, 2:int32, 3:int32, 4:int32, 5:int32, 6:int32, 7:int32, 8:int32] [1:int32, 2:int32, 3:int32, 4:int32, 5:int32, 6:int32, 7:int32, 8:int32])))

(ert-deftest test-lists-negconcat-negempty-lists-included ()

  (should (equal [1:int32, 2:int32, 3:int32] [1:int32, 2:int32, 3:int32])))

(ert-deftest test-lists-negconcat-negsingle-list ()

  (should (equal [1:int32, 2:int32, 3:int32] [1:int32, 2:int32, 3:int32])))

(ert-deftest test-lists-negconcat-negall-empty-lists ()

  (should (equal [] [])))

(ert-deftest test-lists-negconcat-negempty-list-of-lists ()

  (should (equal [] [])))

;; concat2

(ert-deftest test-lists-negconcat2-negtwo-non-negempty-lists ()

  (should (equal [1:int32, 2:int32, 3:int32, 4:int32] [1:int32, 2:int32, 3:int32, 4:int32])))

(ert-deftest test-lists-negconcat2-negfirst-list-empty ()

  (should (equal [1:int32, 2:int32] [1:int32, 2:int32])))

(ert-deftest test-lists-negconcat2-negsecond-list-empty ()

  (should (equal [1:int32, 2:int32] [1:int32, 2:int32])))

(ert-deftest test-lists-negconcat2-negboth-lists-empty ()

  (should (equal [] [])))

(ert-deftest test-lists-negconcat2-negsingle-elements ()

  (should (equal [1:int32, 2:int32] [1:int32, 2:int32])))

(ert-deftest test-lists-negconcat2-negstring-lists ()

  (should (equal ["a", "b", "c", "d"] ["a", "b", "c", "d"])))

;; cons

(ert-deftest test-lists-negcons-negcons-to-non-negempty-list ()

  (should (equal [1:int32, 2:int32, 3:int32] [1:int32, 2:int32, 3:int32])))

(ert-deftest test-lists-negcons-negcons-to-empty-list ()

  (should (equal [1:int32] [1:int32])))

(ert-deftest test-lists-negcons-negcons-negative-number ()

  (should (equal [-1:int32, 2:int32, 3:int32] [-1:int32, 2:int32, 3:int32])))

(ert-deftest test-lists-negcons-negcons-string ()

  (should (equal ["hello", "world"] ["hello", "world"])))

;; drop

(ert-deftest test-lists-negdrop-negdrop-from-beginning ()

  (should (equal [3:int32, 4:int32, 5:int32] [3:int32, 4:int32, 5:int32])))

(ert-deftest test-lists-negdrop-negdrop-zero-elements ()

  (should (equal [1:int32, 2:int32, 3:int32] [1:int32, 2:int32, 3:int32])))

(ert-deftest test-lists-negdrop-negdrop-all-elements ()

  (should (equal [] [])))

(ert-deftest test-lists-negdrop-negdrop-more-than-length ()

  (should (equal [] [])))

(ert-deftest test-lists-negdrop-negdrop-from-empty-list ()

  (should (equal [] [])))

(ert-deftest test-lists-negdrop-negdrop-negative-amount ()

  (should (equal [1:int32, 2:int32, 3:int32] [1:int32, 2:int32, 3:int32])))

;; dropWhile

(ert-deftest test-lists-negdropwhile-negdrop-while-less-than-3 ()

  (should (equal [3:int32, 2:int32, 1:int32] [3:int32, 2:int32, 1:int32])))

(ert-deftest test-lists-negdropwhile-negdrop-all-elements ()

  (should (equal [] [])))

(ert-deftest test-lists-negdropwhile-negdrop-no-elements ()

  (should (equal [1:int32, 2:int32, 3:int32] [1:int32, 2:int32, 3:int32])))

(ert-deftest test-lists-negdropwhile-negempty-list ()

  (should (equal [] [])))

;; elem

(ert-deftest test-lists-negelem-negelement-present ()

  (should (equal true true)))

(ert-deftest test-lists-negelem-negelement-not-present ()

  (should (equal false false)))

(ert-deftest test-lists-negelem-negempty-list ()

  (should (equal false false)))

(ert-deftest test-lists-negelem-negsingle-element-present ()

  (should (equal true true)))

(ert-deftest test-lists-negelem-negsingle-element-not-present ()

  (should (equal false false)))

(ert-deftest test-lists-negelem-negduplicate-elements ()

  (should (equal true true)))

(ert-deftest test-lists-negelem-negstring-element-present ()

  (should (equal true true)))

(ert-deftest test-lists-negelem-negstring-element-not-present ()

  (should (equal false false)))

;; filter

(ert-deftest test-lists-negfilter-negfilter-positive-numbers ()

  (should (equal [2:int32, 4:int32, 5:int32] [2:int32, 4:int32, 5:int32])))

(ert-deftest test-lists-negfilter-negfilter-all-elements ()

  (should (equal [1:int32, 2:int32, 3:int32] [1:int32, 2:int32, 3:int32])))

(ert-deftest test-lists-negfilter-negfilter-no-elements ()

  (should (equal [] [])))

(ert-deftest test-lists-negfilter-negempty-list ()

  (should (equal [] [])))

;; find

(ert-deftest test-lists-negfind-negfind-existing-element ()

  (should (equal just(4:int32) just(4:int32))))

(ert-deftest test-lists-negfind-negfind-first-matching ()

  (should (equal just(1:int32) just(1:int32))))

(ert-deftest test-lists-negfind-negfind-no-match ()

  (should (equal nothing nothing)))

(ert-deftest test-lists-negfind-negfind-in-empty-list ()

  (should (equal nothing nothing)))

(ert-deftest test-lists-negfind-negfind-single-element ()

  (should (equal just(42:int32) just(42:int32))))

;; foldl

(ert-deftest test-lists-negfoldl-negsum-with-addition ()

  (should (equal 10:int32 10:int32)))

(ert-deftest test-lists-negfoldl-negproduct-with-multiplication ()

  (should (equal 24:int32 24:int32)))

(ert-deftest test-lists-negfoldl-negempty-list ()

  (should (equal 5:int32 5:int32)))

(ert-deftest test-lists-negfoldl-negsingle-element ()

  (should (equal 15:int32 15:int32)))

(ert-deftest test-lists-negfoldl-negsubtraction-fold ()

  (should (equal 4:int32 4:int32)))

;; foldr

(ert-deftest test-lists-negfoldr-negsubtraction-fold-right ()

  (should (equal 2:int32 2:int32)))

(ert-deftest test-lists-negfoldr-negempty-list ()

  (should (equal 5:int32 5:int32)))

(ert-deftest test-lists-negfoldr-negsingle-element ()

  (should (equal 15:int32 15:int32)))

(ert-deftest test-lists-negfoldr-negsum-with-addition ()

  (should (equal 10:int32 10:int32)))

(ert-deftest test-lists-negfoldr-negsubtraction-vs-foldl ()

  (should (equal -8:int32 -8:int32)))

;; group

(ert-deftest test-lists-neggroup-negconsecutive-duplicates ()

  (should (equal [[1:int32, 1:int32], [2:int32, 2:int32, 2:int32], [3:int32], [1:int32]] [[1:int32, 1:int32], [2:int32, 2:int32, 2:int32], [3:int32], [1:int32]])))

(ert-deftest test-lists-neggroup-negno-duplicates ()

  (should (equal [[1:int32], [2:int32], [3:int32]] [[1:int32], [2:int32], [3:int32]])))

(ert-deftest test-lists-neggroup-negall-same ()

  (should (equal [[1:int32, 1:int32, 1:int32]] [[1:int32, 1:int32, 1:int32]])))

(ert-deftest test-lists-neggroup-negempty-list ()

  (should (equal [] [])))

(ert-deftest test-lists-neggroup-negsingle-element ()

  (should (equal [[1:int32]] [[1:int32]])))

;; head

(ert-deftest test-lists-neghead-negthree-element-list ()

  (should (equal 1:int32 1:int32)))

(ert-deftest test-lists-neghead-negsingle-element-list ()

  (should (equal 42:int32 42:int32)))

(ert-deftest test-lists-neghead-negnegative-numbers ()

  (should (equal -1:int32 -1:int32)))

(ert-deftest test-lists-neghead-negstring-list ()

  (should (equal "hello" "hello")))

;; init

(ert-deftest test-lists-neginit-negmultiple-elements ()

  (should (equal [1:int32, 2:int32, 3:int32] [1:int32, 2:int32, 3:int32])))

(ert-deftest test-lists-neginit-negtwo-elements ()

  (should (equal [1:int32] [1:int32])))

(ert-deftest test-lists-neginit-negsingle-element ()

  (should (equal [] [])))

(ert-deftest test-lists-neginit-negstring-list ()

  (should (equal ["a", "b"] ["a", "b"])))

;; intercalate

(ert-deftest test-lists-negintercalate-negdouble-zero-separator ()

  (should (equal [1:int32, 2:int32, 3:int32, 0:int32, 0:int32, 4:int32, 5:int32, 0:int32, 0:int32, 6:int32, 7:int32, 8:int32] [1:int32, 2:int32, 3:int32, 0:int32, 0:int32, 4:int32, 5:int32, 0:int32, 0:int32, 6:int32, 7:int32, 8:int32])))

(ert-deftest test-lists-negintercalate-negempty-separator ()

  (should (equal [1:int32, 2:int32, 3:int32, 4:int32] [1:int32, 2:int32, 3:int32, 4:int32])))

(ert-deftest test-lists-negintercalate-negsingle-element-separator ()

  (should (equal [1:int32, 99:int32, 2:int32, 99:int32, 3:int32] [1:int32, 99:int32, 2:int32, 99:int32, 3:int32])))

(ert-deftest test-lists-negintercalate-negempty-list-of-lists ()

  (should (equal [] [])))

(ert-deftest test-lists-negintercalate-negsingle-list ()

  (should (equal [1:int32, 2:int32, 3:int32] [1:int32, 2:int32, 3:int32])))

(ert-deftest test-lists-negintercalate-neglists-with-empty-lists ()

  (should (equal [0:int32, 1:int32, 0:int32] [0:int32, 1:int32, 0:int32])))

;; intersperse

(ert-deftest test-lists-negintersperse-negstring-interspersion ()

  (should (equal ["one", "and", "two", "and", "three"] ["one", "and", "two", "and", "three"])))

(ert-deftest test-lists-negintersperse-negsingle-element ()

  (should (equal ["only"] ["only"])))

(ert-deftest test-lists-negintersperse-negempty-list ()

  (should (equal [] [])))

(ert-deftest test-lists-negintersperse-negtwo-elements ()

  (should (equal ["a", "+", "b"] ["a", "+", "b"])))

(ert-deftest test-lists-negintersperse-negnumber-interspersion ()

  (should (equal [1:int32, 0:int32, 2:int32, 0:int32, 3:int32] [1:int32, 0:int32, 2:int32, 0:int32, 3:int32])))

;; last

(ert-deftest test-lists-neglast-negthree-element-list ()

  (should (equal 3:int32 3:int32)))

(ert-deftest test-lists-neglast-negsingle-element-list ()

  (should (equal 42:int32 42:int32)))

(ert-deftest test-lists-neglast-negnegative-numbers ()

  (should (equal -3:int32 -3:int32)))

(ert-deftest test-lists-neglast-negstring-list ()

  (should (equal "world" "world")))

;; length

(ert-deftest test-lists-neglength-negthree-elements ()

  (should (equal 3:int32 3:int32)))

(ert-deftest test-lists-neglength-negempty-list ()

  (should (equal 0:int32 0:int32)))

(ert-deftest test-lists-neglength-negsingle-element ()

  (should (equal 1:int32 1:int32)))

(ert-deftest test-lists-neglength-negmany-elements ()

  (should (equal 10:int32 10:int32)))

(ert-deftest test-lists-neglength-negstring-list ()

  (should (equal 3:int32 3:int32)))

;; map

(ert-deftest test-lists-negmap-negstring-to-uppercase ()

  (should (equal ["ONE", "TWO"] ["ONE", "TWO"])))

(ert-deftest test-lists-negmap-negempty-list ()

  (should (equal [] [])))

(ert-deftest test-lists-negmap-negsingle-element ()

  (should (equal ["HELLO"] ["HELLO"])))

(ert-deftest test-lists-negmap-negnumber-negation ()

  (should (equal [-1:int32, -2:int32, -3:int32] [-1:int32, -2:int32, -3:int32])))

(ert-deftest test-lists-negmap-negidentity-function ()

  (should (equal [1:int32, 2:int32, 3:int32] [1:int32, 2:int32, 3:int32])))

;; maybeAt

(ert-deftest test-lists-negmaybeat-negvalid-index ()

  (should (equal just(20:int32) just(20:int32))))

(ert-deftest test-lists-negmaybeat-negfirst-element ()

  (should (equal just(10:int32) just(10:int32))))

(ert-deftest test-lists-negmaybeat-neglast-element ()

  (should (equal just(30:int32) just(30:int32))))

(ert-deftest test-lists-negmaybeat-negout-of-bounds ()

  (should (equal nothing nothing)))

(ert-deftest test-lists-negmaybeat-negnegative-index ()

  (should (equal nothing nothing)))

(ert-deftest test-lists-negmaybeat-negempty-list ()

  (should (equal nothing nothing)))

;; maybeHead

(ert-deftest test-lists-negmaybehead-negnon-negempty-int-list ()

  (should (equal just(1:int32) just(1:int32))))

(ert-deftest test-lists-negmaybehead-negempty-int-list ()

  (should (equal nothing nothing)))

(ert-deftest test-lists-negmaybehead-negsingle-element ()

  (should (equal just(42:int32) just(42:int32))))

(ert-deftest test-lists-negmaybehead-negnon-negempty-string-list ()

  (should (equal just("hello") just("hello"))))

(ert-deftest test-lists-negmaybehead-negempty-string-list ()

  (should (equal nothing nothing)))

;; maybeInit

(ert-deftest test-lists-negmaybeinit-negthree-elements ()

  (should (equal just([1:int32, 2:int32]) just([1:int32, 2:int32]))))

(ert-deftest test-lists-negmaybeinit-negsingle-element ()

  (should (equal just([]) just([]))))

(ert-deftest test-lists-negmaybeinit-negempty-list ()

  (should (equal nothing nothing)))

;; maybeLast

(ert-deftest test-lists-negmaybelast-negthree-elements ()

  (should (equal just(3:int32) just(3:int32))))

(ert-deftest test-lists-negmaybelast-negsingle-element ()

  (should (equal just(42:int32) just(42:int32))))

(ert-deftest test-lists-negmaybelast-negempty-list ()

  (should (equal nothing nothing)))

;; maybeTail

(ert-deftest test-lists-negmaybetail-negthree-elements ()

  (should (equal just([2:int32, 3:int32]) just([2:int32, 3:int32]))))

(ert-deftest test-lists-negmaybetail-negsingle-element ()

  (should (equal just([]) just([]))))

(ert-deftest test-lists-negmaybetail-negempty-list ()

  (should (equal nothing nothing)))

;; nub

(ert-deftest test-lists-negnub-negremove-duplicates ()

  (should (equal [1:int32, 2:int32, 3:int32, 4:int32] [1:int32, 2:int32, 3:int32, 4:int32])))

(ert-deftest test-lists-negnub-negno-duplicates ()

  (should (equal [1:int32, 2:int32, 3:int32] [1:int32, 2:int32, 3:int32])))

(ert-deftest test-lists-negnub-negall-duplicates ()

  (should (equal [1:int32] [1:int32])))

(ert-deftest test-lists-negnub-negempty-list ()

  (should (equal [] [])))

(ert-deftest test-lists-negnub-negsingle-element ()

  (should (equal [1:int32] [1:int32])))

(ert-deftest test-lists-negnub-negstring-duplicates ()

  (should (equal ["a", "b", "c"] ["a", "b", "c"])))

;; null

(ert-deftest test-lists-negnull-negempty-int-list ()

  (should (equal true true)))

(ert-deftest test-lists-negnull-negsingle-element ()

  (should (equal false false)))

(ert-deftest test-lists-negnull-negmultiple-elements ()

  (should (equal false false)))

(ert-deftest test-lists-negnull-negempty-string-list ()

  (should (equal true true)))

(ert-deftest test-lists-negnull-negnon-negempty-string-list ()

  (should (equal false false)))

;; partition

(ert-deftest test-lists-negpartition-negpartition-greater-than-3 ()

  (should (equal ([4:int32, 5:int32, 6:int32], [1:int32, 2:int32, 3:int32]) ([4:int32, 5:int32, 6:int32], [1:int32, 2:int32, 3:int32]))))

(ert-deftest test-lists-negpartition-negpartition-all-elements ()

  (should (equal ([1:int32, 2:int32, 3:int32], []) ([1:int32, 2:int32, 3:int32], []))))

(ert-deftest test-lists-negpartition-negpartition-no-elements ()

  (should (equal ([], [1:int32, 2:int32, 3:int32]) ([], [1:int32, 2:int32, 3:int32]))))

(ert-deftest test-lists-negpartition-negpartition-even-numbers ()

  (should (equal ([2:int32, 4:int32, 6:int32], [1:int32, 3:int32, 5:int32]) ([2:int32, 4:int32, 6:int32], [1:int32, 3:int32, 5:int32]))))

(ert-deftest test-lists-negpartition-negempty-list ()

  (should (equal ([], []) ([], []))))

;; pure

(ert-deftest test-lists-negpure-negstring-element ()

  (should (equal ["one"] ["one"])))

(ert-deftest test-lists-negpure-negempty-string ()

  (should (equal [""] [""])))

(ert-deftest test-lists-negpure-negnumber-element ()

  (should (equal [42:int32] [42:int32])))

(ert-deftest test-lists-negpure-negnegative-number ()

  (should (equal [-5:int32] [-5:int32])))

;; replicate

(ert-deftest test-lists-negreplicate-negreplicate-three-times ()

  (should (equal [42:int32, 42:int32, 42:int32] [42:int32, 42:int32, 42:int32])))

(ert-deftest test-lists-negreplicate-negreplicate-zero-times ()

  (should (equal [] [])))

(ert-deftest test-lists-negreplicate-negreplicate-once ()

  (should (equal [99:int32] [99:int32])))

(ert-deftest test-lists-negreplicate-negreplicate-string ()

  (should (equal ["hello", "hello"] ["hello", "hello"])))

;; reverse

(ert-deftest test-lists-negreverse-negmultiple-elements ()

  (should (equal [4:int32, 3:int32, 2:int32, 1:int32] [4:int32, 3:int32, 2:int32, 1:int32])))

(ert-deftest test-lists-negreverse-negsingle-element ()

  (should (equal [1:int32] [1:int32])))

(ert-deftest test-lists-negreverse-negempty-list ()

  (should (equal [] [])))

(ert-deftest test-lists-negreverse-negtwo-elements ()

  (should (equal [2:int32, 1:int32] [2:int32, 1:int32])))

(ert-deftest test-lists-negreverse-negstring-list ()

  (should (equal ["c", "b", "a"] ["c", "b", "a"])))

;; safeHead

(ert-deftest test-lists-negsafehead-negnon-negempty-int-list ()

  (should (equal just(1:int32) just(1:int32))))

(ert-deftest test-lists-negsafehead-negempty-int-list ()

  (should (equal nothing nothing)))

(ert-deftest test-lists-negsafehead-negsingle-element ()

  (should (equal just(42:int32) just(42:int32))))

(ert-deftest test-lists-negsafehead-negnon-negempty-string-list ()

  (should (equal just("hello") just("hello"))))

(ert-deftest test-lists-negsafehead-negempty-string-list ()

  (should (equal nothing nothing)))

;; singleton

(ert-deftest test-lists-negsingleton-negnumber-element ()

  (should (equal [42:int32] [42:int32])))

(ert-deftest test-lists-negsingleton-negnegative-number ()

  (should (equal [-1:int32] [-1:int32])))

(ert-deftest test-lists-negsingleton-negzero ()

  (should (equal [0:int32] [0:int32])))

(ert-deftest test-lists-negsingleton-negstring-element ()

  (should (equal ["hello"] ["hello"])))

;; sort

(ert-deftest test-lists-negsort-negunsorted-numbers ()

  (should (equal [1:int32, 1:int32, 3:int32, 4:int32, 5:int32] [1:int32, 1:int32, 3:int32, 4:int32, 5:int32])))

(ert-deftest test-lists-negsort-negalready-sorted ()

  (should (equal [1:int32, 2:int32, 3:int32] [1:int32, 2:int32, 3:int32])))

(ert-deftest test-lists-negsort-negreverse-sorted ()

  (should (equal [1:int32, 2:int32, 3:int32] [1:int32, 2:int32, 3:int32])))

(ert-deftest test-lists-negsort-negsingle-element ()

  (should (equal [1:int32] [1:int32])))

(ert-deftest test-lists-negsort-negempty-list ()

  (should (equal [] [])))

(ert-deftest test-lists-negsort-negduplicates ()

  (should (equal [1:int32, 1:int32, 2:int32, 2:int32, 3:int32] [1:int32, 1:int32, 2:int32, 2:int32, 3:int32])))

(ert-deftest test-lists-negsort-negstring-sort ()

  (should (equal ["apple", "banana", "zebra"] ["apple", "banana", "zebra"])))

;; sortOn

(ert-deftest test-lists-negsorton-negsort-by-string-length ()

  (should (equal ["hi", "hello", "world"] ["hi", "hello", "world"])))

(ert-deftest test-lists-negsorton-negempty-string-list ()

  (should (equal [] [])))

(ert-deftest test-lists-negsorton-negsingle-string-element ()

  (should (equal ["test"] ["test"])))

(ert-deftest test-lists-negsorton-negsort-by-negation ()

  (should (equal [3:int32, 2:int32, 1:int32] [3:int32, 2:int32, 1:int32])))

(ert-deftest test-lists-negsorton-negsort-by-absolute-value ()

  (should (equal [-1:int32, 2:int32, -3:int32] [-1:int32, 2:int32, -3:int32])))

;; span

(ert-deftest test-lists-negspan-negspan-less-than-3 ()

  (should (equal ([1:int32, 2:int32], [3:int32, 1:int32, 2:int32]) ([1:int32, 2:int32], [3:int32, 1:int32, 2:int32]))))

(ert-deftest test-lists-negspan-negspan-all-elements ()

  (should (equal ([1:int32, 2:int32, 3:int32], []) ([1:int32, 2:int32, 3:int32], []))))

(ert-deftest test-lists-negspan-negspan-no-elements ()

  (should (equal ([], [1:int32, 2:int32, 3:int32]) ([], [1:int32, 2:int32, 3:int32]))))

(ert-deftest test-lists-negspan-negempty-list ()

  (should (equal ([], []) ([], []))))

;; tail

(ert-deftest test-lists-negtail-negmultiple-elements ()

  (should (equal [2:int32, 3:int32, 4:int32] [2:int32, 3:int32, 4:int32])))

(ert-deftest test-lists-negtail-negtwo-elements ()

  (should (equal [2:int32] [2:int32])))

(ert-deftest test-lists-negtail-negsingle-element ()

  (should (equal [] [])))

(ert-deftest test-lists-negtail-negstring-list ()

  (should (equal ["b", "c"] ["b", "c"])))

;; take

(ert-deftest test-lists-negtake-negtake-from-beginning ()

  (should (equal [1:int32, 2:int32] [1:int32, 2:int32])))

(ert-deftest test-lists-negtake-negtake-zero-elements ()

  (should (equal [] [])))

(ert-deftest test-lists-negtake-negtake-all-elements ()

  (should (equal [1:int32, 2:int32, 3:int32] [1:int32, 2:int32, 3:int32])))

(ert-deftest test-lists-negtake-negtake-more-than-length ()

  (should (equal [1:int32, 2:int32] [1:int32, 2:int32])))

(ert-deftest test-lists-negtake-negtake-from-empty-list ()

  (should (equal [] [])))

(ert-deftest test-lists-negtake-negtake-negative-amount ()

  (should (equal [] [])))

;; transpose

(ert-deftest test-lists-negtranspose-negsquare-matrix ()

  (should (equal [[1:int32, 4:int32], [2:int32, 5:int32], [3:int32, 6:int32]] [[1:int32, 4:int32], [2:int32, 5:int32], [3:int32, 6:int32]])))

(ert-deftest test-lists-negtranspose-negempty-lists ()

  (should (equal [] [])))

(ert-deftest test-lists-negtranspose-negsingle-row ()

  (should (equal [[1:int32], [2:int32], [3:int32]] [[1:int32], [2:int32], [3:int32]])))

(ert-deftest test-lists-negtranspose-negsingle-column ()

  (should (equal [[1:int32, 2:int32, 3:int32]] [[1:int32, 2:int32, 3:int32]])))

(ert-deftest test-lists-negtranspose-negragged-matrix ()

  (should (equal [[1:int32, 3:int32, 4:int32], [2:int32, 5:int32], [6:int32]] [[1:int32, 3:int32, 4:int32], [2:int32, 5:int32], [6:int32]])))

;; zip

(ert-deftest test-lists-negzip-negequal-length-lists ()

  (should (equal [(1:int32, "a"), (2:int32, "b"), (3:int32, "c")] [(1:int32, "a"), (2:int32, "b"), (3:int32, "c")])))

(ert-deftest test-lists-negzip-negfirst-list-shorter ()

  (should (equal [(1:int32, "a"), (2:int32, "b")] [(1:int32, "a"), (2:int32, "b")])))

(ert-deftest test-lists-negzip-negsecond-list-shorter ()

  (should (equal [(1:int32, "a"), (2:int32, "b")] [(1:int32, "a"), (2:int32, "b")])))

(ert-deftest test-lists-negzip-negempty-first-list ()

  (should (equal [] [])))

(ert-deftest test-lists-negzip-negempty-second-list ()

  (should (equal [] [])))

(ert-deftest test-lists-negzip-negboth-empty-lists ()

  (should (equal [] [])))

;; zipWith

(ert-deftest test-lists-negzipwith-negaddition ()

  (should (equal [5:int32, 7:int32, 9:int32] [5:int32, 7:int32, 9:int32])))

(ert-deftest test-lists-negzipwith-negfirst-list-shorter ()

  (should (equal [5:int32, 7:int32] [5:int32, 7:int32])))

(ert-deftest test-lists-negzipwith-negsecond-list-shorter ()

  (should (equal [5:int32, 7:int32] [5:int32, 7:int32])))

(ert-deftest test-lists-negzipwith-negempty-first-list ()

  (should (equal [] [])))

(ert-deftest test-lists-negzipwith-negempty-second-list ()

  (should (equal [] [])))

(ert-deftest test-lists-negzipwith-negstring-concatenation ()

  (should (equal ["a1", "b2"] ["a1", "b2"])))
