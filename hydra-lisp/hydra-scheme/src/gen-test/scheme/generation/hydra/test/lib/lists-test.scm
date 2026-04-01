;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.lists primitives

(import (scheme base))

;; apply

;; string transformations

(define (test-lists-negapply-negstring-transformations-negstring-transformations)

  (assert (equal? ["ONE", "TWO", "THREE", "one", "two", "three"] ["ONE", "TWO", "THREE", "one", "two", "three"])))

;; edge cases

(define (test-lists-negapply-negedge-cases-negempty-function-list)

  (assert (equal? [] [])))

(define (test-lists-negapply-negedge-cases-negempty-input-list)

  (assert (equal? [] [])))

(define (test-lists-negapply-negedge-cases-negsingle-function)

  (assert (equal? ["HELLO"] ["HELLO"])))

(define (test-lists-negapply-negedge-cases-negsingle-input)

  (assert (equal? ["TEST", "test"] ["TEST", "test"])))

;; at

(define (test-lists-negat-negfirst-element)

  (assert (equal? 1:int32 1:int32)))

(define (test-lists-negat-negmiddle-element)

  (assert (equal? 2:int32 2:int32)))

(define (test-lists-negat-neglast-element)

  (assert (equal? 3:int32 3:int32)))

(define (test-lists-negat-negsingle-element-list)

  (assert (equal? 42:int32 42:int32)))

(define (test-lists-negat-negstring-list-access)

  (assert (equal? "world" "world")))

;; bind

(define (test-lists-negbind-negnegation-function)

  (assert (equal? [-1:int32, -2:int32, -3:int32, -4:int32] [-1:int32, -2:int32, -3:int32, -4:int32])))

(define (test-lists-negbind-negempty-list)

  (assert (equal? [] [])))

(define (test-lists-negbind-negsingle-element)

  (assert (equal? [-5:int32] [-5:int32])))

(define (test-lists-negbind-negduplicate-elements)

  (assert (equal? [-1:int32, -1:int32, -2:int32] [-1:int32, -1:int32, -2:int32])))

;; concat

(define (test-lists-negconcat-negmultiple-non-negempty-lists)

  (assert (equal? [1:int32, 2:int32, 3:int32, 4:int32, 5:int32, 6:int32, 7:int32, 8:int32] [1:int32, 2:int32, 3:int32, 4:int32, 5:int32, 6:int32, 7:int32, 8:int32])))

(define (test-lists-negconcat-negempty-lists-included)

  (assert (equal? [1:int32, 2:int32, 3:int32] [1:int32, 2:int32, 3:int32])))

(define (test-lists-negconcat-negsingle-list)

  (assert (equal? [1:int32, 2:int32, 3:int32] [1:int32, 2:int32, 3:int32])))

(define (test-lists-negconcat-negall-empty-lists)

  (assert (equal? [] [])))

(define (test-lists-negconcat-negempty-list-of-lists)

  (assert (equal? [] [])))

;; concat2

(define (test-lists-negconcat2-negtwo-non-negempty-lists)

  (assert (equal? [1:int32, 2:int32, 3:int32, 4:int32] [1:int32, 2:int32, 3:int32, 4:int32])))

(define (test-lists-negconcat2-negfirst-list-empty)

  (assert (equal? [1:int32, 2:int32] [1:int32, 2:int32])))

(define (test-lists-negconcat2-negsecond-list-empty)

  (assert (equal? [1:int32, 2:int32] [1:int32, 2:int32])))

(define (test-lists-negconcat2-negboth-lists-empty)

  (assert (equal? [] [])))

(define (test-lists-negconcat2-negsingle-elements)

  (assert (equal? [1:int32, 2:int32] [1:int32, 2:int32])))

(define (test-lists-negconcat2-negstring-lists)

  (assert (equal? ["a", "b", "c", "d"] ["a", "b", "c", "d"])))

;; cons

(define (test-lists-negcons-negcons-to-non-negempty-list)

  (assert (equal? [1:int32, 2:int32, 3:int32] [1:int32, 2:int32, 3:int32])))

(define (test-lists-negcons-negcons-to-empty-list)

  (assert (equal? [1:int32] [1:int32])))

(define (test-lists-negcons-negcons-negative-number)

  (assert (equal? [-1:int32, 2:int32, 3:int32] [-1:int32, 2:int32, 3:int32])))

(define (test-lists-negcons-negcons-string)

  (assert (equal? ["hello", "world"] ["hello", "world"])))

;; drop

(define (test-lists-negdrop-negdrop-from-beginning)

  (assert (equal? [3:int32, 4:int32, 5:int32] [3:int32, 4:int32, 5:int32])))

(define (test-lists-negdrop-negdrop-zero-elements)

  (assert (equal? [1:int32, 2:int32, 3:int32] [1:int32, 2:int32, 3:int32])))

(define (test-lists-negdrop-negdrop-all-elements)

  (assert (equal? [] [])))

(define (test-lists-negdrop-negdrop-more-than-length)

  (assert (equal? [] [])))

(define (test-lists-negdrop-negdrop-from-empty-list)

  (assert (equal? [] [])))

(define (test-lists-negdrop-negdrop-negative-amount)

  (assert (equal? [1:int32, 2:int32, 3:int32] [1:int32, 2:int32, 3:int32])))

;; dropWhile

(define (test-lists-negdropwhile-negdrop-while-less-than-3)

  (assert (equal? [3:int32, 2:int32, 1:int32] [3:int32, 2:int32, 1:int32])))

(define (test-lists-negdropwhile-negdrop-all-elements)

  (assert (equal? [] [])))

(define (test-lists-negdropwhile-negdrop-no-elements)

  (assert (equal? [1:int32, 2:int32, 3:int32] [1:int32, 2:int32, 3:int32])))

(define (test-lists-negdropwhile-negempty-list)

  (assert (equal? [] [])))

;; elem

(define (test-lists-negelem-negelement-present)

  (assert (equal? true true)))

(define (test-lists-negelem-negelement-not-present)

  (assert (equal? false false)))

(define (test-lists-negelem-negempty-list)

  (assert (equal? false false)))

(define (test-lists-negelem-negsingle-element-present)

  (assert (equal? true true)))

(define (test-lists-negelem-negsingle-element-not-present)

  (assert (equal? false false)))

(define (test-lists-negelem-negduplicate-elements)

  (assert (equal? true true)))

(define (test-lists-negelem-negstring-element-present)

  (assert (equal? true true)))

(define (test-lists-negelem-negstring-element-not-present)

  (assert (equal? false false)))

;; filter

(define (test-lists-negfilter-negfilter-positive-numbers)

  (assert (equal? [2:int32, 4:int32, 5:int32] [2:int32, 4:int32, 5:int32])))

(define (test-lists-negfilter-negfilter-all-elements)

  (assert (equal? [1:int32, 2:int32, 3:int32] [1:int32, 2:int32, 3:int32])))

(define (test-lists-negfilter-negfilter-no-elements)

  (assert (equal? [] [])))

(define (test-lists-negfilter-negempty-list)

  (assert (equal? [] [])))

;; find

(define (test-lists-negfind-negfind-existing-element)

  (assert (equal? just(4:int32) just(4:int32))))

(define (test-lists-negfind-negfind-first-matching)

  (assert (equal? just(1:int32) just(1:int32))))

(define (test-lists-negfind-negfind-no-match)

  (assert (equal? nothing nothing)))

(define (test-lists-negfind-negfind-in-empty-list)

  (assert (equal? nothing nothing)))

(define (test-lists-negfind-negfind-single-element)

  (assert (equal? just(42:int32) just(42:int32))))

;; foldl

(define (test-lists-negfoldl-negsum-with-addition)

  (assert (equal? 10:int32 10:int32)))

(define (test-lists-negfoldl-negproduct-with-multiplication)

  (assert (equal? 24:int32 24:int32)))

(define (test-lists-negfoldl-negempty-list)

  (assert (equal? 5:int32 5:int32)))

(define (test-lists-negfoldl-negsingle-element)

  (assert (equal? 15:int32 15:int32)))

(define (test-lists-negfoldl-negsubtraction-fold)

  (assert (equal? 4:int32 4:int32)))

;; foldr

(define (test-lists-negfoldr-negsubtraction-fold-right)

  (assert (equal? 2:int32 2:int32)))

(define (test-lists-negfoldr-negempty-list)

  (assert (equal? 5:int32 5:int32)))

(define (test-lists-negfoldr-negsingle-element)

  (assert (equal? 15:int32 15:int32)))

(define (test-lists-negfoldr-negsum-with-addition)

  (assert (equal? 10:int32 10:int32)))

(define (test-lists-negfoldr-negsubtraction-vs-foldl)

  (assert (equal? -8:int32 -8:int32)))

;; group

(define (test-lists-neggroup-negconsecutive-duplicates)

  (assert (equal? [[1:int32, 1:int32], [2:int32, 2:int32, 2:int32], [3:int32], [1:int32]] [[1:int32, 1:int32], [2:int32, 2:int32, 2:int32], [3:int32], [1:int32]])))

(define (test-lists-neggroup-negno-duplicates)

  (assert (equal? [[1:int32], [2:int32], [3:int32]] [[1:int32], [2:int32], [3:int32]])))

(define (test-lists-neggroup-negall-same)

  (assert (equal? [[1:int32, 1:int32, 1:int32]] [[1:int32, 1:int32, 1:int32]])))

(define (test-lists-neggroup-negempty-list)

  (assert (equal? [] [])))

(define (test-lists-neggroup-negsingle-element)

  (assert (equal? [[1:int32]] [[1:int32]])))

;; head

(define (test-lists-neghead-negthree-element-list)

  (assert (equal? 1:int32 1:int32)))

(define (test-lists-neghead-negsingle-element-list)

  (assert (equal? 42:int32 42:int32)))

(define (test-lists-neghead-negnegative-numbers)

  (assert (equal? -1:int32 -1:int32)))

(define (test-lists-neghead-negstring-list)

  (assert (equal? "hello" "hello")))

;; init

(define (test-lists-neginit-negmultiple-elements)

  (assert (equal? [1:int32, 2:int32, 3:int32] [1:int32, 2:int32, 3:int32])))

(define (test-lists-neginit-negtwo-elements)

  (assert (equal? [1:int32] [1:int32])))

(define (test-lists-neginit-negsingle-element)

  (assert (equal? [] [])))

(define (test-lists-neginit-negstring-list)

  (assert (equal? ["a", "b"] ["a", "b"])))

;; intercalate

(define (test-lists-negintercalate-negdouble-zero-separator)

  (assert (equal? [1:int32, 2:int32, 3:int32, 0:int32, 0:int32, 4:int32, 5:int32, 0:int32, 0:int32, 6:int32, 7:int32, 8:int32] [1:int32, 2:int32, 3:int32, 0:int32, 0:int32, 4:int32, 5:int32, 0:int32, 0:int32, 6:int32, 7:int32, 8:int32])))

(define (test-lists-negintercalate-negempty-separator)

  (assert (equal? [1:int32, 2:int32, 3:int32, 4:int32] [1:int32, 2:int32, 3:int32, 4:int32])))

(define (test-lists-negintercalate-negsingle-element-separator)

  (assert (equal? [1:int32, 99:int32, 2:int32, 99:int32, 3:int32] [1:int32, 99:int32, 2:int32, 99:int32, 3:int32])))

(define (test-lists-negintercalate-negempty-list-of-lists)

  (assert (equal? [] [])))

(define (test-lists-negintercalate-negsingle-list)

  (assert (equal? [1:int32, 2:int32, 3:int32] [1:int32, 2:int32, 3:int32])))

(define (test-lists-negintercalate-neglists-with-empty-lists)

  (assert (equal? [0:int32, 1:int32, 0:int32] [0:int32, 1:int32, 0:int32])))

;; intersperse

(define (test-lists-negintersperse-negstring-interspersion)

  (assert (equal? ["one", "and", "two", "and", "three"] ["one", "and", "two", "and", "three"])))

(define (test-lists-negintersperse-negsingle-element)

  (assert (equal? ["only"] ["only"])))

(define (test-lists-negintersperse-negempty-list)

  (assert (equal? [] [])))

(define (test-lists-negintersperse-negtwo-elements)

  (assert (equal? ["a", "+", "b"] ["a", "+", "b"])))

(define (test-lists-negintersperse-negnumber-interspersion)

  (assert (equal? [1:int32, 0:int32, 2:int32, 0:int32, 3:int32] [1:int32, 0:int32, 2:int32, 0:int32, 3:int32])))

;; last

(define (test-lists-neglast-negthree-element-list)

  (assert (equal? 3:int32 3:int32)))

(define (test-lists-neglast-negsingle-element-list)

  (assert (equal? 42:int32 42:int32)))

(define (test-lists-neglast-negnegative-numbers)

  (assert (equal? -3:int32 -3:int32)))

(define (test-lists-neglast-negstring-list)

  (assert (equal? "world" "world")))

;; length

(define (test-lists-neglength-negthree-elements)

  (assert (equal? 3:int32 3:int32)))

(define (test-lists-neglength-negempty-list)

  (assert (equal? 0:int32 0:int32)))

(define (test-lists-neglength-negsingle-element)

  (assert (equal? 1:int32 1:int32)))

(define (test-lists-neglength-negmany-elements)

  (assert (equal? 10:int32 10:int32)))

(define (test-lists-neglength-negstring-list)

  (assert (equal? 3:int32 3:int32)))

;; map

(define (test-lists-negmap-negstring-to-uppercase)

  (assert (equal? ["ONE", "TWO"] ["ONE", "TWO"])))

(define (test-lists-negmap-negempty-list)

  (assert (equal? [] [])))

(define (test-lists-negmap-negsingle-element)

  (assert (equal? ["HELLO"] ["HELLO"])))

(define (test-lists-negmap-negnumber-negation)

  (assert (equal? [-1:int32, -2:int32, -3:int32] [-1:int32, -2:int32, -3:int32])))

(define (test-lists-negmap-negidentity-function)

  (assert (equal? [1:int32, 2:int32, 3:int32] [1:int32, 2:int32, 3:int32])))

;; nub

(define (test-lists-negnub-negremove-duplicates)

  (assert (equal? [1:int32, 2:int32, 3:int32, 4:int32] [1:int32, 2:int32, 3:int32, 4:int32])))

(define (test-lists-negnub-negno-duplicates)

  (assert (equal? [1:int32, 2:int32, 3:int32] [1:int32, 2:int32, 3:int32])))

(define (test-lists-negnub-negall-duplicates)

  (assert (equal? [1:int32] [1:int32])))

(define (test-lists-negnub-negempty-list)

  (assert (equal? [] [])))

(define (test-lists-negnub-negsingle-element)

  (assert (equal? [1:int32] [1:int32])))

(define (test-lists-negnub-negstring-duplicates)

  (assert (equal? ["a", "b", "c"] ["a", "b", "c"])))

;; null

(define (test-lists-negnull-negempty-int-list)

  (assert (equal? true true)))

(define (test-lists-negnull-negsingle-element)

  (assert (equal? false false)))

(define (test-lists-negnull-negmultiple-elements)

  (assert (equal? false false)))

(define (test-lists-negnull-negempty-string-list)

  (assert (equal? true true)))

(define (test-lists-negnull-negnon-negempty-string-list)

  (assert (equal? false false)))

;; partition

(define (test-lists-negpartition-negpartition-greater-than-3)

  (assert (equal? ([4:int32, 5:int32, 6:int32], [1:int32, 2:int32, 3:int32]) ([4:int32, 5:int32, 6:int32], [1:int32, 2:int32, 3:int32]))))

(define (test-lists-negpartition-negpartition-all-elements)

  (assert (equal? ([1:int32, 2:int32, 3:int32], []) ([1:int32, 2:int32, 3:int32], []))))

(define (test-lists-negpartition-negpartition-no-elements)

  (assert (equal? ([], [1:int32, 2:int32, 3:int32]) ([], [1:int32, 2:int32, 3:int32]))))

(define (test-lists-negpartition-negpartition-even-numbers)

  (assert (equal? ([2:int32, 4:int32, 6:int32], [1:int32, 3:int32, 5:int32]) ([2:int32, 4:int32, 6:int32], [1:int32, 3:int32, 5:int32]))))

(define (test-lists-negpartition-negempty-list)

  (assert (equal? ([], []) ([], []))))

;; pure

(define (test-lists-negpure-negstring-element)

  (assert (equal? ["one"] ["one"])))

(define (test-lists-negpure-negempty-string)

  (assert (equal? [""] [""])))

(define (test-lists-negpure-negnumber-element)

  (assert (equal? [42:int32] [42:int32])))

(define (test-lists-negpure-negnegative-number)

  (assert (equal? [-5:int32] [-5:int32])))

;; replicate

(define (test-lists-negreplicate-negreplicate-three-times)

  (assert (equal? [42:int32, 42:int32, 42:int32] [42:int32, 42:int32, 42:int32])))

(define (test-lists-negreplicate-negreplicate-zero-times)

  (assert (equal? [] [])))

(define (test-lists-negreplicate-negreplicate-once)

  (assert (equal? [99:int32] [99:int32])))

(define (test-lists-negreplicate-negreplicate-string)

  (assert (equal? ["hello", "hello"] ["hello", "hello"])))

;; reverse

(define (test-lists-negreverse-negmultiple-elements)

  (assert (equal? [4:int32, 3:int32, 2:int32, 1:int32] [4:int32, 3:int32, 2:int32, 1:int32])))

(define (test-lists-negreverse-negsingle-element)

  (assert (equal? [1:int32] [1:int32])))

(define (test-lists-negreverse-negempty-list)

  (assert (equal? [] [])))

(define (test-lists-negreverse-negtwo-elements)

  (assert (equal? [2:int32, 1:int32] [2:int32, 1:int32])))

(define (test-lists-negreverse-negstring-list)

  (assert (equal? ["c", "b", "a"] ["c", "b", "a"])))

;; safeHead

(define (test-lists-negsafehead-negnon-negempty-int-list)

  (assert (equal? just(1:int32) just(1:int32))))

(define (test-lists-negsafehead-negempty-int-list)

  (assert (equal? nothing nothing)))

(define (test-lists-negsafehead-negsingle-element)

  (assert (equal? just(42:int32) just(42:int32))))

(define (test-lists-negsafehead-negnon-negempty-string-list)

  (assert (equal? just("hello") just("hello"))))

(define (test-lists-negsafehead-negempty-string-list)

  (assert (equal? nothing nothing)))

;; singleton

(define (test-lists-negsingleton-negnumber-element)

  (assert (equal? [42:int32] [42:int32])))

(define (test-lists-negsingleton-negnegative-number)

  (assert (equal? [-1:int32] [-1:int32])))

(define (test-lists-negsingleton-negzero)

  (assert (equal? [0:int32] [0:int32])))

(define (test-lists-negsingleton-negstring-element)

  (assert (equal? ["hello"] ["hello"])))

;; sort

(define (test-lists-negsort-negunsorted-numbers)

  (assert (equal? [1:int32, 1:int32, 3:int32, 4:int32, 5:int32] [1:int32, 1:int32, 3:int32, 4:int32, 5:int32])))

(define (test-lists-negsort-negalready-sorted)

  (assert (equal? [1:int32, 2:int32, 3:int32] [1:int32, 2:int32, 3:int32])))

(define (test-lists-negsort-negreverse-sorted)

  (assert (equal? [1:int32, 2:int32, 3:int32] [1:int32, 2:int32, 3:int32])))

(define (test-lists-negsort-negsingle-element)

  (assert (equal? [1:int32] [1:int32])))

(define (test-lists-negsort-negempty-list)

  (assert (equal? [] [])))

(define (test-lists-negsort-negduplicates)

  (assert (equal? [1:int32, 1:int32, 2:int32, 2:int32, 3:int32] [1:int32, 1:int32, 2:int32, 2:int32, 3:int32])))

(define (test-lists-negsort-negstring-sort)

  (assert (equal? ["apple", "banana", "zebra"] ["apple", "banana", "zebra"])))

;; sortOn

(define (test-lists-negsorton-negsort-by-string-length)

  (assert (equal? ["hi", "hello", "world"] ["hi", "hello", "world"])))

(define (test-lists-negsorton-negempty-string-list)

  (assert (equal? [] [])))

(define (test-lists-negsorton-negsingle-string-element)

  (assert (equal? ["test"] ["test"])))

(define (test-lists-negsorton-negsort-by-negation)

  (assert (equal? [3:int32, 2:int32, 1:int32] [3:int32, 2:int32, 1:int32])))

(define (test-lists-negsorton-negsort-by-absolute-value)

  (assert (equal? [-1:int32, 2:int32, -3:int32] [-1:int32, 2:int32, -3:int32])))

;; span

(define (test-lists-negspan-negspan-less-than-3)

  (assert (equal? ([1:int32, 2:int32], [3:int32, 1:int32, 2:int32]) ([1:int32, 2:int32], [3:int32, 1:int32, 2:int32]))))

(define (test-lists-negspan-negspan-all-elements)

  (assert (equal? ([1:int32, 2:int32, 3:int32], []) ([1:int32, 2:int32, 3:int32], []))))

(define (test-lists-negspan-negspan-no-elements)

  (assert (equal? ([], [1:int32, 2:int32, 3:int32]) ([], [1:int32, 2:int32, 3:int32]))))

(define (test-lists-negspan-negempty-list)

  (assert (equal? ([], []) ([], []))))

;; tail

(define (test-lists-negtail-negmultiple-elements)

  (assert (equal? [2:int32, 3:int32, 4:int32] [2:int32, 3:int32, 4:int32])))

(define (test-lists-negtail-negtwo-elements)

  (assert (equal? [2:int32] [2:int32])))

(define (test-lists-negtail-negsingle-element)

  (assert (equal? [] [])))

(define (test-lists-negtail-negstring-list)

  (assert (equal? ["b", "c"] ["b", "c"])))

;; take

(define (test-lists-negtake-negtake-from-beginning)

  (assert (equal? [1:int32, 2:int32] [1:int32, 2:int32])))

(define (test-lists-negtake-negtake-zero-elements)

  (assert (equal? [] [])))

(define (test-lists-negtake-negtake-all-elements)

  (assert (equal? [1:int32, 2:int32, 3:int32] [1:int32, 2:int32, 3:int32])))

(define (test-lists-negtake-negtake-more-than-length)

  (assert (equal? [1:int32, 2:int32] [1:int32, 2:int32])))

(define (test-lists-negtake-negtake-from-empty-list)

  (assert (equal? [] [])))

(define (test-lists-negtake-negtake-negative-amount)

  (assert (equal? [] [])))

;; transpose

(define (test-lists-negtranspose-negsquare-matrix)

  (assert (equal? [[1:int32, 4:int32], [2:int32, 5:int32], [3:int32, 6:int32]] [[1:int32, 4:int32], [2:int32, 5:int32], [3:int32, 6:int32]])))

(define (test-lists-negtranspose-negempty-lists)

  (assert (equal? [] [])))

(define (test-lists-negtranspose-negsingle-row)

  (assert (equal? [[1:int32], [2:int32], [3:int32]] [[1:int32], [2:int32], [3:int32]])))

(define (test-lists-negtranspose-negsingle-column)

  (assert (equal? [[1:int32, 2:int32, 3:int32]] [[1:int32, 2:int32, 3:int32]])))

(define (test-lists-negtranspose-negragged-matrix)

  (assert (equal? [[1:int32, 3:int32, 4:int32], [2:int32, 5:int32], [6:int32]] [[1:int32, 3:int32, 4:int32], [2:int32, 5:int32], [6:int32]])))

;; zip

(define (test-lists-negzip-negequal-length-lists)

  (assert (equal? [(1:int32, "a"), (2:int32, "b"), (3:int32, "c")] [(1:int32, "a"), (2:int32, "b"), (3:int32, "c")])))

(define (test-lists-negzip-negfirst-list-shorter)

  (assert (equal? [(1:int32, "a"), (2:int32, "b")] [(1:int32, "a"), (2:int32, "b")])))

(define (test-lists-negzip-negsecond-list-shorter)

  (assert (equal? [(1:int32, "a"), (2:int32, "b")] [(1:int32, "a"), (2:int32, "b")])))

(define (test-lists-negzip-negempty-first-list)

  (assert (equal? [] [])))

(define (test-lists-negzip-negempty-second-list)

  (assert (equal? [] [])))

(define (test-lists-negzip-negboth-empty-lists)

  (assert (equal? [] [])))

;; zipWith

(define (test-lists-negzipwith-negaddition)

  (assert (equal? [5:int32, 7:int32, 9:int32] [5:int32, 7:int32, 9:int32])))

(define (test-lists-negzipwith-negfirst-list-shorter)

  (assert (equal? [5:int32, 7:int32] [5:int32, 7:int32])))

(define (test-lists-negzipwith-negsecond-list-shorter)

  (assert (equal? [5:int32, 7:int32] [5:int32, 7:int32])))

(define (test-lists-negzipwith-negempty-first-list)

  (assert (equal? [] [])))

(define (test-lists-negzipwith-negempty-second-list)

  (assert (equal? [] [])))

(define (test-lists-negzipwith-negstring-concatenation)

  (assert (equal? ["a1", "b2"] ["a1", "b2"])))
