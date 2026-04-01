;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.lists primitives

;; apply

;; string transformations

(defun test-lists-negapply-negstring-transformations-negstring-transformations ()

  (assert (equal ["ONE", "TWO", "THREE", "one", "two", "three"] ["ONE", "TWO", "THREE", "one", "two", "three"])))

;; edge cases

(defun test-lists-negapply-negedge-cases-negempty-function-list ()

  (assert (equal [] [])))

(defun test-lists-negapply-negedge-cases-negempty-input-list ()

  (assert (equal [] [])))

(defun test-lists-negapply-negedge-cases-negsingle-function ()

  (assert (equal ["HELLO"] ["HELLO"])))

(defun test-lists-negapply-negedge-cases-negsingle-input ()

  (assert (equal ["TEST", "test"] ["TEST", "test"])))

;; at

(defun test-lists-negat-negfirst-element ()

  (assert (equal 1:int32 1:int32)))

(defun test-lists-negat-negmiddle-element ()

  (assert (equal 2:int32 2:int32)))

(defun test-lists-negat-neglast-element ()

  (assert (equal 3:int32 3:int32)))

(defun test-lists-negat-negsingle-element-list ()

  (assert (equal 42:int32 42:int32)))

(defun test-lists-negat-negstring-list-access ()

  (assert (equal "world" "world")))

;; bind

(defun test-lists-negbind-negnegation-function ()

  (assert (equal [-1:int32, -2:int32, -3:int32, -4:int32] [-1:int32, -2:int32, -3:int32, -4:int32])))

(defun test-lists-negbind-negempty-list ()

  (assert (equal [] [])))

(defun test-lists-negbind-negsingle-element ()

  (assert (equal [-5:int32] [-5:int32])))

(defun test-lists-negbind-negduplicate-elements ()

  (assert (equal [-1:int32, -1:int32, -2:int32] [-1:int32, -1:int32, -2:int32])))

;; concat

(defun test-lists-negconcat-negmultiple-non-negempty-lists ()

  (assert (equal [1:int32, 2:int32, 3:int32, 4:int32, 5:int32, 6:int32, 7:int32, 8:int32] [1:int32, 2:int32, 3:int32, 4:int32, 5:int32, 6:int32, 7:int32, 8:int32])))

(defun test-lists-negconcat-negempty-lists-included ()

  (assert (equal [1:int32, 2:int32, 3:int32] [1:int32, 2:int32, 3:int32])))

(defun test-lists-negconcat-negsingle-list ()

  (assert (equal [1:int32, 2:int32, 3:int32] [1:int32, 2:int32, 3:int32])))

(defun test-lists-negconcat-negall-empty-lists ()

  (assert (equal [] [])))

(defun test-lists-negconcat-negempty-list-of-lists ()

  (assert (equal [] [])))

;; concat2

(defun test-lists-negconcat2-negtwo-non-negempty-lists ()

  (assert (equal [1:int32, 2:int32, 3:int32, 4:int32] [1:int32, 2:int32, 3:int32, 4:int32])))

(defun test-lists-negconcat2-negfirst-list-empty ()

  (assert (equal [1:int32, 2:int32] [1:int32, 2:int32])))

(defun test-lists-negconcat2-negsecond-list-empty ()

  (assert (equal [1:int32, 2:int32] [1:int32, 2:int32])))

(defun test-lists-negconcat2-negboth-lists-empty ()

  (assert (equal [] [])))

(defun test-lists-negconcat2-negsingle-elements ()

  (assert (equal [1:int32, 2:int32] [1:int32, 2:int32])))

(defun test-lists-negconcat2-negstring-lists ()

  (assert (equal ["a", "b", "c", "d"] ["a", "b", "c", "d"])))

;; cons

(defun test-lists-negcons-negcons-to-non-negempty-list ()

  (assert (equal [1:int32, 2:int32, 3:int32] [1:int32, 2:int32, 3:int32])))

(defun test-lists-negcons-negcons-to-empty-list ()

  (assert (equal [1:int32] [1:int32])))

(defun test-lists-negcons-negcons-negative-number ()

  (assert (equal [-1:int32, 2:int32, 3:int32] [-1:int32, 2:int32, 3:int32])))

(defun test-lists-negcons-negcons-string ()

  (assert (equal ["hello", "world"] ["hello", "world"])))

;; drop

(defun test-lists-negdrop-negdrop-from-beginning ()

  (assert (equal [3:int32, 4:int32, 5:int32] [3:int32, 4:int32, 5:int32])))

(defun test-lists-negdrop-negdrop-zero-elements ()

  (assert (equal [1:int32, 2:int32, 3:int32] [1:int32, 2:int32, 3:int32])))

(defun test-lists-negdrop-negdrop-all-elements ()

  (assert (equal [] [])))

(defun test-lists-negdrop-negdrop-more-than-length ()

  (assert (equal [] [])))

(defun test-lists-negdrop-negdrop-from-empty-list ()

  (assert (equal [] [])))

(defun test-lists-negdrop-negdrop-negative-amount ()

  (assert (equal [1:int32, 2:int32, 3:int32] [1:int32, 2:int32, 3:int32])))

;; dropWhile

(defun test-lists-negdropwhile-negdrop-while-less-than-3 ()

  (assert (equal [3:int32, 2:int32, 1:int32] [3:int32, 2:int32, 1:int32])))

(defun test-lists-negdropwhile-negdrop-all-elements ()

  (assert (equal [] [])))

(defun test-lists-negdropwhile-negdrop-no-elements ()

  (assert (equal [1:int32, 2:int32, 3:int32] [1:int32, 2:int32, 3:int32])))

(defun test-lists-negdropwhile-negempty-list ()

  (assert (equal [] [])))

;; elem

(defun test-lists-negelem-negelement-present ()

  (assert (equal true true)))

(defun test-lists-negelem-negelement-not-present ()

  (assert (equal false false)))

(defun test-lists-negelem-negempty-list ()

  (assert (equal false false)))

(defun test-lists-negelem-negsingle-element-present ()

  (assert (equal true true)))

(defun test-lists-negelem-negsingle-element-not-present ()

  (assert (equal false false)))

(defun test-lists-negelem-negduplicate-elements ()

  (assert (equal true true)))

(defun test-lists-negelem-negstring-element-present ()

  (assert (equal true true)))

(defun test-lists-negelem-negstring-element-not-present ()

  (assert (equal false false)))

;; filter

(defun test-lists-negfilter-negfilter-positive-numbers ()

  (assert (equal [2:int32, 4:int32, 5:int32] [2:int32, 4:int32, 5:int32])))

(defun test-lists-negfilter-negfilter-all-elements ()

  (assert (equal [1:int32, 2:int32, 3:int32] [1:int32, 2:int32, 3:int32])))

(defun test-lists-negfilter-negfilter-no-elements ()

  (assert (equal [] [])))

(defun test-lists-negfilter-negempty-list ()

  (assert (equal [] [])))

;; find

(defun test-lists-negfind-negfind-existing-element ()

  (assert (equal just(4:int32) just(4:int32))))

(defun test-lists-negfind-negfind-first-matching ()

  (assert (equal just(1:int32) just(1:int32))))

(defun test-lists-negfind-negfind-no-match ()

  (assert (equal nothing nothing)))

(defun test-lists-negfind-negfind-in-empty-list ()

  (assert (equal nothing nothing)))

(defun test-lists-negfind-negfind-single-element ()

  (assert (equal just(42:int32) just(42:int32))))

;; foldl

(defun test-lists-negfoldl-negsum-with-addition ()

  (assert (equal 10:int32 10:int32)))

(defun test-lists-negfoldl-negproduct-with-multiplication ()

  (assert (equal 24:int32 24:int32)))

(defun test-lists-negfoldl-negempty-list ()

  (assert (equal 5:int32 5:int32)))

(defun test-lists-negfoldl-negsingle-element ()

  (assert (equal 15:int32 15:int32)))

(defun test-lists-negfoldl-negsubtraction-fold ()

  (assert (equal 4:int32 4:int32)))

;; foldr

(defun test-lists-negfoldr-negsubtraction-fold-right ()

  (assert (equal 2:int32 2:int32)))

(defun test-lists-negfoldr-negempty-list ()

  (assert (equal 5:int32 5:int32)))

(defun test-lists-negfoldr-negsingle-element ()

  (assert (equal 15:int32 15:int32)))

(defun test-lists-negfoldr-negsum-with-addition ()

  (assert (equal 10:int32 10:int32)))

(defun test-lists-negfoldr-negsubtraction-vs-foldl ()

  (assert (equal -8:int32 -8:int32)))

;; group

(defun test-lists-neggroup-negconsecutive-duplicates ()

  (assert (equal [[1:int32, 1:int32], [2:int32, 2:int32, 2:int32], [3:int32], [1:int32]] [[1:int32, 1:int32], [2:int32, 2:int32, 2:int32], [3:int32], [1:int32]])))

(defun test-lists-neggroup-negno-duplicates ()

  (assert (equal [[1:int32], [2:int32], [3:int32]] [[1:int32], [2:int32], [3:int32]])))

(defun test-lists-neggroup-negall-same ()

  (assert (equal [[1:int32, 1:int32, 1:int32]] [[1:int32, 1:int32, 1:int32]])))

(defun test-lists-neggroup-negempty-list ()

  (assert (equal [] [])))

(defun test-lists-neggroup-negsingle-element ()

  (assert (equal [[1:int32]] [[1:int32]])))

;; head

(defun test-lists-neghead-negthree-element-list ()

  (assert (equal 1:int32 1:int32)))

(defun test-lists-neghead-negsingle-element-list ()

  (assert (equal 42:int32 42:int32)))

(defun test-lists-neghead-negnegative-numbers ()

  (assert (equal -1:int32 -1:int32)))

(defun test-lists-neghead-negstring-list ()

  (assert (equal "hello" "hello")))

;; init

(defun test-lists-neginit-negmultiple-elements ()

  (assert (equal [1:int32, 2:int32, 3:int32] [1:int32, 2:int32, 3:int32])))

(defun test-lists-neginit-negtwo-elements ()

  (assert (equal [1:int32] [1:int32])))

(defun test-lists-neginit-negsingle-element ()

  (assert (equal [] [])))

(defun test-lists-neginit-negstring-list ()

  (assert (equal ["a", "b"] ["a", "b"])))

;; intercalate

(defun test-lists-negintercalate-negdouble-zero-separator ()

  (assert (equal [1:int32, 2:int32, 3:int32, 0:int32, 0:int32, 4:int32, 5:int32, 0:int32, 0:int32, 6:int32, 7:int32, 8:int32] [1:int32, 2:int32, 3:int32, 0:int32, 0:int32, 4:int32, 5:int32, 0:int32, 0:int32, 6:int32, 7:int32, 8:int32])))

(defun test-lists-negintercalate-negempty-separator ()

  (assert (equal [1:int32, 2:int32, 3:int32, 4:int32] [1:int32, 2:int32, 3:int32, 4:int32])))

(defun test-lists-negintercalate-negsingle-element-separator ()

  (assert (equal [1:int32, 99:int32, 2:int32, 99:int32, 3:int32] [1:int32, 99:int32, 2:int32, 99:int32, 3:int32])))

(defun test-lists-negintercalate-negempty-list-of-lists ()

  (assert (equal [] [])))

(defun test-lists-negintercalate-negsingle-list ()

  (assert (equal [1:int32, 2:int32, 3:int32] [1:int32, 2:int32, 3:int32])))

(defun test-lists-negintercalate-neglists-with-empty-lists ()

  (assert (equal [0:int32, 1:int32, 0:int32] [0:int32, 1:int32, 0:int32])))

;; intersperse

(defun test-lists-negintersperse-negstring-interspersion ()

  (assert (equal ["one", "and", "two", "and", "three"] ["one", "and", "two", "and", "three"])))

(defun test-lists-negintersperse-negsingle-element ()

  (assert (equal ["only"] ["only"])))

(defun test-lists-negintersperse-negempty-list ()

  (assert (equal [] [])))

(defun test-lists-negintersperse-negtwo-elements ()

  (assert (equal ["a", "+", "b"] ["a", "+", "b"])))

(defun test-lists-negintersperse-negnumber-interspersion ()

  (assert (equal [1:int32, 0:int32, 2:int32, 0:int32, 3:int32] [1:int32, 0:int32, 2:int32, 0:int32, 3:int32])))

;; last

(defun test-lists-neglast-negthree-element-list ()

  (assert (equal 3:int32 3:int32)))

(defun test-lists-neglast-negsingle-element-list ()

  (assert (equal 42:int32 42:int32)))

(defun test-lists-neglast-negnegative-numbers ()

  (assert (equal -3:int32 -3:int32)))

(defun test-lists-neglast-negstring-list ()

  (assert (equal "world" "world")))

;; length

(defun test-lists-neglength-negthree-elements ()

  (assert (equal 3:int32 3:int32)))

(defun test-lists-neglength-negempty-list ()

  (assert (equal 0:int32 0:int32)))

(defun test-lists-neglength-negsingle-element ()

  (assert (equal 1:int32 1:int32)))

(defun test-lists-neglength-negmany-elements ()

  (assert (equal 10:int32 10:int32)))

(defun test-lists-neglength-negstring-list ()

  (assert (equal 3:int32 3:int32)))

;; map

(defun test-lists-negmap-negstring-to-uppercase ()

  (assert (equal ["ONE", "TWO"] ["ONE", "TWO"])))

(defun test-lists-negmap-negempty-list ()

  (assert (equal [] [])))

(defun test-lists-negmap-negsingle-element ()

  (assert (equal ["HELLO"] ["HELLO"])))

(defun test-lists-negmap-negnumber-negation ()

  (assert (equal [-1:int32, -2:int32, -3:int32] [-1:int32, -2:int32, -3:int32])))

(defun test-lists-negmap-negidentity-function ()

  (assert (equal [1:int32, 2:int32, 3:int32] [1:int32, 2:int32, 3:int32])))

;; nub

(defun test-lists-negnub-negremove-duplicates ()

  (assert (equal [1:int32, 2:int32, 3:int32, 4:int32] [1:int32, 2:int32, 3:int32, 4:int32])))

(defun test-lists-negnub-negno-duplicates ()

  (assert (equal [1:int32, 2:int32, 3:int32] [1:int32, 2:int32, 3:int32])))

(defun test-lists-negnub-negall-duplicates ()

  (assert (equal [1:int32] [1:int32])))

(defun test-lists-negnub-negempty-list ()

  (assert (equal [] [])))

(defun test-lists-negnub-negsingle-element ()

  (assert (equal [1:int32] [1:int32])))

(defun test-lists-negnub-negstring-duplicates ()

  (assert (equal ["a", "b", "c"] ["a", "b", "c"])))

;; null

(defun test-lists-negnull-negempty-int-list ()

  (assert (equal true true)))

(defun test-lists-negnull-negsingle-element ()

  (assert (equal false false)))

(defun test-lists-negnull-negmultiple-elements ()

  (assert (equal false false)))

(defun test-lists-negnull-negempty-string-list ()

  (assert (equal true true)))

(defun test-lists-negnull-negnon-negempty-string-list ()

  (assert (equal false false)))

;; partition

(defun test-lists-negpartition-negpartition-greater-than-3 ()

  (assert (equal ([4:int32, 5:int32, 6:int32], [1:int32, 2:int32, 3:int32]) ([4:int32, 5:int32, 6:int32], [1:int32, 2:int32, 3:int32]))))

(defun test-lists-negpartition-negpartition-all-elements ()

  (assert (equal ([1:int32, 2:int32, 3:int32], []) ([1:int32, 2:int32, 3:int32], []))))

(defun test-lists-negpartition-negpartition-no-elements ()

  (assert (equal ([], [1:int32, 2:int32, 3:int32]) ([], [1:int32, 2:int32, 3:int32]))))

(defun test-lists-negpartition-negpartition-even-numbers ()

  (assert (equal ([2:int32, 4:int32, 6:int32], [1:int32, 3:int32, 5:int32]) ([2:int32, 4:int32, 6:int32], [1:int32, 3:int32, 5:int32]))))

(defun test-lists-negpartition-negempty-list ()

  (assert (equal ([], []) ([], []))))

;; pure

(defun test-lists-negpure-negstring-element ()

  (assert (equal ["one"] ["one"])))

(defun test-lists-negpure-negempty-string ()

  (assert (equal [""] [""])))

(defun test-lists-negpure-negnumber-element ()

  (assert (equal [42:int32] [42:int32])))

(defun test-lists-negpure-negnegative-number ()

  (assert (equal [-5:int32] [-5:int32])))

;; replicate

(defun test-lists-negreplicate-negreplicate-three-times ()

  (assert (equal [42:int32, 42:int32, 42:int32] [42:int32, 42:int32, 42:int32])))

(defun test-lists-negreplicate-negreplicate-zero-times ()

  (assert (equal [] [])))

(defun test-lists-negreplicate-negreplicate-once ()

  (assert (equal [99:int32] [99:int32])))

(defun test-lists-negreplicate-negreplicate-string ()

  (assert (equal ["hello", "hello"] ["hello", "hello"])))

;; reverse

(defun test-lists-negreverse-negmultiple-elements ()

  (assert (equal [4:int32, 3:int32, 2:int32, 1:int32] [4:int32, 3:int32, 2:int32, 1:int32])))

(defun test-lists-negreverse-negsingle-element ()

  (assert (equal [1:int32] [1:int32])))

(defun test-lists-negreverse-negempty-list ()

  (assert (equal [] [])))

(defun test-lists-negreverse-negtwo-elements ()

  (assert (equal [2:int32, 1:int32] [2:int32, 1:int32])))

(defun test-lists-negreverse-negstring-list ()

  (assert (equal ["c", "b", "a"] ["c", "b", "a"])))

;; safeHead

(defun test-lists-negsafehead-negnon-negempty-int-list ()

  (assert (equal just(1:int32) just(1:int32))))

(defun test-lists-negsafehead-negempty-int-list ()

  (assert (equal nothing nothing)))

(defun test-lists-negsafehead-negsingle-element ()

  (assert (equal just(42:int32) just(42:int32))))

(defun test-lists-negsafehead-negnon-negempty-string-list ()

  (assert (equal just("hello") just("hello"))))

(defun test-lists-negsafehead-negempty-string-list ()

  (assert (equal nothing nothing)))

;; singleton

(defun test-lists-negsingleton-negnumber-element ()

  (assert (equal [42:int32] [42:int32])))

(defun test-lists-negsingleton-negnegative-number ()

  (assert (equal [-1:int32] [-1:int32])))

(defun test-lists-negsingleton-negzero ()

  (assert (equal [0:int32] [0:int32])))

(defun test-lists-negsingleton-negstring-element ()

  (assert (equal ["hello"] ["hello"])))

;; sort

(defun test-lists-negsort-negunsorted-numbers ()

  (assert (equal [1:int32, 1:int32, 3:int32, 4:int32, 5:int32] [1:int32, 1:int32, 3:int32, 4:int32, 5:int32])))

(defun test-lists-negsort-negalready-sorted ()

  (assert (equal [1:int32, 2:int32, 3:int32] [1:int32, 2:int32, 3:int32])))

(defun test-lists-negsort-negreverse-sorted ()

  (assert (equal [1:int32, 2:int32, 3:int32] [1:int32, 2:int32, 3:int32])))

(defun test-lists-negsort-negsingle-element ()

  (assert (equal [1:int32] [1:int32])))

(defun test-lists-negsort-negempty-list ()

  (assert (equal [] [])))

(defun test-lists-negsort-negduplicates ()

  (assert (equal [1:int32, 1:int32, 2:int32, 2:int32, 3:int32] [1:int32, 1:int32, 2:int32, 2:int32, 3:int32])))

(defun test-lists-negsort-negstring-sort ()

  (assert (equal ["apple", "banana", "zebra"] ["apple", "banana", "zebra"])))

;; sortOn

(defun test-lists-negsorton-negsort-by-string-length ()

  (assert (equal ["hi", "hello", "world"] ["hi", "hello", "world"])))

(defun test-lists-negsorton-negempty-string-list ()

  (assert (equal [] [])))

(defun test-lists-negsorton-negsingle-string-element ()

  (assert (equal ["test"] ["test"])))

(defun test-lists-negsorton-negsort-by-negation ()

  (assert (equal [3:int32, 2:int32, 1:int32] [3:int32, 2:int32, 1:int32])))

(defun test-lists-negsorton-negsort-by-absolute-value ()

  (assert (equal [-1:int32, 2:int32, -3:int32] [-1:int32, 2:int32, -3:int32])))

;; span

(defun test-lists-negspan-negspan-less-than-3 ()

  (assert (equal ([1:int32, 2:int32], [3:int32, 1:int32, 2:int32]) ([1:int32, 2:int32], [3:int32, 1:int32, 2:int32]))))

(defun test-lists-negspan-negspan-all-elements ()

  (assert (equal ([1:int32, 2:int32, 3:int32], []) ([1:int32, 2:int32, 3:int32], []))))

(defun test-lists-negspan-negspan-no-elements ()

  (assert (equal ([], [1:int32, 2:int32, 3:int32]) ([], [1:int32, 2:int32, 3:int32]))))

(defun test-lists-negspan-negempty-list ()

  (assert (equal ([], []) ([], []))))

;; tail

(defun test-lists-negtail-negmultiple-elements ()

  (assert (equal [2:int32, 3:int32, 4:int32] [2:int32, 3:int32, 4:int32])))

(defun test-lists-negtail-negtwo-elements ()

  (assert (equal [2:int32] [2:int32])))

(defun test-lists-negtail-negsingle-element ()

  (assert (equal [] [])))

(defun test-lists-negtail-negstring-list ()

  (assert (equal ["b", "c"] ["b", "c"])))

;; take

(defun test-lists-negtake-negtake-from-beginning ()

  (assert (equal [1:int32, 2:int32] [1:int32, 2:int32])))

(defun test-lists-negtake-negtake-zero-elements ()

  (assert (equal [] [])))

(defun test-lists-negtake-negtake-all-elements ()

  (assert (equal [1:int32, 2:int32, 3:int32] [1:int32, 2:int32, 3:int32])))

(defun test-lists-negtake-negtake-more-than-length ()

  (assert (equal [1:int32, 2:int32] [1:int32, 2:int32])))

(defun test-lists-negtake-negtake-from-empty-list ()

  (assert (equal [] [])))

(defun test-lists-negtake-negtake-negative-amount ()

  (assert (equal [] [])))

;; transpose

(defun test-lists-negtranspose-negsquare-matrix ()

  (assert (equal [[1:int32, 4:int32], [2:int32, 5:int32], [3:int32, 6:int32]] [[1:int32, 4:int32], [2:int32, 5:int32], [3:int32, 6:int32]])))

(defun test-lists-negtranspose-negempty-lists ()

  (assert (equal [] [])))

(defun test-lists-negtranspose-negsingle-row ()

  (assert (equal [[1:int32], [2:int32], [3:int32]] [[1:int32], [2:int32], [3:int32]])))

(defun test-lists-negtranspose-negsingle-column ()

  (assert (equal [[1:int32, 2:int32, 3:int32]] [[1:int32, 2:int32, 3:int32]])))

(defun test-lists-negtranspose-negragged-matrix ()

  (assert (equal [[1:int32, 3:int32, 4:int32], [2:int32, 5:int32], [6:int32]] [[1:int32, 3:int32, 4:int32], [2:int32, 5:int32], [6:int32]])))

;; zip

(defun test-lists-negzip-negequal-length-lists ()

  (assert (equal [(1:int32, "a"), (2:int32, "b"), (3:int32, "c")] [(1:int32, "a"), (2:int32, "b"), (3:int32, "c")])))

(defun test-lists-negzip-negfirst-list-shorter ()

  (assert (equal [(1:int32, "a"), (2:int32, "b")] [(1:int32, "a"), (2:int32, "b")])))

(defun test-lists-negzip-negsecond-list-shorter ()

  (assert (equal [(1:int32, "a"), (2:int32, "b")] [(1:int32, "a"), (2:int32, "b")])))

(defun test-lists-negzip-negempty-first-list ()

  (assert (equal [] [])))

(defun test-lists-negzip-negempty-second-list ()

  (assert (equal [] [])))

(defun test-lists-negzip-negboth-empty-lists ()

  (assert (equal [] [])))

;; zipWith

(defun test-lists-negzipwith-negaddition ()

  (assert (equal [5:int32, 7:int32, 9:int32] [5:int32, 7:int32, 9:int32])))

(defun test-lists-negzipwith-negfirst-list-shorter ()

  (assert (equal [5:int32, 7:int32] [5:int32, 7:int32])))

(defun test-lists-negzipwith-negsecond-list-shorter ()

  (assert (equal [5:int32, 7:int32] [5:int32, 7:int32])))

(defun test-lists-negzipwith-negempty-first-list ()

  (assert (equal [] [])))

(defun test-lists-negzipwith-negempty-second-list ()

  (assert (equal [] [])))

(defun test-lists-negzipwith-negstring-concatenation ()

  (assert (equal ["a1", "b2"] ["a1", "b2"])))
