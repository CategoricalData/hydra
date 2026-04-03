;;; Note: this is an automatically generated file. Do not edit. -*- lexical-binding: t; coding: utf-8 -*-
;;; hydra.lib.eithers primitives

(require 'ert)

;; bind

(ert-deftest test-eithers-negbind-negbind-right-with-success ()

  (should (equal right(2) right(2))))

(ert-deftest test-eithers-negbind-negbind-right-with-failure ()

  (should (equal left(0) left(0))))

(ert-deftest test-eithers-negbind-negbind-left-returns-left-unchanged ()

  (should (equal left(42) left(42))))

;; bimap

(ert-deftest test-eithers-negbimap-negmap-left-value ()

  (should (equal left(10) left(10))))

(ert-deftest test-eithers-negbimap-negmap-right-value ()

  (should (equal right(2) right(2))))

;; isLeft

(ert-deftest test-eithers-negisleft-negleft-value ()

  (should (equal true true)))

(ert-deftest test-eithers-negisleft-negright-value ()

  (should (equal false false)))

;; isRight

(ert-deftest test-eithers-negisright-negright-value ()

  (should (equal true true)))

(ert-deftest test-eithers-negisright-negleft-value ()

  (should (equal false false)))

;; fromLeft

(ert-deftest test-eithers-negfromleft-negextract-left ()

  (should (equal 42 42)))

(ert-deftest test-eithers-negfromleft-neguse-default-for-right ()

  (should (equal 99 99)))

;; fromRight

(ert-deftest test-eithers-negfromright-negextract-right ()

  (should (equal "test" "test")))

(ert-deftest test-eithers-negfromright-neguse-default-for-left ()

  (should (equal "default" "default")))

;; either

(ert-deftest test-eithers-negeither-negapply-left-function ()

  (should (equal 10 10)))

(ert-deftest test-eithers-negeither-negapply-right-function ()

  (should (equal 2 2)))

;; lefts

(ert-deftest test-eithers-neglefts-negfilter-left-values ()

  (should (equal [1, 2] [1, 2])))

(ert-deftest test-eithers-neglefts-negall-lefts ()

  (should (equal [1, 2] [1, 2])))

(ert-deftest test-eithers-neglefts-negall-rights ()

  (should (equal [] [])))

(ert-deftest test-eithers-neglefts-negempty-list ()

  (should (equal [] [])))

;; rights

(ert-deftest test-eithers-negrights-negfilter-right-values ()

  (should (equal ["a", "b"] ["a", "b"])))

(ert-deftest test-eithers-negrights-negall-rights ()

  (should (equal ["a", "b"] ["a", "b"])))

(ert-deftest test-eithers-negrights-negall-lefts ()

  (should (equal [] [])))

(ert-deftest test-eithers-negrights-negempty-list ()

  (should (equal [] [])))

;; partitionEithers

(ert-deftest test-eithers-negpartitioneithers-negpartition-mixed ()

  (should (equal ([1, 2], ["a", "b"]) ([1, 2], ["a", "b"]))))

(ert-deftest test-eithers-negpartitioneithers-negall-lefts ()

  (should (equal ([1, 2], []) ([1, 2], []))))

(ert-deftest test-eithers-negpartitioneithers-negall-rights ()

  (should (equal ([], ["a", "b"]) ([], ["a", "b"]))))

(ert-deftest test-eithers-negpartitioneithers-negempty-list ()

  (should (equal ([], []) ([], []))))

;; map

(ert-deftest test-eithers-negmap-negmap-right-value ()

  (should (equal right(10) right(10))))

(ert-deftest test-eithers-negmap-negpreserve-left ()

  (should (equal left(99) left(99))))

;; mapList

(ert-deftest test-eithers-negmaplist-negall-succeed ()

  (should (equal right([2, 4, 6]) right([2, 4, 6]))))

(ert-deftest test-eithers-negmaplist-negfirst-fails ()

  (should (equal left("zero") left("zero"))))

(ert-deftest test-eithers-negmaplist-negempty-list ()

  (should (equal right([]) right([]))))

;; mapMaybe

(ert-deftest test-eithers-negmapmaybe-negjust-succeeds ()

  (should (equal right(just(10)) right(just(10)))))

(ert-deftest test-eithers-negmapmaybe-negjust-fails ()

  (should (equal left("zero") left("zero"))))

(ert-deftest test-eithers-negmapmaybe-negnothing ()

  (should (equal right(nothing) right(nothing))))
