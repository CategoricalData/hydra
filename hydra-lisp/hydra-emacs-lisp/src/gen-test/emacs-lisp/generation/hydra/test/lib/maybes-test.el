;;; Note: this is an automatically generated file. Do not edit. -*- lexical-binding: t; coding: utf-8 -*-
;;; hydra.lib.maybes primitives

(require 'ert)

;; apply

(ert-deftest test-maybes-negapply-negboth-just ()

  (should (equal just(8) just(8))))

(ert-deftest test-maybes-negapply-negnothing-function ()

  (should (equal nothing nothing)))

(ert-deftest test-maybes-negapply-negnothing-value ()

  (should (equal nothing nothing)))

;; bind

(ert-deftest test-maybes-negbind-negjust-to-just ()

  (should (equal just(10) just(10))))

(ert-deftest test-maybes-negbind-negnothing-to-nothing ()

  (should (equal nothing nothing)))

;; cases

(ert-deftest test-maybes-negcases-negjust-applies-function ()

  (should (equal 10 10)))

(ert-deftest test-maybes-negcases-negnothing-returns-default ()

  (should (equal 99 99)))

;; cat

(ert-deftest test-maybes-negcat-negfilters-nothings ()

  (should (equal [1, 2] [1, 2])))

(ert-deftest test-maybes-negcat-negall-justs ()

  (should (equal [1, 2] [1, 2])))

(ert-deftest test-maybes-negcat-negall-nothings ()

  (should (equal [] [])))

(ert-deftest test-maybes-negcat-negempty-list ()

  (should (equal [] [])))

;; compose

(ert-deftest test-maybes-negcompose-negboth-succeed ()

  (should (equal just(12) just(12))))

(ert-deftest test-maybes-negcompose-negfirst-fails ()

  (should (equal nothing nothing)))

(ert-deftest test-maybes-negcompose-negsecond-fails ()

  (should (equal nothing nothing)))

;; fromJust

(ert-deftest test-maybes-negfromjust-negextract-from-just ()

  (should (equal 42 42)))

;; fromMaybe

(ert-deftest test-maybes-negfrommaybe-negjust-value ()

  (should (equal 42 42)))

(ert-deftest test-maybes-negfrommaybe-negnothing-with-default ()

  (should (equal 99 99)))

;; isJust

(ert-deftest test-maybes-negisjust-negjust-value ()

  (should (equal true true)))

(ert-deftest test-maybes-negisjust-negnothing ()

  (should (equal false false)))

;; isNothing

(ert-deftest test-maybes-negisnothing-negjust-value ()

  (should (equal false false)))

(ert-deftest test-maybes-negisnothing-negnothing ()

  (should (equal true true)))

;; map

(ert-deftest test-maybes-negmap-negmaps-just-value ()

  (should (equal just(10) just(10))))

(ert-deftest test-maybes-negmap-negnothing-unchanged ()

  (should (equal nothing nothing)))

;; mapMaybe

(ert-deftest test-maybes-negmapmaybe-negfilter-and-transform ()

  (should (equal [6, 8, 10] [6, 8, 10])))

(ert-deftest test-maybes-negmapmaybe-negempty-result ()

  (should (equal [] [])))

(ert-deftest test-maybes-negmapmaybe-negempty-input ()

  (should (equal [] [])))

;; maybe

(ert-deftest test-maybes-negmaybe-negjust-value-applies-function ()

  (should (equal 10 10)))

(ert-deftest test-maybes-negmaybe-negnothing-returns-default ()

  (should (equal 99 99)))

;; pure

(ert-deftest test-maybes-negpure-negwraps-integer ()

  (should (equal just(42) just(42))))

(ert-deftest test-maybes-negpure-negwraps-string ()

  (should (equal just("hello") just("hello"))))

;; toList

(ert-deftest test-maybes-negtolist-negjust-value ()

  (should (equal [42] [42])))

(ert-deftest test-maybes-negtolist-negnothing ()

  (should (equal [] [])))
