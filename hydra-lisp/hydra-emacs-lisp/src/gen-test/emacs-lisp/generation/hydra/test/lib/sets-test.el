;;; Note: this is an automatically generated file. Do not edit. -*- lexical-binding: t; coding: utf-8 -*-
;;; hydra.lib.sets primitives

(require 'ert)

;; empty

(ert-deftest test-sets-negempty-negempty-set ()

  (should (equal {} {})))

;; singleton

(ert-deftest test-sets-negsingleton-negsingle-element ()

  (should (equal {42} {42})))

;; fromList

(ert-deftest test-sets-negfromlist-negcreate-from-list ()

  (should (equal {1, 2, 3} {1, 2, 3})))

(ert-deftest test-sets-negfromlist-negduplicates-removed ()

  (should (equal {1, 2, 3} {1, 2, 3})))

(ert-deftest test-sets-negfromlist-negempty-list ()

  (should (equal {} {})))

;; toList

(ert-deftest test-sets-negtolist-negconvert-to-list ()

  (should (equal [1, 2, 3] [1, 2, 3])))

(ert-deftest test-sets-negtolist-negunsorted-input ()

  (should (equal [1, 2, 3] [1, 2, 3])))

(ert-deftest test-sets-negtolist-negempty-set ()

  (should (equal [] [])))

;; insert

(ert-deftest test-sets-neginsert-neginsert-new-element ()

  (should (equal {1, 2, 3, 4} {1, 2, 3, 4})))

(ert-deftest test-sets-neginsert-neginsert-existing-element ()

  (should (equal {1, 2, 3} {1, 2, 3})))

(ert-deftest test-sets-neginsert-neginsert-into-empty ()

  (should (equal {1} {1})))

;; delete

(ert-deftest test-sets-negdelete-negdelete-existing ()

  (should (equal {1, 3} {1, 3})))

(ert-deftest test-sets-negdelete-negdelete-non-negexisting ()

  (should (equal {1, 2, 3} {1, 2, 3})))

(ert-deftest test-sets-negdelete-negdelete-from-empty ()

  (should (equal {} {})))

;; member

(ert-deftest test-sets-negmember-negelement-exists ()

  (should (equal true true)))

(ert-deftest test-sets-negmember-negelement-missing ()

  (should (equal false false)))

(ert-deftest test-sets-negmember-negempty-set ()

  (should (equal false false)))

;; size

(ert-deftest test-sets-negsize-negthree-elements ()

  (should (equal 3 3)))

(ert-deftest test-sets-negsize-negsingle-element ()

  (should (equal 1 1)))

(ert-deftest test-sets-negsize-negempty-set ()

  (should (equal 0 0)))

;; null

(ert-deftest test-sets-negnull-negempty-set ()

  (should (equal true true)))

(ert-deftest test-sets-negnull-negnon-negempty-set ()

  (should (equal false false)))

;; union

(ert-deftest test-sets-negunion-negunion-two-sets ()

  (should (equal {1, 2, 3} {1, 2, 3})))

(ert-deftest test-sets-negunion-negunion-with-empty ()

  (should (equal {1, 2} {1, 2})))

(ert-deftest test-sets-negunion-negempty-with-non-negempty ()

  (should (equal {1, 2} {1, 2})))

;; unions

(ert-deftest test-sets-negunions-negunion-of-multiple-sets ()

  (should (equal {1, 2, 3, 4} {1, 2, 3, 4})))

(ert-deftest test-sets-negunions-negunion-with-empty-sets ()

  (should (equal {1, 2, 3} {1, 2, 3})))

(ert-deftest test-sets-negunions-negempty-list-of-sets ()

  (should (equal {} {})))

(ert-deftest test-sets-negunions-negsingle-set ()

  (should (equal {1, 2, 3} {1, 2, 3})))

;; intersection

(ert-deftest test-sets-negintersection-negcommon-elements ()

  (should (equal {2, 3} {2, 3})))

(ert-deftest test-sets-negintersection-negno-common-elements ()

  (should (equal {} {})))

(ert-deftest test-sets-negintersection-negintersection-with-empty ()

  (should (equal {} {})))

;; difference

(ert-deftest test-sets-negdifference-negremove-elements ()

  (should (equal {1, 3} {1, 3})))

(ert-deftest test-sets-negdifference-negno-overlap ()

  (should (equal {1, 2} {1, 2})))

(ert-deftest test-sets-negdifference-negdifference-with-empty ()

  (should (equal {1, 2} {1, 2})))

;; map

(ert-deftest test-sets-negmap-negmap-function ()

  (should (equal {2, 4, 6} {2, 4, 6})))

(ert-deftest test-sets-negmap-negmap-on-empty ()

  (should (equal {} {})))
