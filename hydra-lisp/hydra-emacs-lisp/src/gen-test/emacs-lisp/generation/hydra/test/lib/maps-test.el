;;; Note: this is an automatically generated file. Do not edit. -*- lexical-binding: t; coding: utf-8 -*-
;;; hydra.lib.maps primitives

(require 'ert)

;; alter

(ert-deftest test-maps-negalter-neginsert-new-key ()

  (should (equal {1: "a", 2: "b", 3: "new"} {1: "a", 2: "b", 3: "new"})))

(ert-deftest test-maps-negalter-negupdate-existing-key ()

  (should (equal {1: "a", 2: "updated"} {1: "a", 2: "updated"})))

(ert-deftest test-maps-negalter-negdelete-key ()

  (should (equal {1: "a"} {1: "a"})))

;; bimap

(ert-deftest test-maps-negbimap-negtransform-both ()

  (should (equal {2: "A", 4: "B"} {2: "A", 4: "B"})))

(ert-deftest test-maps-negbimap-negempty-map ()

  (should (equal {} {})))

;; elems

(ert-deftest test-maps-negelems-negget-all-elements ()

  (should (equal ["a", "b"] ["a", "b"])))

(ert-deftest test-maps-negelems-negunsorted-keys ()

  (should (equal ["a", "b", "c"] ["a", "b", "c"])))

(ert-deftest test-maps-negelems-negempty-map ()

  (should (equal [] [])))

;; empty

(ert-deftest test-maps-negempty-negempty-map ()

  (should (equal {} {})))

;; filter

(ert-deftest test-maps-negfilter-negfilter-values-starting-with-a ()

  (should (equal {1: "a", 3: "ab"} {1: "a", 3: "ab"})))

(ert-deftest test-maps-negfilter-negfilter-all ()

  (should (equal {} {})))

(ert-deftest test-maps-negfilter-negempty-map ()

  (should (equal {} {})))

;; filterWithKey

(ert-deftest test-maps-negfilterwithkey-negfilter-by-key-1 ()

  (should (equal {2: "b", 3: "c"} {2: "b", 3: "c"})))

(ert-deftest test-maps-negfilterwithkey-negfilter-all ()

  (should (equal {} {})))

(ert-deftest test-maps-negfilterwithkey-negempty-map ()

  (should (equal {} {})))

;; findWithDefault

(ert-deftest test-maps-negfindwithdefault-negfind-existing ()

  (should (equal b b)))

(ert-deftest test-maps-negfindwithdefault-neguse-default ()

  (should (equal default default)))

;; fromList

(ert-deftest test-maps-negfromlist-negcreate-from-pairs ()

  (should (equal {1: "a", 2: "b"} {1: "a", 2: "b"})))

(ert-deftest test-maps-negfromlist-negduplicate-keys ()

  (should (equal {1: "b"} {1: "b"})))

(ert-deftest test-maps-negfromlist-negempty-list ()

  (should (equal {} {})))

;; insert

(ert-deftest test-maps-neginsert-neginsert-new-key ()

  (should (equal {1: "a", 2: "b", 3: "c"} {1: "a", 2: "b", 3: "c"})))

(ert-deftest test-maps-neginsert-negupdate-existing ()

  (should (equal {1: "a", 2: "updated"} {1: "a", 2: "updated"})))

(ert-deftest test-maps-neginsert-neginsert-into-empty ()

  (should (equal {1: "x"} {1: "x"})))

;; keys

(ert-deftest test-maps-negkeys-negget-all-keys ()

  (should (equal [1, 2, 3] [1, 2, 3])))

(ert-deftest test-maps-negkeys-negunsorted-keys ()

  (should (equal [1, 2, 3] [1, 2, 3])))

(ert-deftest test-maps-negkeys-negempty-map ()

  (should (equal [] [])))

;; lookup

(ert-deftest test-maps-neglookup-negfind-existing-key ()

  (should (equal just("b") just("b"))))

(ert-deftest test-maps-neglookup-negkey-not-found ()

  (should (equal nothing nothing)))

(ert-deftest test-maps-neglookup-neglookup-in-empty ()

  (should (equal nothing nothing)))

;; map

(ert-deftest test-maps-negmap-negmap-over-values ()

  (should (equal {1: "A", 2: "B"} {1: "A", 2: "B"})))

(ert-deftest test-maps-negmap-negmap-empty ()

  (should (equal {} {})))

;; mapKeys

(ert-deftest test-maps-negmapkeys-negdouble-keys ()

  (should (equal {2: "a", 4: "b"} {2: "a", 4: "b"})))

(ert-deftest test-maps-negmapkeys-negempty-map ()

  (should (equal {} {})))

;; member

(ert-deftest test-maps-negmember-negkey-exists ()

  (should (equal true true)))

(ert-deftest test-maps-negmember-negkey-missing ()

  (should (equal false false)))

(ert-deftest test-maps-negmember-negempty-map ()

  (should (equal false false)))

;; null

(ert-deftest test-maps-negnull-negempty-map ()

  (should (equal true true)))

(ert-deftest test-maps-negnull-negnon-negempty-map ()

  (should (equal false false)))

;; remove

(ert-deftest test-maps-negremove-negremove-existing ()

  (should (equal {1: "a", 3: "c"} {1: "a", 3: "c"})))

(ert-deftest test-maps-negremove-negremove-non-negexisting ()

  (should (equal {1: "a", 2: "b"} {1: "a", 2: "b"})))

(ert-deftest test-maps-negremove-negremove-from-empty ()

  (should (equal {} {})))

;; singleton

(ert-deftest test-maps-negsingleton-negsingle-entry ()

  (should (equal {42: "hello"} {42: "hello"})))

;; size

(ert-deftest test-maps-negsize-negthree-entries ()

  (should (equal 3 3)))

(ert-deftest test-maps-negsize-negsingle-entry ()

  (should (equal 1 1)))

(ert-deftest test-maps-negsize-negempty-map ()

  (should (equal 0 0)))

;; toList

(ert-deftest test-maps-negtolist-negconvert-to-pairs ()

  (should (equal [(1, "a"), (2, "b")] [(1, "a"), (2, "b")])))

(ert-deftest test-maps-negtolist-negunsorted-keys ()

  (should (equal [(1, "a"), (2, "b"), (3, "c")] [(1, "a"), (2, "b"), (3, "c")])))

(ert-deftest test-maps-negtolist-negempty-map ()

  (should (equal [] [])))

;; union

(ert-deftest test-maps-negunion-negunion-two-maps ()

  (should (equal {1: "a", 2: "b", 3: "c"} {1: "a", 2: "b", 3: "c"})))

(ert-deftest test-maps-negunion-negunion-with-empty ()

  (should (equal {1: "a"} {1: "a"})))

(ert-deftest test-maps-negunion-negempty-with-map ()

  (should (equal {1: "a"} {1: "a"})))
