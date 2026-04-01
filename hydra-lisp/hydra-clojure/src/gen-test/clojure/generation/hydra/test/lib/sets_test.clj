;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.sets primitives

(ns test-ns
  (:require [clojure.test :refer :all]))

;; empty

(deftest test-sets-negempty-negempty-set

  (is (= {}

         {})))

;; singleton

(deftest test-sets-negsingleton-negsingle-element

  (is (= {42}

         {42})))

;; fromList

(deftest test-sets-negfromlist-negcreate-from-list

  (is (= {1, 2, 3}

         {1, 2, 3})))

(deftest test-sets-negfromlist-negduplicates-removed

  (is (= {1, 2, 3}

         {1, 2, 3})))

(deftest test-sets-negfromlist-negempty-list

  (is (= {}

         {})))

;; toList

(deftest test-sets-negtolist-negconvert-to-list

  (is (= [1, 2, 3]

         [1, 2, 3])))

(deftest test-sets-negtolist-negunsorted-input

  (is (= [1, 2, 3]

         [1, 2, 3])))

(deftest test-sets-negtolist-negempty-set

  (is (= []

         [])))

;; insert

(deftest test-sets-neginsert-neginsert-new-element

  (is (= {1, 2, 3, 4}

         {1, 2, 3, 4})))

(deftest test-sets-neginsert-neginsert-existing-element

  (is (= {1, 2, 3}

         {1, 2, 3})))

(deftest test-sets-neginsert-neginsert-into-empty

  (is (= {1}

         {1})))

;; delete

(deftest test-sets-negdelete-negdelete-existing

  (is (= {1, 3}

         {1, 3})))

(deftest test-sets-negdelete-negdelete-non-negexisting

  (is (= {1, 2, 3}

         {1, 2, 3})))

(deftest test-sets-negdelete-negdelete-from-empty

  (is (= {}

         {})))

;; member

(deftest test-sets-negmember-negelement-exists

  (is (= true

         true)))

(deftest test-sets-negmember-negelement-missing

  (is (= false

         false)))

(deftest test-sets-negmember-negempty-set

  (is (= false

         false)))

;; size

(deftest test-sets-negsize-negthree-elements

  (is (= 3

         3)))

(deftest test-sets-negsize-negsingle-element

  (is (= 1

         1)))

(deftest test-sets-negsize-negempty-set

  (is (= 0

         0)))

;; null

(deftest test-sets-negnull-negempty-set

  (is (= true

         true)))

(deftest test-sets-negnull-negnon-negempty-set

  (is (= false

         false)))

;; union

(deftest test-sets-negunion-negunion-two-sets

  (is (= {1, 2, 3}

         {1, 2, 3})))

(deftest test-sets-negunion-negunion-with-empty

  (is (= {1, 2}

         {1, 2})))

(deftest test-sets-negunion-negempty-with-non-negempty

  (is (= {1, 2}

         {1, 2})))

;; unions

(deftest test-sets-negunions-negunion-of-multiple-sets

  (is (= {1, 2, 3, 4}

         {1, 2, 3, 4})))

(deftest test-sets-negunions-negunion-with-empty-sets

  (is (= {1, 2, 3}

         {1, 2, 3})))

(deftest test-sets-negunions-negempty-list-of-sets

  (is (= {}

         {})))

(deftest test-sets-negunions-negsingle-set

  (is (= {1, 2, 3}

         {1, 2, 3})))

;; intersection

(deftest test-sets-negintersection-negcommon-elements

  (is (= {2, 3}

         {2, 3})))

(deftest test-sets-negintersection-negno-common-elements

  (is (= {}

         {})))

(deftest test-sets-negintersection-negintersection-with-empty

  (is (= {}

         {})))

;; difference

(deftest test-sets-negdifference-negremove-elements

  (is (= {1, 3}

         {1, 3})))

(deftest test-sets-negdifference-negno-overlap

  (is (= {1, 2}

         {1, 2})))

(deftest test-sets-negdifference-negdifference-with-empty

  (is (= {1, 2}

         {1, 2})))

;; map

(deftest test-sets-negmap-negmap-function

  (is (= {2, 4, 6}

         {2, 4, 6})))

(deftest test-sets-negmap-negmap-on-empty

  (is (= {}

         {})))
