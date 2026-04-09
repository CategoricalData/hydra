;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.maps primitives

(ns test-ns
  (:require [clojure.test :refer :all]))

;; alter

(deftest test-maps-negalter-neginsert-new-key

  (is (= {1: "a", 2: "b", 3: "new"}

         {1: "a", 2: "b", 3: "new"})))

(deftest test-maps-negalter-negupdate-existing-key

  (is (= {1: "a", 2: "updated"}

         {1: "a", 2: "updated"})))

(deftest test-maps-negalter-negdelete-key

  (is (= {1: "a"}

         {1: "a"})))

;; bimap

(deftest test-maps-negbimap-negtransform-both

  (is (= {2: "A", 4: "B"}

         {2: "A", 4: "B"})))

(deftest test-maps-negbimap-negempty-map

  (is (= {}

         {})))

;; elems

(deftest test-maps-negelems-negget-all-elements

  (is (= ["a", "b"]

         ["a", "b"])))

(deftest test-maps-negelems-negunsorted-keys

  (is (= ["a", "b", "c"]

         ["a", "b", "c"])))

(deftest test-maps-negelems-negempty-map

  (is (= []

         [])))

;; empty

(deftest test-maps-negempty-negempty-map

  (is (= {}

         {})))

;; filter

(deftest test-maps-negfilter-negfilter-values-starting-with-a

  (is (= {1: "a", 3: "ab"}

         {1: "a", 3: "ab"})))

(deftest test-maps-negfilter-negfilter-all

  (is (= {}

         {})))

(deftest test-maps-negfilter-negempty-map

  (is (= {}

         {})))

;; filterWithKey

(deftest test-maps-negfilterwithkey-negfilter-by-key-1

  (is (= {2: "b", 3: "c"}

         {2: "b", 3: "c"})))

(deftest test-maps-negfilterwithkey-negfilter-all

  (is (= {}

         {})))

(deftest test-maps-negfilterwithkey-negempty-map

  (is (= {}

         {})))

;; findWithDefault

(deftest test-maps-negfindwithdefault-negfind-existing

  (is (= b

         b)))

(deftest test-maps-negfindwithdefault-neguse-default

  (is (= default

         default)))

;; fromList

(deftest test-maps-negfromlist-negcreate-from-pairs

  (is (= {1: "a", 2: "b"}

         {1: "a", 2: "b"})))

(deftest test-maps-negfromlist-negduplicate-keys

  (is (= {1: "b"}

         {1: "b"})))

(deftest test-maps-negfromlist-negempty-list

  (is (= {}

         {})))

;; insert

(deftest test-maps-neginsert-neginsert-new-key

  (is (= {1: "a", 2: "b", 3: "c"}

         {1: "a", 2: "b", 3: "c"})))

(deftest test-maps-neginsert-negupdate-existing

  (is (= {1: "a", 2: "updated"}

         {1: "a", 2: "updated"})))

(deftest test-maps-neginsert-neginsert-into-empty

  (is (= {1: "x"}

         {1: "x"})))

;; keys

(deftest test-maps-negkeys-negget-all-keys

  (is (= [1, 2, 3]

         [1, 2, 3])))

(deftest test-maps-negkeys-negunsorted-keys

  (is (= [1, 2, 3]

         [1, 2, 3])))

(deftest test-maps-negkeys-negempty-map

  (is (= []

         [])))

;; lookup

(deftest test-maps-neglookup-negfind-existing-key

  (is (= just("b")

         just("b"))))

(deftest test-maps-neglookup-negkey-not-found

  (is (= nothing

         nothing)))

(deftest test-maps-neglookup-neglookup-in-empty

  (is (= nothing

         nothing)))

;; map

(deftest test-maps-negmap-negmap-over-values

  (is (= {1: "A", 2: "B"}

         {1: "A", 2: "B"})))

(deftest test-maps-negmap-negmap-empty

  (is (= {}

         {})))

;; mapKeys

(deftest test-maps-negmapkeys-negdouble-keys

  (is (= {2: "a", 4: "b"}

         {2: "a", 4: "b"})))

(deftest test-maps-negmapkeys-negempty-map

  (is (= {}

         {})))

;; member

(deftest test-maps-negmember-negkey-exists

  (is (= true

         true)))

(deftest test-maps-negmember-negkey-missing

  (is (= false

         false)))

(deftest test-maps-negmember-negempty-map

  (is (= false

         false)))

;; null

(deftest test-maps-negnull-negempty-map

  (is (= true

         true)))

(deftest test-maps-negnull-negnon-negempty-map

  (is (= false

         false)))

;; remove

(deftest test-maps-negremove-negremove-existing

  (is (= {1: "a", 3: "c"}

         {1: "a", 3: "c"})))

(deftest test-maps-negremove-negremove-non-negexisting

  (is (= {1: "a", 2: "b"}

         {1: "a", 2: "b"})))

(deftest test-maps-negremove-negremove-from-empty

  (is (= {}

         {})))

;; singleton

(deftest test-maps-negsingleton-negsingle-entry

  (is (= {42: "hello"}

         {42: "hello"})))

;; size

(deftest test-maps-negsize-negthree-entries

  (is (= 3

         3)))

(deftest test-maps-negsize-negsingle-entry

  (is (= 1

         1)))

(deftest test-maps-negsize-negempty-map

  (is (= 0

         0)))

;; toList

(deftest test-maps-negtolist-negconvert-to-pairs

  (is (= [(1, "a"), (2, "b")]

         [(1, "a"), (2, "b")])))

(deftest test-maps-negtolist-negunsorted-keys

  (is (= [(1, "a"), (2, "b"), (3, "c")]

         [(1, "a"), (2, "b"), (3, "c")])))

(deftest test-maps-negtolist-negempty-map

  (is (= []

         [])))

;; union

(deftest test-maps-negunion-negunion-two-maps

  (is (= {1: "a", 2: "b", 3: "c"}

         {1: "a", 2: "b", 3: "c"})))

(deftest test-maps-negunion-negunion-with-empty

  (is (= {1: "a"}

         {1: "a"})))

(deftest test-maps-negunion-negempty-with-map

  (is (= {1: "a"}

         {1: "a"})))
