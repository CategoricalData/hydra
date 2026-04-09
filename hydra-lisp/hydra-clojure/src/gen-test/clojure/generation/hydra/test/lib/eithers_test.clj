;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.eithers primitives

(ns test-ns
  (:require [clojure.test :refer :all]))

;; bind

(deftest test-eithers-negbind-negbind-right-with-success

  (is (= right(2)

         right(2))))

(deftest test-eithers-negbind-negbind-right-with-failure

  (is (= left(0)

         left(0))))

(deftest test-eithers-negbind-negbind-left-returns-left-unchanged

  (is (= left(42)

         left(42))))

;; bimap

(deftest test-eithers-negbimap-negmap-left-value

  (is (= left(10)

         left(10))))

(deftest test-eithers-negbimap-negmap-right-value

  (is (= right(2)

         right(2))))

;; isLeft

(deftest test-eithers-negisleft-negleft-value

  (is (= true

         true)))

(deftest test-eithers-negisleft-negright-value

  (is (= false

         false)))

;; isRight

(deftest test-eithers-negisright-negright-value

  (is (= true

         true)))

(deftest test-eithers-negisright-negleft-value

  (is (= false

         false)))

;; fromLeft

(deftest test-eithers-negfromleft-negextract-left

  (is (= 42

         42)))

(deftest test-eithers-negfromleft-neguse-default-for-right

  (is (= 99

         99)))

;; fromRight

(deftest test-eithers-negfromright-negextract-right

  (is (= "test"

         "test")))

(deftest test-eithers-negfromright-neguse-default-for-left

  (is (= "default"

         "default")))

;; either

(deftest test-eithers-negeither-negapply-left-function

  (is (= 10

         10)))

(deftest test-eithers-negeither-negapply-right-function

  (is (= 2

         2)))

;; lefts

(deftest test-eithers-neglefts-negfilter-left-values

  (is (= [1, 2]

         [1, 2])))

(deftest test-eithers-neglefts-negall-lefts

  (is (= [1, 2]

         [1, 2])))

(deftest test-eithers-neglefts-negall-rights

  (is (= []

         [])))

(deftest test-eithers-neglefts-negempty-list

  (is (= []

         [])))

;; rights

(deftest test-eithers-negrights-negfilter-right-values

  (is (= ["a", "b"]

         ["a", "b"])))

(deftest test-eithers-negrights-negall-rights

  (is (= ["a", "b"]

         ["a", "b"])))

(deftest test-eithers-negrights-negall-lefts

  (is (= []

         [])))

(deftest test-eithers-negrights-negempty-list

  (is (= []

         [])))

;; partitionEithers

(deftest test-eithers-negpartitioneithers-negpartition-mixed

  (is (= ([1, 2], ["a", "b"])

         ([1, 2], ["a", "b"]))))

(deftest test-eithers-negpartitioneithers-negall-lefts

  (is (= ([1, 2], [])

         ([1, 2], []))))

(deftest test-eithers-negpartitioneithers-negall-rights

  (is (= ([], ["a", "b"])

         ([], ["a", "b"]))))

(deftest test-eithers-negpartitioneithers-negempty-list

  (is (= ([], [])

         ([], []))))

;; map

(deftest test-eithers-negmap-negmap-right-value

  (is (= right(10)

         right(10))))

(deftest test-eithers-negmap-negpreserve-left

  (is (= left(99)

         left(99))))

;; mapList

(deftest test-eithers-negmaplist-negall-succeed

  (is (= right([2, 4, 6])

         right([2, 4, 6]))))

(deftest test-eithers-negmaplist-negfirst-fails

  (is (= left("zero")

         left("zero"))))

(deftest test-eithers-negmaplist-negempty-list

  (is (= right([])

         right([]))))

;; mapMaybe

(deftest test-eithers-negmapmaybe-negjust-succeeds

  (is (= right(just(10))

         right(just(10)))))

(deftest test-eithers-negmapmaybe-negjust-fails

  (is (= left("zero")

         left("zero"))))

(deftest test-eithers-negmapmaybe-negnothing

  (is (= right(nothing)

         right(nothing))))
