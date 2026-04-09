;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.equality primitives

(ns test-ns
  (:require [clojure.test :refer :all]))

;; compare

(deftest test-equality-negcompare-negless-than

  (is (= inject(hydra.util.Comparison){lessThan=unit}

         inject(hydra.util.Comparison){lessThan=unit})))

(deftest test-equality-negcompare-negequal

  (is (= inject(hydra.util.Comparison){equalTo=unit}

         inject(hydra.util.Comparison){equalTo=unit})))

(deftest test-equality-negcompare-neggreater-than

  (is (= inject(hydra.util.Comparison){greaterThan=unit}

         inject(hydra.util.Comparison){greaterThan=unit})))

;; equal

(deftest test-equality-negequal-negequal-integers

  (is (= true

         true)))

(deftest test-equality-negequal-negunequal-integers

  (is (= false

         false)))

;; gt

(deftest test-equality-neggt-neggreater

  (is (= true

         true)))

(deftest test-equality-neggt-negequal

  (is (= false

         false)))

(deftest test-equality-neggt-negless

  (is (= false

         false)))

;; gte

(deftest test-equality-neggte-neggreater

  (is (= true

         true)))

(deftest test-equality-neggte-negequal

  (is (= true

         true)))

(deftest test-equality-neggte-negless

  (is (= false

         false)))

;; identity

(deftest test-equality-negidentity-neginteger

  (is (= 42:int32

         42:int32)))

;; lt

(deftest test-equality-neglt-negless

  (is (= true

         true)))

(deftest test-equality-neglt-negequal

  (is (= false

         false)))

(deftest test-equality-neglt-neggreater

  (is (= false

         false)))

;; lte

(deftest test-equality-neglte-negless

  (is (= true

         true)))

(deftest test-equality-neglte-negequal

  (is (= true

         true)))

(deftest test-equality-neglte-neggreater

  (is (= false

         false)))

;; max

(deftest test-equality-negmax-negfirst-greater

  (is (= 5:int32

         5:int32)))

(deftest test-equality-negmax-negsecond-greater

  (is (= 5:int32

         5:int32)))

(deftest test-equality-negmax-negequal

  (is (= 5:int32

         5:int32)))

;; min

(deftest test-equality-negmin-negfirst-less

  (is (= 3:int32

         3:int32)))

(deftest test-equality-negmin-negsecond-less

  (is (= 3:int32

         3:int32)))

(deftest test-equality-negmin-negequal

  (is (= 5:int32

         5:int32)))

;; compare strings

(deftest test-equality-negcompare-strings-negless-than-lexicographic

  (is (= inject(hydra.util.Comparison){lessThan=unit}

         inject(hydra.util.Comparison){lessThan=unit})))

(deftest test-equality-negcompare-strings-negequal

  (is (= inject(hydra.util.Comparison){equalTo=unit}

         inject(hydra.util.Comparison){equalTo=unit})))

(deftest test-equality-negcompare-strings-neggreater-than-lexicographic

  (is (= inject(hydra.util.Comparison){greaterThan=unit}

         inject(hydra.util.Comparison){greaterThan=unit})))

(deftest test-equality-negcompare-strings-negempty-vs-non-negempty

  (is (= inject(hydra.util.Comparison){lessThan=unit}

         inject(hydra.util.Comparison){lessThan=unit})))

(deftest test-equality-negcompare-strings-negprefix-vs-longer

  (is (= inject(hydra.util.Comparison){lessThan=unit}

         inject(hydra.util.Comparison){lessThan=unit})))

;; lt strings

(deftest test-equality-neglt-strings-negless-lexicographic

  (is (= true

         true)))

(deftest test-equality-neglt-strings-negequal

  (is (= false

         false)))

(deftest test-equality-neglt-strings-neggreater

  (is (= false

         false)))

;; gt strings

(deftest test-equality-neggt-strings-neggreater-lexicographic

  (is (= true

         true)))

(deftest test-equality-neggt-strings-negequal

  (is (= false

         false)))

(deftest test-equality-neggt-strings-negless

  (is (= false

         false)))

;; max strings

(deftest test-equality-negmax-strings-negfirst-greater

  (is (= "zebra"

         "zebra")))

(deftest test-equality-negmax-strings-negsecond-greater

  (is (= "zebra"

         "zebra")))

(deftest test-equality-negmax-strings-negequal

  (is (= "hello"

         "hello")))

;; min strings

(deftest test-equality-negmin-strings-negfirst-less

  (is (= "apple"

         "apple")))

(deftest test-equality-negmin-strings-negsecond-less

  (is (= "apple"

         "apple")))

(deftest test-equality-negmin-strings-negequal

  (is (= "hello"

         "hello")))

;; compare floats

(deftest test-equality-negcompare-floats-negless-than

  (is (= inject(hydra.util.Comparison){lessThan=unit}

         inject(hydra.util.Comparison){lessThan=unit})))

(deftest test-equality-negcompare-floats-negequal

  (is (= inject(hydra.util.Comparison){equalTo=unit}

         inject(hydra.util.Comparison){equalTo=unit})))

(deftest test-equality-negcompare-floats-neggreater-than

  (is (= inject(hydra.util.Comparison){greaterThan=unit}

         inject(hydra.util.Comparison){greaterThan=unit})))

(deftest test-equality-negcompare-floats-negnegative-vs-positive

  (is (= inject(hydra.util.Comparison){lessThan=unit}

         inject(hydra.util.Comparison){lessThan=unit})))

;; lt floats

(deftest test-equality-neglt-floats-negless

  (is (= true

         true)))

(deftest test-equality-neglt-floats-negequal

  (is (= false

         false)))

(deftest test-equality-neglt-floats-neggreater

  (is (= false

         false)))

;; gt floats

(deftest test-equality-neggt-floats-neggreater

  (is (= true

         true)))

(deftest test-equality-neggt-floats-negequal

  (is (= false

         false)))

(deftest test-equality-neggt-floats-negless

  (is (= false

         false)))
