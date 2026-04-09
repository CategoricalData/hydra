;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.regex primitives

(ns test-ns
  (:require [clojure.test :refer :all]))

;; matches

(deftest test-regex-negmatches-negexact-match

  (is (= true

         true)))

(deftest test-regex-negmatches-negpattern-match

  (is (= true

         true)))

(deftest test-regex-negmatches-negno-match

  (is (= false

         false)))

(deftest test-regex-negmatches-negpartial-content-does-not-match

  (is (= false

         false)))

(deftest test-regex-negmatches-negdigit-pattern

  (is (= true

         true)))

(deftest test-regex-negmatches-negmixed-pattern

  (is (= true

         true)))

(deftest test-regex-negmatches-negempty-pattern-matches-empty

  (is (= true

         true)))

(deftest test-regex-negmatches-negempty-pattern-does-not-match-non-negempty

  (is (= false

         false)))

(deftest test-regex-negmatches-negstar-matches-empty

  (is (= true

         true)))

(deftest test-regex-negmatches-negalternation

  (is (= true

         true)))

(deftest test-regex-negmatches-negalternation-second

  (is (= true

         true)))

(deftest test-regex-negmatches-negalternation-no-match

  (is (= false

         false)))

(deftest test-regex-negmatches-negquantifier

  (is (= true

         true)))

(deftest test-regex-negmatches-negquantifier-with-optional

  (is (= true

         true)))

;; find

(deftest test-regex-negfind-negsimple-find

  (is (= just("123")

         just("123"))))

(deftest test-regex-negfind-negno-match

  (is (= nothing

         nothing)))

(deftest test-regex-negfind-negfind-first

  (is (= just("abc")

         just("abc"))))

(deftest test-regex-negfind-negempty-input

  (is (= nothing

         nothing)))

(deftest test-regex-negfind-negfull-match

  (is (= just("hello")

         just("hello"))))

;; findAll

(deftest test-regex-negfindall-negmultiple-matches

  (is (= ["1", "2", "3"]

         ["1", "2", "3"])))

(deftest test-regex-negfindall-negno-matches

  (is (= []

         [])))

(deftest test-regex-negfindall-negoverlapping-words

  (is (= ["abc", "def", "ghi"]

         ["abc", "def", "ghi"])))

(deftest test-regex-negfindall-negsingle-match

  (is (= ["hello"]

         ["hello"])))

;; replace

(deftest test-regex-negreplace-negbasic-replace

  (is (= "abcXdef456"

         "abcXdef456")))

(deftest test-regex-negreplace-negno-match

  (is (= "abcdef"

         "abcdef")))

(deftest test-regex-negreplace-negreplace-at-start

  (is (= "X123"

         "X123")))

(deftest test-regex-negreplace-negempty-replacement

  (is (= "abcdef"

         "abcdef")))

;; replaceAll

(deftest test-regex-negreplaceall-negreplace-all-digits

  (is (= "aXbXcX"

         "aXbXcX")))

(deftest test-regex-negreplaceall-negno-match

  (is (= "abc"

         "abc")))

(deftest test-regex-negreplaceall-negreplace-all-words

  (is (= "X 123 X"

         "X 123 X")))

(deftest test-regex-negreplaceall-negempty-replacement

  (is (= "abc"

         "abc")))

;; split

(deftest test-regex-negsplit-negsplit-on-comma

  (is (= ["a", "b", "c"]

         ["a", "b", "c"])))

(deftest test-regex-negsplit-negsplit-on-spaces

  (is (= ["a", "b", "c"]

         ["a", "b", "c"])))

(deftest test-regex-negsplit-negno-match

  (is (= ["abc"]

         ["abc"])))

(deftest test-regex-negsplit-negsplit-on-digits

  (is (= ["a", "b", "c"]

         ["a", "b", "c"])))

(deftest test-regex-negsplit-negtrailing-delimiter

  (is (= ["a", "b", ""]

         ["a", "b", ""])))
