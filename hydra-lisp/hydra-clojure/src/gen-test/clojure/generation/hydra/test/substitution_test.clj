;; Note: this is an automatically generated file. Do not edit.
;; substitution

(ns test-ns
  (:require [clojure.test :refer :all]))

;; substInType

(deftest test-substitution-negsubstintype-negempty-substitution-returns-type-unchanged

  (is (= string

         string)))

(deftest test-substitution-negsubstintype-negsubstitute-type-variable-with-int32

  (is (= int32

         int32)))

(deftest test-substitution-negsubstintype-negnon-negmatching-variable-unchanged

  (is (= b

         b)))

(deftest test-substitution-negsubstintype-negsubstitute-in-function-domain

  (is (= (int32 → string)

         (int32 → string))))

(deftest test-substitution-negsubstintype-negsubstitute-in-function-codomain

  (is (= (int32 → string)

         (int32 → string))))

(deftest test-substitution-negsubstintype-negsubstitute-in-list-element-type

  (is (= list<int32>

         list<int32>)))

(deftest test-substitution-negsubstintype-negsubstitute-in-optional-type

  (is (= maybe<string>

         maybe<string>)))

(deftest test-substitution-negsubstintype-negsubstitute-in-pair-type-both-sides

  (is (= (int32, int32)

         (int32, int32))))

(deftest test-substitution-negsubstintype-negsubstitute-in-either-type

  (is (= either<string, int32>

         either<string, int32>)))

(deftest test-substitution-negsubstintype-negsubstitute-in-map-key-type

  (is (= map<string, int32>

         map<string, int32>)))

(deftest test-substitution-negsubstintype-negsubstitute-in-set-type

  (is (= set<int32>

         set<int32>)))

(deftest test-substitution-negsubstintype-negnested-substitution-in-list-of-pairs

  (is (= list<(int32, string)>

         list<(int32, string)>)))

(deftest test-substitution-negsubstintype-negmultiple-substitutions

  (is (= (int32, string)

         (int32, string))))

(deftest test-substitution-negsubstintype-negforall-bound-variable-not-substituted

  (is (= (∀a.(a → a))

         (∀a.(a → a)))))

(deftest test-substitution-negsubstintype-negforall-free-variable-substituted

  (is (= (∀a.(a → string))

         (∀a.(a → string)))))
