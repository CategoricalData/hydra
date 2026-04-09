;; Note: this is an automatically generated file. Do not edit.
;; strip

(ns test-ns
  (:require [clojure.test :refer :all]))

;; deannotateTerm

(deftest test-strip-negdeannotateterm-negunannotated-literal-unchanged

  (is (= 42:int32

         42:int32)))

(deftest test-strip-negdeannotateterm-negunannotated-variable-unchanged

  (is (= x

         x)))

(deftest test-strip-negdeannotateterm-negunannotated-lambda-unchanged

  (is (= λx.x

         λx.x)))

(deftest test-strip-negdeannotateterm-negsingle-annotation-stripped

  (is (= 42:int32

         42:int32)))

(deftest test-strip-negdeannotateterm-negnested-annotations-stripped

  (is (= 42:int32

         42:int32)))

(deftest test-strip-negdeannotateterm-negannotated-lambda-stripped

  (is (= λx.x

         λx.x)))

(deftest test-strip-negdeannotateterm-negannotated-application-stripped

  (is (= (f @ x)

         (f @ x))))

;; deannotateType

(deftest test-strip-negdeannotatetype-negunannotated-primitive-type-unchanged

  (is (= int32

         int32)))

(deftest test-strip-negdeannotatetype-negunannotated-string-type-unchanged

  (is (= string

         string)))

(deftest test-strip-negdeannotatetype-negunannotated-function-type-unchanged

  (is (= (int32 → string)

         (int32 → string))))

(deftest test-strip-negdeannotatetype-negsingle-annotation-stripped

  (is (= int32

         int32)))

(deftest test-strip-negdeannotatetype-negnested-annotations-stripped

  (is (= string

         string)))

(deftest test-strip-negdeannotatetype-negannotated-list-type-stripped

  (is (= list<int32>

         list<int32>)))

(deftest test-strip-negdeannotatetype-negannotated-function-type-stripped

  (is (= (int32 → string)

         (int32 → string))))
