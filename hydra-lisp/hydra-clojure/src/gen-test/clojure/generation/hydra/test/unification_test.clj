;; Note: this is an automatically generated file. Do not edit.
;; unification

(ns test-ns
  (:require [clojure.test :refer :all]))

;; variableOccursInType

(deftest test-unification-negvariableoccursintype-negvariable-occurs-in-itself

  (is (= true

         true)))

(deftest test-unification-negvariableoccursintype-negvariable-does-not-occur-in-different-variable

  (is (= false

         false)))

(deftest test-unification-negvariableoccursintype-negvariable-does-not-occur-in-int32

  (is (= false

         false)))

(deftest test-unification-negvariableoccursintype-negvariable-does-not-occur-in-string

  (is (= false

         false)))

(deftest test-unification-negvariableoccursintype-negvariable-occurs-in-list-element-type

  (is (= true

         true)))

(deftest test-unification-negvariableoccursintype-negvariable-does-not-occur-in-list-of-different-type

  (is (= false

         false)))

(deftest test-unification-negvariableoccursintype-negvariable-occurs-in-function-domain

  (is (= true

         true)))

(deftest test-unification-negvariableoccursintype-negvariable-occurs-in-function-codomain

  (is (= true

         true)))

(deftest test-unification-negvariableoccursintype-negvariable-does-not-occur-in-function-with-different-vars

  (is (= false

         false)))

(deftest test-unification-negvariableoccursintype-negvariable-occurs-in-optional-type

  (is (= true

         true)))

(deftest test-unification-negvariableoccursintype-negvariable-occurs-in-pair-first

  (is (= true

         true)))

(deftest test-unification-negvariableoccursintype-negvariable-occurs-in-pair-second

  (is (= true

         true)))

(deftest test-unification-negvariableoccursintype-negvariable-occurs-in-either-left

  (is (= true

         true)))

(deftest test-unification-negvariableoccursintype-negvariable-occurs-in-either-right

  (is (= true

         true)))

(deftest test-unification-negvariableoccursintype-negvariable-occurs-in-map-key-type

  (is (= true

         true)))

(deftest test-unification-negvariableoccursintype-negvariable-occurs-in-map-value-type

  (is (= true

         true)))

(deftest test-unification-negvariableoccursintype-negvariable-occurs-in-set-type

  (is (= true

         true)))

(deftest test-unification-negvariableoccursintype-negvariable-occurs-in-nested-list

  (is (= true

         true)))

(deftest test-unification-negvariableoccursintype-negvariable-occurs-in-list-of-functions

  (is (= true

         true)))

(deftest test-unification-negvariableoccursintype-negvariable-does-not-occur-in-complex-type-without-it

  (is (= false

         false)))

(deftest test-unification-negvariableoccursintype-negvariable-occurs-deep-in-complex-type

  (is (= true

         true)))

(deftest test-unification-negvariableoccursintype-negvariable-occurs-in-forall-body

  (is (= true

         true)))

(deftest test-unification-negvariableoccursintype-negvariable-occurs-in-forall-bound-position

  (is (= true

         true)))

;; unifyTypes

(deftest test-unification-negunifytypes-negunify-identical-int32-types

  (is (= {}

         {})))

(deftest test-unification-negunifytypes-negunify-identical-string-types

  (is (= {}

         {})))

(deftest test-unification-negunifytypes-negunify-identical-variable-types

  (is (= {}

         {})))

(deftest test-unification-negunifytypes-negunify-variable-with-int32

  (is (= {a: int32}

         {a: int32})))

(deftest test-unification-negunifytypes-negunify-int32-with-variable

  (is (= {a: int32}

         {a: int32})))

(deftest test-unification-negunifytypes-negunify-two-different-variables

  (is (= {a: b}

         {a: b})))

(deftest test-unification-negunifytypes-negunify-list-of-variables-with-list-of-int32

  (is (= {a: int32}

         {a: int32})))

(deftest test-unification-negunifytypes-negunify-identical-list-types

  (is (= {}

         {})))

(deftest test-unification-negunifytypes-negunify-function-types-with-variables

  (is (= {a: int32, b: string}

         {a: int32, b: string})))

(deftest test-unification-negunifytypes-negunify-identical-function-types

  (is (= {}

         {})))

(deftest test-unification-negunifytypes-negunify-optional-types

  (is (= {a: int32}

         {a: int32})))

(deftest test-unification-negunifytypes-negunify-pair-types

  (is (= {a: int32, b: string}

         {a: int32, b: string})))

(deftest test-unification-negunifytypes-negunify-either-types

  (is (= {a: int32, b: string}

         {a: int32, b: string})))

(deftest test-unification-negunifytypes-negunify-map-types

  (is (= {k: string, v: int32}

         {k: string, v: int32})))

(deftest test-unification-negunifytypes-negunify-set-types

  (is (= {a: int32}

         {a: int32})))

(deftest test-unification-negunifytypes-negunify-unit-types

  (is (= {}

         {})))

(deftest test-unification-negunifytypes-negfail-to-unify-int32-with-string

  (is (= failure

         failure)))

(deftest test-unification-negunifytypes-negfail-to-unify-list-with-function

  (is (= failure

         failure)))

(deftest test-unification-negunifytypes-negoccur-check-variable-with-list-containing-it

  (is (= failure

         failure)))

;; joinTypes

(deftest test-unification-negjointypes-negjoin-identical-int32

  (is (= []

         [])))

(deftest test-unification-negjointypes-negjoin-identical-string

  (is (= []

         [])))

(deftest test-unification-negjointypes-negjoin-list-types

  (is (= [(a ~ int32)]

         [(a ~ int32)])))

(deftest test-unification-negjointypes-negjoin-function-types

  (is (= [(a ~ int32), (b ~ string)]

         [(a ~ int32), (b ~ string)])))

(deftest test-unification-negjointypes-negjoin-optional-types

  (is (= [(a ~ int32)]

         [(a ~ int32)])))

(deftest test-unification-negjointypes-negjoin-pair-types

  (is (= [(a ~ int32), (b ~ string)]

         [(a ~ int32), (b ~ string)])))

(deftest test-unification-negjointypes-negjoin-either-types

  (is (= [(a ~ int32), (b ~ string)]

         [(a ~ int32), (b ~ string)])))

(deftest test-unification-negjointypes-negjoin-map-types

  (is (= [(k ~ string), (v ~ int32)]

         [(k ~ string), (v ~ int32)])))

(deftest test-unification-negjointypes-negjoin-set-types

  (is (= [(a ~ int32)]

         [(a ~ int32)])))

(deftest test-unification-negjointypes-negjoin-unit-types

  (is (= []

         [])))

(deftest test-unification-negjointypes-negfail-to-join-int32-with-string

  (is (= failure

         failure)))

(deftest test-unification-negjointypes-negfail-to-join-list-with-function

  (is (= failure

         failure)))

(deftest test-unification-negjointypes-negfail-to-join-pair-with-either

  (is (= failure

         failure)))
