;;; Note: this is an automatically generated file. Do not edit. -*- lexical-binding: t; coding: utf-8 -*-
;;; unification

(require 'ert)

;; variableOccursInType

(ert-deftest test-unification-negvariableoccursintype-negvariable-occurs-in-itself ()

  (should (equal true true)))

(ert-deftest test-unification-negvariableoccursintype-negvariable-does-not-occur-in-different-variable ()

  (should (equal false false)))

(ert-deftest test-unification-negvariableoccursintype-negvariable-does-not-occur-in-int32 ()

  (should (equal false false)))

(ert-deftest test-unification-negvariableoccursintype-negvariable-does-not-occur-in-string ()

  (should (equal false false)))

(ert-deftest test-unification-negvariableoccursintype-negvariable-occurs-in-list-element-type ()

  (should (equal true true)))

(ert-deftest test-unification-negvariableoccursintype-negvariable-does-not-occur-in-list-of-different-type ()

  (should (equal false false)))

(ert-deftest test-unification-negvariableoccursintype-negvariable-occurs-in-function-domain ()

  (should (equal true true)))

(ert-deftest test-unification-negvariableoccursintype-negvariable-occurs-in-function-codomain ()

  (should (equal true true)))

(ert-deftest test-unification-negvariableoccursintype-negvariable-does-not-occur-in-function-with-different-vars ()

  (should (equal false false)))

(ert-deftest test-unification-negvariableoccursintype-negvariable-occurs-in-optional-type ()

  (should (equal true true)))

(ert-deftest test-unification-negvariableoccursintype-negvariable-occurs-in-pair-first ()

  (should (equal true true)))

(ert-deftest test-unification-negvariableoccursintype-negvariable-occurs-in-pair-second ()

  (should (equal true true)))

(ert-deftest test-unification-negvariableoccursintype-negvariable-occurs-in-either-left ()

  (should (equal true true)))

(ert-deftest test-unification-negvariableoccursintype-negvariable-occurs-in-either-right ()

  (should (equal true true)))

(ert-deftest test-unification-negvariableoccursintype-negvariable-occurs-in-map-key-type ()

  (should (equal true true)))

(ert-deftest test-unification-negvariableoccursintype-negvariable-occurs-in-map-value-type ()

  (should (equal true true)))

(ert-deftest test-unification-negvariableoccursintype-negvariable-occurs-in-set-type ()

  (should (equal true true)))

(ert-deftest test-unification-negvariableoccursintype-negvariable-occurs-in-nested-list ()

  (should (equal true true)))

(ert-deftest test-unification-negvariableoccursintype-negvariable-occurs-in-list-of-functions ()

  (should (equal true true)))

(ert-deftest test-unification-negvariableoccursintype-negvariable-does-not-occur-in-complex-type-without-it ()

  (should (equal false false)))

(ert-deftest test-unification-negvariableoccursintype-negvariable-occurs-deep-in-complex-type ()

  (should (equal true true)))

(ert-deftest test-unification-negvariableoccursintype-negvariable-occurs-in-forall-body ()

  (should (equal true true)))

(ert-deftest test-unification-negvariableoccursintype-negvariable-occurs-in-forall-bound-position ()

  (should (equal true true)))

;; unifyTypes

(ert-deftest test-unification-negunifytypes-negunify-identical-int32-types ()

  (should (equal {} {})))

(ert-deftest test-unification-negunifytypes-negunify-identical-string-types ()

  (should (equal {} {})))

(ert-deftest test-unification-negunifytypes-negunify-identical-variable-types ()

  (should (equal {} {})))

(ert-deftest test-unification-negunifytypes-negunify-variable-with-int32 ()

  (should (equal {a: int32} {a: int32})))

(ert-deftest test-unification-negunifytypes-negunify-int32-with-variable ()

  (should (equal {a: int32} {a: int32})))

(ert-deftest test-unification-negunifytypes-negunify-two-different-variables ()

  (should (equal {a: b} {a: b})))

(ert-deftest test-unification-negunifytypes-negunify-list-of-variables-with-list-of-int32 ()

  (should (equal {a: int32} {a: int32})))

(ert-deftest test-unification-negunifytypes-negunify-identical-list-types ()

  (should (equal {} {})))

(ert-deftest test-unification-negunifytypes-negunify-function-types-with-variables ()

  (should (equal {a: int32, b: string} {a: int32, b: string})))

(ert-deftest test-unification-negunifytypes-negunify-identical-function-types ()

  (should (equal {} {})))

(ert-deftest test-unification-negunifytypes-negunify-optional-types ()

  (should (equal {a: int32} {a: int32})))

(ert-deftest test-unification-negunifytypes-negunify-pair-types ()

  (should (equal {a: int32, b: string} {a: int32, b: string})))

(ert-deftest test-unification-negunifytypes-negunify-either-types ()

  (should (equal {a: int32, b: string} {a: int32, b: string})))

(ert-deftest test-unification-negunifytypes-negunify-map-types ()

  (should (equal {k: string, v: int32} {k: string, v: int32})))

(ert-deftest test-unification-negunifytypes-negunify-set-types ()

  (should (equal {a: int32} {a: int32})))

(ert-deftest test-unification-negunifytypes-negunify-unit-types ()

  (should (equal {} {})))

(ert-deftest test-unification-negunifytypes-negfail-to-unify-int32-with-string ()

  (should (equal failure failure)))

(ert-deftest test-unification-negunifytypes-negfail-to-unify-list-with-function ()

  (should (equal failure failure)))

(ert-deftest test-unification-negunifytypes-negoccur-check-variable-with-list-containing-it ()

  (should (equal failure failure)))

;; joinTypes

(ert-deftest test-unification-negjointypes-negjoin-identical-int32 ()

  (should (equal [] [])))

(ert-deftest test-unification-negjointypes-negjoin-identical-string ()

  (should (equal [] [])))

(ert-deftest test-unification-negjointypes-negjoin-list-types ()

  (should (equal [(a ~ int32)] [(a ~ int32)])))

(ert-deftest test-unification-negjointypes-negjoin-function-types ()

  (should (equal [(a ~ int32), (b ~ string)] [(a ~ int32), (b ~ string)])))

(ert-deftest test-unification-negjointypes-negjoin-optional-types ()

  (should (equal [(a ~ int32)] [(a ~ int32)])))

(ert-deftest test-unification-negjointypes-negjoin-pair-types ()

  (should (equal [(a ~ int32), (b ~ string)] [(a ~ int32), (b ~ string)])))

(ert-deftest test-unification-negjointypes-negjoin-either-types ()

  (should (equal [(a ~ int32), (b ~ string)] [(a ~ int32), (b ~ string)])))

(ert-deftest test-unification-negjointypes-negjoin-map-types ()

  (should (equal [(k ~ string), (v ~ int32)] [(k ~ string), (v ~ int32)])))

(ert-deftest test-unification-negjointypes-negjoin-set-types ()

  (should (equal [(a ~ int32)] [(a ~ int32)])))

(ert-deftest test-unification-negjointypes-negjoin-unit-types ()

  (should (equal [] [])))

(ert-deftest test-unification-negjointypes-negfail-to-join-int32-with-string ()

  (should (equal failure failure)))

(ert-deftest test-unification-negjointypes-negfail-to-join-list-with-function ()

  (should (equal failure failure)))

(ert-deftest test-unification-negjointypes-negfail-to-join-pair-with-either ()

  (should (equal failure failure)))
