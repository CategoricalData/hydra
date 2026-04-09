;; Note: this is an automatically generated file. Do not edit.
;; JSON serialization

(ns test-ns
  (:require [clojure.test :refer :all]))

;; primitives

(deftest test-writer-negprimitives-negnull

  (is (= null

         null)))

(deftest test-writer-negprimitives-negtrue

  (is (= true

         true)))

(deftest test-writer-negprimitives-negfalse

  (is (= false

         false)))

(deftest test-writer-negprimitives-negzero

  (is (= 0

         0)))

(deftest test-writer-negprimitives-negpositive-integer

  (is (= 42

         42)))

(deftest test-writer-negprimitives-negnegative-integer

  (is (= -17

         -17)))

(deftest test-writer-negprimitives-neglarge-integer

  (is (= 1000000

         1000000)))

(deftest test-writer-negprimitives-negdecimal

  (is (= 3.14

         3.14)))

(deftest test-writer-negprimitives-negnegative-decimal

  (is (= -2.5

         -2.5)))

(deftest test-writer-negprimitives-negsmall-decimal

  (is (= 1.0e-3

         1.0e-3)))

;; strings

(deftest test-writer-negstrings-negempty-string

  (is (= ""

         "")))

(deftest test-writer-negstrings-negsimple-string

  (is (= "hello"

         "hello")))

(deftest test-writer-negstrings-negstring-with-spaces

  (is (= "hello world"

         "hello world")))

(deftest test-writer-negstrings-negstring-with-double-quote

  (is (= "say \"hi\""

         "say \"hi\"")))

(deftest test-writer-negstrings-negstring-with-backslash

  (is (= "path\\to\\file"

         "path\\to\\file")))

(deftest test-writer-negstrings-negstring-with-newline

  (is (= "line1\nline2"

         "line1\nline2")))

(deftest test-writer-negstrings-negstring-with-carriage-return

  (is (= "line1\rline2"

         "line1\rline2")))

(deftest test-writer-negstrings-negstring-with-tab

  (is (= "col1\tcol2"

         "col1\tcol2")))

(deftest test-writer-negstrings-negstring-with-mixed-escapes

  (is (= "a\"b\\c\nd"

         "a\"b\\c\nd")))

;; arrays

(deftest test-writer-negarrays-negempty-array

  (is (= []

         [])))

(deftest test-writer-negarrays-negsingle-element

  (is (= [1]

         [1])))

(deftest test-writer-negarrays-negmultiple-numbers

  (is (= [1, 2, 3]

         [1, 2, 3])))

(deftest test-writer-negarrays-negmultiple-strings

  (is (= ["a", "b"]

         ["a", "b"])))

(deftest test-writer-negarrays-negmixed-types

  (is (= [1, "two", true, null]

         [1, "two", true, null])))

;; objects

(deftest test-writer-negobjects-negempty-object

  (is (= {}

         {})))

(deftest test-writer-negobjects-negsingle-key-negvalue

  (is (= {"name": "Alice"}

         {"name": "Alice"})))

(deftest test-writer-negobjects-negmultiple-keys

  (is (= {"a": 1, "b": 2}

         {"a": 1, "b": 2})))

(deftest test-writer-negobjects-negmixed-value-types

  (is (= {"active": true, "count": 42, "name": "test"}

         {"active": true, "count": 42, "name": "test"})))

;; nested structures

(deftest test-writer-negnested-structures-negnested-arrays

  (is (= [[1, 2], [3, 4]]

         [[1, 2], [3, 4]])))

(deftest test-writer-negnested-structures-negobject-with-array

  (is (= {"items": [1, 2]}

         {"items": [1, 2]})))

(deftest test-writer-negnested-structures-negarray-of-objects

  (is (= [{"id": 1}, {"id": 2}]

         [{"id": 1}, {"id": 2}])))

(deftest test-writer-negnested-structures-negnested-object

  (is (= {"user": {"name": "Bob"}}

         {"user": {"name": "Bob"}})))
