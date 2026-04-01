;;; Note: this is an automatically generated file. Do not edit. -*- lexical-binding: t; coding: utf-8 -*-
;;; JSON serialization

(require 'ert)

;; primitives

(ert-deftest test-writer-negprimitives-negnull ()

  (should (equal null null)))

(ert-deftest test-writer-negprimitives-negtrue ()

  (should (equal true true)))

(ert-deftest test-writer-negprimitives-negfalse ()

  (should (equal false false)))

(ert-deftest test-writer-negprimitives-negzero ()

  (should (equal 0 0)))

(ert-deftest test-writer-negprimitives-negpositive-integer ()

  (should (equal 42 42)))

(ert-deftest test-writer-negprimitives-negnegative-integer ()

  (should (equal -17 -17)))

(ert-deftest test-writer-negprimitives-neglarge-integer ()

  (should (equal 1000000 1000000)))

(ert-deftest test-writer-negprimitives-negdecimal ()

  (should (equal 3.14 3.14)))

(ert-deftest test-writer-negprimitives-negnegative-decimal ()

  (should (equal -2.5 -2.5)))

(ert-deftest test-writer-negprimitives-negsmall-decimal ()

  (should (equal 1.0e-3 1.0e-3)))

;; strings

(ert-deftest test-writer-negstrings-negempty-string ()

  (should (equal "" "")))

(ert-deftest test-writer-negstrings-negsimple-string ()

  (should (equal "hello" "hello")))

(ert-deftest test-writer-negstrings-negstring-with-spaces ()

  (should (equal "hello world" "hello world")))

(ert-deftest test-writer-negstrings-negstring-with-double-quote ()

  (should (equal "say \"hi\"" "say \"hi\"")))

(ert-deftest test-writer-negstrings-negstring-with-backslash ()

  (should (equal "path\\to\\file" "path\\to\\file")))

(ert-deftest test-writer-negstrings-negstring-with-newline ()

  (should (equal "line1\nline2" "line1\nline2")))

(ert-deftest test-writer-negstrings-negstring-with-carriage-return ()

  (should (equal "line1\rline2" "line1\rline2")))

(ert-deftest test-writer-negstrings-negstring-with-tab ()

  (should (equal "col1\tcol2" "col1\tcol2")))

(ert-deftest test-writer-negstrings-negstring-with-mixed-escapes ()

  (should (equal "a\"b\\c\nd" "a\"b\\c\nd")))

;; arrays

(ert-deftest test-writer-negarrays-negempty-array ()

  (should (equal [] [])))

(ert-deftest test-writer-negarrays-negsingle-element ()

  (should (equal [1] [1])))

(ert-deftest test-writer-negarrays-negmultiple-numbers ()

  (should (equal [1, 2, 3] [1, 2, 3])))

(ert-deftest test-writer-negarrays-negmultiple-strings ()

  (should (equal ["a", "b"] ["a", "b"])))

(ert-deftest test-writer-negarrays-negmixed-types ()

  (should (equal [1, "two", true, null] [1, "two", true, null])))

;; objects

(ert-deftest test-writer-negobjects-negempty-object ()

  (should (equal {} {})))

(ert-deftest test-writer-negobjects-negsingle-key-negvalue ()

  (should (equal {"name": "Alice"} {"name": "Alice"})))

(ert-deftest test-writer-negobjects-negmultiple-keys ()

  (should (equal {"a": 1, "b": 2} {"a": 1, "b": 2})))

(ert-deftest test-writer-negobjects-negmixed-value-types ()

  (should (equal {"active": true, "count": 42, "name": "test"} {"active": true, "count": 42, "name": "test"})))

;; nested structures

(ert-deftest test-writer-negnested-structures-negnested-arrays ()

  (should (equal [[1, 2], [3, 4]] [[1, 2], [3, 4]])))

(ert-deftest test-writer-negnested-structures-negobject-with-array ()

  (should (equal {"items": [1, 2]} {"items": [1, 2]})))

(ert-deftest test-writer-negnested-structures-negarray-of-objects ()

  (should (equal [{"id": 1}, {"id": 2}] [{"id": 1}, {"id": 2}])))

(ert-deftest test-writer-negnested-structures-negnested-object ()

  (should (equal {"user": {"name": "Bob"}} {"user": {"name": "Bob"}})))
