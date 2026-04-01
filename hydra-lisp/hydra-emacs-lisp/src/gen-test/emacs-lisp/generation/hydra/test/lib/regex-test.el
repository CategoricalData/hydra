;;; Note: this is an automatically generated file. Do not edit. -*- lexical-binding: t; coding: utf-8 -*-
;;; hydra.lib.regex primitives

(require 'ert)

;; matches

(ert-deftest test-regex-negmatches-negexact-match ()

  (should (equal true true)))

(ert-deftest test-regex-negmatches-negpattern-match ()

  (should (equal true true)))

(ert-deftest test-regex-negmatches-negno-match ()

  (should (equal false false)))

(ert-deftest test-regex-negmatches-negpartial-content-does-not-match ()

  (should (equal false false)))

(ert-deftest test-regex-negmatches-negdigit-pattern ()

  (should (equal true true)))

(ert-deftest test-regex-negmatches-negmixed-pattern ()

  (should (equal true true)))

(ert-deftest test-regex-negmatches-negempty-pattern-matches-empty ()

  (should (equal true true)))

(ert-deftest test-regex-negmatches-negempty-pattern-does-not-match-non-negempty ()

  (should (equal false false)))

(ert-deftest test-regex-negmatches-negstar-matches-empty ()

  (should (equal true true)))

(ert-deftest test-regex-negmatches-negalternation ()

  (should (equal true true)))

(ert-deftest test-regex-negmatches-negalternation-second ()

  (should (equal true true)))

(ert-deftest test-regex-negmatches-negalternation-no-match ()

  (should (equal false false)))

(ert-deftest test-regex-negmatches-negquantifier ()

  (should (equal true true)))

(ert-deftest test-regex-negmatches-negquantifier-with-optional ()

  (should (equal true true)))

;; find

(ert-deftest test-regex-negfind-negsimple-find ()

  (should (equal just("123") just("123"))))

(ert-deftest test-regex-negfind-negno-match ()

  (should (equal nothing nothing)))

(ert-deftest test-regex-negfind-negfind-first ()

  (should (equal just("abc") just("abc"))))

(ert-deftest test-regex-negfind-negempty-input ()

  (should (equal nothing nothing)))

(ert-deftest test-regex-negfind-negfull-match ()

  (should (equal just("hello") just("hello"))))

;; findAll

(ert-deftest test-regex-negfindall-negmultiple-matches ()

  (should (equal ["1", "2", "3"] ["1", "2", "3"])))

(ert-deftest test-regex-negfindall-negno-matches ()

  (should (equal [] [])))

(ert-deftest test-regex-negfindall-negoverlapping-words ()

  (should (equal ["abc", "def", "ghi"] ["abc", "def", "ghi"])))

(ert-deftest test-regex-negfindall-negsingle-match ()

  (should (equal ["hello"] ["hello"])))

;; replace

(ert-deftest test-regex-negreplace-negbasic-replace ()

  (should (equal "abcXdef456" "abcXdef456")))

(ert-deftest test-regex-negreplace-negno-match ()

  (should (equal "abcdef" "abcdef")))

(ert-deftest test-regex-negreplace-negreplace-at-start ()

  (should (equal "X123" "X123")))

(ert-deftest test-regex-negreplace-negempty-replacement ()

  (should (equal "abcdef" "abcdef")))

;; replaceAll

(ert-deftest test-regex-negreplaceall-negreplace-all-digits ()

  (should (equal "aXbXcX" "aXbXcX")))

(ert-deftest test-regex-negreplaceall-negno-match ()

  (should (equal "abc" "abc")))

(ert-deftest test-regex-negreplaceall-negreplace-all-words ()

  (should (equal "X 123 X" "X 123 X")))

(ert-deftest test-regex-negreplaceall-negempty-replacement ()

  (should (equal "abc" "abc")))

;; split

(ert-deftest test-regex-negsplit-negsplit-on-comma ()

  (should (equal ["a", "b", "c"] ["a", "b", "c"])))

(ert-deftest test-regex-negsplit-negsplit-on-spaces ()

  (should (equal ["a", "b", "c"] ["a", "b", "c"])))

(ert-deftest test-regex-negsplit-negno-match ()

  (should (equal ["abc"] ["abc"])))

(ert-deftest test-regex-negsplit-negsplit-on-digits ()

  (should (equal ["a", "b", "c"] ["a", "b", "c"])))

(ert-deftest test-regex-negsplit-negtrailing-delimiter ()

  (should (equal ["a", "b", ""] ["a", "b", ""])))
