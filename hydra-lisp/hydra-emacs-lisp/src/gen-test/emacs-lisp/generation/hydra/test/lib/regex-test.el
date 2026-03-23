;;; Note: this is an automatically generated file. Do not edit. -*- lexical-binding: t; coding: utf-8 -*-
;;; hydra.lib.regex primitives

(require 'ert)

;; matches

(ert-deftest test-regex-negmatches-negexact-match ()

  (should (equal t (funcall (funcall hydra_lib_regex_matches "hello") "hello"))))

(ert-deftest test-regex-negmatches-negpattern-match ()

  (should (equal t (funcall (funcall hydra_lib_regex_matches "[a-z]+") "hello"))))

(ert-deftest test-regex-negmatches-negno-match ()

  (should (equal nil (funcall (funcall hydra_lib_regex_matches "[0-9]+") "hello"))))

(ert-deftest test-regex-negmatches-negpartial-content-does-not-match ()

  (should (equal nil (funcall (funcall hydra_lib_regex_matches "[a-z]+") "hello123"))))

(ert-deftest test-regex-negmatches-negdigit-pattern ()

  (should (equal t (funcall (funcall hydra_lib_regex_matches "[0-9]+") "12345"))))

(ert-deftest test-regex-negmatches-negmixed-pattern ()

  (should (equal t (funcall (funcall hydra_lib_regex_matches "[a-z]+[0-9]+") "hello123"))))

(ert-deftest test-regex-negmatches-negempty-pattern-matches-empty ()

  (should (equal t (funcall (funcall hydra_lib_regex_matches "") ""))))

(ert-deftest test-regex-negmatches-negempty-pattern-does-not-match-non-negempty ()

  (should (equal nil (funcall (funcall hydra_lib_regex_matches "") "hello"))))

(ert-deftest test-regex-negmatches-negstar-matches-empty ()

  (should (equal t (funcall (funcall hydra_lib_regex_matches "a*") ""))))

(ert-deftest test-regex-negmatches-negalternation ()

  (should (equal t (funcall (funcall hydra_lib_regex_matches "cat|dog") "cat"))))

(ert-deftest test-regex-negmatches-negalternation-second ()

  (should (equal t (funcall (funcall hydra_lib_regex_matches "cat|dog") "dog"))))

(ert-deftest test-regex-negmatches-negalternation-no-match ()

  (should (equal nil (funcall (funcall hydra_lib_regex_matches "cat|dog") "bird"))))

(ert-deftest test-regex-negmatches-negquantifier ()

  (should (equal t (funcall (funcall hydra_lib_regex_matches "ab?c") "ac"))))

(ert-deftest test-regex-negmatches-negquantifier-with-optional ()

  (should (equal t (funcall (funcall hydra_lib_regex_matches "ab?c") "abc"))))

;; find

(ert-deftest test-regex-negfind-negsimple-find ()

  (should (equal (list :just "123") (funcall (funcall hydra_lib_regex_find "[0-9]+") "abc123def"))))

(ert-deftest test-regex-negfind-negno-match ()

  (should (equal (list :nothing) (funcall (funcall hydra_lib_regex_find "[0-9]+") "abcdef"))))

(ert-deftest test-regex-negfind-negfind-first ()

  (should (equal (list :just "abc") (funcall (funcall hydra_lib_regex_find "[a-z]+") "123abc456def"))))

(ert-deftest test-regex-negfind-negempty-input ()

  (should (equal (list :nothing) (funcall (funcall hydra_lib_regex_find "[0-9]+") ""))))

(ert-deftest test-regex-negfind-negfull-match ()

  (should (equal (list :just "hello") (funcall (funcall hydra_lib_regex_find ".*") "hello"))))

;; findAll

(ert-deftest test-regex-negfindall-negmultiple-matches ()

  (should (equal (list "1" "2" "3") (funcall (funcall hydra_lib_regex_find_all "[0-9]+") "a1b2c3"))))

(ert-deftest test-regex-negfindall-negno-matches ()

  (should (equal (list ) (funcall (funcall hydra_lib_regex_find_all "[0-9]+") "abc"))))

(ert-deftest test-regex-negfindall-negoverlapping-words ()

  (should (equal (list "abc" "def" "ghi") (funcall (funcall hydra_lib_regex_find_all "[a-z]+") "abc def ghi"))))

(ert-deftest test-regex-negfindall-negsingle-match ()

  (should (equal (list "hello") (funcall (funcall hydra_lib_regex_find_all "hello") "say hello world"))))

;; replace

(ert-deftest test-regex-negreplace-negbasic-replace ()

  (should (equal "abcXdef456" (funcall (funcall (funcall hydra_lib_regex_replace "[0-9]+") "X") "abc123def456"))))

(ert-deftest test-regex-negreplace-negno-match ()

  (should (equal "abcdef" (funcall (funcall (funcall hydra_lib_regex_replace "[0-9]+") "X") "abcdef"))))

(ert-deftest test-regex-negreplace-negreplace-at-start ()

  (should (equal "X123" (funcall (funcall (funcall hydra_lib_regex_replace "^[a-z]+") "X") "abc123"))))

(ert-deftest test-regex-negreplace-negempty-replacement ()

  (should (equal "abcdef" (funcall (funcall (funcall hydra_lib_regex_replace "[0-9]+") "") "abc123def"))))

;; replaceAll

(ert-deftest test-regex-negreplaceall-negreplace-all-digits ()

  (should (equal "aXbXcX" (funcall (funcall (funcall hydra_lib_regex_replace_all "[0-9]+") "X") "a1b2c3"))))

(ert-deftest test-regex-negreplaceall-negno-match ()

  (should (equal "abc" (funcall (funcall (funcall hydra_lib_regex_replace_all "[0-9]+") "X") "abc"))))

(ert-deftest test-regex-negreplaceall-negreplace-all-words ()

  (should (equal "X 123 X" (funcall (funcall (funcall hydra_lib_regex_replace_all "[a-z]+") "X") "abc 123 def"))))

(ert-deftest test-regex-negreplaceall-negempty-replacement ()

  (should (equal "abc" (funcall (funcall (funcall hydra_lib_regex_replace_all "[0-9]+") "") "a1b2c3"))))

;; split

(ert-deftest test-regex-negsplit-negsplit-on-comma ()

  (should (equal (list "a" "b" "c") (funcall (funcall hydra_lib_regex_split ",") "a,b,c"))))

(ert-deftest test-regex-negsplit-negsplit-on-spaces ()

  (should (equal (list "a" "b" "c") (funcall (funcall hydra_lib_regex_split " +") "a b  c"))))

(ert-deftest test-regex-negsplit-negno-match ()

  (should (equal (list "abc") (funcall (funcall hydra_lib_regex_split ",") "abc"))))

(ert-deftest test-regex-negsplit-negsplit-on-digits ()

  (should (equal (list "a" "b" "c") (funcall (funcall hydra_lib_regex_split "[0-9]+") "a1b2c"))))

(ert-deftest test-regex-negsplit-negtrailing-delimiter ()

  (should (equal (list "a" "b" "") (funcall (funcall hydra_lib_regex_split ",") "a,b,"))))
