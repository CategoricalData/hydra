;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.regex primitives

(ns generation.hydra.test.lib.regex-test
  (:require [clojure.test :refer :all]))

;; matches

(deftest test-regex-negmatches-negexact-match

  (is (= true

         ((hydra_lib_regex_matches "hello") "hello"))))

(deftest test-regex-negmatches-negpattern-match

  (is (= true

         ((hydra_lib_regex_matches "[a-z]+") "hello"))))

(deftest test-regex-negmatches-negno-match

  (is (= false

         ((hydra_lib_regex_matches "[0-9]+") "hello"))))

(deftest test-regex-negmatches-negpartial-content-does-not-match

  (is (= false

         ((hydra_lib_regex_matches "[a-z]+") "hello123"))))

(deftest test-regex-negmatches-negdigit-pattern

  (is (= true

         ((hydra_lib_regex_matches "[0-9]+") "12345"))))

(deftest test-regex-negmatches-negmixed-pattern

  (is (= true

         ((hydra_lib_regex_matches "[a-z]+[0-9]+") "hello123"))))

(deftest test-regex-negmatches-negempty-pattern-matches-empty

  (is (= true

         ((hydra_lib_regex_matches "") ""))))

(deftest test-regex-negmatches-negempty-pattern-does-not-match-non-negempty

  (is (= false

         ((hydra_lib_regex_matches "") "hello"))))

(deftest test-regex-negmatches-negstar-matches-empty

  (is (= true

         ((hydra_lib_regex_matches "a*") ""))))

(deftest test-regex-negmatches-negalternation

  (is (= true

         ((hydra_lib_regex_matches "cat|dog") "cat"))))

(deftest test-regex-negmatches-negalternation-second

  (is (= true

         ((hydra_lib_regex_matches "cat|dog") "dog"))))

(deftest test-regex-negmatches-negalternation-no-match

  (is (= false

         ((hydra_lib_regex_matches "cat|dog") "bird"))))

(deftest test-regex-negmatches-negquantifier

  (is (= true

         ((hydra_lib_regex_matches "ab?c") "ac"))))

(deftest test-regex-negmatches-negquantifier-with-optional

  (is (= true

         ((hydra_lib_regex_matches "ab?c") "abc"))))

;; find

(deftest test-regex-negfind-negsimple-find

  (is (= (list :just "123")

         ((hydra_lib_regex_find "[0-9]+") "abc123def"))))

(deftest test-regex-negfind-negno-match

  (is (= (list :nothing)

         ((hydra_lib_regex_find "[0-9]+") "abcdef"))))

(deftest test-regex-negfind-negfind-first

  (is (= (list :just "abc")

         ((hydra_lib_regex_find "[a-z]+") "123abc456def"))))

(deftest test-regex-negfind-negempty-input

  (is (= (list :nothing)

         ((hydra_lib_regex_find "[0-9]+") ""))))

(deftest test-regex-negfind-negfull-match

  (is (= (list :just "hello")

         ((hydra_lib_regex_find ".*") "hello"))))

;; findAll

(deftest test-regex-negfindall-negmultiple-matches

  (is (= (list "1" "2" "3")

         ((hydra_lib_regex_find_all "[0-9]+") "a1b2c3"))))

(deftest test-regex-negfindall-negno-matches

  (is (= (list )

         ((hydra_lib_regex_find_all "[0-9]+") "abc"))))

(deftest test-regex-negfindall-negoverlapping-words

  (is (= (list "abc" "def" "ghi")

         ((hydra_lib_regex_find_all "[a-z]+") "abc def ghi"))))

(deftest test-regex-negfindall-negsingle-match

  (is (= (list "hello")

         ((hydra_lib_regex_find_all "hello") "say hello world"))))

;; replace

(deftest test-regex-negreplace-negbasic-replace

  (is (= "abcXdef456"

         (((hydra_lib_regex_replace "[0-9]+") "X") "abc123def456"))))

(deftest test-regex-negreplace-negno-match

  (is (= "abcdef"

         (((hydra_lib_regex_replace "[0-9]+") "X") "abcdef"))))

(deftest test-regex-negreplace-negreplace-at-start

  (is (= "X123"

         (((hydra_lib_regex_replace "^[a-z]+") "X") "abc123"))))

(deftest test-regex-negreplace-negempty-replacement

  (is (= "abcdef"

         (((hydra_lib_regex_replace "[0-9]+") "") "abc123def"))))

;; replaceAll

(deftest test-regex-negreplaceall-negreplace-all-digits

  (is (= "aXbXcX"

         (((hydra_lib_regex_replace_all "[0-9]+") "X") "a1b2c3"))))

(deftest test-regex-negreplaceall-negno-match

  (is (= "abc"

         (((hydra_lib_regex_replace_all "[0-9]+") "X") "abc"))))

(deftest test-regex-negreplaceall-negreplace-all-words

  (is (= "X 123 X"

         (((hydra_lib_regex_replace_all "[a-z]+") "X") "abc 123 def"))))

(deftest test-regex-negreplaceall-negempty-replacement

  (is (= "abc"

         (((hydra_lib_regex_replace_all "[0-9]+") "") "a1b2c3"))))

;; split

(deftest test-regex-negsplit-negsplit-on-comma

  (is (= (list "a" "b" "c")

         ((hydra_lib_regex_split ",") "a,b,c"))))

(deftest test-regex-negsplit-negsplit-on-spaces

  (is (= (list "a" "b" "c")

         ((hydra_lib_regex_split " +") "a b  c"))))

(deftest test-regex-negsplit-negno-match

  (is (= (list "abc")

         ((hydra_lib_regex_split ",") "abc"))))

(deftest test-regex-negsplit-negsplit-on-digits

  (is (= (list "a" "b" "c")

         ((hydra_lib_regex_split "[0-9]+") "a1b2c"))))

(deftest test-regex-negsplit-negtrailing-delimiter

  (is (= (list "a" "b" "")

         ((hydra_lib_regex_split ",") "a,b,"))))
