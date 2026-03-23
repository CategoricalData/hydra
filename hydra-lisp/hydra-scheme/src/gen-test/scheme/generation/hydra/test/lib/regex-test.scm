;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.regex primitives

(import (scheme base))

;; matches

(define (test-regex-negmatches-negexact-match)

  (assert (equal? #t ((hydra_lib_regex_matches "hello") "hello"))))

(define (test-regex-negmatches-negpattern-match)

  (assert (equal? #t ((hydra_lib_regex_matches "[a-z]+") "hello"))))

(define (test-regex-negmatches-negno-match)

  (assert (equal? #f ((hydra_lib_regex_matches "[0-9]+") "hello"))))

(define (test-regex-negmatches-negpartial-content-does-not-match)

  (assert (equal? #f ((hydra_lib_regex_matches "[a-z]+") "hello123"))))

(define (test-regex-negmatches-negdigit-pattern)

  (assert (equal? #t ((hydra_lib_regex_matches "[0-9]+") "12345"))))

(define (test-regex-negmatches-negmixed-pattern)

  (assert (equal? #t ((hydra_lib_regex_matches "[a-z]+[0-9]+") "hello123"))))

(define (test-regex-negmatches-negempty-pattern-matches-empty)

  (assert (equal? #t ((hydra_lib_regex_matches "") ""))))

(define (test-regex-negmatches-negempty-pattern-does-not-match-non-negempty)

  (assert (equal? #f ((hydra_lib_regex_matches "") "hello"))))

(define (test-regex-negmatches-negstar-matches-empty)

  (assert (equal? #t ((hydra_lib_regex_matches "a*") ""))))

(define (test-regex-negmatches-negalternation)

  (assert (equal? #t ((hydra_lib_regex_matches "cat|dog") "cat"))))

(define (test-regex-negmatches-negalternation-second)

  (assert (equal? #t ((hydra_lib_regex_matches "cat|dog") "dog"))))

(define (test-regex-negmatches-negalternation-no-match)

  (assert (equal? #f ((hydra_lib_regex_matches "cat|dog") "bird"))))

(define (test-regex-negmatches-negquantifier)

  (assert (equal? #t ((hydra_lib_regex_matches "ab?c") "ac"))))

(define (test-regex-negmatches-negquantifier-with-optional)

  (assert (equal? #t ((hydra_lib_regex_matches "ab?c") "abc"))))

;; find

(define (test-regex-negfind-negsimple-find)

  (assert (equal? (list 'just "123") ((hydra_lib_regex_find "[0-9]+") "abc123def"))))

(define (test-regex-negfind-negno-match)

  (assert (equal? (list 'nothing) ((hydra_lib_regex_find "[0-9]+") "abcdef"))))

(define (test-regex-negfind-negfind-first)

  (assert (equal? (list 'just "abc") ((hydra_lib_regex_find "[a-z]+") "123abc456def"))))

(define (test-regex-negfind-negempty-input)

  (assert (equal? (list 'nothing) ((hydra_lib_regex_find "[0-9]+") ""))))

(define (test-regex-negfind-negfull-match)

  (assert (equal? (list 'just "hello") ((hydra_lib_regex_find ".*") "hello"))))

;; findAll

(define (test-regex-negfindall-negmultiple-matches)

  (assert (equal? (list "1" "2" "3") ((hydra_lib_regex_find_all "[0-9]+") "a1b2c3"))))

(define (test-regex-negfindall-negno-matches)

  (assert (equal? (list ) ((hydra_lib_regex_find_all "[0-9]+") "abc"))))

(define (test-regex-negfindall-negoverlapping-words)

  (assert (equal? (list "abc" "def" "ghi") ((hydra_lib_regex_find_all "[a-z]+") "abc def ghi"))))

(define (test-regex-negfindall-negsingle-match)

  (assert (equal? (list "hello") ((hydra_lib_regex_find_all "hello") "say hello world"))))

;; replace

(define (test-regex-negreplace-negbasic-replace)

  (assert (equal? "abcXdef456" (((hydra_lib_regex_replace "[0-9]+") "X") "abc123def456"))))

(define (test-regex-negreplace-negno-match)

  (assert (equal? "abcdef" (((hydra_lib_regex_replace "[0-9]+") "X") "abcdef"))))

(define (test-regex-negreplace-negreplace-at-start)

  (assert (equal? "X123" (((hydra_lib_regex_replace "^[a-z]+") "X") "abc123"))))

(define (test-regex-negreplace-negempty-replacement)

  (assert (equal? "abcdef" (((hydra_lib_regex_replace "[0-9]+") "") "abc123def"))))

;; replaceAll

(define (test-regex-negreplaceall-negreplace-all-digits)

  (assert (equal? "aXbXcX" (((hydra_lib_regex_replace_all "[0-9]+") "X") "a1b2c3"))))

(define (test-regex-negreplaceall-negno-match)

  (assert (equal? "abc" (((hydra_lib_regex_replace_all "[0-9]+") "X") "abc"))))

(define (test-regex-negreplaceall-negreplace-all-words)

  (assert (equal? "X 123 X" (((hydra_lib_regex_replace_all "[a-z]+") "X") "abc 123 def"))))

(define (test-regex-negreplaceall-negempty-replacement)

  (assert (equal? "abc" (((hydra_lib_regex_replace_all "[0-9]+") "") "a1b2c3"))))

;; split

(define (test-regex-negsplit-negsplit-on-comma)

  (assert (equal? (list "a" "b" "c") ((hydra_lib_regex_split ",") "a,b,c"))))

(define (test-regex-negsplit-negsplit-on-spaces)

  (assert (equal? (list "a" "b" "c") ((hydra_lib_regex_split " +") "a b  c"))))

(define (test-regex-negsplit-negno-match)

  (assert (equal? (list "abc") ((hydra_lib_regex_split ",") "abc"))))

(define (test-regex-negsplit-negsplit-on-digits)

  (assert (equal? (list "a" "b" "c") ((hydra_lib_regex_split "[0-9]+") "a1b2c"))))

(define (test-regex-negsplit-negtrailing-delimiter)

  (assert (equal? (list "a" "b" "") ((hydra_lib_regex_split ",") "a,b,"))))
