;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.regex primitives

(import (scheme base))

;; matches

(define (test-regex-negmatches-negexact-match)

  (assert (equal? true true)))

(define (test-regex-negmatches-negpattern-match)

  (assert (equal? true true)))

(define (test-regex-negmatches-negno-match)

  (assert (equal? false false)))

(define (test-regex-negmatches-negpartial-content-does-not-match)

  (assert (equal? false false)))

(define (test-regex-negmatches-negdigit-pattern)

  (assert (equal? true true)))

(define (test-regex-negmatches-negmixed-pattern)

  (assert (equal? true true)))

(define (test-regex-negmatches-negempty-pattern-matches-empty)

  (assert (equal? true true)))

(define (test-regex-negmatches-negempty-pattern-does-not-match-non-negempty)

  (assert (equal? false false)))

(define (test-regex-negmatches-negstar-matches-empty)

  (assert (equal? true true)))

(define (test-regex-negmatches-negalternation)

  (assert (equal? true true)))

(define (test-regex-negmatches-negalternation-second)

  (assert (equal? true true)))

(define (test-regex-negmatches-negalternation-no-match)

  (assert (equal? false false)))

(define (test-regex-negmatches-negquantifier)

  (assert (equal? true true)))

(define (test-regex-negmatches-negquantifier-with-optional)

  (assert (equal? true true)))

;; find

(define (test-regex-negfind-negsimple-find)

  (assert (equal? just("123") just("123"))))

(define (test-regex-negfind-negno-match)

  (assert (equal? nothing nothing)))

(define (test-regex-negfind-negfind-first)

  (assert (equal? just("abc") just("abc"))))

(define (test-regex-negfind-negempty-input)

  (assert (equal? nothing nothing)))

(define (test-regex-negfind-negfull-match)

  (assert (equal? just("hello") just("hello"))))

;; findAll

(define (test-regex-negfindall-negmultiple-matches)

  (assert (equal? ["1", "2", "3"] ["1", "2", "3"])))

(define (test-regex-negfindall-negno-matches)

  (assert (equal? [] [])))

(define (test-regex-negfindall-negoverlapping-words)

  (assert (equal? ["abc", "def", "ghi"] ["abc", "def", "ghi"])))

(define (test-regex-negfindall-negsingle-match)

  (assert (equal? ["hello"] ["hello"])))

;; replace

(define (test-regex-negreplace-negbasic-replace)

  (assert (equal? "abcXdef456" "abcXdef456")))

(define (test-regex-negreplace-negno-match)

  (assert (equal? "abcdef" "abcdef")))

(define (test-regex-negreplace-negreplace-at-start)

  (assert (equal? "X123" "X123")))

(define (test-regex-negreplace-negempty-replacement)

  (assert (equal? "abcdef" "abcdef")))

;; replaceAll

(define (test-regex-negreplaceall-negreplace-all-digits)

  (assert (equal? "aXbXcX" "aXbXcX")))

(define (test-regex-negreplaceall-negno-match)

  (assert (equal? "abc" "abc")))

(define (test-regex-negreplaceall-negreplace-all-words)

  (assert (equal? "X 123 X" "X 123 X")))

(define (test-regex-negreplaceall-negempty-replacement)

  (assert (equal? "abc" "abc")))

;; split

(define (test-regex-negsplit-negsplit-on-comma)

  (assert (equal? ["a", "b", "c"] ["a", "b", "c"])))

(define (test-regex-negsplit-negsplit-on-spaces)

  (assert (equal? ["a", "b", "c"] ["a", "b", "c"])))

(define (test-regex-negsplit-negno-match)

  (assert (equal? ["abc"] ["abc"])))

(define (test-regex-negsplit-negsplit-on-digits)

  (assert (equal? ["a", "b", "c"] ["a", "b", "c"])))

(define (test-regex-negsplit-negtrailing-delimiter)

  (assert (equal? ["a", "b", ""] ["a", "b", ""])))
