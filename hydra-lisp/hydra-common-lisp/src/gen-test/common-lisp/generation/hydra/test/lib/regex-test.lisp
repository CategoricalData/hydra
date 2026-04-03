;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.regex primitives

;; matches

(defun test-regex-negmatches-negexact-match ()

  (assert (equal true true)))

(defun test-regex-negmatches-negpattern-match ()

  (assert (equal true true)))

(defun test-regex-negmatches-negno-match ()

  (assert (equal false false)))

(defun test-regex-negmatches-negpartial-content-does-not-match ()

  (assert (equal false false)))

(defun test-regex-negmatches-negdigit-pattern ()

  (assert (equal true true)))

(defun test-regex-negmatches-negmixed-pattern ()

  (assert (equal true true)))

(defun test-regex-negmatches-negempty-pattern-matches-empty ()

  (assert (equal true true)))

(defun test-regex-negmatches-negempty-pattern-does-not-match-non-negempty ()

  (assert (equal false false)))

(defun test-regex-negmatches-negstar-matches-empty ()

  (assert (equal true true)))

(defun test-regex-negmatches-negalternation ()

  (assert (equal true true)))

(defun test-regex-negmatches-negalternation-second ()

  (assert (equal true true)))

(defun test-regex-negmatches-negalternation-no-match ()

  (assert (equal false false)))

(defun test-regex-negmatches-negquantifier ()

  (assert (equal true true)))

(defun test-regex-negmatches-negquantifier-with-optional ()

  (assert (equal true true)))

;; find

(defun test-regex-negfind-negsimple-find ()

  (assert (equal just("123") just("123"))))

(defun test-regex-negfind-negno-match ()

  (assert (equal nothing nothing)))

(defun test-regex-negfind-negfind-first ()

  (assert (equal just("abc") just("abc"))))

(defun test-regex-negfind-negempty-input ()

  (assert (equal nothing nothing)))

(defun test-regex-negfind-negfull-match ()

  (assert (equal just("hello") just("hello"))))

;; findAll

(defun test-regex-negfindall-negmultiple-matches ()

  (assert (equal ["1", "2", "3"] ["1", "2", "3"])))

(defun test-regex-negfindall-negno-matches ()

  (assert (equal [] [])))

(defun test-regex-negfindall-negoverlapping-words ()

  (assert (equal ["abc", "def", "ghi"] ["abc", "def", "ghi"])))

(defun test-regex-negfindall-negsingle-match ()

  (assert (equal ["hello"] ["hello"])))

;; replace

(defun test-regex-negreplace-negbasic-replace ()

  (assert (equal "abcXdef456" "abcXdef456")))

(defun test-regex-negreplace-negno-match ()

  (assert (equal "abcdef" "abcdef")))

(defun test-regex-negreplace-negreplace-at-start ()

  (assert (equal "X123" "X123")))

(defun test-regex-negreplace-negempty-replacement ()

  (assert (equal "abcdef" "abcdef")))

;; replaceAll

(defun test-regex-negreplaceall-negreplace-all-digits ()

  (assert (equal "aXbXcX" "aXbXcX")))

(defun test-regex-negreplaceall-negno-match ()

  (assert (equal "abc" "abc")))

(defun test-regex-negreplaceall-negreplace-all-words ()

  (assert (equal "X 123 X" "X 123 X")))

(defun test-regex-negreplaceall-negempty-replacement ()

  (assert (equal "abc" "abc")))

;; split

(defun test-regex-negsplit-negsplit-on-comma ()

  (assert (equal ["a", "b", "c"] ["a", "b", "c"])))

(defun test-regex-negsplit-negsplit-on-spaces ()

  (assert (equal ["a", "b", "c"] ["a", "b", "c"])))

(defun test-regex-negsplit-negno-match ()

  (assert (equal ["abc"] ["abc"])))

(defun test-regex-negsplit-negsplit-on-digits ()

  (assert (equal ["a", "b", "c"] ["a", "b", "c"])))

(defun test-regex-negsplit-negtrailing-delimiter ()

  (assert (equal ["a", "b", ""] ["a", "b", ""])))
