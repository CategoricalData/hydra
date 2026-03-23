;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.regex primitives

;; matches

(defun test-regex-negmatches-negexact-match ()

  (assert (equal cl:t ((hydra_lib_regex_matches "hello") "hello"))))

(defun test-regex-negmatches-negpattern-match ()

  (assert (equal cl:t ((hydra_lib_regex_matches "[a-z]+") "hello"))))

(defun test-regex-negmatches-negno-match ()

  (assert (equal cl:nil ((hydra_lib_regex_matches "[0-9]+") "hello"))))

(defun test-regex-negmatches-negpartial-content-does-not-match ()

  (assert (equal cl:nil ((hydra_lib_regex_matches "[a-z]+") "hello123"))))

(defun test-regex-negmatches-negdigit-pattern ()

  (assert (equal cl:t ((hydra_lib_regex_matches "[0-9]+") "12345"))))

(defun test-regex-negmatches-negmixed-pattern ()

  (assert (equal cl:t ((hydra_lib_regex_matches "[a-z]+[0-9]+") "hello123"))))

(defun test-regex-negmatches-negempty-pattern-matches-empty ()

  (assert (equal cl:t ((hydra_lib_regex_matches "") ""))))

(defun test-regex-negmatches-negempty-pattern-does-not-match-non-negempty ()

  (assert (equal cl:nil ((hydra_lib_regex_matches "") "hello"))))

(defun test-regex-negmatches-negstar-matches-empty ()

  (assert (equal cl:t ((hydra_lib_regex_matches "a*") ""))))

(defun test-regex-negmatches-negalternation ()

  (assert (equal cl:t ((hydra_lib_regex_matches "cat|dog") "cat"))))

(defun test-regex-negmatches-negalternation-second ()

  (assert (equal cl:t ((hydra_lib_regex_matches "cat|dog") "dog"))))

(defun test-regex-negmatches-negalternation-no-match ()

  (assert (equal cl:nil ((hydra_lib_regex_matches "cat|dog") "bird"))))

(defun test-regex-negmatches-negquantifier ()

  (assert (equal cl:t ((hydra_lib_regex_matches "ab?c") "ac"))))

(defun test-regex-negmatches-negquantifier-with-optional ()

  (assert (equal cl:t ((hydra_lib_regex_matches "ab?c") "abc"))))

;; find

(defun test-regex-negfind-negsimple-find ()

  (assert (equal (list :just "123") ((hydra_lib_regex_find "[0-9]+") "abc123def"))))

(defun test-regex-negfind-negno-match ()

  (assert (equal (list :nothing) ((hydra_lib_regex_find "[0-9]+") "abcdef"))))

(defun test-regex-negfind-negfind-first ()

  (assert (equal (list :just "abc") ((hydra_lib_regex_find "[a-z]+") "123abc456def"))))

(defun test-regex-negfind-negempty-input ()

  (assert (equal (list :nothing) ((hydra_lib_regex_find "[0-9]+") ""))))

(defun test-regex-negfind-negfull-match ()

  (assert (equal (list :just "hello") ((hydra_lib_regex_find ".*") "hello"))))

;; findAll

(defun test-regex-negfindall-negmultiple-matches ()

  (assert (equal (list "1" "2" "3") ((hydra_lib_regex_find_all "[0-9]+") "a1b2c3"))))

(defun test-regex-negfindall-negno-matches ()

  (assert (equal (list ) ((hydra_lib_regex_find_all "[0-9]+") "abc"))))

(defun test-regex-negfindall-negoverlapping-words ()

  (assert (equal (list "abc" "def" "ghi") ((hydra_lib_regex_find_all "[a-z]+") "abc def ghi"))))

(defun test-regex-negfindall-negsingle-match ()

  (assert (equal (list "hello") ((hydra_lib_regex_find_all "hello") "say hello world"))))

;; replace

(defun test-regex-negreplace-negbasic-replace ()

  (assert (equal "abcXdef456" (((hydra_lib_regex_replace "[0-9]+") "X") "abc123def456"))))

(defun test-regex-negreplace-negno-match ()

  (assert (equal "abcdef" (((hydra_lib_regex_replace "[0-9]+") "X") "abcdef"))))

(defun test-regex-negreplace-negreplace-at-start ()

  (assert (equal "X123" (((hydra_lib_regex_replace "^[a-z]+") "X") "abc123"))))

(defun test-regex-negreplace-negempty-replacement ()

  (assert (equal "abcdef" (((hydra_lib_regex_replace "[0-9]+") "") "abc123def"))))

;; replaceAll

(defun test-regex-negreplaceall-negreplace-all-digits ()

  (assert (equal "aXbXcX" (((hydra_lib_regex_replace_all "[0-9]+") "X") "a1b2c3"))))

(defun test-regex-negreplaceall-negno-match ()

  (assert (equal "abc" (((hydra_lib_regex_replace_all "[0-9]+") "X") "abc"))))

(defun test-regex-negreplaceall-negreplace-all-words ()

  (assert (equal "X 123 X" (((hydra_lib_regex_replace_all "[a-z]+") "X") "abc 123 def"))))

(defun test-regex-negreplaceall-negempty-replacement ()

  (assert (equal "abc" (((hydra_lib_regex_replace_all "[0-9]+") "") "a1b2c3"))))

;; split

(defun test-regex-negsplit-negsplit-on-comma ()

  (assert (equal (list "a" "b" "c") ((hydra_lib_regex_split ",") "a,b,c"))))

(defun test-regex-negsplit-negsplit-on-spaces ()

  (assert (equal (list "a" "b" "c") ((hydra_lib_regex_split " +") "a b  c"))))

(defun test-regex-negsplit-negno-match ()

  (assert (equal (list "abc") ((hydra_lib_regex_split ",") "abc"))))

(defun test-regex-negsplit-negsplit-on-digits ()

  (assert (equal (list "a" "b" "c") ((hydra_lib_regex_split "[0-9]+") "a1b2c"))))

(defun test-regex-negsplit-negtrailing-delimiter ()

  (assert (equal (list "a" "b" "") ((hydra_lib_regex_split ",") "a,b,"))))
