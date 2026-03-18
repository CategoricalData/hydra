;;; Note: this is an automatically generated file. Do not edit.
;;; hydra.lib.pairs primitives

(require 'ert)

;; bimap

(ert-deftest test-bimap-negtransform-both-elements ()

  (should (equal (list 10 2) (((hydra_lib_pairs_bimap (lambda (x) ((hydra_lib_math_mul x) 2))) (lambda (s) (hydra_lib_strings_length s))) (list 5 "ab")))))

(ert-deftest test-bimap-negwith-zero ()

  (should (equal (list 0 5) (((hydra_lib_pairs_bimap (lambda (x) ((hydra_lib_math_mul x) 2))) (lambda (s) (hydra_lib_strings_length s))) (list 0 "hello")))))

;; first

(ert-deftest test-first-negextract-first-element ()

  (should (equal 42 (hydra_lib_pairs_first (list 42 "hello")))))

(ert-deftest test-first-negwith-zero ()

  (should (equal 0 (hydra_lib_pairs_first (list 0 "world")))))

(ert-deftest test-first-negnegative-number ()

  (should (equal -5 (hydra_lib_pairs_first (list -5 "test")))))

;; second

(ert-deftest test-second-negextract-second-element ()

  (should (equal "hello" (hydra_lib_pairs_second (list 42 "hello")))))

(ert-deftest test-second-negempty-string ()

  (should (equal "" (hydra_lib_pairs_second (list 0 "")))))

(ert-deftest test-second-neglong-string ()

  (should (equal "testing" (hydra_lib_pairs_second (list 123 "testing")))))
