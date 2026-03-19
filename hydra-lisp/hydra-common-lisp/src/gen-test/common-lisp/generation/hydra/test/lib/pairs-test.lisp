;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.pairs primitives

;; bimap

(defun test-pairs-negbimap-negtransform-both-elements ()

  (assert (equal (list 10 2) (((hydra_lib_pairs_bimap (cl:lambda (x) ((hydra_lib_math_mul x) 2))) (cl:lambda (s) (hydra_lib_strings_length s))) (list 5 "ab")))))

(defun test-pairs-negbimap-negwith-zero ()

  (assert (equal (list 0 5) (((hydra_lib_pairs_bimap (cl:lambda (x) ((hydra_lib_math_mul x) 2))) (cl:lambda (s) (hydra_lib_strings_length s))) (list 0 "hello")))))

;; first

(defun test-pairs-negfirst-negextract-first-element ()

  (assert (equal 42 (hydra_lib_pairs_first (list 42 "hello")))))

(defun test-pairs-negfirst-negwith-zero ()

  (assert (equal 0 (hydra_lib_pairs_first (list 0 "world")))))

(defun test-pairs-negfirst-negnegative-number ()

  (assert (equal -5 (hydra_lib_pairs_first (list -5 "test")))))

;; second

(defun test-pairs-negsecond-negextract-second-element ()

  (assert (equal "hello" (hydra_lib_pairs_second (list 42 "hello")))))

(defun test-pairs-negsecond-negempty-string ()

  (assert (equal "" (hydra_lib_pairs_second (list 0 "")))))

(defun test-pairs-negsecond-neglong-string ()

  (assert (equal "testing" (hydra_lib_pairs_second (list 123 "testing")))))
