;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.pairs primitives

;; bimap

(defun test-bimap-negtransform-both-elements ()

  (assert (equal (list 10 2) (((hydra_lib_pairs_bimap (cl:lambda (x) ((hydra_lib_math_mul x) 2))) (cl:lambda (s) (hydra_lib_strings_length s))) (list 5 "ab")))))

(defun test-bimap-negwith-zero ()

  (assert (equal (list 0 5) (((hydra_lib_pairs_bimap (cl:lambda (x) ((hydra_lib_math_mul x) 2))) (cl:lambda (s) (hydra_lib_strings_length s))) (list 0 "hello")))))

;; first

(defun test-first-negextract-first-element ()

  (assert (equal 42 (hydra_lib_pairs_first (list 42 "hello")))))

(defun test-first-negwith-zero ()

  (assert (equal 0 (hydra_lib_pairs_first (list 0 "world")))))

(defun test-first-negnegative-number ()

  (assert (equal -5 (hydra_lib_pairs_first (list -5 "test")))))

;; second

(defun test-second-negextract-second-element ()

  (assert (equal "hello" (hydra_lib_pairs_second (list 42 "hello")))))

(defun test-second-negempty-string ()

  (assert (equal "" (hydra_lib_pairs_second (list 0 "")))))

(defun test-second-neglong-string ()

  (assert (equal "testing" (hydra_lib_pairs_second (list 123 "testing")))))
