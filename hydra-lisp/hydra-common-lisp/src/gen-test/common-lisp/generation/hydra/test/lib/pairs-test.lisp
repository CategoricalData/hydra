;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.pairs primitives

;; bimap

(defun test-pairs-negbimap-negtransform-both-elements ()

  (assert (equal (10, 2) (10, 2))))

(defun test-pairs-negbimap-negwith-zero ()

  (assert (equal (0, 5) (0, 5))))

;; first

(defun test-pairs-negfirst-negextract-first-element ()

  (assert (equal 42 42)))

(defun test-pairs-negfirst-negwith-zero ()

  (assert (equal 0 0)))

(defun test-pairs-negfirst-negnegative-number ()

  (assert (equal -5 -5)))

;; second

(defun test-pairs-negsecond-negextract-second-element ()

  (assert (equal hello hello)))

(defun test-pairs-negsecond-negempty-string ()

  (assert (equal  )))

(defun test-pairs-negsecond-neglong-string ()

  (assert (equal testing testing)))
