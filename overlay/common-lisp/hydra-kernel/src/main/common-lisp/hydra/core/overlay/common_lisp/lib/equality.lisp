(in-package :cl-user)

;; equal :: a -> a -> Bool
;; Check if two values are equal.
;; Uses generic-compare, defined in overlay/common_lisp/lib/ordering.lisp.
(defvar hydra_overlay_common_lisp_lib_equality_equal
  (lambda (a)
    (lambda (b)
      (= (generic-compare a b) 0))))
