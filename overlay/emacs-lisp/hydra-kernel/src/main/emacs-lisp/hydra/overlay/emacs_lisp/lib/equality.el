;;; equality.el --- Hydra equality primitives -*- lexical-binding: t; -*-

(require 'cl-lib)

;; equal :: a -> a -> Bool
(defvar hydra_overlay_emacs_lisp_lib_equality_equal
  (lambda (a)
    "Check if two values are equal."
    (lambda (b)
      (equal a b))))

(provide 'hydra.lib.equality)
