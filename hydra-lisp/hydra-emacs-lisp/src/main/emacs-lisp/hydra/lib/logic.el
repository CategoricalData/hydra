;;; logic.el --- Hydra logic primitives -*- lexical-binding: t; -*-

(require 'cl-lib)

;; and :: Bool -> Bool -> Bool
(defvar hydra_lib_logic_and
  (lambda (a)
    "Compute the logical AND of two boolean values."
    (lambda (b)
      (and a b t))))

;; if_else :: Bool -> a -> a -> a
(defvar hydra_lib_logic_if_else
  (lambda (cond-val)
    "Compute a conditional expression."
    (lambda (then-val)
      (lambda (else-val)
        (if cond-val then-val else-val)))))

;; not :: Bool -> Bool
(defvar hydra_lib_logic_not
  (lambda (x)
    "Compute the logical NOT of a boolean value."
    (not x)))

;; or :: Bool -> Bool -> Bool
(defvar hydra_lib_logic_or
  (lambda (a)
    "Compute the logical OR of two boolean values."
    (lambda (b)
      (if (or a b) t nil))))

(provide 'hydra.lib.logic)
