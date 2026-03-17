;;; logic.el --- Hydra logic primitives -*- lexical-binding: t; -*-

(require 'cl-lib)

;; and :: Bool -> Bool -> Bool
(defvar hydra_lib_logic_and
  (lambda (a)
    (lambda (b)
      (and a b t))))

;; if_else :: Bool -> a -> a -> a
(defvar hydra_lib_logic_if_else
  (lambda (cond-val)
    (lambda (then-val)
      (lambda (else-val)
        (if cond-val then-val else-val)))))

;; not :: Bool -> Bool
(defvar hydra_lib_logic_not
  (lambda (x)
    (not x)))

;; or :: Bool -> Bool -> Bool
(defvar hydra_lib_logic_or
  (lambda (a)
    (lambda (b)
      (if (or a b) t nil))))

(provide 'hydra.lib.logic)
