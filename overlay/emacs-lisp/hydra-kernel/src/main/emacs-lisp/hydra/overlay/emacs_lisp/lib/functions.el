;;; functions.el --- Hydra function primitives -*- lexical-binding: t; -*-

(require 'cl-lib)

;; absurd :: void -> x
(defvar hydra_overlay_emacs_lisp_lib_functions_absurd
  (lambda (v)
    "Eliminate a value of the uninhabited void type. Unreachable in any well-typed program."
    (ignore v)
    (error "hydra.lib.functions.absurd: void has no inhabitants")))

;; identity :: a -> a
(defvar hydra_overlay_emacs_lisp_lib_functions_identity
  (lambda (x)
    "Return a value unchanged."
    x))

(provide 'hydra.lib.functions)
