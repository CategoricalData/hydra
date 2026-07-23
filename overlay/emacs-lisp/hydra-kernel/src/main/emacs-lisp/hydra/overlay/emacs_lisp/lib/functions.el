;;; functions.el --- Hydra function primitives -*- lexical-binding: t; -*-

(require 'cl-lib)

;; identity :: a -> a
(defvar hydra_overlay_emacs_lisp_lib_functions_identity
  (lambda (x)
    "Return a value unchanged."
    x))

(provide 'hydra.lib.functions)
