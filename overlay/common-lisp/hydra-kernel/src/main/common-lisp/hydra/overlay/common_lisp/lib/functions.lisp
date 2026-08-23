(in-package :cl-user)

;; absurd :: void -> x
;; Eliminate a value of the uninhabited void type. Unreachable in any well-typed program.
(defvar hydra_overlay_common_lisp_lib_functions_absurd
  (lambda (v) (declare (ignore v)) (error "hydra.lib.functions.absurd: void has no inhabitants")))

;; identity :: a -> a
;; Return a value unchanged.
(defvar hydra_overlay_common_lisp_lib_functions_identity
  (lambda (x) x))
