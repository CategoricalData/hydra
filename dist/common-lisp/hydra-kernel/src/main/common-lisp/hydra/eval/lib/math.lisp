(defpackage :hydra.eval.lib.math
(:use :cl :hydra.core)
(:export :hydra_eval_lib_math_even :hydra_eval_lib_math_odd))

(in-package :hydra.eval.lib.math)

(cl:defvar hydra_eval_lib_math_even (cl:lambda (cx) (cl:lambda (g) (cl:lambda (x) (list :right (list :application (make-hydra_core_application (list :application (make-hydra_core_application (list :variable "hydra.lib.equality.equal") (list :application (make-hydra_core_application (list :application (make-hydra_core_application (list :variable "hydra.lib.maybes.fromMaybe") (list :literal (list :integer (list :int32 0))))) (list :application (make-hydra_core_application (list :application (make-hydra_core_application (list :variable "hydra.lib.math.maybeMod") x)) (list :literal (list :integer (list :int32 2))))))))) (list :literal (list :integer (list :int32 0))))))))))

(cl:defvar hydra_eval_lib_math_odd (cl:lambda (cx) (cl:lambda (g) (cl:lambda (x) (list :right (list :application (make-hydra_core_application (list :variable "hydra.lib.logic.not") (list :application (make-hydra_core_application (list :variable "hydra.lib.math.even") x)))))))))
