(require 'cl-lib)

(require 'hydra.core)

(defvar hydra_eval_lib_math_even (lambda (cx) (lambda (g) (lambda (x) (list :right (list :application (make-hydra_core_application (list :application (make-hydra_core_application (list :variable "hydra.lib.equality.equal") (list :application (make-hydra_core_application (list :application (make-hydra_core_application (list :variable "hydra.lib.maybes.fromMaybe") (list :literal (list :integer (list :int32 0))))) (list :application (make-hydra_core_application (list :application (make-hydra_core_application (list :variable "hydra.lib.math.maybeMod") x)) (list :literal (list :integer (list :int32 2))))))))) (list :literal (list :integer (list :int32 0))))))))))

(defvar hydra_eval_lib_math_odd (lambda (cx) (lambda (g) (lambda (x) (list :right (list :application (make-hydra_core_application (list :variable "hydra.lib.logic.not") (list :application (make-hydra_core_application (list :variable "hydra.lib.math.even") x)))))))))

(provide 'hydra.eval.lib.math)
