(require 'cl-lib)

(require 'hydra.core)

(defvar hydra_eval_lib_math_even (lambda (cx) (lambda (g) (lambda (x) (list :right (list :application (make-hydra_core_application (list :application (make-hydra_core_application (list :variable "hydra.lib.equality.equal") (list :application (make-hydra_core_application (list :application (make-hydra_core_application (list :variable "hydra.lib.math.mod") x)) (list :literal (list :integer (list :int32 2))))))) (list :literal (list :integer (list :int32 0))))))))))

(defvar hydra_eval_lib_math_odd (lambda (cx) (lambda (g) (lambda (x) (list :right (list :application (make-hydra_core_application (list :variable "hydra.lib.logic.not") (list :application (make-hydra_core_application (list :variable "hydra.lib.math.even") x)))))))))

(defvar hydra_eval_lib_math_pred (lambda (cx) (lambda (g) (lambda (x) (list :right (list :application (make-hydra_core_application (list :application (make-hydra_core_application (list :variable "hydra.lib.math.sub") x)) (list :literal (list :integer (list :int32 1))))))))))

(defvar hydra_eval_lib_math_succ (lambda (cx) (lambda (g) (lambda (x) (list :right (list :application (make-hydra_core_application (list :application (make-hydra_core_application (list :variable "hydra.lib.math.add") x)) (list :literal (list :integer (list :int32 1))))))))))

(provide 'hydra.eval.lib.math)
