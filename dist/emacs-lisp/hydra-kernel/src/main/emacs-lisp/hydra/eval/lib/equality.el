(require 'cl-lib)

(require 'hydra.core)

(defvar hydra_eval_lib_equality_identity (lambda (cx) (lambda (g) (lambda (x) (list :right x)))))

(defvar hydra_eval_lib_equality_max (lambda (cx) (lambda (g) (lambda (x) (lambda (y) (list :right (list :application (make-hydra_core_application (list :application (make-hydra_core_application (list :application (make-hydra_core_application (list :variable "hydra.lib.logic.ifElse") (list :application (make-hydra_core_application (list :application (make-hydra_core_application (list :variable "hydra.lib.equality.gte") x)) y)))) x)) y))))))))

(defvar hydra_eval_lib_equality_min (lambda (cx) (lambda (g) (lambda (x) (lambda (y) (list :right (list :application (make-hydra_core_application (list :application (make-hydra_core_application (list :application (make-hydra_core_application (list :variable "hydra.lib.logic.ifElse") (list :application (make-hydra_core_application (list :application (make-hydra_core_application (list :variable "hydra.lib.equality.lte") x)) y)))) x)) y))))))))

(provide 'hydra.eval.lib.equality)
