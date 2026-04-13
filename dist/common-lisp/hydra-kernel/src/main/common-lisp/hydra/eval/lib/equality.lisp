(defpackage :hydra.eval.lib.equality
(:use :cl :hydra.core)
(:export :hydra_eval_lib_equality_identity :hydra_eval_lib_equality_max :hydra_eval_lib_equality_min))

(in-package :hydra.eval.lib.equality)

(cl:defvar hydra_eval_lib_equality_identity (cl:lambda (cx) (cl:lambda (g) (cl:lambda (x) (list :right x)))))

(cl:defvar hydra_eval_lib_equality_max (cl:lambda (cx) (cl:lambda (g) (cl:lambda (x) (cl:lambda (y) (list :right (list :application (make-hydra_core_application (list :application (make-hydra_core_application (list :application (make-hydra_core_application (list :variable "hydra.lib.logic.ifElse") (list :application (make-hydra_core_application (list :application (make-hydra_core_application (list :variable "hydra.lib.equality.gte") x)) y)))) x)) y))))))))

(cl:defvar hydra_eval_lib_equality_min (cl:lambda (cx) (cl:lambda (g) (cl:lambda (x) (cl:lambda (y) (list :right (list :application (make-hydra_core_application (list :application (make-hydra_core_application (list :application (make-hydra_core_application (list :variable "hydra.lib.logic.ifElse") (list :application (make-hydra_core_application (list :application (make-hydra_core_application (list :variable "hydra.lib.equality.lte") x)) y)))) x)) y))))))))
