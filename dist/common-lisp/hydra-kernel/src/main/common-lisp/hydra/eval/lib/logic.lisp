(defpackage :hydra.eval.lib.logic
(:use :cl :hydra.core)
(:export :hydra_eval_lib_logic_and :hydra_eval_lib_logic_not :hydra_eval_lib_logic_or))

(in-package :hydra.eval.lib.logic)

(cl:defvar hydra_eval_lib_logic_and (cl:lambda (cx) (cl:lambda (g) (cl:lambda (a) (cl:lambda (b) (list :right (list :application (make-hydra_core_application (list :application (make-hydra_core_application (list :application (make-hydra_core_application (list :variable "hydra.lib.logic.ifElse") a)) b)) (list :literal (list :boolean cl:nil))))))))))

(cl:defvar hydra_eval_lib_logic_not (cl:lambda (cx) (cl:lambda (g) (cl:lambda (a) (list :right (list :application (make-hydra_core_application (list :application (make-hydra_core_application (list :application (make-hydra_core_application (list :variable "hydra.lib.logic.ifElse") a)) (list :literal (list :boolean cl:nil)))) (list :literal (list :boolean cl:t)))))))))

(cl:defvar hydra_eval_lib_logic_or (cl:lambda (cx) (cl:lambda (g) (cl:lambda (a) (cl:lambda (b) (list :right (list :application (make-hydra_core_application (list :application (make-hydra_core_application (list :application (make-hydra_core_application (list :variable "hydra.lib.logic.ifElse") a)) (list :literal (list :boolean cl:t)))) b))))))))
