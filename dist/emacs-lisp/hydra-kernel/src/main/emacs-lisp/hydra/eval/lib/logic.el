(require 'cl-lib)

(require 'hydra.core)

(defvar hydra_eval_lib_logic_and (lambda (cx) (lambda (g) (lambda (a) (lambda (b) (list :right (list :application (make-hydra_core_application (list :application (make-hydra_core_application (list :application (make-hydra_core_application (list :variable "hydra.lib.logic.ifElse") a)) b)) (list :literal (list :boolean nil))))))))))

(defvar hydra_eval_lib_logic_not (lambda (cx) (lambda (g) (lambda (a) (list :right (list :application (make-hydra_core_application (list :application (make-hydra_core_application (list :application (make-hydra_core_application (list :variable "hydra.lib.logic.ifElse") a)) (list :literal (list :boolean nil)))) (list :literal (list :boolean t)))))))))

(defvar hydra_eval_lib_logic_or (lambda (cx) (lambda (g) (lambda (a) (lambda (b) (list :right (list :application (make-hydra_core_application (list :application (make-hydra_core_application (list :application (make-hydra_core_application (list :variable "hydra.lib.logic.ifElse") a)) (list :literal (list :boolean t)))) b))))))))

(provide 'hydra.eval.lib.logic)
