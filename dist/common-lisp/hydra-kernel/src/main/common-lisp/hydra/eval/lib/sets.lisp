(defpackage :hydra.eval.lib.sets
(:use :cl :hydra.core :hydra.extract.core :hydra.lib.eithers :hydra.lib.lists :hydra.lib.sets)
(:export :hydra_eval_lib_sets_map))

(in-package :hydra.eval.lib.sets)

(cl:defvar hydra_eval_lib_sets_map (cl:lambda (cx) (cl:lambda (g) (cl:lambda (fun) (cl:lambda (set_term) ((hydra_lib_eithers_bind (((hydra_extract_core_set cx) g) set_term)) (cl:lambda (elements) (list :right (list :application (make-hydra_core_application (list :function (list :primitive "hydra.lib.sets.fromList")) (list :list ((hydra_lib_lists_map (cl:lambda (el) (list :application (make-hydra_core_application fun el)))) (hydra_lib_sets_to_list elements)))))))))))))
