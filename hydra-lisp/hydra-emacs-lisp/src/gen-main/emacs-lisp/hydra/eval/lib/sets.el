(require 'cl-lib)

(require 'hydra.core)

(require 'hydra.extract.core)

(require 'hydra.lib.eithers)

(require 'hydra.lib.lists)

(require 'hydra.lib.sets)

(defvar hydra_eval_lib_sets_map (lambda (cx) (lambda (g) (lambda (fun) (lambda (set_term) (funcall (hydra_lib_eithers_bind (funcall (funcall (hydra_extract_core_set cx) g) set_term)) (lambda (elements) (list :right (list :application (make-hydra_core_application (list :function (list :primitive "hydra.lib.sets.fromList")) (list :list (funcall (hydra_lib_lists_map (lambda (el) (list :application (make-hydra_core_application fun el)))) (hydra_lib_sets_to_list elements)))))))))))))

(provide 'hydra.eval.lib.sets)
