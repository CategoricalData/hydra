(require 'cl-lib)

(require 'hydra.core)

(require 'hydra.extract.core)

(require 'hydra.lib.eithers)

(require 'hydra.lib.lists)

(require 'hydra.lib.sets)

(defvar hydra_eval_lib_sets_difference (lambda (cx) (lambda (g) (lambda (set1_term) (lambda (set2_term) (funcall (hydra_lib_eithers_bind (funcall (hydra_extract_core_set g) set1_term)) (lambda (elements) (list :right (funcall (funcall (hydra_lib_lists_foldl (lambda (acc) (lambda (el) (list :application (make-hydra_core_application (list :application (make-hydra_core_application (list :application (make-hydra_core_application (list :variable "hydra.lib.logic.ifElse") (list :application (make-hydra_core_application (list :application (make-hydra_core_application (list :variable "hydra.lib.sets.member") el)) set2_term)))) acc)) (list :application (make-hydra_core_application (list :application (make-hydra_core_application (list :variable "hydra.lib.sets.insert") el)) acc))))))) (list :set (hydra_lib_sets_from_list (list)))) (hydra_lib_sets_to_list elements))))))))))

(defvar hydra_eval_lib_sets_intersection (lambda (cx) (lambda (g) (lambda (set1_term) (lambda (set2_term) (funcall (hydra_lib_eithers_bind (funcall (hydra_extract_core_set g) set1_term)) (lambda (elements) (list :right (funcall (funcall (hydra_lib_lists_foldl (lambda (acc) (lambda (el) (list :application (make-hydra_core_application (list :application (make-hydra_core_application (list :application (make-hydra_core_application (list :variable "hydra.lib.logic.ifElse") (list :application (make-hydra_core_application (list :application (make-hydra_core_application (list :variable "hydra.lib.sets.member") el)) set2_term)))) (list :application (make-hydra_core_application (list :application (make-hydra_core_application (list :variable "hydra.lib.sets.insert") el)) acc)))) acc))))) (list :set (hydra_lib_sets_from_list (list)))) (hydra_lib_sets_to_list elements))))))))))

(defvar hydra_eval_lib_sets_map (lambda (cx) (lambda (g) (lambda (fun) (lambda (set_term) (funcall (hydra_lib_eithers_bind (funcall (hydra_extract_core_set g) set_term)) (lambda (elements) (list :right (list :application (make-hydra_core_application (list :variable "hydra.lib.sets.fromList") (list :list (funcall (hydra_lib_lists_map (lambda (el) (list :application (make-hydra_core_application fun el)))) (hydra_lib_sets_to_list elements)))))))))))))

(defvar hydra_eval_lib_sets_union (lambda (cx) (lambda (g) (lambda (set1_term) (lambda (set2_term) (funcall (hydra_lib_eithers_bind (funcall (hydra_extract_core_set g) set1_term)) (lambda (elements) (list :right (funcall (funcall (hydra_lib_lists_foldl (lambda (acc) (lambda (el) (list :application (make-hydra_core_application (list :application (make-hydra_core_application (list :variable "hydra.lib.sets.insert") el)) acc))))) set2_term) (hydra_lib_sets_to_list elements))))))))))

(defvar hydra_eval_lib_sets_unions (lambda (cx) (lambda (g) (lambda (list_term) (funcall (hydra_lib_eithers_bind (funcall (hydra_extract_core_list g) list_term)) (lambda (elements) (list :right (funcall (funcall (hydra_lib_lists_foldl (lambda (acc) (lambda (s) (list :application (make-hydra_core_application (list :application (make-hydra_core_application (list :variable "hydra.lib.sets.union") acc)) s))))) (list :set (hydra_lib_sets_from_list (list)))) elements))))))))

(provide 'hydra.eval.lib.sets)
