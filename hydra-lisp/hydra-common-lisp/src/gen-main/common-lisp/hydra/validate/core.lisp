(defpackage :hydra.validate.core
(:use :cl :hydra.accessors :hydra.core :hydra.error.core :hydra.lib.lists :hydra.lib.logic :hydra.lib.maybes :hydra.lib.pairs :hydra.lib.sets :hydra.rewriting)
(:export :hydra_validate_core_find_duplicate :hydra_validate_core_check_duplicate_bindings :hydra_validate_core_check_duplicate_fields :hydra_validate_core_check_term :hydra_validate_core_term))

(in-package :hydra.validate.core)

(cl:defvar hydra_validate_core_find_duplicate (cl:lambda (names) (let ((result (((hydra_lib_lists_foldl (cl:lambda (acc) (cl:lambda (name) (let ((seen (hydra_lib_pairs_first acc))) (let ((dup (hydra_lib_pairs_second acc))) (((hydra_lib_maybes_cases dup) (cl:lambda () (if ((hydra_lib_sets_member name) seen) (cl:list seen (list :just name)) (cl:list ((hydra_lib_sets_insert name) seen) (list :nothing))))) (cl:lambda (_) acc))))))) (cl:list hydra_lib_sets_empty (list :nothing))) names))) (hydra_lib_pairs_second result))))

(cl:defvar hydra_validate_core_check_duplicate_bindings (cl:lambda (path) (cl:lambda (bindings) (let ((names ((hydra_lib_lists_map (cl:lambda (v) (hydra_core_binding-name v))) bindings))) (let ((dup (hydra_validate_core_find_duplicate names))) ((hydra_lib_maybes_map (cl:lambda (name) (list :duplicate_binding (make-hydra_error_core_duplicate_binding_error path name)))) dup))))))

(cl:defvar hydra_validate_core_check_duplicate_fields (cl:lambda (path) (cl:lambda (names) (let ((dup (hydra_validate_core_find_duplicate names))) ((hydra_lib_maybes_map (cl:lambda (name) (list :duplicate_field (make-hydra_error_core_duplicate_field_error path name)))) dup)))))

(cl:defvar hydra_validate_core_check_term (cl:lambda (path) (cl:lambda (term) ((cl:lambda (match_target) ((cl:lambda (match_value) (cond ((equal (car match_target) :let) ((cl:lambda (lt) ((hydra_validate_core_check_duplicate_bindings path) ((cl:lambda (v) (hydra_core_let-bindings v)) lt))) match_value)) ((equal (car match_target) :record) ((cl:lambda (rec) ((hydra_validate_core_check_duplicate_fields path) ((hydra_lib_lists_map (cl:lambda (v) (hydra_core_field-name v))) ((cl:lambda (v) (hydra_core_record-fields v)) rec)))) match_value)) (t (list :nothing)))) (cadr match_target))) term))))

(cl:defvar hydra_validate_core_term (cl:lambda (g) (cl:lambda (t_) ((((hydra_rewriting_fold_term_with_graph_and_path (cl:lambda (recurse) (cl:lambda (path) (cl:lambda (cx) (cl:lambda (acc) (cl:lambda (trm) (((hydra_lib_maybes_cases acc) (cl:lambda () (let ((check_result ((hydra_validate_core_check_term path) trm))) (((hydra_lib_maybes_cases check_result) (cl:lambda () ((recurse (list :nothing)) trm))) (cl:lambda (err) (list :just err)))))) (cl:lambda (_) acc)))))))) g) (list :nothing)) t_))))
