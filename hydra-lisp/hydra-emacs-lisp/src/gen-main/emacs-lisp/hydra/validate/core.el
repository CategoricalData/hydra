(require 'cl-lib)

(require 'hydra.accessors)

(require 'hydra.core)

(require 'hydra.error.core)

(require 'hydra.lib.lists)

(require 'hydra.lib.logic)

(require 'hydra.lib.maybes)

(require 'hydra.lib.pairs)

(require 'hydra.lib.sets)

(require 'hydra.rewriting)

(defvar hydra_validate_core_find_duplicate (lambda (names) (let ((result (funcall (funcall (hydra_lib_lists_foldl (lambda (acc) (lambda (name) (let ((seen (hydra_lib_pairs_first acc))) (let ((dup (hydra_lib_pairs_second acc))) (funcall (funcall (hydra_lib_maybes_cases dup) (lambda () (if (funcall (hydra_lib_sets_member name) seen) (list seen (list :just name)) (list (funcall (hydra_lib_sets_insert name) seen) (list :nothing))))) (lambda (_) acc))))))) (list hydra_lib_sets_empty (list :nothing))) names))) (hydra_lib_pairs_second result))))

(defvar hydra_validate_core_check_duplicate_bindings (lambda (path) (lambda (bindings) (let ((names (funcall (hydra_lib_lists_map (lambda (v) (hydra_core_binding-name v))) bindings))) (let ((dup (hydra_validate_core_find_duplicate names))) (funcall (hydra_lib_maybes_map (lambda (name) (list :duplicate_binding (make-hydra_error_core_duplicate_binding_error path name)))) dup))))))

(defvar hydra_validate_core_check_duplicate_fields (lambda (path) (lambda (names) (let ((dup (hydra_validate_core_find_duplicate names))) (funcall (hydra_lib_maybes_map (lambda (name) (list :duplicate_field (make-hydra_error_core_duplicate_field_error path name)))) dup)))))

(defvar hydra_validate_core_check_term (lambda (path) (lambda (term) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :let) (funcall (lambda (lt) (funcall (hydra_validate_core_check_duplicate_bindings path) (funcall (lambda (v) (hydra_core_let-bindings v)) lt))) match_value)) ((equal (car match_target) :record) (funcall (lambda (rec) (funcall (hydra_validate_core_check_duplicate_fields path) (funcall (hydra_lib_lists_map (lambda (v) (hydra_core_field-name v))) (funcall (lambda (v) (hydra_core_record-fields v)) rec)))) match_value)) (t (list :nothing)))) (cadr match_target))) term))))

(defvar hydra_validate_core_term (lambda (g) (lambda (t_) (funcall (funcall (funcall (hydra_rewriting_fold_term_with_graph_and_path (lambda (recurse) (lambda (path) (lambda (cx) (lambda (acc) (lambda (trm) (funcall (funcall (hydra_lib_maybes_cases acc) (lambda () (let ((check_result (funcall (hydra_validate_core_check_term path) trm))) (funcall (funcall (hydra_lib_maybes_cases check_result) (lambda () (funcall (recurse (list :nothing)) trm))) (lambda (err) (list :just err)))))) (lambda (_) acc)))))))) g) (list :nothing)) t_))))

(provide 'hydra.validate.core)
