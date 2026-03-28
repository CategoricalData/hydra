(require 'cl-lib)

(require 'hydra.core)

(require 'hydra.error.packaging)

(require 'hydra.formatting)

(require 'hydra.lib.equality)

(require 'hydra.lib.lists)

(require 'hydra.lib.logic)

(require 'hydra.lib.maps)

(require 'hydra.lib.maybes)

(require 'hydra.lib.pairs)

(require 'hydra.lib.sets)

(require 'hydra.lib.strings)

(require 'hydra.module)

(require 'hydra.names)

(require 'hydra.packaging)

(defvar hydra_validate_packaging_check_conflicting_module_namespaces (lambda (pkg) (let ((result (funcall (funcall (hydra_lib_lists_foldl (lambda (acc) (lambda (mod) (let ((seen (hydra_lib_pairs_first acc))) (let ((err (hydra_lib_pairs_second acc))) (funcall (funcall (hydra_lib_maybes_cases err) (lambda () (let ((ns_ (funcall (lambda (v) (hydra_module_module-namespace v)) mod))) (let ((key (hydra_lib_strings_to_lower (funcall (lambda (v) v) ns_)))) (let ((existing (funcall (hydra_lib_maps_lookup key) seen))) (funcall (funcall (hydra_lib_maybes_cases existing) (lambda () (list (funcall (funcall (hydra_lib_maps_insert key) ns_) seen) (list :nothing)))) (lambda (first) (list seen (list :just (list :conflicting_module_namespace (make-hydra_error_packaging_conflicting_module_namespace_error first ns_))))))))))) (lambda (_) acc))))))) (list hydra_lib_maps_empty (list :nothing))) (funcall (lambda (v) (hydra_packaging_package-modules v)) pkg)))) (hydra_lib_pairs_second result))))

(defvar hydra_validate_packaging_definition_name (lambda (def_) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :term) (funcall (lambda (td) (funcall (lambda (v) (hydra_module_term_definition-name v)) td)) match_value)) ((equal (car match_target) :type) (funcall (lambda (td) (funcall (lambda (v) (hydra_module_type_definition-name v)) td)) match_value)))) (cadr match_target))) def_)))

(defvar hydra_validate_packaging_check_conflicting_variant_names (lambda (mod) (let ((ns_ (funcall (lambda (v) (hydra_module_module-namespace v)) mod))) (let ((defs (funcall (lambda (v) (hydra_module_module-definitions v)) mod))) (let ((def_names (funcall (funcall (hydra_lib_lists_foldl (lambda (acc) (lambda (def_) (funcall (hydra_lib_sets_insert (hydra_names_local_name_of (hydra_validate_packaging_definition_name def_))) acc)))) hydra_lib_sets_empty) defs))) (funcall (funcall (hydra_lib_lists_foldl (lambda (acc) (lambda (def_) (funcall (funcall (hydra_lib_maybes_cases acc) (lambda () (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :type) (funcall (lambda (td) (let ((type_name (funcall (lambda (v) (hydra_module_type_definition-name v)) td))) (let ((local_type_name (hydra_names_local_name_of type_name))) (let ((typ (funcall (lambda (v) (hydra_module_type_definition-type v)) td))) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :union) (funcall (lambda (fields) (funcall (funcall (hydra_lib_lists_foldl (lambda (inner_acc) (lambda (field) (funcall (funcall (hydra_lib_maybes_cases inner_acc) (lambda () (let ((field_name (funcall (lambda (v) (hydra_core_field_type-name v)) field))) (let ((local_field_name (hydra_names_local_name_of field_name))) (let ((constructor_name (funcall (hydra_lib_strings_cat2 (hydra_formatting_capitalize local_type_name)) (hydra_formatting_capitalize local_field_name)))) (if (funcall (hydra_lib_sets_member constructor_name) def_names) (list :just (list :conflicting_variant_name (make-hydra_error_packaging_conflicting_variant_name_error ns_ type_name field_name constructor_name))) (list :nothing))))))) (lambda (_) inner_acc))))) (list :nothing)) fields)) match_value)) (t (list :nothing)))) (cadr match_target))) typ))))) match_value)) (t (list :nothing)))) (cadr match_target))) def_))) (lambda (_) acc))))) (list :nothing)) defs))))))

(defvar hydra_validate_packaging_check_definition_namespaces (lambda (mod) (let ((ns_ (funcall (lambda (v) (hydra_module_module-namespace v)) mod))) (let ((prefix (funcall (hydra_lib_strings_cat2 (funcall (lambda (v) v) ns_)) "."))) (let ((prefix_len (hydra_lib_strings_length prefix))) (funcall (funcall (hydra_lib_lists_foldl (lambda (acc) (lambda (def_) (funcall (funcall (hydra_lib_maybes_cases acc) (lambda () (let ((name (hydra_validate_packaging_definition_name def_))) (let ((name_str (funcall (lambda (v) v) name))) (let ((name_prefix (funcall (hydra_lib_lists_take prefix_len) (hydra_lib_strings_to_list name_str)))) (if (funcall (hydra_lib_equality_equal (hydra_lib_strings_from_list name_prefix)) prefix) (list :nothing) (list :just (list :definition_not_in_module_namespace (make-hydra_error_packaging_definition_not_in_module_namespace_error ns_ name))))))))) (lambda (_) acc))))) (list :nothing)) (funcall (lambda (v) (hydra_module_module-definitions v)) mod)))))))

(defvar hydra_validate_packaging_check_duplicate_definition_names (lambda (mod) (let ((ns_ (funcall (lambda (v) (hydra_module_module-namespace v)) mod))) (let ((result (funcall (funcall (hydra_lib_lists_foldl (lambda (acc) (lambda (def_) (let ((seen (hydra_lib_pairs_first acc))) (let ((err (hydra_lib_pairs_second acc))) (funcall (funcall (hydra_lib_maybes_cases err) (lambda () (let ((name (hydra_validate_packaging_definition_name def_))) (if (funcall (hydra_lib_sets_member name) seen) (list seen (list :just (list :duplicate_definition_name (make-hydra_error_packaging_duplicate_definition_name_error ns_ name)))) (list (funcall (hydra_lib_sets_insert name) seen) (list :nothing)))))) (lambda (_) acc))))))) (list hydra_lib_sets_empty (list :nothing))) (funcall (lambda (v) (hydra_module_module-definitions v)) mod)))) (hydra_lib_pairs_second result)))))

(defvar hydra_validate_packaging_check_duplicate_module_namespaces (lambda (pkg) (let ((result (funcall (funcall (hydra_lib_lists_foldl (lambda (acc) (lambda (mod) (let ((seen (hydra_lib_pairs_first acc))) (let ((err (hydra_lib_pairs_second acc))) (funcall (funcall (hydra_lib_maybes_cases err) (lambda () (let ((ns_ (funcall (lambda (v) (hydra_module_module-namespace v)) mod))) (if (funcall (hydra_lib_sets_member ns_) seen) (list seen (list :just (list :duplicate_module_namespace (make-hydra_error_packaging_duplicate_module_namespace_error ns_)))) (list (funcall (hydra_lib_sets_insert ns_) seen) (list :nothing)))))) (lambda (_) acc))))))) (list hydra_lib_sets_empty (list :nothing))) (funcall (lambda (v) (hydra_packaging_package-modules v)) pkg)))) (hydra_lib_pairs_second result))))

(defvar hydra_validate_packaging_module (lambda (mod) (let ((r1 (hydra_validate_packaging_check_definition_namespaces mod))) (funcall (funcall (hydra_lib_maybes_cases r1) (lambda () (let ((r2 (hydra_validate_packaging_check_duplicate_definition_names mod))) (funcall (funcall (hydra_lib_maybes_cases r2) (lambda () (hydra_validate_packaging_check_conflicting_variant_names mod))) (lambda (_) r2))))) (lambda (_) r1)))))

(defvar hydra_validate_packaging_package (lambda (pkg) (let ((r1 (hydra_validate_packaging_check_duplicate_module_namespaces pkg))) (funcall (funcall (hydra_lib_maybes_cases r1) (lambda () (let ((r2 (hydra_validate_packaging_check_conflicting_module_namespaces pkg))) (funcall (funcall (hydra_lib_maybes_cases r2) (lambda () (funcall (funcall (hydra_lib_lists_foldl (lambda (acc) (lambda (mod) (funcall (funcall (hydra_lib_maybes_cases acc) (lambda () (funcall (hydra_lib_maybes_map (lambda (err) (list :invalid_module err))) (hydra_validate_packaging_module mod)))) (lambda (_) acc))))) (list :nothing)) (funcall (lambda (v) (hydra_packaging_package-modules v)) pkg)))) (lambda (_) r2))))) (lambda (_) r1)))))

(provide 'hydra.validate.packaging)
