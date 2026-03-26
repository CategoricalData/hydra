(require 'cl-lib)

(require 'hydra.core)

(require 'hydra.ext.python.environment)

(require 'hydra.ext.python.language)

(require 'hydra.ext.python.serde)

(require 'hydra.ext.python.syntax)

(require 'hydra.formatting)

(require 'hydra.lib.equality)

(require 'hydra.lib.lists)

(require 'hydra.lib.logic)

(require 'hydra.lib.maps)

(require 'hydra.lib.maybes)

(require 'hydra.lib.pairs)

(require 'hydra.lib.strings)

(require 'hydra.module)

(require 'hydra.names)

(require 'hydra.util)

(defvar hydra_ext_python_names_encode_constant_for_field_name (lambda (env) (lambda (tname) (lambda (fname) (funcall (funcall (hydra_formatting_convert_case (list :camel nil)) (list :upper_snake nil)) (funcall (lambda (v) v) fname))))))

(defvar hydra_ext_python_names_encode_constant_for_type_name (lambda (env) (lambda (tname) "TYPE_")))

(defvar hydra_ext_python_names_sanitize_python_name (hydra_formatting_sanitize_with_underscores hydra_ext_python_language_python_reserved_words))

(defvar hydra_ext_python_names_use_future_annotations t)

(defvar hydra_ext_python_names_encode_name (lambda (is_qualified) (lambda (conv) (lambda (env) (lambda (name) (let* ((bound_vars (hydra_lib_pairs_second (funcall (lambda (v) (hydra_ext_python_environment_python_environment-bound_type_variables v)) env))) (namespaces (funcall (lambda (v) (hydra_ext_python_environment_python_environment-namespaces v)) env)) (focus_pair (funcall (lambda (v) (hydra_module_namespaces-focus v)) namespaces)) (focus_ns (hydra_lib_pairs_first focus_pair)) (qual_name (hydra_names_qualify_name name)) (local (funcall (lambda (v) (hydra_module_qualified_name-local v)) qual_name)) (mns (funcall (lambda (v) (hydra_module_qualified_name-namespace v)) qual_name)) (py_local (hydra_ext_python_names_sanitize_python_name (funcall (funcall (hydra_formatting_convert_case (list :camel nil)) conv) local))) (py_ns (lambda (ns_val) (funcall (hydra_lib_strings_intercalate ".") (funcall (hydra_lib_lists_map (funcall (hydra_formatting_convert_case (list :camel nil)) (list :lower_snake nil))) (funcall (hydra_lib_strings_split_on ".") (funcall (lambda (v) v) ns_val))))))) (if is_qualified (funcall (funcall (hydra_lib_maybes_maybe (lambda () (if (funcall (hydra_lib_equality_equal mns) (list :just focus_ns)) (if hydra_ext_python_names_use_future_annotations py_local (funcall (hydra_ext_python_serde_escape_python_string t) py_local)) (funcall (funcall (hydra_lib_maybes_maybe (lambda () py_local)) (lambda (ns_val) (funcall (hydra_lib_strings_cat2 (py_ns ns_val)) (funcall (hydra_lib_strings_cat2 ".") py_local)))) mns)))) (lambda (n) n)) (funcall (hydra_lib_maps_lookup name) bound_vars)) py_local)))))))

(defvar hydra_ext_python_names_encode_enum_value (funcall (hydra_ext_python_names_encode_name nil) (list :upper_snake nil)))

(defvar hydra_ext_python_names_encode_field_name (lambda (env) (lambda (fname) (funcall (funcall (funcall (hydra_ext_python_names_encode_name nil) (list :lower_snake nil)) env) fname))))

(defvar hydra_ext_python_names_encode_name_qualified (lambda (env) (lambda (name) (let* ((bound_vars (hydra_lib_pairs_second (funcall (lambda (v) (hydra_ext_python_environment_python_environment-bound_type_variables v)) env))) (namespaces (funcall (lambda (v) (hydra_ext_python_environment_python_environment-namespaces v)) env)) (focus_pair (funcall (lambda (v) (hydra_module_namespaces-focus v)) namespaces)) (focus_ns (hydra_lib_pairs_first focus_pair)) (qual_name (hydra_names_qualify_name name)) (local (funcall (lambda (v) (hydra_module_qualified_name-local v)) qual_name)) (mns (funcall (lambda (v) (hydra_module_qualified_name-namespace v)) qual_name))) (funcall (funcall (hydra_lib_maybes_maybe (lambda () (if (funcall (hydra_lib_equality_equal mns) (list :just focus_ns)) (if hydra_ext_python_names_use_future_annotations local (funcall (hydra_ext_python_serde_escape_python_string t) local)) (funcall (hydra_lib_strings_intercalate ".") (funcall (hydra_lib_lists_map hydra_ext_python_names_sanitize_python_name) (funcall (hydra_lib_strings_split_on ".") (funcall (lambda (v) v) name))))))) (lambda (n) n)) (funcall (hydra_lib_maps_lookup name) bound_vars))))))

(defvar hydra_ext_python_names_encode_namespace (lambda (ns_val) (funcall (hydra_lib_lists_map (lambda (part) (funcall (funcall (hydra_formatting_convert_case (list :camel nil)) (list :lower_snake nil)) part))) (funcall (hydra_lib_strings_split_on ".") (funcall (lambda (v) v) ns_val)))))

(defvar hydra_ext_python_names_encode_type_variable (lambda (name) (hydra_formatting_capitalize (funcall (lambda (v) v) name))))

(defvar hydra_ext_python_names_variable_reference (lambda (conv) (lambda (quoted) (lambda (env) (lambda (name) (let* ((namespaces (funcall (lambda (v) (hydra_ext_python_environment_python_environment-namespaces v)) env)) (focus_pair (funcall (lambda (v) (hydra_module_namespaces-focus v)) namespaces)) (focus_ns (hydra_lib_pairs_first focus_pair)) (mns (hydra_names_namespace_of name)) (py_name (funcall (funcall (funcall (hydra_ext_python_names_encode_name t) conv) env) name)) (same_namespace (funcall (funcall (hydra_lib_maybes_maybe (lambda () nil)) (lambda (ns_) (funcall (hydra_lib_equality_equal ns_) focus_ns))) mns)) (unquoted (list :simple (list (list (list :simple (make-hydra_ext_python_syntax_comparison (make-hydra_ext_python_syntax_bitwise_or (list :nothing) (make-hydra_ext_python_syntax_bitwise_xor (list :nothing) (make-hydra_ext_python_syntax_bitwise_and (list :nothing) (make-hydra_ext_python_syntax_shift_expression (list :nothing) (make-hydra_ext_python_syntax_sum (list :nothing) (make-hydra_ext_python_syntax_term (list :nothing) (list :simple (make-hydra_ext_python_syntax_power (make-hydra_ext_python_syntax_await_primary nil (list :simple (list :name py_name))) (list :nothing))))))))) (list)))))))) (if (funcall (hydra_lib_logic_and quoted) same_namespace) (list :simple (list (list (list :simple (make-hydra_ext_python_syntax_comparison (make-hydra_ext_python_syntax_bitwise_or (list :nothing) (make-hydra_ext_python_syntax_bitwise_xor (list :nothing) (make-hydra_ext_python_syntax_bitwise_and (list :nothing) (make-hydra_ext_python_syntax_shift_expression (list :nothing) (make-hydra_ext_python_syntax_sum (list :nothing) (make-hydra_ext_python_syntax_term (list :nothing) (list :simple (make-hydra_ext_python_syntax_power (make-hydra_ext_python_syntax_await_primary nil (list :simple (list :string (make-hydra_ext_python_syntax_string (funcall (lambda (v) v) py_name) (list :double nil))))) (list :nothing))))))))) (list)))))) unquoted)))))))

(defvar hydra_ext_python_names_term_variable_reference (funcall (hydra_ext_python_names_variable_reference (list :lower_snake nil)) nil))

(defvar hydra_ext_python_names_type_variable_reference (funcall (hydra_ext_python_names_variable_reference (list :pascal nil)) nil))

(defvar hydra_ext_python_names_variant_name (lambda (is_qualified) (lambda (env) (lambda (tname) (lambda (fname) (funcall (funcall (funcall (hydra_ext_python_names_encode_name is_qualified) (list :pascal nil)) env) (funcall (hydra_lib_strings_cat2 (funcall (lambda (v) v) tname)) (hydra_formatting_capitalize (funcall (lambda (v) v) fname)))))))))

(provide 'hydra.ext.python.names)
