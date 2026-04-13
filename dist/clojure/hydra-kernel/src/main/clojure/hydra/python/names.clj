(ns hydra.python.names
  (:require [hydra.core :refer :all] [hydra.formatting :refer :all] [hydra.lib.equality :refer :all] [hydra.lib.lists :refer :all] [hydra.lib.logic :refer :all] [hydra.lib.maps :refer :all] [hydra.lib.maybes :refer :all] [hydra.lib.pairs :refer :all] [hydra.lib.strings :refer :all] [hydra.names :refer :all] [hydra.packaging :refer :all] [hydra.python.environment :refer :all] [hydra.python.language :refer :all] [hydra.python.serde :refer :all] [hydra.python.syntax :refer :all] [hydra.util :refer :all]
))

(declare hydra_python_names_encode_constant_for_field_name hydra_python_names_encode_constant_for_type_name hydra_python_names_sanitize_python_name hydra_python_names_use_future_annotations hydra_python_names_encode_name hydra_python_names_encode_enum_value hydra_python_names_encode_field_name hydra_python_names_encode_name_qualified hydra_python_names_encode_namespace hydra_python_names_encode_type_variable hydra_python_names_variable_reference hydra_python_names_term_variable_reference hydra_python_names_type_variable_reference hydra_python_names_variant_name)

(def hydra_python_names_encode_constant_for_field_name (fn [env] (fn [tname] (fn [fname] (((hydra_formatting_convert_case (list :camel nil)) (list :upper_snake nil)) ((fn [v] v) fname))))))

(def hydra_python_names_encode_constant_for_type_name (fn [env] (fn [tname] "TYPE_")))

(def hydra_python_names_sanitize_python_name (hydra_formatting_sanitize_with_underscores hydra_python_language_python_reserved_words))

(def hydra_python_names_use_future_annotations true)

(def hydra_python_names_encode_name (fn [is_qualified] (fn [conv] (fn [env] (fn [name] (let [bound_vars (hydra_lib_pairs_second ((fn [v] (:bound_type_variables v)) env)) namespaces ((fn [v] (:namespaces v)) env) focus_pair ((fn [v] (:focus v)) namespaces) focus_ns (hydra_lib_pairs_first focus_pair) qual_name (hydra_names_qualify_name name) local ((fn [v] (:local v)) qual_name) mns ((fn [v] (:namespace v)) qual_name) py_local (hydra_python_names_sanitize_python_name (((hydra_formatting_convert_case (list :camel nil)) conv) local)) py_ns (fn [ns_val] ((hydra_lib_strings_intercalate ".") ((hydra_lib_lists_map ((hydra_formatting_convert_case (list :camel nil)) (list :lower_snake nil))) ((hydra_lib_strings_split_on ".") ((fn [v] v) ns_val)))))] (if is_qualified (((hydra_lib_maybes_maybe (fn [] (if ((hydra_lib_equality_equal mns) (list :just focus_ns)) (if hydra_python_names_use_future_annotations py_local ((hydra_python_serde_escape_python_string true) py_local)) (((hydra_lib_maybes_maybe (fn [] py_local)) (fn [ns_val] ((hydra_lib_strings_cat2 (py_ns ns_val)) ((hydra_lib_strings_cat2 ".") py_local)))) mns)))) (fn [n] n)) ((hydra_lib_maps_lookup name) bound_vars)) py_local)))))))

(def hydra_python_names_encode_enum_value ((hydra_python_names_encode_name false) (list :upper_snake nil)))

(def hydra_python_names_encode_field_name (fn [env] (fn [fname] ((((hydra_python_names_encode_name false) (list :lower_snake nil)) env) fname))))

(def hydra_python_names_encode_name_qualified (fn [env] (fn [name] (let [bound_vars (hydra_lib_pairs_second ((fn [v] (:bound_type_variables v)) env)) namespaces ((fn [v] (:namespaces v)) env) focus_pair ((fn [v] (:focus v)) namespaces) focus_ns (hydra_lib_pairs_first focus_pair) qual_name (hydra_names_qualify_name name) local ((fn [v] (:local v)) qual_name) mns ((fn [v] (:namespace v)) qual_name)] (((hydra_lib_maybes_maybe (fn [] (if ((hydra_lib_equality_equal mns) (list :just focus_ns)) (if hydra_python_names_use_future_annotations local ((hydra_python_serde_escape_python_string true) local)) ((hydra_lib_strings_intercalate ".") ((hydra_lib_lists_map hydra_python_names_sanitize_python_name) ((hydra_lib_strings_split_on ".") ((fn [v] v) name))))))) (fn [n] n)) ((hydra_lib_maps_lookup name) bound_vars))))))

(def hydra_python_names_encode_namespace (fn [ns_val] ((hydra_lib_lists_map (fn [part] (((hydra_formatting_convert_case (list :camel nil)) (list :lower_snake nil)) part))) ((hydra_lib_strings_split_on ".") ((fn [v] v) ns_val)))))

(def hydra_python_names_encode_type_variable (fn [name] (hydra_formatting_capitalize ((fn [v] v) name))))

(def hydra_python_names_variable_reference (fn [conv] (fn [quoted] (fn [env] (fn [name] (let [namespaces ((fn [v] (:namespaces v)) env) focus_pair ((fn [v] (:focus v)) namespaces) focus_ns (hydra_lib_pairs_first focus_pair) mns (hydra_names_namespace_of name) py_name ((((hydra_python_names_encode_name true) conv) env) name) same_namespace (((hydra_lib_maybes_maybe (fn [] false)) (fn [ns_] ((hydra_lib_equality_equal ns_) focus_ns))) mns) unquoted (list :simple (list (list (list :simple (->hydra_python_syntax_comparison (->hydra_python_syntax_bitwise_or (list :nothing) (->hydra_python_syntax_bitwise_xor (list :nothing) (->hydra_python_syntax_bitwise_and (list :nothing) (->hydra_python_syntax_shift_expression (list :nothing) (->hydra_python_syntax_sum (list :nothing) (->hydra_python_syntax_term (list :nothing) (list :simple (->hydra_python_syntax_power (->hydra_python_syntax_await_primary false (list :simple (list :name py_name))) (list :nothing))))))))) (list))))))] (if ((hydra_lib_logic_and quoted) same_namespace) (list :simple (list (list (list :simple (->hydra_python_syntax_comparison (->hydra_python_syntax_bitwise_or (list :nothing) (->hydra_python_syntax_bitwise_xor (list :nothing) (->hydra_python_syntax_bitwise_and (list :nothing) (->hydra_python_syntax_shift_expression (list :nothing) (->hydra_python_syntax_sum (list :nothing) (->hydra_python_syntax_term (list :nothing) (list :simple (->hydra_python_syntax_power (->hydra_python_syntax_await_primary false (list :simple (list :string (->hydra_python_syntax_string ((fn [v] v) py_name) (list :double nil))))) (list :nothing))))))))) (list)))))) unquoted)))))))

(def hydra_python_names_term_variable_reference ((hydra_python_names_variable_reference (list :lower_snake nil)) false))

(def hydra_python_names_type_variable_reference ((hydra_python_names_variable_reference (list :pascal nil)) false))

(def hydra_python_names_variant_name (fn [is_qualified] (fn [env] (fn [tname] (fn [fname] ((((hydra_python_names_encode_name is_qualified) (list :pascal nil)) env) ((hydra_lib_strings_cat2 ((fn [v] v) tname)) (hydra_formatting_capitalize ((fn [v] v) fname)))))))))
