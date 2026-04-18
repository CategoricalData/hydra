(ns hydra.cpp.names
  (:require [hydra.core :refer :all] [hydra.cpp.environment :refer :all] [hydra.cpp.language :refer :all] [hydra.cpp.syntax :refer :all] [hydra.cpp.utils :refer :all] [hydra.formatting :refer :all] [hydra.lib.equality :refer :all] [hydra.lib.lists :refer :all] [hydra.lib.logic :refer :all] [hydra.lib.maps :refer :all] [hydra.lib.maybes :refer :all] [hydra.lib.pairs :refer :all] [hydra.lib.strings :refer :all] [hydra.names :refer :all] [hydra.packaging :refer :all] [hydra.util :refer :all]
))

(declare hydra_cpp_names_sanitize_cpp_name hydra_cpp_names_class_name hydra_cpp_names_encode_name hydra_cpp_names_create_type_reference hydra_cpp_names_encode_enum_value hydra_cpp_names_encode_field_name hydra_cpp_names_encode_name_qualified hydra_cpp_names_encode_namespace hydra_cpp_names_encode_type_variable hydra_cpp_names_fwd_header_name hydra_cpp_names_namespace_decl hydra_cpp_names_partial_visitor_name hydra_cpp_names_variable_reference hydra_cpp_names_term_variable_reference hydra_cpp_names_type_variable_reference hydra_cpp_names_variant_name hydra_cpp_names_visitor_name)

(def hydra_cpp_names_sanitize_cpp_name (hydra_formatting_sanitize_with_underscores hydra_cpp_language_cpp_reserved_words))

(def hydra_cpp_names_class_name (fn [name] (hydra_cpp_names_sanitize_cpp_name (hydra_names_local_name_of name))))

(def hydra_cpp_names_encode_name (fn [is_qualified] (fn [conv] (fn [env] (fn [name] (let [bound_vars (hydra_lib_pairs_second ((fn [v] (:bound_type_variables v)) env)) qual_name (hydra_names_qualify_name name) local ((fn [v] (:local v)) qual_name) cpp_local (hydra_cpp_names_sanitize_cpp_name (((hydra_formatting_convert_case (list :camel nil)) conv) local)) cpp_ns (fn [ns_val] ((hydra_lib_strings_intercalate "::") ((hydra_lib_lists_map ((hydra_formatting_convert_case (list :camel nil)) (list :lower_snake nil))) ((hydra_lib_strings_split_on ".") ((fn [v] v) ns_val))))) focus_ns (hydra_lib_pairs_first ((fn [v] (:focus v)) ((fn [v] (:namespaces v)) env))) mns ((fn [v] (:namespace v)) qual_name)] (if is_qualified (((hydra_lib_maybes_maybe (fn [] (((hydra_lib_maybes_maybe (fn [] cpp_local)) (fn [ns_val] ((hydra_lib_strings_cat2 (cpp_ns ns_val)) ((hydra_lib_strings_cat2 "::") cpp_local)))) mns))) (fn [n] n)) ((hydra_lib_maps_lookup name) bound_vars)) cpp_local)))))))

(def hydra_cpp_names_create_type_reference (fn [is_pointer] (fn [env] (fn [name] (let [base_type (list :basic (list :named ((((hydra_cpp_names_encode_name true) (list :pascal nil)) env) name)))] (if is_pointer (hydra_cpp_utils_to_const_type base_type) base_type))))))

(def hydra_cpp_names_encode_enum_value ((hydra_cpp_names_encode_name false) (list :upper_snake nil)))

(def hydra_cpp_names_encode_field_name (fn [env] (fn [fname] ((((hydra_cpp_names_encode_name false) (list :lower_snake nil)) env) fname))))

(def hydra_cpp_names_encode_name_qualified (fn [env] (fn [name] (let [bound_vars (hydra_lib_pairs_second ((fn [v] (:bound_type_variables v)) env)) focus_ns (hydra_lib_pairs_first ((fn [v] (:focus v)) ((fn [v] (:namespaces v)) env))) qual_name (hydra_names_qualify_name name) local ((fn [v] (:local v)) qual_name) mns ((fn [v] (:namespace v)) qual_name)] (((hydra_lib_maybes_maybe (fn [] (if ((hydra_lib_equality_equal mns) (list :just focus_ns)) (hydra_cpp_names_sanitize_cpp_name local) ((hydra_lib_strings_intercalate "::") ((hydra_lib_lists_map hydra_cpp_names_sanitize_cpp_name) ((hydra_lib_strings_split_on ".") ((fn [v] v) name))))))) (fn [n] n)) ((hydra_lib_maps_lookup name) bound_vars))))))

(def hydra_cpp_names_encode_namespace (fn [ns_val] ((hydra_lib_strings_intercalate "::") ((hydra_lib_lists_map ((hydra_formatting_convert_case (list :camel nil)) (list :lower_snake nil))) ((hydra_lib_strings_split_on ".") ((fn [v] v) ns_val))))))

(def hydra_cpp_names_encode_type_variable (fn [name] (hydra_formatting_capitalize ((fn [v] v) name))))

(def hydra_cpp_names_fwd_header_name (fn [ns_val] (hydra_names_unqualify_name (->hydra_packaging_qualified_name (list :just ns_val) "Fwd"))))

(def hydra_cpp_names_namespace_decl (fn [ns_val] (fn [decls] (list :namespace (->hydra_cpp_syntax_namespace_declaration (hydra_cpp_names_encode_namespace ns_val) decls)))))

(def hydra_cpp_names_partial_visitor_name (fn [name] (hydra_cpp_names_sanitize_cpp_name ((hydra_lib_strings_cat2 (hydra_names_local_name_of name)) "PartialVisitor"))))

(def hydra_cpp_names_variable_reference (fn [conv] (fn [env] (fn [name] (hydra_cpp_utils_create_identifier_expr ((((hydra_cpp_names_encode_name true) conv) env) name))))))

(def hydra_cpp_names_term_variable_reference (hydra_cpp_names_variable_reference (list :lower_snake nil)))

(def hydra_cpp_names_type_variable_reference (fn [env] (fn [name] (list :basic (list :named ((((hydra_cpp_names_encode_name true) (list :pascal nil)) env) name))))))

(def hydra_cpp_names_variant_name (fn [tname] (fn [fname] (hydra_cpp_names_sanitize_cpp_name ((hydra_lib_strings_cat2 (hydra_names_local_name_of tname)) (hydra_formatting_capitalize ((fn [v] v) fname)))))))

(def hydra_cpp_names_visitor_name (fn [name] (hydra_cpp_names_sanitize_cpp_name ((hydra_lib_strings_cat2 (hydra_names_local_name_of name)) "Visitor"))))
