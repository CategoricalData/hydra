(ns hydra.ext.haskell.utils
  (:require [hydra.core :refer :all] [hydra.ext.haskell.ast :refer :all] [hydra.ext.haskell.language :refer :all] [hydra.formatting :refer :all] [hydra.lib.eithers :refer :all] [hydra.lib.equality :refer :all] [hydra.lib.lists :refer :all] [hydra.lib.logic :refer :all] [hydra.lib.maps :refer :all] [hydra.lib.maybes :refer :all] [hydra.lib.pairs :refer :all] [hydra.lib.sets :refer :all] [hydra.lib.strings :refer :all] [hydra.module :refer :all] [hydra.names :refer :all] [hydra.rewriting :refer :all] [hydra.schemas :refer :all]
))

(declare hydra_ext_haskell_utils_application_pattern hydra_ext_haskell_utils_raw_name hydra_ext_haskell_utils_sanitize_haskell_name hydra_ext_haskell_utils_simple_name hydra_ext_haskell_utils_element_reference hydra_ext_haskell_utils_hsapp hydra_ext_haskell_utils_hslambda hydra_ext_haskell_utils_hslit hydra_ext_haskell_utils_hsvar hydra_ext_haskell_utils_namespaces_for_module hydra_ext_haskell_utils_newtype_accessor_name hydra_ext_haskell_utils_type_name_for_record hydra_ext_haskell_utils_record_field_reference hydra_ext_haskell_utils_simple_value_binding hydra_ext_haskell_utils_to_type_application hydra_ext_haskell_utils_union_field_reference hydra_ext_haskell_utils_unpack_forall_type)

(def hydra_ext_haskell_utils_application_pattern (fn [name] (fn [args] (list :application (->hydra_ext_haskell_ast_application_pattern name args)))))

(def hydra_ext_haskell_utils_raw_name (fn [n] (list :normal (->hydra_ext_haskell_ast_qualified_name (list) n))))

(def hydra_ext_haskell_utils_sanitize_haskell_name (hydra_formatting_sanitize_with_underscores hydra_ext_haskell_language_reserved_words))

(def hydra_ext_haskell_utils_simple_name (fn [arg_] (hydra_ext_haskell_utils_raw_name (hydra_ext_haskell_utils_sanitize_haskell_name arg_))))

(def hydra_ext_haskell_utils_element_reference (fn [namespaces] (fn [name] (let [qname (hydra_names_qualify_name name) local ((fn [v] (:local v)) qname) esc_local (hydra_ext_haskell_utils_sanitize_haskell_name local) namespace_pair ((fn [v] (:focus v)) namespaces) gmod ((fn [v] v) (hydra_lib_pairs_second namespace_pair)) gname (hydra_lib_pairs_first namespace_pair) mns ((fn [v] (:namespace v)) qname) namespaces_map ((fn [v] (:mapping v)) namespaces)] (((hydra_lib_maybes_cases ((fn [v] (:namespace v)) qname)) (fn [] (hydra_ext_haskell_utils_simple_name local))) (fn [ns_] (((hydra_lib_maybes_cases ((hydra_lib_maps_lookup ns_) namespaces_map)) (fn [] (hydra_ext_haskell_utils_simple_name local))) (fn [mn] (let [alias_str ((fn [v] v) mn)] (if ((hydra_lib_equality_equal ns_) gname) (hydra_ext_haskell_utils_simple_name esc_local) (hydra_ext_haskell_utils_raw_name (hydra_lib_strings_cat (list alias_str "." (hydra_ext_haskell_utils_sanitize_haskell_name local))))))))))))))

(def hydra_ext_haskell_utils_hsapp (fn [l] (fn [r] (list :application (->hydra_ext_haskell_ast_application_expression l r)))))

(def hydra_ext_haskell_utils_hslambda (fn [name] (fn [rhs] (list :lambda (->hydra_ext_haskell_ast_lambda_expression (list (list :name name)) rhs)))))

(def hydra_ext_haskell_utils_hslit (fn [lit] (list :literal lit)))

(def hydra_ext_haskell_utils_hsvar (fn [s] (list :variable (hydra_ext_haskell_utils_raw_name s))))

(def hydra_ext_haskell_utils_namespaces_for_module (fn [mod] (fn [cx] (fn [g] ((hydra_lib_eithers_bind (((((((hydra_schemas_module_dependency_namespaces cx) g) true) true) true) true) mod)) (fn [nss] (let [ns_ ((fn [v] (:namespace v)) mod)] (let [to_module_name (fn [namespace] (let [namespace_str ((fn [v] v) namespace) parts ((hydra_lib_strings_split_on ".") namespace_str) last_part (hydra_lib_lists_last parts) capitalized (hydra_formatting_capitalize last_part)] capitalized))] (let [to_pair (fn [name] (list name (to_module_name name)))] (let [add_pair (fn add_pair [_arg] ((fn [state] (fn [name_pair] (let [alias (hydra_lib_pairs_second name_pair) alias_str ((fn [v] v) alias) current_map (hydra_lib_pairs_first state) current_set (hydra_lib_pairs_second state) name (hydra_lib_pairs_first name_pair)] (if ((hydra_lib_sets_member alias) current_set) ((add_pair state) (list name ((hydra_lib_strings_cat2 alias_str) "_"))) (list (((hydra_lib_maps_insert name) alias) current_map) ((hydra_lib_sets_insert alias) current_set)))))) _arg))] (let [focus_pair (to_pair ns_)] (let [nss_as_list (hydra_lib_sets_to_list nss)] (let [nss_pairs ((hydra_lib_lists_map to_pair) nss_as_list)] (let [empty_state (list hydra_lib_maps_empty hydra_lib_sets_empty)] (let [final_state (((hydra_lib_lists_foldl add_pair) empty_state) nss_pairs)] (let [result_map (hydra_lib_pairs_first final_state)] (list :right (->hydra_module_namespaces focus_pair result_map))))))))))))))))))

(def hydra_ext_haskell_utils_newtype_accessor_name (fn [name] ((hydra_lib_strings_cat2 "un") (hydra_names_local_name_of name))))

(def hydra_ext_haskell_utils_type_name_for_record (fn [sname] (let [sname_str ((fn [v] v) sname) parts ((hydra_lib_strings_split_on ".") sname_str)] (hydra_lib_lists_last parts))))

(def hydra_ext_haskell_utils_record_field_reference (fn [namespaces] (fn [sname] (fn [fname] (let [fname_str ((fn [v] v) fname) capitalized (hydra_formatting_capitalize fname_str) type_name_str (hydra_ext_haskell_utils_type_name_for_record sname) decapitalized (hydra_formatting_decapitalize type_name_str) nm ((hydra_lib_strings_cat2 decapitalized) capitalized) qname (hydra_names_qualify_name sname) ns_ ((fn [v] (:namespace v)) qname) qual_name (->hydra_module_qualified_name ns_ nm) unqual_name (hydra_names_unqualify_name qual_name)] ((hydra_ext_haskell_utils_element_reference namespaces) unqual_name))))))

(def hydra_ext_haskell_utils_simple_value_binding (fn [hname] (fn [rhs] (fn [bindings] (let [pat (list :application (->hydra_ext_haskell_ast_application_pattern hname (list))) right_hand_side rhs] (list :simple (->hydra_ext_haskell_ast_simple_value_binding pat right_hand_side bindings)))))))

(def hydra_ext_haskell_utils_to_type_application (fn [types] (let [app (fn app [l] (if ((hydra_lib_equality_gt (hydra_lib_lists_length l)) 1) (list :application (->hydra_ext_haskell_ast_application_type (app (hydra_lib_lists_tail l)) (hydra_lib_lists_head l))) (hydra_lib_lists_head l)))] (app (hydra_lib_lists_reverse types)))))

(def hydra_ext_haskell_utils_union_field_reference (fn [bound_names] (fn [namespaces] (fn [sname] (fn [fname] (let [fname_str ((fn [v] v) fname) capitalized_field_name (hydra_formatting_capitalize fname_str) type_name_str (hydra_ext_haskell_utils_type_name_for_record sname) capitalized_type_name (hydra_formatting_capitalize type_name_str) qname (hydra_names_qualify_name sname) ns_ ((fn [v] (:namespace v)) qname) deconflict (fn deconflict [name] (let [tname (hydra_names_unqualify_name (->hydra_module_qualified_name ns_ name))] (if ((hydra_lib_sets_member tname) bound_names) (deconflict ((hydra_lib_strings_cat2 name) "_")) name))) nm (deconflict ((hydra_lib_strings_cat2 capitalized_type_name) capitalized_field_name)) qual_name (->hydra_module_qualified_name ns_ nm) unqual_name (hydra_names_unqualify_name qual_name)] ((hydra_ext_haskell_utils_element_reference namespaces) unqual_name)))))))

(def hydra_ext_haskell_utils_unpack_forall_type (fn [t_] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :forall) ((fn [fat] (let [tbody ((fn [v] (:body v)) fat) recursive_result (hydra_ext_haskell_utils_unpack_forall_type tbody) final_type (hydra_lib_pairs_second recursive_result) v ((fn [v] (:parameter v)) fat) vars (hydra_lib_pairs_first recursive_result)] (list ((hydra_lib_lists_cons v) vars) final_type))) match_value) :else (list (list) t_))) (second match_target))) (hydra_rewriting_deannotate_type t_))))
