(ns hydra.graphql.serde
  (:require [hydra.graphql.syntax :refer :all] [hydra.lib.lists :refer :all] [hydra.lib.maybes :refer :all] [hydra.serialization :refer :all]
))

(declare hydra_graphql_serde_expr_name hydra_graphql_serde_expr_enum_value hydra_graphql_serde_expr_description hydra_graphql_serde_with_description hydra_graphql_serde_expr_enum_value_definition hydra_graphql_serde_expr_enum_type_definition hydra_graphql_serde_expr_named_type hydra_graphql_serde_expr_list_type hydra_graphql_serde_expr_non_null_type hydra_graphql_serde_expr_type hydra_graphql_serde_expr_field_definition hydra_graphql_serde_expr_object_type_definition hydra_graphql_serde_expr_type_definition hydra_graphql_serde_expr_type_system_definition hydra_graphql_serde_expr_type_system_definition_or_extension hydra_graphql_serde_expr_definition hydra_graphql_serde_expr_document)

(def hydra_graphql_serde_expr_name (fn [n] (hydra_serialization_cst ((fn [v] v) n))))

(def hydra_graphql_serde_expr_enum_value (fn [ev] (hydra_graphql_serde_expr_name ((fn [v] v) ev))))

(def hydra_graphql_serde_expr_description (fn [desc] (let [delim (hydra_serialization_cst "\"\"\"") text ((fn [v] v) ((fn [v] v) desc))] (hydra_serialization_newline_sep (list delim (hydra_serialization_cst text) delim)))))

(def hydra_graphql_serde_with_description (fn [mdesc] (fn [expr] (hydra_serialization_newline_sep (hydra_lib_maybes_cat (list ((hydra_lib_maybes_map hydra_graphql_serde_expr_description) mdesc) (hydra_lib_maybes_pure expr)))))))

(def hydra_graphql_serde_expr_enum_value_definition (fn [def_] (let [desc ((fn [v] (:description v)) def_) ev ((fn [v] (:enum_value v)) def_)] ((hydra_graphql_serde_with_description desc) (hydra_graphql_serde_expr_enum_value ev)))))

(def hydra_graphql_serde_expr_enum_type_definition (fn [def_] (let [desc ((fn [v] (:description v)) def_) name ((fn [v] (:name v)) def_) values_ ((fn [v] (:enum_values_definition v)) def_) values_expr (((hydra_lib_maybes_maybe (fn [] (list))) (fn [vs] ((hydra_lib_lists_map hydra_graphql_serde_expr_enum_value_definition) ((fn [v] v) vs)))) values_)] ((hydra_graphql_serde_with_description desc) (hydra_serialization_space_sep (list (hydra_serialization_cst "enum") (hydra_graphql_serde_expr_name name) (((hydra_serialization_curly_braces_list (list :nothing)) hydra_serialization_full_block_style) values_expr)))))))

(def hydra_graphql_serde_expr_named_type (fn [nt] (hydra_graphql_serde_expr_name ((fn [v] v) nt))))

(def hydra_graphql_serde_expr_list_type (fn [lt] (hydra_serialization_no_sep (list (hydra_serialization_cst "[") (hydra_graphql_serde_expr_type ((fn [v] v) lt)) (hydra_serialization_cst "]")))))

(def hydra_graphql_serde_expr_non_null_type (fn [nnt] (let [type_expr ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :named) ((fn [nt] (hydra_graphql_serde_expr_named_type nt)) match_value) (= (first match_target) :list) ((fn [lt] (hydra_graphql_serde_expr_list_type lt)) match_value))) (second match_target))) nnt)] (hydra_serialization_no_sep (list type_expr (hydra_serialization_cst "!"))))))

(def hydra_graphql_serde_expr_type (fn [typ] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :named) ((fn [nt] (hydra_graphql_serde_expr_named_type nt)) match_value) (= (first match_target) :list) ((fn [lt] (hydra_graphql_serde_expr_list_type lt)) match_value) (= (first match_target) :non_null) ((fn [nnt] (hydra_graphql_serde_expr_non_null_type nnt)) match_value))) (second match_target))) typ)))

(def hydra_graphql_serde_expr_field_definition (fn [def_] (let [desc ((fn [v] (:description v)) def_) name ((fn [v] (:name v)) def_) name_part (hydra_serialization_no_sep (list (hydra_graphql_serde_expr_name name) (hydra_serialization_cst ":"))) typ ((fn [v] (:type v)) def_) type_part (hydra_graphql_serde_expr_type typ)] ((hydra_graphql_serde_with_description desc) (hydra_serialization_space_sep (list name_part type_part))))))

(def hydra_graphql_serde_expr_object_type_definition (fn [def_] (let [desc ((fn [v] (:description v)) def_) fields ((fn [v] (:fields_definition v)) def_) fields_expr (((hydra_lib_maybes_maybe (fn [] (list))) (fn [fs] ((hydra_lib_lists_map hydra_graphql_serde_expr_field_definition) ((fn [v] v) fs)))) fields) name ((fn [v] (:name v)) def_)] ((hydra_graphql_serde_with_description desc) (hydra_serialization_space_sep (list (hydra_serialization_cst "type") (hydra_graphql_serde_expr_name name) (((hydra_serialization_curly_braces_list (list :nothing)) hydra_serialization_full_block_style) fields_expr)))))))

(def hydra_graphql_serde_expr_type_definition (fn [def_] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :scalar) ((fn [_] (hydra_serialization_cst "Unsupported: scalar type definition")) match_value) (= (first match_target) :object) ((fn [od] (hydra_graphql_serde_expr_object_type_definition od)) match_value) (= (first match_target) :interface) ((fn [_] (hydra_serialization_cst "Unsupported: interface type definition")) match_value) (= (first match_target) :union) ((fn [_] (hydra_serialization_cst "Unsupported: union type definition")) match_value) (= (first match_target) :enum) ((fn [ed] (hydra_graphql_serde_expr_enum_type_definition ed)) match_value) (= (first match_target) :input_object) ((fn [_] (hydra_serialization_cst "Unsupported: input object type definition")) match_value))) (second match_target))) def_)))

(def hydra_graphql_serde_expr_type_system_definition (fn [def_] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :schema) ((fn [_] (hydra_serialization_cst "Unsupported: schema definition")) match_value) (= (first match_target) :type) ((fn [dt] (hydra_graphql_serde_expr_type_definition dt)) match_value) (= (first match_target) :directive) ((fn [_] (hydra_serialization_cst "Unsupported: directive definition")) match_value))) (second match_target))) def_)))

(def hydra_graphql_serde_expr_type_system_definition_or_extension (fn [de] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :definition) ((fn [d] (hydra_graphql_serde_expr_type_system_definition d)) match_value) (= (first match_target) :extension) ((fn [_] (hydra_serialization_cst "Unsupported: type system extension")) match_value))) (second match_target))) de)))

(def hydra_graphql_serde_expr_definition (fn [def_] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :executable) ((fn [_] (hydra_serialization_cst "Unsupported: executable definition")) match_value) (= (first match_target) :type_system) ((fn [de] (hydra_graphql_serde_expr_type_system_definition_or_extension de)) match_value))) (second match_target))) def_)))

(def hydra_graphql_serde_expr_document (fn [d] (hydra_serialization_double_newline_sep ((hydra_lib_lists_map hydra_graphql_serde_expr_definition) ((fn [v] v) d)))))
