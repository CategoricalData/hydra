(ns hydra.ext.scala.utils
  (:require [hydra.core :refer :all] [hydra.ext.scala.language :refer :all] [hydra.ext.scala.syntax :refer :all] [hydra.formatting :refer :all] [hydra.lib.equality :refer :all] [hydra.lib.lists :refer :all] [hydra.lib.logic :refer :all] [hydra.lib.math :refer :all] [hydra.lib.maybes :refer :all] [hydra.lib.sets :refer :all] [hydra.lib.strings :refer :all] [hydra.module :refer :all] [hydra.names :refer :all] [hydra.rewriting :refer :all]
))

(declare hydra_ext_scala_utils_name_of_type hydra_ext_scala_utils_scala_reserved_words hydra_ext_scala_utils_scala_escape_name hydra_ext_scala_utils_scala_type_name hydra_ext_scala_utils_qualify_union_field_name hydra_ext_scala_utils_sapply hydra_ext_scala_utils_sname hydra_ext_scala_utils_type_to_string hydra_ext_scala_utils_sapply_types hydra_ext_scala_utils_sassign hydra_ext_scala_utils_slambda hydra_ext_scala_utils_sprim hydra_ext_scala_utils_stapply hydra_ext_scala_utils_stapply1 hydra_ext_scala_utils_stapply2 hydra_ext_scala_utils_stparam hydra_ext_scala_utils_stref hydra_ext_scala_utils_svar)

(def hydra_ext_scala_utils_name_of_type (fn [cx] (fn [t_] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :variable) ((fn [name] (list :just name)) match_value) (= (first match_target) :forall) ((fn [ft] ((hydra_ext_scala_utils_name_of_type cx) ((fn [v] (:body v)) ft))) match_value) :else (list :nothing))) (second match_target))) (hydra_rewriting_deannotate_type t_)))))

(def hydra_ext_scala_utils_scala_reserved_words hydra_ext_scala_language_scala_reserved_words)

(def hydra_ext_scala_utils_scala_escape_name (fn [s] (let [sanitized (hydra_lib_strings_from_list ((hydra_lib_lists_map (fn [c] (if ((hydra_lib_equality_equal c) 39) 95 c))) (hydra_lib_strings_to_list s))) sanitized2 (if ((hydra_lib_equality_equal sanitized) "_") "_x" sanitized) sanitized3 (if ((hydra_lib_equality_equal sanitized2) "toString") "toString_" sanitized2) needs_backticks ((hydra_lib_logic_or ((hydra_lib_sets_member sanitized3) hydra_ext_scala_utils_scala_reserved_words)) ((hydra_lib_logic_and ((hydra_lib_equality_gt (hydra_lib_strings_length sanitized3)) 0)) ((hydra_lib_equality_equal ((hydra_lib_strings_char_at ((hydra_lib_math_sub (hydra_lib_strings_length sanitized3)) 1)) sanitized3)) 95)))] (if needs_backticks (hydra_lib_strings_cat (list "`" sanitized3 "`")) sanitized3))))

(def hydra_ext_scala_utils_scala_type_name (fn [qualify] (fn [name] (if ((hydra_lib_logic_or qualify) ((hydra_lib_sets_member (hydra_names_local_name_of name)) hydra_ext_scala_utils_scala_reserved_words)) ((fn [v] v) name) (hydra_names_local_name_of name)))))

(def hydra_ext_scala_utils_qualify_union_field_name (fn [dlft] (fn [sname] (fn [fname] ((hydra_lib_strings_cat2 (((hydra_lib_maybes_maybe (fn [] dlft)) (fn [n] ((hydra_lib_strings_cat2 ((hydra_ext_scala_utils_scala_type_name true) n)) "."))) sname)) (hydra_ext_scala_utils_scala_escape_name ((fn [v] v) fname)))))))

(def hydra_ext_scala_utils_sapply (fn [fun] (fn [args] (list :apply (->hydra_ext_scala_syntax_data_apply fun args)))))

(def hydra_ext_scala_utils_sname (fn [s] (list :ref (list :name (->hydra_ext_scala_syntax_data_name s)))))

(def hydra_ext_scala_utils_type_to_string (fn [t_] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :ref) ((fn [tr] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :name) ((fn [tn] ((fn [v] (:value v)) tn)) match_value) :else "Any")) (second match_target))) tr)) match_value) (= (first match_target) :var) ((fn [tv] ((fn [v] (:value v)) ((fn [v] (:name v)) tv))) match_value) (= (first match_target) :function_type) ((fn [ft] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :function) ((fn [fn_] (let [params ((hydra_lib_lists_map hydra_ext_scala_utils_type_to_string) ((fn [v] (:params v)) fn_)) res (hydra_ext_scala_utils_type_to_string ((fn [v] (:res v)) fn_))] (hydra_lib_strings_cat (list "(" ((hydra_lib_strings_intercalate ", ") params) ") => " res)))) match_value) :else "Any")) (second match_target))) ft)) match_value) (= (first match_target) :apply) ((fn [ta] (let [arg_strs ((hydra_lib_lists_map hydra_ext_scala_utils_type_to_string) ((fn [v] (:args v)) ta)) base (hydra_ext_scala_utils_type_to_string ((fn [v] (:tpe v)) ta))] (hydra_lib_strings_cat (list base "[" ((hydra_lib_strings_intercalate ", ") arg_strs) "]")))) match_value) :else "Any")) (second match_target))) t_)))

(def hydra_ext_scala_utils_sapply_types (fn [fun] (fn [type_args] (let [type_to_str (fn [t_] (hydra_ext_scala_utils_type_to_string t_)) type_strings ((hydra_lib_lists_map type_to_str) type_args) type_arg_str (hydra_lib_strings_cat (list "[" ((hydra_lib_strings_intercalate ", ") type_strings) "]"))] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :ref) ((fn [ref] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :name) ((fn [dn] (let [name_str ((fn [v] (:value v)) dn) raw_name ((fn [v] v) name_str)] (hydra_ext_scala_utils_sname ((hydra_lib_strings_cat2 raw_name) type_arg_str)))) match_value) :else fun)) (second match_target))) ref)) match_value) :else fun)) (second match_target))) fun)))))

(def hydra_ext_scala_utils_sassign (fn [lhs] (fn [rhs] (list :assign (->hydra_ext_scala_syntax_data_assign lhs rhs)))))

(def hydra_ext_scala_utils_slambda (fn [v] (fn [body] (fn [sdom] (list :function_data (list :function (->hydra_ext_scala_syntax_data_function (list (->hydra_ext_scala_syntax_data_param (list) (list :value v) sdom (list :nothing))) body)))))))

(def hydra_ext_scala_utils_sprim (fn [name] (let [qname (hydra_names_qualify_name name) local (hydra_ext_scala_utils_scala_escape_name ((fn [v] (:local v)) qname)) prefix ((fn [v] v) (hydra_lib_maybes_from_just ((fn [v] (:namespace v)) qname)))] (hydra_ext_scala_utils_sname ((hydra_lib_strings_cat2 ((hydra_lib_strings_cat2 prefix) ".")) local)))))

(def hydra_ext_scala_utils_stapply (fn [t_] (fn [args] (list :apply (->hydra_ext_scala_syntax_type_apply t_ args)))))

(def hydra_ext_scala_utils_stapply1 (fn [t1] (fn [t2] ((hydra_ext_scala_utils_stapply t1) (list t2)))))

(def hydra_ext_scala_utils_stapply2 (fn [t1] (fn [t2] (fn [t3] ((hydra_ext_scala_utils_stapply t1) (list t2 t3))))))

(def hydra_ext_scala_utils_stparam (fn [name] (let [v (hydra_formatting_capitalize ((fn [v] v) name))] (->hydra_ext_scala_syntax_type_param (list) (list :value v) (list) (list) (list) (list)))))

(def hydra_ext_scala_utils_stref (fn [s] (list :ref (list :name (->hydra_ext_scala_syntax_type_name s)))))

(def hydra_ext_scala_utils_svar (fn [name] (let [v ((fn [v] v) name)] (list :var (->hydra_ext_scala_syntax_pat_var (->hydra_ext_scala_syntax_data_name v))))))
