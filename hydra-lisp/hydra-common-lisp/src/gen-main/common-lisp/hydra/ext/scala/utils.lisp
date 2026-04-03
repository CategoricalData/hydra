(defpackage :hydra.ext.scala.utils
(:use :cl :hydra.core :hydra.ext.scala.language :hydra.ext.scala.syntax :hydra.formatting :hydra.lib.equality :hydra.lib.lists :hydra.lib.logic :hydra.lib.math :hydra.lib.maybes :hydra.lib.sets :hydra.lib.strings :hydra.module :hydra.names :hydra.strip)
(:export :hydra_ext_scala_utils_name_of_type :hydra_ext_scala_utils_scala_reserved_words :hydra_ext_scala_utils_scala_escape_name :hydra_ext_scala_utils_scala_type_name :hydra_ext_scala_utils_qualify_union_field_name :hydra_ext_scala_utils_sapply :hydra_ext_scala_utils_sname :hydra_ext_scala_utils_type_to_string :hydra_ext_scala_utils_sapply_types :hydra_ext_scala_utils_sassign :hydra_ext_scala_utils_slambda :hydra_ext_scala_utils_sprim :hydra_ext_scala_utils_stapply :hydra_ext_scala_utils_stapply1 :hydra_ext_scala_utils_stapply2 :hydra_ext_scala_utils_stparam :hydra_ext_scala_utils_stref :hydra_ext_scala_utils_svar))

(in-package :hydra.ext.scala.utils)

(cl:defvar hydra_ext_scala_utils_name_of_type (cl:lambda (cx) (cl:lambda (t_) ((cl:lambda (match_target) ((cl:lambda (match_value) (cond ((equal (car match_target) :variable) ((cl:lambda (name) (list :just name)) match_value)) ((equal (car match_target) :forall) ((cl:lambda (ft) ((hydra_ext_scala_utils_name_of_type cx) ((cl:lambda (v) (hydra_core_forall_type-body v)) ft))) match_value)) (t (list :nothing)))) (cadr match_target))) (hydra_strip_deannotate_type t_)))))

(cl:defvar hydra_ext_scala_utils_scala_reserved_words hydra_ext_scala_language_scala_reserved_words)

(cl:defvar hydra_ext_scala_utils_scala_escape_name (cl:lambda (s) (let* ((sanitized (hydra_lib_strings_from_list ((hydra_lib_lists_map (cl:lambda (c) (if ((hydra_lib_equality_equal c) 39) 95 c))) (hydra_lib_strings_to_list s)))) (sanitized2 (if ((hydra_lib_equality_equal sanitized) "_") "_x" sanitized)) (sanitized3 (if ((hydra_lib_equality_equal sanitized2) "toString") "toString_" sanitized2)) (needs_backticks ((hydra_lib_logic_or ((hydra_lib_sets_member sanitized3) hydra_ext_scala_utils_scala_reserved_words)) ((hydra_lib_logic_and ((hydra_lib_equality_gt (hydra_lib_strings_length sanitized3)) 0)) ((hydra_lib_equality_equal ((hydra_lib_strings_char_at ((hydra_lib_math_sub (hydra_lib_strings_length sanitized3)) 1)) sanitized3)) 95))))) (if needs_backticks (hydra_lib_strings_cat (cl:list "`" sanitized3 "`")) sanitized3))))

(cl:defvar hydra_ext_scala_utils_scala_type_name (cl:lambda (qualify) (cl:lambda (name) (if ((hydra_lib_logic_or qualify) ((hydra_lib_sets_member (hydra_names_local_name_of name)) hydra_ext_scala_utils_scala_reserved_words)) ((cl:lambda (v) v) name) (hydra_names_local_name_of name)))))

(cl:defvar hydra_ext_scala_utils_qualify_union_field_name (cl:lambda (dlft) (cl:lambda (sname) (cl:lambda (fname) ((hydra_lib_strings_cat2 (((hydra_lib_maybes_maybe (cl:lambda () dlft)) (cl:lambda (n) ((hydra_lib_strings_cat2 ((hydra_ext_scala_utils_scala_type_name cl:t) n)) "."))) sname)) (hydra_ext_scala_utils_scala_escape_name ((cl:lambda (v) v) fname)))))))

(cl:defvar hydra_ext_scala_utils_sapply (cl:lambda (fun) (cl:lambda (args) (list :apply (make-hydra_ext_scala_syntax_data_apply fun args)))))

(cl:defvar hydra_ext_scala_utils_sname (cl:lambda (s) (list :ref (list :name (make-hydra_ext_scala_syntax_data_name s)))))

(cl:defvar hydra_ext_scala_utils_type_to_string (cl:lambda (t_) ((cl:lambda (match_target) ((cl:lambda (match_value) (cond ((equal (car match_target) :ref) ((cl:lambda (tr) ((cl:lambda (match_target) ((cl:lambda (match_value) (cond ((equal (car match_target) :name) ((cl:lambda (tn) ((cl:lambda (v) (hydra_ext_scala_syntax_type_name-value v)) tn)) match_value)) (t "Any"))) (cadr match_target))) tr)) match_value)) ((equal (car match_target) :var) ((cl:lambda (tv) ((cl:lambda (v) (hydra_ext_scala_syntax_type_name-value v)) ((cl:lambda (v) (hydra_ext_scala_syntax_type_var-name v)) tv))) match_value)) ((equal (car match_target) :function_type) ((cl:lambda (ft) ((cl:lambda (match_target) ((cl:lambda (match_value) (cond ((equal (car match_target) :function) ((cl:lambda (fn_) (let* ((params ((hydra_lib_lists_map hydra_ext_scala_utils_type_to_string) ((cl:lambda (v) (hydra_ext_scala_syntax_type_function-params v)) fn_))) (res (hydra_ext_scala_utils_type_to_string ((cl:lambda (v) (hydra_ext_scala_syntax_type_function-res v)) fn_)))) (hydra_lib_strings_cat (cl:list "(" ((hydra_lib_strings_intercalate ", ") params) ") => " res)))) match_value)) (t "Any"))) (cadr match_target))) ft)) match_value)) ((equal (car match_target) :apply) ((cl:lambda (ta) (let* ((arg_strs ((hydra_lib_lists_map hydra_ext_scala_utils_type_to_string) ((cl:lambda (v) (hydra_ext_scala_syntax_type_apply-args v)) ta))) (base (hydra_ext_scala_utils_type_to_string ((cl:lambda (v) (hydra_ext_scala_syntax_type_apply-tpe v)) ta)))) (hydra_lib_strings_cat (cl:list base "[" ((hydra_lib_strings_intercalate ", ") arg_strs) "]")))) match_value)) (t "Any"))) (cadr match_target))) t_)))

(cl:defvar hydra_ext_scala_utils_sapply_types (cl:lambda (fun) (cl:lambda (type_args) (let* ((type_to_str (cl:lambda (t_) (hydra_ext_scala_utils_type_to_string t_))) (type_strings ((hydra_lib_lists_map type_to_str) type_args)) (type_arg_str (hydra_lib_strings_cat (cl:list "[" ((hydra_lib_strings_intercalate ", ") type_strings) "]")))) ((cl:lambda (match_target) ((cl:lambda (match_value) (cond ((equal (car match_target) :ref) ((cl:lambda (ref) ((cl:lambda (match_target) ((cl:lambda (match_value) (cond ((equal (car match_target) :name) ((cl:lambda (dn) (let* ((name_str ((cl:lambda (v) (hydra_ext_scala_syntax_data_name-value v)) dn)) (raw_name ((cl:lambda (v) v) name_str))) (hydra_ext_scala_utils_sname ((hydra_lib_strings_cat2 raw_name) type_arg_str)))) match_value)) (t fun))) (cadr match_target))) ref)) match_value)) (t fun))) (cadr match_target))) fun)))))

(cl:defvar hydra_ext_scala_utils_sassign (cl:lambda (lhs) (cl:lambda (rhs) (list :assign (make-hydra_ext_scala_syntax_data_assign lhs rhs)))))

(cl:defvar hydra_ext_scala_utils_slambda (cl:lambda (v) (cl:lambda (body) (cl:lambda (sdom) (list :function_data (list :function (make-hydra_ext_scala_syntax_data_function (cl:list (make-hydra_ext_scala_syntax_data_param (cl:list) (list :value v) sdom (list :nothing))) body)))))))

(cl:defvar hydra_ext_scala_utils_sprim (cl:lambda (name) (let* ((qname (hydra_names_qualify_name name)) (local (hydra_ext_scala_utils_scala_escape_name ((cl:lambda (v) (hydra_module_qualified_name-local v)) qname))) (prefix ((cl:lambda (v) v) (hydra_lib_maybes_from_just ((cl:lambda (v) (hydra_module_qualified_name-namespace v)) qname))))) (hydra_ext_scala_utils_sname ((hydra_lib_strings_cat2 ((hydra_lib_strings_cat2 prefix) ".")) local)))))

(cl:defvar hydra_ext_scala_utils_stapply (cl:lambda (t_) (cl:lambda (args) (list :apply (make-hydra_ext_scala_syntax_type_apply t_ args)))))

(cl:defvar hydra_ext_scala_utils_stapply1 (cl:lambda (t1) (cl:lambda (t2) ((hydra_ext_scala_utils_stapply t1) (cl:list t2)))))

(cl:defvar hydra_ext_scala_utils_stapply2 (cl:lambda (t1) (cl:lambda (t2) (cl:lambda (t3) ((hydra_ext_scala_utils_stapply t1) (cl:list t2 t3))))))

(cl:defvar hydra_ext_scala_utils_stparam (cl:lambda (name) (let ((v (hydra_formatting_capitalize ((cl:lambda (v) v) name)))) (make-hydra_ext_scala_syntax_type_param (cl:list) (list :value v) (cl:list) (cl:list) (cl:list) (cl:list)))))

(cl:defvar hydra_ext_scala_utils_stref (cl:lambda (s) (list :ref (list :name (make-hydra_ext_scala_syntax_type_name s)))))

(cl:defvar hydra_ext_scala_utils_svar (cl:lambda (name) (let ((v ((cl:lambda (v) v) name))) (list :var (make-hydra_ext_scala_syntax_pat_var (make-hydra_ext_scala_syntax_data_name v))))))
