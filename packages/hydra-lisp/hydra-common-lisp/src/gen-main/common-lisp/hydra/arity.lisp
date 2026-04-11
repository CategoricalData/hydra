(defpackage :hydra.arity
(:use :cl :hydra.core :hydra.graph :hydra.lib.lists :hydra.lib.math)
(:export :hydra_arity_function_arity :hydra_arity_term_arity :hydra_arity_type_arity :hydra_arity_primitive_arity :hydra_arity_type_scheme_arity :hydra_arity_uncurry_type))

(in-package :hydra.arity)

(cl:defvar hydra_arity_function_arity (cl:lambda (match_target) ((cl:lambda (match_value) (cond ((equal (car match_target) :elimination) ((cl:lambda (_) 1) match_value)) ((equal (car match_target) :lambda) ((cl:lambda (arg_) ((cl:lambda (i) ((hydra_lib_math_add 1) i)) ((cl:lambda (arg_2) (hydra_arity_term_arity ((cl:lambda (v) (hydra_core_lambda-body v)) arg_2))) arg_))) match_value)))) (cadr match_target))))

(cl:defvar hydra_arity_term_arity (cl:lambda (match_target) ((cl:lambda (match_value) (cond ((equal (car match_target) :application) ((cl:lambda (arg_) ((cl:lambda (arg_2) ((cl:lambda (xapp) ((hydra_lib_math_sub xapp) 1)) (hydra_arity_term_arity arg_2))) ((cl:lambda (v) (hydra_core_application-function v)) arg_))) match_value)) ((equal (car match_target) :function) (hydra_arity_function_arity match_value)) (t 0))) (cadr match_target))))

(cl:defvar hydra_arity_type_arity (cl:lambda (match_target) ((cl:lambda (match_value) (cond ((equal (car match_target) :annotated) ((cl:lambda (arg_) (hydra_arity_type_arity ((cl:lambda (v) (hydra_core_annotated_type-body v)) arg_))) match_value)) ((equal (car match_target) :application) ((cl:lambda (arg_) (hydra_arity_type_arity ((cl:lambda (v) (hydra_core_application_type-function v)) arg_))) match_value)) ((equal (car match_target) :forall) ((cl:lambda (arg_) (hydra_arity_type_arity ((cl:lambda (v) (hydra_core_forall_type-body v)) arg_))) match_value)) ((equal (car match_target) :function) ((cl:lambda (f) ((hydra_lib_math_add 1) ((cl:lambda (arg_) (hydra_arity_type_arity ((cl:lambda (v) (hydra_core_function_type-codomain v)) arg_))) f))) match_value)) (t 0))) (cadr match_target))))

(cl:defvar hydra_arity_primitive_arity (cl:lambda (arg_) ((cl:lambda (arg_2) (hydra_arity_type_arity ((cl:lambda (v) (hydra_core_type_scheme-type v)) arg_2))) ((cl:lambda (v) (hydra_graph_primitive-type v)) arg_))))

(cl:defvar hydra_arity_type_scheme_arity (cl:lambda (arg_) (hydra_arity_type_arity ((cl:lambda (v) (hydra_core_type_scheme-type v)) arg_))))

(cl:defvar hydra_arity_uncurry_type (cl:lambda (t_) ((cl:lambda (match_target) ((cl:lambda (match_value) (cond ((equal (car match_target) :annotated) ((cl:lambda (arg_) (hydra_arity_uncurry_type ((cl:lambda (v) (hydra_core_annotated_type-body v)) arg_))) match_value)) ((equal (car match_target) :application) ((cl:lambda (arg_) (hydra_arity_uncurry_type ((cl:lambda (v) (hydra_core_application_type-function v)) arg_))) match_value)) ((equal (car match_target) :forall) ((cl:lambda (arg_) (hydra_arity_uncurry_type ((cl:lambda (v) (hydra_core_forall_type-body v)) arg_))) match_value)) ((equal (car match_target) :function) ((cl:lambda (ft) ((hydra_lib_lists_cons ((cl:lambda (v) (hydra_core_function_type-domain v)) ft)) ((cl:lambda (arg_) (hydra_arity_uncurry_type ((cl:lambda (v) (hydra_core_function_type-codomain v)) arg_))) ft))) match_value)) (t (cl:list t_)))) (cadr match_target))) t_)))
