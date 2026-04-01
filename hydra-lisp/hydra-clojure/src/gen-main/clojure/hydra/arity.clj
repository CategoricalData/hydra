(ns hydra.arity
  (:require [hydra.core :refer :all] [hydra.graph :refer :all] [hydra.lib.lists :refer :all] [hydra.lib.math :refer :all]
))

(declare hydra_arity_function_arity hydra_arity_term_arity hydra_arity_type_arity hydra_arity_primitive_arity hydra_arity_type_scheme_arity hydra_arity_uncurry_type)

(def hydra_arity_function_arity (fn [match_target] ((fn [match_value] (cond (= (first match_target) :elimination) ((fn [_] 1) match_value) (= (first match_target) :lambda) ((fn [arg_] ((fn [i] ((hydra_lib_math_add 1) i)) ((fn [arg_2] (hydra_arity_term_arity ((fn [v] (:body v)) arg_2))) arg_))) match_value) (= (first match_target) :primitive) ((fn [_] 42) match_value))) (second match_target))))

(def hydra_arity_term_arity (fn [match_target] ((fn [match_value] (cond (= (first match_target) :application) ((fn [arg_] ((fn [arg_2] ((fn [xapp] ((hydra_lib_math_sub xapp) 1)) (hydra_arity_term_arity arg_2))) ((fn [v] (:function v)) arg_))) match_value) (= (first match_target) :function) (hydra_arity_function_arity match_value) :else 0)) (second match_target))))

(def hydra_arity_type_arity (fn [match_target] ((fn [match_value] (cond (= (first match_target) :annotated) ((fn [arg_] (hydra_arity_type_arity ((fn [v] (:body v)) arg_))) match_value) (= (first match_target) :application) ((fn [arg_] (hydra_arity_type_arity ((fn [v] (:function v)) arg_))) match_value) (= (first match_target) :forall) ((fn [arg_] (hydra_arity_type_arity ((fn [v] (:body v)) arg_))) match_value) (= (first match_target) :function) ((fn [f] ((hydra_lib_math_add 1) ((fn [arg_] (hydra_arity_type_arity ((fn [v] (:codomain v)) arg_))) f))) match_value) :else 0)) (second match_target))))

(def hydra_arity_primitive_arity (fn [arg_] ((fn [arg_2] (hydra_arity_type_arity ((fn [v] (:type v)) arg_2))) ((fn [v] (:type v)) arg_))))

(def hydra_arity_type_scheme_arity (fn [arg_] (hydra_arity_type_arity ((fn [v] (:type v)) arg_))))

(def hydra_arity_uncurry_type (fn [t_] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :annotated) ((fn [arg_] (hydra_arity_uncurry_type ((fn [v] (:body v)) arg_))) match_value) (= (first match_target) :application) ((fn [arg_] (hydra_arity_uncurry_type ((fn [v] (:function v)) arg_))) match_value) (= (first match_target) :forall) ((fn [arg_] (hydra_arity_uncurry_type ((fn [v] (:body v)) arg_))) match_value) (= (first match_target) :function) ((fn [ft] ((hydra_lib_lists_cons ((fn [v] (:domain v)) ft)) ((fn [arg_] (hydra_arity_uncurry_type ((fn [v] (:codomain v)) arg_))) ft))) match_value) :else (list t_))) (second match_target))) t_)))
