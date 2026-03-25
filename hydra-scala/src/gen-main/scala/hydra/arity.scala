package hydra.arity

import hydra.core.*

import hydra.graph.*

import hydra.lib.lists

import hydra.lib.math

def functionArity(v1: hydra.core.Function): Int =
  v1 match
  case hydra.core.Function.elimination(v_Function_elimination__) => 1
  case hydra.core.Function.lambda(v_Function_lambda_arg_) => hydra.lib.math.add(1)(hydra.arity.termArity(`v_Function_lambda_arg_`.body))
  case hydra.core.Function.primitive(v_Function_primitive__) => 42

def primitiveArity(`arg_`: hydra.graph.Primitive): Int = hydra.arity.typeArity(`arg_`.`type`.`type`)

def termArity(v1: hydra.core.Term): Int =
  v1 match
  case hydra.core.Term.application(v_Term_application_arg_) => hydra.lib.math.sub(hydra.arity.termArity(`v_Term_application_arg_`.function))(1)
  case hydra.core.Term.function(v_Term_function_v12) => hydra.arity.functionArity(v_Term_function_v12)
  case _ => 0

def typeArity(v1: hydra.core.Type): Int =
  v1 match
  case hydra.core.Type.annotated(v_Type_annotated_arg_) => hydra.arity.typeArity(`v_Type_annotated_arg_`.body)
  case hydra.core.Type.application(v_Type_application_arg_) => hydra.arity.typeArity(`v_Type_application_arg_`.function)
  case hydra.core.Type.forall(v_Type_forall_arg_) => hydra.arity.typeArity(`v_Type_forall_arg_`.body)
  case hydra.core.Type.function(v_Type_function_f) => hydra.lib.math.add(1)(hydra.arity.typeArity(v_Type_function_f.codomain))
  case _ => 0

def typeSchemeArity(`arg_`: hydra.core.TypeScheme): Int = hydra.arity.typeArity(`arg_`.`type`)

def uncurryType(t: hydra.core.Type): Seq[hydra.core.Type] =
  t match
  case hydra.core.Type.annotated(v_Type_annotated_arg_) => hydra.arity.uncurryType(`v_Type_annotated_arg_`.body)
  case hydra.core.Type.application(v_Type_application_arg_) => hydra.arity.uncurryType(`v_Type_application_arg_`.function)
  case hydra.core.Type.forall(v_Type_forall_arg_) => hydra.arity.uncurryType(`v_Type_forall_arg_`.body)
  case hydra.core.Type.function(v_Type_function_ft) => hydra.lib.lists.cons[hydra.core.Type](v_Type_function_ft.domain)(hydra.arity.uncurryType(v_Type_function_ft.codomain))
  case _ => Seq(t)
