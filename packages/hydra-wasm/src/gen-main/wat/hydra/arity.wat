(module
  (import "hydra.lib.lists" "hydra.lib.lists.cons" (func $hydra.lib.lists.cons (param i32) (result i32) ) )
  (import "hydra.lib.math" "hydra.lib.math.add" (func $hydra.lib.math.add (param i32) (result i32) ) )
  (import "hydra.lib.math" "hydra.lib.math.sub" (func $hydra.lib.math.sub (param i32) (result i32) ) )
  (memory $memory 1 )
  (export "memory" (memory $memory) )
  (export "hydra.arity.function_arity" (func $hydra.arity.function_arity) )
  (export "hydra.arity.primitive_arity" (func $hydra.arity.primitive_arity) )
  (export "hydra.arity.term_arity" (func $hydra.arity.term_arity) )
  (export "hydra.arity.type_arity" (func $hydra.arity.type_arity) )
  (export "hydra.arity.type_scheme_arity" (func $hydra.arity.type_scheme_arity) )
  (export "hydra.arity.uncurry_type" (func $hydra.arity.uncurry_type) )
  (func $hydra.arity.function_arity (param $arg_0 i32) (result i32)
  (local $i i32)
  (block $end_function (result i32)
  (block $lambda
  (block $elimination
  local.get $arg_0
  br_table $elimination $lambda $lambda
)
  i32.const 1
  br $end_function
)
  i32.const 1
  local.get $i
  call $hydra.lib.math.add
  br $end_function
)
)
  (func $hydra.arity.primitive_arity (param $arg_ i32) (result i32)
  (local $arg_2 i32)
  local.get $arg_2
  ;; project field: type
  call $hydra.arity.type_arity
)
  (func $hydra.arity.term_arity (param $arg_0 i32) (result i32)
  (local $v i32)
  (local $xapp i32)
  (block $end_term (result i32)
  (block $function
  (block $application
  local.get $arg_0
  br_table $application $function $function
)
  local.get $xapp
  i32.const 1
  call $hydra.lib.math.sub
  br $end_term
)
  local.get $v
  call $hydra.arity.function_arity
  br $end_term
)
)
  (func $hydra.arity.type_arity (param $arg_0 i32) (result i32)
  (local $arg_ i32)
  (block $end_type (result i32)
  (block $function
  (block $forall
  (block $application
  (block $annotated
  local.get $arg_0
  br_table $annotated $application $forall $function $function
)
  local.get $arg_
  ;; project field: body
  call $hydra.arity.type_arity
  br $end_type
)
  local.get $arg_
  ;; project field: function
  call $hydra.arity.type_arity
  br $end_type
)
  local.get $arg_
  ;; project field: body
  call $hydra.arity.type_arity
  br $end_type
)
  i32.const 1
  local.get $arg_
  ;; project field: codomain
  call $hydra.arity.type_arity
  call $hydra.lib.math.add
  br $end_type
)
)
  (func $hydra.arity.type_scheme_arity (param $arg_ i32) (result i32)
  local.get $arg_
  ;; project field: type
  call $hydra.arity.type_arity
)
  (func $hydra.arity.uncurry_type (param $t i32) (result i32)
  (local $arg_ i32)
  (local $ft i32)
  (block $end_type (result i32)
  (block $function
  (block $forall
  (block $application
  (block $annotated
  local.get $t
  br_table $annotated $application $forall $function $function
)
  local.get $arg_
  ;; project field: body
  call $hydra.arity.uncurry_type
  br $end_type
)
  local.get $arg_
  ;; project field: function
  call $hydra.arity.uncurry_type
  br $end_type
)
  local.get $arg_
  ;; project field: body
  call $hydra.arity.uncurry_type
  br $end_type
)
  local.get $ft
  ;; project field: domain
  local.get $arg_
  ;; project field: codomain
  call $hydra.arity.uncurry_type
  call $hydra.lib.lists.cons
  br $end_type
)
)
)
