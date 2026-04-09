(module
  (import "hydra.encode.context" "hydra.encode.context.context" (func $hydra.encode.context.context (param i32) (result i32) ) )
  (import "hydra.encode.core" "hydra.encode.core.binding" (func $hydra.encode.core.binding (param i32) (result i32) ) )
  (import "hydra.encode.core" "hydra.encode.core.name" (func $hydra.encode.core.name (param i32) (result i32) ) )
  (import "hydra.encode.core" "hydra.encode.core.term" (func $hydra.encode.core.term (param i32) (result i32) ) )
  (import "hydra.encode.core" "hydra.encode.core.type" (func $hydra.encode.core.type (param i32) (result i32) ) )
  (import "hydra.encode.core" "hydra.encode.core.type_variable_metadata" (func $hydra.encode.core.type_variable_metadata (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.bimap" (func $hydra.lib.maps.bimap (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.map" (func $hydra.lib.maybes.map (param i32) (result i32) ) )
  (memory $memory 1 )
  (export "memory" (memory $memory) )
  (export "hydra.encode.typing.function_structure" (func $hydra.encode.typing.function_structure) )
  (export "hydra.encode.typing.inference_result" (func $hydra.encode.typing.inference_result) )
  (export "hydra.encode.typing.term_subst" (func $hydra.encode.typing.term_subst) )
  (export "hydra.encode.typing.type_constraint" (func $hydra.encode.typing.type_constraint) )
  (export "hydra.encode.typing.type_subst" (func $hydra.encode.typing.type_subst) )
  (func $hydra.encode.typing.function_structure (param $env i32) (param $x i32) (result i32)
  (local $opt i32)
  (local $xs i32)
  i32.const 0 ;; string: "hydra.typing.FunctionStructure"
  i32.const 7
  ;; list elements follow
  i32.const 0 ;; string: "typeParams"
  call $hydra.encode.core.name
  local.get $xs
  call $hydra.lib.lists.map
  i32.const 0 ;; string: "params"
  call $hydra.encode.core.name
  local.get $xs
  call $hydra.lib.lists.map
  i32.const 0 ;; string: "bindings"
  call $hydra.encode.core.binding
  local.get $xs
  call $hydra.lib.lists.map
  i32.const 0 ;; string: "body"
  local.get $x
  ;; project field: body
  call $hydra.encode.core.term
  i32.const 0 ;; string: "domains"
  call $hydra.encode.core.type
  local.get $xs
  call $hydra.lib.lists.map
  i32.const 0 ;; string: "codomain"
  call $hydra.encode.core.type
  local.get $opt
  call $hydra.lib.maybes.map
  i32.const 0 ;; string: "environment"
  local.get $x
  ;; project field: environment
  local.get $env
)
  (func $hydra.encode.typing.inference_result (param $x i32) (result i32)
  (local $m i32)
  i32.const 0 ;; string: "hydra.typing.InferenceResult"
  i32.const 5
  ;; list elements follow
  i32.const 0 ;; string: "term"
  local.get $x
  ;; project field: term
  call $hydra.encode.core.term
  i32.const 0 ;; string: "type"
  local.get $x
  ;; project field: type
  call $hydra.encode.core.type
  i32.const 0 ;; string: "subst"
  local.get $x
  ;; project field: subst
  call $hydra.encode.typing.type_subst
  i32.const 0 ;; string: "classConstraints"
  call $hydra.encode.core.name
  call $hydra.encode.core.type_variable_metadata
  local.get $m
  call $hydra.lib.maps.bimap
  i32.const 0 ;; string: "context"
  local.get $x
  ;; project field: context
  call $hydra.encode.context.context
)
  (func $hydra.encode.typing.term_subst (param $x i32) (result i32)
  (local $m i32)
  i32.const 0 ;; string: "hydra.typing.TermSubst"
  call $hydra.encode.core.name
  call $hydra.encode.core.term
  local.get $m
  call $hydra.lib.maps.bimap
)
  (func $hydra.encode.typing.type_constraint (param $x i32) (result i32)
  (local $x2 i32)
  i32.const 0 ;; string: "hydra.typing.TypeConstraint"
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "left"
  local.get $x
  ;; project field: left
  call $hydra.encode.core.type
  i32.const 0 ;; string: "right"
  local.get $x
  ;; project field: right
  call $hydra.encode.core.type
  i32.const 0 ;; string: "comment"
  local.get $x2
)
  (func $hydra.encode.typing.type_subst (param $x i32) (result i32)
  (local $m i32)
  i32.const 0 ;; string: "hydra.typing.TypeSubst"
  call $hydra.encode.core.name
  call $hydra.encode.core.type
  local.get $m
  call $hydra.lib.maps.bimap
)
)
