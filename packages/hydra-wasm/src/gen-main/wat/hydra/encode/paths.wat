(module
  (import "hydra.encode.core" "hydra.encode.core.name" (func $hydra.encode.core.name (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (result i32) ) )
  (memory $memory 1 )
  (export "memory" (memory $memory) )
  (export "hydra.encode.paths.subterm_edge" (func $hydra.encode.paths.subterm_edge) )
  (export "hydra.encode.paths.subterm_graph" (func $hydra.encode.paths.subterm_graph) )
  (export "hydra.encode.paths.subterm_node" (func $hydra.encode.paths.subterm_node) )
  (export "hydra.encode.paths.subterm_path" (func $hydra.encode.paths.subterm_path) )
  (export "hydra.encode.paths.subterm_step" (func $hydra.encode.paths.subterm_step) )
  (export "hydra.encode.paths.subtype_edge" (func $hydra.encode.paths.subtype_edge) )
  (export "hydra.encode.paths.subtype_graph" (func $hydra.encode.paths.subtype_graph) )
  (export "hydra.encode.paths.subtype_node" (func $hydra.encode.paths.subtype_node) )
  (export "hydra.encode.paths.subtype_path" (func $hydra.encode.paths.subtype_path) )
  (export "hydra.encode.paths.subtype_step" (func $hydra.encode.paths.subtype_step) )
  (func $hydra.encode.paths.subterm_edge (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.paths.SubtermEdge"
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "source"
  local.get $x
  ;; project field: source
  call $hydra.encode.paths.subterm_node
  i32.const 0 ;; string: "path"
  local.get $x
  ;; project field: path
  call $hydra.encode.paths.subterm_path
  i32.const 0 ;; string: "target"
  local.get $x
  ;; project field: target
  call $hydra.encode.paths.subterm_node
)
  (func $hydra.encode.paths.subterm_graph (param $x i32) (result i32)
  (local $xs i32)
  i32.const 0 ;; string: "hydra.paths.SubtermGraph"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "nodes"
  call $hydra.encode.paths.subterm_node
  local.get $xs
  call $hydra.lib.lists.map
  i32.const 0 ;; string: "edges"
  call $hydra.encode.paths.subterm_edge
  local.get $xs
  call $hydra.lib.lists.map
)
  (func $hydra.encode.paths.subterm_node (param $x i32) (result i32)
  (local $x2 i32)
  i32.const 0 ;; string: "hydra.paths.SubtermNode"
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "name"
  local.get $x
  ;; project field: name
  call $hydra.encode.core.name
  i32.const 0 ;; string: "label"
  local.get $x2
  i32.const 0 ;; string: "id"
  local.get $x2
)
  (func $hydra.encode.paths.subterm_path (param $x i32) (result i32)
  (local $xs i32)
  i32.const 0 ;; string: "hydra.paths.SubtermPath"
  call $hydra.encode.paths.subterm_step
  local.get $xs
  call $hydra.lib.lists.map
)
  (func $hydra.encode.paths.subterm_step (param $arg_0 i32) (result i32)
  (local $x i32)
  (local $y i32)
  (block $end_subterm_step (result i32)
  (block $wrapped_term
  (block $injection_term
  (block $type_application_term
  (block $type_lambda_body
  (block $sum_term
  (block $set_element
  (block $record_field
  (block $product_term
  (block $maybe_term
  (block $map_value
  (block $map_key
  (block $list_element
  (block $let_binding
  (block $let_body
  (block $union_cases_branch
  (block $union_cases_default
  (block $lambda_body
  (block $application_argument
  (block $application_function
  (block $annotated_body
  local.get $arg_0
  br_table $annotated_body $application_function $application_argument $lambda_body $union_cases_default $union_cases_branch $let_body $let_binding $list_element $map_key $map_value $maybe_term $product_term $record_field $set_element $sum_term $type_lambda_body $type_application_term $injection_term $wrapped_term $wrapped_term
)
  i32.const 0 ;; string: "hydra.paths.SubtermStep"
  i32.const 0 ;; string: "annotatedBody"
  i32.const 0
  br $end_subterm_step
)
  i32.const 0 ;; string: "hydra.paths.SubtermStep"
  i32.const 0 ;; string: "applicationFunction"
  i32.const 0
  br $end_subterm_step
)
  i32.const 0 ;; string: "hydra.paths.SubtermStep"
  i32.const 0 ;; string: "applicationArgument"
  i32.const 0
  br $end_subterm_step
)
  i32.const 0 ;; string: "hydra.paths.SubtermStep"
  i32.const 0 ;; string: "lambdaBody"
  i32.const 0
  br $end_subterm_step
)
  i32.const 0 ;; string: "hydra.paths.SubtermStep"
  i32.const 0 ;; string: "unionCasesDefault"
  i32.const 0
  br $end_subterm_step
)
  i32.const 0 ;; string: "hydra.paths.SubtermStep"
  i32.const 0 ;; string: "unionCasesBranch"
  local.get $y
  call $hydra.encode.core.name
  br $end_subterm_step
)
  i32.const 0 ;; string: "hydra.paths.SubtermStep"
  i32.const 0 ;; string: "letBody"
  i32.const 0
  br $end_subterm_step
)
  i32.const 0 ;; string: "hydra.paths.SubtermStep"
  i32.const 0 ;; string: "letBinding"
  local.get $y
  call $hydra.encode.core.name
  br $end_subterm_step
)
  i32.const 0 ;; string: "hydra.paths.SubtermStep"
  i32.const 0 ;; string: "listElement"
  local.get $x
  br $end_subterm_step
)
  i32.const 0 ;; string: "hydra.paths.SubtermStep"
  i32.const 0 ;; string: "mapKey"
  local.get $x
  br $end_subterm_step
)
  i32.const 0 ;; string: "hydra.paths.SubtermStep"
  i32.const 0 ;; string: "mapValue"
  local.get $x
  br $end_subterm_step
)
  i32.const 0 ;; string: "hydra.paths.SubtermStep"
  i32.const 0 ;; string: "maybeTerm"
  i32.const 0
  br $end_subterm_step
)
  i32.const 0 ;; string: "hydra.paths.SubtermStep"
  i32.const 0 ;; string: "productTerm"
  local.get $x
  br $end_subterm_step
)
  i32.const 0 ;; string: "hydra.paths.SubtermStep"
  i32.const 0 ;; string: "recordField"
  local.get $y
  call $hydra.encode.core.name
  br $end_subterm_step
)
  i32.const 0 ;; string: "hydra.paths.SubtermStep"
  i32.const 0 ;; string: "setElement"
  local.get $x
  br $end_subterm_step
)
  i32.const 0 ;; string: "hydra.paths.SubtermStep"
  i32.const 0 ;; string: "sumTerm"
  i32.const 0
  br $end_subterm_step
)
  i32.const 0 ;; string: "hydra.paths.SubtermStep"
  i32.const 0 ;; string: "typeLambdaBody"
  i32.const 0
  br $end_subterm_step
)
  i32.const 0 ;; string: "hydra.paths.SubtermStep"
  i32.const 0 ;; string: "typeApplicationTerm"
  i32.const 0
  br $end_subterm_step
)
  i32.const 0 ;; string: "hydra.paths.SubtermStep"
  i32.const 0 ;; string: "injectionTerm"
  i32.const 0
  br $end_subterm_step
)
  i32.const 0 ;; string: "hydra.paths.SubtermStep"
  i32.const 0 ;; string: "wrappedTerm"
  i32.const 0
  br $end_subterm_step
)
)
  (func $hydra.encode.paths.subtype_edge (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.paths.SubtypeEdge"
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "source"
  local.get $x
  ;; project field: source
  call $hydra.encode.paths.subtype_node
  i32.const 0 ;; string: "path"
  local.get $x
  ;; project field: path
  call $hydra.encode.paths.subtype_path
  i32.const 0 ;; string: "target"
  local.get $x
  ;; project field: target
  call $hydra.encode.paths.subtype_node
)
  (func $hydra.encode.paths.subtype_graph (param $x i32) (result i32)
  (local $xs i32)
  i32.const 0 ;; string: "hydra.paths.SubtypeGraph"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "nodes"
  call $hydra.encode.paths.subtype_node
  local.get $xs
  call $hydra.lib.lists.map
  i32.const 0 ;; string: "edges"
  call $hydra.encode.paths.subtype_edge
  local.get $xs
  call $hydra.lib.lists.map
)
  (func $hydra.encode.paths.subtype_node (param $x i32) (result i32)
  (local $x2 i32)
  i32.const 0 ;; string: "hydra.paths.SubtypeNode"
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "name"
  local.get $x
  ;; project field: name
  call $hydra.encode.core.name
  i32.const 0 ;; string: "label"
  local.get $x2
  i32.const 0 ;; string: "id"
  local.get $x2
)
  (func $hydra.encode.paths.subtype_path (param $x i32) (result i32)
  (local $xs i32)
  i32.const 0 ;; string: "hydra.paths.SubtypePath"
  call $hydra.encode.paths.subtype_step
  local.get $xs
  call $hydra.lib.lists.map
)
  (func $hydra.encode.paths.subtype_step (param $arg_0 i32) (result i32)
  (local $y i32)
  (block $end_subtype_step (result i32)
  (block $wrapped_type
  (block $union_field
  (block $set_element
  (block $record_field
  (block $pair_second
  (block $pair_first
  (block $maybe_element
  (block $map_values
  (block $map_keys
  (block $list_element
  (block $function_codomain
  (block $function_domain
  (block $forall_body
  (block $either_right
  (block $either_left
  (block $application_argument
  (block $application_function
  (block $annotated_body
  local.get $arg_0
  br_table $annotated_body $application_function $application_argument $either_left $either_right $forall_body $function_domain $function_codomain $list_element $map_keys $map_values $maybe_element $pair_first $pair_second $record_field $set_element $union_field $wrapped_type $wrapped_type
)
  i32.const 0 ;; string: "hydra.paths.SubtypeStep"
  i32.const 0 ;; string: "annotatedBody"
  i32.const 0
  br $end_subtype_step
)
  i32.const 0 ;; string: "hydra.paths.SubtypeStep"
  i32.const 0 ;; string: "applicationFunction"
  i32.const 0
  br $end_subtype_step
)
  i32.const 0 ;; string: "hydra.paths.SubtypeStep"
  i32.const 0 ;; string: "applicationArgument"
  i32.const 0
  br $end_subtype_step
)
  i32.const 0 ;; string: "hydra.paths.SubtypeStep"
  i32.const 0 ;; string: "eitherLeft"
  i32.const 0
  br $end_subtype_step
)
  i32.const 0 ;; string: "hydra.paths.SubtypeStep"
  i32.const 0 ;; string: "eitherRight"
  i32.const 0
  br $end_subtype_step
)
  i32.const 0 ;; string: "hydra.paths.SubtypeStep"
  i32.const 0 ;; string: "forallBody"
  i32.const 0
  br $end_subtype_step
)
  i32.const 0 ;; string: "hydra.paths.SubtypeStep"
  i32.const 0 ;; string: "functionDomain"
  i32.const 0
  br $end_subtype_step
)
  i32.const 0 ;; string: "hydra.paths.SubtypeStep"
  i32.const 0 ;; string: "functionCodomain"
  i32.const 0
  br $end_subtype_step
)
  i32.const 0 ;; string: "hydra.paths.SubtypeStep"
  i32.const 0 ;; string: "listElement"
  i32.const 0
  br $end_subtype_step
)
  i32.const 0 ;; string: "hydra.paths.SubtypeStep"
  i32.const 0 ;; string: "mapKeys"
  i32.const 0
  br $end_subtype_step
)
  i32.const 0 ;; string: "hydra.paths.SubtypeStep"
  i32.const 0 ;; string: "mapValues"
  i32.const 0
  br $end_subtype_step
)
  i32.const 0 ;; string: "hydra.paths.SubtypeStep"
  i32.const 0 ;; string: "maybeElement"
  i32.const 0
  br $end_subtype_step
)
  i32.const 0 ;; string: "hydra.paths.SubtypeStep"
  i32.const 0 ;; string: "pairFirst"
  i32.const 0
  br $end_subtype_step
)
  i32.const 0 ;; string: "hydra.paths.SubtypeStep"
  i32.const 0 ;; string: "pairSecond"
  i32.const 0
  br $end_subtype_step
)
  i32.const 0 ;; string: "hydra.paths.SubtypeStep"
  i32.const 0 ;; string: "recordField"
  local.get $y
  call $hydra.encode.core.name
  br $end_subtype_step
)
  i32.const 0 ;; string: "hydra.paths.SubtypeStep"
  i32.const 0 ;; string: "setElement"
  i32.const 0
  br $end_subtype_step
)
  i32.const 0 ;; string: "hydra.paths.SubtypeStep"
  i32.const 0 ;; string: "unionField"
  local.get $y
  call $hydra.encode.core.name
  br $end_subtype_step
)
  i32.const 0 ;; string: "hydra.paths.SubtypeStep"
  i32.const 0 ;; string: "wrappedType"
  i32.const 0
  br $end_subtype_step
)
)
)
