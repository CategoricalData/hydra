(module
  (import "hydra.lib.lists" "hydra.lib.lists.cons" (func $hydra.lib.lists.cons (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.foldl" (func $hydra.lib.lists.foldl (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.reverse" (func $hydra.lib.lists.reverse (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.delete" (func $hydra.lib.maps.delete (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.from_list" (func $hydra.lib.maps.from_list (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.insert" (func $hydra.lib.maps.insert (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.union" (func $hydra.lib.maps.union (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.cat" (func $hydra.lib.maybes.cat (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.map" (func $hydra.lib.maybes.map (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.maybe" (func $hydra.lib.maybes.maybe (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.delete" (func $hydra.lib.sets.delete (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.insert" (func $hydra.lib.sets.insert (param i32) (result i32) ) )
  (memory $memory 1 )
  (export "memory" (memory $memory) )
  (export "hydra.scoping.extend_graph_for_lambda" (func $hydra.scoping.extend_graph_for_lambda) )
  (export "hydra.scoping.extend_graph_for_let" (func $hydra.scoping.extend_graph_for_let) )
  (export "hydra.scoping.extend_graph_for_type_lambda" (func $hydra.scoping.extend_graph_for_type_lambda) )
  (export "hydra.scoping.extend_graph_with_bindings" (func $hydra.scoping.extend_graph_with_bindings) )
  (export "hydra.scoping.f_type_to_type_scheme" (func $hydra.scoping.f_type_to_type_scheme) )
  (export "hydra.scoping.type_scheme_to_f_type" (func $hydra.scoping.type_scheme_to_f_type) )
  (func $hydra.scoping.extend_graph_for_lambda (param $g i32) (param $lam i32) (result i32)
  (local $dom i32)
  (local $var i32)
  local.get $lam
  ;; project field: parameter
  local.set $var
  local.get $g
  ;; project field: bound_terms
  local.get $g
  ;; project field: bound_types
  local.get $var
  local.get $dom
  call $hydra.scoping.f_type_to_type_scheme
  local.get $g
  ;; project field: bound_types
  call $hydra.lib.maps.insert
  local.get $lam
  ;; project field: domain
  call $hydra.lib.maybes.maybe
  local.get $g
  ;; project field: class_constraints
  local.get $var
  local.get $g
  ;; project field: lambda_variables
  call $hydra.lib.sets.insert
  local.get $var
  local.get $g
  ;; project field: metadata
  call $hydra.lib.maps.delete
  local.get $g
  ;; project field: primitives
  local.get $g
  ;; project field: schema_types
  local.get $g
  ;; project field: type_variables
)
  (func $hydra.scoping.extend_graph_for_let (param $for_binding i32) (param $g i32) (param $letrec i32) (result i32)
  (local $b i32)
  (local $bindings i32)
  (local $g2 i32)
  (local $g_acc i32)
  (local $m i32)
  (local $new_meta i32)
  (local $s i32)
  (local $t i32)
  (local $ts i32)
  local.get $letrec
  ;; project field: bindings
  local.set $bindings
  local.get $bindings
  local.get $g
  call $hydra.scoping.extend_graph_with_bindings
  local.set $g2
  local.get $b
  ;; project field: name
  local.get $b
  ;; project field: term
  local.get $bindings
  call $hydra.lib.lists.map
  call $hydra.lib.maps.from_list
  local.get $g
  ;; project field: bound_terms
  call $hydra.lib.maps.union
  local.get $b
  ;; project field: name
  local.get $ts
  local.get $b
  ;; project field: type
  call $hydra.lib.maybes.map
  local.get $bindings
  call $hydra.lib.lists.map
  call $hydra.lib.maybes.cat
  call $hydra.lib.maps.from_list
  local.get $g
  ;; project field: bound_types
  call $hydra.lib.maps.union
  local.get $g
  ;; project field: class_constraints
  local.get $b
  ;; project field: name
  local.get $s
  call $hydra.lib.sets.delete
  local.get $g
  ;; project field: lambda_variables
  local.get $bindings
  call $hydra.lib.lists.foldl
  local.get $g_acc
  ;; project field: metadata
  local.set $m
  local.get $b
  ;; project field: name
  local.get $m
  call $hydra.lib.maps.delete
  local.get $b
  ;; project field: name
  local.get $t
  local.get $m
  call $hydra.lib.maps.insert
  local.get $g_acc
  local.get $b
  local.get $for_binding
  call $hydra.lib.maybes.maybe
  local.set $new_meta
  local.get $g_acc
  ;; project field: bound_terms
  local.get $g_acc
  ;; project field: bound_types
  local.get $g_acc
  ;; project field: class_constraints
  local.get $g_acc
  ;; project field: lambda_variables
  local.get $new_meta
  local.get $g_acc
  ;; project field: primitives
  local.get $g_acc
  ;; project field: schema_types
  local.get $g_acc
  ;; project field: type_variables
  local.get $g2
  local.get $bindings
  call $hydra.lib.lists.foldl
  ;; project field: metadata
  local.get $g
  ;; project field: primitives
  local.get $g
  ;; project field: schema_types
  local.get $g
  ;; project field: type_variables
)
  (func $hydra.scoping.extend_graph_for_type_lambda (param $g i32) (param $tlam i32) (result i32)
  (local $name i32)
  local.get $tlam
  ;; project field: parameter
  local.set $name
  local.get $g
  ;; project field: bound_terms
  local.get $g
  ;; project field: bound_types
  local.get $g
  ;; project field: class_constraints
  local.get $g
  ;; project field: lambda_variables
  local.get $g
  ;; project field: metadata
  local.get $g
  ;; project field: primitives
  local.get $g
  ;; project field: schema_types
  local.get $name
  local.get $g
  ;; project field: type_variables
  call $hydra.lib.sets.insert
)
  (func $hydra.scoping.extend_graph_with_bindings (param $bindings i32) (param $g i32) (result i32)
  (local $b i32)
  (local $new_terms i32)
  (local $new_types i32)
  (local $ts i32)
  local.get $b
  ;; project field: name
  local.get $b
  ;; project field: term
  local.get $bindings
  call $hydra.lib.lists.map
  call $hydra.lib.maps.from_list
  local.set $new_terms
  local.get $b
  ;; project field: name
  local.get $ts
  local.get $b
  ;; project field: type
  call $hydra.lib.maybes.map
  local.get $bindings
  call $hydra.lib.lists.map
  call $hydra.lib.maybes.cat
  call $hydra.lib.maps.from_list
  local.set $new_types
  local.get $new_terms
  local.get $g
  ;; project field: bound_terms
  call $hydra.lib.maps.union
  local.get $new_types
  local.get $g
  ;; project field: bound_types
  call $hydra.lib.maps.union
  local.get $g
  ;; project field: class_constraints
  local.get $g
  ;; project field: lambda_variables
  local.get $g
  ;; project field: metadata
  local.get $g
  ;; project field: primitives
  local.get $g
  ;; project field: schema_types
  local.get $g
  ;; project field: type_variables
)
  (func $hydra.scoping.f_type_to_type_scheme (param $typ i32) (result i32)
  (local $at i32)
  (local $ft i32)
  (local $gather_forall i32)
  (local $strip_annotations i32)
  (local $t i32)
  (local $typ2 i32)
  (local $vars i32)
  (block $end_type (result i32)
  (block $annotated
  local.get $t
  br_table $annotated $annotated
)
  local.get $at
  ;; project field: body
  local.get $strip_annotations
  br $end_type
)
  local.set $strip_annotations
  (block $end_type (result i32)
  (block $forall
  local.get $typ2
  local.get $strip_annotations
  br_table $forall $forall
)
  local.get $ft
  ;; project field: parameter
  local.get $vars
  call $hydra.lib.lists.cons
  local.get $ft
  ;; project field: body
  local.get $gather_forall
  br $end_type
)
  local.set $gather_forall
  i32.const 0
  ;; list elements follow
  local.get $typ
  local.get $gather_forall
)
  (func $hydra.scoping.type_scheme_to_f_type (param $ts i32) (result i32)
  (local $body i32)
  (local $t i32)
  (local $v i32)
  (local $vars i32)
  local.get $ts
  ;; project field: variables
  local.set $vars
  local.get $ts
  ;; project field: type
  local.set $body
  local.get $v
  local.get $t
  local.get $body
  local.get $vars
  call $hydra.lib.lists.reverse
  call $hydra.lib.lists.foldl
)
)
