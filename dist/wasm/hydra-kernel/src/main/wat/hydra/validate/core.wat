(module
  (import "hydra.lib.equality" "hydra.lib.equality.equal" (func $hydra.lib.equality.equal (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.foldl" (func $hydra.lib.lists.foldl (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.length" (func $hydra.lib.lists.length (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.maybe_head" (func $hydra.lib.lists.maybe_head (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.null" (func $hydra.lib.lists.null (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.and" (func $hydra.lib.logic.and (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.if_else" (func $hydra.lib.logic.if_else (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.not" (func $hydra.lib.logic.not (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.or" (func $hydra.lib.logic.or (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.lookup" (func $hydra.lib.maps.lookup (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.null" (func $hydra.lib.maps.null (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.cases" (func $hydra.lib.maybes.cases (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.is_just" (func $hydra.lib.maybes.is_just (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.is_nothing" (func $hydra.lib.maybes.is_nothing (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.map" (func $hydra.lib.maybes.map (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.maybe" (func $hydra.lib.maybes.maybe (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.first" (func $hydra.lib.pairs.first (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.second" (func $hydra.lib.pairs.second (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.delete" (func $hydra.lib.sets.delete (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.difference" (func $hydra.lib.sets.difference (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.insert" (func $hydra.lib.sets.insert (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.member" (func $hydra.lib.sets.member (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.to_list" (func $hydra.lib.sets.to_list (param i32) (result i32) ) )
  (import "hydra.rewriting" "hydra.rewriting.fold_term_with_graph_and_path" (func $hydra.rewriting.fold_term_with_graph_and_path (param i32) (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.variables" "hydra.variables.free_variables_in_type" (func $hydra.variables.free_variables_in_type (param i32) (result i32) ) )
  (import "hydra.variables" "hydra.variables.free_variables_in_type_scheme" (func $hydra.variables.free_variables_in_type_scheme (param i32) (result i32) ) )
  (memory $memory 2 )
  (export "memory" (memory $memory) )
  (data (offset i32.const 1024 ) "\00\00\00\00\16\00\00\00\68\79\64\72\61\2e\6c\69\62\2e\6c\6f\67\69\63\2e\69\66\45\6c\73\65")
  (global $__bump_ptr (mut i32) i32.const 1056 )
  (export "__bump_ptr" (global $__bump_ptr) )
  (func $__alloc (param $sz i32) (result i32)
  global.get $__bump_ptr
  global.get $__bump_ptr
  local.get $sz
  i32.add
  global.set $__bump_ptr
)
  (export "hydra.validate.core.check_duplicate_bindings" (func $hydra.validate.core.check_duplicate_bindings) )
  (export "hydra.validate.core.check_duplicate_field_types" (func $hydra.validate.core.check_duplicate_field_types) )
  (export "hydra.validate.core.check_duplicate_fields" (func $hydra.validate.core.check_duplicate_fields) )
  (export "hydra.validate.core.check_shadowing" (func $hydra.validate.core.check_shadowing) )
  (export "hydra.validate.core.check_term" (func $hydra.validate.core.check_term) )
  (export "hydra.validate.core.check_undefined_type_variables_in_type" (func $hydra.validate.core.check_undefined_type_variables_in_type) )
  (export "hydra.validate.core.check_undefined_type_variables_in_type_scheme" (func $hydra.validate.core.check_undefined_type_variables_in_type_scheme) )
  (export "hydra.validate.core.check_void" (func $hydra.validate.core.check_void) )
  (export "hydra.validate.core.find_duplicate" (func $hydra.validate.core.find_duplicate) )
  (export "hydra.validate.core.find_duplicate_field_type" (func $hydra.validate.core.find_duplicate_field_type) )
  (export "hydra.validate.core.first_error" (func $hydra.validate.core.first_error) )
  (export "hydra.validate.core.first_type_error" (func $hydra.validate.core.first_type_error) )
  (export "hydra.validate.core.is_valid_name" (func $hydra.validate.core.is_valid_name) )
  (export "hydra.validate.core.term" (func $hydra.validate.core.term) )
  (export "hydra.validate.core.type" (func $hydra.validate.core.type) )
  (export "hydra.validate.core.validate_type_node" (func $hydra.validate.core.validate_type_node) )
  (func $hydra.validate.core.check_duplicate_bindings (param $path i32) (param $bindings i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $dup i32)
  (local $name i32)
  (local $names i32)
  i32.const 0
  local.get $bindings
  call $hydra.lib.lists.map
  local.set $names
  local.get $names
  call $hydra.validate.core.find_duplicate
  local.set $dup
  i32.const 1
  local.get $path
  local.get $name
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  local.get $dup
  call $hydra.lib.maybes.map
)
  (func $hydra.validate.core.check_duplicate_field_types (param $fields i32) (param $mk_error i32) (result i32)
  (local $dup i32)
  (local $name i32)
  (local $names i32)
  i32.const 0
  local.get $fields
  call $hydra.lib.lists.map
  local.set $names
  local.get $names
  call $hydra.validate.core.find_duplicate_field_type
  local.set $dup
  local.get $dup
  i32.const 0
  local.get $name
  drop
  local.get $mk_error
  drop
  i32.const 0
  call $hydra.lib.maybes.cases
)
  (func $hydra.validate.core.check_duplicate_fields (param $path i32) (param $names i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $dup i32)
  (local $name i32)
  local.get $names
  call $hydra.validate.core.find_duplicate
  local.set $dup
  i32.const 2
  local.get $path
  local.get $name
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  local.get $dup
  call $hydra.lib.maybes.map
)
  (func $hydra.validate.core.check_shadowing (param $path i32) (param $cx i32) (param $names i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $acc i32)
  (local $name i32)
  (local $result i32)
  local.get $acc
  local.get $name
  local.get $cx
  i32.load
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.is_just
  local.get $name
  local.get $cx
  i32.load offset=12
  call $hydra.lib.sets.member
  call $hydra.lib.logic.or
  i32.const 13
  local.get $path
  local.get $name
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 0
  call $hydra.lib.logic.if_else
  local.get $acc
  call $hydra.lib.maybes.cases
  i32.const 0
  local.get $names
  call $hydra.lib.lists.foldl
  local.set $result
  local.get $result
)
  (func $hydra.validate.core.check_term (param $typed i32) (param $path i32) (param $cx i32) (param $term i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $ann i32)
  (local $ann_map i32)
  (local $app i32)
  (local $arg i32)
  (local $arg_name i32)
  (local $b i32)
  (local $bindings i32)
  (local $bname i32)
  (local $body i32)
  (local $body_var i32)
  (local $bool_val i32)
  (local $cs i32)
  (local $cs_cases i32)
  (local $cs_default i32)
  (local $dom i32)
  (local $flds i32)
  (local $fun i32)
  (local $fun_name i32)
  (local $inj i32)
  (local $lam i32)
  (local $lit i32)
  (local $lt i32)
  (local $names i32)
  (local $param i32)
  (local $param_name i32)
  (local $prim_name i32)
  (local $proj i32)
  (local $rec i32)
  (local $ta i32)
  (local $tl i32)
  (local $tname i32)
  (local $ts i32)
  (local $tv_name i32)
  (local $unwrap_name i32)
  (local $uv_name i32)
  (local $v i32)
  (local $var_name i32)
  (local $wrap_name i32)
  (local $wt i32)
  (block $end_term (result i32)
  (block $wrap
  (block $variable
  (block $type_lambda
  (block $type_application
  (block $cases
  (block $project
  (block $lambda
  (block $inject
  (block $let
  (block $record
  (block $application
  (block $annotated
  local.get $term
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $annotated $application $record $let $inject $lambda $project $cases $type_application $type_lambda $variable $wrap $wrap
)
  local.get $v
  drop
  local.get $ann
  i32.load
  local.set $body
  local.get $ann
  i32.load offset=4
  local.set $ann_map
  local.get $ann_map
  call $hydra.lib.maps.null
  i32.const 5
  local.get $path
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 0
  call $hydra.lib.logic.if_else
  (block $end_term (result i32)
  (block $annotated
  local.get $body
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $annotated $annotated
)
  local.get $v
  drop
  i32.const 10
  local.get $path
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  br $end_term
)
  i32.const 12
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 2
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=8
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.get $__rec_ptr
  call $hydra.validate.core.first_error
  br $end_term
)
  local.get $v
  drop
  local.get $app
  i32.load
  local.set $fun
  local.get $app
  i32.load offset=4
  local.set $arg
  (block $end_term (result i32)
  (block $variable
  local.get $fun
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $variable $variable
)
  local.get $v
  drop
  local.get $prim_name
  drop
  i32.const 0
  drop
  i32.const 0
  i32.const 1028
  call $hydra.lib.equality.equal
  (block $end_term (result i32)
  (block $literal
  local.get $arg
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $literal $literal
)
  local.get $v
  drop
  (block $end_literal (result i32)
  (block $boolean
  local.get $lit
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $boolean $boolean
)
  local.get $v
  drop
  i32.const 0
  local.get $path
  local.get $bool_val
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  br $end_literal
)
  br $end_term
)
  i32.const 0
  call $hydra.lib.logic.if_else
  br $end_term
)
  (block $end_term (result i32)
  (block $variable
  local.get $fun
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $variable $variable
)
  local.get $v
  drop
  (block $end_term (result i32)
  (block $variable
  local.get $arg
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $variable $variable
)
  local.get $v
  drop
  local.get $fun_name
  local.get $arg_name
  call $hydra.lib.equality.equal
  i32.const 12
  local.get $path
  local.get $fun_name
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 0
  call $hydra.lib.logic.if_else
  br $end_term
)
  br $end_term
)
  (block $end_term (result i32)
  (block $lambda
  local.get $fun
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $lambda $lambda
)
  local.get $v
  drop
  local.get $lam
  i32.load
  local.set $param
  local.get $lam
  i32.load offset=8
  local.set $body
  (block $end_term (result i32)
  (block $variable
  local.get $body
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $variable $variable
)
  local.get $v
  drop
  local.get $param
  local.get $body_var
  call $hydra.lib.equality.equal
  i32.const 20
  local.get $path
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 0
  call $hydra.lib.logic.if_else
  br $end_term
)
  br $end_term
)
  (block $end_term (result i32)
  (block $unwrap
  local.get $fun
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $unwrap $unwrap
)
  local.get $v
  drop
  (block $end_term (result i32)
  (block $wrap
  local.get $arg
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $wrap $wrap
)
  local.get $v
  drop
  local.get $wt
  i32.load
  local.set $wrap_name
  local.get $unwrap_name
  local.get $wrap_name
  call $hydra.lib.equality.equal
  i32.const 11
  local.get $path
  local.get $unwrap_name
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 0
  call $hydra.lib.logic.if_else
  br $end_term
)
  br $end_term
)
  i32.const 20
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 4
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=16
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=12
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=8
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.get $__rec_ptr
  call $hydra.validate.core.first_error
  br $end_term
)
  local.get $v
  drop
  local.get $rec
  i32.load
  local.set $tname
  local.get $rec
  i32.load offset=4
  local.set $flds
  local.get $tname
  drop
  i32.const 0
  drop
  i32.const 0
  i32.const 1024
  call $hydra.lib.equality.equal
  i32.const 6
  local.get $path
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 0
  call $hydra.lib.logic.if_else
  local.get $path
  i32.const 0
  local.get $flds
  call $hydra.lib.lists.map
  call $hydra.validate.core.check_duplicate_fields
  i32.const 12
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 2
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=8
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.get $__rec_ptr
  call $hydra.validate.core.first_error
  br $end_term
)
  local.get $v
  drop
  local.get $lt
  i32.load
  local.set $bindings
  i32.const 0
  local.get $bindings
  call $hydra.lib.lists.map
  local.set $names
  local.get $bindings
  call $hydra.lib.lists.null
  i32.const 4
  local.get $path
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 0
  call $hydra.lib.logic.if_else
  local.get $path
  local.get $bindings
  call $hydra.validate.core.check_duplicate_bindings
  i32.const 0
  local.get $bname
  call $hydra.validate.core.is_valid_name
  i32.const 0
  i32.const 8
  local.get $path
  local.get $bname
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  call $hydra.lib.logic.if_else
  local.get $names
  call $hydra.lib.lists.map
  call $hydra.validate.core.first_error
  local.get $typed
  local.get $b
  i32.load offset=8
  i32.const 0
  local.get $path
  local.get $cx
  local.get $ts
  i32.const 16
  local.get $path
  local.get $uv_name
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  call $hydra.validate.core.check_undefined_type_variables_in_type_scheme
  call $hydra.lib.maybes.cases
  local.get $bindings
  call $hydra.lib.lists.map
  call $hydra.validate.core.first_error
  i32.const 0
  call $hydra.lib.logic.if_else
  i32.const 24
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 5
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=20
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=16
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=12
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=8
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.get $__rec_ptr
  call $hydra.validate.core.first_error
  br $end_term
)
  local.get $v
  drop
  local.get $inj
  i32.load
  local.set $tname
  local.get $tname
  drop
  i32.const 0
  drop
  i32.const 0
  i32.const 1024
  call $hydra.lib.equality.equal
  i32.const 6
  local.get $path
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 0
  call $hydra.lib.logic.if_else
  br $end_term
)
  local.get $v
  drop
  local.get $lam
  i32.load
  local.set $param_name
  local.get $param_name
  local.get $cx
  i32.load
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.is_just
  i32.const 13
  local.get $path
  local.get $param_name
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 0
  call $hydra.lib.logic.if_else
  local.get $param_name
  call $hydra.validate.core.is_valid_name
  i32.const 0
  i32.const 7
  local.get $path
  local.get $param_name
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  call $hydra.lib.logic.if_else
  local.get $typed
  local.get $lam
  i32.load offset=4
  i32.const 0
  local.get $path
  local.get $cx
  local.get $dom
  i32.const 17
  local.get $path
  local.get $uv_name
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  call $hydra.validate.core.check_undefined_type_variables_in_type
  call $hydra.lib.maybes.cases
  i32.const 0
  call $hydra.lib.logic.if_else
  i32.const 16
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 3
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=12
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=8
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.get $__rec_ptr
  call $hydra.validate.core.first_error
  br $end_term
)
  local.get $v
  drop
  local.get $proj
  i32.load
  local.set $tname
  local.get $tname
  drop
  i32.const 0
  drop
  i32.const 0
  i32.const 1024
  call $hydra.lib.equality.equal
  i32.const 6
  local.get $path
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 0
  call $hydra.lib.logic.if_else
  br $end_term
)
  local.get $v
  drop
  local.get $cs
  i32.load
  local.set $tname
  local.get $cs
  i32.load offset=4
  local.set $cs_default
  local.get $cs
  i32.load offset=8
  local.set $cs_cases
  local.get $tname
  drop
  i32.const 0
  drop
  i32.const 0
  i32.const 1024
  call $hydra.lib.equality.equal
  i32.const 6
  local.get $path
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 0
  call $hydra.lib.logic.if_else
  local.get $cs_cases
  call $hydra.lib.lists.null
  local.get $cs_default
  call $hydra.lib.maybes.is_nothing
  call $hydra.lib.logic.and
  i32.const 3
  local.get $path
  local.get $tname
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 0
  call $hydra.lib.logic.if_else
  local.get $path
  i32.const 0
  local.get $cs_cases
  call $hydra.lib.lists.map
  call $hydra.validate.core.check_duplicate_fields
  i32.const 16
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 3
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=12
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=8
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.get $__rec_ptr
  call $hydra.validate.core.first_error
  br $end_term
)
  local.get $v
  drop
  local.get $typed
  local.get $path
  local.get $cx
  local.get $ta
  i32.load offset=4
  i32.const 18
  local.get $path
  local.get $uv_name
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  call $hydra.validate.core.check_undefined_type_variables_in_type
  i32.const 0
  call $hydra.lib.logic.if_else
  br $end_term
)
  local.get $v
  drop
  local.get $tl
  i32.load
  local.set $tv_name
  local.get $tv_name
  local.get $tv_name
  local.get $cx
  i32.load offset=28
  call $hydra.lib.sets.delete
  call $hydra.lib.sets.member
  i32.const 14
  local.get $path
  local.get $tv_name
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 0
  call $hydra.lib.logic.if_else
  local.get $tv_name
  call $hydra.validate.core.is_valid_name
  i32.const 0
  i32.const 9
  local.get $path
  local.get $tv_name
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  call $hydra.lib.logic.if_else
  i32.const 12
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 2
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=8
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.get $__rec_ptr
  call $hydra.validate.core.first_error
  br $end_term
)
  local.get $v
  drop
  local.get $var_name
  local.get $cx
  i32.load
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.is_just
  local.get $var_name
  local.get $cx
  i32.load offset=12
  call $hydra.lib.sets.member
  local.get $var_name
  local.get $cx
  i32.load offset=20
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.is_just
  call $hydra.lib.logic.or
  call $hydra.lib.logic.or
  i32.const 0
  i32.const 15
  local.get $path
  local.get $var_name
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  call $hydra.lib.logic.if_else
  br $end_term
)
  local.get $v
  drop
  local.get $wt
  i32.load
  local.set $tname
  local.get $tname
  drop
  i32.const 0
  drop
  i32.const 0
  i32.const 1024
  call $hydra.lib.equality.equal
  i32.const 6
  local.get $path
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 0
  call $hydra.lib.logic.if_else
  br $end_term
)
)
  (func $hydra.validate.core.check_undefined_type_variables_in_type (param $path i32) (param $cx i32) (param $typ i32) (param $mk_error i32) (result i32)
  (local $first_undefined i32)
  (local $free_vars i32)
  (local $undefined i32)
  local.get $typ
  call $hydra.variables.free_variables_in_type
  local.set $free_vars
  local.get $free_vars
  local.get $cx
  i32.load offset=28
  call $hydra.lib.sets.difference
  local.set $undefined
  i32.const 0
  local.get $first_undefined
  drop
  local.get $mk_error
  drop
  i32.const 0
  local.get $undefined
  call $hydra.lib.sets.to_list
  call $hydra.lib.lists.maybe_head
  call $hydra.lib.maybes.maybe
)
  (func $hydra.validate.core.check_undefined_type_variables_in_type_scheme (param $path i32) (param $cx i32) (param $ts i32) (param $mk_error i32) (result i32)
  (local $first_undefined i32)
  (local $free_vars i32)
  (local $undefined i32)
  local.get $ts
  call $hydra.variables.free_variables_in_type_scheme
  local.set $free_vars
  local.get $free_vars
  local.get $cx
  i32.load offset=28
  call $hydra.lib.sets.difference
  local.set $undefined
  i32.const 0
  local.get $first_undefined
  drop
  local.get $mk_error
  drop
  i32.const 0
  local.get $undefined
  call $hydra.lib.sets.to_list
  call $hydra.lib.lists.maybe_head
  call $hydra.lib.maybes.maybe
)
  (func $hydra.validate.core.check_void (param $typ i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $v i32)
  (block $end_type (result i32)
  (block $void
  local.get $typ
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $void $void
)
  local.get $v
  drop
  i32.const 13
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  br $end_type
)
)
  (func $hydra.validate.core.find_duplicate (param $names i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $acc i32)
  (local $dup i32)
  (local $name i32)
  (local $result i32)
  (local $seen i32)
  local.get $acc
  call $hydra.lib.pairs.first
  local.set $seen
  local.get $acc
  call $hydra.lib.pairs.second
  local.set $dup
  local.get $dup
  local.get $name
  local.get $seen
  call $hydra.lib.sets.member
  local.get $seen
  local.get $name
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  local.get $name
  local.get $seen
  call $hydra.lib.sets.insert
  i32.const 0
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  call $hydra.lib.logic.if_else
  local.get $acc
  call $hydra.lib.maybes.cases
  i32.const 0
  i32.const 0
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  local.get $names
  call $hydra.lib.lists.foldl
  local.set $result
  local.get $result
  call $hydra.lib.pairs.second
)
  (func $hydra.validate.core.find_duplicate_field_type (param $names i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $acc i32)
  (local $dup i32)
  (local $name i32)
  (local $result i32)
  (local $seen i32)
  local.get $acc
  call $hydra.lib.pairs.first
  local.set $seen
  local.get $acc
  call $hydra.lib.pairs.second
  local.set $dup
  local.get $dup
  local.get $name
  local.get $seen
  call $hydra.lib.sets.member
  local.get $seen
  local.get $name
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  local.get $name
  local.get $seen
  call $hydra.lib.sets.insert
  i32.const 0
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  call $hydra.lib.logic.if_else
  local.get $acc
  call $hydra.lib.maybes.cases
  i32.const 0
  i32.const 0
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  local.get $names
  call $hydra.lib.lists.foldl
  local.set $result
  local.get $result
  call $hydra.lib.pairs.second
)
  (func $hydra.validate.core.first_error (param $checks i32) (result i32)
  (local $acc i32)
  (local $check i32)
  local.get $acc
  local.get $check
  local.get $acc
  call $hydra.lib.maybes.cases
  i32.const 0
  local.get $checks
  call $hydra.lib.lists.foldl
)
  (func $hydra.validate.core.first_type_error (param $checks i32) (result i32)
  (local $acc i32)
  (local $check i32)
  local.get $acc
  local.get $check
  local.get $acc
  call $hydra.lib.maybes.cases
  i32.const 0
  local.get $checks
  call $hydra.lib.lists.foldl
)
  (func $hydra.validate.core.is_valid_name (param $name i32) (result i32)
  local.get $name
  drop
  i32.const 0
  drop
  i32.const 0
  i32.const 1024
  call $hydra.lib.equality.equal
  call $hydra.lib.logic.not
)
  (func $hydra.validate.core.term (param $typed i32) (param $g i32) (param $t i32) (result i32)
  (local $acc i32)
  (local $check_result i32)
  (local $cx i32)
  (local $err i32)
  (local $path i32)
  (local $recurse i32)
  (local $trm i32)
  local.get $acc
  local.get $typed
  local.get $path
  local.get $cx
  local.get $trm
  call $hydra.validate.core.check_term
  local.set $check_result
  local.get $check_result
  i32.const 0
  drop
  local.get $trm
  drop
  local.get $recurse
  drop
  i32.const 0
  local.get $err
  call $hydra.lib.maybes.cases
  local.get $acc
  call $hydra.lib.maybes.cases
  local.get $g
  i32.const 0
  local.get $t
  call $hydra.rewriting.fold_term_with_graph_and_path
)
  (func $hydra.validate.core.type (param $bound_vars i32) (param $typ i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $ann i32)
  (local $at i32)
  (local $check_result i32)
  (local $err i32)
  (local $et i32)
  (local $f i32)
  (local $fields i32)
  (local $ft i32)
  (local $lt i32)
  (local $mt i32)
  (local $new_bound i32)
  (local $pt i32)
  (local $st i32)
  (local $v i32)
  (local $wt i32)
  local.get $bound_vars
  local.get $typ
  call $hydra.validate.core.validate_type_node
  local.set $check_result
  local.get $check_result
  (block $end_type (result i32)
  (block $wrap
  (block $union
  (block $set
  (block $record
  (block $pair
  (block $maybe
  (block $map
  (block $list
  (block $function
  (block $either
  (block $application
  (block $annotated
  (block $forall
  local.get $typ
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $forall $annotated $application $either $function $list $map $maybe $pair $record $set $union $wrap $wrap
)
  local.get $v
  drop
  local.get $ft
  i32.load
  local.get $bound_vars
  call $hydra.lib.sets.insert
  local.set $new_bound
  local.get $new_bound
  local.get $ft
  i32.load offset=4
  call $hydra.validate.core.type
  br $end_type
)
  local.get $v
  drop
  local.get $bound_vars
  local.get $ann
  i32.load
  call $hydra.validate.core.type
  br $end_type
)
  local.get $v
  drop
  local.get $bound_vars
  local.get $at
  i32.load
  call $hydra.validate.core.type
  local.get $bound_vars
  local.get $at
  i32.load offset=4
  call $hydra.validate.core.type
  i32.const 12
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 2
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=8
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.get $__rec_ptr
  call $hydra.validate.core.first_type_error
  br $end_type
)
  local.get $v
  drop
  local.get $bound_vars
  local.get $et
  i32.load
  call $hydra.validate.core.type
  local.get $bound_vars
  local.get $et
  i32.load offset=4
  call $hydra.validate.core.type
  i32.const 12
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 2
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=8
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.get $__rec_ptr
  call $hydra.validate.core.first_type_error
  br $end_type
)
  local.get $v
  drop
  local.get $bound_vars
  local.get $ft
  i32.load
  call $hydra.validate.core.type
  local.get $bound_vars
  local.get $ft
  i32.load offset=4
  call $hydra.validate.core.type
  i32.const 12
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 2
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=8
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.get $__rec_ptr
  call $hydra.validate.core.first_type_error
  br $end_type
)
  local.get $v
  drop
  local.get $bound_vars
  local.get $lt
  call $hydra.validate.core.type
  br $end_type
)
  local.get $v
  drop
  local.get $bound_vars
  local.get $mt
  i32.load
  call $hydra.validate.core.type
  local.get $bound_vars
  local.get $mt
  i32.load offset=4
  call $hydra.validate.core.type
  i32.const 12
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 2
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=8
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.get $__rec_ptr
  call $hydra.validate.core.first_type_error
  br $end_type
)
  local.get $v
  drop
  local.get $bound_vars
  local.get $mt
  call $hydra.validate.core.type
  br $end_type
)
  local.get $v
  drop
  local.get $bound_vars
  local.get $pt
  i32.load
  call $hydra.validate.core.type
  local.get $bound_vars
  local.get $pt
  i32.load offset=4
  call $hydra.validate.core.type
  i32.const 12
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 2
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=8
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.get $__rec_ptr
  call $hydra.validate.core.first_type_error
  br $end_type
)
  local.get $v
  drop
  local.get $bound_vars
  local.get $f
  i32.load offset=4
  call $hydra.validate.core.type
  local.get $fields
  call $hydra.lib.lists.map
  call $hydra.validate.core.first_type_error
  br $end_type
)
  local.get $v
  drop
  local.get $bound_vars
  local.get $st
  call $hydra.validate.core.type
  br $end_type
)
  local.get $v
  drop
  local.get $bound_vars
  local.get $f
  i32.load offset=4
  call $hydra.validate.core.type
  local.get $fields
  call $hydra.lib.lists.map
  call $hydra.validate.core.first_type_error
  br $end_type
)
  local.get $v
  drop
  local.get $bound_vars
  local.get $wt
  call $hydra.validate.core.type
  br $end_type
)
  local.get $err
  call $hydra.lib.maybes.cases
)
  (func $hydra.validate.core.validate_type_node (param $bound_vars i32) (param $typ i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $ann i32)
  (local $ann_map i32)
  (local $body i32)
  (local $dup_name i32)
  (local $elem_type i32)
  (local $et i32)
  (local $f i32)
  (local $fields i32)
  (local $ft i32)
  (local $key_type i32)
  (local $lt i32)
  (local $mt i32)
  (local $param_name i32)
  (local $pt i32)
  (local $single_field i32)
  (local $v i32)
  (local $var_name i32)
  (block $end_type (result i32)
  (block $variable
  (block $union
  (block $set
  (block $record
  (block $pair
  (block $map
  (block $list
  (block $function
  (block $forall
  (block $either
  (block $annotated
  local.get $typ
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $annotated $either $forall $function $list $map $pair $record $set $union $variable $variable
)
  local.get $v
  drop
  local.get $ann
  i32.load
  local.set $body
  local.get $ann
  i32.load offset=4
  local.set $ann_map
  local.get $ann_map
  call $hydra.lib.maps.null
  i32.const 3
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 0
  call $hydra.lib.logic.if_else
  (block $end_type (result i32)
  (block $annotated
  local.get $body
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $annotated $annotated
)
  local.get $v
  drop
  i32.const 7
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  br $end_type
)
  i32.const 12
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 2
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=8
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.get $__rec_ptr
  call $hydra.validate.core.first_type_error
  br $end_type
)
  local.get $v
  drop
  local.get $et
  i32.load
  call $hydra.validate.core.check_void
  local.get $et
  i32.load offset=4
  call $hydra.validate.core.check_void
  i32.const 12
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 2
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=8
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.get $__rec_ptr
  call $hydra.validate.core.first_type_error
  br $end_type
)
  local.get $v
  drop
  local.get $ft
  i32.load
  local.set $param_name
  local.get $param_name
  local.get $bound_vars
  call $hydra.lib.sets.member
  i32.const 11
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  local.get $param_name
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 0
  call $hydra.lib.logic.if_else
  local.get $param_name
  call $hydra.validate.core.is_valid_name
  i32.const 0
  i32.const 5
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  local.get $param_name
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  call $hydra.lib.logic.if_else
  i32.const 12
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 2
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=8
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.get $__rec_ptr
  call $hydra.validate.core.first_type_error
  br $end_type
)
  local.get $v
  drop
  local.get $ft
  i32.load offset=4
  call $hydra.validate.core.check_void
  br $end_type
)
  local.get $v
  drop
  local.get $lt
  call $hydra.validate.core.check_void
  br $end_type
)
  local.get $v
  drop
  local.get $mt
  i32.load
  local.set $key_type
  (block $end_type (result i32)
  (block $function
  local.get $key_type
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $function $function
)
  local.get $v
  drop
  i32.const 8
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  local.get $key_type
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  br $end_type
)
  local.get $key_type
  call $hydra.validate.core.check_void
  local.get $mt
  i32.load offset=4
  call $hydra.validate.core.check_void
  i32.const 16
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 3
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=12
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=8
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.get $__rec_ptr
  call $hydra.validate.core.first_type_error
  br $end_type
)
  local.get $v
  drop
  local.get $pt
  i32.load
  call $hydra.validate.core.check_void
  local.get $pt
  i32.load offset=4
  call $hydra.validate.core.check_void
  i32.const 12
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 2
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=8
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.get $__rec_ptr
  call $hydra.validate.core.first_type_error
  br $end_type
)
  local.get $v
  drop
  local.get $fields
  call $hydra.lib.lists.null
  i32.const 2
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 0
  call $hydra.lib.logic.if_else
  local.get $fields
  i32.const 0
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  local.get $dup_name
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  call $hydra.validate.core.check_duplicate_field_types
  local.get $f
  i32.load offset=4
  call $hydra.validate.core.check_void
  local.get $fields
  call $hydra.lib.lists.map
  call $hydra.validate.core.first_type_error
  i32.const 16
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 3
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=12
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=8
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.get $__rec_ptr
  call $hydra.validate.core.first_type_error
  br $end_type
)
  local.get $v
  drop
  (block $end_type (result i32)
  (block $function
  local.get $elem_type
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $function $function
)
  local.get $v
  drop
  i32.const 9
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  local.get $elem_type
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  br $end_type
)
  local.get $elem_type
  call $hydra.validate.core.check_void
  i32.const 12
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 2
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=8
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.get $__rec_ptr
  call $hydra.validate.core.first_type_error
  br $end_type
)
  local.get $v
  drop
  local.get $fields
  call $hydra.lib.lists.null
  i32.const 4
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 0
  call $hydra.lib.logic.if_else
  local.get $fields
  call $hydra.lib.lists.length
  i32.const 1
  call $hydra.lib.equality.equal
  i32.const 0
  i32.const 10
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  local.get $single_field
  i32.load
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  local.get $fields
  call $hydra.lib.lists.maybe_head
  call $hydra.lib.maybes.maybe
  i32.const 0
  call $hydra.lib.logic.if_else
  local.get $fields
  i32.const 1
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  local.get $dup_name
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  call $hydra.validate.core.check_duplicate_field_types
  local.get $f
  i32.load offset=4
  call $hydra.validate.core.check_void
  local.get $fields
  call $hydra.lib.lists.map
  call $hydra.validate.core.first_type_error
  i32.const 20
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 4
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=16
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=12
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=8
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.get $__rec_ptr
  call $hydra.validate.core.first_type_error
  br $end_type
)
  local.get $v
  drop
  local.get $var_name
  local.get $bound_vars
  call $hydra.lib.sets.member
  i32.const 0
  i32.const 12
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  local.get $var_name
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  call $hydra.lib.logic.if_else
  br $end_type
)
)
)
