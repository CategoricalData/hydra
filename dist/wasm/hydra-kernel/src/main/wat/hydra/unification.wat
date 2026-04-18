(module
  (import "hydra.lib.eithers" "hydra.lib.eithers.bind" (func $hydra.lib.eithers.bind (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map" (func $hydra.lib.eithers.map (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.equality" "hydra.lib.equality.equal" (func $hydra.lib.equality.equal (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.concat2" (func $hydra.lib.lists.concat2 (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.foldl" (func $hydra.lib.lists.foldl (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.length" (func $hydra.lib.lists.length (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.uncons" (func $hydra.lib.lists.uncons (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.zip_with" (func $hydra.lib.lists.zip_with (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.and" (func $hydra.lib.logic.and (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.if_else" (func $hydra.lib.logic.if_else (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.or" (func $hydra.lib.logic.or (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.lookup" (func $hydra.lib.maps.lookup (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.is_just" (func $hydra.lib.maybes.is_just (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.maybe" (func $hydra.lib.maybes.maybe (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.first" (func $hydra.lib.pairs.first (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.second" (func $hydra.lib.pairs.second (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat2" (func $hydra.lib.strings.cat2 (param i32) (param i32) (result i32) ) )
  (import "hydra.rewriting" "hydra.rewriting.fold_over_type" (func $hydra.rewriting.fold_over_type (param i32) (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.show.core" "hydra.show.core.type" (func $hydra.show.core.type (param i32) (result i32) ) )
  (import "hydra.strip" "hydra.strip.deannotate_type" (func $hydra.strip.deannotate_type (param i32) (result i32) ) )
  (import "hydra.substitution" "hydra.substitution.compose_type_subst" (func $hydra.substitution.compose_type_subst (param i32) (param i32) (result i32) ) )
  (import "hydra.substitution" "hydra.substitution.singleton_type_subst" (func $hydra.substitution.singleton_type_subst (param i32) (param i32) (result i32) ) )
  (import "hydra.substitution" "hydra.substitution.substitute_in_constraints" (func $hydra.substitution.substitute_in_constraints (param i32) (param i32) (result i32) ) )
  (memory $memory 2 )
  (export "memory" (memory $memory) )
  (data (offset i32.const 1024 ) "\02\00\00\00\20\28\05\00\00\00\20\61\6e\64\20\16\00\00\00\20\61\70\70\65\61\72\73\20\66\72\65\65\20\69\6e\20\74\79\70\65\20\06\00\00\00\20\77\69\74\68\20\01\00\00\00\29\20\00\00\00\41\74\74\65\6d\70\74\65\64\20\74\6f\20\75\6e\69\66\79\20\73\63\68\65\6d\61\20\6e\61\6d\65\73\20\09\00\00\00\56\61\72\69\61\62\6c\65\20\0d\00\00\00\63\61\6e\6e\6f\74\20\75\6e\69\66\79\20\0c\00\00\00\6a\6f\69\6e\20\74\79\70\65\73\3b\20")
  (global $__bump_ptr (mut i32) i32.const 1168 )
  (export "__bump_ptr" (global $__bump_ptr) )
  (func $__alloc (param $sz i32) (result i32)
  global.get $__bump_ptr
  global.get $__bump_ptr
  local.get $sz
  i32.add
  global.set $__bump_ptr
)
  (export "hydra.unification.join_types" (func $hydra.unification.join_types) )
  (export "hydra.unification.unify_type_constraints" (func $hydra.unification.unify_type_constraints) )
  (export "hydra.unification.unify_type_lists" (func $hydra.unification.unify_type_lists) )
  (export "hydra.unification.unify_types" (func $hydra.unification.unify_types) )
  (export "hydra.unification.variable_occurs_in_type" (func $hydra.unification.variable_occurs_in_type) )
  (func $hydra.unification.join_types (param $cx i32) (param $left i32) (param $right i32) (param $comment i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $assert_equal i32)
  (local $cannot_unify i32)
  (local $join_list i32)
  (local $join_one i32)
  (local $join_row_types i32)
  (local $l i32)
  (local $left2 i32)
  (local $left3 i32)
  (local $lefts i32)
  (local $r i32)
  (local $right2 i32)
  (local $right3 i32)
  (local $rights i32)
  (local $sleft i32)
  (local $sright i32)
  (local $v i32)
  local.get $left
  call $hydra.strip.deannotate_type
  local.set $sleft
  local.get $right
  call $hydra.strip.deannotate_type
  local.set $sright
  local.get $l
  local.get $r
  i32.const 1146
  local.get $comment
  call $hydra.lib.strings.cat2
  i32.const 12
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=8
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  local.set $join_one
  i32.const 0
  local.get $sleft
  local.get $sright
  i32.const 1129
  local.get $sleft
  call $hydra.show.core.type
  call $hydra.lib.strings.cat2
  i32.const 1065
  call $hydra.lib.strings.cat2
  local.get $sright
  call $hydra.show.core.type
  call $hydra.lib.strings.cat2
  i32.const 12
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=8
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
  local.set $cannot_unify
  local.get $sleft
  local.get $sright
  call $hydra.lib.equality.equal
  i32.const 1
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
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
  local.get $cannot_unify
  call $hydra.lib.logic.if_else
  local.set $assert_equal
  local.get $lefts
  call $hydra.lib.lists.length
  local.get $rights
  call $hydra.lib.lists.length
  call $hydra.lib.equality.equal
  i32.const 1
  local.get $join_one
  local.get $lefts
  local.get $rights
  call $hydra.lib.lists.zip_with
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
  local.get $cannot_unify
  call $hydra.lib.logic.if_else
  local.set $join_list
  i32.const 0
  local.get $left2
  call $hydra.lib.lists.map
  call $hydra.lib.lists.length
  i32.const 0
  local.get $right2
  call $hydra.lib.lists.map
  call $hydra.lib.lists.length
  call $hydra.lib.equality.equal
  i32.const 0
  i32.const 1
  local.get $left3
  drop
  i32.const 0
  drop
  i32.const 0
  local.get $right3
  drop
  i32.const 0
  drop
  i32.const 0
  call $hydra.lib.equality.equal
  i32.const 0
  local.get $left2
  call $hydra.lib.lists.map
  i32.const 0
  local.get $right2
  call $hydra.lib.lists.map
  call $hydra.lib.lists.zip_with
  call $hydra.lib.lists.foldl
  call $hydra.lib.logic.and
  i32.const 0
  local.get $left2
  call $hydra.lib.lists.map
  drop
  i32.const 0
  local.get $right2
  call $hydra.lib.lists.map
  drop
  local.get $join_list
  drop
  i32.const 0
  local.get $cannot_unify
  call $hydra.lib.logic.if_else
  local.set $join_row_types
  (block $end_type (result i32)
  (block $wrap
  (block $void
  (block $unit
  (block $union
  (block $set
  (block $record
  (block $pair
  (block $maybe
  (block $map
  (block $literal
  (block $list
  (block $function
  (block $either
  (block $application
  local.get $sleft
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $application $either $function $list $literal $map $maybe $pair $record $set $union $unit $void $wrap $wrap
)
  local.get $v
  drop
  (block $end_type (result i32)
  (block $application
  local.get $sright
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $application $application
)
  local.get $v
  drop
  i32.const 1
  local.get $l
  i32.load
  drop
  local.get $r
  i32.load
  drop
  local.get $join_one
  drop
  i32.const 0
  local.get $l
  i32.load offset=4
  drop
  local.get $r
  i32.load offset=4
  drop
  local.get $join_one
  drop
  i32.const 0
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
  br $end_type
)
  local.get $v
  drop
  (block $end_type (result i32)
  (block $either
  local.get $sright
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $either $either
)
  local.get $v
  drop
  i32.const 1
  local.get $l
  i32.load
  drop
  local.get $r
  i32.load
  drop
  local.get $join_one
  drop
  i32.const 0
  local.get $l
  i32.load offset=4
  drop
  local.get $r
  i32.load offset=4
  drop
  local.get $join_one
  drop
  i32.const 0
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
  br $end_type
)
  local.get $v
  drop
  (block $end_type (result i32)
  (block $function
  local.get $sright
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
  i32.const 1
  local.get $l
  i32.load
  drop
  local.get $r
  i32.load
  drop
  local.get $join_one
  drop
  i32.const 0
  local.get $l
  i32.load offset=4
  drop
  local.get $r
  i32.load offset=4
  drop
  local.get $join_one
  drop
  i32.const 0
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
  br $end_type
)
  local.get $v
  drop
  (block $end_type (result i32)
  (block $list
  local.get $sright
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $list $list
)
  local.get $v
  drop
  i32.const 1
  local.get $l
  drop
  local.get $r
  drop
  local.get $join_one
  drop
  i32.const 0
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 1
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
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
  br $end_type
)
  local.get $v
  drop
  local.get $assert_equal
  br $end_type
)
  local.get $v
  drop
  (block $end_type (result i32)
  (block $map
  local.get $sright
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $map $map
)
  local.get $v
  drop
  i32.const 1
  local.get $l
  i32.load
  drop
  local.get $r
  i32.load
  drop
  local.get $join_one
  drop
  i32.const 0
  local.get $l
  i32.load offset=4
  drop
  local.get $r
  i32.load offset=4
  drop
  local.get $join_one
  drop
  i32.const 0
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
  br $end_type
)
  local.get $v
  drop
  (block $end_type (result i32)
  (block $maybe
  local.get $sright
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $maybe $maybe
)
  local.get $v
  drop
  i32.const 1
  local.get $l
  drop
  local.get $r
  drop
  local.get $join_one
  drop
  i32.const 0
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 1
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
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
  br $end_type
)
  local.get $v
  drop
  (block $end_type (result i32)
  (block $pair
  local.get $sright
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $pair $pair
)
  local.get $v
  drop
  i32.const 1
  local.get $l
  i32.load
  drop
  local.get $r
  i32.load
  drop
  local.get $join_one
  drop
  i32.const 0
  local.get $l
  i32.load offset=4
  drop
  local.get $r
  i32.load offset=4
  drop
  local.get $join_one
  drop
  i32.const 0
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
  br $end_type
)
  local.get $v
  drop
  (block $end_type (result i32)
  (block $record
  local.get $sright
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $record $record
)
  local.get $v
  drop
  local.get $l
  drop
  local.get $r
  drop
  local.get $join_row_types
  drop
  i32.const 0
  br $end_type
)
  br $end_type
)
  local.get $v
  drop
  (block $end_type (result i32)
  (block $set
  local.get $sright
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $set $set
)
  local.get $v
  drop
  i32.const 1
  local.get $l
  drop
  local.get $r
  drop
  local.get $join_one
  drop
  i32.const 0
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 1
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
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
  br $end_type
)
  local.get $v
  drop
  (block $end_type (result i32)
  (block $union
  local.get $sright
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $union $union
)
  local.get $v
  drop
  local.get $l
  drop
  local.get $r
  drop
  local.get $join_row_types
  drop
  i32.const 0
  br $end_type
)
  br $end_type
)
  local.get $v
  drop
  (block $end_type (result i32)
  (block $unit
  local.get $sright
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $unit $unit
)
  local.get $v
  drop
  i32.const 1
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
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
  br $end_type
)
  local.get $v
  drop
  (block $end_type (result i32)
  (block $void
  local.get $sright
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
  i32.const 1
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
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
  br $end_type
)
  local.get $v
  drop
  (block $end_type (result i32)
  (block $wrap
  local.get $sright
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
  i32.const 1
  local.get $l
  drop
  local.get $r
  drop
  local.get $join_one
  drop
  i32.const 0
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 1
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
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
  br $end_type
)
)
  (func $hydra.unification.unify_type_constraints (param $cx i32) (param $schema_types i32) (param $constraints i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $bind i32)
  (local $c i32)
  (local $comment i32)
  (local $constraints2 i32)
  (local $dflt i32)
  (local $name i32)
  (local $name2 i32)
  (local $no_vars i32)
  (local $rest i32)
  (local $s i32)
  (local $sleft i32)
  (local $sright i32)
  (local $subst i32)
  (local $t i32)
  (local $try_binding i32)
  (local $uc i32)
  (local $v i32)
  (local $with_constraint i32)
  (local $with_constraints i32)
  (local $with_result i32)
  local.get $c
  i32.load
  call $hydra.strip.deannotate_type
  local.set $sleft
  local.get $c
  i32.load offset=4
  call $hydra.strip.deannotate_type
  local.set $sright
  local.get $c
  i32.load offset=8
  local.set $comment
  local.get $v
  local.get $t
  call $hydra.substitution.singleton_type_subst
  local.set $subst
  local.get $subst
  local.get $s
  call $hydra.substitution.compose_type_subst
  local.set $with_result
  local.get $with_result
  local.get $cx
  local.get $schema_types
  local.get $subst
  local.get $rest
  call $hydra.substitution.substitute_in_constraints
  call $hydra.unification.unify_type_constraints
  call $hydra.lib.eithers.map
  local.set $bind
  local.get $v
  local.get $t
  call $hydra.unification.variable_occurs_in_type
  i32.const 0
  local.get $sleft
  local.get $sright
  i32.const 1116
  local.get $v
  drop
  i32.const 0
  drop
  i32.const 0
  call $hydra.lib.strings.cat2
  i32.const 1039
  call $hydra.lib.strings.cat2
  local.get $t
  call $hydra.show.core.type
  call $hydra.lib.strings.cat2
  i32.const 1024
  call $hydra.lib.strings.cat2
  local.get $comment
  call $hydra.lib.strings.cat2
  i32.const 1075
  call $hydra.lib.strings.cat2
  i32.const 12
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=8
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
  local.get $v
  drop
  local.get $t
  drop
  local.get $bind
  drop
  i32.const 0
  call $hydra.lib.logic.if_else
  local.set $try_binding
  local.get $cx
  local.get $schema_types
  local.get $constraints2
  local.get $rest
  call $hydra.lib.lists.concat2
  call $hydra.unification.unify_type_constraints
  local.set $with_constraints
  local.get $cx
  local.get $sleft
  local.get $sright
  local.get $comment
  call $hydra.unification.join_types
  local.get $with_constraints
  call $hydra.lib.eithers.bind
  local.set $no_vars
  (block $end_type (result i32)
  (block $variable
  local.get $sright
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
  local.get $name
  drop
  local.get $sleft
  drop
  local.get $try_binding
  drop
  i32.const 0
  br $end_type
)
  local.set $dflt
  (block $end_type (result i32)
  (block $variable
  local.get $sleft
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
  (block $end_type (result i32)
  (block $variable
  local.get $sright
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
  local.get $name
  drop
  i32.const 0
  drop
  i32.const 0
  local.get $name2
  drop
  i32.const 0
  drop
  i32.const 0
  call $hydra.lib.equality.equal
  local.get $cx
  local.get $schema_types
  local.get $rest
  call $hydra.unification.unify_type_constraints
  local.get $name
  local.get $schema_types
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.is_just
  local.get $name2
  local.get $schema_types
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.is_just
  i32.const 0
  local.get $sleft
  local.get $sright
  i32.const 1080
  local.get $name
  drop
  i32.const 0
  drop
  i32.const 0
  call $hydra.lib.strings.cat2
  i32.const 1030
  call $hydra.lib.strings.cat2
  local.get $name2
  drop
  i32.const 0
  drop
  i32.const 0
  call $hydra.lib.strings.cat2
  i32.const 1024
  call $hydra.lib.strings.cat2
  local.get $comment
  call $hydra.lib.strings.cat2
  i32.const 1075
  call $hydra.lib.strings.cat2
  i32.const 12
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=8
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
  local.get $name2
  drop
  local.get $sleft
  drop
  local.get $bind
  drop
  i32.const 0
  call $hydra.lib.logic.if_else
  local.get $name
  drop
  local.get $sright
  drop
  local.get $bind
  drop
  i32.const 0
  call $hydra.lib.logic.if_else
  call $hydra.lib.logic.if_else
  br $end_type
)
  br $end_type
)
  local.set $with_constraint
  i32.const 1
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
  local.get $uc
  call $hydra.lib.pairs.first
  drop
  local.get $uc
  call $hydra.lib.pairs.second
  drop
  local.get $with_constraint
  drop
  i32.const 0
  local.get $constraints
  call $hydra.lib.lists.uncons
  call $hydra.lib.maybes.maybe
)
  (func $hydra.unification.unify_type_lists (param $cx i32) (param $schema_types i32) (param $l i32) (param $r i32) (param $comment i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $l2 i32)
  (local $r2 i32)
  (local $to_constraint i32)
  local.get $l2
  local.get $r2
  local.get $comment
  i32.const 12
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=8
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  local.set $to_constraint
  local.get $cx
  local.get $schema_types
  local.get $to_constraint
  local.get $l
  local.get $r
  call $hydra.lib.lists.zip_with
  call $hydra.unification.unify_type_constraints
)
  (func $hydra.unification.unify_types (param $cx i32) (param $schema_types i32) (param $l i32) (param $r i32) (param $comment i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  local.get $cx
  local.get $schema_types
  local.get $l
  local.get $r
  local.get $comment
  i32.const 12
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=8
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
  local.get $__rec_ptr
  i32.const 1
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.get $__rec_ptr
  call $hydra.unification.unify_type_constraints
)
  (func $hydra.unification.variable_occurs_in_type (param $var i32) (param $typ0 i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $b i32)
  (local $try_type i32)
  (local $typ i32)
  (local $v i32)
  (block $end_type (result i32)
  (block $variable
  local.get $typ
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
  local.get $b
  local.get $v
  drop
  i32.const 0
  drop
  i32.const 0
  local.get $var
  drop
  i32.const 0
  drop
  i32.const 0
  call $hydra.lib.equality.equal
  call $hydra.lib.logic.or
  br $end_type
)
  local.set $try_type
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
  local.get $try_type
  i32.const 0
  local.get $typ0
  call $hydra.rewriting.fold_over_type
)
)
