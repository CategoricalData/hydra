(module
  (import "hydra.lib.eithers" "hydra.lib.eithers.bind" (func $hydra.lib.eithers.bind (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map" (func $hydra.lib.eithers.map (param i32) (result i32) ) )
  (import "hydra.lib.equality" "hydra.lib.equality.equal" (func $hydra.lib.equality.equal (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.concat2" (func $hydra.lib.lists.concat2 (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.foldl" (func $hydra.lib.lists.foldl (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.head" (func $hydra.lib.lists.head (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.length" (func $hydra.lib.lists.length (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.null" (func $hydra.lib.lists.null (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.tail" (func $hydra.lib.lists.tail (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.zip_with" (func $hydra.lib.lists.zip_with (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.and" (func $hydra.lib.logic.and (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.if_else" (func $hydra.lib.logic.if_else (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.or" (func $hydra.lib.logic.or (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.lookup" (func $hydra.lib.maps.lookup (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.is_just" (func $hydra.lib.maybes.is_just (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat2" (func $hydra.lib.strings.cat2 (param i32) (result i32) ) )
  (import "hydra.rewriting" "hydra.rewriting.fold_over_type" (func $hydra.rewriting.fold_over_type (param i32) (result i32) ) )
  (import "hydra.show.core" "hydra.show.core.type" (func $hydra.show.core.type (param i32) (result i32) ) )
  (import "hydra.strip" "hydra.strip.deannotate_type" (func $hydra.strip.deannotate_type (param i32) (result i32) ) )
  (import "hydra.substitution" "hydra.substitution.compose_type_subst" (func $hydra.substitution.compose_type_subst (param i32) (result i32) ) )
  (import "hydra.substitution" "hydra.substitution.id_type_subst" (func $hydra.substitution.id_type_subst (param i32) (result i32) ) )
  (import "hydra.substitution" "hydra.substitution.singleton_type_subst" (func $hydra.substitution.singleton_type_subst (param i32) (result i32) ) )
  (import "hydra.substitution" "hydra.substitution.substitute_in_constraints" (func $hydra.substitution.substitute_in_constraints (param i32) (result i32) ) )
  (memory $memory 1 )
  (export "memory" (memory $memory) )
  (export "hydra.unification.join_types" (func $hydra.unification.join_types) )
  (export "hydra.unification.unify_type_constraints" (func $hydra.unification.unify_type_constraints) )
  (export "hydra.unification.unify_type_lists" (func $hydra.unification.unify_type_lists) )
  (export "hydra.unification.unify_types" (func $hydra.unification.unify_types) )
  (export "hydra.unification.variable_occurs_in_type" (func $hydra.unification.variable_occurs_in_type) )
  (func $hydra.unification.join_types (param $cx i32) (param $left i32) (param $right i32) (param $comment i32) (result i32)
  (local $assert_equal i32)
  (local $cannot_unify i32)
  (local $join_list i32)
  (local $join_one i32)
  (local $join_row_types i32)
  (local $l i32)
  (local $left2 i32)
  (local $lefts i32)
  (local $r i32)
  (local $right2 i32)
  (local $rights i32)
  (local $sleft i32)
  (local $sright i32)
  local.get $left
  call $hydra.strip.deannotate_type
  local.set $sleft
  local.get $right
  call $hydra.strip.deannotate_type
  local.set $sright
  local.get $l
  local.get $r
  i32.const 0 ;; string: "join types; "
  local.get $comment
  call $hydra.lib.strings.cat2
  local.set $join_one
  i32.const 0
  local.get $sleft
  local.get $sright
  i32.const 0 ;; string: "cannot unify "
  local.get $sleft
  call $hydra.show.core.type
  call $hydra.lib.strings.cat2
  i32.const 0 ;; string: " with "
  call $hydra.lib.strings.cat2
  local.get $sright
  call $hydra.show.core.type
  call $hydra.lib.strings.cat2
  local.get $cx
  local.set $cannot_unify
  local.get $sleft
  local.get $sright
  call $hydra.lib.equality.equal
  i32.const 1
  i32.const 0
  ;; list elements follow
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
  local.get $cannot_unify
  call $hydra.lib.logic.if_else
  local.set $join_list
  ;; project field: name
  local.get $left2
  call $hydra.lib.lists.map
  call $hydra.lib.lists.length
  ;; project field: name
  local.get $right2
  call $hydra.lib.lists.map
  call $hydra.lib.lists.length
  call $hydra.lib.equality.equal
  call $hydra.lib.logic.and
  i32.const 1
  nop
  nop
  call $hydra.lib.equality.equal
  ;; project field: name
  local.get $left2
  call $hydra.lib.lists.map
  ;; project field: name
  local.get $right2
  call $hydra.lib.lists.map
  call $hydra.lib.lists.zip_with
  call $hydra.lib.lists.foldl
  call $hydra.lib.logic.and
  ;; project field: type
  local.get $left2
  call $hydra.lib.lists.map
  ;; project field: type
  local.get $right2
  call $hydra.lib.lists.map
  local.get $join_list
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
  br_table $application $either $function $list $literal $map $maybe $pair $record $set $union $unit $void $wrap $wrap
)
  (block $end_type (result i32)
  (block $application
  local.get $sright
  br_table $application $application
)
  i32.const 1
  i32.const 2
  ;; list elements follow
  local.get $l
  ;; project field: function
  local.get $r
  ;; project field: function
  local.get $join_one
  local.get $l
  ;; project field: argument
  local.get $r
  ;; project field: argument
  local.get $join_one
  br $end_type
)
  br $end_type
)
  (block $end_type (result i32)
  (block $either
  local.get $sright
  br_table $either $either
)
  i32.const 1
  i32.const 2
  ;; list elements follow
  local.get $l
  ;; project field: left
  local.get $r
  ;; project field: left
  local.get $join_one
  local.get $l
  ;; project field: right
  local.get $r
  ;; project field: right
  local.get $join_one
  br $end_type
)
  br $end_type
)
  (block $end_type (result i32)
  (block $function
  local.get $sright
  br_table $function $function
)
  i32.const 1
  i32.const 2
  ;; list elements follow
  local.get $l
  ;; project field: domain
  local.get $r
  ;; project field: domain
  local.get $join_one
  local.get $l
  ;; project field: codomain
  local.get $r
  ;; project field: codomain
  local.get $join_one
  br $end_type
)
  br $end_type
)
  (block $end_type (result i32)
  (block $list
  local.get $sright
  br_table $list $list
)
  i32.const 1
  i32.const 1
  ;; list elements follow
  local.get $l
  local.get $r
  local.get $join_one
  br $end_type
)
  br $end_type
)
  local.get $assert_equal
  br $end_type
)
  (block $end_type (result i32)
  (block $map
  local.get $sright
  br_table $map $map
)
  i32.const 1
  i32.const 2
  ;; list elements follow
  local.get $l
  ;; project field: keys
  local.get $r
  ;; project field: keys
  local.get $join_one
  local.get $l
  ;; project field: values
  local.get $r
  ;; project field: values
  local.get $join_one
  br $end_type
)
  br $end_type
)
  (block $end_type (result i32)
  (block $maybe
  local.get $sright
  br_table $maybe $maybe
)
  i32.const 1
  i32.const 1
  ;; list elements follow
  local.get $l
  local.get $r
  local.get $join_one
  br $end_type
)
  br $end_type
)
  (block $end_type (result i32)
  (block $pair
  local.get $sright
  br_table $pair $pair
)
  i32.const 1
  i32.const 2
  ;; list elements follow
  local.get $l
  ;; project field: first
  local.get $r
  ;; project field: first
  local.get $join_one
  local.get $l
  ;; project field: second
  local.get $r
  ;; project field: second
  local.get $join_one
  br $end_type
)
  br $end_type
)
  (block $end_type (result i32)
  (block $record
  local.get $sright
  br_table $record $record
)
  local.get $l
  local.get $r
  local.get $join_row_types
  br $end_type
)
  br $end_type
)
  (block $end_type (result i32)
  (block $set
  local.get $sright
  br_table $set $set
)
  i32.const 1
  i32.const 1
  ;; list elements follow
  local.get $l
  local.get $r
  local.get $join_one
  br $end_type
)
  br $end_type
)
  (block $end_type (result i32)
  (block $union
  local.get $sright
  br_table $union $union
)
  local.get $l
  local.get $r
  local.get $join_row_types
  br $end_type
)
  br $end_type
)
  (block $end_type (result i32)
  (block $unit
  local.get $sright
  br_table $unit $unit
)
  i32.const 1
  i32.const 0
  ;; list elements follow
  br $end_type
)
  br $end_type
)
  (block $end_type (result i32)
  (block $void
  local.get $sright
  br_table $void $void
)
  i32.const 1
  i32.const 0
  ;; list elements follow
  br $end_type
)
  br $end_type
)
  (block $end_type (result i32)
  (block $wrap
  local.get $sright
  br_table $wrap $wrap
)
  i32.const 1
  i32.const 1
  ;; list elements follow
  local.get $l
  local.get $r
  local.get $join_one
  br $end_type
)
  br $end_type
)
)
  (func $hydra.unification.unify_type_constraints (param $cx i32) (param $schema_types i32) (param $constraints i32) (result i32)
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
  (local $v i32)
  (local $with_constraint i32)
  (local $with_constraints i32)
  (local $with_result i32)
  local.get $c
  ;; project field: left
  call $hydra.strip.deannotate_type
  local.set $sleft
  local.get $c
  ;; project field: right
  call $hydra.strip.deannotate_type
  local.set $sright
  local.get $c
  ;; project field: comment
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
  i32.const 0 ;; string: "Variable "
  nop
  call $hydra.lib.strings.cat2
  i32.const 0 ;; string: " appears free in type "
  call $hydra.lib.strings.cat2
  local.get $t
  call $hydra.show.core.type
  call $hydra.lib.strings.cat2
  i32.const 0 ;; string: " ("
  call $hydra.lib.strings.cat2
  local.get $comment
  call $hydra.lib.strings.cat2
  i32.const 0 ;; string: ")"
  call $hydra.lib.strings.cat2
  local.get $cx
  local.get $v
  local.get $t
  local.get $bind
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
  br_table $variable $variable
)
  local.get $name
  local.get $sleft
  local.get $try_binding
  br $end_type
)
  local.set $dflt
  (block $end_type (result i32)
  (block $variable
  local.get $sleft
  br_table $variable $variable
)
  (block $end_type (result i32)
  (block $variable
  local.get $sright
  br_table $variable $variable
)
  nop
  nop
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
  i32.const 0 ;; string: "Attempted to unify schema names "
  nop
  call $hydra.lib.strings.cat2
  i32.const 0 ;; string: " and "
  call $hydra.lib.strings.cat2
  nop
  call $hydra.lib.strings.cat2
  i32.const 0 ;; string: " ("
  call $hydra.lib.strings.cat2
  local.get $comment
  call $hydra.lib.strings.cat2
  i32.const 0 ;; string: ")"
  call $hydra.lib.strings.cat2
  local.get $cx
  local.get $name2
  local.get $sleft
  local.get $bind
  call $hydra.lib.logic.if_else
  local.get $name
  local.get $sright
  local.get $bind
  call $hydra.lib.logic.if_else
  call $hydra.lib.logic.if_else
  br $end_type
)
  br $end_type
)
  local.set $with_constraint
  local.get $constraints
  call $hydra.lib.lists.null
  i32.const 1
  call $hydra.substitution.id_type_subst
  local.get $constraints
  call $hydra.lib.lists.head
  local.get $constraints
  call $hydra.lib.lists.tail
  local.get $with_constraint
  call $hydra.lib.logic.if_else
)
  (func $hydra.unification.unify_type_lists (param $cx i32) (param $schema_types i32) (param $l i32) (param $r i32) (param $comment i32) (result i32)
  (local $l2 i32)
  (local $r2 i32)
  (local $to_constraint i32)
  local.get $l2
  local.get $r2
  local.get $comment
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
  local.get $cx
  local.get $schema_types
  i32.const 1
  ;; list elements follow
  local.get $l
  local.get $r
  local.get $comment
  call $hydra.unification.unify_type_constraints
)
  (func $hydra.unification.variable_occurs_in_type (param $var i32) (param $typ0 i32) (result i32)
  (local $b i32)
  (local $try_type i32)
  (local $typ i32)
  (block $end_type (result i32)
  (block $variable
  local.get $typ
  br_table $variable $variable
)
  local.get $b
  nop
  nop
  call $hydra.lib.equality.equal
  call $hydra.lib.logic.or
  br $end_type
)
  local.set $try_type
  i32.const 0
  local.get $try_type
  i32.const 0
  local.get $typ0
  call $hydra.rewriting.fold_over_type
)
)
