(module
  (import "hydra.decode.core" "hydra.decode.core.type" (func $hydra.decode.core.type (param i32) (param i32) (result i32) ) )
  (import "hydra.lexical" "hydra.lexical.lookup_binding" (func $hydra.lexical.lookup_binding (param i32) (param i32) (result i32) ) )
  (import "hydra.lexical" "hydra.lexical.require_binding" (func $hydra.lexical.require_binding (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.bimap" (func $hydra.lib.eithers.bimap (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.bind" (func $hydra.lib.eithers.bind (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map" (func $hydra.lib.eithers.map (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.equality" "hydra.lib.equality.equal" (func $hydra.lib.equality.equal (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.cons" (func $hydra.lib.lists.cons (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.filter" (func $hydra.lib.lists.filter (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.find" (func $hydra.lib.lists.find (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.foldl" (func $hydra.lib.lists.foldl (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.length" (func $hydra.lib.lists.length (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.maybe_head" (func $hydra.lib.lists.maybe_head (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.null" (func $hydra.lib.lists.null (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.zip" (func $hydra.lib.lists.zip (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_int32" (func $hydra.lib.literals.show_int32 (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.if_else" (func $hydra.lib.logic.if_else (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.from_list" (func $hydra.lib.maps.from_list (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.insert" (func $hydra.lib.maps.insert (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.lookup" (func $hydra.lib.maps.lookup (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.to_list" (func $hydra.lib.maps.to_list (param i32) (result i32) ) )
  (import "hydra.lib.math" "hydra.lib.math.add" (func $hydra.lib.math.add (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.from_maybe" (func $hydra.lib.maybes.from_maybe (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.map" (func $hydra.lib.maybes.map (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.maybe" (func $hydra.lib.maybes.maybe (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.first" (func $hydra.lib.pairs.first (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.second" (func $hydra.lib.pairs.second (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat2" (func $hydra.lib.strings.cat2 (param i32) (param i32) (result i32) ) )
  (import "hydra.names" "hydra.names.fresh_names" (func $hydra.names.fresh_names (param i32) (param i32) (result i32) ) )
  (import "hydra.scoping" "hydra.scoping.type_scheme_to_f_type" (func $hydra.scoping.type_scheme_to_f_type (param i32) (result i32) ) )
  (import "hydra.show.core" "hydra.show.core.type" (func $hydra.show.core.type (param i32) (result i32) ) )
  (import "hydra.strip" "hydra.strip.deannotate_type" (func $hydra.strip.deannotate_type (param i32) (result i32) ) )
  (import "hydra.strip" "hydra.strip.deannotate_type_scheme_recursive" (func $hydra.strip.deannotate_type_scheme_recursive (param i32) (result i32) ) )
  (import "hydra.substitution" "hydra.substitution.subst_in_type" (func $hydra.substitution.subst_in_type (param i32) (param i32) (result i32) ) )
  (import "hydra.variables" "hydra.variables.substitute_type_variables" (func $hydra.variables.substitute_type_variables (param i32) (param i32) (result i32) ) )
  (memory $memory 2 )
  (export "memory" (memory $memory) )
  (data (offset i32.const 1024 ) "\05\00\00\00\20\74\79\70\65\02\00\00\00\3a\20\01\00\00\00\5f\14\00\00\00\72\65\63\6f\72\64\20\6f\72\20\75\6e\69\6f\6e\20\74\79\70\65\0b\00\00\00\72\65\63\6f\72\64\20\74\79\70\65\04\00\00\00\74\79\70\65\05\00\00\00\75\6e\69\6f\6e")
  (global $__bump_ptr (mut i32) i32.const 1104 )
  (export "__bump_ptr" (global $__bump_ptr) )
  (func $__alloc (param $sz i32) (result i32)
  global.get $__bump_ptr
  global.get $__bump_ptr
  local.get $sz
  i32.add
  global.set $__bump_ptr
)
  (export "hydra.resolution.dereference_type" (func $hydra.resolution.dereference_type) )
  (export "hydra.resolution.f_type_is_polymorphic" (func $hydra.resolution.f_type_is_polymorphic) )
  (export "hydra.resolution.field_map" (func $hydra.resolution.field_map) )
  (export "hydra.resolution.field_type_map" (func $hydra.resolution.field_type_map) )
  (export "hydra.resolution.field_types" (func $hydra.resolution.field_types) )
  (export "hydra.resolution.find_field_type" (func $hydra.resolution.find_field_type) )
  (export "hydra.resolution.fully_strip_and_normalize_type" (func $hydra.resolution.fully_strip_and_normalize_type) )
  (export "hydra.resolution.fully_strip_type" (func $hydra.resolution.fully_strip_type) )
  (export "hydra.resolution.instantiate_type" (func $hydra.resolution.instantiate_type) )
  (export "hydra.resolution.instantiate_type_scheme" (func $hydra.resolution.instantiate_type_scheme) )
  (export "hydra.resolution.nominal_application" (func $hydra.resolution.nominal_application) )
  (export "hydra.resolution.require_record_type" (func $hydra.resolution.require_record_type) )
  (export "hydra.resolution.require_row_type" (func $hydra.resolution.require_row_type) )
  (export "hydra.resolution.require_schema_type" (func $hydra.resolution.require_schema_type) )
  (export "hydra.resolution.require_type" (func $hydra.resolution.require_type) )
  (export "hydra.resolution.require_union_field" (func $hydra.resolution.require_union_field) )
  (export "hydra.resolution.require_union_type" (func $hydra.resolution.require_union_type) )
  (export "hydra.resolution.resolve_type" (func $hydra.resolution.resolve_type) )
  (export "hydra.resolution.type_to_type_scheme" (func $hydra.resolution.type_to_type_scheme) )
  (func $hydra.resolution.dereference_type (param $cx i32) (param $graph i32) (param $name i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $_a i32)
  (local $_e i32)
  (local $el i32)
  (local $mel i32)
  local.get $graph
  local.get $name
  call $hydra.lexical.lookup_binding
  local.set $mel
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
  i32.const 0
  i32.const 7
  i32.const 4
  i32.const 1083
  local.get $_e
  drop
  i32.const 0
  drop
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
  local.get $_a
  local.get $graph
  local.get $el
  i32.load offset=4
  call $hydra.decode.core.type
  call $hydra.lib.eithers.bimap
  call $hydra.lib.eithers.map
  local.get $mel
  call $hydra.lib.maybes.maybe
)
  (func $hydra.resolution.f_type_is_polymorphic (param $typ i32) (result i32)
  (local $__rec_ptr i32)
  (local $at i32)
  (local $v i32)
  (block $end_type (result i32)
  (block $forall
  (block $annotated
  local.get $typ
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $annotated $forall $forall
)
  local.get $v
  drop
  local.get $at
  i32.load
  call $hydra.resolution.f_type_is_polymorphic
  br $end_type
)
  local.get $v
  drop
  i32.const 1
  br $end_type
)
)
  (func $hydra.resolution.field_map (param $fields i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $f i32)
  (local $to_pair i32)
  local.get $f
  i32.load
  local.get $f
  i32.load offset=4
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
  local.set $to_pair
  local.get $to_pair
  local.get $fields
  call $hydra.lib.lists.map
  call $hydra.lib.maps.from_list
)
  (func $hydra.resolution.field_type_map (param $fields i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $f i32)
  (local $to_pair i32)
  local.get $f
  i32.load
  local.get $f
  i32.load offset=4
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
  local.set $to_pair
  local.get $to_pair
  local.get $fields
  call $hydra.lib.lists.map
  call $hydra.lib.maps.from_list
)
  (func $hydra.resolution.field_types (param $cx i32) (param $graph i32) (param $t i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $_a i32)
  (local $_e i32)
  (local $decoded_type i32)
  (local $el i32)
  (local $fields i32)
  (local $ft i32)
  (local $name i32)
  (local $rt i32)
  (local $to_map i32)
  (local $ts i32)
  (local $v i32)
  local.get $ft
  i32.load
  local.get $ft
  i32.load offset=4
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
  call $hydra.lib.lists.map
  call $hydra.lib.maps.from_list
  local.set $to_map
  (block $end_type (result i32)
  (block $variable
  (block $union
  (block $record
  (block $forall
  local.get $t
  call $hydra.strip.deannotate_type
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $forall $record $union $variable $variable
)
  local.get $v
  drop
  local.get $cx
  local.get $graph
  local.get $ft
  i32.load offset=4
  call $hydra.resolution.field_types
  br $end_type
)
  local.get $v
  drop
  i32.const 1
  local.get $rt
  drop
  local.get $to_map
  drop
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
  br $end_type
)
  local.get $v
  drop
  i32.const 1
  local.get $rt
  drop
  local.get $to_map
  drop
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
  br $end_type
)
  local.get $v
  drop
  local.get $graph
  local.get $name
  call $hydra.lexical.require_binding
  i32.const 7
  i32.const 4
  i32.const 1083
  local.get $_e
  drop
  i32.const 0
  drop
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
  local.get $_a
  local.get $graph
  local.get $el
  i32.load offset=4
  call $hydra.decode.core.type
  call $hydra.lib.eithers.bimap
  local.get $cx
  local.get $graph
  local.get $decoded_type
  call $hydra.resolution.field_types
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  local.get $cx
  local.get $graph
  local.get $ts
  i32.load offset=4
  call $hydra.resolution.field_types
  local.get $name
  local.get $graph
  i32.load offset=24
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.maybe
  br $end_type
)
)
  (func $hydra.resolution.find_field_type (param $cx i32) (param $fname i32) (param $fields i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $ft i32)
  (local $matching_fields i32)
  (local $no_match i32)
  local.get $ft
  i32.load
  drop
  i32.const 0
  drop
  i32.const 0
  local.get $fname
  drop
  i32.const 0
  drop
  i32.const 0
  call $hydra.lib.equality.equal
  local.get $fields
  call $hydra.lib.lists.filter
  local.set $matching_fields
  i32.const 0
  i32.const 7
  i32.const 2
  local.get $fname
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
  local.set $no_match
  local.get $matching_fields
  call $hydra.lib.lists.null
  local.get $no_match
  local.get $matching_fields
  call $hydra.lib.lists.length
  i32.const 1
  call $hydra.lib.equality.equal
  local.get $no_match
  i32.const 1
  local.get $ft
  i32.load offset=4
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
  local.get $matching_fields
  call $hydra.lib.lists.maybe_head
  call $hydra.lib.maybes.maybe
  i32.const 0
  i32.const 4
  i32.const 2
  local.get $fname
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
  call $hydra.lib.logic.if_else
)
  (func $hydra.resolution.fully_strip_and_normalize_type (param $typ i32) (result i32)
  (local $__rec_ptr i32)
  (local $body i32)
  (local $depth i32)
  (local $ft i32)
  (local $go i32)
  (local $new_var i32)
  (local $old_var i32)
  (local $result i32)
  (local $subst i32)
  (local $t i32)
  (local $v i32)
  (block $end_type (result i32)
  (block $forall
  local.get $t
  call $hydra.strip.deannotate_type
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $forall $forall
)
  local.get $v
  drop
  local.get $ft
  i32.load
  local.set $old_var
  i32.const 1039
  local.get $depth
  call $hydra.lib.literals.show_int32
  call $hydra.lib.strings.cat2
  local.set $new_var
  local.get $depth
  i32.const 1
  call $hydra.lib.math.add
  drop
  local.get $old_var
  local.get $new_var
  local.get $subst
  call $hydra.lib.maps.insert
  drop
  local.get $ft
  i32.load offset=4
  drop
  local.get $go
  drop
  i32.const 0
  br $end_type
)
  local.set $go
  i32.const 0
  drop
  i32.const 0
  drop
  local.get $typ
  drop
  local.get $go
  drop
  i32.const 0
  local.set $result
  local.get $result
  call $hydra.lib.pairs.first
  local.set $subst
  local.get $result
  call $hydra.lib.pairs.second
  local.set $body
  local.get $subst
  local.get $body
  call $hydra.variables.substitute_type_variables
)
  (func $hydra.resolution.fully_strip_type (param $typ i32) (result i32)
  (local $__rec_ptr i32)
  (local $ft i32)
  (local $v i32)
  (block $end_type (result i32)
  (block $forall
  local.get $typ
  call $hydra.strip.deannotate_type
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $forall $forall
)
  local.get $v
  drop
  local.get $ft
  i32.load offset=4
  call $hydra.resolution.fully_strip_type
  br $end_type
)
)
  (func $hydra.resolution.instantiate_type (param $cx i32) (param $typ i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $result i32)
  local.get $cx
  local.get $typ
  call $hydra.resolution.type_to_type_scheme
  call $hydra.resolution.instantiate_type_scheme
  local.set $result
  local.get $result
  call $hydra.lib.pairs.first
  call $hydra.scoping.type_scheme_to_f_type
  local.get $result
  call $hydra.lib.pairs.second
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
)
  (func $hydra.resolution.instantiate_type_scheme (param $cx i32) (param $scheme i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $cx2 i32)
  (local $kv i32)
  (local $name_subst i32)
  (local $new_vars i32)
  (local $old_constraints i32)
  (local $old_vars i32)
  (local $renamed_constraints i32)
  (local $result i32)
  (local $subst i32)
  (local $x i32)
  local.get $scheme
  i32.load
  local.set $old_vars
  local.get $old_vars
  call $hydra.lib.lists.length
  local.get $cx
  call $hydra.names.fresh_names
  local.set $result
  local.get $result
  call $hydra.lib.pairs.first
  local.set $new_vars
  local.get $result
  call $hydra.lib.pairs.second
  local.set $cx2
  local.get $old_vars
  i32.const 14
  local.get $x
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
  local.get $new_vars
  call $hydra.lib.lists.map
  call $hydra.lib.lists.zip
  call $hydra.lib.maps.from_list
  local.set $subst
  local.get $old_vars
  local.get $new_vars
  call $hydra.lib.lists.zip
  call $hydra.lib.maps.from_list
  local.set $name_subst
  local.get $kv
  call $hydra.lib.pairs.first
  local.get $kv
  call $hydra.lib.pairs.first
  local.get $name_subst
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.from_maybe
  local.get $kv
  call $hydra.lib.pairs.second
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
  local.get $old_constraints
  call $hydra.lib.maps.to_list
  call $hydra.lib.lists.map
  call $hydra.lib.maps.from_list
  local.get $scheme
  i32.load offset=8
  call $hydra.lib.maybes.map
  local.set $renamed_constraints
  local.get $new_vars
  local.get $subst
  local.get $scheme
  i32.load offset=4
  call $hydra.substitution.subst_in_type
  local.get $renamed_constraints
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
  local.get $cx2
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
)
  (func $hydra.resolution.nominal_application (param $tname i32) (param $args i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $a i32)
  (local $t i32)
  i32.const 1
  local.get $t
  local.get $a
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
  i32.const 14
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
  local.get $args
  call $hydra.lib.lists.foldl
)
  (func $hydra.resolution.require_record_type (param $cx i32) (param $graph i32) (param $name i32) (result i32)
  (local $__rec_ptr i32)
  (local $rt i32)
  (local $t i32)
  (local $to_record i32)
  (local $v i32)
  (block $end_type (result i32)
  (block $record
  local.get $t
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
  local.get $rt
  br $end_type
)
  local.set $to_record
  local.get $cx
  i32.const 1068
  local.get $to_record
  local.get $graph
  local.get $name
  call $hydra.resolution.require_row_type
)
  (func $hydra.resolution.require_row_type (param $cx i32) (param $label i32) (param $getter i32) (param $graph i32) (param $name i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $at i32)
  (local $ft i32)
  (local $raw_type i32)
  (local $t i32)
  (local $v i32)
  (local $x i32)
  (block $end_type (result i32)
  (block $forall
  (block $annotated
  local.get $t
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $annotated $forall $forall
)
  local.get $v
  drop
  local.get $at
  i32.load
  drop
  local.get $raw_type
  drop
  i32.const 0
  br $end_type
)
  local.get $v
  drop
  local.get $ft
  i32.load offset=4
  drop
  local.get $raw_type
  drop
  i32.const 0
  br $end_type
)
  local.set $raw_type
  local.get $cx
  local.get $graph
  local.get $name
  call $hydra.resolution.require_type
  i32.const 0
  i32.const 7
  i32.const 4
  local.get $label
  i32.const 1024
  call $hydra.lib.strings.cat2
  local.get $name
  drop
  i32.const 0
  drop
  i32.const 0
  i32.const 1033
  local.get $t
  call $hydra.show.core.type
  call $hydra.lib.strings.cat2
  call $hydra.lib.strings.cat2
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
  i32.const 1
  local.get $x
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
  local.get $t
  drop
  local.get $raw_type
  drop
  i32.const 0
  drop
  local.get $getter
  drop
  i32.const 0
  call $hydra.lib.maybes.maybe
  call $hydra.lib.eithers.bind
)
  (func $hydra.resolution.require_schema_type (param $cx i32) (param $types i32) (param $tname i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $ts i32)
  i32.const 0
  i32.const 7
  i32.const 0
  local.get $tname
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
  i32.const 1
  local.get $cx
  local.get $ts
  call $hydra.strip.deannotate_type_scheme_recursive
  call $hydra.resolution.instantiate_type_scheme
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
  local.get $tname
  local.get $types
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.maybe
)
  (func $hydra.resolution.require_type (param $cx i32) (param $graph i32) (param $name i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $ts i32)
  i32.const 0
  i32.const 7
  i32.const 0
  local.get $name
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
  i32.const 1
  local.get $ts
  call $hydra.scoping.type_scheme_to_f_type
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
  local.get $graph
  i32.load offset=4
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.maybe
  i32.const 1
  local.get $ts
  call $hydra.scoping.type_scheme_to_f_type
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
  local.get $graph
  i32.load offset=24
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.maybe
)
  (func $hydra.resolution.require_union_field (param $cx i32) (param $graph i32) (param $tname i32) (param $fname i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $ft i32)
  (local $no_match_err i32)
  (local $rt i32)
  (local $with_row_type i32)
  i32.const 0
  i32.const 7
  i32.const 2
  local.get $fname
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
  local.set $no_match_err
  local.get $no_match_err
  i32.const 1
  local.get $ft
  i32.load offset=4
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
  local.get $ft
  i32.load
  local.get $fname
  call $hydra.lib.equality.equal
  local.get $rt
  call $hydra.lib.lists.find
  call $hydra.lib.maybes.maybe
  local.set $with_row_type
  local.get $cx
  local.get $graph
  local.get $tname
  call $hydra.resolution.require_union_type
  local.get $with_row_type
  call $hydra.lib.eithers.bind
)
  (func $hydra.resolution.require_union_type (param $cx i32) (param $graph i32) (param $name i32) (result i32)
  (local $__rec_ptr i32)
  (local $rt i32)
  (local $t i32)
  (local $to_union i32)
  (local $v i32)
  (block $end_type (result i32)
  (block $union
  local.get $t
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
  local.get $rt
  br $end_type
)
  local.set $to_union
  local.get $cx
  i32.const 1091
  local.get $to_union
  local.get $graph
  local.get $name
  call $hydra.resolution.require_row_type
)
  (func $hydra.resolution.resolve_type (param $graph i32) (param $typ i32) (result i32)
  (local $__rec_ptr i32)
  (local $name i32)
  (local $ts i32)
  (local $v i32)
  (block $end_type (result i32)
  (block $variable
  local.get $typ
  call $hydra.strip.deannotate_type
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
  local.get $ts
  call $hydra.scoping.type_scheme_to_f_type
  local.get $name
  local.get $graph
  i32.load offset=4
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.map
  local.get $ts
  call $hydra.scoping.type_scheme_to_f_type
  local.get $name
  local.get $graph
  i32.load offset=24
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.maybe
  br $end_type
)
)
  (func $hydra.resolution.type_to_type_scheme (param $t0 i32) (result i32)
  (local $__rec_ptr i32)
  (local $ft i32)
  (local $helper i32)
  (local $t i32)
  (local $v i32)
  (local $vars i32)
  (block $end_type (result i32)
  (block $forall
  local.get $t
  call $hydra.strip.deannotate_type
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $forall $forall
)
  local.get $v
  drop
  local.get $ft
  i32.load
  local.get $vars
  call $hydra.lib.lists.cons
  drop
  local.get $ft
  i32.load offset=4
  drop
  local.get $helper
  drop
  i32.const 0
  br $end_type
)
  local.set $helper
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  drop
  local.get $t0
  drop
  local.get $helper
  drop
  i32.const 0
)
)
