(module
  (import "hydra.formatting" "hydra.formatting.show_list" (func $hydra.formatting.show_list (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.length" (func $hydra.lib.lists.length (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_int32" (func $hydra.lib.literals.show_int32 (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.to_list" (func $hydra.lib.sets.to_list (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat" (func $hydra.lib.strings.cat (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat2" (func $hydra.lib.strings.cat2 (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.intercalate" (func $hydra.lib.strings.intercalate (param i32) (param i32) (result i32) ) )
  (import "hydra.show.core" "hydra.show.core.binding" (func $hydra.show.core.binding (param i32) (result i32) ) )
  (import "hydra.show.core" "hydra.show.core.type" (func $hydra.show.core.type (param i32) (result i32) ) )
  (import "hydra.show.error.core" "hydra.show.error.core.duplicate_binding_error" (func $hydra.show.error.core.duplicate_binding_error (param i32) (result i32) ) )
  (import "hydra.show.error.core" "hydra.show.error.core.duplicate_field_error" (func $hydra.show.error.core.duplicate_field_error (param i32) (result i32) ) )
  (import "hydra.show.error.core" "hydra.show.error.core.undefined_field_error" (func $hydra.show.error.core.undefined_field_error (param i32) (result i32) ) )
  (import "hydra.show.error.core" "hydra.show.error.core.undefined_term_variable_error" (func $hydra.show.error.core.undefined_term_variable_error (param i32) (result i32) ) )
  (import "hydra.show.error.core" "hydra.show.error.core.unexpected_term_variant_error" (func $hydra.show.error.core.unexpected_term_variant_error (param i32) (result i32) ) )
  (import "hydra.show.error.core" "hydra.show.error.core.unexpected_type_variant_error" (func $hydra.show.error.core.unexpected_type_variant_error (param i32) (result i32) ) )
  (import "hydra.show.error.core" "hydra.show.error.core.untyped_term_variable_error" (func $hydra.show.error.core.untyped_term_variable_error (param i32) (result i32) ) )
  (import "hydra.show.typing" "hydra.show.typing.type_subst" (func $hydra.show.typing.type_subst (param i32) (result i32) ) )
  (import "hydra.show.variants" "hydra.show.variants.term_variant" (func $hydra.show.variants.term_variant (param i32) (result i32) ) )
  (memory $memory 2 )
  (export "memory" (memory $memory) )
  (data (offset i32.const 1024 ) "\39\00\00\00\20\61\70\70\6c\69\65\64\20\74\6f\20\74\68\65\20\77\72\6f\6e\67\20\6e\75\6d\62\65\72\20\6f\66\20\74\79\70\65\20\61\72\67\75\6d\65\6e\74\73\20\28\65\78\70\65\63\74\65\64\20\0b\00\00\00\20\62\75\74\20\66\6f\75\6e\64\20\04\00\00\00\20\69\6e\20\13\00\00\00\20\74\79\70\65\20\61\72\67\75\6d\65\6e\74\28\73\29\3a\20\06\00\00\00\20\77\69\74\68\20\03\00\00\00\29\3a\20\02\00\00\00\2c\20\06\00\00\00\2c\20\67\6f\74\20\12\00\00\00\2e\20\54\72\79\69\6e\67\20\74\6f\20\61\70\70\6c\79\20\02\00\00\00\3a\20\10\00\00\00\64\65\63\6f\64\69\6e\67\20\65\72\72\6f\72\3a\20\10\00\00\00\65\78\74\72\61\63\74\69\6f\6e\20\65\72\72\6f\72\17\00\00\00\69\6e\63\6f\72\72\65\63\74\20\75\6e\69\66\69\63\61\74\69\6f\6e\3a\20\0f\00\00\00\69\6e\66\65\72\65\6e\63\65\20\65\72\72\6f\72\13\00\00\00\6e\6f\74\20\61\20\66\6f\72\61\6c\6c\20\74\79\70\65\3a\20\15\00\00\00\6e\6f\74\20\61\20\66\75\6e\63\74\69\6f\6e\20\74\79\70\65\3a\20\10\00\00\00\72\65\73\6f\6c\75\74\69\6f\6e\20\65\72\72\6f\72\05\00\00\00\74\79\70\65\20\18\00\00\00\74\79\70\65\20\6d\69\73\6d\61\74\63\68\3a\20\65\78\70\65\63\74\65\64\20\19\00\00\00\75\6e\62\6f\75\6e\64\20\74\79\70\65\20\76\61\72\69\61\62\6c\65\73\3a\20\7b\0e\00\00\00\75\6e\65\71\75\61\6c\20\74\79\70\65\73\20\20\00\00\00\75\6e\69\66\69\63\61\74\69\6f\6e\20\65\72\72\6f\72\3a\20\63\61\6e\6e\6f\74\20\75\6e\69\66\79\20\1a\00\00\00\75\6e\73\75\70\70\6f\72\74\65\64\20\74\65\72\6d\20\76\61\72\69\61\6e\74\3a\20\0e\00\00\00\75\6e\74\79\70\65\64\20\6c\61\6d\62\64\61\15\00\00\00\75\6e\74\79\70\65\64\20\6c\65\74\20\62\69\6e\64\69\6e\67\3a\20\0a\00\00\00\7d\20\69\6e\20\74\79\70\65\20")
  (global $__bump_ptr (mut i32) i32.const 1568 )
  (export "__bump_ptr" (global $__bump_ptr) )
  (func $__alloc (param $sz i32) (result i32)
  global.get $__bump_ptr
  global.get $__bump_ptr
  local.get $sz
  i32.add
  global.set $__bump_ptr
)
  (export "hydra.show.errors.checking_error" (func $hydra.show.errors.checking_error) )
  (export "hydra.show.errors.decoding_error" (func $hydra.show.errors.decoding_error) )
  (export "hydra.show.errors.error" (func $hydra.show.errors.error) )
  (export "hydra.show.errors.incorrect_unification_error" (func $hydra.show.errors.incorrect_unification_error) )
  (export "hydra.show.errors.not_a_forall_type_error" (func $hydra.show.errors.not_a_forall_type_error) )
  (export "hydra.show.errors.not_a_function_type_error" (func $hydra.show.errors.not_a_function_type_error) )
  (export "hydra.show.errors.other_error" (func $hydra.show.errors.other_error) )
  (export "hydra.show.errors.type_arity_mismatch_error" (func $hydra.show.errors.type_arity_mismatch_error) )
  (export "hydra.show.errors.type_mismatch_error" (func $hydra.show.errors.type_mismatch_error) )
  (export "hydra.show.errors.unbound_type_variables_error" (func $hydra.show.errors.unbound_type_variables_error) )
  (export "hydra.show.errors.unequal_types_error" (func $hydra.show.errors.unequal_types_error) )
  (export "hydra.show.errors.unification_error" (func $hydra.show.errors.unification_error) )
  (export "hydra.show.errors.unsupported_term_variant_error" (func $hydra.show.errors.unsupported_term_variant_error) )
  (export "hydra.show.errors.untyped_lambda_error" (func $hydra.show.errors.untyped_lambda_error) )
  (export "hydra.show.errors.untyped_let_binding_error" (func $hydra.show.errors.untyped_let_binding_error) )
  (func $hydra.show.errors.checking_error (param $ce i32) (result i32)
  (local $__rec_ptr i32)
  (local $v i32)
  (block $end_checking_error (result i32)
  (block $untyped_let_binding
  (block $untyped_lambda
  (block $unsupported_term_variant
  (block $unequal_types
  (block $unbound_type_variables
  (block $type_mismatch
  (block $type_arity_mismatch
  (block $not_a_function_type
  (block $not_a_forall_type
  (block $incorrect_unification
  local.get $ce
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $incorrect_unification $not_a_forall_type $not_a_function_type $type_arity_mismatch $type_mismatch $unbound_type_variables $unequal_types $unsupported_term_variant $untyped_lambda $untyped_let_binding $untyped_let_binding
)
  local.get $v
  call $hydra.show.errors.incorrect_unification_error
  br $end_checking_error
)
  local.get $v
  call $hydra.show.errors.not_a_forall_type_error
  br $end_checking_error
)
  local.get $v
  call $hydra.show.errors.not_a_function_type_error
  br $end_checking_error
)
  local.get $v
  call $hydra.show.errors.type_arity_mismatch_error
  br $end_checking_error
)
  local.get $v
  call $hydra.show.errors.type_mismatch_error
  br $end_checking_error
)
  local.get $v
  call $hydra.show.errors.unbound_type_variables_error
  br $end_checking_error
)
  local.get $v
  call $hydra.show.errors.unequal_types_error
  br $end_checking_error
)
  local.get $v
  call $hydra.show.errors.unsupported_term_variant_error
  br $end_checking_error
)
  local.get $v
  call $hydra.show.errors.untyped_lambda_error
  br $end_checking_error
)
  local.get $v
  call $hydra.show.errors.untyped_let_binding_error
  br $end_checking_error
)
)
  (func $hydra.show.errors.decoding_error (param $de i32) (result i32)
  i32.const 1192
  local.get $de
  drop
  i32.const 0
  drop
  i32.const 0
  call $hydra.lib.strings.cat2
)
  (func $hydra.show.errors.error (param $e i32) (result i32)
  (local $__rec_ptr i32)
  (local $v i32)
  (block $end_error (result i32)
  (block $unification
  (block $unexpected_type_variant
  (block $unexpected_term_variant
  (block $untyped_term_variable
  (block $undefined_term_variable
  (block $undefined_field
  (block $resolution
  (block $other
  (block $inference
  (block $extraction
  (block $duplicate_field
  (block $duplicate_binding
  (block $decoding
  (block $checking
  local.get $e
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $checking $decoding $duplicate_binding $duplicate_field $extraction $inference $other $resolution $undefined_field $undefined_term_variable $untyped_term_variable $unexpected_term_variant $unexpected_type_variant $unification $unification
)
  local.get $v
  call $hydra.show.errors.checking_error
  br $end_error
)
  local.get $v
  call $hydra.show.errors.decoding_error
  br $end_error
)
  local.get $v
  call $hydra.show.error.core.duplicate_binding_error
  br $end_error
)
  local.get $v
  call $hydra.show.error.core.duplicate_field_error
  br $end_error
)
  local.get $v
  drop
  i32.const 1212
  br $end_error
)
  local.get $v
  drop
  i32.const 1259
  br $end_error
)
  local.get $v
  call $hydra.show.errors.other_error
  br $end_error
)
  local.get $v
  drop
  i32.const 1326
  br $end_error
)
  local.get $v
  call $hydra.show.error.core.undefined_field_error
  br $end_error
)
  local.get $v
  call $hydra.show.error.core.undefined_term_variable_error
  br $end_error
)
  local.get $v
  call $hydra.show.error.core.untyped_term_variable_error
  br $end_error
)
  local.get $v
  call $hydra.show.error.core.unexpected_term_variant_error
  br $end_error
)
  local.get $v
  call $hydra.show.error.core.unexpected_type_variant_error
  br $end_error
)
  local.get $v
  call $hydra.show.errors.unification_error
  br $end_error
)
)
  (func $hydra.show.errors.incorrect_unification_error (param $e i32) (result i32)
  (local $subst i32)
  local.get $e
  i32.load
  local.set $subst
  i32.const 1232
  local.get $subst
  call $hydra.show.typing.type_subst
  call $hydra.lib.strings.cat2
)
  (func $hydra.show.errors.not_a_forall_type_error (param $e i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $args i32)
  (local $typ i32)
  local.get $e
  i32.load
  local.set $typ
  local.get $e
  i32.load offset=4
  local.set $args
  i32.const 1278
  local.get $typ
  call $hydra.show.core.type
  i32.const 1164
  local.get $args
  call $hydra.lib.lists.length
  call $hydra.lib.literals.show_int32
  i32.const 1108
  i32.const 0
  local.get $args
  call $hydra.formatting.show_list
  i32.const 28
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 6
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=24
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
  call $hydra.lib.strings.cat
)
  (func $hydra.show.errors.not_a_function_type_error (param $e i32) (result i32)
  (local $typ i32)
  local.get $e
  i32.load
  local.set $typ
  i32.const 1301
  local.get $typ
  call $hydra.show.core.type
  call $hydra.lib.strings.cat2
)
  (func $hydra.show.errors.other_error (param $oe i32) (result i32)
  local.get $oe
  drop
  i32.const 0
  drop
  i32.const 0
)
  (func $hydra.show.errors.type_arity_mismatch_error (param $e i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $actual i32)
  (local $args i32)
  (local $expected i32)
  (local $typ i32)
  local.get $e
  i32.load
  local.set $typ
  local.get $e
  i32.load offset=4
  local.set $expected
  local.get $e
  i32.load offset=8
  local.set $actual
  local.get $e
  i32.load offset=12
  local.set $args
  i32.const 1346
  local.get $typ
  call $hydra.show.core.type
  i32.const 1024
  local.get $expected
  call $hydra.lib.literals.show_int32
  i32.const 1154
  local.get $actual
  call $hydra.lib.literals.show_int32
  i32.const 1141
  i32.const 0
  local.get $args
  call $hydra.formatting.show_list
  i32.const 36
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 8
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=32
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=28
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=24
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
  call $hydra.lib.strings.cat
)
  (func $hydra.show.errors.type_mismatch_error (param $e i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $actual i32)
  (local $expected i32)
  local.get $e
  i32.load
  local.set $expected
  local.get $e
  i32.load offset=4
  local.set $actual
  i32.const 1355
  local.get $expected
  call $hydra.show.core.type
  i32.const 1085
  local.get $actual
  call $hydra.show.core.type
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
  call $hydra.lib.strings.cat
)
  (func $hydra.show.errors.unbound_type_variables_error (param $e i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $typ i32)
  (local $vars i32)
  local.get $e
  i32.load
  local.set $vars
  local.get $e
  i32.load offset=4
  local.set $typ
  i32.const 1383
  i32.const 1148
  i32.const 0
  local.get $vars
  call $hydra.lib.sets.to_list
  call $hydra.lib.lists.map
  call $hydra.lib.strings.intercalate
  i32.const 1539
  local.get $typ
  call $hydra.show.core.type
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
  call $hydra.lib.strings.cat
)
  (func $hydra.show.errors.unequal_types_error (param $e i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $desc i32)
  (local $types i32)
  local.get $e
  i32.load
  local.set $types
  local.get $e
  i32.load offset=4
  local.set $desc
  i32.const 1412
  i32.const 0
  local.get $types
  call $hydra.formatting.show_list
  i32.const 1100
  local.get $desc
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
  call $hydra.lib.strings.cat
)
  (func $hydra.show.errors.unification_error (param $e i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $lt i32)
  (local $msg i32)
  (local $rt i32)
  local.get $e
  i32.load
  local.set $lt
  local.get $e
  i32.load offset=4
  local.set $rt
  local.get $e
  i32.load offset=8
  local.set $msg
  i32.const 1430
  local.get $lt
  call $hydra.show.core.type
  i32.const 1131
  local.get $rt
  call $hydra.show.core.type
  i32.const 1186
  local.get $msg
  i32.const 28
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 6
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=24
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
  call $hydra.lib.strings.cat
)
  (func $hydra.show.errors.unsupported_term_variant_error (param $e i32) (result i32)
  i32.const 1466
  local.get $e
  i32.load
  call $hydra.show.variants.term_variant
  call $hydra.lib.strings.cat2
)
  (func $hydra.show.errors.untyped_lambda_error (param $_ i32) (result i32)
  i32.const 1496
)
  (func $hydra.show.errors.untyped_let_binding_error (param $e i32) (result i32)
  (local $b i32)
  local.get $e
  i32.load
  local.set $b
  i32.const 1514
  local.get $b
  call $hydra.show.core.binding
  call $hydra.lib.strings.cat2
)
)
