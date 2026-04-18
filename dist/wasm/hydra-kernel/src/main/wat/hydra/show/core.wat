(module
  (import "hydra.lib.eithers" "hydra.lib.eithers.either" (func $hydra.lib.eithers.either (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.concat" (func $hydra.lib.lists.concat (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.cons" (func $hydra.lib.lists.cons (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.null" (func $hydra.lib.lists.null (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_bigfloat" (func $hydra.lib.literals.show_bigfloat (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_bigint" (func $hydra.lib.literals.show_bigint (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_decimal" (func $hydra.lib.literals.show_decimal (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_float32" (func $hydra.lib.literals.show_float32 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_float64" (func $hydra.lib.literals.show_float64 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_int16" (func $hydra.lib.literals.show_int16 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_int32" (func $hydra.lib.literals.show_int32 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_int64" (func $hydra.lib.literals.show_int64 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_int8" (func $hydra.lib.literals.show_int8 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_string" (func $hydra.lib.literals.show_string (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_uint16" (func $hydra.lib.literals.show_uint16 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_uint32" (func $hydra.lib.literals.show_uint32 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_uint64" (func $hydra.lib.literals.show_uint64 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_uint8" (func $hydra.lib.literals.show_uint8 (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.if_else" (func $hydra.lib.logic.if_else (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.to_list" (func $hydra.lib.maps.to_list (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.maybe" (func $hydra.lib.maybes.maybe (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.first" (func $hydra.lib.pairs.first (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.second" (func $hydra.lib.pairs.second (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.to_list" (func $hydra.lib.sets.to_list (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat" (func $hydra.lib.strings.cat (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat2" (func $hydra.lib.strings.cat2 (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.intercalate" (func $hydra.lib.strings.intercalate (param i32) (param i32) (result i32) ) )
  (memory $memory 2 )
  (export "memory" (memory $memory) )
  (data (offset i32.const 1024 ) "\00\00\00\00\01\00\00\00\20\03\00\00\00\20\3d\20\03\00\00\00\20\40\20\04\00\00\00\20\69\6e\20\03\00\00\00\20\92\20\01\00\00\00\28\02\00\00\00\28\00\01\00\00\00\29\05\00\00\00\29\20\3d\3e\20\02\00\00\00\29\7b\01\00\00\00\2c\02\00\00\00\2c\20\01\00\00\00\2e\02\00\00\00\2e\20\01\00\00\00\3a\02\00\00\00\3a\20\02\00\00\00\3a\28\09\00\00\00\3a\62\69\67\66\6c\6f\61\74\07\00\00\00\3a\62\69\67\69\6e\74\08\00\00\00\3a\66\6c\6f\61\74\33\32\08\00\00\00\3a\66\6c\6f\61\74\36\34\06\00\00\00\3a\69\6e\74\31\36\06\00\00\00\3a\69\6e\74\33\32\06\00\00\00\3a\69\6e\74\36\34\05\00\00\00\3a\69\6e\74\38\07\00\00\00\3a\75\69\6e\74\31\36\07\00\00\00\3a\75\69\6e\74\33\32\07\00\00\00\3a\75\69\6e\74\36\34\06\00\00\00\3a\75\69\6e\74\38\01\00\00\00\3d\01\00\00\00\3e\01\00\00\00\5b\08\00\00\00\5b\62\69\6e\61\72\79\5d\09\00\00\00\5b\64\65\66\61\75\6c\74\5d\01\00\00\00\5d\08\00\00\00\62\69\67\66\6c\6f\61\74\06\00\00\00\62\69\67\69\6e\74\06\00\00\00\62\69\6e\61\72\79\07\00\00\00\62\6f\6f\6c\65\61\6e\05\00\00\00\63\61\73\65\28\07\00\00\00\64\65\63\69\6d\61\6c\07\00\00\00\65\69\74\68\65\72\3c\05\00\00\00\66\61\6c\73\65\07\00\00\00\66\6c\6f\61\74\33\32\07\00\00\00\66\6c\6f\61\74\36\34\07\00\00\00\66\6f\72\61\6c\6c\20\07\00\00\00\69\6e\6a\65\63\74\28\05\00\00\00\69\6e\74\31\36\05\00\00\00\69\6e\74\33\32\05\00\00\00\69\6e\74\36\34\04\00\00\00\69\6e\74\38\05\00\00\00\6a\75\73\74\28\05\00\00\00\6c\65\66\74\28\04\00\00\00\6c\65\74\20\05\00\00\00\6c\69\73\74\3c\04\00\00\00\6d\61\70\3c\06\00\00\00\6d\61\79\62\65\3c\07\00\00\00\6e\6f\74\68\69\6e\67\08\00\00\00\70\72\6f\6a\65\63\74\28\06\00\00\00\72\65\63\6f\72\64\07\00\00\00\72\65\63\6f\72\64\28\06\00\00\00\72\69\67\68\74\28\04\00\00\00\73\65\74\3c\06\00\00\00\73\74\72\69\6e\67\04\00\00\00\74\72\75\65\06\00\00\00\75\69\6e\74\31\36\06\00\00\00\75\69\6e\74\33\32\06\00\00\00\75\69\6e\74\36\34\05\00\00\00\75\69\6e\74\38\05\00\00\00\75\6e\69\6f\6e\04\00\00\00\75\6e\69\74\07\00\00\00\75\6e\77\72\61\70\28\04\00\00\00\76\6f\69\64\05\00\00\00\77\72\61\70\28\01\00\00\00\7b\01\00\00\00\7d\01\00\00\00\9b\01\00\00\00\bb\01\00\00\00\e8\01\00\00\00\e9")
  (global $__bump_ptr (mut i32) i32.const 1728 )
  (export "__bump_ptr" (global $__bump_ptr) )
  (func $__alloc (param $sz i32) (result i32)
  global.get $__bump_ptr
  global.get $__bump_ptr
  local.get $sz
  i32.add
  global.set $__bump_ptr
)
  (export "hydra.show.core.binding" (func $hydra.show.core.binding) )
  (export "hydra.show.core.case_statement" (func $hydra.show.core.case_statement) )
  (export "hydra.show.core.either" (func $hydra.show.core.either) )
  (export "hydra.show.core.field" (func $hydra.show.core.field) )
  (export "hydra.show.core.field_type" (func $hydra.show.core.field_type) )
  (export "hydra.show.core.fields" (func $hydra.show.core.fields) )
  (export "hydra.show.core.float" (func $hydra.show.core.float) )
  (export "hydra.show.core.float_type" (func $hydra.show.core.float_type) )
  (export "hydra.show.core.injection" (func $hydra.show.core.injection) )
  (export "hydra.show.core.integer" (func $hydra.show.core.integer) )
  (export "hydra.show.core.integer_type" (func $hydra.show.core.integer_type) )
  (export "hydra.show.core.lambda" (func $hydra.show.core.lambda) )
  (export "hydra.show.core.let" (func $hydra.show.core.let) )
  (export "hydra.show.core.list" (func $hydra.show.core.list) )
  (export "hydra.show.core.literal" (func $hydra.show.core.literal) )
  (export "hydra.show.core.literal_type" (func $hydra.show.core.literal_type) )
  (export "hydra.show.core.map" (func $hydra.show.core.map) )
  (export "hydra.show.core.maybe" (func $hydra.show.core.maybe) )
  (export "hydra.show.core.pair" (func $hydra.show.core.pair) )
  (export "hydra.show.core.projection" (func $hydra.show.core.projection) )
  (export "hydra.show.core.read_term" (func $hydra.show.core.read_term) )
  (export "hydra.show.core.set" (func $hydra.show.core.set) )
  (export "hydra.show.core.term" (func $hydra.show.core.term) )
  (export "hydra.show.core.type" (func $hydra.show.core.type) )
  (export "hydra.show.core.type_scheme" (func $hydra.show.core.type_scheme) )
  (func $hydra.show.core.binding (param $el i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $name i32)
  (local $t i32)
  (local $ts i32)
  (local $type_str i32)
  local.get $el
  i32.load
  drop
  i32.const 0
  drop
  i32.const 0
  local.set $name
  local.get $el
  i32.load offset=4
  local.set $t
  i32.const 1024
  i32.const 1126
  local.get $ts
  call $hydra.show.core.type_scheme
  i32.const 1073
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
  call $hydra.lib.strings.cat
  local.get $el
  i32.load offset=8
  call $hydra.lib.maybes.maybe
  local.set $type_str
  local.get $name
  local.get $type_str
  i32.const 1033
  local.get $t
  call $hydra.show.core.term
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
  (func $hydra.show.core.case_statement (param $cs i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $all_fields i32)
  (local $cs_cases i32)
  (local $d i32)
  (local $default_field i32)
  (local $mdef i32)
  (local $tname i32)
  local.get $cs
  i32.load
  drop
  i32.const 0
  drop
  i32.const 0
  local.set $tname
  local.get $cs
  i32.load offset=4
  local.set $mdef
  local.get $cs
  i32.load offset=8
  local.set $cs_cases
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  i32.const 1289
  local.get $d
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
  local.get $__rec_ptr
  i32.const 1
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.get $__rec_ptr
  local.get $mdef
  call $hydra.lib.maybes.maybe
  local.set $default_field
  local.get $cs_cases
  local.get $default_field
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
  call $hydra.lib.lists.concat
  local.set $all_fields
  i32.const 1350
  local.get $tname
  i32.const 1073
  local.get $all_fields
  call $hydra.show.core.fields
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
  (func $hydra.show.core.either (param $show_a i32) (param $show_b i32) (param $e i32) (result i32)
  (local $a i32)
  (local $b i32)
  i32.const 1478
  local.get $a
  drop
  local.get $show_a
  drop
  i32.const 0
  i32.const 1073
  call $hydra.lib.strings.cat2
  call $hydra.lib.strings.cat2
  i32.const 1566
  local.get $b
  drop
  local.get $show_b
  drop
  i32.const 0
  i32.const 1073
  call $hydra.lib.strings.cat2
  call $hydra.lib.strings.cat2
  local.get $e
  call $hydra.lib.eithers.either
)
  (func $hydra.show.core.field (param $field i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $fname i32)
  (local $fterm i32)
  local.get $field
  i32.load
  drop
  i32.const 0
  drop
  i32.const 0
  local.set $fname
  local.get $field
  i32.load offset=4
  local.set $fterm
  local.get $fname
  i32.const 1262
  local.get $fterm
  call $hydra.show.core.term
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
  call $hydra.lib.strings.cat
)
  (func $hydra.show.core.field_type (param $ft i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $fname i32)
  (local $ftyp i32)
  local.get $ft
  i32.load
  drop
  i32.const 0
  drop
  i32.const 0
  local.set $fname
  local.get $ft
  i32.load offset=4
  local.set $ftyp
  local.get $fname
  i32.const 1115
  local.get $ftyp
  call $hydra.show.core.type
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
  call $hydra.lib.strings.cat
)
  (func $hydra.show.core.fields (param $flds i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $field_strs i32)
  i32.const 0
  local.get $flds
  call $hydra.lib.lists.map
  local.set $field_strs
  i32.const 1686
  i32.const 1098
  local.get $field_strs
  call $hydra.lib.strings.intercalate
  i32.const 1691
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
  call $hydra.lib.strings.cat
)
  (func $hydra.show.core.float (param $fv i32) (result i32)
  (local $__rec_ptr i32)
  (local $v i32)
  (block $end_float_value (result i32)
  (block $float64
  (block $float32
  (block $bigfloat
  local.get $fv
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $bigfloat $float32 $float64 $float64
)
  local.get $v
  drop
  local.get $v
  call $hydra.lib.literals.show_bigfloat
  i32.const 1132
  call $hydra.lib.strings.cat2
  br $end_float_value
)
  local.get $v
  drop
  local.get $v
  call $hydra.lib.literals.show_float32
  i32.const 1156
  call $hydra.lib.strings.cat2
  br $end_float_value
)
  local.get $v
  drop
  local.get $v
  call $hydra.lib.literals.show_float64
  i32.const 1168
  call $hydra.lib.strings.cat2
  br $end_float_value
)
)
  (func $hydra.show.core.float_type (param $ft i32) (result i32)
  (local $__rec_ptr i32)
  (local $v i32)
  (block $end_float_type (result i32)
  (block $float64
  (block $float32
  (block $bigfloat
  local.get $ft
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $bigfloat $float32 $float64 $float64
)
  local.get $v
  drop
  i32.const 1307
  br $end_float_type
)
  local.get $v
  drop
  i32.const 1390
  br $end_float_type
)
  local.get $v
  drop
  i32.const 1401
  br $end_float_type
)
)
  (func $hydra.show.core.injection (param $inj i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $f i32)
  (local $tname i32)
  local.get $inj
  i32.load
  local.set $tname
  local.get $inj
  i32.load offset=4
  local.set $f
  i32.const 1423
  local.get $tname
  drop
  i32.const 0
  drop
  i32.const 0
  i32.const 1073
  local.get $f
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
  call $hydra.show.core.fields
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
  (func $hydra.show.core.integer (param $iv i32) (result i32)
  (local $__rec_ptr i32)
  (local $v i32)
  (block $end_integer_value (result i32)
  (block $uint64
  (block $uint32
  (block $uint16
  (block $uint8
  (block $int64
  (block $int32
  (block $int16
  (block $int8
  (block $bigint
  local.get $iv
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $bigint $int8 $int16 $int32 $int64 $uint8 $uint16 $uint32 $uint64 $uint64
)
  local.get $v
  drop
  local.get $v
  call $hydra.lib.literals.show_bigint
  i32.const 1145
  call $hydra.lib.strings.cat2
  br $end_integer_value
)
  local.get $v
  drop
  local.get $v
  call $hydra.lib.literals.show_int8
  i32.const 1210
  call $hydra.lib.strings.cat2
  br $end_integer_value
)
  local.get $v
  drop
  local.get $v
  call $hydra.lib.literals.show_int16
  i32.const 1180
  call $hydra.lib.strings.cat2
  br $end_integer_value
)
  local.get $v
  drop
  local.get $v
  call $hydra.lib.literals.show_int32
  i32.const 1190
  call $hydra.lib.strings.cat2
  br $end_integer_value
)
  local.get $v
  drop
  local.get $v
  call $hydra.lib.literals.show_int64
  i32.const 1200
  call $hydra.lib.strings.cat2
  br $end_integer_value
)
  local.get $v
  drop
  local.get $v
  call $hydra.lib.literals.show_uint8
  i32.const 1252
  call $hydra.lib.strings.cat2
  br $end_integer_value
)
  local.get $v
  drop
  local.get $v
  call $hydra.lib.literals.show_uint16
  i32.const 1219
  call $hydra.lib.strings.cat2
  br $end_integer_value
)
  local.get $v
  drop
  local.get $v
  call $hydra.lib.literals.show_uint32
  i32.const 1230
  call $hydra.lib.strings.cat2
  br $end_integer_value
)
  local.get $v
  drop
  local.get $v
  call $hydra.lib.literals.show_uint64
  i32.const 1241
  call $hydra.lib.strings.cat2
  br $end_integer_value
)
)
  (func $hydra.show.core.integer_type (param $it i32) (result i32)
  (local $__rec_ptr i32)
  (local $v i32)
  (block $end_integer_type (result i32)
  (block $uint64
  (block $uint32
  (block $uint16
  (block $uint8
  (block $int64
  (block $int32
  (block $int16
  (block $int8
  (block $bigint
  local.get $it
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $bigint $int8 $int16 $int32 $int64 $uint8 $uint16 $uint32 $uint64 $uint64
)
  local.get $v
  drop
  i32.const 1319
  br $end_integer_type
)
  local.get $v
  drop
  i32.const 1461
  br $end_integer_type
)
  local.get $v
  drop
  i32.const 1434
  br $end_integer_type
)
  local.get $v
  drop
  i32.const 1443
  br $end_integer_type
)
  local.get $v
  drop
  i32.const 1452
  br $end_integer_type
)
  local.get $v
  drop
  i32.const 1632
  br $end_integer_type
)
  local.get $v
  drop
  i32.const 1602
  br $end_integer_type
)
  local.get $v
  drop
  i32.const 1612
  br $end_integer_type
)
  local.get $v
  drop
  i32.const 1622
  br $end_integer_type
)
)
  (func $hydra.show.core.lambda (param $l i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $body i32)
  (local $mt i32)
  (local $t i32)
  (local $type_str i32)
  (local $v i32)
  local.get $l
  i32.load
  drop
  i32.const 0
  drop
  i32.const 0
  local.set $v
  local.get $l
  i32.load offset=4
  local.set $mt
  local.get $l
  i32.load offset=8
  local.set $body
  i32.const 1024
  i32.const 1115
  local.get $t
  call $hydra.show.core.type
  call $hydra.lib.strings.cat2
  local.get $mt
  call $hydra.lib.maybes.maybe
  local.set $type_str
  i32.const 1701
  local.get $v
  local.get $type_str
  i32.const 1104
  local.get $body
  call $hydra.show.core.term
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
  call $hydra.lib.strings.cat
)
  (func $hydra.show.core.let (param $l i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $binding_strs i32)
  (local $bindings i32)
  (local $env i32)
  local.get $l
  i32.load
  local.set $bindings
  local.get $l
  i32.load offset=4
  local.set $env
  i32.const 0
  local.get $bindings
  call $hydra.lib.lists.map
  local.set $binding_strs
  i32.const 1487
  i32.const 1098
  local.get $binding_strs
  call $hydra.lib.strings.intercalate
  i32.const 1047
  local.get $env
  call $hydra.show.core.term
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
  (func $hydra.show.core.list (param $f i32) (param $xs i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $element_strs i32)
  local.get $f
  local.get $xs
  call $hydra.lib.lists.map
  local.set $element_strs
  i32.const 1272
  i32.const 1098
  local.get $element_strs
  call $hydra.lib.strings.intercalate
  i32.const 1302
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
  call $hydra.lib.strings.cat
)
  (func $hydra.show.core.literal (param $l i32) (result i32)
  (local $__rec_ptr i32)
  (local $b i32)
  (local $d i32)
  (local $fv i32)
  (local $iv i32)
  (local $s i32)
  (local $v i32)
  (block $end_literal (result i32)
  (block $string
  (block $integer
  (block $float
  (block $decimal
  (block $boolean
  (block $binary
  local.get $l
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $binary $boolean $decimal $float $integer $string $string
)
  local.get $v
  drop
  i32.const 1277
  br $end_literal
)
  local.get $v
  drop
  local.get $b
  i32.const 1594
  i32.const 1381
  call $hydra.lib.logic.if_else
  br $end_literal
)
  local.get $v
  drop
  local.get $d
  call $hydra.lib.literals.show_decimal
  br $end_literal
)
  local.get $v
  drop
  local.get $fv
  call $hydra.show.core.float
  br $end_literal
)
  local.get $v
  drop
  local.get $iv
  call $hydra.show.core.integer
  br $end_literal
)
  local.get $v
  drop
  local.get $s
  call $hydra.lib.literals.show_string
  br $end_literal
)
)
  (func $hydra.show.core.literal_type (param $lt i32) (result i32)
  (local $__rec_ptr i32)
  (local $ft i32)
  (local $it i32)
  (local $v i32)
  (block $end_literal_type (result i32)
  (block $string
  (block $integer
  (block $float
  (block $decimal
  (block $boolean
  (block $binary
  local.get $lt
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $binary $boolean $decimal $float $integer $string $string
)
  local.get $v
  drop
  i32.const 1329
  br $end_literal_type
)
  local.get $v
  drop
  i32.const 1339
  br $end_literal_type
)
  local.get $v
  drop
  i32.const 1359
  br $end_literal_type
)
  local.get $v
  drop
  local.get $ft
  call $hydra.show.core.float_type
  br $end_literal_type
)
  local.get $v
  drop
  local.get $it
  call $hydra.show.core.integer_type
  br $end_literal_type
)
  local.get $v
  drop
  i32.const 1584
  br $end_literal_type
)
)
  (func $hydra.show.core.map (param $show_k i32) (param $show_v i32) (param $m i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $p i32)
  (local $pair_strs i32)
  local.get $p
  call $hydra.lib.pairs.first
  drop
  local.get $show_k
  drop
  i32.const 0
  i32.const 1120
  local.get $p
  call $hydra.lib.pairs.second
  drop
  local.get $show_v
  drop
  i32.const 0
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
  call $hydra.lib.strings.cat
  local.get $m
  call $hydra.lib.maps.to_list
  call $hydra.lib.lists.map
  local.set $pair_strs
  i32.const 1686
  i32.const 1098
  local.get $pair_strs
  call $hydra.lib.strings.intercalate
  i32.const 1691
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
  call $hydra.lib.strings.cat
)
  (func $hydra.show.core.maybe (param $f i32) (param $mx i32) (result i32)
  (local $x i32)
  i32.const 1522
  i32.const 1469
  local.get $x
  drop
  local.get $f
  drop
  i32.const 0
  i32.const 1073
  call $hydra.lib.strings.cat2
  call $hydra.lib.strings.cat2
  local.get $mx
  call $hydra.lib.maybes.maybe
)
  (func $hydra.show.core.pair (param $show_a i32) (param $show_b i32) (param $p i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 1062
  local.get $p
  call $hydra.lib.pairs.first
  drop
  local.get $show_a
  drop
  i32.const 0
  i32.const 1098
  local.get $p
  call $hydra.lib.pairs.second
  drop
  local.get $show_b
  drop
  i32.const 0
  i32.const 1073
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
  call $hydra.lib.strings.cat
)
  (func $hydra.show.core.projection (param $proj i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $fname i32)
  (local $tname i32)
  local.get $proj
  i32.load
  drop
  i32.const 0
  drop
  i32.const 0
  local.set $tname
  local.get $proj
  i32.load offset=4
  drop
  i32.const 0
  drop
  i32.const 0
  local.set $fname
  i32.const 1533
  local.get $tname
  i32.const 1087
  local.get $fname
  i32.const 1691
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
  call $hydra.lib.strings.cat
)
  (func $hydra.show.core.read_term (param $s i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 8
  i32.const 5
  local.get $s
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
)
  (func $hydra.show.core.set (param $f i32) (param $xs i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $element_strs i32)
  local.get $f
  local.get $xs
  call $hydra.lib.sets.to_list
  call $hydra.lib.lists.map
  local.set $element_strs
  i32.const 1686
  i32.const 1098
  local.get $element_strs
  call $hydra.lib.strings.intercalate
  i32.const 1691
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
  call $hydra.lib.strings.cat
)
  (func $hydra.show.core.term (param $t i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $app i32)
  (local $app2 i32)
  (local $at i32)
  (local $body i32)
  (local $e i32)
  (local $els i32)
  (local $entry i32)
  (local $flds i32)
  (local $gather_terms i32)
  (local $l i32)
  (local $lhs i32)
  (local $lit i32)
  (local $m i32)
  (local $mt i32)
  (local $name i32)
  (local $p i32)
  (local $param i32)
  (local $prev i32)
  (local $r i32)
  (local $rec i32)
  (local $rhs i32)
  (local $s i32)
  (local $t2 i32)
  (local $ta i32)
  (local $term1 i32)
  (local $term_strs i32)
  (local $terms i32)
  (local $tname i32)
  (local $tt i32)
  (local $typ i32)
  (local $v i32)
  (local $wt i32)
  local.get $app
  i32.load
  local.set $lhs
  local.get $app
  i32.load offset=4
  local.set $rhs
  (block $end_term (result i32)
  (block $application
  local.get $lhs
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
  local.get $rhs
  local.get $prev
  call $hydra.lib.lists.cons
  drop
  local.get $app2
  drop
  local.get $gather_terms
  drop
  i32.const 0
  br $end_term
)
  local.set $gather_terms
  (block $end_term (result i32)
  (block $wrap
  (block $variable
  (block $unwrap
  (block $unit
  (block $inject
  (block $type_application
  (block $type_lambda
  (block $set
  (block $record
  (block $project
  (block $pair
  (block $maybe
  (block $map
  (block $literal
  (block $list
  (block $let
  (block $lambda
  (block $either
  (block $cases
  (block $application
  (block $annotated
  local.get $t
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $annotated $application $cases $either $lambda $let $list $literal $map $maybe $pair $project $record $set $type_lambda $type_application $inject $unit $unwrap $variable $wrap $wrap
)
  local.get $v
  drop
  local.get $at
  i32.load
  call $hydra.show.core.term
  br $end_term
)
  local.get $v
  drop
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  drop
  local.get $app
  drop
  local.get $gather_terms
  drop
  i32.const 0
  local.set $terms
  i32.const 0
  local.get $terms
  call $hydra.lib.lists.map
  local.set $term_strs
  i32.const 1062
  i32.const 1040
  local.get $term_strs
  call $hydra.lib.strings.intercalate
  i32.const 1073
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
  call $hydra.lib.strings.cat
  br $end_term
)
  local.get $v
  call $hydra.show.core.case_statement
  br $end_term
)
  local.get $v
  drop
  i32.const 1478
  local.get $l
  call $hydra.show.core.term
  i32.const 1073
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
  call $hydra.lib.strings.cat
  i32.const 1566
  local.get $r
  call $hydra.show.core.term
  i32.const 1073
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
  call $hydra.lib.strings.cat
  local.get $e
  call $hydra.lib.eithers.either
  br $end_term
)
  local.get $v
  call $hydra.show.core.lambda
  br $end_term
)
  local.get $v
  drop
  local.get $l
  call $hydra.show.core.let
  br $end_term
)
  local.get $v
  drop
  i32.const 0
  local.get $els
  call $hydra.lib.lists.map
  local.set $term_strs
  i32.const 1272
  i32.const 1098
  local.get $term_strs
  call $hydra.lib.strings.intercalate
  i32.const 1302
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
  call $hydra.lib.strings.cat
  br $end_term
)
  local.get $v
  drop
  local.get $lit
  call $hydra.show.core.literal
  br $end_term
)
  local.get $v
  drop
  local.get $p
  call $hydra.lib.pairs.first
  call $hydra.show.core.term
  i32.const 1262
  local.get $p
  call $hydra.lib.pairs.second
  call $hydra.show.core.term
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
  call $hydra.lib.strings.cat
  local.set $entry
  i32.const 1686
  i32.const 1098
  local.get $entry
  local.get $m
  call $hydra.lib.maps.to_list
  call $hydra.lib.lists.map
  call $hydra.lib.strings.intercalate
  i32.const 1691
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
  call $hydra.lib.strings.cat
  br $end_term
)
  local.get $v
  drop
  i32.const 1522
  i32.const 1469
  local.get $t2
  call $hydra.show.core.term
  i32.const 1073
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
  call $hydra.lib.strings.cat
  local.get $mt
  call $hydra.lib.maybes.maybe
  br $end_term
)
  local.get $v
  drop
  i32.const 1062
  local.get $p
  call $hydra.lib.pairs.first
  call $hydra.show.core.term
  i32.const 1098
  local.get $p
  call $hydra.lib.pairs.second
  call $hydra.show.core.term
  i32.const 1073
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
  call $hydra.lib.strings.cat
  br $end_term
)
  local.get $v
  call $hydra.show.core.projection
  br $end_term
)
  local.get $v
  drop
  local.get $rec
  i32.load
  drop
  i32.const 0
  drop
  i32.const 0
  local.set $tname
  local.get $rec
  i32.load offset=4
  local.set $flds
  i32.const 1555
  local.get $tname
  i32.const 1073
  local.get $flds
  call $hydra.show.core.fields
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
  br $end_term
)
  local.get $v
  drop
  i32.const 1686
  i32.const 1098
  i32.const 0
  local.get $s
  call $hydra.lib.sets.to_list
  call $hydra.lib.lists.map
  call $hydra.lib.strings.intercalate
  i32.const 1691
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
  call $hydra.lib.strings.cat
  br $end_term
)
  local.get $v
  drop
  local.get $ta
  i32.load
  drop
  i32.const 0
  drop
  i32.const 0
  local.set $param
  local.get $ta
  i32.load offset=4
  local.set $body
  i32.const 1696
  local.get $param
  i32.const 1104
  local.get $body
  call $hydra.show.core.term
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
  br $end_term
)
  local.get $v
  drop
  local.get $tt
  i32.load
  local.set $t2
  local.get $tt
  i32.load offset=4
  local.set $typ
  local.get $t2
  call $hydra.show.core.term
  i32.const 1706
  local.get $typ
  call $hydra.show.core.type
  i32.const 1711
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
  br $end_term
)
  local.get $v
  call $hydra.show.core.injection
  br $end_term
)
  local.get $v
  drop
  i32.const 1650
  br $end_term
)
  local.get $v
  drop
  i32.const 1658
  local.get $tname
  drop
  i32.const 0
  drop
  i32.const 0
  i32.const 1073
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
  call $hydra.lib.strings.cat
  br $end_term
)
  local.get $v
  drop
  local.get $name
  drop
  i32.const 0
  drop
  i32.const 0
  br $end_term
)
  local.get $v
  drop
  local.get $wt
  i32.load
  drop
  i32.const 0
  drop
  i32.const 0
  local.set $tname
  local.get $wt
  i32.load offset=4
  local.set $term1
  i32.const 1677
  local.get $tname
  i32.const 1087
  local.get $term1
  call $hydra.show.core.term
  i32.const 1691
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
  call $hydra.lib.strings.cat
  br $end_term
)
)
  (func $hydra.show.core.type (param $typ i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $app i32)
  (local $app2 i32)
  (local $at i32)
  (local $body i32)
  (local $cod i32)
  (local $dom i32)
  (local $et i32)
  (local $etyp i32)
  (local $field_strs i32)
  (local $first_typ i32)
  (local $flds i32)
  (local $ft i32)
  (local $gather_function_types i32)
  (local $gather_types i32)
  (local $key_typ i32)
  (local $left_typ i32)
  (local $lhs i32)
  (local $lt i32)
  (local $mt i32)
  (local $name i32)
  (local $prev i32)
  (local $pt i32)
  (local $rhs i32)
  (local $right_typ i32)
  (local $rt i32)
  (local $second_typ i32)
  (local $show_row_type i32)
  (local $t i32)
  (local $type_strs i32)
  (local $types i32)
  (local $v i32)
  (local $val_typ i32)
  (local $var i32)
  (local $wt i32)
  i32.const 0
  local.get $flds
  call $hydra.lib.lists.map
  local.set $field_strs
  i32.const 1686
  i32.const 1098
  local.get $field_strs
  call $hydra.lib.strings.intercalate
  i32.const 1691
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
  call $hydra.lib.strings.cat
  local.set $show_row_type
  local.get $app
  i32.load
  local.set $lhs
  local.get $app
  i32.load offset=4
  local.set $rhs
  (block $end_type (result i32)
  (block $application
  local.get $lhs
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
  local.get $rhs
  local.get $prev
  call $hydra.lib.lists.cons
  drop
  local.get $app2
  drop
  local.get $gather_types
  drop
  i32.const 0
  br $end_type
)
  local.set $gather_types
  (block $end_type (result i32)
  (block $function
  local.get $t
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
  local.get $ft
  i32.load
  local.set $dom
  local.get $ft
  i32.load offset=4
  local.set $cod
  local.get $dom
  local.get $prev
  call $hydra.lib.lists.cons
  drop
  local.get $cod
  drop
  local.get $gather_function_types
  drop
  i32.const 0
  br $end_type
)
  local.set $gather_function_types
  (block $end_type (result i32)
  (block $wrap
  (block $void
  (block $variable
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
  (block $forall
  (block $either
  (block $application
  (block $annotated
  local.get $typ
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $annotated $application $either $forall $function $list $literal $map $maybe $pair $record $set $union $unit $variable $void $wrap $wrap
)
  local.get $v
  drop
  local.get $at
  i32.load
  call $hydra.show.core.type
  br $end_type
)
  local.get $v
  drop
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  drop
  local.get $app
  drop
  local.get $gather_types
  drop
  i32.const 0
  local.set $types
  i32.const 0
  local.get $types
  call $hydra.lib.lists.map
  local.set $type_strs
  i32.const 1062
  i32.const 1040
  local.get $type_strs
  call $hydra.lib.strings.intercalate
  i32.const 1073
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
  call $hydra.lib.strings.cat
  br $end_type
)
  local.get $v
  drop
  local.get $et
  i32.load
  local.set $left_typ
  local.get $et
  i32.load offset=4
  local.set $right_typ
  i32.const 1370
  local.get $left_typ
  call $hydra.show.core.type
  i32.const 1098
  local.get $right_typ
  call $hydra.show.core.type
  i32.const 1267
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
  call $hydra.lib.strings.cat
  br $end_type
)
  local.get $v
  drop
  local.get $ft
  i32.load
  drop
  i32.const 0
  drop
  i32.const 0
  local.set $var
  local.get $ft
  i32.load offset=4
  local.set $body
  i32.const 1067
  local.get $var
  i32.const 1104
  local.get $body
  call $hydra.show.core.type
  i32.const 1073
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
  call $hydra.lib.strings.cat
  br $end_type
)
  local.get $v
  drop
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  drop
  local.get $typ
  drop
  local.get $gather_function_types
  drop
  i32.const 0
  local.set $types
  i32.const 0
  local.get $types
  call $hydra.lib.lists.map
  local.set $type_strs
  i32.const 1062
  i32.const 1055
  local.get $type_strs
  call $hydra.lib.strings.intercalate
  i32.const 1073
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
  call $hydra.lib.strings.cat
  br $end_type
)
  local.get $v
  drop
  i32.const 1495
  local.get $etyp
  call $hydra.show.core.type
  i32.const 1267
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
  call $hydra.lib.strings.cat
  br $end_type
)
  local.get $v
  drop
  local.get $lt
  call $hydra.show.core.literal_type
  br $end_type
)
  local.get $v
  drop
  local.get $mt
  i32.load
  local.set $key_typ
  local.get $mt
  i32.load offset=4
  local.set $val_typ
  i32.const 1504
  local.get $key_typ
  call $hydra.show.core.type
  i32.const 1098
  local.get $val_typ
  call $hydra.show.core.type
  i32.const 1267
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
  call $hydra.lib.strings.cat
  br $end_type
)
  local.get $v
  drop
  i32.const 1512
  local.get $etyp
  call $hydra.show.core.type
  i32.const 1267
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
  call $hydra.lib.strings.cat
  br $end_type
)
  local.get $v
  drop
  local.get $pt
  i32.load
  local.set $first_typ
  local.get $pt
  i32.load offset=4
  local.set $second_typ
  i32.const 1062
  local.get $first_typ
  call $hydra.show.core.type
  i32.const 1098
  local.get $second_typ
  call $hydra.show.core.type
  i32.const 1073
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
  call $hydra.lib.strings.cat
  br $end_type
)
  local.get $v
  drop
  i32.const 1545
  local.get $rt
  drop
  local.get $show_row_type
  drop
  i32.const 0
  call $hydra.lib.strings.cat2
  br $end_type
)
  local.get $v
  drop
  i32.const 1576
  local.get $etyp
  call $hydra.show.core.type
  i32.const 1267
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
  call $hydra.lib.strings.cat
  br $end_type
)
  local.get $v
  drop
  i32.const 1641
  local.get $rt
  drop
  local.get $show_row_type
  drop
  i32.const 0
  call $hydra.lib.strings.cat2
  br $end_type
)
  local.get $v
  drop
  i32.const 1650
  br $end_type
)
  local.get $v
  drop
  local.get $name
  drop
  i32.const 0
  drop
  i32.const 0
  br $end_type
)
  local.get $v
  drop
  i32.const 1669
  br $end_type
)
  local.get $v
  drop
  i32.const 1677
  local.get $wt
  call $hydra.show.core.type
  i32.const 1073
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
  call $hydra.lib.strings.cat
  br $end_type
)
)
  (func $hydra.show.core.type_scheme (param $ts i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $body i32)
  (local $c i32)
  (local $fa i32)
  (local $m i32)
  (local $p i32)
  (local $tc i32)
  (local $to_constraint_pair i32)
  (local $to_constraint_pairs i32)
  (local $v i32)
  (local $var_names i32)
  (local $vars i32)
  local.get $ts
  i32.load
  local.set $vars
  local.get $ts
  i32.load offset=4
  local.set $body
  i32.const 0
  local.get $vars
  call $hydra.lib.lists.map
  local.set $var_names
  local.get $vars
  call $hydra.lib.lists.null
  i32.const 1024
  i32.const 1412
  i32.const 1093
  local.get $var_names
  call $hydra.lib.strings.intercalate
  i32.const 1109
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
  call $hydra.lib.strings.cat
  call $hydra.lib.logic.if_else
  local.set $fa
  local.get $c
  drop
  i32.const 0
  drop
  i32.const 0
  i32.const 1028
  local.get $v
  drop
  i32.const 0
  drop
  i32.const 0
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
  call $hydra.lib.strings.cat
  local.set $to_constraint_pair
  local.get $p
  call $hydra.lib.pairs.first
  drop
  local.get $to_constraint_pair
  drop
  i32.const 0
  local.get $p
  call $hydra.lib.pairs.second
  i32.load
  call $hydra.lib.sets.to_list
  call $hydra.lib.lists.map
  local.set $to_constraint_pairs
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  local.get $to_constraint_pairs
  local.get $m
  call $hydra.lib.maps.to_list
  call $hydra.lib.lists.map
  call $hydra.lib.lists.concat
  local.get $ts
  i32.load offset=8
  call $hydra.lib.maybes.maybe
  local.set $tc
  i32.const 1062
  local.get $fa
  local.get $tc
  call $hydra.lib.lists.null
  i32.const 1024
  i32.const 1062
  i32.const 1098
  local.get $tc
  call $hydra.lib.strings.intercalate
  i32.const 1078
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
  call $hydra.lib.strings.cat
  call $hydra.lib.logic.if_else
  local.get $body
  call $hydra.show.core.type
  i32.const 1073
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
  call $hydra.lib.strings.cat
)
)
