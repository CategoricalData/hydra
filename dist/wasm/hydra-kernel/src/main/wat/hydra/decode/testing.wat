(module
  (import "hydra.extract.core" "hydra.extract.core.decode_list" (func $hydra.extract.core.decode_list (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.extract.core" "hydra.extract.core.decode_maybe" (func $hydra.extract.core.decode_maybe (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.extract.core" "hydra.extract.core.require_field" (func $hydra.extract.core.require_field (param i32) (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.extract.core" "hydra.extract.core.strip_with_decoding_error" (func $hydra.extract.core.strip_with_decoding_error (param i32) (param i32) (result i32) ) )
  (import "hydra.extract.core" "hydra.extract.core.to_field_map" (func $hydra.extract.core.to_field_map (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.bind" (func $hydra.lib.eithers.bind (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.either" (func $hydra.lib.eithers.either (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map" (func $hydra.lib.eithers.map (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.from_list" (func $hydra.lib.maps.from_list (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.lookup" (func $hydra.lib.maps.lookup (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.maybe" (func $hydra.lib.maybes.maybe (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat" (func $hydra.lib.strings.cat (param i32) (result i32) ) )
  (memory $memory 2 )
  (export "memory" (memory $memory) )
  (data (offset i32.const 1024 ) "\09\00\00\00\20\69\6e\20\75\6e\69\6f\6e\06\00\00\00\61\63\74\75\61\6c\04\00\00\00\63\61\73\65\05\00\00\00\63\61\73\65\73\0b\00\00\00\64\65\73\63\72\69\70\74\69\6f\6e\08\00\00\00\65\78\70\65\63\74\65\64\10\00\00\00\65\78\70\65\63\74\65\64\20\6c\69\74\65\72\61\6c\0f\00\00\00\65\78\70\65\63\74\65\64\20\72\65\63\6f\72\64\17\00\00\00\65\78\70\65\63\74\65\64\20\73\74\72\69\6e\67\20\6c\69\74\65\72\61\6c\0e\00\00\00\65\78\70\65\63\74\65\64\20\75\6e\69\6f\6e\15\00\00\00\65\78\70\65\63\74\65\64\20\77\72\61\70\70\65\64\20\74\79\70\65\04\00\00\00\6e\61\6d\65\0e\00\00\00\6e\6f\20\73\75\63\68\20\66\69\65\6c\64\20\09\00\00\00\73\75\62\67\72\6f\75\70\73\04\00\00\00\74\61\67\73\09\00\00\00\75\6e\69\76\65\72\73\61\6c")
  (global $__bump_ptr (mut i32) i32.const 1264 )
  (export "__bump_ptr" (global $__bump_ptr) )
  (func $__alloc (param $sz i32) (result i32)
  global.get $__bump_ptr
  global.get $__bump_ptr
  local.get $sz
  i32.add
  global.set $__bump_ptr
)
  (export "hydra.decode.testing.tag" (func $hydra.decode.testing.tag) )
  (export "hydra.decode.testing.test_case" (func $hydra.decode.testing.test_case) )
  (export "hydra.decode.testing.test_case_with_metadata" (func $hydra.decode.testing.test_case_with_metadata) )
  (export "hydra.decode.testing.test_group" (func $hydra.decode.testing.test_group) )
  (export "hydra.decode.testing.universal_test_case" (func $hydra.decode.testing.universal_test_case) )
  (func $hydra.decode.testing.tag (param $cx i32) (param $raw i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $b i32)
  (local $cx2 i32)
  (local $err i32)
  (local $raw2 i32)
  (local $s i32)
  (local $stripped i32)
  (local $stripped2 i32)
  (local $v i32)
  (local $wrapped_term i32)
  i32.const 0
  local.get $err
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
  (block $end_term (result i32)
  (block $wrap
  local.get $stripped
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
  local.get $b
  local.get $cx
  drop
  local.get $wrapped_term
  i32.load offset=4
  drop
  i32.const 0
  local.get $err
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
  (block $end_term (result i32)
  (block $literal
  local.get $stripped2
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
  (block $string
  local.get $v
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $string $string
)
  local.get $v
  drop
  i32.const 1
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
  br $end_literal
)
  br $end_term
)
  local.get $cx2
  local.get $raw2
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
  call $hydra.lib.eithers.map
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.testing.test_case (param $cx i32) (param $raw i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $err i32)
  (local $f i32)
  (local $field i32)
  (local $fname i32)
  (local $fterm i32)
  (local $inj i32)
  (local $input i32)
  (local $stripped i32)
  (local $t i32)
  (local $v i32)
  (local $variant_map i32)
  i32.const 0
  local.get $err
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
  (block $end_term (result i32)
  (block $inject
  local.get $stripped
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $inject $inject
)
  local.get $v
  drop
  local.get $inj
  i32.load offset=4
  local.set $field
  local.get $field
  i32.load
  local.set $fname
  local.get $field
  i32.load offset=4
  local.set $fterm
  i32.const 1247
  i32.const 0
  local.get $t
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
  local.get $cx
  local.get $input
  call $hydra.decode.testing.universal_test_case
  call $hydra.lib.eithers.map
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
  call $hydra.lib.maps.from_list
  local.set $variant_map
  i32.const 0
  i32.const 1208
  local.get $fname
  drop
  i32.const 0
  drop
  i32.const 0
  i32.const 1024
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
  local.get $fterm
  drop
  local.get $f
  drop
  i32.const 0
  local.get $fname
  local.get $variant_map
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.maybe
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.testing.test_case_with_metadata (param $cx i32) (param $raw i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $cx2 i32)
  (local $err i32)
  (local $field_case i32)
  (local $field_description i32)
  (local $field_map i32)
  (local $field_name i32)
  (local $field_tags i32)
  (local $raw2 i32)
  (local $record i32)
  (local $s i32)
  (local $stripped i32)
  (local $stripped2 i32)
  (local $v i32)
  i32.const 0
  local.get $err
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
  (block $end_term (result i32)
  (block $record
  local.get $stripped
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
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 1200
  i32.const 0
  local.get $err
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
  (block $end_term (result i32)
  (block $literal
  local.get $stripped2
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
  (block $string
  local.get $v
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $string $string
)
  local.get $v
  drop
  i32.const 1
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
  br $end_literal
)
  br $end_term
)
  local.get $cx2
  local.get $raw2
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1047
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1064
  i32.const 0
  local.get $err
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
  (block $end_term (result i32)
  (block $literal
  local.get $stripped2
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
  (block $string
  local.get $v
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $string $string
)
  local.get $v
  drop
  i32.const 1
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
  br $end_literal
)
  br $end_term
)
  local.get $cx2
  local.get $raw2
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
  i32.const 0
  i32.const 0
  call $hydra.extract.core.decode_maybe
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1239
  i32.const 0
  i32.const 0
  i32.const 0
  call $hydra.extract.core.decode_list
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_name
  local.get $field_case
  local.get $field_description
  local.get $field_tags
  i32.const 16
  call $__alloc
  local.set $__rec_ptr
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
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.testing.test_group (param $cx i32) (param $raw i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $cx2 i32)
  (local $err i32)
  (local $field_cases i32)
  (local $field_description i32)
  (local $field_map i32)
  (local $field_name i32)
  (local $field_subgroups i32)
  (local $raw2 i32)
  (local $record i32)
  (local $s i32)
  (local $stripped i32)
  (local $stripped2 i32)
  (local $v i32)
  i32.const 0
  local.get $err
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
  (block $end_term (result i32)
  (block $record
  local.get $stripped
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
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 1200
  i32.const 0
  local.get $err
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
  (block $end_term (result i32)
  (block $literal
  local.get $stripped2
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
  (block $string
  local.get $v
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $string $string
)
  local.get $v
  drop
  i32.const 1
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
  br $end_literal
)
  br $end_term
)
  local.get $cx2
  local.get $raw2
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1064
  i32.const 0
  local.get $err
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
  (block $end_term (result i32)
  (block $literal
  local.get $stripped2
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
  (block $string
  local.get $v
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $string $string
)
  local.get $v
  drop
  i32.const 1
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
  br $end_literal
)
  br $end_term
)
  local.get $cx2
  local.get $raw2
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
  i32.const 0
  i32.const 0
  call $hydra.extract.core.decode_maybe
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1226
  i32.const 0
  i32.const 0
  i32.const 0
  call $hydra.extract.core.decode_list
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1055
  i32.const 0
  i32.const 0
  i32.const 0
  call $hydra.extract.core.decode_list
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_name
  local.get $field_description
  local.get $field_subgroups
  local.get $field_cases
  i32.const 16
  call $__alloc
  local.set $__rec_ptr
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
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.testing.universal_test_case (param $cx i32) (param $raw i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $cx2 i32)
  (local $err i32)
  (local $field_actual i32)
  (local $field_expected i32)
  (local $field_map i32)
  (local $raw2 i32)
  (local $record i32)
  (local $s i32)
  (local $stripped i32)
  (local $stripped2 i32)
  (local $v i32)
  i32.const 0
  local.get $err
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
  (block $end_term (result i32)
  (block $record
  local.get $stripped
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
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 1037
  i32.const 0
  local.get $err
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
  (block $end_term (result i32)
  (block $literal
  local.get $stripped2
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
  (block $string
  local.get $v
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $string $string
)
  local.get $v
  drop
  i32.const 1
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
  br $end_literal
)
  br $end_term
)
  local.get $cx2
  local.get $raw2
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1079
  i32.const 0
  local.get $err
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
  (block $end_term (result i32)
  (block $literal
  local.get $stripped2
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
  (block $string
  local.get $v
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $string $string
)
  local.get $v
  drop
  i32.const 1
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
  br $end_literal
)
  br $end_term
)
  local.get $cx2
  local.get $raw2
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_actual
  local.get $field_expected
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
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
)
)
