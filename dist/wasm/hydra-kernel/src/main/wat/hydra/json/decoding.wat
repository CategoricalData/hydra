(module
  (import "hydra.lib.eithers" "hydra.lib.eithers.bind" (func $hydra.lib.eithers.bind (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map" (func $hydra.lib.eithers.map (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map_list" (func $hydra.lib.eithers.map_list (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.lookup" (func $hydra.lib.maps.lookup (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.maybe" (func $hydra.lib.maybes.maybe (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat2" (func $hydra.lib.strings.cat2 (param i32) (param i32) (result i32) ) )
  (memory $memory 2 )
  (export "memory" (memory $memory) )
  (data (offset i32.const 1024 ) "\12\00\00\00\65\78\70\65\63\74\65\64\20\61\20\62\6f\6f\6c\65\61\6e\11\00\00\00\65\78\70\65\63\74\65\64\20\61\20\73\74\72\69\6e\67\11\00\00\00\65\78\70\65\63\74\65\64\20\61\6e\20\61\72\72\61\79\12\00\00\00\65\78\70\65\63\74\65\64\20\61\6e\20\6f\62\6a\65\63\74\0f\00\00\00\6d\69\73\73\69\6e\67\20\66\69\65\6c\64\3a\20")
  (global $__bump_ptr (mut i32) i32.const 1136 )
  (export "__bump_ptr" (global $__bump_ptr) )
  (func $__alloc (param $sz i32) (result i32)
  global.get $__bump_ptr
  global.get $__bump_ptr
  local.get $sz
  i32.add
  global.set $__bump_ptr
)
  (export "hydra.json.decoding.decode_array" (func $hydra.json.decoding.decode_array) )
  (export "hydra.json.decoding.decode_boolean" (func $hydra.json.decoding.decode_boolean) )
  (export "hydra.json.decoding.decode_field" (func $hydra.json.decoding.decode_field) )
  (export "hydra.json.decoding.decode_object" (func $hydra.json.decoding.decode_object) )
  (export "hydra.json.decoding.decode_optional_field" (func $hydra.json.decoding.decode_optional_field) )
  (export "hydra.json.decoding.decode_string" (func $hydra.json.decoding.decode_string) )
  (func $hydra.json.decoding.decode_array (param $decode_elem i32) (param $arg_0 i32) (result i32)
  (local $__rec_ptr i32)
  (local $a i32)
  (local $v i32)
  (block $end_value (result i32)
  (block $array
  local.get $decode_elem
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $array $array
)
  local.get $v
  drop
  local.get $decode_elem
  local.get $a
  call $hydra.lib.eithers.map_list
  br $end_value
)
)
  (func $hydra.json.decoding.decode_boolean (param $arg_0 i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $b i32)
  (local $v i32)
  (block $end_value (result i32)
  (block $boolean
  local.get $arg_0
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
  i32.const 1
  local.get $b
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
  br $end_value
)
)
  (func $hydra.json.decoding.decode_field (param $decode_value i32) (param $name i32) (param $m i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $f i32)
  local.get $decode_value
  local.get $name
  local.get $m
  call $hydra.json.decoding.decode_optional_field
  i32.const 0
  i32.const 1110
  local.get $name
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
  i32.const 1
  local.get $f
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
  call $hydra.lib.maybes.maybe
  call $hydra.lib.eithers.bind
)
  (func $hydra.json.decoding.decode_object (param $arg_0 i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $o i32)
  (local $v i32)
  (block $end_value (result i32)
  (block $object
  local.get $arg_0
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $object $object
)
  local.get $v
  drop
  i32.const 1
  local.get $o
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
  br $end_value
)
)
  (func $hydra.json.decoding.decode_optional_field (param $decode_value i32) (param $name i32) (param $m i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $v i32)
  (local $x i32)
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
  local.get $x
  local.get $v
  drop
  local.get $decode_value
  drop
  i32.const 0
  call $hydra.lib.eithers.map
  local.get $name
  local.get $m
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.maybe
)
  (func $hydra.json.decoding.decode_string (param $arg_0 i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $s i32)
  (local $v i32)
  (block $end_value (result i32)
  (block $string
  local.get $arg_0
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
  br $end_value
)
)
)
