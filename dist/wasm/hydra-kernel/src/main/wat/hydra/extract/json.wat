(module
  (import "hydra.lib.eithers" "hydra.lib.eithers.bind" (func $hydra.lib.eithers.bind (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map" (func $hydra.lib.eithers.map (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.lookup" (func $hydra.lib.maps.lookup (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.maybe" (func $hydra.lib.maybes.maybe (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat" (func $hydra.lib.strings.cat (param i32) (result i32) ) )
  (memory $memory 2 )
  (export "memory" (memory $memory) )
  (data (offset i32.const 1024 ) "\0b\00\00\00\20\62\75\74\20\66\6f\75\6e\64\20\0a\00\00\00\20\6e\6f\74\20\66\6f\75\6e\64\0a\00\00\00\4a\53\4f\4e\20\61\72\72\61\79\0b\00\00\00\4a\53\4f\4e\20\6e\75\6d\62\65\72\0b\00\00\00\4a\53\4f\4e\20\6f\62\6a\65\63\74\0b\00\00\00\4a\53\4f\4e\20\73\74\72\69\6e\67\19\00\00\00\54\4f\44\4f\3a\20\69\6d\70\6c\65\6d\65\6e\74\20\73\68\6f\77\56\61\6c\75\65\09\00\00\00\65\78\70\65\63\74\65\64\20\13\00\00\00\72\65\71\75\69\72\65\64\20\61\74\74\72\69\62\75\74\65\20")
  (global $__bump_ptr (mut i32) i32.const 1184 )
  (export "__bump_ptr" (global $__bump_ptr) )
  (func $__alloc (param $sz i32) (result i32)
  global.get $__bump_ptr
  global.get $__bump_ptr
  local.get $sz
  i32.add
  global.set $__bump_ptr
)
  (export "hydra.extract.json.expect_array" (func $hydra.extract.json.expect_array) )
  (export "hydra.extract.json.expect_number" (func $hydra.extract.json.expect_number) )
  (export "hydra.extract.json.expect_object" (func $hydra.extract.json.expect_object) )
  (export "hydra.extract.json.expect_string" (func $hydra.extract.json.expect_string) )
  (export "hydra.extract.json.opt" (func $hydra.extract.json.opt) )
  (export "hydra.extract.json.opt_array" (func $hydra.extract.json.opt_array) )
  (export "hydra.extract.json.opt_string" (func $hydra.extract.json.opt_string) )
  (export "hydra.extract.json.require" (func $hydra.extract.json.require) )
  (export "hydra.extract.json.require_array" (func $hydra.extract.json.require_array) )
  (export "hydra.extract.json.require_number" (func $hydra.extract.json.require_number) )
  (export "hydra.extract.json.require_string" (func $hydra.extract.json.require_string) )
  (export "hydra.extract.json.show_value" (func $hydra.extract.json.show_value) )
  (func $hydra.extract.json.expect_array (param $value i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $els i32)
  (local $v i32)
  (block $end_value (result i32)
  (block $array
  local.get $value
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
  i32.const 1
  local.get $els
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
  (func $hydra.extract.json.expect_number (param $value i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $d i32)
  (local $v i32)
  (block $end_value (result i32)
  (block $number
  local.get $value
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $number $number
)
  local.get $v
  drop
  i32.const 1
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
  br $end_value
)
)
  (func $hydra.extract.json.expect_object (param $value i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $m i32)
  (local $v i32)
  (block $end_value (result i32)
  (block $object
  local.get $value
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
  local.get $m
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
  (func $hydra.extract.json.expect_string (param $value i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $s i32)
  (local $v i32)
  (block $end_value (result i32)
  (block $string
  local.get $value
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
  (func $hydra.extract.json.opt (param $fname i32) (param $m i32) (result i32)
  local.get $fname
  local.get $m
  call $hydra.lib.maps.lookup
)
  (func $hydra.extract.json.opt_array (param $fname i32) (param $m i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $a i32)
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
  local.get $a
  call $hydra.extract.json.expect_array
  call $hydra.lib.eithers.map
  local.get $fname
  local.get $m
  call $hydra.extract.json.opt
  call $hydra.lib.maybes.maybe
)
  (func $hydra.extract.json.opt_string (param $fname i32) (param $m i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $s i32)
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
  local.get $s
  call $hydra.extract.json.expect_string
  call $hydra.lib.eithers.map
  local.get $fname
  local.get $m
  call $hydra.extract.json.opt
  call $hydra.lib.maybes.maybe
)
  (func $hydra.extract.json.require (param $fname i32) (param $m i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $value i32)
  i32.const 0
  i32.const 1154
  local.get $fname
  call $hydra.extract.json.show_value
  i32.const 1039
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
  i32.const 1
  local.get $value
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
  local.get $fname
  local.get $m
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.maybe
)
  (func $hydra.extract.json.require_array (param $fname i32) (param $m i32) (result i32)
  local.get $fname
  local.get $m
  call $hydra.extract.json.require
  i32.const 0
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.json.require_number (param $fname i32) (param $m i32) (result i32)
  local.get $fname
  local.get $m
  call $hydra.extract.json.require
  i32.const 0
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.json.require_string (param $fname i32) (param $m i32) (result i32)
  local.get $fname
  local.get $m
  call $hydra.extract.json.require
  i32.const 0
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.json.show_value (param $value i32) (result i32)
  i32.const 1112
)
)
