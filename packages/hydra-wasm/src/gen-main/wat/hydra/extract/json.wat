(module
  (import "hydra.lib.eithers" "hydra.lib.eithers.bind" (func $hydra.lib.eithers.bind (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map" (func $hydra.lib.eithers.map (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.lookup" (func $hydra.lib.maps.lookup (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.maybe" (func $hydra.lib.maybes.maybe (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat" (func $hydra.lib.strings.cat (param i32) (result i32) ) )
  (memory $memory 1 )
  (export "memory" (memory $memory) )
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
  (local $els i32)
  (block $end_value (result i32)
  (block $array
  local.get $value
  br_table $array $array
)
  i32.const 1
  local.get $els
  br $end_value
)
)
  (func $hydra.extract.json.expect_number (param $value i32) (result i32)
  (local $d i32)
  (block $end_value (result i32)
  (block $number
  local.get $value
  br_table $number $number
)
  i32.const 1
  local.get $d
  br $end_value
)
)
  (func $hydra.extract.json.expect_object (param $value i32) (result i32)
  (local $m i32)
  (block $end_value (result i32)
  (block $object
  local.get $value
  br_table $object $object
)
  i32.const 1
  local.get $m
  br $end_value
)
)
  (func $hydra.extract.json.expect_string (param $value i32) (result i32)
  (local $s i32)
  (block $end_value (result i32)
  (block $string
  local.get $value
  br_table $string $string
)
  i32.const 1
  local.get $s
  br $end_value
)
)
  (func $hydra.extract.json.opt (param $fname i32) (param $m i32) (result i32)
  local.get $fname
  local.get $m
  call $hydra.lib.maps.lookup
)
  (func $hydra.extract.json.opt_array (param $fname i32) (param $m i32) (result i32)
  (local $a i32)
  (local $x i32)
  i32.const 1
  i32.const 0
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
  (local $s i32)
  (local $x i32)
  i32.const 1
  i32.const 0
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
  (local $value i32)
  i32.const 0
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "required attribute "
  local.get $fname
  call $hydra.extract.json.show_value
  i32.const 0 ;; string: " not found"
  call $hydra.lib.strings.cat
  i32.const 1
  local.get $value
  local.get $fname
  local.get $m
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.maybe
)
  (func $hydra.extract.json.require_array (param $fname i32) (param $m i32) (result i32)
  local.get $fname
  local.get $m
  call $hydra.extract.json.require
  call $hydra.extract.json.expect_array
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.json.require_number (param $fname i32) (param $m i32) (result i32)
  local.get $fname
  local.get $m
  call $hydra.extract.json.require
  call $hydra.extract.json.expect_number
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.json.require_string (param $fname i32) (param $m i32) (result i32)
  local.get $fname
  local.get $m
  call $hydra.extract.json.require
  call $hydra.extract.json.expect_string
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.json.show_value (param $value i32) (result i32)
  i32.const 0 ;; string: "TODO: implement showValue"
)
)
