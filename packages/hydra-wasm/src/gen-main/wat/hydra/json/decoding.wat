(module
  (import "hydra.lib.eithers" "hydra.lib.eithers.bind" (func $hydra.lib.eithers.bind (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map" (func $hydra.lib.eithers.map (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map_list" (func $hydra.lib.eithers.map_list (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.lookup" (func $hydra.lib.maps.lookup (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.maybe" (func $hydra.lib.maybes.maybe (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat2" (func $hydra.lib.strings.cat2 (param i32) (result i32) ) )
  (memory $memory 1 )
  (export "memory" (memory $memory) )
  (export "hydra.ext.org.json.decoding.decode_array" (func $hydra.ext.org.json.decoding.decode_array) )
  (export "hydra.ext.org.json.decoding.decode_boolean" (func $hydra.ext.org.json.decoding.decode_boolean) )
  (export "hydra.ext.org.json.decoding.decode_field" (func $hydra.ext.org.json.decoding.decode_field) )
  (export "hydra.ext.org.json.decoding.decode_object" (func $hydra.ext.org.json.decoding.decode_object) )
  (export "hydra.ext.org.json.decoding.decode_optional_field" (func $hydra.ext.org.json.decoding.decode_optional_field) )
  (export "hydra.ext.org.json.decoding.decode_string" (func $hydra.ext.org.json.decoding.decode_string) )
  (func $hydra.ext.org.json.decoding.decode_array (param $decode_elem i32) (param $arg_0 i32) (result i32)
  (local $a i32)
  (block $end_value (result i32)
  (block $array
  local.get $decode_elem
  br_table $array $array
)
  local.get $decode_elem
  local.get $a
  call $hydra.lib.eithers.map_list
  br $end_value
)
)
  (func $hydra.ext.org.json.decoding.decode_boolean (param $arg_0 i32) (result i32)
  (local $b i32)
  (block $end_value (result i32)
  (block $boolean
  local.get $arg_0
  br_table $boolean $boolean
)
  i32.const 1
  local.get $b
  br $end_value
)
)
  (func $hydra.ext.org.json.decoding.decode_field (param $decode_value i32) (param $name i32) (param $m i32) (result i32)
  (local $f i32)
  local.get $decode_value
  local.get $name
  local.get $m
  call $hydra.ext.org.json.decoding.decode_optional_field
  i32.const 0
  i32.const 0 ;; string: "missing field: "
  local.get $name
  call $hydra.lib.strings.cat2
  i32.const 1
  local.get $f
  call $hydra.lib.maybes.maybe
  call $hydra.lib.eithers.bind
)
  (func $hydra.ext.org.json.decoding.decode_object (param $arg_0 i32) (result i32)
  (local $o i32)
  (block $end_value (result i32)
  (block $object
  local.get $arg_0
  br_table $object $object
)
  i32.const 1
  local.get $o
  br $end_value
)
)
  (func $hydra.ext.org.json.decoding.decode_optional_field (param $decode_value i32) (param $name i32) (param $m i32) (result i32)
  (local $v i32)
  (local $x i32)
  i32.const 1
  i32.const 0
  local.get $x
  local.get $v
  local.get $decode_value
  call $hydra.lib.eithers.map
  local.get $name
  local.get $m
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.maybe
)
  (func $hydra.ext.org.json.decoding.decode_string (param $arg_0 i32) (result i32)
  (local $s i32)
  (block $end_value (result i32)
  (block $string
  local.get $arg_0
  br_table $string $string
)
  i32.const 1
  local.get $s
  br $end_value
)
)
)
