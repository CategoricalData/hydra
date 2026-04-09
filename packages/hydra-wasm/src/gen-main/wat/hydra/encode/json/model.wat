(module
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.bimap" (func $hydra.lib.maps.bimap (param i32) (result i32) ) )
  (memory $memory 1 )
  (export "memory" (memory $memory) )
  (export "hydra.encode.json.model.value" (func $hydra.encode.json.model.value) )
  (func $hydra.encode.json.model.value (param $arg_0 i32) (result i32)
  (local $m i32)
  (local $x i32)
  (local $xs i32)
  (block $end_value (result i32)
  (block $string
  (block $object
  (block $number
  (block $null
  (block $boolean
  (block $array
  local.get $arg_0
  br_table $array $boolean $null $number $object $string $string
)
  i32.const 0 ;; string: "hydra.json.model.Value"
  i32.const 0 ;; string: "array"
  call $hydra.encode.json.model.value
  local.get $xs
  call $hydra.lib.lists.map
  br $end_value
)
  i32.const 0 ;; string: "hydra.json.model.Value"
  i32.const 0 ;; string: "boolean"
  local.get $x
  br $end_value
)
  i32.const 0 ;; string: "hydra.json.model.Value"
  i32.const 0 ;; string: "null"
  i32.const 0
  br $end_value
)
  i32.const 0 ;; string: "hydra.json.model.Value"
  i32.const 0 ;; string: "number"
  local.get $x
  br $end_value
)
  i32.const 0 ;; string: "hydra.json.model.Value"
  i32.const 0 ;; string: "object"
  local.get $x
  call $hydra.encode.json.model.value
  local.get $m
  call $hydra.lib.maps.bimap
  br $end_value
)
  i32.const 0 ;; string: "hydra.json.model.Value"
  i32.const 0 ;; string: "string"
  local.get $x
  br $end_value
)
)
)
