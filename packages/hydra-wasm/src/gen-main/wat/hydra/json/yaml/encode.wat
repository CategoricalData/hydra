(module
  (import "hydra.json.encode" "hydra.json.encode.to_json" (func $hydra.json.encode.to_json (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map" (func $hydra.lib.eithers.map (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.from_list" (func $hydra.lib.maps.from_list (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.to_list" (func $hydra.lib.maps.to_list (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.first" (func $hydra.lib.pairs.first (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.second" (func $hydra.lib.pairs.second (param i32) (result i32) ) )
  (memory $memory 1 )
  (export "memory" (memory $memory) )
  (export "hydra.json.yaml.encode.json_to_yaml" (func $hydra.json.yaml.encode.json_to_yaml) )
  (export "hydra.json.yaml.encode.to_yaml" (func $hydra.json.yaml.encode.to_yaml) )
  (func $hydra.json.yaml.encode.json_to_yaml (param $value i32) (result i32)
  (local $arr i32)
  (local $b i32)
  (local $kv i32)
  (local $n i32)
  (local $obj i32)
  (local $s i32)
  (local $v i32)
  (block $end_value (result i32)
  (block $string
  (block $object
  (block $number
  (block $null
  (block $boolean
  (block $array
  local.get $value
  br_table $array $boolean $null $number $object $string $end_value
  local.get $v
  call $hydra.json.yaml.encode.json_to_yaml
  local.get $arr
  call $hydra.lib.lists.map
  br $end_value
)
  local.get $b
  br $end_value
)
  i32.const 0
  br $end_value
)
  local.get $n
  br $end_value
)
  local.get $kv
  call $hydra.lib.pairs.first
  local.get $kv
  call $hydra.lib.pairs.second
  call $hydra.json.yaml.encode.json_to_yaml
  local.get $obj
  call $hydra.lib.maps.to_list
  call $hydra.lib.lists.map
  call $hydra.lib.maps.from_list
  br $end_value
)
  local.get $s
  br $end_value
)
)
)
  (func $hydra.json.yaml.encode.to_yaml (param $types i32) (param $tname i32) (param $typ i32) (param $term i32) (result i32)
  (local $v i32)
  local.get $v
  call $hydra.json.yaml.encode.json_to_yaml
  local.get $types
  local.get $tname
  local.get $typ
  local.get $term
  call $hydra.json.encode.to_json
  call $hydra.lib.eithers.map
)
)
