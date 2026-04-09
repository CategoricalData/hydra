(module
  (import "hydra.json.decode" "hydra.json.decode.from_json" (func $hydra.json.decode.from_json (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.either" (func $hydra.lib.eithers.either (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map" (func $hydra.lib.eithers.map (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map_list" (func $hydra.lib.eithers.map_list (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.bigint_to_bigfloat" (func $hydra.lib.literals.bigint_to_bigfloat (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.from_list" (func $hydra.lib.maps.from_list (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.to_list" (func $hydra.lib.maps.to_list (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.first" (func $hydra.lib.pairs.first (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.second" (func $hydra.lib.pairs.second (param i32) (result i32) ) )
  (memory $memory 1 )
  (export "memory" (memory $memory) )
  (export "hydra.json.yaml.decode.from_yaml" (func $hydra.json.yaml.decode.from_yaml) )
  (export "hydra.json.yaml.decode.yaml_to_json" (func $hydra.json.yaml.decode.yaml_to_json) )
  (func $hydra.json.yaml.decode.from_yaml (param $types i32) (param $tname i32) (param $typ i32) (param $node i32) (result i32)
  (local $err i32)
  (local $json i32)
  (local $json_result i32)
  local.get $node
  call $hydra.json.yaml.decode.yaml_to_json
  local.set $json_result
  i32.const 0
  local.get $err
  local.get $types
  local.get $tname
  local.get $typ
  local.get $json
  call $hydra.json.decode.from_json
  local.get $json_result
  call $hydra.lib.eithers.either
)
  (func $hydra.json.yaml.decode.yaml_to_json (param $node i32) (result i32)
  (local $b i32)
  (local $convert_entry i32)
  (local $entries i32)
  (local $err i32)
  (local $es i32)
  (local $f i32)
  (local $i i32)
  (local $key i32)
  (local $key_node i32)
  (local $key_result i32)
  (local $kv i32)
  (local $m i32)
  (local $n i32)
  (local $nodes i32)
  (local $results i32)
  (local $s i32)
  (local $str i32)
  (local $v i32)
  (local $val_node i32)
  (local $val_result i32)
  (local $vs i32)
  (block $end_node (result i32)
  (block $sequence
  (block $scalar
  (block $mapping
  local.get $node
  br_table $mapping $scalar $sequence $end_node
  local.get $kv
  call $hydra.lib.pairs.first
  local.set $key_node
  local.get $kv
  call $hydra.lib.pairs.second
  local.set $val_node
  (block $end_node (result i32)
  (block $scalar
  local.get $key_node
  br_table $scalar $end_node
  (block $end_scalar (result i32)
  (block $str
  local.get $s
  br_table $str $end_scalar
  i32.const 1
  local.get $str
  br $end_scalar
)
)
  br $end_node
)
)
  local.set $key_result
  i32.const 0
  local.get $err
  local.get $val_node
  call $hydra.json.yaml.decode.yaml_to_json
  local.set $val_result
  local.get $key
  local.get $v
  local.get $val_result
  call $hydra.lib.eithers.map
  local.get $key_result
  call $hydra.lib.eithers.either
  local.set $convert_entry
  local.get $convert_entry
  local.get $m
  call $hydra.lib.maps.to_list
  call $hydra.lib.eithers.map_list
  local.set $entries
  local.get $es
  call $hydra.lib.maps.from_list
  local.get $entries
  call $hydra.lib.eithers.map
  br $end_node
)
  (block $end_scalar (result i32)
  (block $str
  (block $null
  (block $int
  (block $float
  (block $bool
  local.get $s
  br_table $bool $float $int $null $str $end_scalar
  i32.const 1
  local.get $b
  br $end_scalar
)
  i32.const 1
  local.get $f
  br $end_scalar
)
  i32.const 1
  local.get $i
  call $hydra.lib.literals.bigint_to_bigfloat
  br $end_scalar
)
  i32.const 1
  i32.const 0
  br $end_scalar
)
  i32.const 1
  local.get $str
  br $end_scalar
)
)
  br $end_node
)
  local.get $n
  call $hydra.json.yaml.decode.yaml_to_json
  local.get $nodes
  call $hydra.lib.eithers.map_list
  local.set $results
  local.get $vs
  local.get $results
  call $hydra.lib.eithers.map
  br $end_node
)
)
)
)
