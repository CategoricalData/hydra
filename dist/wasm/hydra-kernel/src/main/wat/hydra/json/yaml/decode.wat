(module
  (import "hydra.json.decode" "hydra.json.decode.from_json" (func $hydra.json.decode.from_json (param i32) (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.either" (func $hydra.lib.eithers.either (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map" (func $hydra.lib.eithers.map (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map_list" (func $hydra.lib.eithers.map_list (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.bigfloat_to_float64" (func $hydra.lib.literals.bigfloat_to_float64 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.bigint_to_decimal" (func $hydra.lib.literals.bigint_to_decimal (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.float64_to_decimal" (func $hydra.lib.literals.float64_to_decimal (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.from_list" (func $hydra.lib.maps.from_list (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.to_list" (func $hydra.lib.maps.to_list (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.first" (func $hydra.lib.pairs.first (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.second" (func $hydra.lib.pairs.second (param i32) (result i32) ) )
  (memory $memory 2 )
  (export "memory" (memory $memory) )
  (data (offset i32.const 1024 ) "\1b\00\00\00\6e\6f\6e\2d\73\63\61\6c\61\72\20\59\41\4d\4c\20\6d\61\70\70\69\6e\67\20\6b\65\79\1b\00\00\00\6e\6f\6e\2d\73\74\72\69\6e\67\20\59\41\4d\4c\20\6d\61\70\70\69\6e\67\20\6b\65\79")
  (global $__bump_ptr (mut i32) i32.const 1088 )
  (export "__bump_ptr" (global $__bump_ptr) )
  (func $__alloc (param $sz i32) (result i32)
  global.get $__bump_ptr
  global.get $__bump_ptr
  local.get $sz
  i32.add
  global.set $__bump_ptr
)
  (export "hydra.json.yaml.decode.from_yaml" (func $hydra.json.yaml.decode.from_yaml) )
  (export "hydra.json.yaml.decode.yaml_to_json" (func $hydra.json.yaml.decode.yaml_to_json) )
  (func $hydra.json.yaml.decode.from_yaml (param $types i32) (param $tname i32) (param $typ i32) (param $node i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $err i32)
  (local $json i32)
  (local $json_result i32)
  local.get $node
  call $hydra.json.yaml.decode.yaml_to_json
  local.set $json_result
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
  local.get $types
  local.get $tname
  local.get $typ
  local.get $json
  call $hydra.json.decode.from_json
  local.get $json_result
  call $hydra.lib.eithers.either
)
  (func $hydra.json.yaml.decode.yaml_to_json (param $node i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $b i32)
  (local $convert_entry i32)
  (local $d i32)
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
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $mapping $scalar $sequence $sequence
)
  local.get $v
  drop
  local.get $kv
  call $hydra.lib.pairs.first
  local.set $key_node
  local.get $kv
  call $hydra.lib.pairs.second
  local.set $val_node
  (block $end_node (result i32)
  (block $scalar
  local.get $key_node
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $scalar $scalar
)
  local.get $v
  drop
  (block $end_scalar (result i32)
  (block $str
  local.get $s
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $str $str
)
  local.get $v
  drop
  i32.const 1
  local.get $str
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
  br $end_scalar
)
  br $end_node
)
  local.set $key_result
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
  local.get $val_node
  call $hydra.json.yaml.decode.yaml_to_json
  local.set $val_result
  local.get $key
  local.get $v
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
  i32.const 4
  local.get $es
  call $hydra.lib.maps.from_list
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
  local.get $entries
  call $hydra.lib.eithers.map
  br $end_node
)
  local.get $v
  drop
  (block $end_scalar (result i32)
  (block $str
  (block $null
  (block $int
  (block $float
  (block $decimal
  (block $bool
  local.get $s
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $bool $decimal $float $int $null $str $str
)
  local.get $v
  drop
  i32.const 1
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
  br $end_scalar
)
  local.get $v
  drop
  i32.const 1
  i32.const 3
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
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  br $end_scalar
)
  local.get $v
  drop
  i32.const 1
  i32.const 3
  local.get $f
  call $hydra.lib.literals.bigfloat_to_float64
  call $hydra.lib.literals.float64_to_decimal
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
  br $end_scalar
)
  local.get $v
  drop
  i32.const 1
  i32.const 3
  local.get $i
  call $hydra.lib.literals.bigint_to_decimal
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
  br $end_scalar
)
  local.get $v
  drop
  i32.const 1
  i32.const 2
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
  br $end_scalar
)
  local.get $v
  drop
  i32.const 1
  i32.const 5
  local.get $str
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
  br $end_scalar
)
  br $end_node
)
  local.get $v
  drop
  local.get $n
  call $hydra.json.yaml.decode.yaml_to_json
  local.get $nodes
  call $hydra.lib.eithers.map_list
  local.set $results
  i32.const 0
  local.get $vs
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
  local.get $results
  call $hydra.lib.eithers.map
  br $end_node
)
)
)
