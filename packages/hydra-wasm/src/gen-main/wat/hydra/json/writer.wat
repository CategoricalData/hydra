(module
  (import "hydra.lib.equality" "hydra.lib.equality.equal" (func $hydra.lib.equality.equal (param i32) (result i32) ) )
  (import "hydra.lib.equality" "hydra.lib.equality.lt" (func $hydra.lib.equality.lt (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.pure" (func $hydra.lib.lists.pure (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.bigfloat_to_bigint" (func $hydra.lib.literals.bigfloat_to_bigint (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.bigint_to_bigfloat" (func $hydra.lib.literals.bigint_to_bigfloat (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_bigfloat" (func $hydra.lib.literals.show_bigfloat (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_bigint" (func $hydra.lib.literals.show_bigint (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.if_else" (func $hydra.lib.logic.if_else (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.to_list" (func $hydra.lib.maps.to_list (param i32) (result i32) ) )
  (import "hydra.lib.math" "hydra.lib.math.div" (func $hydra.lib.math.div (param i32) (result i32) ) )
  (import "hydra.lib.math" "hydra.lib.math.mod" (func $hydra.lib.math.mod (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.first" (func $hydra.lib.pairs.first (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.second" (func $hydra.lib.pairs.second (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat" (func $hydra.lib.strings.cat (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat2" (func $hydra.lib.strings.cat2 (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.char_at" (func $hydra.lib.strings.char_at (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.from_list" (func $hydra.lib.strings.from_list (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.to_list" (func $hydra.lib.strings.to_list (param i32) (result i32) ) )
  (import "hydra.serialization" "hydra.serialization.braces_list_adaptive" (func $hydra.serialization.braces_list_adaptive (param i32) (result i32) ) )
  (import "hydra.serialization" "hydra.serialization.bracket_list_adaptive" (func $hydra.serialization.bracket_list_adaptive (param i32) (result i32) ) )
  (import "hydra.serialization" "hydra.serialization.cst" (func $hydra.serialization.cst (param i32) (result i32) ) )
  (import "hydra.serialization" "hydra.serialization.ifx" (func $hydra.serialization.ifx (param i32) (result i32) ) )
  (import "hydra.serialization" "hydra.serialization.print_expr" (func $hydra.serialization.print_expr (param i32) (result i32) ) )
  (memory $memory 1 )
  (export "memory" (memory $memory) )
  (export "hydra.json.writer.colon_op" (func $hydra.json.writer.colon_op) )
  (export "hydra.json.writer.json_string" (func $hydra.json.writer.json_string) )
  (export "hydra.json.writer.key_value_to_expr" (func $hydra.json.writer.key_value_to_expr) )
  (export "hydra.json.writer.print_json" (func $hydra.json.writer.print_json) )
  (export "hydra.json.writer.value_to_expr" (func $hydra.json.writer.value_to_expr) )
  (func $hydra.json.writer.colon_op (result i32)
  i32.const 0 ;; string: ":"
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
)
  (func $hydra.json.writer.json_string (param $s i32) (result i32)
  (local $c i32)
  (local $escape i32)
  (local $escaped i32)
  (local $hex_escape i32)
  (local $hi i32)
  (local $lo i32)
  local.get $c
  i32.const 16
  call $hydra.lib.math.div
  i32.const 0 ;; string: "0123456789abcdef"
  call $hydra.lib.strings.char_at
  call $hydra.lib.lists.pure
  call $hydra.lib.strings.from_list
  local.set $hi
  local.get $c
  i32.const 16
  call $hydra.lib.math.mod
  i32.const 0 ;; string: "0123456789abcdef"
  call $hydra.lib.strings.char_at
  call $hydra.lib.lists.pure
  call $hydra.lib.strings.from_list
  local.set $lo
  i32.const 0 ;; string: "\u00"
  local.get $hi
  call $hydra.lib.strings.cat2
  local.get $lo
  call $hydra.lib.strings.cat2
  local.set $hex_escape
  local.get $c
  i32.const 34
  call $hydra.lib.equality.equal
  i32.const 0 ;; string: "\""
  local.get $c
  i32.const 92
  call $hydra.lib.equality.equal
  i32.const 0 ;; string: "\\"
  local.get $c
  i32.const 8
  call $hydra.lib.equality.equal
  i32.const 0 ;; string: "\b"
  local.get $c
  i32.const 12
  call $hydra.lib.equality.equal
  i32.const 0 ;; string: "\f"
  local.get $c
  i32.const 10
  call $hydra.lib.equality.equal
  i32.const 0 ;; string: "\n"
  local.get $c
  i32.const 13
  call $hydra.lib.equality.equal
  i32.const 0 ;; string: "\r"
  local.get $c
  i32.const 9
  call $hydra.lib.equality.equal
  i32.const 0 ;; string: "\t"
  local.get $c
  i32.const 32
  call $hydra.lib.equality.lt
  local.get $c
  local.get $hex_escape
  local.get $c
  call $hydra.lib.lists.pure
  call $hydra.lib.strings.from_list
  call $hydra.lib.logic.if_else
  call $hydra.lib.logic.if_else
  call $hydra.lib.logic.if_else
  call $hydra.lib.logic.if_else
  call $hydra.lib.logic.if_else
  call $hydra.lib.logic.if_else
  call $hydra.lib.logic.if_else
  call $hydra.lib.logic.if_else
  local.set $escape
  local.get $escape
  local.get $s
  call $hydra.lib.strings.to_list
  call $hydra.lib.lists.map
  call $hydra.lib.strings.cat
  local.set $escaped
  i32.const 0 ;; string: """
  local.get $escaped
  call $hydra.lib.strings.cat2
  i32.const 0 ;; string: """
  call $hydra.lib.strings.cat2
)
  (func $hydra.json.writer.key_value_to_expr (param $pair i32) (result i32)
  (local $key i32)
  (local $value i32)
  local.get $pair
  call $hydra.lib.pairs.first
  local.set $key
  local.get $pair
  call $hydra.lib.pairs.second
  local.set $value
  call $hydra.json.writer.colon_op
  local.get $key
  call $hydra.json.writer.json_string
  call $hydra.serialization.cst
  local.get $value
  call $hydra.json.writer.value_to_expr
  call $hydra.serialization.ifx
)
  (func $hydra.json.writer.print_json (param $value i32) (result i32)
  local.get $value
  call $hydra.json.writer.value_to_expr
  call $hydra.serialization.print_expr
)
  (func $hydra.json.writer.value_to_expr (param $value i32) (result i32)
  (local $arr i32)
  (local $b i32)
  (local $n i32)
  (local $obj i32)
  (local $rounded i32)
  (local $s i32)
  (block $end_value (result i32)
  (block $string
  (block $object
  (block $number
  (block $null
  (block $boolean
  (block $array
  local.get $value
  br_table $array $boolean $null $number $object $string $string
)
  call $hydra.json.writer.value_to_expr
  local.get $arr
  call $hydra.lib.lists.map
  call $hydra.serialization.bracket_list_adaptive
  br $end_value
)
  local.get $b
  i32.const 0 ;; string: "true"
  i32.const 0 ;; string: "false"
  call $hydra.lib.logic.if_else
  call $hydra.serialization.cst
  br $end_value
)
  i32.const 0 ;; string: "null"
  call $hydra.serialization.cst
  br $end_value
)
  local.get $n
  call $hydra.lib.literals.bigfloat_to_bigint
  local.set $rounded
  local.get $n
  local.get $rounded
  call $hydra.lib.literals.bigint_to_bigfloat
  call $hydra.lib.equality.equal
  local.get $rounded
  call $hydra.lib.literals.show_bigint
  local.get $n
  call $hydra.lib.literals.show_bigfloat
  call $hydra.lib.logic.if_else
  call $hydra.serialization.cst
  br $end_value
)
  call $hydra.json.writer.key_value_to_expr
  local.get $obj
  call $hydra.lib.maps.to_list
  call $hydra.lib.lists.map
  call $hydra.serialization.braces_list_adaptive
  br $end_value
)
  local.get $s
  call $hydra.json.writer.json_string
  call $hydra.serialization.cst
  br $end_value
)
)
)
