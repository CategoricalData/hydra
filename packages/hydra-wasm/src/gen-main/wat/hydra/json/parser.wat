(module
  (import "hydra.lib.equality" "hydra.lib.equality.equal" (func $hydra.lib.equality.equal (param i32) (result i32) ) )
  (import "hydra.lib.equality" "hydra.lib.equality.gte" (func $hydra.lib.equality.gte (param i32) (result i32) ) )
  (import "hydra.lib.equality" "hydra.lib.equality.identity" (func $hydra.lib.equality.identity (param i32) (result i32) ) )
  (import "hydra.lib.equality" "hydra.lib.equality.lte" (func $hydra.lib.equality.lte (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.foldl" (func $hydra.lib.lists.foldl (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.pure" (func $hydra.lib.lists.pure (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.read_bigfloat" (func $hydra.lib.literals.read_bigfloat (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.and" (func $hydra.lib.logic.and (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.not" (func $hydra.lib.logic.not (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.or" (func $hydra.lib.logic.or (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.maybe" (func $hydra.lib.maybes.maybe (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat2" (func $hydra.lib.strings.cat2 (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.from_list" (func $hydra.lib.strings.from_list (param i32) (result i32) ) )
  (import "hydra.parsers" "hydra.parsers.alt" (func $hydra.parsers.alt (param i32) (result i32) ) )
  (import "hydra.parsers" "hydra.parsers.between" (func $hydra.parsers.between (param i32) (result i32) ) )
  (import "hydra.parsers" "hydra.parsers.bind" (func $hydra.parsers.bind (param i32) (result i32) ) )
  (import "hydra.parsers" "hydra.parsers.char" (func $hydra.parsers.char (param i32) (result i32) ) )
  (import "hydra.parsers" "hydra.parsers.choice" (func $hydra.parsers.choice (param i32) (result i32) ) )
  (import "hydra.parsers" "hydra.parsers.lazy" (func $hydra.parsers.lazy (param i32) (result i32) ) )
  (import "hydra.parsers" "hydra.parsers.many" (func $hydra.parsers.many (param i32) (result i32) ) )
  (import "hydra.parsers" "hydra.parsers.map" (func $hydra.parsers.map (param i32) (result i32) ) )
  (import "hydra.parsers" "hydra.parsers.optional" (func $hydra.parsers.optional (param i32) (result i32) ) )
  (import "hydra.parsers" "hydra.parsers.pure" (func $hydra.parsers.pure (param i32) (result i32) ) )
  (import "hydra.parsers" "hydra.parsers.satisfy" (func $hydra.parsers.satisfy (param i32) (result i32) ) )
  (import "hydra.parsers" "hydra.parsers.sep_by" (func $hydra.parsers.sep_by (param i32) (result i32) ) )
  (import "hydra.parsers" "hydra.parsers.some" (func $hydra.parsers.some (param i32) (result i32) ) )
  (import "hydra.parsers" "hydra.parsers.string" (func $hydra.parsers.string (param i32) (result i32) ) )
  (memory $memory 1 )
  (export "memory" (memory $memory) )
  (export "hydra.json.parser.digit" (func $hydra.json.parser.digit) )
  (export "hydra.json.parser.digits" (func $hydra.json.parser.digits) )
  (export "hydra.json.parser.json_array" (func $hydra.json.parser.json_array) )
  (export "hydra.json.parser.json_bool" (func $hydra.json.parser.json_bool) )
  (export "hydra.json.parser.json_escape_char" (func $hydra.json.parser.json_escape_char) )
  (export "hydra.json.parser.json_exponent_part" (func $hydra.json.parser.json_exponent_part) )
  (export "hydra.json.parser.json_fraction_part" (func $hydra.json.parser.json_fraction_part) )
  (export "hydra.json.parser.json_integer_part" (func $hydra.json.parser.json_integer_part) )
  (export "hydra.json.parser.json_key_value" (func $hydra.json.parser.json_key_value) )
  (export "hydra.json.parser.json_null" (func $hydra.json.parser.json_null) )
  (export "hydra.json.parser.json_number" (func $hydra.json.parser.json_number) )
  (export "hydra.json.parser.json_object" (func $hydra.json.parser.json_object) )
  (export "hydra.json.parser.json_string" (func $hydra.json.parser.json_string) )
  (export "hydra.json.parser.json_string_char" (func $hydra.json.parser.json_string_char) )
  (export "hydra.json.parser.json_value" (func $hydra.json.parser.json_value) )
  (export "hydra.json.parser.parse_json" (func $hydra.json.parser.parse_json) )
  (export "hydra.json.parser.token" (func $hydra.json.parser.token) )
  (export "hydra.json.parser.whitespace" (func $hydra.json.parser.whitespace) )
  (func $hydra.json.parser.digit (result i32)
  (local $c i32)
  local.get $c
  i32.const 48
  call $hydra.lib.equality.gte
  local.get $c
  i32.const 57
  call $hydra.lib.equality.lte
  call $hydra.lib.logic.and
  call $hydra.parsers.satisfy
)
  (func $hydra.json.parser.digits (result i32)
  call $hydra.lib.strings.from_list
  call $hydra.json.parser.digit
  call $hydra.parsers.some
  call $hydra.parsers.map
)
  (func $hydra.json.parser.json_array (result i32)
  (local $x i32)
  local.get $x
  i32.const 91
  call $hydra.parsers.char
  call $hydra.json.parser.token
  i32.const 93
  call $hydra.parsers.char
  call $hydra.json.parser.token
  call $hydra.json.parser.json_value
  call $hydra.parsers.lazy
  i32.const 44
  call $hydra.parsers.char
  call $hydra.json.parser.token
  call $hydra.parsers.sep_by
  call $hydra.parsers.between
  call $hydra.parsers.map
)
  (func $hydra.json.parser.json_bool (result i32)
  i32.const 1
  i32.const 0 ;; string: "true"
  call $hydra.parsers.string
  call $hydra.json.parser.token
  call $hydra.parsers.map
  i32.const 0
  i32.const 0 ;; string: "false"
  call $hydra.parsers.string
  call $hydra.json.parser.token
  call $hydra.parsers.map
  call $hydra.parsers.alt
)
  (func $hydra.json.parser.json_escape_char (result i32)
  i32.const 8
  ;; list elements follow
  i32.const 34
  i32.const 34
  call $hydra.parsers.char
  call $hydra.parsers.map
  i32.const 92
  i32.const 92
  call $hydra.parsers.char
  call $hydra.parsers.map
  i32.const 47
  i32.const 47
  call $hydra.parsers.char
  call $hydra.parsers.map
  i32.const 8
  i32.const 98
  call $hydra.parsers.char
  call $hydra.parsers.map
  i32.const 12
  i32.const 102
  call $hydra.parsers.char
  call $hydra.parsers.map
  i32.const 10
  i32.const 110
  call $hydra.parsers.char
  call $hydra.parsers.map
  i32.const 13
  i32.const 114
  call $hydra.parsers.char
  call $hydra.parsers.map
  i32.const 9
  i32.const 116
  call $hydra.parsers.char
  call $hydra.parsers.map
  call $hydra.parsers.choice
)
  (func $hydra.json.parser.json_exponent_part (result i32)
  (local $arg_ i32)
  (local $c i32)
  (local $digits i32)
  (local $sign i32)
  local.get $c
  i32.const 101
  call $hydra.lib.equality.equal
  local.get $c
  i32.const 69
  call $hydra.lib.equality.equal
  call $hydra.lib.logic.or
  call $hydra.parsers.satisfy
  local.get $c
  i32.const 43
  call $hydra.lib.equality.equal
  local.get $c
  i32.const 45
  call $hydra.lib.equality.equal
  call $hydra.lib.logic.or
  call $hydra.parsers.satisfy
  call $hydra.parsers.optional
  i32.const 0 ;; string: "e"
  i32.const 0 ;; string: ""
  local.get $arg_
  call $hydra.lib.lists.pure
  call $hydra.lib.strings.from_list
  local.get $sign
  call $hydra.lib.maybes.maybe
  call $hydra.lib.strings.cat2
  local.get $digits
  call $hydra.lib.strings.cat2
  call $hydra.json.parser.digits
  call $hydra.parsers.map
  call $hydra.parsers.bind
  call $hydra.parsers.bind
  call $hydra.parsers.optional
)
  (func $hydra.json.parser.json_fraction_part (result i32)
  (local $d i32)
  i32.const 46
  call $hydra.parsers.char
  i32.const 0 ;; string: "."
  local.get $d
  call $hydra.lib.strings.cat2
  call $hydra.json.parser.digits
  call $hydra.parsers.map
  call $hydra.parsers.bind
  call $hydra.parsers.optional
)
  (func $hydra.json.parser.json_integer_part (result i32)
  (local $digits i32)
  (local $sign i32)
  i32.const 45
  call $hydra.parsers.char
  call $hydra.parsers.optional
  call $hydra.json.parser.digits
  local.get $digits
  i32.const 0 ;; string: "-"
  local.get $digits
  call $hydra.lib.strings.cat2
  local.get $sign
  call $hydra.lib.maybes.maybe
  call $hydra.parsers.pure
  call $hydra.parsers.bind
  call $hydra.parsers.bind
)
  (func $hydra.json.parser.json_key_value (result i32)
  (local $chars i32)
  (local $key i32)
  (local $v i32)
  i32.const 34
  call $hydra.parsers.char
  call $hydra.json.parser.json_string_char
  call $hydra.parsers.many
  i32.const 34
  call $hydra.parsers.char
  local.get $chars
  call $hydra.lib.strings.from_list
  call $hydra.parsers.pure
  call $hydra.parsers.bind
  call $hydra.parsers.bind
  call $hydra.parsers.bind
  call $hydra.json.parser.token
  i32.const 58
  call $hydra.parsers.char
  call $hydra.json.parser.token
  local.get $key
  local.get $v
  call $hydra.json.parser.json_value
  call $hydra.parsers.lazy
  call $hydra.parsers.map
  call $hydra.parsers.bind
  call $hydra.parsers.bind
)
  (func $hydra.json.parser.json_null (result i32)
  i32.const 0
  i32.const 0 ;; string: "null"
  call $hydra.parsers.string
  call $hydra.json.parser.token
  call $hydra.parsers.map
)
  (func $hydra.json.parser.json_number (result i32)
  (local $exp_part i32)
  (local $frac_part i32)
  (local $int_part i32)
  (local $num_str i32)
  call $hydra.json.parser.json_integer_part
  call $hydra.json.parser.json_fraction_part
  call $hydra.json.parser.json_exponent_part
  local.get $int_part
  i32.const 0 ;; string: ""
  call $hydra.lib.equality.identity
  local.get $frac_part
  call $hydra.lib.maybes.maybe
  call $hydra.lib.strings.cat2
  i32.const 0 ;; string: ""
  call $hydra.lib.equality.identity
  local.get $exp_part
  call $hydra.lib.maybes.maybe
  call $hydra.lib.strings.cat2
  local.set $num_str
  f64.const 0.0
  call $hydra.lib.equality.identity
  local.get $num_str
  call $hydra.lib.literals.read_bigfloat
  call $hydra.lib.maybes.maybe
  call $hydra.parsers.pure
  call $hydra.parsers.bind
  call $hydra.parsers.bind
  call $hydra.parsers.bind
  call $hydra.json.parser.token
)
  (func $hydra.json.parser.json_object (result i32)
  (local $x i32)
  local.get $x
  i32.const 123
  call $hydra.parsers.char
  call $hydra.json.parser.token
  i32.const 125
  call $hydra.parsers.char
  call $hydra.json.parser.token
  call $hydra.json.parser.json_key_value
  i32.const 44
  call $hydra.parsers.char
  call $hydra.json.parser.token
  call $hydra.parsers.sep_by
  call $hydra.parsers.between
  call $hydra.parsers.map
)
  (func $hydra.json.parser.json_string (result i32)
  (local $chars i32)
  i32.const 34
  call $hydra.parsers.char
  call $hydra.json.parser.json_string_char
  call $hydra.parsers.many
  i32.const 34
  call $hydra.parsers.char
  local.get $chars
  call $hydra.lib.strings.from_list
  call $hydra.parsers.pure
  call $hydra.parsers.bind
  call $hydra.parsers.bind
  call $hydra.parsers.bind
  call $hydra.json.parser.token
)
  (func $hydra.json.parser.json_string_char (result i32)
  (local $c i32)
  i32.const 92
  call $hydra.parsers.char
  call $hydra.json.parser.json_escape_char
  call $hydra.parsers.bind
  local.get $c
  i32.const 34
  call $hydra.lib.equality.equal
  call $hydra.lib.logic.not
  local.get $c
  i32.const 92
  call $hydra.lib.equality.equal
  call $hydra.lib.logic.not
  call $hydra.lib.logic.and
  call $hydra.parsers.satisfy
  call $hydra.parsers.alt
)
  (func $hydra.json.parser.json_value (result i32)
  i32.const 6
  ;; list elements follow
  call $hydra.json.parser.json_null
  call $hydra.json.parser.json_bool
  call $hydra.json.parser.json_number
  call $hydra.json.parser.json_string
  call $hydra.json.parser.json_array
  call $hydra.json.parser.json_object
  call $hydra.parsers.choice
)
  (func $hydra.json.parser.parse_json (param $input i32) (result i32)
  nop
)
  (func $hydra.json.parser.token (param $p i32) (result i32)
  (local $x i32)
  local.get $p
  call $hydra.json.parser.whitespace
  local.get $x
  call $hydra.parsers.pure
  call $hydra.parsers.bind
  call $hydra.parsers.bind
)
  (func $hydra.json.parser.whitespace (result i32)
  (local $c i32)
  call $hydra.lib.logic.or
  i32.const 0
  i32.const 4
  ;; list elements follow
  local.get $c
  i32.const 32
  call $hydra.lib.equality.equal
  local.get $c
  i32.const 9
  call $hydra.lib.equality.equal
  local.get $c
  i32.const 10
  call $hydra.lib.equality.equal
  local.get $c
  i32.const 13
  call $hydra.lib.equality.equal
  call $hydra.lib.lists.foldl
  call $hydra.parsers.satisfy
  call $hydra.parsers.many
  call $hydra.parsers.map
)
)
