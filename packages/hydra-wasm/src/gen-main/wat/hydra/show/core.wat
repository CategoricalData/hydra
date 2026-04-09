(module
  (import "hydra.lib.eithers" "hydra.lib.eithers.either" (func $hydra.lib.eithers.either (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.concat" (func $hydra.lib.lists.concat (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.cons" (func $hydra.lib.lists.cons (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.null" (func $hydra.lib.lists.null (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_bigfloat" (func $hydra.lib.literals.show_bigfloat (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_bigint" (func $hydra.lib.literals.show_bigint (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_float32" (func $hydra.lib.literals.show_float32 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_float64" (func $hydra.lib.literals.show_float64 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_int16" (func $hydra.lib.literals.show_int16 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_int32" (func $hydra.lib.literals.show_int32 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_int64" (func $hydra.lib.literals.show_int64 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_int8" (func $hydra.lib.literals.show_int8 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_string" (func $hydra.lib.literals.show_string (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_uint16" (func $hydra.lib.literals.show_uint16 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_uint32" (func $hydra.lib.literals.show_uint32 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_uint64" (func $hydra.lib.literals.show_uint64 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_uint8" (func $hydra.lib.literals.show_uint8 (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.if_else" (func $hydra.lib.logic.if_else (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.to_list" (func $hydra.lib.maps.to_list (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.maybe" (func $hydra.lib.maybes.maybe (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.first" (func $hydra.lib.pairs.first (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.second" (func $hydra.lib.pairs.second (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.to_list" (func $hydra.lib.sets.to_list (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat" (func $hydra.lib.strings.cat (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat2" (func $hydra.lib.strings.cat2 (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.intercalate" (func $hydra.lib.strings.intercalate (param i32) (result i32) ) )
  (memory $memory 1 )
  (export "memory" (memory $memory) )
  (export "hydra.show.core.binding" (func $hydra.show.core.binding) )
  (export "hydra.show.core.either" (func $hydra.show.core.either) )
  (export "hydra.show.core.elimination" (func $hydra.show.core.elimination) )
  (export "hydra.show.core.field" (func $hydra.show.core.field) )
  (export "hydra.show.core.field_type" (func $hydra.show.core.field_type) )
  (export "hydra.show.core.fields" (func $hydra.show.core.fields) )
  (export "hydra.show.core.float" (func $hydra.show.core.float) )
  (export "hydra.show.core.float_type" (func $hydra.show.core.float_type) )
  (export "hydra.show.core.function" (func $hydra.show.core.function) )
  (export "hydra.show.core.injection" (func $hydra.show.core.injection) )
  (export "hydra.show.core.integer" (func $hydra.show.core.integer) )
  (export "hydra.show.core.integer_type" (func $hydra.show.core.integer_type) )
  (export "hydra.show.core.lambda" (func $hydra.show.core.lambda) )
  (export "hydra.show.core.let" (func $hydra.show.core.let) )
  (export "hydra.show.core.list" (func $hydra.show.core.list) )
  (export "hydra.show.core.literal" (func $hydra.show.core.literal) )
  (export "hydra.show.core.literal_type" (func $hydra.show.core.literal_type) )
  (export "hydra.show.core.map" (func $hydra.show.core.map) )
  (export "hydra.show.core.maybe" (func $hydra.show.core.maybe) )
  (export "hydra.show.core.pair" (func $hydra.show.core.pair) )
  (export "hydra.show.core.read_term" (func $hydra.show.core.read_term) )
  (export "hydra.show.core.set" (func $hydra.show.core.set) )
  (export "hydra.show.core.term" (func $hydra.show.core.term) )
  (export "hydra.show.core.type" (func $hydra.show.core.type) )
  (export "hydra.show.core.type_scheme" (func $hydra.show.core.type_scheme) )
  (func $hydra.show.core.binding (param $el i32) (result i32)
  (local $name i32)
  (local $t i32)
  (local $ts i32)
  (local $type_str i32)
  nop
  local.set $name
  local.get $el
  ;; project field: term
  local.set $t
  i32.const 0 ;; string: ""
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: ":("
  local.get $ts
  call $hydra.show.core.type_scheme
  i32.const 0 ;; string: ")"
  call $hydra.lib.strings.cat
  local.get $el
  ;; project field: type
  call $hydra.lib.maybes.maybe
  local.set $type_str
  i32.const 4
  ;; list elements follow
  local.get $name
  local.get $type_str
  i32.const 0 ;; string: " = "
  local.get $t
  call $hydra.show.core.term
  call $hydra.lib.strings.cat
)
  (func $hydra.show.core.either (param $show_a i32) (param $show_b i32) (param $e i32) (result i32)
  (local $a i32)
  (local $b i32)
  i32.const 0 ;; string: "left("
  local.get $a
  local.get $show_a
  i32.const 0 ;; string: ")"
  call $hydra.lib.strings.cat2
  call $hydra.lib.strings.cat2
  i32.const 0 ;; string: "right("
  local.get $b
  local.get $show_b
  i32.const 0 ;; string: ")"
  call $hydra.lib.strings.cat2
  call $hydra.lib.strings.cat2
  local.get $e
  call $hydra.lib.eithers.either
)
  (func $hydra.show.core.elimination (param $elm i32) (result i32)
  (local $all_fields i32)
  (local $cases i32)
  (local $cs i32)
  (local $d i32)
  (local $default_field i32)
  (local $fname i32)
  (local $mdef i32)
  (local $tname i32)
  (block $end_elimination (result i32)
  (block $wrap
  (block $union
  (block $record
  local.get $elm
  br_table $record $union $wrap $wrap
)
  nop
  local.set $tname
  nop
  local.set $fname
  i32.const 5
  ;; list elements follow
  i32.const 0 ;; string: "project("
  local.get $tname
  i32.const 0 ;; string: "){"
  local.get $fname
  i32.const 0 ;; string: "}"
  call $hydra.lib.strings.cat
  br $end_elimination
)
  nop
  local.set $tname
  local.get $cs
  ;; project field: default
  local.set $mdef
  local.get $cs
  ;; project field: cases
  local.set $cases
  i32.const 0
  ;; list elements follow
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "[default]"
  local.get $d
  local.get $mdef
  call $hydra.lib.maybes.maybe
  local.set $default_field
  i32.const 2
  ;; list elements follow
  local.get $cases
  local.get $default_field
  call $hydra.lib.lists.concat
  local.set $all_fields
  i32.const 4
  ;; list elements follow
  i32.const 0 ;; string: "case("
  local.get $tname
  i32.const 0 ;; string: ")"
  local.get $all_fields
  call $hydra.show.core.fields
  call $hydra.lib.strings.cat
  br $end_elimination
)
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "unwrap("
  nop
  i32.const 0 ;; string: ")"
  call $hydra.lib.strings.cat
  br $end_elimination
)
)
  (func $hydra.show.core.field (param $field i32) (result i32)
  (local $fname i32)
  (local $fterm i32)
  nop
  local.set $fname
  local.get $field
  ;; project field: term
  local.set $fterm
  i32.const 3
  ;; list elements follow
  local.get $fname
  i32.const 0 ;; string: "="
  local.get $fterm
  call $hydra.show.core.term
  call $hydra.lib.strings.cat
)
  (func $hydra.show.core.field_type (param $ft i32) (result i32)
  (local $fname i32)
  (local $ftyp i32)
  nop
  local.set $fname
  local.get $ft
  ;; project field: type
  local.set $ftyp
  i32.const 3
  ;; list elements follow
  local.get $fname
  i32.const 0 ;; string: ":"
  local.get $ftyp
  call $hydra.show.core.type
  call $hydra.lib.strings.cat
)
  (func $hydra.show.core.fields (param $flds i32) (result i32)
  (local $field_strs i32)
  call $hydra.show.core.field
  local.get $flds
  call $hydra.lib.lists.map
  local.set $field_strs
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "{"
  i32.const 0 ;; string: ", "
  local.get $field_strs
  call $hydra.lib.strings.intercalate
  i32.const 0 ;; string: "}"
  call $hydra.lib.strings.cat
)
  (func $hydra.show.core.float (param $fv i32) (result i32)
  (local $v i32)
  (block $end_float_value (result i32)
  (block $float64
  (block $float32
  (block $bigfloat
  local.get $fv
  br_table $bigfloat $float32 $float64 $float64
)
  local.get $v
  call $hydra.lib.literals.show_bigfloat
  i32.const 0 ;; string: ":bigfloat"
  call $hydra.lib.strings.cat2
  br $end_float_value
)
  local.get $v
  call $hydra.lib.literals.show_float32
  i32.const 0 ;; string: ":float32"
  call $hydra.lib.strings.cat2
  br $end_float_value
)
  local.get $v
  call $hydra.lib.literals.show_float64
  i32.const 0 ;; string: ":float64"
  call $hydra.lib.strings.cat2
  br $end_float_value
)
)
  (func $hydra.show.core.float_type (param $ft i32) (result i32)
  (block $end_float_type (result i32)
  (block $float64
  (block $float32
  (block $bigfloat
  local.get $ft
  br_table $bigfloat $float32 $float64 $float64
)
  i32.const 0 ;; string: "bigfloat"
  br $end_float_type
)
  i32.const 0 ;; string: "float32"
  br $end_float_type
)
  i32.const 0 ;; string: "float64"
  br $end_float_type
)
)
  (func $hydra.show.core.function (param $f i32) (result i32)
  (local $v i32)
  (block $end_function (result i32)
  (block $lambda
  (block $elimination
  local.get $f
  br_table $elimination $lambda $lambda
)
  local.get $v
  call $hydra.show.core.elimination
  br $end_function
)
  local.get $v
  call $hydra.show.core.lambda
  br $end_function
)
)
  (func $hydra.show.core.injection (param $inj i32) (result i32)
  (local $f i32)
  (local $tname i32)
  local.get $inj
  ;; project field: type_name
  local.set $tname
  local.get $inj
  ;; project field: field
  local.set $f
  i32.const 4
  ;; list elements follow
  i32.const 0 ;; string: "inject("
  nop
  i32.const 0 ;; string: ")"
  i32.const 1
  ;; list elements follow
  local.get $f
  call $hydra.show.core.fields
  call $hydra.lib.strings.cat
)
  (func $hydra.show.core.integer (param $iv i32) (result i32)
  (local $v i32)
  (block $end_integer_value (result i32)
  (block $uint64
  (block $uint32
  (block $uint16
  (block $uint8
  (block $int64
  (block $int32
  (block $int16
  (block $int8
  (block $bigint
  local.get $iv
  br_table $bigint $int8 $int16 $int32 $int64 $uint8 $uint16 $uint32 $uint64 $uint64
)
  local.get $v
  call $hydra.lib.literals.show_bigint
  i32.const 0 ;; string: ":bigint"
  call $hydra.lib.strings.cat2
  br $end_integer_value
)
  local.get $v
  call $hydra.lib.literals.show_int8
  i32.const 0 ;; string: ":int8"
  call $hydra.lib.strings.cat2
  br $end_integer_value
)
  local.get $v
  call $hydra.lib.literals.show_int16
  i32.const 0 ;; string: ":int16"
  call $hydra.lib.strings.cat2
  br $end_integer_value
)
  local.get $v
  call $hydra.lib.literals.show_int32
  i32.const 0 ;; string: ":int32"
  call $hydra.lib.strings.cat2
  br $end_integer_value
)
  local.get $v
  call $hydra.lib.literals.show_int64
  i32.const 0 ;; string: ":int64"
  call $hydra.lib.strings.cat2
  br $end_integer_value
)
  local.get $v
  call $hydra.lib.literals.show_uint8
  i32.const 0 ;; string: ":uint8"
  call $hydra.lib.strings.cat2
  br $end_integer_value
)
  local.get $v
  call $hydra.lib.literals.show_uint16
  i32.const 0 ;; string: ":uint16"
  call $hydra.lib.strings.cat2
  br $end_integer_value
)
  local.get $v
  call $hydra.lib.literals.show_uint32
  i32.const 0 ;; string: ":uint32"
  call $hydra.lib.strings.cat2
  br $end_integer_value
)
  local.get $v
  call $hydra.lib.literals.show_uint64
  i32.const 0 ;; string: ":uint64"
  call $hydra.lib.strings.cat2
  br $end_integer_value
)
)
  (func $hydra.show.core.integer_type (param $it i32) (result i32)
  (block $end_integer_type (result i32)
  (block $uint64
  (block $uint32
  (block $uint16
  (block $uint8
  (block $int64
  (block $int32
  (block $int16
  (block $int8
  (block $bigint
  local.get $it
  br_table $bigint $int8 $int16 $int32 $int64 $uint8 $uint16 $uint32 $uint64 $uint64
)
  i32.const 0 ;; string: "bigint"
  br $end_integer_type
)
  i32.const 0 ;; string: "int8"
  br $end_integer_type
)
  i32.const 0 ;; string: "int16"
  br $end_integer_type
)
  i32.const 0 ;; string: "int32"
  br $end_integer_type
)
  i32.const 0 ;; string: "int64"
  br $end_integer_type
)
  i32.const 0 ;; string: "uint8"
  br $end_integer_type
)
  i32.const 0 ;; string: "uint16"
  br $end_integer_type
)
  i32.const 0 ;; string: "uint32"
  br $end_integer_type
)
  i32.const 0 ;; string: "uint64"
  br $end_integer_type
)
)
  (func $hydra.show.core.lambda (param $l i32) (result i32)
  (local $body i32)
  (local $mt i32)
  (local $t i32)
  (local $type_str i32)
  (local $v i32)
  nop
  local.set $v
  local.get $l
  ;; project field: domain
  local.set $mt
  local.get $l
  ;; project field: body
  local.set $body
  i32.const 0 ;; string: ""
  i32.const 0 ;; string: ":"
  local.get $t
  call $hydra.show.core.type
  call $hydra.lib.strings.cat2
  local.get $mt
  call $hydra.lib.maybes.maybe
  local.set $type_str
  i32.const 5
  ;; list elements follow
  i32.const 0 ;; string: "λ"
  local.get $v
  local.get $type_str
  i32.const 0 ;; string: "."
  local.get $body
  call $hydra.show.core.term
  call $hydra.lib.strings.cat
)
  (func $hydra.show.core.let (param $l i32) (result i32)
  (local $binding_strs i32)
  (local $bindings i32)
  (local $env i32)
  local.get $l
  ;; project field: bindings
  local.set $bindings
  local.get $l
  ;; project field: body
  local.set $env
  call $hydra.show.core.binding
  local.get $bindings
  call $hydra.lib.lists.map
  local.set $binding_strs
  i32.const 4
  ;; list elements follow
  i32.const 0 ;; string: "let "
  i32.const 0 ;; string: ", "
  local.get $binding_strs
  call $hydra.lib.strings.intercalate
  i32.const 0 ;; string: " in "
  local.get $env
  call $hydra.show.core.term
  call $hydra.lib.strings.cat
)
  (func $hydra.show.core.list (param $f i32) (param $xs i32) (result i32)
  (local $element_strs i32)
  local.get $f
  local.get $xs
  call $hydra.lib.lists.map
  local.set $element_strs
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "["
  i32.const 0 ;; string: ", "
  local.get $element_strs
  call $hydra.lib.strings.intercalate
  i32.const 0 ;; string: "]"
  call $hydra.lib.strings.cat
)
  (func $hydra.show.core.literal (param $l i32) (result i32)
  (local $b i32)
  (local $fv i32)
  (local $iv i32)
  (local $s i32)
  (block $end_literal (result i32)
  (block $string
  (block $integer
  (block $float
  (block $boolean
  (block $binary
  local.get $l
  br_table $binary $boolean $float $integer $string $string
)
  i32.const 0 ;; string: "[binary]"
  br $end_literal
)
  local.get $b
  i32.const 0 ;; string: "true"
  i32.const 0 ;; string: "false"
  call $hydra.lib.logic.if_else
  br $end_literal
)
  local.get $fv
  call $hydra.show.core.float
  br $end_literal
)
  local.get $iv
  call $hydra.show.core.integer
  br $end_literal
)
  local.get $s
  call $hydra.lib.literals.show_string
  br $end_literal
)
)
  (func $hydra.show.core.literal_type (param $lt i32) (result i32)
  (local $ft i32)
  (local $it i32)
  (block $end_literal_type (result i32)
  (block $string
  (block $integer
  (block $float
  (block $boolean
  (block $binary
  local.get $lt
  br_table $binary $boolean $float $integer $string $string
)
  i32.const 0 ;; string: "binary"
  br $end_literal_type
)
  i32.const 0 ;; string: "boolean"
  br $end_literal_type
)
  local.get $ft
  call $hydra.show.core.float_type
  br $end_literal_type
)
  local.get $it
  call $hydra.show.core.integer_type
  br $end_literal_type
)
  i32.const 0 ;; string: "string"
  br $end_literal_type
)
)
  (func $hydra.show.core.map (param $show_k i32) (param $show_v i32) (param $m i32) (result i32)
  (local $p i32)
  (local $pair_strs i32)
  i32.const 3
  ;; list elements follow
  local.get $p
  call $hydra.lib.pairs.first
  local.get $show_k
  i32.const 0 ;; string: ": "
  local.get $p
  call $hydra.lib.pairs.second
  local.get $show_v
  call $hydra.lib.strings.cat
  local.get $m
  call $hydra.lib.maps.to_list
  call $hydra.lib.lists.map
  local.set $pair_strs
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "{"
  i32.const 0 ;; string: ", "
  local.get $pair_strs
  call $hydra.lib.strings.intercalate
  i32.const 0 ;; string: "}"
  call $hydra.lib.strings.cat
)
  (func $hydra.show.core.maybe (param $f i32) (param $mx i32) (result i32)
  (local $x i32)
  i32.const 0 ;; string: "nothing"
  i32.const 0 ;; string: "just("
  local.get $x
  local.get $f
  i32.const 0 ;; string: ")"
  call $hydra.lib.strings.cat2
  call $hydra.lib.strings.cat2
  local.get $mx
  call $hydra.lib.maybes.maybe
)
  (func $hydra.show.core.pair (param $show_a i32) (param $show_b i32) (param $p i32) (result i32)
  i32.const 5
  ;; list elements follow
  i32.const 0 ;; string: "("
  local.get $p
  call $hydra.lib.pairs.first
  local.get $show_a
  i32.const 0 ;; string: ", "
  local.get $p
  call $hydra.lib.pairs.second
  local.get $show_b
  i32.const 0 ;; string: ")"
  call $hydra.lib.strings.cat
)
  (func $hydra.show.core.read_term (param $s i32) (result i32)
  local.get $s
)
  (func $hydra.show.core.set (param $f i32) (param $xs i32) (result i32)
  (local $element_strs i32)
  local.get $f
  local.get $xs
  call $hydra.lib.sets.to_list
  call $hydra.lib.lists.map
  local.set $element_strs
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "{"
  i32.const 0 ;; string: ", "
  local.get $element_strs
  call $hydra.lib.strings.intercalate
  i32.const 0 ;; string: "}"
  call $hydra.lib.strings.cat
)
  (func $hydra.show.core.term (param $t i32) (result i32)
  (local $app i32)
  (local $app2 i32)
  (local $at i32)
  (local $body i32)
  (local $e i32)
  (local $els i32)
  (local $entry i32)
  (local $flds i32)
  (local $gather_terms i32)
  (local $l i32)
  (local $lhs i32)
  (local $lit i32)
  (local $m i32)
  (local $mt i32)
  (local $p i32)
  (local $param i32)
  (local $prev i32)
  (local $r i32)
  (local $rec i32)
  (local $rhs i32)
  (local $s i32)
  (local $t2 i32)
  (local $ta i32)
  (local $term1 i32)
  (local $term_strs i32)
  (local $terms i32)
  (local $tname i32)
  (local $tt i32)
  (local $typ i32)
  (local $v i32)
  (local $wt i32)
  local.get $app
  ;; project field: function
  local.set $lhs
  local.get $app
  ;; project field: argument
  local.set $rhs
  (block $end_term (result i32)
  (block $application
  local.get $lhs
  br_table $application $application
)
  local.get $rhs
  local.get $prev
  call $hydra.lib.lists.cons
  local.get $app2
  local.get $gather_terms
  br $end_term
)
  local.set $gather_terms
  (block $end_term (result i32)
  (block $wrap
  (block $variable
  (block $unit
  (block $union
  (block $type_application
  (block $type_lambda
  (block $set
  (block $record
  (block $pair
  (block $maybe
  (block $map
  (block $literal
  (block $list
  (block $let
  (block $function
  (block $either
  (block $application
  (block $annotated
  local.get $t
  br_table $annotated $application $either $function $let $list $literal $map $maybe $pair $record $set $type_lambda $type_application $union $unit $variable $wrap $wrap
)
  local.get $at
  ;; project field: body
  call $hydra.show.core.term
  br $end_term
)
  i32.const 0
  ;; list elements follow
  local.get $app
  local.get $gather_terms
  local.set $terms
  call $hydra.show.core.term
  local.get $terms
  call $hydra.lib.lists.map
  local.set $term_strs
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "("
  i32.const 0 ;; string: " @ "
  local.get $term_strs
  call $hydra.lib.strings.intercalate
  i32.const 0 ;; string: ")"
  call $hydra.lib.strings.cat
  br $end_term
)
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "left("
  local.get $l
  call $hydra.show.core.term
  i32.const 0 ;; string: ")"
  call $hydra.lib.strings.cat
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "right("
  local.get $r
  call $hydra.show.core.term
  i32.const 0 ;; string: ")"
  call $hydra.lib.strings.cat
  local.get $e
  call $hydra.lib.eithers.either
  br $end_term
)
  local.get $v
  call $hydra.show.core.function
  br $end_term
)
  local.get $l
  call $hydra.show.core.let
  br $end_term
)
  call $hydra.show.core.term
  local.get $els
  call $hydra.lib.lists.map
  local.set $term_strs
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "["
  i32.const 0 ;; string: ", "
  local.get $term_strs
  call $hydra.lib.strings.intercalate
  i32.const 0 ;; string: "]"
  call $hydra.lib.strings.cat
  br $end_term
)
  local.get $lit
  call $hydra.show.core.literal
  br $end_term
)
  i32.const 3
  ;; list elements follow
  local.get $p
  call $hydra.lib.pairs.first
  call $hydra.show.core.term
  i32.const 0 ;; string: "="
  local.get $p
  call $hydra.lib.pairs.second
  call $hydra.show.core.term
  call $hydra.lib.strings.cat
  local.set $entry
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "{"
  i32.const 0 ;; string: ", "
  local.get $entry
  local.get $m
  call $hydra.lib.maps.to_list
  call $hydra.lib.lists.map
  call $hydra.lib.strings.intercalate
  i32.const 0 ;; string: "}"
  call $hydra.lib.strings.cat
  br $end_term
)
  i32.const 0 ;; string: "nothing"
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "just("
  local.get $t2
  call $hydra.show.core.term
  i32.const 0 ;; string: ")"
  call $hydra.lib.strings.cat
  local.get $mt
  call $hydra.lib.maybes.maybe
  br $end_term
)
  i32.const 5
  ;; list elements follow
  i32.const 0 ;; string: "("
  local.get $p
  call $hydra.lib.pairs.first
  call $hydra.show.core.term
  i32.const 0 ;; string: ", "
  local.get $p
  call $hydra.lib.pairs.second
  call $hydra.show.core.term
  i32.const 0 ;; string: ")"
  call $hydra.lib.strings.cat
  br $end_term
)
  nop
  local.set $tname
  local.get $rec
  ;; project field: fields
  local.set $flds
  i32.const 4
  ;; list elements follow
  i32.const 0 ;; string: "record("
  local.get $tname
  i32.const 0 ;; string: ")"
  local.get $flds
  call $hydra.show.core.fields
  call $hydra.lib.strings.cat
  br $end_term
)
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "{"
  i32.const 0 ;; string: ", "
  call $hydra.show.core.term
  local.get $s
  call $hydra.lib.sets.to_list
  call $hydra.lib.lists.map
  call $hydra.lib.strings.intercalate
  i32.const 0 ;; string: "}"
  call $hydra.lib.strings.cat
  br $end_term
)
  nop
  local.set $param
  local.get $ta
  ;; project field: body
  local.set $body
  i32.const 4
  ;; list elements follow
  i32.const 0 ;; string: "Λ"
  local.get $param
  i32.const 0 ;; string: "."
  local.get $body
  call $hydra.show.core.term
  call $hydra.lib.strings.cat
  br $end_term
)
  local.get $tt
  ;; project field: body
  local.set $t2
  local.get $tt
  ;; project field: type
  local.set $typ
  i32.const 4
  ;; list elements follow
  local.get $t2
  call $hydra.show.core.term
  i32.const 0 ;; string: "⟨"
  local.get $typ
  call $hydra.show.core.type
  i32.const 0 ;; string: "⟩"
  call $hydra.lib.strings.cat
  br $end_term
)
  local.get $v
  call $hydra.show.core.injection
  br $end_term
)
  i32.const 0 ;; string: "unit"
  br $end_term
)
  nop
  br $end_term
)
  nop
  local.set $tname
  local.get $wt
  ;; project field: body
  local.set $term1
  i32.const 5
  ;; list elements follow
  i32.const 0 ;; string: "wrap("
  local.get $tname
  i32.const 0 ;; string: "){"
  local.get $term1
  call $hydra.show.core.term
  i32.const 0 ;; string: "}"
  call $hydra.lib.strings.cat
  br $end_term
)
)
  (func $hydra.show.core.type (param $typ i32) (result i32)
  (local $app i32)
  (local $app2 i32)
  (local $at i32)
  (local $body i32)
  (local $cod i32)
  (local $dom i32)
  (local $et i32)
  (local $etyp i32)
  (local $field_strs i32)
  (local $first_typ i32)
  (local $flds i32)
  (local $ft i32)
  (local $gather_function_types i32)
  (local $gather_types i32)
  (local $key_typ i32)
  (local $left_typ i32)
  (local $lhs i32)
  (local $lt i32)
  (local $mt i32)
  (local $prev i32)
  (local $pt i32)
  (local $rhs i32)
  (local $right_typ i32)
  (local $rt i32)
  (local $second_typ i32)
  (local $show_row_type i32)
  (local $t i32)
  (local $type_strs i32)
  (local $types i32)
  (local $val_typ i32)
  (local $var i32)
  (local $wt i32)
  call $hydra.show.core.field_type
  local.get $flds
  call $hydra.lib.lists.map
  local.set $field_strs
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "{"
  i32.const 0 ;; string: ", "
  local.get $field_strs
  call $hydra.lib.strings.intercalate
  i32.const 0 ;; string: "}"
  call $hydra.lib.strings.cat
  local.set $show_row_type
  local.get $app
  ;; project field: function
  local.set $lhs
  local.get $app
  ;; project field: argument
  local.set $rhs
  (block $end_type (result i32)
  (block $application
  local.get $lhs
  br_table $application $application
)
  local.get $rhs
  local.get $prev
  call $hydra.lib.lists.cons
  local.get $app2
  local.get $gather_types
  br $end_type
)
  local.set $gather_types
  (block $end_type (result i32)
  (block $function
  local.get $t
  br_table $function $function
)
  local.get $ft
  ;; project field: domain
  local.set $dom
  local.get $ft
  ;; project field: codomain
  local.set $cod
  local.get $dom
  local.get $prev
  call $hydra.lib.lists.cons
  local.get $cod
  local.get $gather_function_types
  br $end_type
)
  local.set $gather_function_types
  (block $end_type (result i32)
  (block $wrap
  (block $void
  (block $variable
  (block $unit
  (block $union
  (block $set
  (block $record
  (block $pair
  (block $maybe
  (block $map
  (block $literal
  (block $list
  (block $function
  (block $forall
  (block $either
  (block $application
  (block $annotated
  local.get $typ
  br_table $annotated $application $either $forall $function $list $literal $map $maybe $pair $record $set $union $unit $variable $void $wrap $wrap
)
  local.get $at
  ;; project field: body
  call $hydra.show.core.type
  br $end_type
)
  i32.const 0
  ;; list elements follow
  local.get $app
  local.get $gather_types
  local.set $types
  call $hydra.show.core.type
  local.get $types
  call $hydra.lib.lists.map
  local.set $type_strs
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "("
  i32.const 0 ;; string: " @ "
  local.get $type_strs
  call $hydra.lib.strings.intercalate
  i32.const 0 ;; string: ")"
  call $hydra.lib.strings.cat
  br $end_type
)
  local.get $et
  ;; project field: left
  local.set $left_typ
  local.get $et
  ;; project field: right
  local.set $right_typ
  i32.const 5
  ;; list elements follow
  i32.const 0 ;; string: "either<"
  local.get $left_typ
  call $hydra.show.core.type
  i32.const 0 ;; string: ", "
  local.get $right_typ
  call $hydra.show.core.type
  i32.const 0 ;; string: ">"
  call $hydra.lib.strings.cat
  br $end_type
)
  nop
  local.set $var
  local.get $ft
  ;; project field: body
  local.set $body
  i32.const 5
  ;; list elements follow
  i32.const 0 ;; string: "(∀"
  local.get $var
  i32.const 0 ;; string: "."
  local.get $body
  call $hydra.show.core.type
  i32.const 0 ;; string: ")"
  call $hydra.lib.strings.cat
  br $end_type
)
  i32.const 0
  ;; list elements follow
  local.get $typ
  local.get $gather_function_types
  local.set $types
  call $hydra.show.core.type
  local.get $types
  call $hydra.lib.lists.map
  local.set $type_strs
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "("
  i32.const 0 ;; string: " → "
  local.get $type_strs
  call $hydra.lib.strings.intercalate
  i32.const 0 ;; string: ")"
  call $hydra.lib.strings.cat
  br $end_type
)
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "list<"
  local.get $etyp
  call $hydra.show.core.type
  i32.const 0 ;; string: ">"
  call $hydra.lib.strings.cat
  br $end_type
)
  local.get $lt
  call $hydra.show.core.literal_type
  br $end_type
)
  local.get $mt
  ;; project field: keys
  local.set $key_typ
  local.get $mt
  ;; project field: values
  local.set $val_typ
  i32.const 5
  ;; list elements follow
  i32.const 0 ;; string: "map<"
  local.get $key_typ
  call $hydra.show.core.type
  i32.const 0 ;; string: ", "
  local.get $val_typ
  call $hydra.show.core.type
  i32.const 0 ;; string: ">"
  call $hydra.lib.strings.cat
  br $end_type
)
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "maybe<"
  local.get $etyp
  call $hydra.show.core.type
  i32.const 0 ;; string: ">"
  call $hydra.lib.strings.cat
  br $end_type
)
  local.get $pt
  ;; project field: first
  local.set $first_typ
  local.get $pt
  ;; project field: second
  local.set $second_typ
  i32.const 5
  ;; list elements follow
  i32.const 0 ;; string: "("
  local.get $first_typ
  call $hydra.show.core.type
  i32.const 0 ;; string: ", "
  local.get $second_typ
  call $hydra.show.core.type
  i32.const 0 ;; string: ")"
  call $hydra.lib.strings.cat
  br $end_type
)
  i32.const 0 ;; string: "record"
  local.get $rt
  local.get $show_row_type
  call $hydra.lib.strings.cat2
  br $end_type
)
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "set<"
  local.get $etyp
  call $hydra.show.core.type
  i32.const 0 ;; string: ">"
  call $hydra.lib.strings.cat
  br $end_type
)
  i32.const 0 ;; string: "union"
  local.get $rt
  local.get $show_row_type
  call $hydra.lib.strings.cat2
  br $end_type
)
  i32.const 0 ;; string: "unit"
  br $end_type
)
  nop
  br $end_type
)
  i32.const 0 ;; string: "void"
  br $end_type
)
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "wrap("
  local.get $wt
  call $hydra.show.core.type
  i32.const 0 ;; string: ")"
  call $hydra.lib.strings.cat
  br $end_type
)
)
  (func $hydra.show.core.type_scheme (param $ts i32) (result i32)
  (local $body i32)
  (local $fa i32)
  (local $m i32)
  (local $p i32)
  (local $tc i32)
  (local $to_constraint_pair i32)
  (local $to_constraint_pairs i32)
  (local $var_names i32)
  (local $vars i32)
  local.get $ts
  ;; project field: variables
  local.set $vars
  local.get $ts
  ;; project field: type
  local.set $body
  nop
  local.get $vars
  call $hydra.lib.lists.map
  local.set $var_names
  local.get $vars
  call $hydra.lib.lists.null
  i32.const 0 ;; string: ""
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "forall "
  i32.const 0 ;; string: ","
  local.get $var_names
  call $hydra.lib.strings.intercalate
  i32.const 0 ;; string: ". "
  call $hydra.lib.strings.cat
  call $hydra.lib.logic.if_else
  local.set $fa
  i32.const 3
  ;; list elements follow
  nop
  i32.const 0 ;; string: " "
  nop
  call $hydra.lib.strings.cat
  local.set $to_constraint_pair
  local.get $p
  call $hydra.lib.pairs.first
  local.get $to_constraint_pair
  local.get $p
  call $hydra.lib.pairs.second
  ;; project field: classes
  call $hydra.lib.sets.to_list
  call $hydra.lib.lists.map
  local.set $to_constraint_pairs
  i32.const 0
  ;; list elements follow
  local.get $to_constraint_pairs
  local.get $m
  call $hydra.lib.maps.to_list
  call $hydra.lib.lists.map
  call $hydra.lib.lists.concat
  local.get $ts
  ;; project field: constraints
  call $hydra.lib.maybes.maybe
  local.set $tc
  i32.const 5
  ;; list elements follow
  i32.const 0 ;; string: "("
  local.get $fa
  local.get $tc
  call $hydra.lib.lists.null
  i32.const 0 ;; string: ""
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "("
  i32.const 0 ;; string: ", "
  local.get $tc
  call $hydra.lib.strings.intercalate
  i32.const 0 ;; string: ") => "
  call $hydra.lib.strings.cat
  call $hydra.lib.logic.if_else
  local.get $body
  call $hydra.show.core.type
  i32.const 0 ;; string: ")"
  call $hydra.lib.strings.cat
)
)
