(module
  (import "hydra.lib.eithers" "hydra.lib.eithers.bimap" (func $hydra.lib.eithers.bimap (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.bimap" (func $hydra.lib.maps.bimap (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.map" (func $hydra.lib.maybes.map (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.bimap" (func $hydra.lib.pairs.bimap (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.map" (func $hydra.lib.sets.map (param i32) (result i32) ) )
  (memory $memory 1 )
  (export "memory" (memory $memory) )
  (export "hydra.encode.core.annotated_term" (func $hydra.encode.core.annotated_term) )
  (export "hydra.encode.core.annotated_type" (func $hydra.encode.core.annotated_type) )
  (export "hydra.encode.core.application" (func $hydra.encode.core.application) )
  (export "hydra.encode.core.application_type" (func $hydra.encode.core.application_type) )
  (export "hydra.encode.core.binding" (func $hydra.encode.core.binding) )
  (export "hydra.encode.core.case_statement" (func $hydra.encode.core.case_statement) )
  (export "hydra.encode.core.either_type" (func $hydra.encode.core.either_type) )
  (export "hydra.encode.core.elimination" (func $hydra.encode.core.elimination) )
  (export "hydra.encode.core.field" (func $hydra.encode.core.field) )
  (export "hydra.encode.core.field_type" (func $hydra.encode.core.field_type) )
  (export "hydra.encode.core.float_type" (func $hydra.encode.core.float_type) )
  (export "hydra.encode.core.float_value" (func $hydra.encode.core.float_value) )
  (export "hydra.encode.core.forall_type" (func $hydra.encode.core.forall_type) )
  (export "hydra.encode.core.function" (func $hydra.encode.core.function) )
  (export "hydra.encode.core.function_type" (func $hydra.encode.core.function_type) )
  (export "hydra.encode.core.injection" (func $hydra.encode.core.injection) )
  (export "hydra.encode.core.integer_type" (func $hydra.encode.core.integer_type) )
  (export "hydra.encode.core.integer_value" (func $hydra.encode.core.integer_value) )
  (export "hydra.encode.core.lambda" (func $hydra.encode.core.lambda) )
  (export "hydra.encode.core.let" (func $hydra.encode.core.let) )
  (export "hydra.encode.core.literal" (func $hydra.encode.core.literal) )
  (export "hydra.encode.core.literal_type" (func $hydra.encode.core.literal_type) )
  (export "hydra.encode.core.map_type" (func $hydra.encode.core.map_type) )
  (export "hydra.encode.core.name" (func $hydra.encode.core.name) )
  (export "hydra.encode.core.pair_type" (func $hydra.encode.core.pair_type) )
  (export "hydra.encode.core.projection" (func $hydra.encode.core.projection) )
  (export "hydra.encode.core.record" (func $hydra.encode.core.record) )
  (export "hydra.encode.core.term" (func $hydra.encode.core.term) )
  (export "hydra.encode.core.type" (func $hydra.encode.core.type) )
  (export "hydra.encode.core.type_application_term" (func $hydra.encode.core.type_application_term) )
  (export "hydra.encode.core.type_lambda" (func $hydra.encode.core.type_lambda) )
  (export "hydra.encode.core.type_scheme" (func $hydra.encode.core.type_scheme) )
  (export "hydra.encode.core.type_variable_metadata" (func $hydra.encode.core.type_variable_metadata) )
  (export "hydra.encode.core.wrapped_term" (func $hydra.encode.core.wrapped_term) )
  (func $hydra.encode.core.annotated_term (param $x i32) (result i32)
  (local $m i32)
  i32.const 0 ;; string: "hydra.core.AnnotatedTerm"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "body"
  local.get $x
  ;; project field: body
  call $hydra.encode.core.term
  i32.const 0 ;; string: "annotation"
  call $hydra.encode.core.name
  call $hydra.encode.core.term
  local.get $m
  call $hydra.lib.maps.bimap
)
  (func $hydra.encode.core.annotated_type (param $x i32) (result i32)
  (local $m i32)
  i32.const 0 ;; string: "hydra.core.AnnotatedType"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "body"
  local.get $x
  ;; project field: body
  call $hydra.encode.core.type
  i32.const 0 ;; string: "annotation"
  call $hydra.encode.core.name
  call $hydra.encode.core.term
  local.get $m
  call $hydra.lib.maps.bimap
)
  (func $hydra.encode.core.application (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.core.Application"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "function"
  local.get $x
  ;; project field: function
  call $hydra.encode.core.term
  i32.const 0 ;; string: "argument"
  local.get $x
  ;; project field: argument
  call $hydra.encode.core.term
)
  (func $hydra.encode.core.application_type (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.core.ApplicationType"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "function"
  local.get $x
  ;; project field: function
  call $hydra.encode.core.type
  i32.const 0 ;; string: "argument"
  local.get $x
  ;; project field: argument
  call $hydra.encode.core.type
)
  (func $hydra.encode.core.binding (param $x i32) (result i32)
  (local $opt i32)
  i32.const 0 ;; string: "hydra.core.Binding"
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "name"
  local.get $x
  ;; project field: name
  call $hydra.encode.core.name
  i32.const 0 ;; string: "term"
  local.get $x
  ;; project field: term
  call $hydra.encode.core.term
  i32.const 0 ;; string: "type"
  call $hydra.encode.core.type_scheme
  local.get $opt
  call $hydra.lib.maybes.map
)
  (func $hydra.encode.core.case_statement (param $x i32) (result i32)
  (local $opt i32)
  (local $xs i32)
  i32.const 0 ;; string: "hydra.core.CaseStatement"
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "typeName"
  local.get $x
  ;; project field: type_name
  call $hydra.encode.core.name
  i32.const 0 ;; string: "default"
  call $hydra.encode.core.term
  local.get $opt
  call $hydra.lib.maybes.map
  i32.const 0 ;; string: "cases"
  call $hydra.encode.core.field
  local.get $xs
  call $hydra.lib.lists.map
)
  (func $hydra.encode.core.either_type (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.core.EitherType"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "left"
  local.get $x
  ;; project field: left
  call $hydra.encode.core.type
  i32.const 0 ;; string: "right"
  local.get $x
  ;; project field: right
  call $hydra.encode.core.type
)
  (func $hydra.encode.core.elimination (param $arg_0 i32) (result i32)
  (local $y i32)
  (block $end_elimination (result i32)
  (block $wrap
  (block $union
  (block $record
  local.get $arg_0
  br_table $record $union $wrap $wrap
)
  i32.const 0 ;; string: "hydra.core.Elimination"
  i32.const 0 ;; string: "record"
  local.get $y
  call $hydra.encode.core.projection
  br $end_elimination
)
  i32.const 0 ;; string: "hydra.core.Elimination"
  i32.const 0 ;; string: "union"
  local.get $y
  call $hydra.encode.core.case_statement
  br $end_elimination
)
  i32.const 0 ;; string: "hydra.core.Elimination"
  i32.const 0 ;; string: "wrap"
  local.get $y
  call $hydra.encode.core.name
  br $end_elimination
)
)
  (func $hydra.encode.core.field (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.core.Field"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "name"
  local.get $x
  ;; project field: name
  call $hydra.encode.core.name
  i32.const 0 ;; string: "term"
  local.get $x
  ;; project field: term
  call $hydra.encode.core.term
)
  (func $hydra.encode.core.field_type (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.core.FieldType"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "name"
  local.get $x
  ;; project field: name
  call $hydra.encode.core.name
  i32.const 0 ;; string: "type"
  local.get $x
  ;; project field: type
  call $hydra.encode.core.type
)
  (func $hydra.encode.core.float_type (param $arg_0 i32) (result i32)
  (block $end_float_type (result i32)
  (block $float64
  (block $float32
  (block $bigfloat
  local.get $arg_0
  br_table $bigfloat $float32 $float64 $float64
)
  i32.const 0 ;; string: "hydra.core.FloatType"
  i32.const 0 ;; string: "bigfloat"
  i32.const 0
  br $end_float_type
)
  i32.const 0 ;; string: "hydra.core.FloatType"
  i32.const 0 ;; string: "float32"
  i32.const 0
  br $end_float_type
)
  i32.const 0 ;; string: "hydra.core.FloatType"
  i32.const 0 ;; string: "float64"
  i32.const 0
  br $end_float_type
)
)
  (func $hydra.encode.core.float_value (param $arg_0 i32) (result i32)
  (local $x i32)
  (block $end_float_value (result i32)
  (block $float64
  (block $float32
  (block $bigfloat
  local.get $arg_0
  br_table $bigfloat $float32 $float64 $float64
)
  i32.const 0 ;; string: "hydra.core.FloatValue"
  i32.const 0 ;; string: "bigfloat"
  local.get $x
  br $end_float_value
)
  i32.const 0 ;; string: "hydra.core.FloatValue"
  i32.const 0 ;; string: "float32"
  local.get $x
  br $end_float_value
)
  i32.const 0 ;; string: "hydra.core.FloatValue"
  i32.const 0 ;; string: "float64"
  local.get $x
  br $end_float_value
)
)
  (func $hydra.encode.core.forall_type (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.core.ForallType"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "parameter"
  local.get $x
  ;; project field: parameter
  call $hydra.encode.core.name
  i32.const 0 ;; string: "body"
  local.get $x
  ;; project field: body
  call $hydra.encode.core.type
)
  (func $hydra.encode.core.function (param $arg_0 i32) (result i32)
  (local $y i32)
  (block $end_function (result i32)
  (block $lambda
  (block $elimination
  local.get $arg_0
  br_table $elimination $lambda $lambda
)
  i32.const 0 ;; string: "hydra.core.Function"
  i32.const 0 ;; string: "elimination"
  local.get $y
  call $hydra.encode.core.elimination
  br $end_function
)
  i32.const 0 ;; string: "hydra.core.Function"
  i32.const 0 ;; string: "lambda"
  local.get $y
  call $hydra.encode.core.lambda
  br $end_function
)
)
  (func $hydra.encode.core.function_type (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.core.FunctionType"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "domain"
  local.get $x
  ;; project field: domain
  call $hydra.encode.core.type
  i32.const 0 ;; string: "codomain"
  local.get $x
  ;; project field: codomain
  call $hydra.encode.core.type
)
  (func $hydra.encode.core.injection (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.core.Injection"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "typeName"
  local.get $x
  ;; project field: type_name
  call $hydra.encode.core.name
  i32.const 0 ;; string: "field"
  local.get $x
  ;; project field: field
  call $hydra.encode.core.field
)
  (func $hydra.encode.core.integer_type (param $arg_0 i32) (result i32)
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
  local.get $arg_0
  br_table $bigint $int8 $int16 $int32 $int64 $uint8 $uint16 $uint32 $uint64 $uint64
)
  i32.const 0 ;; string: "hydra.core.IntegerType"
  i32.const 0 ;; string: "bigint"
  i32.const 0
  br $end_integer_type
)
  i32.const 0 ;; string: "hydra.core.IntegerType"
  i32.const 0 ;; string: "int8"
  i32.const 0
  br $end_integer_type
)
  i32.const 0 ;; string: "hydra.core.IntegerType"
  i32.const 0 ;; string: "int16"
  i32.const 0
  br $end_integer_type
)
  i32.const 0 ;; string: "hydra.core.IntegerType"
  i32.const 0 ;; string: "int32"
  i32.const 0
  br $end_integer_type
)
  i32.const 0 ;; string: "hydra.core.IntegerType"
  i32.const 0 ;; string: "int64"
  i32.const 0
  br $end_integer_type
)
  i32.const 0 ;; string: "hydra.core.IntegerType"
  i32.const 0 ;; string: "uint8"
  i32.const 0
  br $end_integer_type
)
  i32.const 0 ;; string: "hydra.core.IntegerType"
  i32.const 0 ;; string: "uint16"
  i32.const 0
  br $end_integer_type
)
  i32.const 0 ;; string: "hydra.core.IntegerType"
  i32.const 0 ;; string: "uint32"
  i32.const 0
  br $end_integer_type
)
  i32.const 0 ;; string: "hydra.core.IntegerType"
  i32.const 0 ;; string: "uint64"
  i32.const 0
  br $end_integer_type
)
)
  (func $hydra.encode.core.integer_value (param $arg_0 i32) (result i32)
  (local $x i32)
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
  local.get $arg_0
  br_table $bigint $int8 $int16 $int32 $int64 $uint8 $uint16 $uint32 $uint64 $uint64
)
  i32.const 0 ;; string: "hydra.core.IntegerValue"
  i32.const 0 ;; string: "bigint"
  local.get $x
  br $end_integer_value
)
  i32.const 0 ;; string: "hydra.core.IntegerValue"
  i32.const 0 ;; string: "int8"
  local.get $x
  br $end_integer_value
)
  i32.const 0 ;; string: "hydra.core.IntegerValue"
  i32.const 0 ;; string: "int16"
  local.get $x
  br $end_integer_value
)
  i32.const 0 ;; string: "hydra.core.IntegerValue"
  i32.const 0 ;; string: "int32"
  local.get $x
  br $end_integer_value
)
  i32.const 0 ;; string: "hydra.core.IntegerValue"
  i32.const 0 ;; string: "int64"
  local.get $x
  br $end_integer_value
)
  i32.const 0 ;; string: "hydra.core.IntegerValue"
  i32.const 0 ;; string: "uint8"
  local.get $x
  br $end_integer_value
)
  i32.const 0 ;; string: "hydra.core.IntegerValue"
  i32.const 0 ;; string: "uint16"
  local.get $x
  br $end_integer_value
)
  i32.const 0 ;; string: "hydra.core.IntegerValue"
  i32.const 0 ;; string: "uint32"
  local.get $x
  br $end_integer_value
)
  i32.const 0 ;; string: "hydra.core.IntegerValue"
  i32.const 0 ;; string: "uint64"
  local.get $x
  br $end_integer_value
)
)
  (func $hydra.encode.core.lambda (param $x i32) (result i32)
  (local $opt i32)
  i32.const 0 ;; string: "hydra.core.Lambda"
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "parameter"
  local.get $x
  ;; project field: parameter
  call $hydra.encode.core.name
  i32.const 0 ;; string: "domain"
  call $hydra.encode.core.type
  local.get $opt
  call $hydra.lib.maybes.map
  i32.const 0 ;; string: "body"
  local.get $x
  ;; project field: body
  call $hydra.encode.core.term
)
  (func $hydra.encode.core.let (param $x i32) (result i32)
  (local $xs i32)
  i32.const 0 ;; string: "hydra.core.Let"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "bindings"
  call $hydra.encode.core.binding
  local.get $xs
  call $hydra.lib.lists.map
  i32.const 0 ;; string: "body"
  local.get $x
  ;; project field: body
  call $hydra.encode.core.term
)
  (func $hydra.encode.core.literal (param $arg_0 i32) (result i32)
  (local $x i32)
  (local $y i32)
  (block $end_literal (result i32)
  (block $string
  (block $integer
  (block $float
  (block $boolean
  (block $binary
  local.get $arg_0
  br_table $binary $boolean $float $integer $string $string
)
  i32.const 0 ;; string: "hydra.core.Literal"
  i32.const 0 ;; string: "binary"
  local.get $x
  br $end_literal
)
  i32.const 0 ;; string: "hydra.core.Literal"
  i32.const 0 ;; string: "boolean"
  local.get $x
  br $end_literal
)
  i32.const 0 ;; string: "hydra.core.Literal"
  i32.const 0 ;; string: "float"
  local.get $y
  call $hydra.encode.core.float_value
  br $end_literal
)
  i32.const 0 ;; string: "hydra.core.Literal"
  i32.const 0 ;; string: "integer"
  local.get $y
  call $hydra.encode.core.integer_value
  br $end_literal
)
  i32.const 0 ;; string: "hydra.core.Literal"
  i32.const 0 ;; string: "string"
  local.get $x
  br $end_literal
)
)
  (func $hydra.encode.core.literal_type (param $arg_0 i32) (result i32)
  (local $y i32)
  (block $end_literal_type (result i32)
  (block $string
  (block $integer
  (block $float
  (block $boolean
  (block $binary
  local.get $arg_0
  br_table $binary $boolean $float $integer $string $string
)
  i32.const 0 ;; string: "hydra.core.LiteralType"
  i32.const 0 ;; string: "binary"
  i32.const 0
  br $end_literal_type
)
  i32.const 0 ;; string: "hydra.core.LiteralType"
  i32.const 0 ;; string: "boolean"
  i32.const 0
  br $end_literal_type
)
  i32.const 0 ;; string: "hydra.core.LiteralType"
  i32.const 0 ;; string: "float"
  local.get $y
  call $hydra.encode.core.float_type
  br $end_literal_type
)
  i32.const 0 ;; string: "hydra.core.LiteralType"
  i32.const 0 ;; string: "integer"
  local.get $y
  call $hydra.encode.core.integer_type
  br $end_literal_type
)
  i32.const 0 ;; string: "hydra.core.LiteralType"
  i32.const 0 ;; string: "string"
  i32.const 0
  br $end_literal_type
)
)
  (func $hydra.encode.core.map_type (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.core.MapType"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "keys"
  local.get $x
  ;; project field: keys
  call $hydra.encode.core.type
  i32.const 0 ;; string: "values"
  local.get $x
  ;; project field: values
  call $hydra.encode.core.type
)
  (func $hydra.encode.core.name (param $x i32) (result i32)
  (local $x2 i32)
  i32.const 0 ;; string: "hydra.core.Name"
  local.get $x2
)
  (func $hydra.encode.core.pair_type (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.core.PairType"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "first"
  local.get $x
  ;; project field: first
  call $hydra.encode.core.type
  i32.const 0 ;; string: "second"
  local.get $x
  ;; project field: second
  call $hydra.encode.core.type
)
  (func $hydra.encode.core.projection (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.core.Projection"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "typeName"
  local.get $x
  ;; project field: type_name
  call $hydra.encode.core.name
  i32.const 0 ;; string: "field"
  local.get $x
  ;; project field: field
  call $hydra.encode.core.name
)
  (func $hydra.encode.core.record (param $x i32) (result i32)
  (local $xs i32)
  i32.const 0 ;; string: "hydra.core.Record"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "typeName"
  local.get $x
  ;; project field: type_name
  call $hydra.encode.core.name
  i32.const 0 ;; string: "fields"
  call $hydra.encode.core.field
  local.get $xs
  call $hydra.lib.lists.map
)
  (func $hydra.encode.core.term (param $arg_0 i32) (result i32)
  (local $e i32)
  (local $m i32)
  (local $opt i32)
  (local $p i32)
  (local $s i32)
  (local $xs i32)
  (local $y i32)
  (block $end_term (result i32)
  (block $wrap
  (block $variable
  (block $unit
  (block $union
  (block $type_lambda
  (block $type_application
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
  local.get $arg_0
  br_table $annotated $application $either $function $let $list $literal $map $maybe $pair $record $set $type_application $type_lambda $union $unit $variable $wrap $wrap
)
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 0 ;; string: "annotated"
  local.get $y
  call $hydra.encode.core.annotated_term
  br $end_term
)
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 0 ;; string: "application"
  local.get $y
  call $hydra.encode.core.application
  br $end_term
)
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 0 ;; string: "either"
  call $hydra.encode.core.term
  call $hydra.encode.core.term
  local.get $e
  call $hydra.lib.eithers.bimap
  br $end_term
)
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 0 ;; string: "function"
  local.get $y
  call $hydra.encode.core.function
  br $end_term
)
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 0 ;; string: "let"
  local.get $y
  call $hydra.encode.core.let
  br $end_term
)
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 0 ;; string: "list"
  call $hydra.encode.core.term
  local.get $xs
  call $hydra.lib.lists.map
  br $end_term
)
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 0 ;; string: "literal"
  local.get $y
  call $hydra.encode.core.literal
  br $end_term
)
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 0 ;; string: "map"
  call $hydra.encode.core.term
  call $hydra.encode.core.term
  local.get $m
  call $hydra.lib.maps.bimap
  br $end_term
)
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 0 ;; string: "maybe"
  call $hydra.encode.core.term
  local.get $opt
  call $hydra.lib.maybes.map
  br $end_term
)
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 0 ;; string: "pair"
  call $hydra.encode.core.term
  call $hydra.encode.core.term
  local.get $p
  call $hydra.lib.pairs.bimap
  br $end_term
)
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 0 ;; string: "record"
  local.get $y
  call $hydra.encode.core.record
  br $end_term
)
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 0 ;; string: "set"
  call $hydra.encode.core.term
  local.get $s
  call $hydra.lib.sets.map
  br $end_term
)
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 0 ;; string: "typeApplication"
  local.get $y
  call $hydra.encode.core.type_application_term
  br $end_term
)
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 0 ;; string: "typeLambda"
  local.get $y
  call $hydra.encode.core.type_lambda
  br $end_term
)
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 0 ;; string: "union"
  local.get $y
  call $hydra.encode.core.injection
  br $end_term
)
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 0 ;; string: "unit"
  i32.const 0
  br $end_term
)
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 0 ;; string: "variable"
  local.get $y
  call $hydra.encode.core.name
  br $end_term
)
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 0 ;; string: "wrap"
  local.get $y
  call $hydra.encode.core.wrapped_term
  br $end_term
)
)
  (func $hydra.encode.core.type (param $arg_0 i32) (result i32)
  (local $xs i32)
  (local $y i32)
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
  local.get $arg_0
  br_table $annotated $application $either $forall $function $list $literal $map $maybe $pair $record $set $union $unit $variable $void $wrap $wrap
)
  i32.const 0 ;; string: "hydra.core.Type"
  i32.const 0 ;; string: "annotated"
  local.get $y
  call $hydra.encode.core.annotated_type
  br $end_type
)
  i32.const 0 ;; string: "hydra.core.Type"
  i32.const 0 ;; string: "application"
  local.get $y
  call $hydra.encode.core.application_type
  br $end_type
)
  i32.const 0 ;; string: "hydra.core.Type"
  i32.const 0 ;; string: "either"
  local.get $y
  call $hydra.encode.core.either_type
  br $end_type
)
  i32.const 0 ;; string: "hydra.core.Type"
  i32.const 0 ;; string: "forall"
  local.get $y
  call $hydra.encode.core.forall_type
  br $end_type
)
  i32.const 0 ;; string: "hydra.core.Type"
  i32.const 0 ;; string: "function"
  local.get $y
  call $hydra.encode.core.function_type
  br $end_type
)
  i32.const 0 ;; string: "hydra.core.Type"
  i32.const 0 ;; string: "list"
  local.get $y
  call $hydra.encode.core.type
  br $end_type
)
  i32.const 0 ;; string: "hydra.core.Type"
  i32.const 0 ;; string: "literal"
  local.get $y
  call $hydra.encode.core.literal_type
  br $end_type
)
  i32.const 0 ;; string: "hydra.core.Type"
  i32.const 0 ;; string: "map"
  local.get $y
  call $hydra.encode.core.map_type
  br $end_type
)
  i32.const 0 ;; string: "hydra.core.Type"
  i32.const 0 ;; string: "maybe"
  local.get $y
  call $hydra.encode.core.type
  br $end_type
)
  i32.const 0 ;; string: "hydra.core.Type"
  i32.const 0 ;; string: "pair"
  local.get $y
  call $hydra.encode.core.pair_type
  br $end_type
)
  i32.const 0 ;; string: "hydra.core.Type"
  i32.const 0 ;; string: "record"
  call $hydra.encode.core.field_type
  local.get $xs
  call $hydra.lib.lists.map
  br $end_type
)
  i32.const 0 ;; string: "hydra.core.Type"
  i32.const 0 ;; string: "set"
  local.get $y
  call $hydra.encode.core.type
  br $end_type
)
  i32.const 0 ;; string: "hydra.core.Type"
  i32.const 0 ;; string: "union"
  call $hydra.encode.core.field_type
  local.get $xs
  call $hydra.lib.lists.map
  br $end_type
)
  i32.const 0 ;; string: "hydra.core.Type"
  i32.const 0 ;; string: "unit"
  i32.const 0
  br $end_type
)
  i32.const 0 ;; string: "hydra.core.Type"
  i32.const 0 ;; string: "variable"
  local.get $y
  call $hydra.encode.core.name
  br $end_type
)
  i32.const 0 ;; string: "hydra.core.Type"
  i32.const 0 ;; string: "void"
  i32.const 0
  br $end_type
)
  i32.const 0 ;; string: "hydra.core.Type"
  i32.const 0 ;; string: "wrap"
  local.get $y
  call $hydra.encode.core.type
  br $end_type
)
)
  (func $hydra.encode.core.type_application_term (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.core.TypeApplicationTerm"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "body"
  local.get $x
  ;; project field: body
  call $hydra.encode.core.term
  i32.const 0 ;; string: "type"
  local.get $x
  ;; project field: type
  call $hydra.encode.core.type
)
  (func $hydra.encode.core.type_lambda (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.core.TypeLambda"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "parameter"
  local.get $x
  ;; project field: parameter
  call $hydra.encode.core.name
  i32.const 0 ;; string: "body"
  local.get $x
  ;; project field: body
  call $hydra.encode.core.term
)
  (func $hydra.encode.core.type_scheme (param $x i32) (result i32)
  (local $m i32)
  (local $opt i32)
  (local $xs i32)
  i32.const 0 ;; string: "hydra.core.TypeScheme"
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "variables"
  call $hydra.encode.core.name
  local.get $xs
  call $hydra.lib.lists.map
  i32.const 0 ;; string: "type"
  local.get $x
  ;; project field: type
  call $hydra.encode.core.type
  i32.const 0 ;; string: "constraints"
  call $hydra.encode.core.name
  call $hydra.encode.core.type_variable_metadata
  local.get $m
  call $hydra.lib.maps.bimap
  local.get $opt
  call $hydra.lib.maybes.map
)
  (func $hydra.encode.core.type_variable_metadata (param $x i32) (result i32)
  (local $s i32)
  i32.const 0 ;; string: "hydra.core.TypeVariableMetadata"
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "classes"
  call $hydra.encode.core.name
  local.get $s
  call $hydra.lib.sets.map
)
  (func $hydra.encode.core.wrapped_term (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.core.WrappedTerm"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "typeName"
  local.get $x
  ;; project field: type_name
  call $hydra.encode.core.name
  i32.const 0 ;; string: "body"
  local.get $x
  ;; project field: body
  call $hydra.encode.core.term
)
)
