(module
  (import "hydra.annotations" "hydra.annotations.is_native_type" (func $hydra.annotations.is_native_type (param i32) (result i32) ) )
  (import "hydra.annotations" "hydra.annotations.normalize_term_annotations" (func $hydra.annotations.normalize_term_annotations (param i32) (result i32) ) )
  (import "hydra.constants" "hydra.constants.key_type" (func $hydra.constants.key_type (param i32) (result i32) ) )
  (import "hydra.decode.core" "hydra.decode.core.type" (func $hydra.decode.core.type (param i32) (result i32) ) )
  (import "hydra.encode.core" "hydra.encode.core.type" (func $hydra.encode.core.type (param i32) (result i32) ) )
  (import "hydra.formatting" "hydra.formatting.decapitalize" (func $hydra.formatting.decapitalize (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.bimap" (func $hydra.lib.eithers.bimap (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.bind" (func $hydra.lib.eithers.bind (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map" (func $hydra.lib.eithers.map (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map_list" (func $hydra.lib.eithers.map_list (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.concat" (func $hydra.lib.lists.concat (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.concat2" (func $hydra.lib.lists.concat2 (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.cons" (func $hydra.lib.lists.cons (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.elem" (func $hydra.lib.lists.elem (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.filter" (func $hydra.lib.lists.filter (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.init" (func $hydra.lib.lists.init (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.nub" (func $hydra.lib.lists.nub (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.null" (func $hydra.lib.lists.null (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.tail" (func $hydra.lib.lists.tail (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.if_else" (func $hydra.lib.logic.if_else (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.not" (func $hydra.lib.logic.not (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.from_list" (func $hydra.lib.maps.from_list (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.cat" (func $hydra.lib.maybes.cat (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.singleton" (func $hydra.lib.sets.singleton (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat" (func $hydra.lib.strings.cat (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.intercalate" (func $hydra.lib.strings.intercalate (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.split_on" (func $hydra.lib.strings.split_on (param i32) (result i32) ) )
  (import "hydra.names" "hydra.names.local_name_of" (func $hydra.names.local_name_of (param i32) (result i32) ) )
  (import "hydra.predicates" "hydra.predicates.is_serializable_by_name" (func $hydra.predicates.is_serializable_by_name (param i32) (result i32) ) )
  (memory $memory 1 )
  (export "memory" (memory $memory) )
  (export "hydra.encoding.encode_binding" (func $hydra.encoding.encode_binding) )
  (export "hydra.encoding.encode_binding_name" (func $hydra.encoding.encode_binding_name) )
  (export "hydra.encoding.encode_either_type" (func $hydra.encoding.encode_either_type) )
  (export "hydra.encoding.encode_field_value" (func $hydra.encoding.encode_field_value) )
  (export "hydra.encoding.encode_float_value" (func $hydra.encoding.encode_float_value) )
  (export "hydra.encoding.encode_forall_type" (func $hydra.encoding.encode_forall_type) )
  (export "hydra.encoding.encode_injection" (func $hydra.encoding.encode_injection) )
  (export "hydra.encoding.encode_integer_value" (func $hydra.encoding.encode_integer_value) )
  (export "hydra.encoding.encode_list_type" (func $hydra.encoding.encode_list_type) )
  (export "hydra.encoding.encode_literal_type" (func $hydra.encoding.encode_literal_type) )
  (export "hydra.encoding.encode_map_type" (func $hydra.encoding.encode_map_type) )
  (export "hydra.encoding.encode_module" (func $hydra.encoding.encode_module) )
  (export "hydra.encoding.encode_name" (func $hydra.encoding.encode_name) )
  (export "hydra.encoding.encode_namespace" (func $hydra.encoding.encode_namespace) )
  (export "hydra.encoding.encode_optional_type" (func $hydra.encoding.encode_optional_type) )
  (export "hydra.encoding.encode_pair_type" (func $hydra.encoding.encode_pair_type) )
  (export "hydra.encoding.encode_record_type" (func $hydra.encoding.encode_record_type) )
  (export "hydra.encoding.encode_record_type_named" (func $hydra.encoding.encode_record_type_named) )
  (export "hydra.encoding.encode_set_type" (func $hydra.encoding.encode_set_type) )
  (export "hydra.encoding.encode_type" (func $hydra.encoding.encode_type) )
  (export "hydra.encoding.encode_type_named" (func $hydra.encoding.encode_type_named) )
  (export "hydra.encoding.encode_union_type" (func $hydra.encoding.encode_union_type) )
  (export "hydra.encoding.encode_union_type_named" (func $hydra.encoding.encode_union_type_named) )
  (export "hydra.encoding.encode_wrapped_type" (func $hydra.encoding.encode_wrapped_type) )
  (export "hydra.encoding.encode_wrapped_type_named" (func $hydra.encoding.encode_wrapped_type_named) )
  (export "hydra.encoding.encoder_collect_forall_variables" (func $hydra.encoding.encoder_collect_forall_variables) )
  (export "hydra.encoding.encoder_collect_ord_vars" (func $hydra.encoding.encoder_collect_ord_vars) )
  (export "hydra.encoding.encoder_collect_type_vars_from_type" (func $hydra.encoding.encoder_collect_type_vars_from_type) )
  (export "hydra.encoding.encoder_full_result_type" (func $hydra.encoding.encoder_full_result_type) )
  (export "hydra.encoding.encoder_full_result_type_named" (func $hydra.encoding.encoder_full_result_type_named) )
  (export "hydra.encoding.encoder_type" (func $hydra.encoding.encoder_type) )
  (export "hydra.encoding.encoder_type_named" (func $hydra.encoding.encoder_type_named) )
  (export "hydra.encoding.encoder_type_scheme" (func $hydra.encoding.encoder_type_scheme) )
  (export "hydra.encoding.encoder_type_scheme_named" (func $hydra.encoding.encoder_type_scheme_named) )
  (export "hydra.encoding.filter_type_bindings" (func $hydra.encoding.filter_type_bindings) )
  (export "hydra.encoding.is_encodable_binding" (func $hydra.encoding.is_encodable_binding) )
  (export "hydra.encoding.is_unit_type" (func $hydra.encoding.is_unit_type) )
  (export "hydra.encoding.prepend_forall_encoders" (func $hydra.encoding.prepend_forall_encoders) )
  (func $hydra.encoding.encode_binding (param $cx i32) (param $graph i32) (param $b i32) (result i32)
  (local $_wc_a i32)
  (local $_wc_e i32)
  (local $typ i32)
  local.get $_wc_e
  local.get $cx
  local.get $_wc_a
  local.get $graph
  local.get $b
  ;; project field: term
  call $hydra.decode.core.type
  call $hydra.lib.eithers.bimap
  i32.const 1
  local.get $b
  ;; project field: name
  call $hydra.encoding.encode_binding_name
  local.get $b
  ;; project field: name
  local.get $typ
  call $hydra.encoding.encode_type_named
  local.get $b
  ;; project field: name
  local.get $typ
  call $hydra.encoding.encoder_type_scheme_named
  call $hydra.lib.eithers.bind
)
  (func $hydra.encoding.encode_binding_name (param $n i32) (result i32)
  i32.const 0 ;; string: "."
  nop
  call $hydra.lib.strings.split_on
  call $hydra.lib.lists.tail
  call $hydra.lib.lists.null
  call $hydra.lib.logic.not
  i32.const 0 ;; string: "."
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "hydra"
  i32.const 0 ;; string: "encode"
  i32.const 0 ;; string: "."
  nop
  call $hydra.lib.strings.split_on
  call $hydra.lib.lists.init
  call $hydra.lib.lists.tail
  i32.const 1
  ;; list elements follow
  local.get $n
  call $hydra.names.local_name_of
  call $hydra.formatting.decapitalize
  call $hydra.lib.lists.concat2
  call $hydra.lib.lists.concat2
  call $hydra.lib.strings.intercalate
  local.get $n
  call $hydra.names.local_name_of
  call $hydra.formatting.decapitalize
  call $hydra.lib.logic.if_else
)
  (func $hydra.encoding.encode_either_type (param $et i32) (result i32)
  i32.const 0 ;; string: "e"
  i32.const 0
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 0 ;; string: "either"
  i32.const 0 ;; string: "hydra.lib.eithers.bimap"
  local.get $et
  ;; project field: left
  call $hydra.encoding.encode_type
  local.get $et
  ;; project field: right
  call $hydra.encoding.encode_type
  i32.const 0 ;; string: "e"
)
  (func $hydra.encoding.encode_field_value (param $type_name i32) (param $field_name i32) (param $field_type i32) (result i32)
  i32.const 0 ;; string: "y"
  i32.const 0
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 0 ;; string: "union"
  local.get $type_name
  local.get $field_name
  local.get $field_type
  call $hydra.encoding.encode_type
  i32.const 0 ;; string: "y"
  call $hydra.encoding.encode_injection
)
  (func $hydra.encoding.encode_float_value (param $float_type i32) (param $val_term i32) (result i32)
  i32.const 0 ;; string: "hydra.core.FloatValue"
  (block $end_float_type (result i32)
  (block $float64
  (block $float32
  (block $bigfloat
  local.get $float_type
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
  local.get $val_term
)
  (func $hydra.encoding.encode_forall_type (param $ft i32) (result i32)
  local.get $ft
  ;; project field: parameter
  call $hydra.encoding.encode_binding_name
  i32.const 0
  local.get $ft
  ;; project field: body
  call $hydra.encoding.encode_type
)
  (func $hydra.encoding.encode_injection (param $type_name i32) (param $field_name i32) (param $field_term i32) (result i32)
  (local $fname i32)
  (local $fterm i32)
  i32.const 0 ;; string: "hydra.core.Injection"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "typeName"
  local.get $type_name
  call $hydra.encoding.encode_name
  i32.const 0 ;; string: "field"
  i32.const 0 ;; string: "hydra.core.Field"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "name"
  local.get $fname
  call $hydra.encoding.encode_name
  i32.const 0 ;; string: "term"
  local.get $fterm
)
  (func $hydra.encoding.encode_integer_value (param $int_type i32) (param $val_term i32) (result i32)
  i32.const 0 ;; string: "hydra.core.IntegerValue"
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
  local.get $int_type
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
  local.get $val_term
)
  (func $hydra.encoding.encode_list_type (param $elem_type i32) (result i32)
  i32.const 0 ;; string: "xs"
  i32.const 0
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 0 ;; string: "list"
  i32.const 0 ;; string: "hydra.lib.lists.map"
  local.get $elem_type
  call $hydra.encoding.encode_type
  i32.const 0 ;; string: "xs"
)
  (func $hydra.encoding.encode_literal_type (param $arg_0 i32) (result i32)
  (local $float_type i32)
  (local $int_type i32)
  (block $end_literal_type (result i32)
  (block $float
  (block $integer
  (block $string
  (block $boolean
  (block $binary
  local.get $arg_0
  br_table $binary $boolean $string $integer $float $float
)
  i32.const 0 ;; string: "x"
  i32.const 0
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 0 ;; string: "literal"
  i32.const 0 ;; string: "hydra.core.Literal"
  i32.const 0 ;; string: "binary"
  i32.const 0 ;; string: "x"
  br $end_literal_type
)
  i32.const 0 ;; string: "x"
  i32.const 0
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 0 ;; string: "literal"
  i32.const 0 ;; string: "hydra.core.Literal"
  i32.const 0 ;; string: "boolean"
  i32.const 0 ;; string: "x"
  br $end_literal_type
)
  i32.const 0 ;; string: "x"
  i32.const 0
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 0 ;; string: "literal"
  i32.const 0 ;; string: "hydra.core.Literal"
  i32.const 0 ;; string: "string"
  i32.const 0 ;; string: "x"
  br $end_literal_type
)
  i32.const 0 ;; string: "x"
  i32.const 0
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 0 ;; string: "literal"
  i32.const 0 ;; string: "hydra.core.Literal"
  i32.const 0 ;; string: "integer"
  local.get $int_type
  i32.const 0 ;; string: "x"
  call $hydra.encoding.encode_integer_value
  br $end_literal_type
)
  i32.const 0 ;; string: "x"
  i32.const 0
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 0 ;; string: "literal"
  i32.const 0 ;; string: "hydra.core.Literal"
  i32.const 0 ;; string: "float"
  local.get $float_type
  i32.const 0 ;; string: "x"
  call $hydra.encoding.encode_float_value
  br $end_literal_type
)
)
  (func $hydra.encoding.encode_map_type (param $mt i32) (result i32)
  i32.const 0 ;; string: "m"
  i32.const 0
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 0 ;; string: "map"
  i32.const 0 ;; string: "hydra.lib.maps.bimap"
  local.get $mt
  ;; project field: keys
  call $hydra.encoding.encode_type
  local.get $mt
  ;; project field: values
  call $hydra.encoding.encode_type
  i32.const 0 ;; string: "m"
)
  (func $hydra.encoding.encode_module (param $cx i32) (param $graph i32) (param $mod i32) (result i32)
  (local $b i32)
  (local $d i32)
  (local $data_term i32)
  (local $encoded_bindings i32)
  (local $ic i32)
  (local $name i32)
  (local $schema_term i32)
  (local $typ i32)
  (local $type_bindings i32)
  (local $x i32)
  local.get $cx
  local.get $graph
  (block $end_definition (result i32)
  (block $type
  local.get $d
  br_table $type $type
)
  i32.const 0 ;; string: "hydra.core.Type"
  local.set $schema_term
  local.get $typ
  call $hydra.encode.core.type
  i32.const 1
  ;; list elements follow
  call $hydra.constants.key_type
  local.get $schema_term
  call $hydra.lib.maps.from_list
  call $hydra.annotations.normalize_term_annotations
  local.set $data_term
  local.get $name
  local.get $data_term
  i32.const 0
  ;; list elements follow
  i32.const 0 ;; string: "hydra.core.Type"
  i32.const 0
  br $end_definition
)
  local.get $mod
  ;; project field: definitions
  call $hydra.lib.lists.map
  call $hydra.lib.maybes.cat
  call $hydra.encoding.filter_type_bindings
  local.get $type_bindings
  call $hydra.lib.lists.null
  i32.const 1
  i32.const 0
  nop
  local.get $ic
  ;; project field: context
  local.get $x
  local.get $cx
  local.get $graph
  local.get $b
  call $hydra.encoding.encode_binding
  call $hydra.lib.eithers.bimap
  local.get $type_bindings
  call $hydra.lib.eithers.map_list
  i32.const 1
  local.get $mod
  ;; project field: namespace
  call $hydra.encoding.encode_namespace
  local.get $b
  ;; project field: name
  local.get $b
  ;; project field: term
  local.get $b
  ;; project field: type
  local.get $encoded_bindings
  call $hydra.lib.lists.map
  call $hydra.encoding.encode_namespace
  local.get $mod
  ;; project field: type_dependencies
  call $hydra.lib.lists.map
  call $hydra.encoding.encode_namespace
  local.get $mod
  ;; project field: term_dependencies
  call $hydra.lib.lists.map
  call $hydra.lib.lists.concat2
  call $hydra.lib.lists.nub
  i32.const 1
  ;; list elements follow
  local.get $mod
  ;; project field: namespace
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "Term encoders for "
  nop
  call $hydra.lib.strings.cat
  call $hydra.lib.eithers.bind
  call $hydra.lib.logic.if_else
  call $hydra.lib.eithers.bind
)
  (func $hydra.encoding.encode_name (param $n i32) (result i32)
  i32.const 0 ;; string: "hydra.core.Name"
  nop
)
  (func $hydra.encoding.encode_namespace (param $ns i32) (result i32)
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "hydra.encode."
  i32.const 0 ;; string: "."
  i32.const 0 ;; string: "."
  nop
  call $hydra.lib.strings.split_on
  call $hydra.lib.lists.tail
  call $hydra.lib.strings.intercalate
  call $hydra.lib.strings.cat
)
  (func $hydra.encoding.encode_optional_type (param $elem_type i32) (result i32)
  i32.const 0 ;; string: "opt"
  i32.const 0
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 0 ;; string: "maybe"
  i32.const 0 ;; string: "hydra.lib.maybes.map"
  local.get $elem_type
  call $hydra.encoding.encode_type
  i32.const 0 ;; string: "opt"
)
  (func $hydra.encoding.encode_pair_type (param $pt i32) (result i32)
  i32.const 0 ;; string: "p"
  i32.const 0
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 0 ;; string: "pair"
  i32.const 0 ;; string: "hydra.lib.pairs.bimap"
  local.get $pt
  ;; project field: first
  call $hydra.encoding.encode_type
  local.get $pt
  ;; project field: second
  call $hydra.encoding.encode_type
  i32.const 0 ;; string: "p"
)
  (func $hydra.encoding.encode_record_type (param $rt i32) (result i32)
  i32.const 0 ;; string: "unknown"
  local.get $rt
  call $hydra.encoding.encode_record_type_named
)
  (func $hydra.encoding.encode_record_type_named (param $ename i32) (param $rt i32) (result i32)
  (local $ft i32)
  (local $tname i32)
  i32.const 0 ;; string: "x"
  i32.const 0
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 0 ;; string: "record"
  i32.const 0 ;; string: "hydra.core.Record"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "typeName"
  local.get $ename
  call $hydra.encoding.encode_name
  i32.const 0 ;; string: "fields"
  i32.const 0 ;; string: "hydra.core.Field"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "name"
  local.get $ft
  ;; project field: name
  call $hydra.encoding.encode_name
  i32.const 0 ;; string: "term"
  local.get $ft
  ;; project field: type
  call $hydra.encoding.encode_type
  local.get $tname
  local.get $ft
  ;; project field: name
  i32.const 0 ;; string: "x"
  local.get $rt
  call $hydra.lib.lists.map
)
  (func $hydra.encoding.encode_set_type (param $elem_type i32) (result i32)
  i32.const 0 ;; string: "s"
  i32.const 0
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 0 ;; string: "set"
  i32.const 0 ;; string: "hydra.lib.sets.map"
  local.get $elem_type
  call $hydra.encoding.encode_type
  i32.const 0 ;; string: "s"
)
  (func $hydra.encoding.encode_type (param $arg_0 i32) (result i32)
  (local $app_type i32)
  (local $at i32)
  (local $elem_type i32)
  (local $et i32)
  (local $ft i32)
  (local $lt i32)
  (local $mt i32)
  (local $pt i32)
  (local $rt i32)
  (local $type_name i32)
  (local $wt i32)
  (block $end_type (result i32)
  (block $variable
  (block $void
  (block $unit
  (block $wrap
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
  br_table $annotated $application $either $forall $function $list $literal $map $maybe $pair $record $set $union $wrap $unit $void $variable $variable
)
  local.get $at
  ;; project field: body
  call $hydra.encoding.encode_type
  br $end_type
)
  local.get $app_type
  ;; project field: function
  call $hydra.encoding.encode_type
  local.get $app_type
  ;; project field: argument
  call $hydra.encoding.encode_type
  br $end_type
)
  local.get $et
  call $hydra.encoding.encode_either_type
  br $end_type
)
  local.get $ft
  call $hydra.encoding.encode_forall_type
  br $end_type
)
  i32.const 0 ;; string: "x"
  i32.const 0
  i32.const 0 ;; string: "x"
  br $end_type
)
  local.get $elem_type
  call $hydra.encoding.encode_list_type
  br $end_type
)
  local.get $lt
  call $hydra.encoding.encode_literal_type
  br $end_type
)
  local.get $mt
  call $hydra.encoding.encode_map_type
  br $end_type
)
  local.get $elem_type
  call $hydra.encoding.encode_optional_type
  br $end_type
)
  local.get $pt
  call $hydra.encoding.encode_pair_type
  br $end_type
)
  local.get $rt
  call $hydra.encoding.encode_record_type
  br $end_type
)
  local.get $elem_type
  call $hydra.encoding.encode_set_type
  br $end_type
)
  local.get $rt
  call $hydra.encoding.encode_union_type
  br $end_type
)
  local.get $wt
  call $hydra.encoding.encode_wrapped_type
  br $end_type
)
  i32.const 0 ;; string: "_"
  i32.const 0
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 0 ;; string: "unit"
  i32.const 0
  br $end_type
)
  i32.const 0 ;; string: "_"
  i32.const 0
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 0 ;; string: "unit"
  i32.const 0
  br $end_type
)
  local.get $type_name
  call $hydra.encoding.encode_binding_name
  br $end_type
)
)
  (func $hydra.encoding.encode_type_named (param $ename i32) (param $typ i32) (result i32)
  (local $app_type i32)
  (local $at i32)
  (local $elem_type i32)
  (local $et i32)
  (local $ft i32)
  (local $lt i32)
  (local $mt i32)
  (local $pt i32)
  (local $rt i32)
  (local $type_name i32)
  (local $wt i32)
  (block $end_type (result i32)
  (block $variable
  (block $void
  (block $unit
  (block $wrap
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
  br_table $annotated $application $either $forall $function $list $literal $map $maybe $pair $record $set $union $wrap $unit $void $variable $variable
)
  local.get $ename
  local.get $at
  ;; project field: body
  call $hydra.encoding.encode_type_named
  br $end_type
)
  local.get $app_type
  ;; project field: function
  call $hydra.encoding.encode_type
  local.get $app_type
  ;; project field: argument
  call $hydra.encoding.encode_type
  br $end_type
)
  local.get $et
  call $hydra.encoding.encode_either_type
  br $end_type
)
  local.get $ft
  ;; project field: parameter
  call $hydra.encoding.encode_binding_name
  i32.const 0
  local.get $ename
  local.get $ft
  ;; project field: body
  call $hydra.encoding.encode_type_named
  br $end_type
)
  i32.const 0 ;; string: "x"
  i32.const 0
  i32.const 0 ;; string: "x"
  br $end_type
)
  local.get $elem_type
  call $hydra.encoding.encode_list_type
  br $end_type
)
  local.get $lt
  call $hydra.encoding.encode_literal_type
  br $end_type
)
  local.get $mt
  call $hydra.encoding.encode_map_type
  br $end_type
)
  local.get $elem_type
  call $hydra.encoding.encode_optional_type
  br $end_type
)
  local.get $pt
  call $hydra.encoding.encode_pair_type
  br $end_type
)
  local.get $ename
  local.get $rt
  call $hydra.encoding.encode_record_type_named
  br $end_type
)
  local.get $elem_type
  call $hydra.encoding.encode_set_type
  br $end_type
)
  local.get $ename
  local.get $rt
  call $hydra.encoding.encode_union_type_named
  br $end_type
)
  local.get $ename
  local.get $wt
  call $hydra.encoding.encode_wrapped_type_named
  br $end_type
)
  i32.const 0 ;; string: "_"
  i32.const 0
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 0 ;; string: "unit"
  i32.const 0
  br $end_type
)
  i32.const 0 ;; string: "_"
  i32.const 0
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 0 ;; string: "unit"
  i32.const 0
  br $end_type
)
  local.get $type_name
  call $hydra.encoding.encode_binding_name
  br $end_type
)
)
  (func $hydra.encoding.encode_union_type (param $rt i32) (result i32)
  i32.const 0 ;; string: "unknown"
  local.get $rt
  call $hydra.encoding.encode_union_type_named
)
  (func $hydra.encoding.encode_union_type_named (param $ename i32) (param $rt i32) (result i32)
  (local $ft i32)
  local.get $ename
  i32.const 0
  local.get $ft
  ;; project field: name
  local.get $ename
  local.get $ft
  ;; project field: name
  local.get $ft
  ;; project field: type
  call $hydra.encoding.encode_field_value
  local.get $rt
  call $hydra.lib.lists.map
)
  (func $hydra.encoding.encode_wrapped_type (param $wt i32) (result i32)
  i32.const 0 ;; string: "unknown"
  local.get $wt
  call $hydra.encoding.encode_wrapped_type_named
)
  (func $hydra.encoding.encode_wrapped_type_named (param $ename i32) (param $wt i32) (result i32)
  i32.const 0 ;; string: "x"
  i32.const 0
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 0 ;; string: "wrap"
  i32.const 0 ;; string: "hydra.core.WrappedTerm"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "typeName"
  local.get $ename
  call $hydra.encoding.encode_name
  i32.const 0 ;; string: "body"
  local.get $wt
  call $hydra.encoding.encode_type
  local.get $ename
  i32.const 0 ;; string: "x"
)
  (func $hydra.encoding.encoder_collect_forall_variables (param $typ i32) (result i32)
  (local $at i32)
  (local $ft i32)
  (block $end_type (result i32)
  (block $forall
  (block $annotated
  local.get $typ
  br_table $annotated $forall $forall
)
  local.get $at
  ;; project field: body
  call $hydra.encoding.encoder_collect_forall_variables
  br $end_type
)
  local.get $ft
  ;; project field: parameter
  local.get $ft
  ;; project field: body
  call $hydra.encoding.encoder_collect_forall_variables
  call $hydra.lib.lists.cons
  br $end_type
)
)
  (func $hydra.encoding.encoder_collect_ord_vars (param $typ i32) (result i32)
  (local $app_type i32)
  (local $at i32)
  (local $elem_type i32)
  (local $et i32)
  (local $ft i32)
  (local $mt i32)
  (local $pt i32)
  (local $rt i32)
  (local $wt i32)
  (block $end_type (result i32)
  (block $wrap
  (block $union
  (block $set
  (block $record
  (block $pair
  (block $maybe
  (block $map
  (block $list
  (block $forall
  (block $either
  (block $application
  (block $annotated
  local.get $typ
  br_table $annotated $application $either $forall $list $map $maybe $pair $record $set $union $wrap $wrap
)
  local.get $at
  ;; project field: body
  call $hydra.encoding.encoder_collect_ord_vars
  br $end_type
)
  local.get $app_type
  ;; project field: function
  call $hydra.encoding.encoder_collect_ord_vars
  local.get $app_type
  ;; project field: argument
  call $hydra.encoding.encoder_collect_ord_vars
  call $hydra.lib.lists.concat2
  br $end_type
)
  local.get $et
  ;; project field: left
  call $hydra.encoding.encoder_collect_ord_vars
  local.get $et
  ;; project field: right
  call $hydra.encoding.encoder_collect_ord_vars
  call $hydra.lib.lists.concat2
  br $end_type
)
  local.get $ft
  ;; project field: body
  call $hydra.encoding.encoder_collect_ord_vars
  br $end_type
)
  local.get $elem_type
  call $hydra.encoding.encoder_collect_ord_vars
  br $end_type
)
  i32.const 3
  ;; list elements follow
  local.get $mt
  ;; project field: keys
  call $hydra.encoding.encoder_collect_type_vars_from_type
  local.get $mt
  ;; project field: keys
  call $hydra.encoding.encoder_collect_ord_vars
  local.get $mt
  ;; project field: values
  call $hydra.encoding.encoder_collect_ord_vars
  call $hydra.lib.lists.concat
  br $end_type
)
  local.get $elem_type
  call $hydra.encoding.encoder_collect_ord_vars
  br $end_type
)
  local.get $pt
  ;; project field: first
  call $hydra.encoding.encoder_collect_ord_vars
  local.get $pt
  ;; project field: second
  call $hydra.encoding.encoder_collect_ord_vars
  call $hydra.lib.lists.concat2
  br $end_type
)
  local.get $ft
  ;; project field: type
  call $hydra.encoding.encoder_collect_ord_vars
  local.get $rt
  call $hydra.lib.lists.map
  call $hydra.lib.lists.concat
  br $end_type
)
  local.get $elem_type
  call $hydra.encoding.encoder_collect_type_vars_from_type
  local.get $elem_type
  call $hydra.encoding.encoder_collect_ord_vars
  call $hydra.lib.lists.concat2
  br $end_type
)
  local.get $ft
  ;; project field: type
  call $hydra.encoding.encoder_collect_ord_vars
  local.get $rt
  call $hydra.lib.lists.map
  call $hydra.lib.lists.concat
  br $end_type
)
  local.get $wt
  call $hydra.encoding.encoder_collect_ord_vars
  br $end_type
)
)
  (func $hydra.encoding.encoder_collect_type_vars_from_type (param $typ i32) (result i32)
  (local $app_type i32)
  (local $at i32)
  (local $elem_type i32)
  (local $ft i32)
  (local $mt i32)
  (local $name i32)
  (local $pt i32)
  (local $rt i32)
  (local $wt i32)
  (block $end_type (result i32)
  (block $wrap
  (block $variable
  (block $union
  (block $set
  (block $record
  (block $pair
  (block $maybe
  (block $map
  (block $list
  (block $forall
  (block $application
  (block $annotated
  local.get $typ
  br_table $annotated $application $forall $list $map $maybe $pair $record $set $union $variable $wrap $wrap
)
  local.get $at
  ;; project field: body
  call $hydra.encoding.encoder_collect_type_vars_from_type
  br $end_type
)
  local.get $app_type
  ;; project field: function
  call $hydra.encoding.encoder_collect_type_vars_from_type
  local.get $app_type
  ;; project field: argument
  call $hydra.encoding.encoder_collect_type_vars_from_type
  call $hydra.lib.lists.concat2
  br $end_type
)
  local.get $ft
  ;; project field: body
  call $hydra.encoding.encoder_collect_type_vars_from_type
  br $end_type
)
  local.get $elem_type
  call $hydra.encoding.encoder_collect_type_vars_from_type
  br $end_type
)
  local.get $mt
  ;; project field: keys
  call $hydra.encoding.encoder_collect_type_vars_from_type
  local.get $mt
  ;; project field: values
  call $hydra.encoding.encoder_collect_type_vars_from_type
  call $hydra.lib.lists.concat2
  br $end_type
)
  local.get $elem_type
  call $hydra.encoding.encoder_collect_type_vars_from_type
  br $end_type
)
  local.get $pt
  ;; project field: first
  call $hydra.encoding.encoder_collect_type_vars_from_type
  local.get $pt
  ;; project field: second
  call $hydra.encoding.encoder_collect_type_vars_from_type
  call $hydra.lib.lists.concat2
  br $end_type
)
  local.get $ft
  ;; project field: type
  call $hydra.encoding.encoder_collect_type_vars_from_type
  local.get $rt
  call $hydra.lib.lists.map
  call $hydra.lib.lists.concat
  br $end_type
)
  local.get $elem_type
  call $hydra.encoding.encoder_collect_type_vars_from_type
  br $end_type
)
  local.get $ft
  ;; project field: type
  call $hydra.encoding.encoder_collect_type_vars_from_type
  local.get $rt
  call $hydra.lib.lists.map
  call $hydra.lib.lists.concat
  br $end_type
)
  i32.const 1
  ;; list elements follow
  local.get $name
  br $end_type
)
  local.get $wt
  call $hydra.encoding.encoder_collect_type_vars_from_type
  br $end_type
)
)
  (func $hydra.encoding.encoder_full_result_type (param $typ i32) (result i32)
  (local $app_type i32)
  (local $at i32)
  (local $elem_type i32)
  (local $et i32)
  (local $ft i32)
  (local $mt i32)
  (local $name i32)
  (local $pt i32)
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
  (block $forall
  (block $either
  (block $application
  (block $annotated
  local.get $typ
  br_table $annotated $application $either $forall $list $literal $map $maybe $pair $record $set $union $unit $variable $void $wrap $wrap
)
  local.get $at
  ;; project field: body
  call $hydra.encoding.encoder_full_result_type
  br $end_type
)
  local.get $app_type
  ;; project field: function
  call $hydra.encoding.encoder_full_result_type
  local.get $app_type
  ;; project field: argument
  br $end_type
)
  local.get $et
  ;; project field: left
  call $hydra.encoding.encoder_full_result_type
  local.get $et
  ;; project field: right
  call $hydra.encoding.encoder_full_result_type
  br $end_type
)
  local.get $ft
  ;; project field: body
  call $hydra.encoding.encoder_full_result_type
  local.get $ft
  ;; project field: parameter
  br $end_type
)
  local.get $elem_type
  call $hydra.encoding.encoder_full_result_type
  br $end_type
)
  i32.const 0 ;; string: "hydra.core.Literal"
  br $end_type
)
  local.get $mt
  ;; project field: keys
  call $hydra.encoding.encoder_full_result_type
  local.get $mt
  ;; project field: values
  call $hydra.encoding.encoder_full_result_type
  br $end_type
)
  local.get $elem_type
  call $hydra.encoding.encoder_full_result_type
  br $end_type
)
  local.get $pt
  ;; project field: first
  call $hydra.encoding.encoder_full_result_type
  local.get $pt
  ;; project field: second
  call $hydra.encoding.encoder_full_result_type
  br $end_type
)
  i32.const 0 ;; string: "hydra.core.Term"
  br $end_type
)
  local.get $elem_type
  call $hydra.encoding.encoder_full_result_type
  br $end_type
)
  i32.const 0 ;; string: "hydra.core.Term"
  br $end_type
)
  i32.const 0
  br $end_type
)
  local.get $name
  br $end_type
)
  i32.const 0
  br $end_type
)
  i32.const 0 ;; string: "hydra.core.Term"
  br $end_type
)
)
  (func $hydra.encoding.encoder_full_result_type_named (param $ename i32) (param $typ i32) (result i32)
  (local $app_type i32)
  (local $at i32)
  (local $elem_type i32)
  (local $et i32)
  (local $ft i32)
  (local $mt i32)
  (local $name i32)
  (local $pt i32)
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
  (block $forall
  (block $either
  (block $application
  (block $annotated
  local.get $typ
  br_table $annotated $application $either $forall $list $literal $map $maybe $pair $record $set $union $unit $variable $void $wrap $wrap
)
  local.get $ename
  local.get $at
  ;; project field: body
  call $hydra.encoding.encoder_full_result_type_named
  br $end_type
)
  local.get $app_type
  ;; project field: function
  call $hydra.encoding.encoder_full_result_type
  local.get $app_type
  ;; project field: argument
  br $end_type
)
  local.get $et
  ;; project field: left
  call $hydra.encoding.encoder_full_result_type
  local.get $et
  ;; project field: right
  call $hydra.encoding.encoder_full_result_type
  br $end_type
)
  local.get $ename
  local.get $ft
  ;; project field: body
  call $hydra.encoding.encoder_full_result_type_named
  local.get $ft
  ;; project field: parameter
  br $end_type
)
  local.get $elem_type
  call $hydra.encoding.encoder_full_result_type
  br $end_type
)
  i32.const 0 ;; string: "hydra.core.Literal"
  br $end_type
)
  local.get $mt
  ;; project field: keys
  call $hydra.encoding.encoder_full_result_type
  local.get $mt
  ;; project field: values
  call $hydra.encoding.encoder_full_result_type
  br $end_type
)
  local.get $elem_type
  call $hydra.encoding.encoder_full_result_type
  br $end_type
)
  local.get $pt
  ;; project field: first
  call $hydra.encoding.encoder_full_result_type
  local.get $pt
  ;; project field: second
  call $hydra.encoding.encoder_full_result_type
  br $end_type
)
  local.get $ename
  br $end_type
)
  local.get $elem_type
  call $hydra.encoding.encoder_full_result_type
  br $end_type
)
  local.get $ename
  br $end_type
)
  i32.const 0
  br $end_type
)
  local.get $name
  br $end_type
)
  i32.const 0
  br $end_type
)
  local.get $ename
  br $end_type
)
)
  (func $hydra.encoding.encoder_type (param $typ i32) (result i32)
  (local $base_type i32)
  (local $result_type i32)
  local.get $typ
  call $hydra.encoding.encoder_full_result_type
  local.set $result_type
  local.get $result_type
  i32.const 0 ;; string: "hydra.core.Term"
  local.set $base_type
  local.get $base_type
  local.get $typ
  call $hydra.encoding.prepend_forall_encoders
)
  (func $hydra.encoding.encoder_type_named (param $ename i32) (param $typ i32) (result i32)
  (local $base_type i32)
  (local $result_type i32)
  local.get $ename
  local.get $typ
  call $hydra.encoding.encoder_full_result_type_named
  local.set $result_type
  local.get $result_type
  i32.const 0 ;; string: "hydra.core.Term"
  local.set $base_type
  local.get $base_type
  local.get $typ
  call $hydra.encoding.prepend_forall_encoders
)
  (func $hydra.encoding.encoder_type_scheme (param $typ i32) (result i32)
  (local $all_ord_vars i32)
  (local $constraints i32)
  (local $encoder_fun_type i32)
  (local $ord_vars i32)
  (local $type_vars i32)
  (local $v i32)
  local.get $typ
  call $hydra.encoding.encoder_collect_forall_variables
  local.set $type_vars
  local.get $typ
  call $hydra.encoding.encoder_type
  local.set $encoder_fun_type
  local.get $typ
  call $hydra.encoding.encoder_collect_ord_vars
  local.set $all_ord_vars
  local.get $v
  local.get $type_vars
  call $hydra.lib.lists.elem
  local.get $all_ord_vars
  call $hydra.lib.lists.filter
  local.set $ord_vars
  local.get $ord_vars
  call $hydra.lib.lists.null
  i32.const 0
  local.get $v
  i32.const 0 ;; string: "ordering"
  call $hydra.lib.sets.singleton
  local.get $ord_vars
  call $hydra.lib.lists.map
  call $hydra.lib.maps.from_list
  call $hydra.lib.logic.if_else
  local.set $constraints
  local.get $type_vars
  local.get $encoder_fun_type
  local.get $constraints
)
  (func $hydra.encoding.encoder_type_scheme_named (param $ename i32) (param $typ i32) (result i32)
  (local $all_ord_vars i32)
  (local $constraints i32)
  (local $encoder_fun_type i32)
  (local $ord_vars i32)
  (local $type_vars i32)
  (local $v i32)
  local.get $typ
  call $hydra.encoding.encoder_collect_forall_variables
  local.set $type_vars
  local.get $ename
  local.get $typ
  call $hydra.encoding.encoder_type_named
  local.set $encoder_fun_type
  local.get $typ
  call $hydra.encoding.encoder_collect_ord_vars
  local.set $all_ord_vars
  local.get $v
  local.get $type_vars
  call $hydra.lib.lists.elem
  local.get $all_ord_vars
  call $hydra.lib.lists.filter
  local.set $ord_vars
  local.get $ord_vars
  call $hydra.lib.lists.null
  i32.const 0
  local.get $v
  i32.const 0 ;; string: "ordering"
  call $hydra.lib.sets.singleton
  local.get $ord_vars
  call $hydra.lib.lists.map
  call $hydra.lib.maps.from_list
  call $hydra.lib.logic.if_else
  local.set $constraints
  local.get $type_vars
  local.get $encoder_fun_type
  local.get $constraints
)
  (func $hydra.encoding.filter_type_bindings (param $cx i32) (param $graph i32) (param $bindings i32) (result i32)
  call $hydra.lib.maybes.cat
  local.get $cx
  local.get $graph
  call $hydra.encoding.is_encodable_binding
  call $hydra.annotations.is_native_type
  local.get $bindings
  call $hydra.lib.lists.filter
  call $hydra.lib.eithers.map_list
  call $hydra.lib.eithers.map
)
  (func $hydra.encoding.is_encodable_binding (param $cx i32) (param $graph i32) (param $b i32) (result i32)
  (local $serializable i32)
  local.get $cx
  local.get $graph
  local.get $b
  ;; project field: name
  call $hydra.predicates.is_serializable_by_name
  i32.const 1
  local.get $serializable
  local.get $b
  i32.const 0
  call $hydra.lib.logic.if_else
  call $hydra.lib.eithers.bind
)
  (func $hydra.encoding.is_unit_type (param $arg_0 i32) (result i32)
  (block $end_type (result i32)
  (block $unit
  local.get $arg_0
  br_table $unit $unit
)
  i32.const 1
  br $end_type
)
)
  (func $hydra.encoding.prepend_forall_encoders (param $base_type i32) (param $typ i32) (result i32)
  (local $at i32)
  (local $ft i32)
  (block $end_type (result i32)
  (block $forall
  (block $annotated
  local.get $typ
  br_table $annotated $forall $forall
)
  local.get $base_type
  local.get $at
  ;; project field: body
  call $hydra.encoding.prepend_forall_encoders
  br $end_type
)
  local.get $ft
  ;; project field: parameter
  i32.const 0 ;; string: "hydra.core.Term"
  local.get $base_type
  local.get $ft
  ;; project field: body
  call $hydra.encoding.prepend_forall_encoders
  br $end_type
)
)
)
