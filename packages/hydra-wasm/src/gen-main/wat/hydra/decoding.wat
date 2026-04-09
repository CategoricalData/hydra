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
  (import "hydra.lib.lists" "hydra.lib.lists.foldl" (func $hydra.lib.lists.foldl (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.init" (func $hydra.lib.lists.init (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.nub" (func $hydra.lib.lists.nub (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.null" (func $hydra.lib.lists.null (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.reverse" (func $hydra.lib.lists.reverse (param i32) (result i32) ) )
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
  (export "hydra.decoding.collect_forall_variables" (func $hydra.decoding.collect_forall_variables) )
  (export "hydra.decoding.collect_ord_constrained_variables" (func $hydra.decoding.collect_ord_constrained_variables) )
  (export "hydra.decoding.collect_type_variables" (func $hydra.decoding.collect_type_variables) )
  (export "hydra.decoding.collect_type_variables_from_type" (func $hydra.decoding.collect_type_variables_from_type) )
  (export "hydra.decoding.decode_binding" (func $hydra.decoding.decode_binding) )
  (export "hydra.decoding.decode_binding_name" (func $hydra.decoding.decode_binding_name) )
  (export "hydra.decoding.decode_either_type" (func $hydra.decoding.decode_either_type) )
  (export "hydra.decoding.decode_forall_type" (func $hydra.decoding.decode_forall_type) )
  (export "hydra.decoding.decode_list_type" (func $hydra.decoding.decode_list_type) )
  (export "hydra.decoding.decode_literal_type" (func $hydra.decoding.decode_literal_type) )
  (export "hydra.decoding.decode_map_type" (func $hydra.decoding.decode_map_type) )
  (export "hydra.decoding.decode_maybe_type" (func $hydra.decoding.decode_maybe_type) )
  (export "hydra.decoding.decode_module" (func $hydra.decoding.decode_module) )
  (export "hydra.decoding.decode_namespace" (func $hydra.decoding.decode_namespace) )
  (export "hydra.decoding.decode_pair_type" (func $hydra.decoding.decode_pair_type) )
  (export "hydra.decoding.decode_record_type" (func $hydra.decoding.decode_record_type) )
  (export "hydra.decoding.decode_record_type_impl" (func $hydra.decoding.decode_record_type_impl) )
  (export "hydra.decoding.decode_record_type_named" (func $hydra.decoding.decode_record_type_named) )
  (export "hydra.decoding.decode_set_type" (func $hydra.decoding.decode_set_type) )
  (export "hydra.decoding.decode_type" (func $hydra.decoding.decode_type) )
  (export "hydra.decoding.decode_type_named" (func $hydra.decoding.decode_type_named) )
  (export "hydra.decoding.decode_union_type" (func $hydra.decoding.decode_union_type) )
  (export "hydra.decoding.decode_union_type_named" (func $hydra.decoding.decode_union_type_named) )
  (export "hydra.decoding.decode_unit_type" (func $hydra.decoding.decode_unit_type) )
  (export "hydra.decoding.decode_wrapped_type" (func $hydra.decoding.decode_wrapped_type) )
  (export "hydra.decoding.decode_wrapped_type_named" (func $hydra.decoding.decode_wrapped_type_named) )
  (export "hydra.decoding.decoder_full_result_type" (func $hydra.decoding.decoder_full_result_type) )
  (export "hydra.decoding.decoder_full_result_type_named" (func $hydra.decoding.decoder_full_result_type_named) )
  (export "hydra.decoding.decoder_result_type" (func $hydra.decoding.decoder_result_type) )
  (export "hydra.decoding.decoder_type" (func $hydra.decoding.decoder_type) )
  (export "hydra.decoding.decoder_type_named" (func $hydra.decoding.decoder_type_named) )
  (export "hydra.decoding.decoder_type_scheme" (func $hydra.decoding.decoder_type_scheme) )
  (export "hydra.decoding.decoder_type_scheme_named" (func $hydra.decoding.decoder_type_scheme_named) )
  (export "hydra.decoding.filter_type_bindings" (func $hydra.decoding.filter_type_bindings) )
  (export "hydra.decoding.is_decodable_binding" (func $hydra.decoding.is_decodable_binding) )
  (export "hydra.decoding.prepend_forall_decoders" (func $hydra.decoding.prepend_forall_decoders) )
  (func $hydra.decoding.collect_forall_variables (param $typ i32) (result i32)
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
  call $hydra.decoding.collect_forall_variables
  br $end_type
)
  local.get $ft
  ;; project field: parameter
  local.get $ft
  ;; project field: body
  call $hydra.decoding.collect_forall_variables
  call $hydra.lib.lists.cons
  br $end_type
)
)
  (func $hydra.decoding.collect_ord_constrained_variables (param $typ i32) (result i32)
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
  call $hydra.decoding.collect_ord_constrained_variables
  br $end_type
)
  local.get $app_type
  ;; project field: function
  call $hydra.decoding.collect_ord_constrained_variables
  local.get $app_type
  ;; project field: argument
  call $hydra.decoding.collect_ord_constrained_variables
  call $hydra.lib.lists.concat2
  br $end_type
)
  local.get $et
  ;; project field: left
  call $hydra.decoding.collect_ord_constrained_variables
  local.get $et
  ;; project field: right
  call $hydra.decoding.collect_ord_constrained_variables
  call $hydra.lib.lists.concat2
  br $end_type
)
  local.get $ft
  ;; project field: body
  call $hydra.decoding.collect_ord_constrained_variables
  br $end_type
)
  local.get $elem_type
  call $hydra.decoding.collect_ord_constrained_variables
  br $end_type
)
  i32.const 3
  ;; list elements follow
  local.get $mt
  ;; project field: keys
  call $hydra.decoding.collect_type_variables_from_type
  local.get $mt
  ;; project field: keys
  call $hydra.decoding.collect_ord_constrained_variables
  local.get $mt
  ;; project field: values
  call $hydra.decoding.collect_ord_constrained_variables
  call $hydra.lib.lists.concat
  br $end_type
)
  local.get $elem_type
  call $hydra.decoding.collect_ord_constrained_variables
  br $end_type
)
  local.get $pt
  ;; project field: first
  call $hydra.decoding.collect_ord_constrained_variables
  local.get $pt
  ;; project field: second
  call $hydra.decoding.collect_ord_constrained_variables
  call $hydra.lib.lists.concat2
  br $end_type
)
  local.get $ft
  ;; project field: type
  call $hydra.decoding.collect_ord_constrained_variables
  local.get $rt
  call $hydra.lib.lists.map
  call $hydra.lib.lists.concat
  br $end_type
)
  local.get $elem_type
  call $hydra.decoding.collect_type_variables_from_type
  local.get $elem_type
  call $hydra.decoding.collect_ord_constrained_variables
  call $hydra.lib.lists.concat2
  br $end_type
)
  local.get $ft
  ;; project field: type
  call $hydra.decoding.collect_ord_constrained_variables
  local.get $rt
  call $hydra.lib.lists.map
  call $hydra.lib.lists.concat
  br $end_type
)
  local.get $wt
  call $hydra.decoding.collect_ord_constrained_variables
  br $end_type
)
)
  (func $hydra.decoding.collect_type_variables (param $typ i32) (result i32)
  local.get $typ
  call $hydra.decoding.collect_forall_variables
)
  (func $hydra.decoding.collect_type_variables_from_type (param $typ i32) (result i32)
  (local $app_type i32)
  (local $at i32)
  (local $elem_type i32)
  (local $et i32)
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
  (block $either
  (block $application
  (block $annotated
  local.get $typ
  br_table $annotated $application $either $forall $list $map $maybe $pair $record $set $union $variable $wrap $wrap
)
  local.get $at
  ;; project field: body
  call $hydra.decoding.collect_type_variables_from_type
  br $end_type
)
  local.get $app_type
  ;; project field: function
  call $hydra.decoding.collect_type_variables_from_type
  local.get $app_type
  ;; project field: argument
  call $hydra.decoding.collect_type_variables_from_type
  call $hydra.lib.lists.concat2
  br $end_type
)
  local.get $et
  ;; project field: left
  call $hydra.decoding.collect_type_variables_from_type
  local.get $et
  ;; project field: right
  call $hydra.decoding.collect_type_variables_from_type
  call $hydra.lib.lists.concat2
  br $end_type
)
  local.get $ft
  ;; project field: body
  call $hydra.decoding.collect_type_variables_from_type
  br $end_type
)
  local.get $elem_type
  call $hydra.decoding.collect_type_variables_from_type
  br $end_type
)
  local.get $mt
  ;; project field: keys
  call $hydra.decoding.collect_type_variables_from_type
  local.get $mt
  ;; project field: values
  call $hydra.decoding.collect_type_variables_from_type
  call $hydra.lib.lists.concat2
  br $end_type
)
  local.get $elem_type
  call $hydra.decoding.collect_type_variables_from_type
  br $end_type
)
  local.get $pt
  ;; project field: first
  call $hydra.decoding.collect_type_variables_from_type
  local.get $pt
  ;; project field: second
  call $hydra.decoding.collect_type_variables_from_type
  call $hydra.lib.lists.concat2
  br $end_type
)
  local.get $ft
  ;; project field: type
  call $hydra.decoding.collect_type_variables_from_type
  local.get $rt
  call $hydra.lib.lists.map
  call $hydra.lib.lists.concat
  br $end_type
)
  local.get $elem_type
  call $hydra.decoding.collect_type_variables_from_type
  br $end_type
)
  local.get $ft
  ;; project field: type
  call $hydra.decoding.collect_type_variables_from_type
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
  call $hydra.decoding.collect_type_variables_from_type
  br $end_type
)
)
  (func $hydra.decoding.decode_binding (param $cx i32) (param $graph i32) (param $b i32) (result i32)
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
  call $hydra.decoding.decode_binding_name
  local.get $b
  ;; project field: name
  local.get $typ
  call $hydra.decoding.decode_type_named
  local.get $b
  ;; project field: name
  local.get $typ
  call $hydra.decoding.decoder_type_scheme_named
  call $hydra.lib.eithers.bind
)
  (func $hydra.decoding.decode_binding_name (param $n i32) (result i32)
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
  i32.const 0 ;; string: "decode"
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
  (func $hydra.decoding.decode_either_type (param $et i32) (result i32)
  (local $left_decoder i32)
  (local $right_decoder i32)
  local.get $et
  ;; project field: left
  call $hydra.decoding.decode_type
  local.set $left_decoder
  local.get $et
  ;; project field: right
  call $hydra.decoding.decode_type
  local.set $right_decoder
  i32.const 0 ;; string: "hydra.extract.core.decodeEither"
  local.get $left_decoder
  local.get $right_decoder
)
  (func $hydra.decoding.decode_forall_type (param $ft i32) (result i32)
  local.get $ft
  ;; project field: parameter
  call $hydra.decoding.decode_binding_name
  i32.const 0
  local.get $ft
  ;; project field: body
  call $hydra.decoding.decode_type
)
  (func $hydra.decoding.decode_list_type (param $elem_type i32) (result i32)
  (local $elem_decoder i32)
  local.get $elem_type
  call $hydra.decoding.decode_type
  local.set $elem_decoder
  i32.const 0 ;; string: "hydra.extract.core.decodeList"
  local.get $elem_decoder
)
  (func $hydra.decoding.decode_literal_type (param $lt i32) (result i32)
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
  i32.const 0 ;; string: "cx"
  i32.const 0
  i32.const 0 ;; string: "raw"
  i32.const 0
  i32.const 0 ;; string: "hydra.lib.eithers.either"
  i32.const 0 ;; string: "err"
  i32.const 0
  i32.const 0
  i32.const 0 ;; string: "hydra.errors.DecodingError"
  i32.const 0 ;; string: "err"
  i32.const 0 ;; string: "stripped"
  i32.const 0
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 0
  i32.const 0 ;; string: "hydra.errors.DecodingError"
  i32.const 0 ;; string: "expected literal"
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "literal"
  i32.const 0 ;; string: "v"
  i32.const 0
  i32.const 0 ;; string: "hydra.core.Literal"
  i32.const 0
  i32.const 0 ;; string: "hydra.errors.DecodingError"
  i32.const 0 ;; string: "expected binary literal"
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "binary"
  i32.const 0 ;; string: "b"
  i32.const 0
  i32.const 1
  i32.const 0 ;; string: "b"
  i32.const 0 ;; string: "v"
  i32.const 0 ;; string: "stripped"
  i32.const 0 ;; string: "hydra.lexical.stripAndDereferenceTermEither"
  i32.const 0 ;; string: "cx"
  i32.const 0 ;; string: "raw"
  br $end_literal_type
)
  i32.const 0 ;; string: "cx"
  i32.const 0
  i32.const 0 ;; string: "raw"
  i32.const 0
  i32.const 0 ;; string: "hydra.lib.eithers.either"
  i32.const 0 ;; string: "err"
  i32.const 0
  i32.const 0
  i32.const 0 ;; string: "hydra.errors.DecodingError"
  i32.const 0 ;; string: "err"
  i32.const 0 ;; string: "stripped"
  i32.const 0
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 0
  i32.const 0 ;; string: "hydra.errors.DecodingError"
  i32.const 0 ;; string: "expected literal"
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "literal"
  i32.const 0 ;; string: "v"
  i32.const 0
  i32.const 0 ;; string: "hydra.core.Literal"
  i32.const 0
  i32.const 0 ;; string: "hydra.errors.DecodingError"
  i32.const 0 ;; string: "expected boolean literal"
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "boolean"
  i32.const 0 ;; string: "b"
  i32.const 0
  i32.const 1
  i32.const 0 ;; string: "b"
  i32.const 0 ;; string: "v"
  i32.const 0 ;; string: "stripped"
  i32.const 0 ;; string: "hydra.lexical.stripAndDereferenceTermEither"
  i32.const 0 ;; string: "cx"
  i32.const 0 ;; string: "raw"
  br $end_literal_type
)
  (block $end_float_type (result i32)
  (block $float64
  (block $float32
  (block $bigfloat
  local.get $ft
  br_table $bigfloat $float32 $float64 $float64
)
  i32.const 0 ;; string: "cx"
  i32.const 0
  i32.const 0 ;; string: "raw"
  i32.const 0
  i32.const 0 ;; string: "hydra.lib.eithers.either"
  i32.const 0 ;; string: "err"
  i32.const 0
  i32.const 0
  i32.const 0 ;; string: "hydra.errors.DecodingError"
  i32.const 0 ;; string: "err"
  i32.const 0 ;; string: "stripped"
  i32.const 0
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 0
  i32.const 0 ;; string: "hydra.errors.DecodingError"
  i32.const 0 ;; string: "expected literal"
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "literal"
  i32.const 0 ;; string: "v"
  i32.const 0
  i32.const 0 ;; string: "hydra.core.Literal"
  i32.const 0
  i32.const 0 ;; string: "hydra.errors.DecodingError"
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "expected "
  i32.const 0 ;; string: "bigfloat"
  i32.const 0 ;; string: " literal"
  call $hydra.lib.strings.cat
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "float"
  i32.const 0 ;; string: "hydra.core.FloatValue"
  i32.const 0
  i32.const 0 ;; string: "hydra.errors.DecodingError"
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "expected "
  i32.const 0 ;; string: "bigfloat"
  i32.const 0 ;; string: " value"
  call $hydra.lib.strings.cat
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "bigfloat"
  i32.const 0 ;; string: "f"
  i32.const 0
  i32.const 1
  i32.const 0 ;; string: "f"
  i32.const 0 ;; string: "v"
  i32.const 0 ;; string: "stripped"
  i32.const 0 ;; string: "hydra.lexical.stripAndDereferenceTermEither"
  i32.const 0 ;; string: "cx"
  i32.const 0 ;; string: "raw"
  br $end_float_type
)
  i32.const 0 ;; string: "cx"
  i32.const 0
  i32.const 0 ;; string: "raw"
  i32.const 0
  i32.const 0 ;; string: "hydra.lib.eithers.either"
  i32.const 0 ;; string: "err"
  i32.const 0
  i32.const 0
  i32.const 0 ;; string: "hydra.errors.DecodingError"
  i32.const 0 ;; string: "err"
  i32.const 0 ;; string: "stripped"
  i32.const 0
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 0
  i32.const 0 ;; string: "hydra.errors.DecodingError"
  i32.const 0 ;; string: "expected literal"
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "literal"
  i32.const 0 ;; string: "v"
  i32.const 0
  i32.const 0 ;; string: "hydra.core.Literal"
  i32.const 0
  i32.const 0 ;; string: "hydra.errors.DecodingError"
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "expected "
  i32.const 0 ;; string: "float32"
  i32.const 0 ;; string: " literal"
  call $hydra.lib.strings.cat
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "float"
  i32.const 0 ;; string: "hydra.core.FloatValue"
  i32.const 0
  i32.const 0 ;; string: "hydra.errors.DecodingError"
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "expected "
  i32.const 0 ;; string: "float32"
  i32.const 0 ;; string: " value"
  call $hydra.lib.strings.cat
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "float32"
  i32.const 0 ;; string: "f"
  i32.const 0
  i32.const 1
  i32.const 0 ;; string: "f"
  i32.const 0 ;; string: "v"
  i32.const 0 ;; string: "stripped"
  i32.const 0 ;; string: "hydra.lexical.stripAndDereferenceTermEither"
  i32.const 0 ;; string: "cx"
  i32.const 0 ;; string: "raw"
  br $end_float_type
)
  i32.const 0 ;; string: "cx"
  i32.const 0
  i32.const 0 ;; string: "raw"
  i32.const 0
  i32.const 0 ;; string: "hydra.lib.eithers.either"
  i32.const 0 ;; string: "err"
  i32.const 0
  i32.const 0
  i32.const 0 ;; string: "hydra.errors.DecodingError"
  i32.const 0 ;; string: "err"
  i32.const 0 ;; string: "stripped"
  i32.const 0
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 0
  i32.const 0 ;; string: "hydra.errors.DecodingError"
  i32.const 0 ;; string: "expected literal"
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "literal"
  i32.const 0 ;; string: "v"
  i32.const 0
  i32.const 0 ;; string: "hydra.core.Literal"
  i32.const 0
  i32.const 0 ;; string: "hydra.errors.DecodingError"
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "expected "
  i32.const 0 ;; string: "float64"
  i32.const 0 ;; string: " literal"
  call $hydra.lib.strings.cat
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "float"
  i32.const 0 ;; string: "hydra.core.FloatValue"
  i32.const 0
  i32.const 0 ;; string: "hydra.errors.DecodingError"
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "expected "
  i32.const 0 ;; string: "float64"
  i32.const 0 ;; string: " value"
  call $hydra.lib.strings.cat
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "float64"
  i32.const 0 ;; string: "f"
  i32.const 0
  i32.const 1
  i32.const 0 ;; string: "f"
  i32.const 0 ;; string: "v"
  i32.const 0 ;; string: "stripped"
  i32.const 0 ;; string: "hydra.lexical.stripAndDereferenceTermEither"
  i32.const 0 ;; string: "cx"
  i32.const 0 ;; string: "raw"
  br $end_float_type
)
  br $end_literal_type
)
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
  i32.const 0 ;; string: "cx"
  i32.const 0
  i32.const 0 ;; string: "raw"
  i32.const 0
  i32.const 0 ;; string: "hydra.lib.eithers.either"
  i32.const 0 ;; string: "err"
  i32.const 0
  i32.const 0
  i32.const 0 ;; string: "hydra.errors.DecodingError"
  i32.const 0 ;; string: "err"
  i32.const 0 ;; string: "stripped"
  i32.const 0
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 0
  i32.const 0 ;; string: "hydra.errors.DecodingError"
  i32.const 0 ;; string: "expected literal"
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "literal"
  i32.const 0 ;; string: "v"
  i32.const 0
  i32.const 0 ;; string: "hydra.core.Literal"
  i32.const 0
  i32.const 0 ;; string: "hydra.errors.DecodingError"
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "expected "
  i32.const 0 ;; string: "bigint"
  i32.const 0 ;; string: " literal"
  call $hydra.lib.strings.cat
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "integer"
  i32.const 0 ;; string: "hydra.core.IntegerValue"
  i32.const 0
  i32.const 0 ;; string: "hydra.errors.DecodingError"
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "expected "
  i32.const 0 ;; string: "bigint"
  i32.const 0 ;; string: " value"
  call $hydra.lib.strings.cat
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "bigint"
  i32.const 0 ;; string: "i"
  i32.const 0
  i32.const 1
  i32.const 0 ;; string: "i"
  i32.const 0 ;; string: "v"
  i32.const 0 ;; string: "stripped"
  i32.const 0 ;; string: "hydra.lexical.stripAndDereferenceTermEither"
  i32.const 0 ;; string: "cx"
  i32.const 0 ;; string: "raw"
  br $end_integer_type
)
  i32.const 0 ;; string: "cx"
  i32.const 0
  i32.const 0 ;; string: "raw"
  i32.const 0
  i32.const 0 ;; string: "hydra.lib.eithers.either"
  i32.const 0 ;; string: "err"
  i32.const 0
  i32.const 0
  i32.const 0 ;; string: "hydra.errors.DecodingError"
  i32.const 0 ;; string: "err"
  i32.const 0 ;; string: "stripped"
  i32.const 0
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 0
  i32.const 0 ;; string: "hydra.errors.DecodingError"
  i32.const 0 ;; string: "expected literal"
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "literal"
  i32.const 0 ;; string: "v"
  i32.const 0
  i32.const 0 ;; string: "hydra.core.Literal"
  i32.const 0
  i32.const 0 ;; string: "hydra.errors.DecodingError"
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "expected "
  i32.const 0 ;; string: "int8"
  i32.const 0 ;; string: " literal"
  call $hydra.lib.strings.cat
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "integer"
  i32.const 0 ;; string: "hydra.core.IntegerValue"
  i32.const 0
  i32.const 0 ;; string: "hydra.errors.DecodingError"
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "expected "
  i32.const 0 ;; string: "int8"
  i32.const 0 ;; string: " value"
  call $hydra.lib.strings.cat
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "int8"
  i32.const 0 ;; string: "i"
  i32.const 0
  i32.const 1
  i32.const 0 ;; string: "i"
  i32.const 0 ;; string: "v"
  i32.const 0 ;; string: "stripped"
  i32.const 0 ;; string: "hydra.lexical.stripAndDereferenceTermEither"
  i32.const 0 ;; string: "cx"
  i32.const 0 ;; string: "raw"
  br $end_integer_type
)
  i32.const 0 ;; string: "cx"
  i32.const 0
  i32.const 0 ;; string: "raw"
  i32.const 0
  i32.const 0 ;; string: "hydra.lib.eithers.either"
  i32.const 0 ;; string: "err"
  i32.const 0
  i32.const 0
  i32.const 0 ;; string: "hydra.errors.DecodingError"
  i32.const 0 ;; string: "err"
  i32.const 0 ;; string: "stripped"
  i32.const 0
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 0
  i32.const 0 ;; string: "hydra.errors.DecodingError"
  i32.const 0 ;; string: "expected literal"
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "literal"
  i32.const 0 ;; string: "v"
  i32.const 0
  i32.const 0 ;; string: "hydra.core.Literal"
  i32.const 0
  i32.const 0 ;; string: "hydra.errors.DecodingError"
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "expected "
  i32.const 0 ;; string: "int16"
  i32.const 0 ;; string: " literal"
  call $hydra.lib.strings.cat
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "integer"
  i32.const 0 ;; string: "hydra.core.IntegerValue"
  i32.const 0
  i32.const 0 ;; string: "hydra.errors.DecodingError"
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "expected "
  i32.const 0 ;; string: "int16"
  i32.const 0 ;; string: " value"
  call $hydra.lib.strings.cat
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "int16"
  i32.const 0 ;; string: "i"
  i32.const 0
  i32.const 1
  i32.const 0 ;; string: "i"
  i32.const 0 ;; string: "v"
  i32.const 0 ;; string: "stripped"
  i32.const 0 ;; string: "hydra.lexical.stripAndDereferenceTermEither"
  i32.const 0 ;; string: "cx"
  i32.const 0 ;; string: "raw"
  br $end_integer_type
)
  i32.const 0 ;; string: "cx"
  i32.const 0
  i32.const 0 ;; string: "raw"
  i32.const 0
  i32.const 0 ;; string: "hydra.lib.eithers.either"
  i32.const 0 ;; string: "err"
  i32.const 0
  i32.const 0
  i32.const 0 ;; string: "hydra.errors.DecodingError"
  i32.const 0 ;; string: "err"
  i32.const 0 ;; string: "stripped"
  i32.const 0
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 0
  i32.const 0 ;; string: "hydra.errors.DecodingError"
  i32.const 0 ;; string: "expected literal"
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "literal"
  i32.const 0 ;; string: "v"
  i32.const 0
  i32.const 0 ;; string: "hydra.core.Literal"
  i32.const 0
  i32.const 0 ;; string: "hydra.errors.DecodingError"
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "expected "
  i32.const 0 ;; string: "int32"
  i32.const 0 ;; string: " literal"
  call $hydra.lib.strings.cat
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "integer"
  i32.const 0 ;; string: "hydra.core.IntegerValue"
  i32.const 0
  i32.const 0 ;; string: "hydra.errors.DecodingError"
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "expected "
  i32.const 0 ;; string: "int32"
  i32.const 0 ;; string: " value"
  call $hydra.lib.strings.cat
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "int32"
  i32.const 0 ;; string: "i"
  i32.const 0
  i32.const 1
  i32.const 0 ;; string: "i"
  i32.const 0 ;; string: "v"
  i32.const 0 ;; string: "stripped"
  i32.const 0 ;; string: "hydra.lexical.stripAndDereferenceTermEither"
  i32.const 0 ;; string: "cx"
  i32.const 0 ;; string: "raw"
  br $end_integer_type
)
  i32.const 0 ;; string: "cx"
  i32.const 0
  i32.const 0 ;; string: "raw"
  i32.const 0
  i32.const 0 ;; string: "hydra.lib.eithers.either"
  i32.const 0 ;; string: "err"
  i32.const 0
  i32.const 0
  i32.const 0 ;; string: "hydra.errors.DecodingError"
  i32.const 0 ;; string: "err"
  i32.const 0 ;; string: "stripped"
  i32.const 0
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 0
  i32.const 0 ;; string: "hydra.errors.DecodingError"
  i32.const 0 ;; string: "expected literal"
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "literal"
  i32.const 0 ;; string: "v"
  i32.const 0
  i32.const 0 ;; string: "hydra.core.Literal"
  i32.const 0
  i32.const 0 ;; string: "hydra.errors.DecodingError"
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "expected "
  i32.const 0 ;; string: "int64"
  i32.const 0 ;; string: " literal"
  call $hydra.lib.strings.cat
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "integer"
  i32.const 0 ;; string: "hydra.core.IntegerValue"
  i32.const 0
  i32.const 0 ;; string: "hydra.errors.DecodingError"
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "expected "
  i32.const 0 ;; string: "int64"
  i32.const 0 ;; string: " value"
  call $hydra.lib.strings.cat
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "int64"
  i32.const 0 ;; string: "i"
  i32.const 0
  i32.const 1
  i32.const 0 ;; string: "i"
  i32.const 0 ;; string: "v"
  i32.const 0 ;; string: "stripped"
  i32.const 0 ;; string: "hydra.lexical.stripAndDereferenceTermEither"
  i32.const 0 ;; string: "cx"
  i32.const 0 ;; string: "raw"
  br $end_integer_type
)
  i32.const 0 ;; string: "cx"
  i32.const 0
  i32.const 0 ;; string: "raw"
  i32.const 0
  i32.const 0 ;; string: "hydra.lib.eithers.either"
  i32.const 0 ;; string: "err"
  i32.const 0
  i32.const 0
  i32.const 0 ;; string: "hydra.errors.DecodingError"
  i32.const 0 ;; string: "err"
  i32.const 0 ;; string: "stripped"
  i32.const 0
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 0
  i32.const 0 ;; string: "hydra.errors.DecodingError"
  i32.const 0 ;; string: "expected literal"
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "literal"
  i32.const 0 ;; string: "v"
  i32.const 0
  i32.const 0 ;; string: "hydra.core.Literal"
  i32.const 0
  i32.const 0 ;; string: "hydra.errors.DecodingError"
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "expected "
  i32.const 0 ;; string: "uint8"
  i32.const 0 ;; string: " literal"
  call $hydra.lib.strings.cat
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "integer"
  i32.const 0 ;; string: "hydra.core.IntegerValue"
  i32.const 0
  i32.const 0 ;; string: "hydra.errors.DecodingError"
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "expected "
  i32.const 0 ;; string: "uint8"
  i32.const 0 ;; string: " value"
  call $hydra.lib.strings.cat
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "uint8"
  i32.const 0 ;; string: "i"
  i32.const 0
  i32.const 1
  i32.const 0 ;; string: "i"
  i32.const 0 ;; string: "v"
  i32.const 0 ;; string: "stripped"
  i32.const 0 ;; string: "hydra.lexical.stripAndDereferenceTermEither"
  i32.const 0 ;; string: "cx"
  i32.const 0 ;; string: "raw"
  br $end_integer_type
)
  i32.const 0 ;; string: "cx"
  i32.const 0
  i32.const 0 ;; string: "raw"
  i32.const 0
  i32.const 0 ;; string: "hydra.lib.eithers.either"
  i32.const 0 ;; string: "err"
  i32.const 0
  i32.const 0
  i32.const 0 ;; string: "hydra.errors.DecodingError"
  i32.const 0 ;; string: "err"
  i32.const 0 ;; string: "stripped"
  i32.const 0
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 0
  i32.const 0 ;; string: "hydra.errors.DecodingError"
  i32.const 0 ;; string: "expected literal"
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "literal"
  i32.const 0 ;; string: "v"
  i32.const 0
  i32.const 0 ;; string: "hydra.core.Literal"
  i32.const 0
  i32.const 0 ;; string: "hydra.errors.DecodingError"
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "expected "
  i32.const 0 ;; string: "uint16"
  i32.const 0 ;; string: " literal"
  call $hydra.lib.strings.cat
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "integer"
  i32.const 0 ;; string: "hydra.core.IntegerValue"
  i32.const 0
  i32.const 0 ;; string: "hydra.errors.DecodingError"
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "expected "
  i32.const 0 ;; string: "uint16"
  i32.const 0 ;; string: " value"
  call $hydra.lib.strings.cat
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "uint16"
  i32.const 0 ;; string: "i"
  i32.const 0
  i32.const 1
  i32.const 0 ;; string: "i"
  i32.const 0 ;; string: "v"
  i32.const 0 ;; string: "stripped"
  i32.const 0 ;; string: "hydra.lexical.stripAndDereferenceTermEither"
  i32.const 0 ;; string: "cx"
  i32.const 0 ;; string: "raw"
  br $end_integer_type
)
  i32.const 0 ;; string: "cx"
  i32.const 0
  i32.const 0 ;; string: "raw"
  i32.const 0
  i32.const 0 ;; string: "hydra.lib.eithers.either"
  i32.const 0 ;; string: "err"
  i32.const 0
  i32.const 0
  i32.const 0 ;; string: "hydra.errors.DecodingError"
  i32.const 0 ;; string: "err"
  i32.const 0 ;; string: "stripped"
  i32.const 0
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 0
  i32.const 0 ;; string: "hydra.errors.DecodingError"
  i32.const 0 ;; string: "expected literal"
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "literal"
  i32.const 0 ;; string: "v"
  i32.const 0
  i32.const 0 ;; string: "hydra.core.Literal"
  i32.const 0
  i32.const 0 ;; string: "hydra.errors.DecodingError"
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "expected "
  i32.const 0 ;; string: "uint32"
  i32.const 0 ;; string: " literal"
  call $hydra.lib.strings.cat
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "integer"
  i32.const 0 ;; string: "hydra.core.IntegerValue"
  i32.const 0
  i32.const 0 ;; string: "hydra.errors.DecodingError"
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "expected "
  i32.const 0 ;; string: "uint32"
  i32.const 0 ;; string: " value"
  call $hydra.lib.strings.cat
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "uint32"
  i32.const 0 ;; string: "i"
  i32.const 0
  i32.const 1
  i32.const 0 ;; string: "i"
  i32.const 0 ;; string: "v"
  i32.const 0 ;; string: "stripped"
  i32.const 0 ;; string: "hydra.lexical.stripAndDereferenceTermEither"
  i32.const 0 ;; string: "cx"
  i32.const 0 ;; string: "raw"
  br $end_integer_type
)
  i32.const 0 ;; string: "cx"
  i32.const 0
  i32.const 0 ;; string: "raw"
  i32.const 0
  i32.const 0 ;; string: "hydra.lib.eithers.either"
  i32.const 0 ;; string: "err"
  i32.const 0
  i32.const 0
  i32.const 0 ;; string: "hydra.errors.DecodingError"
  i32.const 0 ;; string: "err"
  i32.const 0 ;; string: "stripped"
  i32.const 0
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 0
  i32.const 0 ;; string: "hydra.errors.DecodingError"
  i32.const 0 ;; string: "expected literal"
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "literal"
  i32.const 0 ;; string: "v"
  i32.const 0
  i32.const 0 ;; string: "hydra.core.Literal"
  i32.const 0
  i32.const 0 ;; string: "hydra.errors.DecodingError"
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "expected "
  i32.const 0 ;; string: "uint64"
  i32.const 0 ;; string: " literal"
  call $hydra.lib.strings.cat
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "integer"
  i32.const 0 ;; string: "hydra.core.IntegerValue"
  i32.const 0
  i32.const 0 ;; string: "hydra.errors.DecodingError"
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "expected "
  i32.const 0 ;; string: "uint64"
  i32.const 0 ;; string: " value"
  call $hydra.lib.strings.cat
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "uint64"
  i32.const 0 ;; string: "i"
  i32.const 0
  i32.const 1
  i32.const 0 ;; string: "i"
  i32.const 0 ;; string: "v"
  i32.const 0 ;; string: "stripped"
  i32.const 0 ;; string: "hydra.lexical.stripAndDereferenceTermEither"
  i32.const 0 ;; string: "cx"
  i32.const 0 ;; string: "raw"
  br $end_integer_type
)
  br $end_literal_type
)
  i32.const 0 ;; string: "cx"
  i32.const 0
  i32.const 0 ;; string: "raw"
  i32.const 0
  i32.const 0 ;; string: "hydra.lib.eithers.either"
  i32.const 0 ;; string: "err"
  i32.const 0
  i32.const 0
  i32.const 0 ;; string: "hydra.errors.DecodingError"
  i32.const 0 ;; string: "err"
  i32.const 0 ;; string: "stripped"
  i32.const 0
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 0
  i32.const 0 ;; string: "hydra.errors.DecodingError"
  i32.const 0 ;; string: "expected literal"
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "literal"
  i32.const 0 ;; string: "v"
  i32.const 0
  i32.const 0 ;; string: "hydra.core.Literal"
  i32.const 0
  i32.const 0 ;; string: "hydra.errors.DecodingError"
  i32.const 0 ;; string: "expected string literal"
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "string"
  i32.const 0 ;; string: "s"
  i32.const 0
  i32.const 1
  i32.const 0 ;; string: "s"
  i32.const 0 ;; string: "v"
  i32.const 0 ;; string: "stripped"
  i32.const 0 ;; string: "hydra.lexical.stripAndDereferenceTermEither"
  i32.const 0 ;; string: "cx"
  i32.const 0 ;; string: "raw"
  br $end_literal_type
)
)
  (func $hydra.decoding.decode_map_type (param $mt i32) (result i32)
  (local $key_decoder i32)
  (local $val_decoder i32)
  local.get $mt
  ;; project field: keys
  call $hydra.decoding.decode_type
  local.set $key_decoder
  local.get $mt
  ;; project field: values
  call $hydra.decoding.decode_type
  local.set $val_decoder
  i32.const 0 ;; string: "hydra.extract.core.decodeMap"
  local.get $key_decoder
  local.get $val_decoder
)
  (func $hydra.decoding.decode_maybe_type (param $elem_type i32) (result i32)
  (local $elem_decoder i32)
  local.get $elem_type
  call $hydra.decoding.decode_type
  local.set $elem_decoder
  i32.const 0 ;; string: "hydra.extract.core.decodeMaybe"
  local.get $elem_decoder
)
  (func $hydra.decoding.decode_module (param $cx i32) (param $graph i32) (param $mod i32) (result i32)
  (local $all_decoded_deps i32)
  (local $b i32)
  (local $d i32)
  (local $data_term i32)
  (local $decoded_bindings i32)
  (local $decoded_term_deps i32)
  (local $decoded_type_deps i32)
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
  call $hydra.decoding.filter_type_bindings
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
  call $hydra.decoding.decode_binding
  call $hydra.lib.eithers.bimap
  local.get $type_bindings
  call $hydra.lib.eithers.map_list
  call $hydra.decoding.decode_namespace
  local.get $mod
  ;; project field: type_dependencies
  call $hydra.lib.lists.map
  local.set $decoded_type_deps
  call $hydra.decoding.decode_namespace
  local.get $mod
  ;; project field: term_dependencies
  call $hydra.lib.lists.map
  local.set $decoded_term_deps
  local.get $decoded_type_deps
  local.get $decoded_term_deps
  call $hydra.lib.lists.concat2
  call $hydra.lib.lists.nub
  local.set $all_decoded_deps
  i32.const 1
  local.get $mod
  ;; project field: namespace
  call $hydra.decoding.decode_namespace
  local.get $b
  ;; project field: name
  local.get $b
  ;; project field: term
  local.get $b
  ;; project field: type
  local.get $decoded_bindings
  call $hydra.lib.lists.map
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "hydra.extract.core"
  i32.const 0 ;; string: "hydra.lexical"
  i32.const 0 ;; string: "hydra.rewriting"
  local.get $all_decoded_deps
  call $hydra.lib.lists.concat2
  i32.const 2
  ;; list elements follow
  local.get $mod
  ;; project field: namespace
  i32.const 0 ;; string: "hydra.util"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "Term decoders for "
  nop
  call $hydra.lib.strings.cat
  call $hydra.lib.eithers.bind
  call $hydra.lib.logic.if_else
  call $hydra.lib.eithers.bind
)
  (func $hydra.decoding.decode_namespace (param $ns i32) (result i32)
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "hydra.decode."
  i32.const 0 ;; string: "."
  i32.const 0 ;; string: "."
  nop
  call $hydra.lib.strings.split_on
  call $hydra.lib.lists.tail
  call $hydra.lib.strings.intercalate
  call $hydra.lib.strings.cat
)
  (func $hydra.decoding.decode_pair_type (param $pt i32) (result i32)
  (local $first_decoder i32)
  (local $second_decoder i32)
  local.get $pt
  ;; project field: first
  call $hydra.decoding.decode_type
  local.set $first_decoder
  local.get $pt
  ;; project field: second
  call $hydra.decoding.decode_type
  local.set $second_decoder
  i32.const 0 ;; string: "hydra.extract.core.decodePair"
  local.get $first_decoder
  local.get $second_decoder
)
  (func $hydra.decoding.decode_record_type (param $rt i32) (result i32)
  i32.const 0 ;; string: "unknown"
  local.get $rt
  call $hydra.decoding.decode_record_type_impl
)
  (func $hydra.decoding.decode_record_type_impl (param $tname i32) (param $rt i32) (result i32)
  (local $acc i32)
  (local $body i32)
  (local $decode_body i32)
  (local $decode_field_term i32)
  (local $ft i32)
  (local $local_var_name i32)
  (local $to_field_lambda i32)
  i32.const 0 ;; string: "hydra.extract.core.requireField"
  nop
  local.get $ft
  ;; project field: type
  call $hydra.decoding.decode_type
  i32.const 0 ;; string: "fieldMap"
  i32.const 0 ;; string: "cx"
  local.set $decode_field_term
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "field_"
  nop
  call $hydra.lib.strings.cat
  local.set $local_var_name
  local.get $ft
  local.get $local_var_name
  i32.const 0
  local.get $body
  local.set $to_field_lambda
  i32.const 0 ;; string: "hydra.lib.eithers.bind"
  local.get $ft
  local.get $decode_field_term
  local.get $ft
  local.get $acc
  local.get $to_field_lambda
  i32.const 1
  local.get $tname
  local.get $ft
  ;; project field: name
  local.get $ft
  local.get $local_var_name
  local.get $rt
  call $hydra.lib.lists.map
  local.get $rt
  call $hydra.lib.lists.reverse
  call $hydra.lib.lists.foldl
  local.set $decode_body
  i32.const 0 ;; string: "cx"
  i32.const 0
  i32.const 0 ;; string: "raw"
  i32.const 0
  i32.const 0 ;; string: "hydra.lib.eithers.either"
  i32.const 0 ;; string: "err"
  i32.const 0
  i32.const 0
  i32.const 0 ;; string: "hydra.errors.DecodingError"
  i32.const 0 ;; string: "err"
  i32.const 0 ;; string: "stripped"
  i32.const 0
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 0
  i32.const 0 ;; string: "hydra.errors.DecodingError"
  i32.const 0 ;; string: "expected record"
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "record"
  i32.const 0 ;; string: "record"
  i32.const 0
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "fieldMap"
  i32.const 0 ;; string: "hydra.extract.core.toFieldMap"
  i32.const 0 ;; string: "record"
  i32.const 0
  local.get $decode_body
  i32.const 0 ;; string: "stripped"
  i32.const 0 ;; string: "hydra.lexical.stripAndDereferenceTermEither"
  i32.const 0 ;; string: "cx"
  i32.const 0 ;; string: "raw"
)
  (func $hydra.decoding.decode_record_type_named (param $ename i32) (param $rt i32) (result i32)
  local.get $ename
  local.get $rt
  call $hydra.decoding.decode_record_type_impl
)
  (func $hydra.decoding.decode_set_type (param $elem_type i32) (result i32)
  (local $elem_decoder i32)
  local.get $elem_type
  call $hydra.decoding.decode_type
  local.set $elem_decoder
  i32.const 0 ;; string: "hydra.extract.core.decodeSet"
  local.get $elem_decoder
)
  (func $hydra.decoding.decode_type (param $typ i32) (result i32)
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
  (block $wrap
  (block $void
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
  br_table $annotated $application $either $forall $list $literal $map $maybe $pair $record $set $union $unit $void $wrap $variable $variable
)
  local.get $at
  ;; project field: body
  call $hydra.decoding.decode_type
  br $end_type
)
  local.get $app_type
  ;; project field: function
  call $hydra.decoding.decode_type
  local.get $app_type
  ;; project field: argument
  call $hydra.decoding.decode_type
  br $end_type
)
  local.get $et
  call $hydra.decoding.decode_either_type
  br $end_type
)
  local.get $ft
  call $hydra.decoding.decode_forall_type
  br $end_type
)
  local.get $elem_type
  call $hydra.decoding.decode_list_type
  br $end_type
)
  local.get $lt
  call $hydra.decoding.decode_literal_type
  br $end_type
)
  local.get $mt
  call $hydra.decoding.decode_map_type
  br $end_type
)
  local.get $elem_type
  call $hydra.decoding.decode_maybe_type
  br $end_type
)
  local.get $pt
  call $hydra.decoding.decode_pair_type
  br $end_type
)
  local.get $rt
  call $hydra.decoding.decode_record_type
  br $end_type
)
  local.get $elem_type
  call $hydra.decoding.decode_set_type
  br $end_type
)
  local.get $rt
  call $hydra.decoding.decode_union_type
  br $end_type
)
  call $hydra.decoding.decode_unit_type
  br $end_type
)
  call $hydra.decoding.decode_unit_type
  br $end_type
)
  local.get $wt
  call $hydra.decoding.decode_wrapped_type
  br $end_type
)
  local.get $type_name
  call $hydra.decoding.decode_binding_name
  br $end_type
)
)
  (func $hydra.decoding.decode_type_named (param $ename i32) (param $typ i32) (result i32)
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
  (block $wrap
  (block $void
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
  br_table $annotated $application $either $forall $list $literal $map $maybe $pair $record $set $union $unit $void $wrap $variable $variable
)
  local.get $ename
  local.get $at
  ;; project field: body
  call $hydra.decoding.decode_type_named
  br $end_type
)
  local.get $app_type
  ;; project field: function
  call $hydra.decoding.decode_type
  local.get $app_type
  ;; project field: argument
  call $hydra.decoding.decode_type
  br $end_type
)
  local.get $et
  call $hydra.decoding.decode_either_type
  br $end_type
)
  local.get $ft
  ;; project field: parameter
  call $hydra.decoding.decode_binding_name
  i32.const 0
  local.get $ename
  local.get $ft
  ;; project field: body
  call $hydra.decoding.decode_type_named
  br $end_type
)
  local.get $elem_type
  call $hydra.decoding.decode_list_type
  br $end_type
)
  local.get $lt
  call $hydra.decoding.decode_literal_type
  br $end_type
)
  local.get $mt
  call $hydra.decoding.decode_map_type
  br $end_type
)
  local.get $elem_type
  call $hydra.decoding.decode_maybe_type
  br $end_type
)
  local.get $pt
  call $hydra.decoding.decode_pair_type
  br $end_type
)
  local.get $ename
  local.get $rt
  call $hydra.decoding.decode_record_type_named
  br $end_type
)
  local.get $elem_type
  call $hydra.decoding.decode_set_type
  br $end_type
)
  local.get $ename
  local.get $rt
  call $hydra.decoding.decode_union_type_named
  br $end_type
)
  call $hydra.decoding.decode_unit_type
  br $end_type
)
  call $hydra.decoding.decode_unit_type
  br $end_type
)
  local.get $ename
  local.get $wt
  call $hydra.decoding.decode_wrapped_type_named
  br $end_type
)
  local.get $type_name
  call $hydra.decoding.decode_binding_name
  br $end_type
)
)
  (func $hydra.decoding.decode_union_type (param $rt i32) (result i32)
  i32.const 0 ;; string: "unknown"
  local.get $rt
  call $hydra.decoding.decode_union_type_named
)
  (func $hydra.decoding.decode_union_type_named (param $ename i32) (param $rt i32) (result i32)
  (local $ft i32)
  (local $to_variant_pair i32)
  i32.const 0 ;; string: "hydra.core.Name"
  nop
  i32.const 0 ;; string: "input"
  i32.const 0
  i32.const 0 ;; string: "hydra.lib.eithers.map"
  i32.const 0 ;; string: "t"
  i32.const 0
  local.get $ename
  local.get $ft
  ;; project field: name
  i32.const 0 ;; string: "t"
  local.get $ft
  ;; project field: type
  call $hydra.decoding.decode_type
  i32.const 0 ;; string: "cx"
  i32.const 0 ;; string: "input"
  local.set $to_variant_pair
  i32.const 0 ;; string: "cx"
  i32.const 0
  i32.const 0 ;; string: "raw"
  i32.const 0
  i32.const 0 ;; string: "hydra.lib.eithers.either"
  i32.const 0 ;; string: "err"
  i32.const 0
  i32.const 0
  i32.const 0 ;; string: "hydra.errors.DecodingError"
  i32.const 0 ;; string: "err"
  i32.const 0 ;; string: "stripped"
  i32.const 0
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 0
  i32.const 0 ;; string: "hydra.errors.DecodingError"
  i32.const 0 ;; string: "expected union"
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "union"
  i32.const 0 ;; string: "inj"
  i32.const 0
  i32.const 4
  ;; list elements follow
  i32.const 0 ;; string: "field"
  i32.const 0 ;; string: "hydra.core.Injection"
  i32.const 0 ;; string: "field"
  i32.const 0 ;; string: "inj"
  i32.const 0
  i32.const 0 ;; string: "fname"
  i32.const 0 ;; string: "hydra.core.Field"
  i32.const 0 ;; string: "name"
  i32.const 0 ;; string: "field"
  i32.const 0
  i32.const 0 ;; string: "fterm"
  i32.const 0 ;; string: "hydra.core.Field"
  i32.const 0 ;; string: "term"
  i32.const 0 ;; string: "field"
  i32.const 0
  i32.const 0 ;; string: "variantMap"
  i32.const 0 ;; string: "hydra.lib.maps.fromList"
  local.get $to_variant_pair
  local.get $rt
  call $hydra.lib.lists.map
  i32.const 0
  i32.const 0 ;; string: "hydra.lib.maybes.maybe"
  i32.const 0
  i32.const 0 ;; string: "hydra.errors.DecodingError"
  i32.const 0 ;; string: "hydra.lib.strings.cat"
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "no such field "
  i32.const 0 ;; string: "hydra.core.Name"
  i32.const 0 ;; string: "fname"
  i32.const 0 ;; string: " in union"
  i32.const 0 ;; string: "f"
  i32.const 0
  i32.const 0 ;; string: "f"
  i32.const 0 ;; string: "fterm"
  i32.const 0 ;; string: "hydra.lib.maps.lookup"
  i32.const 0 ;; string: "fname"
  i32.const 0 ;; string: "variantMap"
  i32.const 0 ;; string: "stripped"
  i32.const 0 ;; string: "hydra.lexical.stripAndDereferenceTermEither"
  i32.const 0 ;; string: "cx"
  i32.const 0 ;; string: "raw"
)
  (func $hydra.decoding.decode_unit_type (result i32)
  i32.const 0 ;; string: "hydra.extract.core.decodeUnit"
)
  (func $hydra.decoding.decode_wrapped_type (param $wt i32) (result i32)
  i32.const 0 ;; string: "unknown"
  local.get $wt
  call $hydra.decoding.decode_wrapped_type_named
)
  (func $hydra.decoding.decode_wrapped_type_named (param $ename i32) (param $wt i32) (result i32)
  (local $body_decoder i32)
  local.get $wt
  call $hydra.decoding.decode_type
  local.set $body_decoder
  i32.const 0 ;; string: "cx"
  i32.const 0
  i32.const 0 ;; string: "raw"
  i32.const 0
  i32.const 0 ;; string: "hydra.lib.eithers.either"
  i32.const 0 ;; string: "err"
  i32.const 0
  i32.const 0
  i32.const 0 ;; string: "hydra.errors.DecodingError"
  i32.const 0 ;; string: "err"
  i32.const 0 ;; string: "stripped"
  i32.const 0
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 0
  i32.const 0 ;; string: "hydra.errors.DecodingError"
  i32.const 0 ;; string: "expected wrapped type"
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "wrap"
  i32.const 0 ;; string: "wrappedTerm"
  i32.const 0
  i32.const 0 ;; string: "hydra.lib.eithers.map"
  i32.const 0 ;; string: "b"
  i32.const 0
  local.get $ename
  i32.const 0 ;; string: "b"
  local.get $body_decoder
  i32.const 0 ;; string: "cx"
  i32.const 0 ;; string: "hydra.core.WrappedTerm"
  i32.const 0 ;; string: "body"
  i32.const 0 ;; string: "wrappedTerm"
  i32.const 0 ;; string: "stripped"
  i32.const 0 ;; string: "hydra.lexical.stripAndDereferenceTermEither"
  i32.const 0 ;; string: "cx"
  i32.const 0 ;; string: "raw"
)
  (func $hydra.decoding.decoder_full_result_type (param $typ i32) (result i32)
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
  call $hydra.decoding.decoder_full_result_type
  br $end_type
)
  local.get $app_type
  ;; project field: function
  call $hydra.decoding.decoder_full_result_type
  local.get $app_type
  ;; project field: argument
  br $end_type
)
  local.get $et
  ;; project field: left
  call $hydra.decoding.decoder_full_result_type
  local.get $et
  ;; project field: right
  call $hydra.decoding.decoder_full_result_type
  br $end_type
)
  local.get $ft
  ;; project field: body
  call $hydra.decoding.decoder_full_result_type
  local.get $ft
  ;; project field: parameter
  br $end_type
)
  local.get $elem_type
  call $hydra.decoding.decoder_full_result_type
  br $end_type
)
  i32.const 0 ;; string: "hydra.core.Literal"
  br $end_type
)
  local.get $mt
  ;; project field: keys
  call $hydra.decoding.decoder_full_result_type
  local.get $mt
  ;; project field: values
  call $hydra.decoding.decoder_full_result_type
  br $end_type
)
  local.get $elem_type
  call $hydra.decoding.decoder_full_result_type
  br $end_type
)
  local.get $pt
  ;; project field: first
  call $hydra.decoding.decoder_full_result_type
  local.get $pt
  ;; project field: second
  call $hydra.decoding.decoder_full_result_type
  br $end_type
)
  i32.const 0 ;; string: "hydra.core.Term"
  br $end_type
)
  local.get $elem_type
  call $hydra.decoding.decoder_full_result_type
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
  (func $hydra.decoding.decoder_full_result_type_named (param $ename i32) (param $typ i32) (result i32)
  (local $app_type i32)
  (local $at i32)
  (local $elem_type i32)
  (local $et i32)
  (local $ft i32)
  (local $mt i32)
  (local $name i32)
  (local $pt i32)
  (block $end_type (result i32)
  (block $void
  (block $variable
  (block $unit
  (block $set
  (block $pair
  (block $maybe
  (block $map
  (block $literal
  (block $list
  (block $either
  (block $application
  (block $wrap
  (block $union
  (block $record
  (block $forall
  (block $annotated
  local.get $typ
  br_table $annotated $forall $record $union $wrap $application $either $list $literal $map $maybe $pair $set $unit $variable $void $void
)
  local.get $ename
  local.get $at
  ;; project field: body
  call $hydra.decoding.decoder_full_result_type_named
  br $end_type
)
  local.get $ename
  local.get $ft
  ;; project field: body
  call $hydra.decoding.decoder_full_result_type_named
  local.get $ft
  ;; project field: parameter
  br $end_type
)
  local.get $ename
  br $end_type
)
  local.get $ename
  br $end_type
)
  local.get $ename
  br $end_type
)
  local.get $app_type
  ;; project field: function
  call $hydra.decoding.decoder_full_result_type
  local.get $app_type
  ;; project field: argument
  br $end_type
)
  local.get $et
  ;; project field: left
  call $hydra.decoding.decoder_full_result_type
  local.get $et
  ;; project field: right
  call $hydra.decoding.decoder_full_result_type
  br $end_type
)
  local.get $elem_type
  call $hydra.decoding.decoder_full_result_type
  br $end_type
)
  i32.const 0 ;; string: "hydra.core.Literal"
  br $end_type
)
  local.get $mt
  ;; project field: keys
  call $hydra.decoding.decoder_full_result_type
  local.get $mt
  ;; project field: values
  call $hydra.decoding.decoder_full_result_type
  br $end_type
)
  local.get $elem_type
  call $hydra.decoding.decoder_full_result_type
  br $end_type
)
  local.get $pt
  ;; project field: first
  call $hydra.decoding.decoder_full_result_type
  local.get $pt
  ;; project field: second
  call $hydra.decoding.decoder_full_result_type
  br $end_type
)
  local.get $elem_type
  call $hydra.decoding.decoder_full_result_type
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
)
  (func $hydra.decoding.decoder_result_type (param $typ i32) (result i32)
  (local $app_type i32)
  (local $at i32)
  (local $ft i32)
  (block $end_type (result i32)
  (block $wrap
  (block $union
  (block $record
  (block $literal
  (block $forall
  (block $application
  (block $annotated
  local.get $typ
  br_table $annotated $application $forall $literal $record $union $wrap $wrap
)
  local.get $at
  ;; project field: body
  call $hydra.decoding.decoder_result_type
  br $end_type
)
  local.get $app_type
  ;; project field: function
  call $hydra.decoding.decoder_result_type
  br $end_type
)
  local.get $ft
  ;; project field: body
  call $hydra.decoding.decoder_result_type
  br $end_type
)
  i32.const 0 ;; string: "hydra.core.Literal"
  br $end_type
)
  i32.const 0 ;; string: "hydra.core.Term"
  br $end_type
)
  i32.const 0 ;; string: "hydra.core.Term"
  br $end_type
)
  i32.const 0 ;; string: "hydra.core.Term"
  br $end_type
)
)
  (func $hydra.decoding.decoder_type (param $typ i32) (result i32)
  (local $base_type i32)
  (local $result_type i32)
  local.get $typ
  call $hydra.decoding.decoder_full_result_type
  local.set $result_type
  i32.const 0 ;; string: "hydra.graph.Graph"
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 0 ;; string: "hydra.errors.DecodingError"
  local.get $result_type
  local.set $base_type
  local.get $base_type
  local.get $typ
  call $hydra.decoding.prepend_forall_decoders
)
  (func $hydra.decoding.decoder_type_named (param $ename i32) (param $typ i32) (result i32)
  (local $base_type i32)
  (local $result_type i32)
  local.get $ename
  local.get $typ
  call $hydra.decoding.decoder_full_result_type_named
  local.set $result_type
  i32.const 0 ;; string: "hydra.graph.Graph"
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 0 ;; string: "hydra.errors.DecodingError"
  local.get $result_type
  local.set $base_type
  local.get $base_type
  local.get $typ
  call $hydra.decoding.prepend_forall_decoders
)
  (func $hydra.decoding.decoder_type_scheme (param $typ i32) (result i32)
  (local $all_ord_vars i32)
  (local $constraints i32)
  (local $ord_vars i32)
  (local $type_vars i32)
  (local $v i32)
  local.get $typ
  call $hydra.decoding.collect_type_variables
  local.set $type_vars
  local.get $typ
  call $hydra.decoding.collect_ord_constrained_variables
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
  local.get $typ
  call $hydra.decoding.decoder_type
  local.get $constraints
)
  (func $hydra.decoding.decoder_type_scheme_named (param $ename i32) (param $typ i32) (result i32)
  (local $all_ord_vars i32)
  (local $constraints i32)
  (local $ord_vars i32)
  (local $type_vars i32)
  (local $v i32)
  local.get $typ
  call $hydra.decoding.collect_type_variables
  local.set $type_vars
  local.get $typ
  call $hydra.decoding.collect_ord_constrained_variables
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
  local.get $ename
  local.get $typ
  call $hydra.decoding.decoder_type_named
  local.get $constraints
)
  (func $hydra.decoding.filter_type_bindings (param $cx i32) (param $graph i32) (param $bindings i32) (result i32)
  call $hydra.lib.maybes.cat
  local.get $cx
  local.get $graph
  call $hydra.decoding.is_decodable_binding
  call $hydra.annotations.is_native_type
  local.get $bindings
  call $hydra.lib.lists.filter
  call $hydra.lib.eithers.map_list
  call $hydra.lib.eithers.map
)
  (func $hydra.decoding.is_decodable_binding (param $cx i32) (param $graph i32) (param $b i32) (result i32)
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
  (func $hydra.decoding.prepend_forall_decoders (param $base_type i32) (param $typ i32) (result i32)
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
  call $hydra.decoding.prepend_forall_decoders
  br $end_type
)
  i32.const 0 ;; string: "hydra.graph.Graph"
  i32.const 0 ;; string: "hydra.core.Term"
  i32.const 0 ;; string: "hydra.errors.DecodingError"
  local.get $ft
  ;; project field: parameter
  local.get $base_type
  local.get $ft
  ;; project field: body
  call $hydra.decoding.prepend_forall_decoders
  br $end_type
)
)
)
