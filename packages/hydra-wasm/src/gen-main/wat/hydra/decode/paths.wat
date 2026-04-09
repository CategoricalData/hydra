(module
  (import "hydra.decode.core" "hydra.decode.core.name" (func $hydra.decode.core.name (param i32) (result i32) ) )
  (import "hydra.extract.core" "hydra.extract.core.decode_list" (func $hydra.extract.core.decode_list (param i32) (result i32) ) )
  (import "hydra.extract.core" "hydra.extract.core.decode_unit" (func $hydra.extract.core.decode_unit (param i32) (result i32) ) )
  (import "hydra.extract.core" "hydra.extract.core.require_field" (func $hydra.extract.core.require_field (param i32) (result i32) ) )
  (import "hydra.extract.core" "hydra.extract.core.to_field_map" (func $hydra.extract.core.to_field_map (param i32) (result i32) ) )
  (import "hydra.lexical" "hydra.lexical.strip_and_dereference_term_either" (func $hydra.lexical.strip_and_dereference_term_either (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.bind" (func $hydra.lib.eithers.bind (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.either" (func $hydra.lib.eithers.either (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map" (func $hydra.lib.eithers.map (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.from_list" (func $hydra.lib.maps.from_list (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.lookup" (func $hydra.lib.maps.lookup (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.maybe" (func $hydra.lib.maybes.maybe (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat" (func $hydra.lib.strings.cat (param i32) (result i32) ) )
  (memory $memory 1 )
  (export "memory" (memory $memory) )
  (export "hydra.decode.paths.subterm_edge" (func $hydra.decode.paths.subterm_edge) )
  (export "hydra.decode.paths.subterm_graph" (func $hydra.decode.paths.subterm_graph) )
  (export "hydra.decode.paths.subterm_node" (func $hydra.decode.paths.subterm_node) )
  (export "hydra.decode.paths.subterm_path" (func $hydra.decode.paths.subterm_path) )
  (export "hydra.decode.paths.subterm_step" (func $hydra.decode.paths.subterm_step) )
  (export "hydra.decode.paths.subtype_edge" (func $hydra.decode.paths.subtype_edge) )
  (export "hydra.decode.paths.subtype_graph" (func $hydra.decode.paths.subtype_graph) )
  (export "hydra.decode.paths.subtype_node" (func $hydra.decode.paths.subtype_node) )
  (export "hydra.decode.paths.subtype_path" (func $hydra.decode.paths.subtype_path) )
  (export "hydra.decode.paths.subtype_step" (func $hydra.decode.paths.subtype_step) )
  (func $hydra.decode.paths.subterm_edge (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_map i32)
  (local $field_path i32)
  (local $field_source i32)
  (local $field_target i32)
  (local $record i32)
  (local $stripped i32)
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  br_table $record $record
)
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 0 ;; string: "source"
  call $hydra.decode.paths.subterm_node
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "path"
  call $hydra.decode.paths.subterm_path
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "target"
  call $hydra.decode.paths.subterm_node
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_source
  local.get $field_path
  local.get $field_target
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.paths.subterm_graph (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_edges i32)
  (local $field_map i32)
  (local $field_nodes i32)
  (local $record i32)
  (local $stripped i32)
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  br_table $record $record
)
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 0 ;; string: "nodes"
  call $hydra.decode.paths.subterm_node
  call $hydra.extract.core.decode_list
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "edges"
  call $hydra.decode.paths.subterm_edge
  call $hydra.extract.core.decode_list
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_nodes
  local.get $field_edges
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.paths.subterm_node (param $cx i32) (param $raw i32) (result i32)
  (local $cx2 i32)
  (local $err i32)
  (local $field_id i32)
  (local $field_label i32)
  (local $field_map i32)
  (local $field_name i32)
  (local $raw2 i32)
  (local $record i32)
  (local $s i32)
  (local $stripped i32)
  (local $stripped2 i32)
  (local $v i32)
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  br_table $record $record
)
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 0 ;; string: "name"
  call $hydra.decode.core.name
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "label"
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $literal
  local.get $stripped2
  br_table $literal $literal
)
  (block $end_literal (result i32)
  (block $string
  local.get $v
  br_table $string $string
)
  i32.const 1
  local.get $s
  br $end_literal
)
  br $end_term
)
  local.get $cx2
  local.get $raw2
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "id"
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $literal
  local.get $stripped2
  br_table $literal $literal
)
  (block $end_literal (result i32)
  (block $string
  local.get $v
  br_table $string $string
)
  i32.const 1
  local.get $s
  br $end_literal
)
  br $end_term
)
  local.get $cx2
  local.get $raw2
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_name
  local.get $field_label
  local.get $field_id
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.paths.subterm_path (param $cx i32) (param $raw i32) (result i32)
  (local $b i32)
  (local $err i32)
  (local $stripped i32)
  (local $wrapped_term i32)
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $wrap
  local.get $stripped
  br_table $wrap $wrap
)
  local.get $b
  call $hydra.decode.paths.subterm_step
  local.get $cx
  local.get $wrapped_term
  ;; project field: body
  call $hydra.extract.core.decode_list
  call $hydra.lib.eithers.map
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.paths.subterm_step (param $cx i32) (param $raw i32) (result i32)
  (local $cx2 i32)
  (local $err i32)
  (local $f i32)
  (local $field i32)
  (local $fname i32)
  (local $fterm i32)
  (local $i i32)
  (local $inj i32)
  (local $input i32)
  (local $raw2 i32)
  (local $stripped i32)
  (local $stripped2 i32)
  (local $t i32)
  (local $v i32)
  (local $variant_map i32)
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $union
  local.get $stripped
  br_table $union $union
)
  local.get $inj
  ;; project field: field
  local.set $field
  local.get $field
  ;; project field: name
  local.set $fname
  local.get $field
  ;; project field: term
  local.set $fterm
  i32.const 20
  ;; list elements follow
  i32.const 0 ;; string: "annotatedBody"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.extract.core.decode_unit
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "applicationFunction"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.extract.core.decode_unit
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "applicationArgument"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.extract.core.decode_unit
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "lambdaBody"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.extract.core.decode_unit
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "unionCasesDefault"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.extract.core.decode_unit
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "unionCasesBranch"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.core.name
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "letBody"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.extract.core.decode_unit
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "letBinding"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.core.name
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "listElement"
  local.get $t
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $literal
  local.get $stripped2
  br_table $literal $literal
)
  (block $end_literal (result i32)
  (block $integer
  local.get $v
  br_table $integer $integer
)
  (block $end_integer_value (result i32)
  (block $int32
  local.get $v
  br_table $int32 $int32
)
  i32.const 1
  local.get $i
  br $end_integer_value
)
  br $end_literal
)
  br $end_term
)
  local.get $cx2
  local.get $raw2
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "mapKey"
  local.get $t
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $literal
  local.get $stripped2
  br_table $literal $literal
)
  (block $end_literal (result i32)
  (block $integer
  local.get $v
  br_table $integer $integer
)
  (block $end_integer_value (result i32)
  (block $int32
  local.get $v
  br_table $int32 $int32
)
  i32.const 1
  local.get $i
  br $end_integer_value
)
  br $end_literal
)
  br $end_term
)
  local.get $cx2
  local.get $raw2
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "mapValue"
  local.get $t
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $literal
  local.get $stripped2
  br_table $literal $literal
)
  (block $end_literal (result i32)
  (block $integer
  local.get $v
  br_table $integer $integer
)
  (block $end_integer_value (result i32)
  (block $int32
  local.get $v
  br_table $int32 $int32
)
  i32.const 1
  local.get $i
  br $end_integer_value
)
  br $end_literal
)
  br $end_term
)
  local.get $cx2
  local.get $raw2
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "maybeTerm"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.extract.core.decode_unit
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "productTerm"
  local.get $t
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $literal
  local.get $stripped2
  br_table $literal $literal
)
  (block $end_literal (result i32)
  (block $integer
  local.get $v
  br_table $integer $integer
)
  (block $end_integer_value (result i32)
  (block $int32
  local.get $v
  br_table $int32 $int32
)
  i32.const 1
  local.get $i
  br $end_integer_value
)
  br $end_literal
)
  br $end_term
)
  local.get $cx2
  local.get $raw2
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "recordField"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.core.name
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "setElement"
  local.get $t
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $literal
  local.get $stripped2
  br_table $literal $literal
)
  (block $end_literal (result i32)
  (block $integer
  local.get $v
  br_table $integer $integer
)
  (block $end_integer_value (result i32)
  (block $int32
  local.get $v
  br_table $int32 $int32
)
  i32.const 1
  local.get $i
  br $end_integer_value
)
  br $end_literal
)
  br $end_term
)
  local.get $cx2
  local.get $raw2
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "sumTerm"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.extract.core.decode_unit
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "typeLambdaBody"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.extract.core.decode_unit
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "typeApplicationTerm"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.extract.core.decode_unit
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "injectionTerm"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.extract.core.decode_unit
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "wrappedTerm"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.extract.core.decode_unit
  call $hydra.lib.eithers.map
  call $hydra.lib.maps.from_list
  local.set $variant_map
  i32.const 0
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "no such field "
  nop
  i32.const 0 ;; string: " in union"
  call $hydra.lib.strings.cat
  local.get $fterm
  local.get $f
  local.get $fname
  local.get $variant_map
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.maybe
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.paths.subtype_edge (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_map i32)
  (local $field_path i32)
  (local $field_source i32)
  (local $field_target i32)
  (local $record i32)
  (local $stripped i32)
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  br_table $record $record
)
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 0 ;; string: "source"
  call $hydra.decode.paths.subtype_node
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "path"
  call $hydra.decode.paths.subtype_path
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "target"
  call $hydra.decode.paths.subtype_node
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_source
  local.get $field_path
  local.get $field_target
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.paths.subtype_graph (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_edges i32)
  (local $field_map i32)
  (local $field_nodes i32)
  (local $record i32)
  (local $stripped i32)
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  br_table $record $record
)
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 0 ;; string: "nodes"
  call $hydra.decode.paths.subtype_node
  call $hydra.extract.core.decode_list
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "edges"
  call $hydra.decode.paths.subtype_edge
  call $hydra.extract.core.decode_list
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_nodes
  local.get $field_edges
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.paths.subtype_node (param $cx i32) (param $raw i32) (result i32)
  (local $cx2 i32)
  (local $err i32)
  (local $field_id i32)
  (local $field_label i32)
  (local $field_map i32)
  (local $field_name i32)
  (local $raw2 i32)
  (local $record i32)
  (local $s i32)
  (local $stripped i32)
  (local $stripped2 i32)
  (local $v i32)
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  br_table $record $record
)
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 0 ;; string: "name"
  call $hydra.decode.core.name
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "label"
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $literal
  local.get $stripped2
  br_table $literal $literal
)
  (block $end_literal (result i32)
  (block $string
  local.get $v
  br_table $string $string
)
  i32.const 1
  local.get $s
  br $end_literal
)
  br $end_term
)
  local.get $cx2
  local.get $raw2
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "id"
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $literal
  local.get $stripped2
  br_table $literal $literal
)
  (block $end_literal (result i32)
  (block $string
  local.get $v
  br_table $string $string
)
  i32.const 1
  local.get $s
  br $end_literal
)
  br $end_term
)
  local.get $cx2
  local.get $raw2
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_name
  local.get $field_label
  local.get $field_id
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.paths.subtype_path (param $cx i32) (param $raw i32) (result i32)
  (local $b i32)
  (local $err i32)
  (local $stripped i32)
  (local $wrapped_term i32)
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $wrap
  local.get $stripped
  br_table $wrap $wrap
)
  local.get $b
  call $hydra.decode.paths.subtype_step
  local.get $cx
  local.get $wrapped_term
  ;; project field: body
  call $hydra.extract.core.decode_list
  call $hydra.lib.eithers.map
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.paths.subtype_step (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $f i32)
  (local $field i32)
  (local $fname i32)
  (local $fterm i32)
  (local $inj i32)
  (local $input i32)
  (local $stripped i32)
  (local $t i32)
  (local $variant_map i32)
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $union
  local.get $stripped
  br_table $union $union
)
  local.get $inj
  ;; project field: field
  local.set $field
  local.get $field
  ;; project field: name
  local.set $fname
  local.get $field
  ;; project field: term
  local.set $fterm
  i32.const 18
  ;; list elements follow
  i32.const 0 ;; string: "annotatedBody"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.extract.core.decode_unit
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "applicationFunction"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.extract.core.decode_unit
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "applicationArgument"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.extract.core.decode_unit
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "eitherLeft"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.extract.core.decode_unit
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "eitherRight"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.extract.core.decode_unit
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "forallBody"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.extract.core.decode_unit
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "functionDomain"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.extract.core.decode_unit
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "functionCodomain"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.extract.core.decode_unit
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "listElement"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.extract.core.decode_unit
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "mapKeys"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.extract.core.decode_unit
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "mapValues"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.extract.core.decode_unit
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "maybeElement"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.extract.core.decode_unit
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "pairFirst"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.extract.core.decode_unit
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "pairSecond"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.extract.core.decode_unit
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "recordField"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.core.name
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "setElement"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.extract.core.decode_unit
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "unionField"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.core.name
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "wrappedType"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.extract.core.decode_unit
  call $hydra.lib.eithers.map
  call $hydra.lib.maps.from_list
  local.set $variant_map
  i32.const 0
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "no such field "
  nop
  i32.const 0 ;; string: " in union"
  call $hydra.lib.strings.cat
  local.get $fterm
  local.get $f
  local.get $fname
  local.get $variant_map
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.maybe
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
)
