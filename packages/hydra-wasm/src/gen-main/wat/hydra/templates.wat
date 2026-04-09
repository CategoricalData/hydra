(module
  (import "hydra.decode.core" "hydra.decode.core.type" (func $hydra.decode.core.type (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.bimap" (func $hydra.lib.eithers.bimap (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.bind" (func $hydra.lib.eithers.bind (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map_list" (func $hydra.lib.eithers.map_list (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.if_else" (func $hydra.lib.logic.if_else (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.empty" (func $hydra.lib.maps.empty (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.from_list" (func $hydra.lib.maps.from_list (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.lookup" (func $hydra.lib.maps.lookup (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.singleton" (func $hydra.lib.maps.singleton (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.maybe" (func $hydra.lib.maybes.maybe (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.empty" (func $hydra.lib.sets.empty (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.from_list" (func $hydra.lib.sets.from_list (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat2" (func $hydra.lib.strings.cat2 (param i32) (result i32) ) )
  (import "hydra.show.core" "hydra.show.core.term" (func $hydra.show.core.term (param i32) (result i32) ) )
  (memory $memory 1 )
  (export "memory" (memory $memory) )
  (export "hydra.templates.graph_to_schema" (func $hydra.templates.graph_to_schema) )
  (export "hydra.templates.instantiate_template" (func $hydra.templates.instantiate_template) )
  (func $hydra.templates.graph_to_schema (param $cx i32) (param $graph i32) (param $els i32) (result i32)
  (local $_wc_a i32)
  (local $_wc_e i32)
  (local $el i32)
  (local $name i32)
  (local $pairs i32)
  (local $t i32)
  (local $to_pair i32)
  local.get $el
  ;; project field: name
  local.set $name
  local.get $_wc_e
  local.get $cx
  local.get $_wc_a
  local.get $graph
  local.get $el
  ;; project field: term
  call $hydra.decode.core.type
  call $hydra.lib.eithers.bimap
  i32.const 1
  local.get $name
  local.get $t
  call $hydra.lib.eithers.bind
  local.set $to_pair
  local.get $to_pair
  local.get $els
  call $hydra.lib.eithers.map_list
  i32.const 1
  local.get $pairs
  call $hydra.lib.maps.from_list
  call $hydra.lib.eithers.bind
)
  (func $hydra.templates.instantiate_template (param $cx i32) (param $minimal i32) (param $schema i32) (param $tname i32) (param $t i32) (result i32)
  (local $at i32)
  (local $dfields i32)
  (local $e i32)
  (local $et i32)
  (local $for_float i32)
  (local $for_integer i32)
  (local $for_literal i32)
  (local $ft i32)
  (local $inst i32)
  (local $it i32)
  (local $ke i32)
  (local $kt i32)
  (local $lt i32)
  (local $mt i32)
  (local $no_poly i32)
  (local $ot i32)
  (local $rt i32)
  (local $tn i32)
  (local $to_field i32)
  (local $ve i32)
  (local $vname i32)
  (local $vt i32)
  (local $wt i32)
  local.get $cx
  local.get $minimal
  local.get $schema
  local.get $tn
  call $hydra.templates.instantiate_template
  local.set $inst
  i32.const 0
  i32.const 0 ;; string: "Polymorphic and function types are not currently supported"
  local.get $cx
  local.set $no_poly
  (block $end_float_type (result i32)
  (block $float64
  (block $float32
  (block $bigfloat
  local.get $ft
  br_table $bigfloat $float32 $float64 $float64
)
  f64.const 0.0
  br $end_float_type
)
  f32.const 0.0
  br $end_float_type
)
  f64.const 0.0
  br $end_float_type
)
  local.set $for_float
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
  i32.const 0 ;; string: "0:bigint"
  br $end_integer_type
)
  i32.const 0
  br $end_integer_type
)
  i32.const 0
  br $end_integer_type
)
  i32.const 0
  br $end_integer_type
)
  i64.const 0
  br $end_integer_type
)
  i32.const 0
  br $end_integer_type
)
  i32.const 0
  br $end_integer_type
)
  i32.const 0
  br $end_integer_type
)
  i64.const 0
  br $end_integer_type
)
  local.set $for_integer
  (block $end_literal_type (result i32)
  (block $string
  (block $float
  (block $integer
  (block $boolean
  (block $binary
  local.get $lt
  br_table $binary $boolean $integer $float $string $string
)
  i32.const 0 ;; string: ""
  br $end_literal_type
)
  i32.const 0
  br $end_literal_type
)
  local.get $it
  local.get $for_integer
  br $end_literal_type
)
  local.get $ft
  local.get $for_float
  br $end_literal_type
)
  i32.const 0 ;; string: ""
  br $end_literal_type
)
  local.set $for_literal
  (block $end_type (result i32)
  (block $wrap
  (block $variable
  (block $set
  (block $record
  (block $maybe
  (block $map
  (block $literal
  (block $list
  (block $forall
  (block $function
  (block $application
  (block $annotated
  local.get $t
  br_table $annotated $application $function $forall $list $literal $map $maybe $record $set $variable $wrap $wrap
)
  local.get $tname
  local.get $at
  ;; project field: body
  local.get $inst
  br $end_type
)
  local.get $no_poly
  br $end_type
)
  local.get $no_poly
  br $end_type
)
  local.get $no_poly
  br $end_type
)
  local.get $minimal
  i32.const 1
  i32.const 0
  ;; list elements follow
  local.get $tname
  local.get $et
  local.get $inst
  i32.const 1
  i32.const 1
  ;; list elements follow
  local.get $e
  call $hydra.lib.eithers.bind
  call $hydra.lib.logic.if_else
  br $end_type
)
  i32.const 1
  local.get $lt
  local.get $for_literal
  br $end_type
)
  local.get $mt
  ;; project field: keys
  local.set $kt
  local.get $mt
  ;; project field: values
  local.set $vt
  local.get $minimal
  i32.const 1
  call $hydra.lib.maps.empty
  local.get $tname
  local.get $kt
  local.get $inst
  local.get $tname
  local.get $vt
  local.get $inst
  i32.const 1
  local.get $ke
  local.get $ve
  call $hydra.lib.maps.singleton
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  call $hydra.lib.logic.if_else
  br $end_type
)
  local.get $minimal
  i32.const 1
  i32.const 0
  local.get $tname
  local.get $ot
  local.get $inst
  i32.const 1
  local.get $e
  call $hydra.lib.eithers.bind
  call $hydra.lib.logic.if_else
  br $end_type
)
  local.get $tname
  local.get $ft
  ;; project field: type
  local.get $inst
  i32.const 1
  local.get $ft
  ;; project field: name
  local.get $e
  call $hydra.lib.eithers.bind
  local.set $to_field
  local.get $to_field
  local.get $rt
  call $hydra.lib.eithers.map_list
  i32.const 1
  local.get $tname
  local.get $dfields
  call $hydra.lib.eithers.bind
  br $end_type
)
  local.get $minimal
  i32.const 1
  call $hydra.lib.sets.empty
  local.get $tname
  local.get $et
  local.get $inst
  i32.const 1
  i32.const 1
  ;; list elements follow
  local.get $e
  call $hydra.lib.sets.from_list
  call $hydra.lib.eithers.bind
  call $hydra.lib.logic.if_else
  br $end_type
)
  i32.const 0
  i32.const 0 ;; string: "Type variable "
  local.get $vname
  call $hydra.show.core.term
  i32.const 0 ;; string: " not found in schema"
  call $hydra.lib.strings.cat2
  call $hydra.lib.strings.cat2
  local.get $cx
  local.get $vname
  local.get $inst
  local.get $vname
  local.get $schema
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.maybe
  br $end_type
)
  local.get $tname
  local.get $wt
  local.get $inst
  i32.const 1
  local.get $tname
  local.get $e
  call $hydra.lib.eithers.bind
  br $end_type
)
)
)
