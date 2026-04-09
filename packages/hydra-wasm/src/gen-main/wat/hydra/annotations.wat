(module
  (import "hydra.constants" "hydra.constants.key_classes" (func $hydra.constants.key_classes (param i32) (result i32) ) )
  (import "hydra.constants" "hydra.constants.key_debug_id" (func $hydra.constants.key_debug_id (param i32) (result i32) ) )
  (import "hydra.constants" "hydra.constants.key_description" (func $hydra.constants.key_description (param i32) (result i32) ) )
  (import "hydra.constants" "hydra.constants.key_first_class_type" (func $hydra.constants.key_first_class_type (param i32) (result i32) ) )
  (import "hydra.constants" "hydra.constants.key_type" (func $hydra.constants.key_type (param i32) (result i32) ) )
  (import "hydra.decode.core" "hydra.decode.core.name" (func $hydra.decode.core.name (param i32) (result i32) ) )
  (import "hydra.decode.core" "hydra.decode.core.type" (func $hydra.decode.core.type (param i32) (result i32) ) )
  (import "hydra.encode.core" "hydra.encode.core.name" (func $hydra.encode.core.name (param i32) (result i32) ) )
  (import "hydra.encode.core" "hydra.encode.core.type" (func $hydra.encode.core.type (param i32) (result i32) ) )
  (import "hydra.extract.core" "hydra.extract.core.boolean" (func $hydra.extract.core.boolean (param i32) (result i32) ) )
  (import "hydra.extract.core" "hydra.extract.core.map" (func $hydra.extract.core.map (param i32) (result i32) ) )
  (import "hydra.extract.core" "hydra.extract.core.set_of" (func $hydra.extract.core.set_of (param i32) (result i32) ) )
  (import "hydra.extract.core" "hydra.extract.core.string" (func $hydra.extract.core.string (param i32) (result i32) ) )
  (import "hydra.extract.core" "hydra.extract.core.unit_variant" (func $hydra.extract.core.unit_variant (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.bimap" (func $hydra.lib.eithers.bimap (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.bind" (func $hydra.lib.eithers.bind (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map" (func $hydra.lib.eithers.map (param i32) (result i32) ) )
  (import "hydra.lib.equality" "hydra.lib.equality.equal" (func $hydra.lib.equality.equal (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.concat" (func $hydra.lib.lists.concat (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.cons" (func $hydra.lib.lists.cons (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.and" (func $hydra.lib.logic.and (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.if_else" (func $hydra.lib.logic.if_else (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.not" (func $hydra.lib.logic.not (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.alter" (func $hydra.lib.maps.alter (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.empty" (func $hydra.lib.maps.empty (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.from_list" (func $hydra.lib.maps.from_list (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.insert" (func $hydra.lib.maps.insert (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.lookup" (func $hydra.lib.maps.lookup (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.null" (func $hydra.lib.maps.null (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.to_list" (func $hydra.lib.maps.to_list (param i32) (result i32) ) )
  (import "hydra.lib.math" "hydra.lib.math.add" (func $hydra.lib.math.add (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.from_maybe" (func $hydra.lib.maybes.from_maybe (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.is_just" (func $hydra.lib.maybes.is_just (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.map" (func $hydra.lib.maybes.map (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.maybe" (func $hydra.lib.maybes.maybe (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.pure" (func $hydra.lib.maybes.pure (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.first" (func $hydra.lib.pairs.first (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.second" (func $hydra.lib.pairs.second (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.empty" (func $hydra.lib.sets.empty (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.from_list" (func $hydra.lib.sets.from_list (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.to_list" (func $hydra.lib.sets.to_list (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat2" (func $hydra.lib.strings.cat2 (param i32) (result i32) ) )
  (import "hydra.show.core" "hydra.show.core.term" (func $hydra.show.core.term (param i32) (result i32) ) )
  (import "hydra.strip" "hydra.strip.deannotate_term" (func $hydra.strip.deannotate_term (param i32) (result i32) ) )
  (import "hydra.strip" "hydra.strip.deannotate_type" (func $hydra.strip.deannotate_type (param i32) (result i32) ) )
  (memory $memory 1 )
  (export "memory" (memory $memory) )
  (export "hydra.annotations.aggregate_annotations" (func $hydra.annotations.aggregate_annotations) )
  (export "hydra.annotations.comments_from_binding" (func $hydra.annotations.comments_from_binding) )
  (export "hydra.annotations.comments_from_field_type" (func $hydra.annotations.comments_from_field_type) )
  (export "hydra.annotations.debug_if" (func $hydra.annotations.debug_if) )
  (export "hydra.annotations.fail_on_flag" (func $hydra.annotations.fail_on_flag) )
  (export "hydra.annotations.get_attr" (func $hydra.annotations.get_attr) )
  (export "hydra.annotations.get_attr_with_default" (func $hydra.annotations.get_attr_with_default) )
  (export "hydra.annotations.get_count" (func $hydra.annotations.get_count) )
  (export "hydra.annotations.get_debug_id" (func $hydra.annotations.get_debug_id) )
  (export "hydra.annotations.get_description" (func $hydra.annotations.get_description) )
  (export "hydra.annotations.get_term_annotation" (func $hydra.annotations.get_term_annotation) )
  (export "hydra.annotations.get_term_description" (func $hydra.annotations.get_term_description) )
  (export "hydra.annotations.get_type" (func $hydra.annotations.get_type) )
  (export "hydra.annotations.get_type_annotation" (func $hydra.annotations.get_type_annotation) )
  (export "hydra.annotations.get_type_classes" (func $hydra.annotations.get_type_classes) )
  (export "hydra.annotations.get_type_description" (func $hydra.annotations.get_type_description) )
  (export "hydra.annotations.has_description" (func $hydra.annotations.has_description) )
  (export "hydra.annotations.has_flag" (func $hydra.annotations.has_flag) )
  (export "hydra.annotations.has_type_description" (func $hydra.annotations.has_type_description) )
  (export "hydra.annotations.is_native_type" (func $hydra.annotations.is_native_type) )
  (export "hydra.annotations.next_count" (func $hydra.annotations.next_count) )
  (export "hydra.annotations.normalize_term_annotations" (func $hydra.annotations.normalize_term_annotations) )
  (export "hydra.annotations.normalize_type_annotations" (func $hydra.annotations.normalize_type_annotations) )
  (export "hydra.annotations.put_attr" (func $hydra.annotations.put_attr) )
  (export "hydra.annotations.put_count" (func $hydra.annotations.put_count) )
  (export "hydra.annotations.reset_count" (func $hydra.annotations.reset_count) )
  (export "hydra.annotations.set_annotation" (func $hydra.annotations.set_annotation) )
  (export "hydra.annotations.set_description" (func $hydra.annotations.set_description) )
  (export "hydra.annotations.set_term_annotation" (func $hydra.annotations.set_term_annotation) )
  (export "hydra.annotations.set_term_description" (func $hydra.annotations.set_term_description) )
  (export "hydra.annotations.set_type" (func $hydra.annotations.set_type) )
  (export "hydra.annotations.set_type_annotation" (func $hydra.annotations.set_type_annotation) )
  (export "hydra.annotations.set_type_classes" (func $hydra.annotations.set_type_classes) )
  (export "hydra.annotations.set_type_description" (func $hydra.annotations.set_type_description) )
  (export "hydra.annotations.term_annotation_internal" (func $hydra.annotations.term_annotation_internal) )
  (export "hydra.annotations.type_annotation_internal" (func $hydra.annotations.type_annotation_internal) )
  (export "hydra.annotations.when_flag" (func $hydra.annotations.when_flag) )
  (func $hydra.annotations.aggregate_annotations (param $get_value i32) (param $get_x i32) (param $get_anns i32) (param $t i32) (result i32)
  (local $rest i32)
  (local $t2 i32)
  (local $to_pairs i32)
  (local $yy i32)
  local.get $rest
  local.get $yy
  local.get $get_anns
  call $hydra.lib.maps.to_list
  local.get $rest
  call $hydra.lib.lists.cons
  local.get $yy
  local.get $get_x
  local.get $to_pairs
  local.get $t2
  local.get $get_value
  call $hydra.lib.maybes.maybe
  local.set $to_pairs
  i32.const 0
  ;; list elements follow
  local.get $t
  local.get $to_pairs
  call $hydra.lib.lists.concat
  call $hydra.lib.maps.from_list
)
  (func $hydra.annotations.comments_from_binding (param $cx i32) (param $g i32) (param $b i32) (result i32)
  local.get $cx
  local.get $g
  local.get $b
  ;; project field: term
  call $hydra.annotations.get_term_description
)
  (func $hydra.annotations.comments_from_field_type (param $cx i32) (param $g i32) (param $ft i32) (result i32)
  local.get $cx
  local.get $g
  local.get $ft
  ;; project field: type
  call $hydra.annotations.get_type_description
)
  (func $hydra.annotations.debug_if (param $cx i32) (param $debug_id i32) (param $message i32) (result i32)
  (local $mid i32)
  local.get $cx
  call $hydra.annotations.get_debug_id
  local.get $mid
  local.get $debug_id
  call $hydra.lib.equality.equal
  i32.const 0
  local.get $message
  local.get $cx
  i32.const 1
  call $hydra.lib.logic.if_else
  call $hydra.lib.eithers.bind
)
  (func $hydra.annotations.fail_on_flag (param $cx i32) (param $flag i32) (param $msg i32) (result i32)
  (local $val i32)
  local.get $cx
  local.get $flag
  call $hydra.annotations.has_flag
  local.get $val
  i32.const 0
  local.get $msg
  local.get $cx
  i32.const 1
  call $hydra.lib.logic.if_else
  call $hydra.lib.eithers.bind
)
  (func $hydra.annotations.get_attr (param $key i32) (param $cx i32) (result i32)
  local.get $key
  local.get $cx
  ;; project field: other
  call $hydra.lib.maps.lookup
)
  (func $hydra.annotations.get_attr_with_default (param $key i32) (param $def i32) (param $cx i32) (result i32)
  local.get $def
  local.get $key
  local.get $cx
  call $hydra.annotations.get_attr
  call $hydra.lib.maybes.from_maybe
)
  (func $hydra.annotations.get_count (param $key i32) (param $cx i32) (result i32)
  (local $i i32)
  (local $iv i32)
  (local $lit i32)
  (local $term i32)
  i32.const 0
  (block $end_term (result i32)
  (block $literal
  local.get $term
  br_table $literal $literal
)
  (block $end_literal (result i32)
  (block $integer
  local.get $lit
  br_table $integer $integer
)
  (block $end_integer_value (result i32)
  (block $int32
  local.get $iv
  br_table $int32 $int32
)
  local.get $i
  br $end_integer_value
)
  br $end_literal
)
  br $end_term
)
  local.get $key
  local.get $cx
  ;; project field: other
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.maybe
)
  (func $hydra.annotations.get_debug_id (param $cx i32) (result i32)
  (local $term i32)
  i32.const 1
  i32.const 0
  call $hydra.lib.maybes.pure
  local.get $cx
  call $hydra.lib.maps.empty
  call $hydra.lib.maps.empty
  call $hydra.lib.maps.empty
  call $hydra.lib.sets.empty
  call $hydra.lib.maps.empty
  call $hydra.lib.maps.empty
  call $hydra.lib.maps.empty
  call $hydra.lib.sets.empty
  local.get $term
  call $hydra.extract.core.string
  call $hydra.lib.eithers.map
  call $hydra.constants.key_debug_id
  local.get $cx
  call $hydra.annotations.get_attr
  call $hydra.lib.maybes.maybe
)
  (func $hydra.annotations.get_description (param $cx i32) (param $graph i32) (param $anns i32) (result i32)
  (local $term i32)
  i32.const 1
  i32.const 0
  call $hydra.lib.maybes.pure
  local.get $cx
  local.get $graph
  local.get $term
  call $hydra.extract.core.string
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "description"
  local.get $anns
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.maybe
)
  (func $hydra.annotations.get_term_annotation (param $key i32) (param $term i32) (result i32)
  local.get $key
  local.get $term
  call $hydra.annotations.term_annotation_internal
  call $hydra.lib.maps.lookup
)
  (func $hydra.annotations.get_term_description (param $cx i32) (param $graph i32) (param $term i32) (result i32)
  (local $peel i32)
  (local $t i32)
  (local $ta i32)
  (local $tl i32)
  (block $end_term (result i32)
  (block $type_application
  (block $type_lambda
  local.get $t
  br_table $type_lambda $type_application $type_application
)
  local.get $tl
  ;; project field: body
  local.get $peel
  br $end_term
)
  local.get $ta
  ;; project field: body
  local.get $peel
  br $end_term
)
  local.set $peel
  local.get $cx
  local.get $graph
  local.get $term
  local.get $peel
  call $hydra.annotations.term_annotation_internal
  call $hydra.annotations.get_description
)
  (func $hydra.annotations.get_type (param $graph i32) (param $anns i32) (result i32)
  (local $dat i32)
  i32.const 1
  i32.const 0
  call $hydra.lib.maybes.pure
  local.get $graph
  local.get $dat
  call $hydra.decode.core.type
  call $hydra.lib.eithers.map
  call $hydra.constants.key_type
  local.get $anns
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.maybe
)
  (func $hydra.annotations.get_type_annotation (param $key i32) (param $typ i32) (result i32)
  local.get $key
  local.get $typ
  call $hydra.annotations.type_annotation_internal
  call $hydra.lib.maps.lookup
)
  (func $hydra.annotations.get_type_classes (param $cx i32) (param $graph i32) (param $term i32) (result i32)
  (local $by_name i32)
  (local $decode_class i32)
  (local $fn i32)
  (local $t i32)
  (local $term2 i32)
  (local $x i32)
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "equality"
  i32.const 0
  i32.const 0 ;; string: "ordering"
  i32.const 0
  call $hydra.lib.maps.from_list
  local.set $by_name
  local.get $cx
  i32.const 0 ;; string: "hydra.classes.TypeClass"
  local.get $graph
  local.get $term2
  call $hydra.extract.core.unit_variant
  i32.const 0
  i32.const 0 ;; string: "unexpected: expected type class, got "
  local.get $term2
  call $hydra.show.core.term
  call $hydra.lib.strings.cat2
  local.get $cx
  i32.const 1
  local.get $x
  local.get $fn
  local.get $by_name
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.maybe
  call $hydra.lib.eithers.bind
  local.set $decode_class
  i32.const 1
  call $hydra.lib.maps.empty
  local.get $cx
  nop
  local.get $cx
  local.get $x
  local.get $graph
  local.get $t
  call $hydra.decode.core.name
  call $hydra.lib.eithers.bimap
  local.get $cx
  local.get $decode_class
  local.get $graph
  call $hydra.extract.core.set_of
  local.get $graph
  local.get $term2
  call $hydra.extract.core.map
  call $hydra.constants.key_classes
  local.get $term
  call $hydra.annotations.get_term_annotation
  call $hydra.lib.maybes.maybe
)
  (func $hydra.annotations.get_type_description (param $cx i32) (param $graph i32) (param $typ i32) (result i32)
  local.get $cx
  local.get $graph
  local.get $typ
  call $hydra.annotations.type_annotation_internal
  call $hydra.annotations.get_description
)
  (func $hydra.annotations.has_description (param $anns i32) (result i32)
  call $hydra.constants.key_description
  local.get $anns
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.is_just
)
  (func $hydra.annotations.has_flag (param $cx i32) (param $flag i32) (result i32)
  (local $term i32)
  local.get $flag
  i32.const 0
  local.get $cx
  call $hydra.annotations.get_attr_with_default
  local.set $term
  local.get $cx
  call $hydra.lib.maps.empty
  call $hydra.lib.maps.empty
  call $hydra.lib.maps.empty
  call $hydra.lib.sets.empty
  call $hydra.lib.maps.empty
  call $hydra.lib.maps.empty
  call $hydra.lib.maps.empty
  call $hydra.lib.sets.empty
  local.get $term
  call $hydra.extract.core.boolean
)
  (func $hydra.annotations.has_type_description (param $typ i32) (result i32)
  local.get $typ
  call $hydra.annotations.type_annotation_internal
  call $hydra.annotations.has_description
)
  (func $hydra.annotations.is_native_type (param $el i32) (result i32)
  (local $is_flagged_as_first_class_type i32)
  (local $ts i32)
  i32.const 0
  i32.const 1
  call $hydra.constants.key_first_class_type
  local.get $el
  ;; project field: term
  call $hydra.annotations.get_term_annotation
  call $hydra.lib.maybes.map
  call $hydra.lib.maybes.from_maybe
  local.set $is_flagged_as_first_class_type
  i32.const 0
  local.get $ts
  i32.const 0
  ;; list elements follow
  i32.const 0 ;; string: "hydra.core.Type"
  i32.const 0
  call $hydra.lib.equality.equal
  local.get $is_flagged_as_first_class_type
  call $hydra.lib.logic.not
  call $hydra.lib.logic.and
  local.get $el
  ;; project field: type
  call $hydra.lib.maybes.maybe
)
  (func $hydra.annotations.next_count (param $key i32) (param $cx i32) (result i32)
  (local $count i32)
  local.get $key
  local.get $cx
  call $hydra.annotations.get_count
  local.set $count
  local.get $count
  local.get $key
  local.get $count
  i32.const 1
  call $hydra.lib.math.add
  local.get $cx
  call $hydra.annotations.put_count
)
  (func $hydra.annotations.normalize_term_annotations (param $term i32) (result i32)
  (local $anns i32)
  (local $stripped i32)
  local.get $term
  call $hydra.annotations.term_annotation_internal
  local.set $anns
  local.get $term
  call $hydra.strip.deannotate_term
  local.set $stripped
  local.get $anns
  call $hydra.lib.maps.null
  local.get $stripped
  local.get $stripped
  local.get $anns
  call $hydra.lib.logic.if_else
)
  (func $hydra.annotations.normalize_type_annotations (param $typ i32) (result i32)
  (local $anns i32)
  (local $stripped i32)
  local.get $typ
  call $hydra.annotations.type_annotation_internal
  local.set $anns
  local.get $typ
  call $hydra.strip.deannotate_type
  local.set $stripped
  local.get $anns
  call $hydra.lib.maps.null
  local.get $stripped
  local.get $stripped
  local.get $anns
  call $hydra.lib.logic.if_else
)
  (func $hydra.annotations.put_attr (param $key i32) (param $val i32) (param $cx i32) (result i32)
  local.get $cx
  ;; project field: trace
  local.get $cx
  ;; project field: messages
  local.get $key
  local.get $val
  local.get $cx
  ;; project field: other
  call $hydra.lib.maps.insert
)
  (func $hydra.annotations.put_count (param $key i32) (param $count i32) (param $cx i32) (result i32)
  local.get $key
  local.get $count
  local.get $cx
  call $hydra.annotations.put_attr
)
  (func $hydra.annotations.reset_count (param $key i32) (param $cx i32) (result i32)
  local.get $key
  i32.const 0
  local.get $cx
  call $hydra.annotations.put_attr
)
  (func $hydra.annotations.set_annotation (param $key i32) (param $val i32) (param $m i32) (result i32)
  local.get $val
  local.get $key
  local.get $m
  call $hydra.lib.maps.alter
)
  (func $hydra.annotations.set_description (param $d i32) (result i32)
  (local $x i32)
  call $hydra.constants.key_description
  local.get $x
  local.get $d
  call $hydra.lib.maybes.map
  call $hydra.annotations.set_annotation
)
  (func $hydra.annotations.set_term_annotation (param $key i32) (param $val i32) (param $term i32) (result i32)
  (local $anns i32)
  (local $term' i32)
  local.get $term
  call $hydra.strip.deannotate_term
  local.set $term'
  local.get $key
  local.get $val
  local.get $term
  call $hydra.annotations.term_annotation_internal
  call $hydra.annotations.set_annotation
  local.set $anns
  local.get $anns
  call $hydra.lib.maps.null
  local.get $term'
  local.get $term'
  local.get $anns
  call $hydra.lib.logic.if_else
)
  (func $hydra.annotations.set_term_description (param $d i32) (result i32)
  (local $s i32)
  call $hydra.constants.key_description
  local.get $s
  local.get $d
  call $hydra.lib.maybes.map
  call $hydra.annotations.set_term_annotation
)
  (func $hydra.annotations.set_type (param $mt i32) (result i32)
  call $hydra.constants.key_type
  call $hydra.encode.core.type
  local.get $mt
  call $hydra.lib.maybes.map
  call $hydra.annotations.set_annotation
)
  (func $hydra.annotations.set_type_annotation (param $key i32) (param $val i32) (param $typ i32) (result i32)
  (local $anns i32)
  (local $typ' i32)
  local.get $typ
  call $hydra.strip.deannotate_type
  local.set $typ'
  local.get $key
  local.get $val
  local.get $typ
  call $hydra.annotations.type_annotation_internal
  call $hydra.annotations.set_annotation
  local.set $anns
  local.get $anns
  call $hydra.lib.maps.null
  local.get $typ'
  local.get $typ'
  local.get $anns
  call $hydra.lib.logic.if_else
)
  (func $hydra.annotations.set_type_classes (param $m i32) (param $term i32) (result i32)
  (local $classes i32)
  (local $encode_class i32)
  (local $encode_pair i32)
  (local $encoded i32)
  (local $name i32)
  (local $name_classes i32)
  (local $tc i32)
  (block $end_type_class (result i32)
  (block $ordering
  (block $equality
  local.get $tc
  br_table $equality $ordering $ordering
)
  i32.const 0 ;; string: "hydra.classes.TypeClass"
  i32.const 0 ;; string: "equality"
  i32.const 0
  br $end_type_class
)
  i32.const 0 ;; string: "hydra.classes.TypeClass"
  i32.const 0 ;; string: "ordering"
  i32.const 0
  br $end_type_class
)
  local.set $encode_class
  local.get $name_classes
  call $hydra.lib.pairs.first
  local.set $name
  local.get $name_classes
  call $hydra.lib.pairs.second
  local.set $classes
  local.get $name
  call $hydra.encode.core.name
  local.get $encode_class
  local.get $classes
  call $hydra.lib.sets.to_list
  call $hydra.lib.lists.map
  call $hydra.lib.sets.from_list
  local.set $encode_pair
  local.get $m
  call $hydra.lib.maps.null
  i32.const 0
  local.get $encode_pair
  local.get $m
  call $hydra.lib.maps.to_list
  call $hydra.lib.lists.map
  call $hydra.lib.maps.from_list
  call $hydra.lib.logic.if_else
  local.set $encoded
  call $hydra.constants.key_classes
  local.get $encoded
  local.get $term
  call $hydra.annotations.set_term_annotation
)
  (func $hydra.annotations.set_type_description (param $d i32) (result i32)
  (local $x i32)
  call $hydra.constants.key_description
  local.get $x
  local.get $d
  call $hydra.lib.maybes.map
  call $hydra.annotations.set_type_annotation
)
  (func $hydra.annotations.term_annotation_internal (param $term i32) (result i32)
  (local $a i32)
  (local $at i32)
  (local $get_ann i32)
  (local $t i32)
  (block $end_term (result i32)
  (block $annotated
  local.get $t
  br_table $annotated $annotated
)
  local.get $a
  br $end_term
)
  local.set $get_ann
  local.get $get_ann
  local.get $at
  ;; project field: body
  local.get $at
  ;; project field: annotation
  local.get $term
  call $hydra.annotations.aggregate_annotations
)
  (func $hydra.annotations.type_annotation_internal (param $typ i32) (result i32)
  (local $a i32)
  (local $at i32)
  (local $get_ann i32)
  (local $t i32)
  (block $end_type (result i32)
  (block $annotated
  local.get $t
  br_table $annotated $annotated
)
  local.get $a
  br $end_type
)
  local.set $get_ann
  local.get $get_ann
  local.get $at
  ;; project field: body
  local.get $at
  ;; project field: annotation
  local.get $typ
  call $hydra.annotations.aggregate_annotations
)
  (func $hydra.annotations.when_flag (param $cx i32) (param $flag i32) (param $ethen i32) (param $eelse i32) (result i32)
  (local $b i32)
  local.get $cx
  local.get $flag
  call $hydra.annotations.has_flag
  local.get $b
  local.get $ethen
  local.get $eelse
  call $hydra.lib.logic.if_else
  call $hydra.lib.eithers.bind
)
)
