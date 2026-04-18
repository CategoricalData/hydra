(module
  (import "hydra.decode.core" "hydra.decode.core.name" (func $hydra.decode.core.name (param i32) (param i32) (result i32) ) )
  (import "hydra.extract.core" "hydra.extract.core.decode_list" (func $hydra.extract.core.decode_list (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.extract.core" "hydra.extract.core.decode_unit" (func $hydra.extract.core.decode_unit (param i32) (param i32) (result i32) ) )
  (import "hydra.extract.core" "hydra.extract.core.require_field" (func $hydra.extract.core.require_field (param i32) (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.extract.core" "hydra.extract.core.strip_with_decoding_error" (func $hydra.extract.core.strip_with_decoding_error (param i32) (param i32) (result i32) ) )
  (import "hydra.extract.core" "hydra.extract.core.to_field_map" (func $hydra.extract.core.to_field_map (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.bind" (func $hydra.lib.eithers.bind (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.either" (func $hydra.lib.eithers.either (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map" (func $hydra.lib.eithers.map (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.from_list" (func $hydra.lib.maps.from_list (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.lookup" (func $hydra.lib.maps.lookup (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.maybe" (func $hydra.lib.maybes.maybe (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat" (func $hydra.lib.strings.cat (param i32) (result i32) ) )
  (memory $memory 2 )
  (export "memory" (memory $memory) )
  (data (offset i32.const 1024 ) "\09\00\00\00\20\69\6e\20\75\6e\69\6f\6e\0d\00\00\00\61\6e\6e\6f\74\61\74\65\64\42\6f\64\79\13\00\00\00\61\70\70\6c\69\63\61\74\69\6f\6e\41\72\67\75\6d\65\6e\74\13\00\00\00\61\70\70\6c\69\63\61\74\69\6f\6e\46\75\6e\63\74\69\6f\6e\05\00\00\00\65\64\67\65\73\0a\00\00\00\65\69\74\68\65\72\4c\65\66\74\0b\00\00\00\65\69\74\68\65\72\52\69\67\68\74\16\00\00\00\65\78\70\65\63\74\65\64\20\69\6e\74\33\32\20\6c\69\74\65\72\61\6c\14\00\00\00\65\78\70\65\63\74\65\64\20\69\6e\74\33\32\20\76\61\6c\75\65\10\00\00\00\65\78\70\65\63\74\65\64\20\6c\69\74\65\72\61\6c\0f\00\00\00\65\78\70\65\63\74\65\64\20\72\65\63\6f\72\64\17\00\00\00\65\78\70\65\63\74\65\64\20\73\74\72\69\6e\67\20\6c\69\74\65\72\61\6c\0e\00\00\00\65\78\70\65\63\74\65\64\20\75\6e\69\6f\6e\15\00\00\00\65\78\70\65\63\74\65\64\20\77\72\61\70\70\65\64\20\74\79\70\65\0a\00\00\00\66\6f\72\61\6c\6c\42\6f\64\79\10\00\00\00\66\75\6e\63\74\69\6f\6e\43\6f\64\6f\6d\61\69\6e\0e\00\00\00\66\75\6e\63\74\69\6f\6e\44\6f\6d\61\69\6e\02\00\00\00\69\64\0d\00\00\00\69\6e\6a\65\63\74\69\6f\6e\54\65\72\6d\05\00\00\00\6c\61\62\65\6c\0a\00\00\00\6c\61\6d\62\64\61\42\6f\64\79\0a\00\00\00\6c\65\74\42\69\6e\64\69\6e\67\07\00\00\00\6c\65\74\42\6f\64\79\0b\00\00\00\6c\69\73\74\45\6c\65\6d\65\6e\74\06\00\00\00\6d\61\70\4b\65\79\07\00\00\00\6d\61\70\4b\65\79\73\08\00\00\00\6d\61\70\56\61\6c\75\65\09\00\00\00\6d\61\70\56\61\6c\75\65\73\0c\00\00\00\6d\61\79\62\65\45\6c\65\6d\65\6e\74\09\00\00\00\6d\61\79\62\65\54\65\72\6d\04\00\00\00\6e\61\6d\65\0e\00\00\00\6e\6f\20\73\75\63\68\20\66\69\65\6c\64\20\05\00\00\00\6e\6f\64\65\73\09\00\00\00\70\61\69\72\46\69\72\73\74\0a\00\00\00\70\61\69\72\53\65\63\6f\6e\64\04\00\00\00\70\61\74\68\0b\00\00\00\70\72\6f\64\75\63\74\54\65\72\6d\0b\00\00\00\72\65\63\6f\72\64\46\69\65\6c\64\0a\00\00\00\73\65\74\45\6c\65\6d\65\6e\74\06\00\00\00\73\6f\75\72\63\65\07\00\00\00\73\75\6d\54\65\72\6d\06\00\00\00\74\61\72\67\65\74\13\00\00\00\74\79\70\65\41\70\70\6c\69\63\61\74\69\6f\6e\54\65\72\6d\0e\00\00\00\74\79\70\65\4c\61\6d\62\64\61\42\6f\64\79\10\00\00\00\75\6e\69\6f\6e\43\61\73\65\73\42\72\61\6e\63\68\11\00\00\00\75\6e\69\6f\6e\43\61\73\65\73\44\65\66\61\75\6c\74\0a\00\00\00\75\6e\69\6f\6e\46\69\65\6c\64\0b\00\00\00\77\72\61\70\70\65\64\54\65\72\6d\0b\00\00\00\77\72\61\70\70\65\64\54\79\70\65")
  (global $__bump_ptr (mut i32) i32.const 1792 )
  (export "__bump_ptr" (global $__bump_ptr) )
  (func $__alloc (param $sz i32) (result i32)
  global.get $__bump_ptr
  global.get $__bump_ptr
  local.get $sz
  i32.add
  global.set $__bump_ptr
)
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
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $err i32)
  (local $field_map i32)
  (local $field_path i32)
  (local $field_source i32)
  (local $field_target i32)
  (local $record i32)
  (local $stripped i32)
  (local $v i32)
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
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $record $record
)
  local.get $v
  drop
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 1624
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1572
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1645
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_source
  local.get $field_path
  local.get $field_target
  i32.const 12
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=8
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
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.paths.subterm_graph (param $cx i32) (param $raw i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $err i32)
  (local $field_edges i32)
  (local $field_map i32)
  (local $field_nodes i32)
  (local $record i32)
  (local $stripped i32)
  (local $v i32)
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
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $record $record
)
  local.get $v
  drop
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 1536
  i32.const 0
  i32.const 0
  i32.const 0
  call $hydra.extract.core.decode_list
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1100
  i32.const 0
  i32.const 0
  i32.const 0
  call $hydra.extract.core.decode_list
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_nodes
  local.get $field_edges
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
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.paths.subterm_node (param $cx i32) (param $raw i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
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
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $record $record
)
  local.get $v
  drop
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 1510
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1372
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
  (block $end_term (result i32)
  (block $literal
  local.get $stripped2
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $literal $literal
)
  local.get $v
  drop
  (block $end_literal (result i32)
  (block $string
  local.get $v
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $string $string
)
  local.get $v
  drop
  i32.const 1
  local.get $s
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
  br $end_literal
)
  br $end_term
)
  local.get $cx2
  local.get $raw2
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1349
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
  (block $end_term (result i32)
  (block $literal
  local.get $stripped2
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $literal $literal
)
  local.get $v
  drop
  (block $end_literal (result i32)
  (block $string
  local.get $v
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $string $string
)
  local.get $v
  drop
  i32.const 1
  local.get $s
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
  br $end_literal
)
  br $end_term
)
  local.get $cx2
  local.get $raw2
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_name
  local.get $field_label
  local.get $field_id
  i32.const 12
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=8
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
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.paths.subterm_path (param $cx i32) (param $raw i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $b i32)
  (local $err i32)
  (local $stripped i32)
  (local $v i32)
  (local $wrapped_term i32)
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
  (block $end_term (result i32)
  (block $wrap
  local.get $stripped
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $wrap $wrap
)
  local.get $v
  drop
  local.get $b
  i32.const 0
  local.get $cx
  local.get $wrapped_term
  i32.load offset=4
  call $hydra.extract.core.decode_list
  call $hydra.lib.eithers.map
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.paths.subterm_step (param $cx i32) (param $raw i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
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
  (block $end_term (result i32)
  (block $inject
  local.get $stripped
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $inject $inject
)
  local.get $v
  drop
  local.get $inj
  i32.load offset=4
  local.set $field
  local.get $field
  i32.load
  local.set $fname
  local.get $field
  i32.load offset=4
  local.set $fterm
  i32.const 1037
  i32.const 0
  local.get $t
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
  local.get $cx
  drop
  local.get $input
  drop
  local.get $cx2
  local.get $t
  call $hydra.extract.core.decode_unit
  call $hydra.lib.eithers.map
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
  i32.const 1077
  i32.const 1
  local.get $t
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
  local.get $cx
  drop
  local.get $input
  drop
  local.get $cx2
  local.get $t
  call $hydra.extract.core.decode_unit
  call $hydra.lib.eithers.map
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
  i32.const 1054
  i32.const 2
  local.get $t
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
  local.get $cx
  drop
  local.get $input
  drop
  local.get $cx2
  local.get $t
  call $hydra.extract.core.decode_unit
  call $hydra.lib.eithers.map
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
  i32.const 1381
  i32.const 3
  local.get $t
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
  local.get $cx
  drop
  local.get $input
  drop
  local.get $cx2
  local.get $t
  call $hydra.extract.core.decode_unit
  call $hydra.lib.eithers.map
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
  i32.const 1716
  i32.const 4
  local.get $t
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
  local.get $cx
  drop
  local.get $input
  drop
  local.get $cx2
  local.get $t
  call $hydra.extract.core.decode_unit
  call $hydra.lib.eithers.map
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
  i32.const 1696
  i32.const 5
  local.get $t
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
  local.get $cx
  local.get $input
  call $hydra.decode.core.name
  call $hydra.lib.eithers.map
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
  i32.const 1409
  i32.const 6
  local.get $t
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
  local.get $cx
  drop
  local.get $input
  drop
  local.get $cx2
  local.get $t
  call $hydra.extract.core.decode_unit
  call $hydra.lib.eithers.map
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
  i32.const 1395
  i32.const 7
  local.get $t
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
  local.get $cx
  local.get $input
  call $hydra.decode.core.name
  call $hydra.lib.eithers.map
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
  i32.const 1420
  i32.const 8
  local.get $t
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
  local.get $cx
  drop
  local.get $input
  drop
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
  (block $end_term (result i32)
  (block $literal
  local.get $stripped2
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $literal $literal
)
  local.get $v
  drop
  (block $end_literal (result i32)
  (block $integer
  local.get $v
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $integer $integer
)
  (block $end_integer_value (result i32)
  (block $int32
  local.get $v
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $int32 $int32
)
  local.get $v
  drop
  i32.const 1
  local.get $i
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
  br $end_integer_value
)
  br $end_literal
)
  br $end_term
)
  local.get $cx2
  local.get $raw2
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
  call $hydra.lib.eithers.map
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
  i32.const 1435
  i32.const 9
  local.get $t
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
  local.get $cx
  drop
  local.get $input
  drop
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
  (block $end_term (result i32)
  (block $literal
  local.get $stripped2
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $literal $literal
)
  local.get $v
  drop
  (block $end_literal (result i32)
  (block $integer
  local.get $v
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $integer $integer
)
  (block $end_integer_value (result i32)
  (block $int32
  local.get $v
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $int32 $int32
)
  local.get $v
  drop
  i32.const 1
  local.get $i
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
  br $end_integer_value
)
  br $end_literal
)
  br $end_term
)
  local.get $cx2
  local.get $raw2
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
  call $hydra.lib.eithers.map
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
  i32.const 1456
  i32.const 10
  local.get $t
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
  local.get $cx
  drop
  local.get $input
  drop
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
  (block $end_term (result i32)
  (block $literal
  local.get $stripped2
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $literal $literal
)
  local.get $v
  drop
  (block $end_literal (result i32)
  (block $integer
  local.get $v
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $integer $integer
)
  (block $end_integer_value (result i32)
  (block $int32
  local.get $v
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $int32 $int32
)
  local.get $v
  drop
  i32.const 1
  local.get $i
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
  br $end_integer_value
)
  br $end_literal
)
  br $end_term
)
  local.get $cx2
  local.get $raw2
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
  call $hydra.lib.eithers.map
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
  i32.const 1497
  i32.const 11
  local.get $t
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
  local.get $cx
  drop
  local.get $input
  drop
  local.get $cx2
  local.get $t
  call $hydra.extract.core.decode_unit
  call $hydra.lib.eithers.map
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
  i32.const 1580
  i32.const 12
  local.get $t
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
  local.get $cx
  drop
  local.get $input
  drop
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
  (block $end_term (result i32)
  (block $literal
  local.get $stripped2
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $literal $literal
)
  local.get $v
  drop
  (block $end_literal (result i32)
  (block $integer
  local.get $v
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $integer $integer
)
  (block $end_integer_value (result i32)
  (block $int32
  local.get $v
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $int32 $int32
)
  local.get $v
  drop
  i32.const 1
  local.get $i
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
  br $end_integer_value
)
  br $end_literal
)
  br $end_term
)
  local.get $cx2
  local.get $raw2
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
  call $hydra.lib.eithers.map
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
  i32.const 1595
  i32.const 13
  local.get $t
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
  local.get $cx
  local.get $input
  call $hydra.decode.core.name
  call $hydra.lib.eithers.map
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
  i32.const 1610
  i32.const 14
  local.get $t
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
  local.get $cx
  drop
  local.get $input
  drop
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
  (block $end_term (result i32)
  (block $literal
  local.get $stripped2
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $literal $literal
)
  local.get $v
  drop
  (block $end_literal (result i32)
  (block $integer
  local.get $v
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $integer $integer
)
  (block $end_integer_value (result i32)
  (block $int32
  local.get $v
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $int32 $int32
)
  local.get $v
  drop
  i32.const 1
  local.get $i
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
  br $end_integer_value
)
  br $end_literal
)
  br $end_term
)
  local.get $cx2
  local.get $raw2
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
  call $hydra.lib.eithers.map
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
  i32.const 1634
  i32.const 15
  local.get $t
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
  local.get $cx
  drop
  local.get $input
  drop
  local.get $cx2
  local.get $t
  call $hydra.extract.core.decode_unit
  call $hydra.lib.eithers.map
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
  i32.const 1678
  i32.const 16
  local.get $t
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
  local.get $cx
  drop
  local.get $input
  drop
  local.get $cx2
  local.get $t
  call $hydra.extract.core.decode_unit
  call $hydra.lib.eithers.map
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
  i32.const 1655
  i32.const 17
  local.get $t
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
  local.get $cx
  drop
  local.get $input
  drop
  local.get $cx2
  local.get $t
  call $hydra.extract.core.decode_unit
  call $hydra.lib.eithers.map
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
  i32.const 1355
  i32.const 18
  local.get $t
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
  local.get $cx
  drop
  local.get $input
  drop
  local.get $cx2
  local.get $t
  call $hydra.extract.core.decode_unit
  call $hydra.lib.eithers.map
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
  i32.const 1751
  i32.const 19
  local.get $t
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
  local.get $cx
  drop
  local.get $input
  drop
  local.get $cx2
  local.get $t
  call $hydra.extract.core.decode_unit
  call $hydra.lib.eithers.map
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
  i32.const 84
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 20
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=80
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=76
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=72
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=68
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=64
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=60
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=56
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=52
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=48
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=44
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=40
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=36
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=32
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=28
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=24
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=20
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=16
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=12
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=8
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.get $__rec_ptr
  call $hydra.lib.maps.from_list
  local.set $variant_map
  i32.const 0
  i32.const 1518
  local.get $fname
  drop
  i32.const 0
  drop
  i32.const 0
  i32.const 1024
  i32.const 16
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 3
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=12
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=8
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.get $__rec_ptr
  call $hydra.lib.strings.cat
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
  local.get $fterm
  drop
  local.get $f
  drop
  i32.const 0
  local.get $fname
  local.get $variant_map
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.maybe
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.paths.subtype_edge (param $cx i32) (param $raw i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $err i32)
  (local $field_map i32)
  (local $field_path i32)
  (local $field_source i32)
  (local $field_target i32)
  (local $record i32)
  (local $stripped i32)
  (local $v i32)
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
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $record $record
)
  local.get $v
  drop
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 1624
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1572
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1645
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_source
  local.get $field_path
  local.get $field_target
  i32.const 12
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=8
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
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.paths.subtype_graph (param $cx i32) (param $raw i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $err i32)
  (local $field_edges i32)
  (local $field_map i32)
  (local $field_nodes i32)
  (local $record i32)
  (local $stripped i32)
  (local $v i32)
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
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $record $record
)
  local.get $v
  drop
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 1536
  i32.const 0
  i32.const 0
  i32.const 0
  call $hydra.extract.core.decode_list
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1100
  i32.const 0
  i32.const 0
  i32.const 0
  call $hydra.extract.core.decode_list
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_nodes
  local.get $field_edges
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
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.paths.subtype_node (param $cx i32) (param $raw i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
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
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $record $record
)
  local.get $v
  drop
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 1510
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1372
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
  (block $end_term (result i32)
  (block $literal
  local.get $stripped2
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $literal $literal
)
  local.get $v
  drop
  (block $end_literal (result i32)
  (block $string
  local.get $v
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $string $string
)
  local.get $v
  drop
  i32.const 1
  local.get $s
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
  br $end_literal
)
  br $end_term
)
  local.get $cx2
  local.get $raw2
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1349
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
  (block $end_term (result i32)
  (block $literal
  local.get $stripped2
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $literal $literal
)
  local.get $v
  drop
  (block $end_literal (result i32)
  (block $string
  local.get $v
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $string $string
)
  local.get $v
  drop
  i32.const 1
  local.get $s
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
  br $end_literal
)
  br $end_term
)
  local.get $cx2
  local.get $raw2
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_name
  local.get $field_label
  local.get $field_id
  i32.const 12
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=8
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
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.paths.subtype_path (param $cx i32) (param $raw i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $b i32)
  (local $err i32)
  (local $stripped i32)
  (local $v i32)
  (local $wrapped_term i32)
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
  (block $end_term (result i32)
  (block $wrap
  local.get $stripped
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $wrap $wrap
)
  local.get $v
  drop
  local.get $b
  i32.const 0
  local.get $cx
  local.get $wrapped_term
  i32.load offset=4
  call $hydra.extract.core.decode_list
  call $hydra.lib.eithers.map
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.paths.subtype_step (param $cx i32) (param $raw i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $cx2 i32)
  (local $err i32)
  (local $f i32)
  (local $field i32)
  (local $fname i32)
  (local $fterm i32)
  (local $inj i32)
  (local $input i32)
  (local $stripped i32)
  (local $t i32)
  (local $v i32)
  (local $variant_map i32)
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
  (block $end_term (result i32)
  (block $inject
  local.get $stripped
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $inject $inject
)
  local.get $v
  drop
  local.get $inj
  i32.load offset=4
  local.set $field
  local.get $field
  i32.load
  local.set $fname
  local.get $field
  i32.load offset=4
  local.set $fterm
  i32.const 1037
  i32.const 0
  local.get $t
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
  local.get $cx
  drop
  local.get $input
  drop
  local.get $cx2
  local.get $t
  call $hydra.extract.core.decode_unit
  call $hydra.lib.eithers.map
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
  i32.const 1077
  i32.const 1
  local.get $t
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
  local.get $cx
  drop
  local.get $input
  drop
  local.get $cx2
  local.get $t
  call $hydra.extract.core.decode_unit
  call $hydra.lib.eithers.map
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
  i32.const 1054
  i32.const 2
  local.get $t
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
  local.get $cx
  drop
  local.get $input
  drop
  local.get $cx2
  local.get $t
  call $hydra.extract.core.decode_unit
  call $hydra.lib.eithers.map
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
  i32.const 1109
  i32.const 3
  local.get $t
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
  local.get $cx
  drop
  local.get $input
  drop
  local.get $cx2
  local.get $t
  call $hydra.extract.core.decode_unit
  call $hydra.lib.eithers.map
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
  i32.const 1123
  i32.const 4
  local.get $t
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
  local.get $cx
  drop
  local.get $input
  drop
  local.get $cx2
  local.get $t
  call $hydra.extract.core.decode_unit
  call $hydra.lib.eithers.map
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
  i32.const 1297
  i32.const 5
  local.get $t
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
  local.get $cx
  drop
  local.get $input
  drop
  local.get $cx2
  local.get $t
  call $hydra.extract.core.decode_unit
  call $hydra.lib.eithers.map
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
  i32.const 1331
  i32.const 6
  local.get $t
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
  local.get $cx
  drop
  local.get $input
  drop
  local.get $cx2
  local.get $t
  call $hydra.extract.core.decode_unit
  call $hydra.lib.eithers.map
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
  i32.const 1311
  i32.const 7
  local.get $t
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
  local.get $cx
  drop
  local.get $input
  drop
  local.get $cx2
  local.get $t
  call $hydra.extract.core.decode_unit
  call $hydra.lib.eithers.map
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
  i32.const 1420
  i32.const 8
  local.get $t
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
  local.get $cx
  drop
  local.get $input
  drop
  local.get $cx2
  local.get $t
  call $hydra.extract.core.decode_unit
  call $hydra.lib.eithers.map
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
  i32.const 1445
  i32.const 9
  local.get $t
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
  local.get $cx
  drop
  local.get $input
  drop
  local.get $cx2
  local.get $t
  call $hydra.extract.core.decode_unit
  call $hydra.lib.eithers.map
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
  i32.const 1468
  i32.const 10
  local.get $t
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
  local.get $cx
  drop
  local.get $input
  drop
  local.get $cx2
  local.get $t
  call $hydra.extract.core.decode_unit
  call $hydra.lib.eithers.map
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
  i32.const 1481
  i32.const 11
  local.get $t
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
  local.get $cx
  drop
  local.get $input
  drop
  local.get $cx2
  local.get $t
  call $hydra.extract.core.decode_unit
  call $hydra.lib.eithers.map
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
  i32.const 1545
  i32.const 12
  local.get $t
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
  local.get $cx
  drop
  local.get $input
  drop
  local.get $cx2
  local.get $t
  call $hydra.extract.core.decode_unit
  call $hydra.lib.eithers.map
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
  i32.const 1558
  i32.const 13
  local.get $t
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
  local.get $cx
  drop
  local.get $input
  drop
  local.get $cx2
  local.get $t
  call $hydra.extract.core.decode_unit
  call $hydra.lib.eithers.map
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
  i32.const 1595
  i32.const 14
  local.get $t
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
  local.get $cx
  local.get $input
  call $hydra.decode.core.name
  call $hydra.lib.eithers.map
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
  i32.const 1610
  i32.const 15
  local.get $t
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
  local.get $cx
  drop
  local.get $input
  drop
  local.get $cx2
  local.get $t
  call $hydra.extract.core.decode_unit
  call $hydra.lib.eithers.map
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
  i32.const 1737
  i32.const 16
  local.get $t
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
  local.get $cx
  local.get $input
  call $hydra.decode.core.name
  call $hydra.lib.eithers.map
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
  i32.const 1766
  i32.const 17
  local.get $t
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
  local.get $cx
  drop
  local.get $input
  drop
  local.get $cx2
  local.get $t
  call $hydra.extract.core.decode_unit
  call $hydra.lib.eithers.map
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
  i32.const 76
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 18
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=72
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=68
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=64
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=60
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=56
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=52
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=48
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=44
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=40
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=36
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=32
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=28
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=24
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=20
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=16
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=12
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=8
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.get $__rec_ptr
  call $hydra.lib.maps.from_list
  local.set $variant_map
  i32.const 0
  i32.const 1518
  local.get $fname
  drop
  i32.const 0
  drop
  i32.const 0
  i32.const 1024
  i32.const 16
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 3
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=12
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=8
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.get $__rec_ptr
  call $hydra.lib.strings.cat
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
  local.get $fterm
  drop
  local.get $f
  drop
  i32.const 0
  local.get $fname
  local.get $variant_map
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.maybe
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
)
)
