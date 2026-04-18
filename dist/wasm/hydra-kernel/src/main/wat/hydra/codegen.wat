(module
  (import "hydra.adapt" "hydra.adapt.data_graph_to_definitions" (func $hydra.adapt.data_graph_to_definitions (param i32) (param i32) (param i32) (param i32) (param i32) (param i32) (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.adapt" "hydra.adapt.schema_graph_to_definitions" (func $hydra.adapt.schema_graph_to_definitions (param i32) (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.annotations" "hydra.annotations.is_native_type" (func $hydra.annotations.is_native_type (param i32) (result i32) ) )
  (import "hydra.annotations" "hydra.annotations.normalize_term_annotations" (func $hydra.annotations.normalize_term_annotations (param i32) (result i32) ) )
  (import "hydra.decode.core" "hydra.decode.core.type" (func $hydra.decode.core.type (param i32) (param i32) (result i32) ) )
  (import "hydra.decode.packaging" "hydra.decode.packaging.module" (func $hydra.decode.packaging.module (param i32) (param i32) (result i32) ) )
  (import "hydra.encode.core" "hydra.encode.core.type" (func $hydra.encode.core.type (param i32) (result i32) ) )
  (import "hydra.encode.packaging" "hydra.encode.packaging.module" (func $hydra.encode.packaging.module (param i32) (result i32) ) )
  (import "hydra.environment" "hydra.environment.schema_graph_to_typing_environment" (func $hydra.environment.schema_graph_to_typing_environment (param i32) (result i32) ) )
  (import "hydra.inference" "hydra.inference.infer_graph_types" (func $hydra.inference.infer_graph_types (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.json.decode" "hydra.json.decode.from_json" (func $hydra.json.decode.from_json (param i32) (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.json.encode" "hydra.json.encode.to_json" (func $hydra.json.encode.to_json (param i32) (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.json.writer" "hydra.json.writer.print_json" (func $hydra.json.writer.print_json (param i32) (result i32) ) )
  (import "hydra.lexical" "hydra.lexical.elements_to_graph" (func $hydra.lexical.elements_to_graph (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lexical" "hydra.lexical.graph_to_bindings" (func $hydra.lexical.graph_to_bindings (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.bimap" (func $hydra.lib.eithers.bimap (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.bind" (func $hydra.lib.eithers.bind (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.either" (func $hydra.lib.eithers.either (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map" (func $hydra.lib.eithers.map (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map_list" (func $hydra.lib.eithers.map_list (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.equality" "hydra.lib.equality.equal" (func $hydra.lib.equality.equal (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.equality" "hydra.lib.equality.lt" (func $hydra.lib.equality.lt (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.concat" (func $hydra.lib.lists.concat (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.concat2" (func $hydra.lib.lists.concat2 (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.cons" (func $hydra.lib.lists.cons (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.drop" (func $hydra.lib.lists.drop (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.filter" (func $hydra.lib.lists.filter (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.find" (func $hydra.lib.lists.find (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.null" (func $hydra.lib.lists.null (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.partition" (func $hydra.lib.lists.partition (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.sort_on" (func $hydra.lib.lists.sort_on (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.uncons" (func $hydra.lib.lists.uncons (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.zip" (func $hydra.lib.lists.zip (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.and" (func $hydra.lib.logic.and (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.if_else" (func $hydra.lib.logic.if_else (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.not" (func $hydra.lib.logic.not (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.elems" (func $hydra.lib.maps.elems (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.from_list" (func $hydra.lib.maps.from_list (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.lookup" (func $hydra.lib.maps.lookup (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.map" (func $hydra.lib.maps.map (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.to_list" (func $hydra.lib.maps.to_list (param i32) (result i32) ) )
  (import "hydra.lib.math" "hydra.lib.math.add" (func $hydra.lib.math.add (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.math" "hydra.lib.math.maybe_div" (func $hydra.lib.math.maybe_div (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.math" "hydra.lib.math.maybe_mod" (func $hydra.lib.math.maybe_mod (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.math" "hydra.lib.math.sub" (func $hydra.lib.math.sub (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.cat" (func $hydra.lib.maybes.cat (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.from_maybe" (func $hydra.lib.maybes.from_maybe (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.map" (func $hydra.lib.maybes.map (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.maybe" (func $hydra.lib.maybes.maybe (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.first" (func $hydra.lib.pairs.first (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.second" (func $hydra.lib.pairs.second (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.difference" (func $hydra.lib.sets.difference (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.from_list" (func $hydra.lib.sets.from_list (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.null" (func $hydra.lib.sets.null (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.to_list" (func $hydra.lib.sets.to_list (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.union" (func $hydra.lib.sets.union (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat2" (func $hydra.lib.strings.cat2 (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.intercalate" (func $hydra.lib.strings.intercalate (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.split_on" (func $hydra.lib.strings.split_on (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.unlines" (func $hydra.lib.strings.unlines (param i32) (result i32) ) )
  (import "hydra.show.core" "hydra.show.core.type" (func $hydra.show.core.type (param i32) (result i32) ) )
  (import "hydra.show.core" "hydra.show.core.type_scheme" (func $hydra.show.core.type_scheme (param i32) (result i32) ) )
  (import "hydra.strip" "hydra.strip.deannotate_type" (func $hydra.strip.deannotate_type (param i32) (result i32) ) )
  (memory $memory 2 )
  (export "memory" (memory $memory) )
  (data (offset i32.const 1024 ) "\08\00\00\00\0a\54\65\72\6d\73\3a\0a\08\00\00\00\0a\54\79\70\65\73\3a\0a\02\00\00\00\20\20\03\00\00\00\20\3a\20\03\00\00\00\20\3d\20\01\00\00\00\2e\08\00\00\00\2e\6d\6f\64\75\6c\65\5f\01\00\00\00\2f\01\00\00\00\3f\0c\00\00\00\50\72\69\6d\69\74\69\76\65\73\3a\0a\12\00\00\00\53\6f\75\72\63\65\20\6d\6f\64\75\6c\65\20\66\6f\72\20\0f\00\00\00\68\79\64\72\61\2e\63\6f\72\65\2e\54\79\70\65\0f\00\00\00\68\79\64\72\61\2e\70\61\63\6b\61\67\69\6e\67\16\00\00\00\68\79\64\72\61\2e\70\61\63\6b\61\67\69\6e\67\2e\4d\6f\64\75\6c\65\0e\00\00\00\68\79\64\72\61\2e\73\6f\75\72\63\65\73\2e")
  (global $__bump_ptr (mut i32) i32.const 1216 )
  (export "__bump_ptr" (global $__bump_ptr) )
  (func $__alloc (param $sz i32) (result i32)
  global.get $__bump_ptr
  global.get $__bump_ptr
  local.get $sz
  i32.add
  global.set $__bump_ptr
)
  (export "hydra.codegen.build_schema_map" (func $hydra.codegen.build_schema_map) )
  (export "hydra.codegen.decode_module_from_json" (func $hydra.codegen.decode_module_from_json) )
  (export "hydra.codegen.escape_control_chars_in_json" (func $hydra.codegen.escape_control_chars_in_json) )
  (export "hydra.codegen.format_primitive" (func $hydra.codegen.format_primitive) )
  (export "hydra.codegen.format_term_binding" (func $hydra.codegen.format_term_binding) )
  (export "hydra.codegen.format_type_binding" (func $hydra.codegen.format_type_binding) )
  (export "hydra.codegen.generate_coder_modules" (func $hydra.codegen.generate_coder_modules) )
  (export "hydra.codegen.generate_lexicon" (func $hydra.codegen.generate_lexicon) )
  (export "hydra.codegen.generate_source_files" (func $hydra.codegen.generate_source_files) )
  (export "hydra.codegen.infer_and_generate_lexicon" (func $hydra.codegen.infer_and_generate_lexicon) )
  (export "hydra.codegen.infer_modules" (func $hydra.codegen.infer_modules) )
  (export "hydra.codegen.infer_modules_given" (func $hydra.codegen.infer_modules_given) )
  (export "hydra.codegen.module_term_deps_transitive" (func $hydra.codegen.module_term_deps_transitive) )
  (export "hydra.codegen.module_to_json" (func $hydra.codegen.module_to_json) )
  (export "hydra.codegen.module_to_source_module" (func $hydra.codegen.module_to_source_module) )
  (export "hydra.codegen.module_type_deps_transitive" (func $hydra.codegen.module_type_deps_transitive) )
  (export "hydra.codegen.modules_to_graph" (func $hydra.codegen.modules_to_graph) )
  (export "hydra.codegen.namespace_to_path" (func $hydra.codegen.namespace_to_path) )
  (export "hydra.codegen.refresh_module" (func $hydra.codegen.refresh_module) )
  (export "hydra.codegen.transitive_deps" (func $hydra.codegen.transitive_deps) )
  (func $hydra.codegen.build_schema_map (param $g i32) (result i32)
  (local $ts i32)
  local.get $ts
  i32.load offset=4
  call $hydra.strip.deannotate_type
  local.get $g
  i32.load offset=24
  call $hydra.lib.maps.map
)
  (func $hydra.codegen.decode_module_from_json (param $bs_graph i32) (param $universe_modules i32) (param $json_val i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $dec_err i32)
  (local $err i32)
  (local $graph i32)
  (local $mod i32)
  (local $mod_type i32)
  (local $schema_map i32)
  (local $term i32)
  local.get $bs_graph
  local.get $universe_modules
  local.get $universe_modules
  call $hydra.codegen.modules_to_graph
  local.set $graph
  local.get $graph
  call $hydra.codegen.build_schema_map
  local.set $schema_map
  i32.const 14
  i32.const 1171
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
  local.set $mod_type
  i32.const 0
  i32.const 6
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
  i32.const 0
  i32.const 1
  local.get $dec_err
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
  i32.const 1
  local.get $mod
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
  local.get $graph
  local.get $term
  call $hydra.decode.packaging.module
  call $hydra.lib.eithers.either
  local.get $schema_map
  i32.const 1171
  local.get $mod_type
  local.get $json_val
  call $hydra.json.decode.from_json
  call $hydra.lib.eithers.either
)
  (func $hydra.codegen.escape_control_chars_in_json (param $input i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $b i32)
  (local $bs i32)
  (local $bytes i32)
  (local $esc i32)
  (local $escape_to_unicode i32)
  (local $go i32)
  (local $hex_digit i32)
  (local $in_str i32)
  (local $n i32)
  (local $uc i32)
  local.get $n
  i32.const 10
  call $hydra.lib.equality.lt
  i32.const 48
  local.get $n
  call $hydra.lib.math.add
  i32.const 97
  local.get $n
  i32.const 10
  call $hydra.lib.math.sub
  call $hydra.lib.math.add
  call $hydra.lib.logic.if_else
  local.set $hex_digit
  i32.const 92
  i32.const 117
  i32.const 48
  i32.const 48
  i32.const 0
  local.get $b
  i32.const 16
  call $hydra.lib.math.maybe_div
  call $hydra.lib.maybes.from_maybe
  drop
  local.get $hex_digit
  drop
  i32.const 0
  i32.const 0
  local.get $b
  i32.const 16
  call $hydra.lib.math.maybe_mod
  call $hydra.lib.maybes.from_maybe
  drop
  local.get $hex_digit
  drop
  i32.const 0
  i32.const 28
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 6
  i32.store
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
  local.set $escape_to_unicode
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  local.get $uc
  call $hydra.lib.pairs.first
  local.set $b
  local.get $uc
  call $hydra.lib.pairs.second
  local.set $bs
  local.get $esc
  local.get $b
  local.get $in_str
  drop
  i32.const 0
  drop
  local.get $bs
  drop
  local.get $go
  drop
  i32.const 0
  call $hydra.lib.lists.cons
  local.get $b
  i32.const 92
  call $hydra.lib.equality.equal
  local.get $in_str
  call $hydra.lib.logic.and
  local.get $b
  local.get $in_str
  drop
  i32.const 1
  drop
  local.get $bs
  drop
  local.get $go
  drop
  i32.const 0
  call $hydra.lib.lists.cons
  local.get $b
  i32.const 34
  call $hydra.lib.equality.equal
  local.get $b
  local.get $in_str
  call $hydra.lib.logic.not
  drop
  i32.const 0
  drop
  local.get $bs
  drop
  local.get $go
  drop
  i32.const 0
  call $hydra.lib.lists.cons
  local.get $in_str
  local.get $b
  i32.const 32
  call $hydra.lib.equality.lt
  call $hydra.lib.logic.and
  local.get $b
  drop
  local.get $escape_to_unicode
  drop
  i32.const 0
  local.get $in_str
  drop
  i32.const 0
  drop
  local.get $bs
  drop
  local.get $go
  drop
  i32.const 0
  call $hydra.lib.lists.concat2
  local.get $b
  local.get $in_str
  drop
  i32.const 0
  drop
  local.get $bs
  drop
  local.get $go
  drop
  i32.const 0
  call $hydra.lib.lists.cons
  call $hydra.lib.logic.if_else
  call $hydra.lib.logic.if_else
  call $hydra.lib.logic.if_else
  call $hydra.lib.logic.if_else
  local.get $bytes
  call $hydra.lib.lists.uncons
  call $hydra.lib.maybes.maybe
  local.set $go
  i32.const 0
  drop
  i32.const 0
  drop
  local.get $input
  drop
  local.get $go
  drop
  i32.const 0
)
  (func $hydra.codegen.format_primitive (param $prim i32) (result i32)
  (local $name i32)
  (local $type_str i32)
  local.get $prim
  i32.load
  drop
  i32.const 0
  drop
  i32.const 0
  local.set $name
  local.get $prim
  i32.load offset=4
  call $hydra.show.core.type_scheme
  local.set $type_str
  i32.const 1048
  local.get $name
  call $hydra.lib.strings.cat2
  i32.const 1054
  call $hydra.lib.strings.cat2
  local.get $type_str
  call $hydra.lib.strings.cat2
)
  (func $hydra.codegen.format_term_binding (param $binding i32) (result i32)
  (local $name i32)
  (local $scheme i32)
  (local $type_str i32)
  local.get $binding
  i32.load
  drop
  i32.const 0
  drop
  i32.const 0
  local.set $name
  i32.const 1090
  local.get $scheme
  call $hydra.show.core.type_scheme
  local.get $binding
  i32.load offset=8
  call $hydra.lib.maybes.maybe
  local.set $type_str
  i32.const 1048
  local.get $name
  call $hydra.lib.strings.cat2
  i32.const 1054
  call $hydra.lib.strings.cat2
  local.get $type_str
  call $hydra.lib.strings.cat2
)
  (func $hydra.codegen.format_type_binding (param $graph i32) (param $binding i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $_a i32)
  (local $_e i32)
  (local $typ i32)
  i32.const 1
  local.get $_e
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
  local.get $_a
  local.get $graph
  local.get $binding
  i32.load offset=4
  call $hydra.decode.core.type
  call $hydra.lib.eithers.bimap
  i32.const 1
  i32.const 1048
  local.get $binding
  i32.load
  drop
  i32.const 0
  drop
  i32.const 0
  call $hydra.lib.strings.cat2
  i32.const 1061
  call $hydra.lib.strings.cat2
  local.get $typ
  call $hydra.show.core.type
  call $hydra.lib.strings.cat2
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
)
  (func $hydra.codegen.generate_coder_modules (param $codec i32) (param $bs_graph i32) (param $universe_modules i32) (param $type_modules i32) (param $cx i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $_r i32)
  (local $all_elements i32)
  (local $d i32)
  (local $data_elements i32)
  (local $data_modules i32)
  (local $data_term i32)
  (local $graph i32)
  (local $m i32)
  (local $name i32)
  (local $results i32)
  (local $schema_elements i32)
  (local $schema_graph i32)
  (local $schema_modules i32)
  (local $schema_term i32)
  (local $schema_types i32)
  (local $td i32)
  (local $typ i32)
  (local $universe i32)
  (local $v i32)
  local.get $m
  i32.load
  local.get $m
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
  local.get $universe_modules
  local.get $universe_modules
  call $hydra.lib.lists.concat2
  call $hydra.lib.lists.map
  call $hydra.lib.maps.from_list
  local.set $universe
  local.get $universe
  local.get $universe_modules
  call $hydra.codegen.module_type_deps_transitive
  local.set $schema_modules
  local.get $universe
  local.get $universe_modules
  call $hydra.codegen.module_term_deps_transitive
  local.set $data_modules
  (block $end_definition (result i32)
  (block $type
  local.get $d
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $type $type
)
  local.get $v
  drop
  local.get $td
  i32.load
  drop
  local.get $td
  i32.load offset=4
  i32.load offset=4
  drop
  i32.const 19
  i32.const 1133
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
  local.set $schema_term
  i32.const 0
  local.get $typ
  call $hydra.encode.core.type
  i32.const 0
  local.get $schema_term
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
  local.get $__rec_ptr
  i32.const 1
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.get $__rec_ptr
  call $hydra.lib.maps.from_list
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
  call $hydra.annotations.normalize_term_annotations
  local.set $data_term
  local.get $name
  local.get $data_term
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  i32.const 14
  i32.const 1133
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
  i32.const 0
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
  br $end_definition
)
  local.get $m
  i32.load offset=4
  call $hydra.lib.lists.map
  call $hydra.lib.maybes.cat
  local.get $schema_modules
  local.get $universe_modules
  call $hydra.lib.lists.concat2
  call $hydra.lib.lists.map
  call $hydra.lib.lists.concat
  local.set $schema_elements
  (block $end_definition (result i32)
  (block $term
  local.get $d
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $term $term
)
  local.get $v
  drop
  local.get $td
  i32.load
  local.get $td
  i32.load offset=4
  local.get $td
  i32.load offset=8
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
  br $end_definition
)
  local.get $m
  i32.load offset=4
  call $hydra.lib.lists.map
  call $hydra.lib.maybes.cat
  local.get $data_modules
  call $hydra.lib.lists.map
  call $hydra.lib.lists.concat
  local.set $data_elements
  local.get $bs_graph
  i32.const 0
  local.get $schema_elements
  call $hydra.lexical.elements_to_graph
  local.set $schema_graph
  i32.const 0
  local.get $_r
  local.get $schema_graph
  call $hydra.environment.schema_graph_to_typing_environment
  call $hydra.lib.eithers.either
  local.set $schema_types
  local.get $schema_elements
  local.get $data_elements
  call $hydra.lib.lists.concat2
  local.set $all_elements
  local.get $bs_graph
  local.get $schema_types
  local.get $all_elements
  call $hydra.lexical.elements_to_graph
  local.set $graph
  local.get $results
  call $hydra.lib.maybes.cat
  local.get $cx
  drop
  local.get $graph
  drop
  local.get $m
  drop
  local.get $codec
  drop
  i32.const 0
  local.get $type_modules
  call $hydra.lib.eithers.map_list
  call $hydra.lib.eithers.map
)
  (func $hydra.codegen.generate_lexicon (param $graph i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $b i32)
  (local $bindings i32)
  (local $p i32)
  (local $partitioned i32)
  (local $primitive_lines i32)
  (local $primitives i32)
  (local $sorted_primitives i32)
  (local $sorted_terms i32)
  (local $sorted_types i32)
  (local $term_bindings i32)
  (local $term_lines i32)
  (local $type_bindings i32)
  (local $type_lines i32)
  local.get $graph
  call $hydra.lexical.graph_to_bindings
  local.set $bindings
  local.get $graph
  i32.load offset=20
  call $hydra.lib.maps.elems
  local.set $primitives
  local.get $b
  call $hydra.annotations.is_native_type
  local.get $bindings
  call $hydra.lib.lists.partition
  local.set $partitioned
  local.get $partitioned
  call $hydra.lib.pairs.first
  local.set $type_bindings
  local.get $partitioned
  call $hydra.lib.pairs.second
  local.set $term_bindings
  local.get $p
  i32.load
  local.get $primitives
  call $hydra.lib.lists.sort_on
  local.set $sorted_primitives
  local.get $b
  i32.load
  local.get $type_bindings
  call $hydra.lib.lists.sort_on
  local.set $sorted_types
  local.get $b
  i32.load
  local.get $term_bindings
  call $hydra.lib.lists.sort_on
  local.set $sorted_terms
  local.get $graph
  local.get $b
  call $hydra.codegen.format_type_binding
  local.get $sorted_types
  call $hydra.lib.eithers.map_list
  local.get $b
  call $hydra.codegen.format_term_binding
  local.get $sorted_terms
  call $hydra.lib.lists.map
  local.set $term_lines
  local.get $p
  call $hydra.codegen.format_primitive
  local.get $sorted_primitives
  call $hydra.lib.lists.map
  local.set $primitive_lines
  i32.const 1
  i32.const 1095
  local.get $primitive_lines
  call $hydra.lib.strings.unlines
  call $hydra.lib.strings.cat2
  i32.const 1036
  call $hydra.lib.strings.cat2
  local.get $type_lines
  call $hydra.lib.strings.unlines
  call $hydra.lib.strings.cat2
  i32.const 1024
  call $hydra.lib.strings.cat2
  local.get $term_lines
  call $hydra.lib.strings.unlines
  call $hydra.lib.strings.cat2
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
)
  (func $hydra.codegen.generate_source_files (param $print_definitions i32) (param $lang i32) (param $do_infer i32) (param $do_expand i32) (param $do_hoist_case_statements i32) (param $do_hoist_polymorphic_let_bindings i32) (param $bs_graph i32) (param $universe_modules i32) (param $mods_to_generate i32) (param $cx i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $_r i32)
  (local $all_bindings i32)
  (local $b i32)
  (local $constraints i32)
  (local $d i32)
  (local $data_elements i32)
  (local $data_graph i32)
  (local $data_mods i32)
  (local $data_result i32)
  (local $data_term i32)
  (local $dedup_defs i32)
  (local $deduped_def_lists i32)
  (local $def_lists i32)
  (local $def_name i32)
  (local $defs i32)
  (local $els i32)
  (local $g1 i32)
  (local $m i32)
  (local $mod i32)
  (local $name i32)
  (local $name_lists i32)
  (local $namespace_map i32)
  (local $namespaces i32)
  (local $p i32)
  (local $refresh_module i32)
  (local $refreshed_mods i32)
  (local $schema_elements i32)
  (local $schema_files i32)
  (local $schema_graph i32)
  (local $schema_graph_with_types i32)
  (local $schema_mods i32)
  (local $schema_result i32)
  (local $schema_term i32)
  (local $schema_types2 i32)
  (local $td i32)
  (local $term_files i32)
  (local $term_modules_to_generate i32)
  (local $typ i32)
  (local $type_modules_to_generate i32)
  (local $v i32)
  (local $xs i32)
  local.get $m
  i32.load
  local.get $m
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
  local.get $universe_modules
  local.get $mods_to_generate
  call $hydra.lib.lists.concat2
  call $hydra.lib.lists.map
  call $hydra.lib.maps.from_list
  local.set $namespace_map
  local.get $lang
  i32.load offset=4
  local.set $constraints
  (block $end_definition (result i32)
  (block $type
  local.get $d
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $type $type
)
  local.get $v
  drop
  local.get $td
  i32.load
  drop
  local.get $td
  i32.load offset=4
  i32.load offset=4
  drop
  i32.const 19
  i32.const 1133
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
  local.set $schema_term
  i32.const 0
  local.get $typ
  call $hydra.encode.core.type
  i32.const 0
  local.get $schema_term
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
  local.get $__rec_ptr
  i32.const 1
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.get $__rec_ptr
  call $hydra.lib.maps.from_list
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
  call $hydra.annotations.normalize_term_annotations
  local.set $data_term
  local.get $name
  local.get $data_term
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  i32.const 14
  i32.const 1133
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
  i32.const 0
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
  br $end_definition
)
  local.get $mod
  i32.load offset=4
  call $hydra.lib.lists.map
  call $hydra.lib.maybes.cat
  call $hydra.lib.lists.null
  call $hydra.lib.logic.not
  local.get $mods_to_generate
  call $hydra.lib.lists.filter
  local.set $type_modules_to_generate
  (block $end_definition (result i32)
  (block $term
  local.get $d
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $term $term
)
  local.get $v
  drop
  local.get $td
  i32.load
  local.get $td
  i32.load offset=4
  local.get $td
  i32.load offset=8
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
  br $end_definition
)
  local.get $mod
  i32.load offset=4
  call $hydra.lib.lists.map
  call $hydra.lib.maybes.cat
  call $hydra.lib.lists.null
  call $hydra.lib.logic.not
  local.get $mods_to_generate
  call $hydra.lib.lists.filter
  local.set $term_modules_to_generate
  local.get $namespace_map
  local.get $mods_to_generate
  call $hydra.codegen.module_type_deps_transitive
  local.set $schema_mods
  (block $end_definition (result i32)
  (block $type
  local.get $d
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $type $type
)
  local.get $v
  drop
  local.get $td
  i32.load
  drop
  local.get $td
  i32.load offset=4
  i32.load offset=4
  drop
  i32.const 19
  i32.const 1133
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
  local.set $schema_term
  i32.const 0
  local.get $typ
  call $hydra.encode.core.type
  i32.const 0
  local.get $schema_term
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
  local.get $__rec_ptr
  i32.const 1
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.get $__rec_ptr
  call $hydra.lib.maps.from_list
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
  call $hydra.annotations.normalize_term_annotations
  local.set $data_term
  local.get $name
  local.get $data_term
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  i32.const 14
  i32.const 1133
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
  i32.const 0
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
  br $end_definition
)
  local.get $m
  i32.load offset=4
  call $hydra.lib.lists.map
  call $hydra.lib.maybes.cat
  local.get $schema_mods
  local.get $type_modules_to_generate
  call $hydra.lib.lists.concat2
  call $hydra.lib.lists.map
  call $hydra.lib.lists.concat
  local.set $schema_elements
  local.get $namespace_map
  local.get $mods_to_generate
  call $hydra.codegen.module_term_deps_transitive
  local.set $data_mods
  (block $end_definition (result i32)
  (block $term
  local.get $d
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $term $term
)
  local.get $v
  drop
  local.get $td
  i32.load
  local.get $td
  i32.load offset=4
  local.get $td
  i32.load offset=8
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
  br $end_definition
)
  local.get $m
  i32.load offset=4
  call $hydra.lib.lists.map
  call $hydra.lib.maybes.cat
  local.get $data_mods
  call $hydra.lib.lists.map
  call $hydra.lib.lists.concat
  local.set $data_elements
  local.get $bs_graph
  i32.const 0
  local.get $schema_elements
  call $hydra.lexical.elements_to_graph
  local.set $schema_graph
  i32.const 0
  local.get $_r
  local.get $schema_graph
  call $hydra.environment.schema_graph_to_typing_environment
  call $hydra.lib.eithers.either
  local.set $schema_types2
  local.get $bs_graph
  local.get $schema_types2
  local.get $data_elements
  call $hydra.lexical.elements_to_graph
  local.set $data_graph
  local.get $type_modules_to_generate
  call $hydra.lib.lists.null
  i32.const 1
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
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
  (block $end_definition (result i32)
  (block $type
  local.get $d
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $type $type
)
  local.get $v
  drop
  local.get $td
  i32.load
  br $end_definition
)
  local.get $m
  i32.load offset=4
  call $hydra.lib.lists.map
  call $hydra.lib.maybes.cat
  local.get $type_modules_to_generate
  call $hydra.lib.lists.map
  local.set $name_lists
  local.get $constraints
  local.get $schema_graph
  local.get $name_lists
  local.get $cx
  call $hydra.adapt.schema_graph_to_definitions
  local.get $schema_result
  call $hydra.lib.pairs.second
  local.set $def_lists
  local.get $schema_graph
  i32.load
  local.get $schema_graph
  i32.load offset=4
  local.get $schema_graph
  i32.load offset=8
  local.get $schema_graph
  i32.load offset=12
  local.get $schema_graph
  i32.load offset=16
  local.get $schema_graph
  i32.load offset=20
  local.get $schema_types2
  local.get $schema_graph
  i32.load offset=28
  i32.const 32
  call $__alloc
  local.set $__rec_ptr
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
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  local.set $schema_graph_with_types
  local.get $xs
  call $hydra.lib.lists.concat
  local.get $p
  call $hydra.lib.pairs.first
  local.set $mod
  local.get $p
  call $hydra.lib.pairs.second
  local.set $defs
  local.get $m
  call $hydra.lib.maps.to_list
  local.get $mod
  drop
  i32.const 1
  local.get $d
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
  local.get $defs
  call $hydra.lib.lists.map
  drop
  local.get $cx
  drop
  local.get $schema_graph_with_types
  drop
  local.get $print_definitions
  drop
  i32.const 0
  call $hydra.lib.eithers.map
  local.get $type_modules_to_generate
  local.get $def_lists
  call $hydra.lib.lists.zip
  call $hydra.lib.eithers.map_list
  call $hydra.lib.eithers.map
  call $hydra.lib.eithers.bind
  call $hydra.lib.logic.if_else
  local.get $term_modules_to_generate
  call $hydra.lib.lists.null
  i32.const 1
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
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
  local.get $m
  i32.load
  local.get $term_modules_to_generate
  call $hydra.lib.lists.map
  local.set $namespaces
  local.get $constraints
  local.get $do_infer
  local.get $do_expand
  local.get $do_hoist_case_statements
  local.get $do_hoist_polymorphic_let_bindings
  local.get $data_elements
  local.get $data_graph
  local.get $namespaces
  local.get $cx
  call $hydra.adapt.data_graph_to_definitions
  local.get $data_result
  call $hydra.lib.pairs.first
  local.set $g1
  local.get $data_result
  call $hydra.lib.pairs.second
  local.set $def_lists
  (block $end_definition (result i32)
  (block $type
  (block $term
  local.get $d
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $term $type $type
)
  local.get $v
  drop
  local.get $td
  i32.load
  br $end_definition
)
  local.get $v
  drop
  local.get $td
  i32.load
  br $end_definition
)
  local.set $def_name
  local.get $m
  i32.load
  (block $end_definition (result i32)
  (block $term
  (block $type
  local.get $d
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $type $term $term
)
  local.get $v
  drop
  i32.const 1
  local.get $td
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
  br $end_definition
)
  local.get $v
  drop
  i32.const 0
  local.get $b
  i32.load
  local.get $b
  i32.load offset=4
  local.get $b
  i32.load offset=8
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
  local.get $b
  i32.load
  local.get $td
  i32.load
  call $hydra.lib.equality.equal
  local.get $els
  call $hydra.lib.lists.find
  call $hydra.lib.maybes.map
  br $end_definition
)
  local.get $m
  i32.load offset=4
  call $hydra.lib.lists.map
  call $hydra.lib.maybes.cat
  local.get $m
  i32.load offset=8
  local.get $m
  i32.load offset=12
  local.get $m
  i32.load offset=16
  i32.const 20
  call $__alloc
  local.set $__rec_ptr
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
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  local.set $refresh_module
  local.get $g1
  call $hydra.lexical.graph_to_bindings
  local.set $all_bindings
  local.get $all_bindings
  drop
  local.get $m
  drop
  local.get $refresh_module
  drop
  i32.const 0
  local.get $term_modules_to_generate
  call $hydra.lib.lists.map
  local.set $refreshed_mods
  local.get $d
  i32.load
  local.get $d
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
  local.get $defs
  call $hydra.lib.lists.map
  call $hydra.lib.maps.from_list
  call $hydra.lib.maps.elems
  local.set $dedup_defs
  local.get $dedup_defs
  local.get $def_lists
  call $hydra.lib.lists.map
  local.set $deduped_def_lists
  local.get $xs
  call $hydra.lib.lists.concat
  local.get $p
  call $hydra.lib.pairs.first
  local.set $mod
  local.get $p
  call $hydra.lib.pairs.second
  local.set $defs
  local.get $m
  call $hydra.lib.maps.to_list
  local.get $mod
  drop
  i32.const 0
  local.get $d
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
  local.get $defs
  call $hydra.lib.lists.map
  drop
  local.get $cx
  drop
  local.get $g1
  drop
  local.get $print_definitions
  drop
  i32.const 0
  call $hydra.lib.eithers.map
  local.get $refreshed_mods
  local.get $deduped_def_lists
  call $hydra.lib.lists.zip
  call $hydra.lib.eithers.map_list
  call $hydra.lib.eithers.map
  call $hydra.lib.eithers.bind
  call $hydra.lib.logic.if_else
  i32.const 1
  local.get $schema_files
  local.get $term_files
  call $hydra.lib.lists.concat2
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
)
  (func $hydra.codegen.infer_and_generate_lexicon (param $cx i32) (param $bs_graph i32) (param $kernel_modules i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $d i32)
  (local $data_elements i32)
  (local $g0 i32)
  (local $g1 i32)
  (local $infer_result_with_cx i32)
  (local $m i32)
  (local $td i32)
  (local $v i32)
  local.get $bs_graph
  local.get $kernel_modules
  local.get $kernel_modules
  call $hydra.codegen.modules_to_graph
  local.set $g0
  (block $end_definition (result i32)
  (block $term
  local.get $d
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $term $term
)
  local.get $v
  drop
  local.get $td
  i32.load
  local.get $td
  i32.load offset=4
  local.get $td
  i32.load offset=8
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
  br $end_definition
)
  local.get $m
  i32.load offset=4
  call $hydra.lib.lists.map
  call $hydra.lib.maybes.cat
  local.get $kernel_modules
  call $hydra.lib.lists.map
  call $hydra.lib.lists.concat
  local.set $data_elements
  local.get $cx
  local.get $data_elements
  local.get $g0
  call $hydra.inference.infer_graph_types
  local.get $infer_result_with_cx
  call $hydra.lib.pairs.first
  call $hydra.lib.pairs.first
  local.set $g1
  local.get $g1
  call $hydra.codegen.generate_lexicon
  call $hydra.lib.eithers.bind
)
  (func $hydra.codegen.infer_modules (param $cx i32) (param $bs_graph i32) (param $universe_mods i32) (param $target_mods i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $d i32)
  (local $data_elements i32)
  (local $g0 i32)
  (local $infer_result i32)
  (local $infer_result_with_cx i32)
  (local $inferred_elements i32)
  (local $m i32)
  (local $td i32)
  (local $v i32)
  local.get $bs_graph
  local.get $universe_mods
  local.get $universe_mods
  call $hydra.codegen.modules_to_graph
  local.set $g0
  (block $end_definition (result i32)
  (block $term
  local.get $d
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $term $term
)
  local.get $v
  drop
  local.get $td
  i32.load
  local.get $td
  i32.load offset=4
  local.get $td
  i32.load offset=8
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
  br $end_definition
)
  local.get $m
  i32.load offset=4
  call $hydra.lib.lists.map
  call $hydra.lib.maybes.cat
  local.get $universe_mods
  call $hydra.lib.lists.map
  call $hydra.lib.lists.concat
  local.set $data_elements
  local.get $cx
  local.get $data_elements
  local.get $g0
  call $hydra.inference.infer_graph_types
  local.get $infer_result_with_cx
  call $hydra.lib.pairs.first
  local.set $infer_result
  local.get $infer_result
  call $hydra.lib.pairs.second
  local.set $inferred_elements
  i32.const 1
  local.get $inferred_elements
  i32.const 0
  call $hydra.codegen.refresh_module
  local.get $target_mods
  call $hydra.lib.lists.map
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
)
  (func $hydra.codegen.infer_modules_given (param $cx i32) (param $bs_graph i32) (param $universe_mods i32) (param $target_mods i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $d i32)
  (local $data_elements i32)
  (local $g0 i32)
  (local $infer_result i32)
  (local $infer_result_with_cx i32)
  (local $inferred_elements i32)
  (local $m i32)
  (local $td i32)
  (local $v i32)
  local.get $bs_graph
  local.get $universe_mods
  local.get $universe_mods
  call $hydra.codegen.modules_to_graph
  local.set $g0
  (block $end_definition (result i32)
  (block $term
  local.get $d
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $term $term
)
  local.get $v
  drop
  local.get $td
  i32.load
  local.get $td
  i32.load offset=4
  local.get $td
  i32.load offset=8
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
  br $end_definition
)
  local.get $m
  i32.load offset=4
  call $hydra.lib.lists.map
  call $hydra.lib.maybes.cat
  local.get $universe_mods
  call $hydra.lib.lists.map
  call $hydra.lib.lists.concat
  local.set $data_elements
  local.get $cx
  local.get $data_elements
  local.get $g0
  call $hydra.inference.infer_graph_types
  local.get $infer_result_with_cx
  call $hydra.lib.pairs.first
  local.set $infer_result
  local.get $infer_result
  call $hydra.lib.pairs.second
  local.set $inferred_elements
  i32.const 1
  local.get $inferred_elements
  i32.const 0
  call $hydra.codegen.refresh_module
  local.get $target_mods
  call $hydra.lib.lists.map
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
)
  (func $hydra.codegen.module_term_deps_transitive (param $ns_map i32) (param $modules i32) (result i32)
  (local $closure i32)
  (local $m i32)
  (local $n i32)
  local.get $m
  i32.load offset=8
  local.get $ns_map
  local.get $modules
  call $hydra.codegen.transitive_deps
  local.get $m
  i32.load
  local.get $modules
  call $hydra.lib.lists.map
  call $hydra.lib.sets.from_list
  call $hydra.lib.sets.union
  local.set $closure
  local.get $n
  local.get $ns_map
  call $hydra.lib.maps.lookup
  local.get $closure
  call $hydra.lib.sets.to_list
  call $hydra.lib.lists.map
  call $hydra.lib.maybes.cat
)
  (func $hydra.codegen.module_to_json (param $schema_map i32) (param $m i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $_a i32)
  (local $_e i32)
  (local $json i32)
  (local $mod_type i32)
  (local $term i32)
  local.get $m
  call $hydra.encode.packaging.module
  local.set $term
  i32.const 14
  i32.const 1171
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
  local.set $mod_type
  local.get $json
  call $hydra.json.writer.print_json
  i32.const 6
  local.get $_e
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
  local.get $_a
  local.get $schema_map
  i32.const 1171
  local.get $mod_type
  local.get $term
  call $hydra.json.encode.to_json
  call $hydra.lib.eithers.bimap
  call $hydra.lib.eithers.map
)
  (func $hydra.codegen.module_to_source_module (param $m i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $mod_type_ns i32)
  (local $module_def i32)
  (local $source_ns i32)
  i32.const 1197
  i32.const 1068
  i32.const 1
  i32.const 1068
  local.get $m
  i32.load
  drop
  i32.const 0
  drop
  i32.const 0
  call $hydra.lib.strings.split_on
  call $hydra.lib.lists.drop
  call $hydra.lib.strings.intercalate
  call $hydra.lib.strings.cat2
  local.set $source_ns
  i32.const 1152
  local.set $mod_type_ns
  i32.const 0
  local.get $source_ns
  drop
  i32.const 0
  drop
  i32.const 0
  i32.const 1073
  call $hydra.lib.strings.cat2
  local.get $m
  call $hydra.encode.packaging.module
  i32.const 0
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
  local.set $module_def
  local.get $source_ns
  local.get $module_def
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 1
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.get $__rec_ptr
  local.get $mod_type_ns
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 1
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.get $__rec_ptr
  local.get $mod_type_ns
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 1
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.get $__rec_ptr
  i32.const 1111
  local.get $m
  i32.load
  drop
  i32.const 0
  drop
  i32.const 0
  call $hydra.lib.strings.cat2
  i32.const 20
  call $__alloc
  local.set $__rec_ptr
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
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
)
  (func $hydra.codegen.module_type_deps_transitive (param $ns_map i32) (param $modules i32) (result i32)
  (local $m i32)
  (local $n i32)
  (local $term_mods i32)
  (local $type_namespaces i32)
  local.get $ns_map
  local.get $modules
  call $hydra.codegen.module_term_deps_transitive
  local.set $term_mods
  local.get $m
  i32.load offset=12
  local.get $ns_map
  local.get $term_mods
  call $hydra.codegen.transitive_deps
  call $hydra.lib.sets.to_list
  local.set $type_namespaces
  local.get $n
  local.get $ns_map
  call $hydra.lib.maps.lookup
  local.get $type_namespaces
  call $hydra.lib.lists.map
  call $hydra.lib.maybes.cat
)
  (func $hydra.codegen.modules_to_graph (param $bs_graph i32) (param $universe_modules i32) (param $modules i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $_r i32)
  (local $b i32)
  (local $base_graph i32)
  (local $d i32)
  (local $data_elements i32)
  (local $data_modules i32)
  (local $data_term i32)
  (local $m i32)
  (local $name i32)
  (local $schema_elements i32)
  (local $schema_graph i32)
  (local $schema_modules i32)
  (local $schema_term i32)
  (local $schema_types i32)
  (local $td i32)
  (local $ts i32)
  (local $typ i32)
  (local $universe i32)
  (local $universe_bound_types i32)
  (local $universe_data_elements i32)
  (local $v i32)
  local.get $m
  i32.load
  local.get $m
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
  local.get $universe_modules
  local.get $modules
  call $hydra.lib.lists.concat2
  call $hydra.lib.lists.map
  call $hydra.lib.maps.from_list
  local.set $universe
  local.get $universe
  local.get $modules
  call $hydra.codegen.module_type_deps_transitive
  local.set $schema_modules
  local.get $universe
  local.get $modules
  call $hydra.codegen.module_term_deps_transitive
  local.set $data_modules
  (block $end_definition (result i32)
  (block $type
  local.get $d
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $type $type
)
  local.get $v
  drop
  local.get $td
  i32.load
  drop
  local.get $td
  i32.load offset=4
  i32.load offset=4
  drop
  i32.const 19
  i32.const 1133
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
  local.set $schema_term
  i32.const 0
  local.get $typ
  call $hydra.encode.core.type
  i32.const 0
  local.get $schema_term
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
  local.get $__rec_ptr
  i32.const 1
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.get $__rec_ptr
  call $hydra.lib.maps.from_list
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
  call $hydra.annotations.normalize_term_annotations
  local.set $data_term
  local.get $name
  local.get $data_term
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  i32.const 14
  i32.const 1133
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
  i32.const 0
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
  br $end_definition
)
  local.get $m
  i32.load offset=4
  call $hydra.lib.lists.map
  call $hydra.lib.maybes.cat
  local.get $schema_modules
  local.get $modules
  call $hydra.lib.lists.concat2
  call $hydra.lib.lists.map
  call $hydra.lib.lists.concat
  local.set $schema_elements
  (block $end_definition (result i32)
  (block $term
  local.get $d
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $term $term
)
  local.get $v
  drop
  local.get $td
  i32.load
  local.get $td
  i32.load offset=4
  local.get $td
  i32.load offset=8
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
  br $end_definition
)
  local.get $m
  i32.load offset=4
  call $hydra.lib.lists.map
  call $hydra.lib.maybes.cat
  local.get $data_modules
  call $hydra.lib.lists.map
  call $hydra.lib.lists.concat
  local.set $data_elements
  local.get $bs_graph
  i32.const 0
  local.get $schema_elements
  call $hydra.lexical.elements_to_graph
  local.set $schema_graph
  i32.const 0
  local.get $_r
  local.get $schema_graph
  call $hydra.environment.schema_graph_to_typing_environment
  call $hydra.lib.eithers.either
  local.set $schema_types
  local.get $bs_graph
  local.get $schema_types
  local.get $data_elements
  call $hydra.lexical.elements_to_graph
  local.set $base_graph
  (block $end_definition (result i32)
  (block $term
  local.get $d
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $term $term
)
  local.get $v
  drop
  local.get $td
  i32.load
  local.get $td
  i32.load offset=4
  local.get $td
  i32.load offset=8
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
  br $end_definition
)
  local.get $m
  i32.load offset=4
  call $hydra.lib.lists.map
  call $hydra.lib.maybes.cat
  local.get $universe_modules
  call $hydra.lib.lists.map
  call $hydra.lib.lists.concat
  local.set $universe_data_elements
  local.get $b
  i32.load
  local.get $ts
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
  local.get $b
  i32.load offset=8
  call $hydra.lib.maybes.map
  local.get $universe_data_elements
  call $hydra.lib.lists.map
  call $hydra.lib.maybes.cat
  call $hydra.lib.maps.from_list
  local.set $universe_bound_types
  local.get $base_graph
  i32.load
  local.get $universe_bound_types
  local.get $base_graph
  i32.load offset=8
  local.get $base_graph
  i32.load offset=12
  local.get $base_graph
  i32.load offset=16
  local.get $base_graph
  i32.load offset=20
  local.get $base_graph
  i32.load offset=24
  local.get $base_graph
  i32.load offset=28
  i32.const 32
  call $__alloc
  local.set $__rec_ptr
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
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
)
  (func $hydra.codegen.namespace_to_path (param $ns i32) (result i32)
  i32.const 1085
  i32.const 1068
  local.get $ns
  drop
  i32.const 0
  drop
  i32.const 0
  call $hydra.lib.strings.split_on
  call $hydra.lib.strings.intercalate
)
  (func $hydra.codegen.refresh_module (param $inferred_elements i32) (param $m i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $b i32)
  (local $d i32)
  (local $td i32)
  (local $v i32)
  (block $end_definition (result i32)
  (block $term
  local.get $d
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $term $term
)
  local.get $v
  drop
  local.get $td
  i32.load
  local.get $td
  i32.load offset=4
  local.get $td
  i32.load offset=8
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
  br $end_definition
)
  local.get $m
  i32.load offset=4
  call $hydra.lib.lists.map
  call $hydra.lib.maybes.cat
  call $hydra.lib.lists.null
  call $hydra.lib.logic.not
  call $hydra.lib.logic.not
  local.get $m
  local.get $m
  i32.load
  (block $end_definition (result i32)
  (block $term
  (block $type
  local.get $d
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $type $term $term
)
  local.get $v
  drop
  i32.const 1
  local.get $td
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
  br $end_definition
)
  local.get $v
  drop
  i32.const 0
  local.get $b
  i32.load
  local.get $b
  i32.load offset=4
  local.get $b
  i32.load offset=8
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
  local.get $b
  i32.load
  local.get $td
  i32.load
  call $hydra.lib.equality.equal
  local.get $inferred_elements
  call $hydra.lib.lists.find
  call $hydra.lib.maybes.map
  br $end_definition
)
  local.get $m
  i32.load offset=4
  call $hydra.lib.lists.map
  call $hydra.lib.maybes.cat
  local.get $m
  i32.load offset=8
  local.get $m
  i32.load offset=12
  local.get $m
  i32.load offset=16
  i32.const 20
  call $__alloc
  local.set $__rec_ptr
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
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  call $hydra.lib.logic.if_else
)
  (func $hydra.codegen.transitive_deps (param $get_deps i32) (param $ns_map i32) (param $start_mods i32) (result i32)
  (local $__rec_ptr i32)
  (local $dep i32)
  (local $dep_mod i32)
  (local $go i32)
  (local $initial_deps i32)
  (local $m i32)
  (local $new_pending i32)
  (local $new_visited i32)
  (local $next_deps i32)
  (local $nsv i32)
  (local $pending i32)
  (local $visited i32)
  local.get $dep
  local.get $m
  i32.load
  call $hydra.lib.equality.equal
  call $hydra.lib.logic.not
  local.get $m
  drop
  local.get $get_deps
  drop
  i32.const 0
  call $hydra.lib.lists.filter
  local.get $start_mods
  call $hydra.lib.lists.map
  call $hydra.lib.lists.concat
  call $hydra.lib.sets.from_list
  local.set $initial_deps
  local.get $pending
  call $hydra.lib.sets.null
  local.get $visited
  local.get $visited
  local.get $pending
  call $hydra.lib.sets.union
  local.set $new_visited
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  local.get $dep_mod
  drop
  local.get $get_deps
  drop
  i32.const 0
  local.get $nsv
  local.get $ns_map
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.maybe
  local.get $pending
  call $hydra.lib.sets.to_list
  call $hydra.lib.lists.map
  call $hydra.lib.lists.concat
  call $hydra.lib.sets.from_list
  local.set $next_deps
  local.get $next_deps
  local.get $new_visited
  call $hydra.lib.sets.difference
  local.set $new_pending
  local.get $new_pending
  drop
  local.get $new_visited
  drop
  local.get $go
  drop
  i32.const 0
  call $hydra.lib.logic.if_else
  local.set $go
  local.get $initial_deps
  drop
  i32.const 0
  drop
  local.get $go
  drop
  i32.const 0
)
)
