(module
  (import "hydra.lib.chars" "hydra.lib.chars.is_upper" (func $hydra.lib.chars.is_upper (param i32) (result i32) ) )
  (import "hydra.lib.equality" "hydra.lib.equality.equal" (func $hydra.lib.equality.equal (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.equality" "hydra.lib.equality.gte" (func $hydra.lib.equality.gte (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.equality" "hydra.lib.equality.lte" (func $hydra.lib.equality.lte (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.concat" (func $hydra.lib.lists.concat (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.concat2" (func $hydra.lib.lists.concat2 (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.cons" (func $hydra.lib.lists.cons (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.drop" (func $hydra.lib.lists.drop (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.drop_while" (func $hydra.lib.lists.drop_while (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.filter" (func $hydra.lib.lists.filter (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.foldl" (func $hydra.lib.lists.foldl (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.intercalate" (func $hydra.lib.lists.intercalate (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.length" (func $hydra.lib.lists.length (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.maybe_init" (func $hydra.lib.lists.maybe_init (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.null" (func $hydra.lib.lists.null (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.pure" (func $hydra.lib.lists.pure (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.reverse" (func $hydra.lib.lists.reverse (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.span" (func $hydra.lib.lists.span (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.take" (func $hydra.lib.lists.take (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.uncons" (func $hydra.lib.lists.uncons (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.and" (func $hydra.lib.logic.and (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.if_else" (func $hydra.lib.logic.if_else (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.not" (func $hydra.lib.logic.not (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.or" (func $hydra.lib.logic.or (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.from_list" (func $hydra.lib.maps.from_list (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.lookup" (func $hydra.lib.maps.lookup (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.math" "hydra.lib.math.sub" (func $hydra.lib.math.sub (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.from_maybe" (func $hydra.lib.maybes.from_maybe (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.map" (func $hydra.lib.maybes.map (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.maybe" (func $hydra.lib.maybes.maybe (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.first" (func $hydra.lib.pairs.first (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.second" (func $hydra.lib.pairs.second (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.member" (func $hydra.lib.sets.member (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat" (func $hydra.lib.strings.cat (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat2" (func $hydra.lib.strings.cat2 (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.from_list" (func $hydra.lib.strings.from_list (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.intercalate" (func $hydra.lib.strings.intercalate (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.length" (func $hydra.lib.strings.length (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.lines" (func $hydra.lib.strings.lines (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.maybe_char_at" (func $hydra.lib.strings.maybe_char_at (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.null" (func $hydra.lib.strings.null (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.split_on" (func $hydra.lib.strings.split_on (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.to_list" (func $hydra.lib.strings.to_list (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.to_lower" (func $hydra.lib.strings.to_lower (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.unlines" (func $hydra.lib.strings.unlines (param i32) (result i32) ) )
  (memory $memory 2 )
  (export "memory" (memory $memory) )
  (data (offset i32.const 1024 ) "\00\00\00\00\04\00\00\00\0a\20\2a\2f\04\00\00\00\20\20\20\20\03\00\00\00\20\2a\20\02\00\00\00\2c\20\01\00\00\00\2e\04\00\00\00\2f\2a\2a\0a\01\00\00\00\5b\01\00\00\00\5d\01\00\00\00\5f\03\00\00\00\61\6d\70\04\00\00\00\61\70\6f\73\03\00\00\00\61\73\74\04\00\00\00\62\73\6f\6c\04\00\00\00\63\69\72\63\05\00\00\00\63\6f\6c\6f\6e\05\00\00\00\63\6f\6d\6d\61\06\00\00\00\63\6f\6d\6d\61\74\06\00\00\00\64\6f\6c\6c\61\72\06\00\00\00\65\71\75\61\6c\73\04\00\00\00\65\78\63\6c\05\00\00\00\67\72\61\76\65\02\00\00\00\67\74\04\00\00\00\6c\63\75\62\06\00\00\00\6c\6f\77\62\61\72\04\00\00\00\6c\70\61\72\04\00\00\00\6c\73\71\62\02\00\00\00\6c\74\05\00\00\00\6d\69\6e\75\73\03\00\00\00\6e\75\6d\06\00\00\00\70\65\72\63\6e\74\06\00\00\00\70\65\72\69\6f\64\04\00\00\00\70\6c\75\73\05\00\00\00\71\75\65\73\74\04\00\00\00\71\75\6f\74\04\00\00\00\72\63\75\62\04\00\00\00\72\70\61\72\04\00\00\00\72\73\71\62\04\00\00\00\73\65\6d\69\03\00\00\00\73\6f\6c\02\00\00\00\73\70\05\00\00\00\74\69\6c\64\65\06\00\00\00\76\65\72\62\61\72")
  (global $__bump_ptr (mut i32) i32.const 1360 )
  (export "__bump_ptr" (global $__bump_ptr) )
  (func $__alloc (param $sz i32) (result i32)
  global.get $__bump_ptr
  global.get $__bump_ptr
  local.get $sz
  i32.add
  global.set $__bump_ptr
)
  (export "hydra.formatting.capitalize" (func $hydra.formatting.capitalize) )
  (export "hydra.formatting.convert_case" (func $hydra.formatting.convert_case) )
  (export "hydra.formatting.convert_case_camel_or_underscore_to_lower_snake" (func $hydra.formatting.convert_case_camel_or_underscore_to_lower_snake) )
  (export "hydra.formatting.convert_case_camel_to_lower_snake" (func $hydra.formatting.convert_case_camel_to_lower_snake) )
  (export "hydra.formatting.convert_case_camel_to_upper_snake" (func $hydra.formatting.convert_case_camel_to_upper_snake) )
  (export "hydra.formatting.convert_case_pascal_to_upper_snake" (func $hydra.formatting.convert_case_pascal_to_upper_snake) )
  (export "hydra.formatting.decapitalize" (func $hydra.formatting.decapitalize) )
  (export "hydra.formatting.escape_with_underscore" (func $hydra.formatting.escape_with_underscore) )
  (export "hydra.formatting.indent_lines" (func $hydra.formatting.indent_lines) )
  (export "hydra.formatting.java_style_comment" (func $hydra.formatting.java_style_comment) )
  (export "hydra.formatting.map_first_letter" (func $hydra.formatting.map_first_letter) )
  (export "hydra.formatting.non_alnum_to_underscores" (func $hydra.formatting.non_alnum_to_underscores) )
  (export "hydra.formatting.normalize_comment" (func $hydra.formatting.normalize_comment) )
  (export "hydra.formatting.sanitize_with_underscores" (func $hydra.formatting.sanitize_with_underscores) )
  (export "hydra.formatting.show_list" (func $hydra.formatting.show_list) )
  (export "hydra.formatting.strip_leading_and_trailing_whitespace" (func $hydra.formatting.strip_leading_and_trailing_whitespace) )
  (export "hydra.formatting.with_character_aliases" (func $hydra.formatting.with_character_aliases) )
  (export "hydra.formatting.wrap_line" (func $hydra.formatting.wrap_line) )
  (func $hydra.formatting.capitalize (result i32)
  i32.const 0
  i32.const 0
  call $hydra.formatting.map_first_letter
)
  (func $hydra.formatting.convert_case (param $from i32) (param $to i32) (param $original i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $acc i32)
  (local $arg_ i32)
  (local $by_caps i32)
  (local $by_underscores i32)
  (local $c i32)
  (local $parts i32)
  (local $split_on_uppercase i32)
  (local $uc i32)
  (local $v i32)
  local.get $c
  call $hydra.lib.chars.is_upper
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
  local.get $__rec_ptr
  i32.const 1
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.get $__rec_ptr
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  call $hydra.lib.logic.if_else
  local.get $acc
  local.get $c
  local.get $uc
  call $hydra.lib.pairs.first
  call $hydra.lib.lists.cons
  local.get $uc
  call $hydra.lib.pairs.second
  call $hydra.lib.lists.cons
  local.get $acc
  call $hydra.lib.lists.uncons
  call $hydra.lib.maybes.map
  call $hydra.lib.maybes.from_maybe
  call $hydra.lib.lists.concat2
  local.set $split_on_uppercase
  i32.const 0
  local.get $split_on_uppercase
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
  local.get $__rec_ptr
  i32.const 1
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.get $__rec_ptr
  local.get $original
  call $hydra.formatting.decapitalize
  call $hydra.lib.strings.to_list
  call $hydra.lib.lists.reverse
  call $hydra.lib.lists.foldl
  call $hydra.lib.lists.map
  local.set $by_caps
  i32.const 1080
  local.get $original
  call $hydra.lib.strings.split_on
  local.set $by_underscores
  (block $end_case_convention (result i32)
  (block $upper_snake
  (block $lower_snake
  (block $pascal
  (block $camel
  local.get $from
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $camel $pascal $lower_snake $upper_snake $upper_snake
)
  local.get $v
  drop
  local.get $by_caps
  br $end_case_convention
)
  local.get $v
  drop
  local.get $by_caps
  br $end_case_convention
)
  local.get $v
  drop
  local.get $by_underscores
  br $end_case_convention
)
  local.get $v
  drop
  local.get $by_underscores
  br $end_case_convention
)
  local.set $parts
  (block $end_case_convention (result i32)
  (block $upper_snake
  (block $lower_snake
  (block $pascal
  (block $camel
  local.get $to
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $camel $pascal $lower_snake $upper_snake $upper_snake
)
  local.get $v
  drop
  local.get $arg_
  call $hydra.lib.strings.to_lower
  call $hydra.formatting.capitalize
  local.get $parts
  call $hydra.lib.lists.map
  call $hydra.lib.strings.cat
  call $hydra.formatting.decapitalize
  br $end_case_convention
)
  local.get $v
  drop
  local.get $arg_
  call $hydra.lib.strings.to_lower
  call $hydra.formatting.capitalize
  local.get $parts
  call $hydra.lib.lists.map
  call $hydra.lib.strings.cat
  br $end_case_convention
)
  local.get $v
  drop
  i32.const 1080
  i32.const 0
  local.get $parts
  call $hydra.lib.lists.map
  call $hydra.lib.strings.intercalate
  br $end_case_convention
)
  local.get $v
  drop
  i32.const 1080
  i32.const 0
  local.get $parts
  call $hydra.lib.lists.map
  call $hydra.lib.strings.intercalate
  br $end_case_convention
)
)
  (func $hydra.formatting.convert_case_camel_or_underscore_to_lower_snake (param $s i32) (result i32)
  (local $p i32)
  (local $parts i32)
  (local $snake_parts i32)
  i32.const 1080
  local.get $s
  call $hydra.lib.strings.split_on
  local.set $parts
  local.get $p
  call $hydra.formatting.convert_case_camel_to_lower_snake
  local.get $parts
  call $hydra.lib.lists.map
  local.set $snake_parts
  i32.const 1080
  local.get $snake_parts
  call $hydra.lib.strings.intercalate
)
  (func $hydra.formatting.convert_case_camel_to_lower_snake (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 0
  i32.const 0
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
  i32.const 2
  i32.const 0
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
  call $hydra.formatting.convert_case
)
  (func $hydra.formatting.convert_case_camel_to_upper_snake (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 0
  i32.const 0
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
  i32.const 3
  i32.const 0
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
  call $hydra.formatting.convert_case
)
  (func $hydra.formatting.convert_case_pascal_to_upper_snake (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 1
  i32.const 0
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
  i32.const 3
  i32.const 0
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
  call $hydra.formatting.convert_case
)
  (func $hydra.formatting.decapitalize (result i32)
  i32.const 0
  i32.const 0
  call $hydra.formatting.map_first_letter
)
  (func $hydra.formatting.escape_with_underscore (param $reserved i32) (param $s i32) (result i32)
  local.get $s
  local.get $reserved
  call $hydra.lib.sets.member
  local.get $s
  i32.const 1080
  call $hydra.lib.strings.cat2
  local.get $s
  call $hydra.lib.logic.if_else
)
  (func $hydra.formatting.indent_lines (param $s i32) (result i32)
  (local $indent i32)
  (local $l i32)
  i32.const 1036
  local.get $l
  call $hydra.lib.strings.cat2
  local.set $indent
  local.get $indent
  local.get $s
  call $hydra.lib.strings.lines
  call $hydra.lib.lists.map
  call $hydra.lib.strings.unlines
)
  (func $hydra.formatting.java_style_comment (param $s i32) (result i32)
  i32.const 1062
  i32.const 1044
  call $hydra.lib.strings.cat2
  local.get $s
  call $hydra.lib.strings.cat2
  i32.const 1028
  call $hydra.lib.strings.cat2
)
  (func $hydra.formatting.map_first_letter (param $mapping i32) (param $s i32) (result i32)
  (local $first_letter i32)
  (local $list i32)
  (local $uc i32)
  local.get $s
  call $hydra.lib.strings.null
  local.get $s
  local.get $s
  call $hydra.lib.strings.to_list
  local.set $list
  local.get $s
  local.get $uc
  call $hydra.lib.pairs.first
  call $hydra.lib.lists.pure
  call $hydra.lib.strings.from_list
  drop
  local.get $mapping
  drop
  i32.const 0
  local.set $first_letter
  local.get $first_letter
  local.get $uc
  call $hydra.lib.pairs.second
  call $hydra.lib.strings.from_list
  call $hydra.lib.strings.cat2
  local.get $list
  call $hydra.lib.lists.uncons
  call $hydra.lib.maybes.map
  call $hydra.lib.maybes.from_maybe
  call $hydra.lib.logic.if_else
)
  (func $hydra.formatting.non_alnum_to_underscores (param $input i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $b i32)
  (local $c i32)
  (local $is_alnum i32)
  (local $p i32)
  (local $replace i32)
  (local $result i32)
  (local $s i32)
  local.get $c
  i32.const 65
  call $hydra.lib.equality.gte
  local.get $c
  i32.const 90
  call $hydra.lib.equality.lte
  call $hydra.lib.logic.and
  local.get $c
  i32.const 97
  call $hydra.lib.equality.gte
  local.get $c
  i32.const 122
  call $hydra.lib.equality.lte
  call $hydra.lib.logic.and
  local.get $c
  i32.const 48
  call $hydra.lib.equality.gte
  local.get $c
  i32.const 57
  call $hydra.lib.equality.lte
  call $hydra.lib.logic.and
  call $hydra.lib.logic.or
  call $hydra.lib.logic.or
  local.set $is_alnum
  local.get $p
  call $hydra.lib.pairs.first
  local.set $s
  local.get $p
  call $hydra.lib.pairs.second
  local.set $b
  local.get $c
  drop
  local.get $is_alnum
  drop
  i32.const 0
  local.get $c
  local.get $s
  call $hydra.lib.lists.cons
  i32.const 0
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
  local.get $s
  i32.const 1
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
  i32.const 95
  local.get $s
  call $hydra.lib.lists.cons
  i32.const 1
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
  call $hydra.lib.logic.if_else
  call $hydra.lib.logic.if_else
  local.set $replace
  local.get $replace
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  i32.const 0
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
  local.get $input
  call $hydra.lib.strings.to_list
  call $hydra.lib.lists.foldl
  local.set $result
  local.get $result
  call $hydra.lib.pairs.first
  call $hydra.lib.lists.reverse
  call $hydra.lib.strings.from_list
)
  (func $hydra.formatting.normalize_comment (param $s i32) (result i32)
  (local $appended i32)
  (local $last_char i32)
  (local $last_idx i32)
  (local $stripped i32)
  local.get $s
  call $hydra.formatting.strip_leading_and_trailing_whitespace
  local.set $stripped
  local.get $stripped
  call $hydra.lib.strings.null
  i32.const 1024
  local.get $stripped
  call $hydra.lib.strings.length
  i32.const 1
  call $hydra.lib.math.sub
  local.set $last_idx
  local.get $stripped
  i32.const 1057
  call $hydra.lib.strings.cat2
  local.set $appended
  local.get $appended
  local.get $last_char
  i32.const 46
  call $hydra.lib.equality.equal
  local.get $stripped
  local.get $appended
  call $hydra.lib.logic.if_else
  local.get $last_idx
  local.get $stripped
  call $hydra.lib.strings.maybe_char_at
  call $hydra.lib.maybes.maybe
  call $hydra.lib.logic.if_else
)
  (func $hydra.formatting.sanitize_with_underscores (param $reserved i32) (param $s i32) (result i32)
  local.get $reserved
  local.get $s
  call $hydra.formatting.non_alnum_to_underscores
  call $hydra.formatting.escape_with_underscore
)
  (func $hydra.formatting.show_list (param $f i32) (param $els i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 1070
  i32.const 1051
  local.get $f
  local.get $els
  call $hydra.lib.lists.map
  call $hydra.lib.strings.intercalate
  i32.const 1075
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
)
  (func $hydra.formatting.strip_leading_and_trailing_whitespace (param $s i32) (result i32)
  i32.const 0
  i32.const 0
  local.get $s
  call $hydra.lib.strings.to_list
  call $hydra.lib.lists.reverse
  call $hydra.lib.lists.drop_while
  call $hydra.lib.lists.reverse
  call $hydra.lib.lists.drop_while
  call $hydra.lib.strings.from_list
)
  (func $hydra.formatting.with_character_aliases (param $original i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $alias i32)
  (local $aliases i32)
  (local $c i32)
  i32.const 32
  i32.const 1334
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
  i32.const 33
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
  i32.const 34
  i32.const 1287
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
  i32.const 35
  i32.const 1243
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
  i32.const 36
  i32.const 1151
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
  i32.const 37
  i32.const 1250
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
  i32.const 38
  i32.const 1085
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
  i32.const 39
  i32.const 1092
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
  i32.const 40
  i32.const 1212
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
  i32.const 41
  i32.const 1303
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
  i32.const 42
  i32.const 1100
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
  i32.const 43
  i32.const 1270
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
  i32.const 44
  i32.const 1132
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
  i32.const 45
  i32.const 1234
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
  i32.const 46
  i32.const 1260
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
  i32.const 47
  i32.const 1327
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
  i32.const 58
  i32.const 1123
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
  i32.const 59
  i32.const 1319
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
  i32.const 60
  i32.const 1228
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
  i32.const 61
  i32.const 1161
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
  i32.const 62
  i32.const 1188
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
  i32.const 63
  i32.const 1278
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
  i32.const 64
  i32.const 1141
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
  i32.const 91
  i32.const 1220
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
  i32.const 92
  i32.const 1107
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
  i32.const 93
  i32.const 1311
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
  i32.const 94
  i32.const 1115
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
  i32.const 95
  i32.const 1202
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
  i32.const 96
  i32.const 1179
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
  i32.const 123
  i32.const 1194
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
  i32.const 124
  i32.const 1349
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
  i32.const 125
  i32.const 1295
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
  i32.const 126
  i32.const 1340
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
  i32.const 136
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 33
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=132
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=128
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=124
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=120
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=116
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=112
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=108
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=104
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=100
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=96
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=92
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=88
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=84
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
  local.set $aliases
  local.get $c
  call $hydra.lib.lists.pure
  i32.const 0
  local.get $c
  local.get $aliases
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.map
  call $hydra.lib.maybes.from_maybe
  local.set $alias
  i32.const 0
  local.get $alias
  local.get $original
  call $hydra.lib.strings.to_list
  call $hydra.lib.lists.map
  call $hydra.lib.lists.concat
  call $hydra.lib.lists.filter
  call $hydra.lib.strings.from_list
)
  (func $hydra.formatting.wrap_line (param $maxlen i32) (param $input i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $c i32)
  (local $helper i32)
  (local $pfx_init i32)
  (local $prefix i32)
  (local $prev i32)
  (local $rem i32)
  (local $span_result i32)
  (local $suffix i32)
  (local $trunc i32)
  local.get $maxlen
  local.get $rem
  call $hydra.lib.lists.take
  local.set $trunc
  local.get $c
  i32.const 32
  call $hydra.lib.equality.equal
  call $hydra.lib.logic.not
  local.get $c
  i32.const 9
  call $hydra.lib.equality.equal
  call $hydra.lib.logic.not
  call $hydra.lib.logic.and
  local.get $trunc
  call $hydra.lib.lists.reverse
  call $hydra.lib.lists.span
  local.set $span_result
  local.get $span_result
  call $hydra.lib.pairs.second
  call $hydra.lib.lists.reverse
  local.set $prefix
  local.get $span_result
  call $hydra.lib.pairs.first
  call $hydra.lib.lists.reverse
  local.set $suffix
  local.get $rem
  call $hydra.lib.lists.length
  local.get $maxlen
  call $hydra.lib.equality.lte
  local.get $rem
  local.get $prev
  call $hydra.lib.lists.cons
  call $hydra.lib.lists.reverse
  local.get $prefix
  call $hydra.lib.lists.null
  local.get $trunc
  local.get $prev
  call $hydra.lib.lists.cons
  drop
  local.get $maxlen
  local.get $rem
  call $hydra.lib.lists.drop
  drop
  local.get $helper
  drop
  i32.const 0
  local.get $trunc
  local.get $prev
  call $hydra.lib.lists.cons
  drop
  local.get $maxlen
  local.get $rem
  call $hydra.lib.lists.drop
  drop
  local.get $helper
  drop
  i32.const 0
  local.get $pfx_init
  local.get $prev
  call $hydra.lib.lists.cons
  drop
  local.get $suffix
  local.get $maxlen
  local.get $rem
  call $hydra.lib.lists.drop
  call $hydra.lib.lists.concat2
  drop
  local.get $helper
  drop
  i32.const 0
  local.get $prefix
  call $hydra.lib.lists.maybe_init
  call $hydra.lib.maybes.map
  call $hydra.lib.maybes.from_maybe
  call $hydra.lib.logic.if_else
  call $hydra.lib.logic.if_else
  local.set $helper
  i32.const 10
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
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  drop
  local.get $input
  call $hydra.lib.strings.to_list
  drop
  local.get $helper
  drop
  i32.const 0
  call $hydra.lib.lists.intercalate
  call $hydra.lib.strings.from_list
)
)
