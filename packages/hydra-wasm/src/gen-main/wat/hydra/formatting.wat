(module
  (import "hydra.lib.chars" "hydra.lib.chars.is_alpha_num" (func $hydra.lib.chars.is_alpha_num (param i32) (result i32) ) )
  (import "hydra.lib.chars" "hydra.lib.chars.is_space" (func $hydra.lib.chars.is_space (param i32) (result i32) ) )
  (import "hydra.lib.chars" "hydra.lib.chars.is_upper" (func $hydra.lib.chars.is_upper (param i32) (result i32) ) )
  (import "hydra.lib.equality" "hydra.lib.equality.equal" (func $hydra.lib.equality.equal (param i32) (result i32) ) )
  (import "hydra.lib.equality" "hydra.lib.equality.gte" (func $hydra.lib.equality.gte (param i32) (result i32) ) )
  (import "hydra.lib.equality" "hydra.lib.equality.lte" (func $hydra.lib.equality.lte (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.concat" (func $hydra.lib.lists.concat (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.concat2" (func $hydra.lib.lists.concat2 (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.cons" (func $hydra.lib.lists.cons (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.drop" (func $hydra.lib.lists.drop (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.drop_while" (func $hydra.lib.lists.drop_while (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.filter" (func $hydra.lib.lists.filter (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.foldl" (func $hydra.lib.lists.foldl (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.head" (func $hydra.lib.lists.head (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.init" (func $hydra.lib.lists.init (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.intercalate" (func $hydra.lib.lists.intercalate (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.length" (func $hydra.lib.lists.length (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.null" (func $hydra.lib.lists.null (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.pure" (func $hydra.lib.lists.pure (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.reverse" (func $hydra.lib.lists.reverse (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.span" (func $hydra.lib.lists.span (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.tail" (func $hydra.lib.lists.tail (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.take" (func $hydra.lib.lists.take (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.and" (func $hydra.lib.logic.and (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.if_else" (func $hydra.lib.logic.if_else (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.not" (func $hydra.lib.logic.not (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.or" (func $hydra.lib.logic.or (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.from_list" (func $hydra.lib.maps.from_list (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.lookup" (func $hydra.lib.maps.lookup (param i32) (result i32) ) )
  (import "hydra.lib.math" "hydra.lib.math.sub" (func $hydra.lib.math.sub (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.from_maybe" (func $hydra.lib.maybes.from_maybe (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.map" (func $hydra.lib.maybes.map (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.first" (func $hydra.lib.pairs.first (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.second" (func $hydra.lib.pairs.second (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.member" (func $hydra.lib.sets.member (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat" (func $hydra.lib.strings.cat (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat2" (func $hydra.lib.strings.cat2 (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.char_at" (func $hydra.lib.strings.char_at (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.from_list" (func $hydra.lib.strings.from_list (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.intercalate" (func $hydra.lib.strings.intercalate (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.length" (func $hydra.lib.strings.length (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.lines" (func $hydra.lib.strings.lines (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.null" (func $hydra.lib.strings.null (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.split_on" (func $hydra.lib.strings.split_on (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.to_list" (func $hydra.lib.strings.to_list (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.to_lower" (func $hydra.lib.strings.to_lower (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.to_upper" (func $hydra.lib.strings.to_upper (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.unlines" (func $hydra.lib.strings.unlines (param i32) (result i32) ) )
  (memory $memory 1 )
  (export "memory" (memory $memory) )
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
  call $hydra.lib.strings.to_upper
  call $hydra.formatting.map_first_letter
)
  (func $hydra.formatting.convert_case (param $from i32) (param $to i32) (param $original i32) (result i32)
  (local $acc i32)
  (local $arg_ i32)
  (local $by_caps i32)
  (local $by_underscores i32)
  (local $c i32)
  (local $parts i32)
  (local $split_on_uppercase i32)
  local.get $c
  call $hydra.lib.chars.is_upper
  i32.const 1
  ;; list elements follow
  i32.const 0
  ;; list elements follow
  i32.const 0
  ;; list elements follow
  call $hydra.lib.logic.if_else
  local.get $c
  local.get $acc
  call $hydra.lib.lists.head
  call $hydra.lib.lists.cons
  local.get $acc
  call $hydra.lib.lists.tail
  call $hydra.lib.lists.cons
  call $hydra.lib.lists.concat2
  local.set $split_on_uppercase
  call $hydra.lib.strings.from_list
  local.get $split_on_uppercase
  i32.const 1
  ;; list elements follow
  i32.const 0
  ;; list elements follow
  local.get $original
  call $hydra.formatting.decapitalize
  call $hydra.lib.strings.to_list
  call $hydra.lib.lists.reverse
  call $hydra.lib.lists.foldl
  call $hydra.lib.lists.map
  local.set $by_caps
  i32.const 0 ;; string: "_"
  local.get $original
  call $hydra.lib.strings.split_on
  local.set $by_underscores
  (block $end_case_convention (result i32)
  (block $upper_snake
  (block $lower_snake
  (block $pascal
  (block $camel
  local.get $from
  br_table $camel $pascal $lower_snake $upper_snake $upper_snake
)
  local.get $by_caps
  br $end_case_convention
)
  local.get $by_caps
  br $end_case_convention
)
  local.get $by_underscores
  br $end_case_convention
)
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
  br_table $camel $pascal $lower_snake $upper_snake $upper_snake
)
  local.get $arg_
  call $hydra.lib.strings.to_lower
  call $hydra.formatting.capitalize
  local.get $parts
  call $hydra.lib.lists.map
  call $hydra.lib.strings.cat
  call $hydra.formatting.decapitalize
  br $end_case_convention
)
  local.get $arg_
  call $hydra.lib.strings.to_lower
  call $hydra.formatting.capitalize
  local.get $parts
  call $hydra.lib.lists.map
  call $hydra.lib.strings.cat
  br $end_case_convention
)
  i32.const 0 ;; string: "_"
  call $hydra.lib.strings.to_lower
  local.get $parts
  call $hydra.lib.lists.map
  call $hydra.lib.strings.intercalate
  br $end_case_convention
)
  i32.const 0 ;; string: "_"
  call $hydra.lib.strings.to_upper
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
  i32.const 0 ;; string: "_"
  local.get $s
  call $hydra.lib.strings.split_on
  local.set $parts
  local.get $p
  call $hydra.formatting.convert_case_camel_to_lower_snake
  local.get $parts
  call $hydra.lib.lists.map
  local.set $snake_parts
  i32.const 0 ;; string: "_"
  local.get $snake_parts
  call $hydra.lib.strings.intercalate
)
  (func $hydra.formatting.convert_case_camel_to_lower_snake (result i32)
  i32.const 0
  i32.const 0
  call $hydra.formatting.convert_case
)
  (func $hydra.formatting.convert_case_camel_to_upper_snake (result i32)
  i32.const 0
  i32.const 0
  call $hydra.formatting.convert_case
)
  (func $hydra.formatting.convert_case_pascal_to_upper_snake (result i32)
  i32.const 0
  i32.const 0
  call $hydra.formatting.convert_case
)
  (func $hydra.formatting.decapitalize (result i32)
  call $hydra.lib.strings.to_lower
  call $hydra.formatting.map_first_letter
)
  (func $hydra.formatting.escape_with_underscore (param $reserved i32) (param $s i32) (result i32)
  local.get $s
  local.get $reserved
  call $hydra.lib.sets.member
  local.get $s
  i32.const 0 ;; string: "_"
  call $hydra.lib.strings.cat2
  local.get $s
  call $hydra.lib.logic.if_else
)
  (func $hydra.formatting.indent_lines (param $s i32) (result i32)
  (local $indent i32)
  (local $l i32)
  i32.const 0 ;; string: "    "
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
  i32.const 0 ;; string: "/**
"
  i32.const 0 ;; string: " * "
  call $hydra.lib.strings.cat2
  local.get $s
  call $hydra.lib.strings.cat2
  i32.const 0 ;; string: "
 */"
  call $hydra.lib.strings.cat2
)
  (func $hydra.formatting.map_first_letter (param $mapping i32) (param $s i32) (result i32)
  (local $first_letter i32)
  (local $list i32)
  local.get $s
  call $hydra.lib.strings.null
  local.get $s
  local.get $s
  call $hydra.lib.strings.to_list
  local.set $list
  local.get $list
  call $hydra.lib.lists.head
  call $hydra.lib.lists.pure
  call $hydra.lib.strings.from_list
  local.get $mapping
  local.set $first_letter
  local.get $first_letter
  local.get $list
  call $hydra.lib.lists.tail
  call $hydra.lib.strings.from_list
  call $hydra.lib.strings.cat2
  call $hydra.lib.logic.if_else
)
  (func $hydra.formatting.non_alnum_to_underscores (param $input i32) (result i32)
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
  local.get $is_alnum
  local.get $c
  local.get $s
  call $hydra.lib.lists.cons
  i32.const 0
  local.get $b
  local.get $s
  i32.const 1
  i32.const 95
  local.get $s
  call $hydra.lib.lists.cons
  i32.const 1
  call $hydra.lib.logic.if_else
  call $hydra.lib.logic.if_else
  local.set $replace
  local.get $replace
  i32.const 0
  ;; list elements follow
  i32.const 0
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
  (local $last_char i32)
  (local $last_idx i32)
  (local $stripped i32)
  local.get $s
  call $hydra.formatting.strip_leading_and_trailing_whitespace
  local.set $stripped
  local.get $stripped
  call $hydra.lib.strings.null
  i32.const 0 ;; string: ""
  local.get $stripped
  call $hydra.lib.strings.length
  i32.const 1
  call $hydra.lib.math.sub
  local.set $last_idx
  local.get $last_idx
  local.get $stripped
  call $hydra.lib.strings.char_at
  local.set $last_char
  local.get $last_char
  i32.const 46
  call $hydra.lib.equality.equal
  local.get $stripped
  local.get $stripped
  i32.const 0 ;; string: "."
  call $hydra.lib.strings.cat2
  call $hydra.lib.logic.if_else
  call $hydra.lib.logic.if_else
)
  (func $hydra.formatting.sanitize_with_underscores (param $reserved i32) (param $s i32) (result i32)
  local.get $reserved
  local.get $s
  call $hydra.formatting.non_alnum_to_underscores
  call $hydra.formatting.escape_with_underscore
)
  (func $hydra.formatting.show_list (param $f i32) (param $els i32) (result i32)
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "["
  i32.const 0 ;; string: ", "
  local.get $f
  local.get $els
  call $hydra.lib.lists.map
  call $hydra.lib.strings.intercalate
  i32.const 0 ;; string: "]"
  call $hydra.lib.strings.cat
)
  (func $hydra.formatting.strip_leading_and_trailing_whitespace (param $s i32) (result i32)
  call $hydra.lib.chars.is_space
  call $hydra.lib.chars.is_space
  local.get $s
  call $hydra.lib.strings.to_list
  call $hydra.lib.lists.reverse
  call $hydra.lib.lists.drop_while
  call $hydra.lib.lists.reverse
  call $hydra.lib.lists.drop_while
  call $hydra.lib.strings.from_list
)
  (func $hydra.formatting.with_character_aliases (param $original i32) (result i32)
  (local $alias i32)
  (local $aliases i32)
  (local $c i32)
  i32.const 33
  ;; list elements follow
  i32.const 32
  i32.const 0 ;; string: "sp"
  i32.const 33
  i32.const 0 ;; string: "excl"
  i32.const 34
  i32.const 0 ;; string: "quot"
  i32.const 35
  i32.const 0 ;; string: "num"
  i32.const 36
  i32.const 0 ;; string: "dollar"
  i32.const 37
  i32.const 0 ;; string: "percnt"
  i32.const 38
  i32.const 0 ;; string: "amp"
  i32.const 39
  i32.const 0 ;; string: "apos"
  i32.const 40
  i32.const 0 ;; string: "lpar"
  i32.const 41
  i32.const 0 ;; string: "rpar"
  i32.const 42
  i32.const 0 ;; string: "ast"
  i32.const 43
  i32.const 0 ;; string: "plus"
  i32.const 44
  i32.const 0 ;; string: "comma"
  i32.const 45
  i32.const 0 ;; string: "minus"
  i32.const 46
  i32.const 0 ;; string: "period"
  i32.const 47
  i32.const 0 ;; string: "sol"
  i32.const 58
  i32.const 0 ;; string: "colon"
  i32.const 59
  i32.const 0 ;; string: "semi"
  i32.const 60
  i32.const 0 ;; string: "lt"
  i32.const 61
  i32.const 0 ;; string: "equals"
  i32.const 62
  i32.const 0 ;; string: "gt"
  i32.const 63
  i32.const 0 ;; string: "quest"
  i32.const 64
  i32.const 0 ;; string: "commat"
  i32.const 91
  i32.const 0 ;; string: "lsqb"
  i32.const 92
  i32.const 0 ;; string: "bsol"
  i32.const 93
  i32.const 0 ;; string: "rsqb"
  i32.const 94
  i32.const 0 ;; string: "circ"
  i32.const 95
  i32.const 0 ;; string: "lowbar"
  i32.const 96
  i32.const 0 ;; string: "grave"
  i32.const 123
  i32.const 0 ;; string: "lcub"
  i32.const 124
  i32.const 0 ;; string: "verbar"
  i32.const 125
  i32.const 0 ;; string: "rcub"
  i32.const 126
  i32.const 0 ;; string: "tilde"
  call $hydra.lib.maps.from_list
  local.set $aliases
  local.get $c
  call $hydra.lib.lists.pure
  call $hydra.lib.strings.to_list
  local.get $c
  local.get $aliases
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.map
  call $hydra.lib.maybes.from_maybe
  local.set $alias
  call $hydra.lib.chars.is_alpha_num
  local.get $alias
  local.get $original
  call $hydra.lib.strings.to_list
  call $hydra.lib.lists.map
  call $hydra.lib.lists.concat
  call $hydra.lib.lists.filter
  call $hydra.lib.strings.from_list
)
  (func $hydra.formatting.wrap_line (param $maxlen i32) (param $input i32) (result i32)
  (local $c i32)
  (local $helper i32)
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
  local.get $maxlen
  local.get $rem
  call $hydra.lib.lists.drop
  local.get $helper
  local.get $prefix
  call $hydra.lib.lists.init
  local.get $prev
  call $hydra.lib.lists.cons
  local.get $suffix
  local.get $maxlen
  local.get $rem
  call $hydra.lib.lists.drop
  call $hydra.lib.lists.concat2
  local.get $helper
  call $hydra.lib.logic.if_else
  call $hydra.lib.logic.if_else
  local.set $helper
  i32.const 1
  ;; list elements follow
  i32.const 10
  i32.const 0
  ;; list elements follow
  local.get $input
  call $hydra.lib.strings.to_list
  local.get $helper
  call $hydra.lib.lists.intercalate
  call $hydra.lib.strings.from_list
)
)
