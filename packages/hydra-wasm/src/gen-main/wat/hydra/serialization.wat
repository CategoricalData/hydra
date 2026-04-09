(module
  (import "hydra.lib.equality" "hydra.lib.equality.compare" (func $hydra.lib.equality.compare (param i32) (result i32) ) )
  (import "hydra.lib.equality" "hydra.lib.equality.equal" (func $hydra.lib.equality.equal (param i32) (result i32) ) )
  (import "hydra.lib.equality" "hydra.lib.equality.gt" (func $hydra.lib.equality.gt (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.cons" (func $hydra.lib.lists.cons (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.drop" (func $hydra.lib.lists.drop (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.filter" (func $hydra.lib.lists.filter (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.foldl" (func $hydra.lib.lists.foldl (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.head" (func $hydra.lib.lists.head (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.intersperse" (func $hydra.lib.lists.intersperse (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.length" (func $hydra.lib.lists.length (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.null" (func $hydra.lib.lists.null (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.reverse" (func $hydra.lib.lists.reverse (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.safe_head" (func $hydra.lib.lists.safe_head (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.tail" (func $hydra.lib.lists.tail (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_int32" (func $hydra.lib.literals.show_int32 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_string" (func $hydra.lib.literals.show_string (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.and" (func $hydra.lib.logic.and (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.if_else" (func $hydra.lib.logic.if_else (param i32) (result i32) ) )
  (import "hydra.lib.math" "hydra.lib.math.add" (func $hydra.lib.math.add (param i32) (result i32) ) )
  (import "hydra.lib.math" "hydra.lib.math.mul" (func $hydra.lib.math.mul (param i32) (result i32) ) )
  (import "hydra.lib.math" "hydra.lib.math.sub" (func $hydra.lib.math.sub (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.from_maybe" (func $hydra.lib.maybes.from_maybe (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.maybe" (func $hydra.lib.maybes.maybe (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat" (func $hydra.lib.strings.cat (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat2" (func $hydra.lib.strings.cat2 (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.intercalate" (func $hydra.lib.strings.intercalate (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.length" (func $hydra.lib.strings.length (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.lines" (func $hydra.lib.strings.lines (param i32) (result i32) ) )
  (memory $memory 1 )
  (export "memory" (memory $memory) )
  (export "hydra.serialization.angle_braces" (func $hydra.serialization.angle_braces) )
  (export "hydra.serialization.angle_braces_list" (func $hydra.serialization.angle_braces_list) )
  (export "hydra.serialization.braces_list_adaptive" (func $hydra.serialization.braces_list_adaptive) )
  (export "hydra.serialization.bracket_list" (func $hydra.serialization.bracket_list) )
  (export "hydra.serialization.bracket_list_adaptive" (func $hydra.serialization.bracket_list_adaptive) )
  (export "hydra.serialization.brackets" (func $hydra.serialization.brackets) )
  (export "hydra.serialization.comma_sep" (func $hydra.serialization.comma_sep) )
  (export "hydra.serialization.cst" (func $hydra.serialization.cst) )
  (export "hydra.serialization.curly_block" (func $hydra.serialization.curly_block) )
  (export "hydra.serialization.curly_braces" (func $hydra.serialization.curly_braces) )
  (export "hydra.serialization.curly_braces_list" (func $hydra.serialization.curly_braces_list) )
  (export "hydra.serialization.custom_indent" (func $hydra.serialization.custom_indent) )
  (export "hydra.serialization.custom_indent_block" (func $hydra.serialization.custom_indent_block) )
  (export "hydra.serialization.dot_sep" (func $hydra.serialization.dot_sep) )
  (export "hydra.serialization.double_newline_sep" (func $hydra.serialization.double_newline_sep) )
  (export "hydra.serialization.double_space" (func $hydra.serialization.double_space) )
  (export "hydra.serialization.expression_length" (func $hydra.serialization.expression_length) )
  (export "hydra.serialization.full_block_style" (func $hydra.serialization.full_block_style) )
  (export "hydra.serialization.half_block_style" (func $hydra.serialization.half_block_style) )
  (export "hydra.serialization.ifx" (func $hydra.serialization.ifx) )
  (export "hydra.serialization.indent" (func $hydra.serialization.indent) )
  (export "hydra.serialization.indent_block" (func $hydra.serialization.indent_block) )
  (export "hydra.serialization.indent_subsequent_lines" (func $hydra.serialization.indent_subsequent_lines) )
  (export "hydra.serialization.infix_ws" (func $hydra.serialization.infix_ws) )
  (export "hydra.serialization.infix_ws_list" (func $hydra.serialization.infix_ws_list) )
  (export "hydra.serialization.inline_style" (func $hydra.serialization.inline_style) )
  (export "hydra.serialization.newline_sep" (func $hydra.serialization.newline_sep) )
  (export "hydra.serialization.no_padding" (func $hydra.serialization.no_padding) )
  (export "hydra.serialization.no_sep" (func $hydra.serialization.no_sep) )
  (export "hydra.serialization.num" (func $hydra.serialization.num) )
  (export "hydra.serialization.op" (func $hydra.serialization.op) )
  (export "hydra.serialization.or_op" (func $hydra.serialization.or_op) )
  (export "hydra.serialization.or_sep" (func $hydra.serialization.or_sep) )
  (export "hydra.serialization.paren_list" (func $hydra.serialization.paren_list) )
  (export "hydra.serialization.parens" (func $hydra.serialization.parens) )
  (export "hydra.serialization.parentheses" (func $hydra.serialization.parentheses) )
  (export "hydra.serialization.parenthesize" (func $hydra.serialization.parenthesize) )
  (export "hydra.serialization.prefix" (func $hydra.serialization.prefix) )
  (export "hydra.serialization.print_expr" (func $hydra.serialization.print_expr) )
  (export "hydra.serialization.semicolon_sep" (func $hydra.serialization.semicolon_sep) )
  (export "hydra.serialization.sep" (func $hydra.serialization.sep) )
  (export "hydra.serialization.space_sep" (func $hydra.serialization.space_sep) )
  (export "hydra.serialization.square_brackets" (func $hydra.serialization.square_brackets) )
  (export "hydra.serialization.structural_sep" (func $hydra.serialization.structural_sep) )
  (export "hydra.serialization.structural_space_sep" (func $hydra.serialization.structural_space_sep) )
  (export "hydra.serialization.suffix" (func $hydra.serialization.suffix) )
  (export "hydra.serialization.sym" (func $hydra.serialization.sym) )
  (export "hydra.serialization.symbol_sep" (func $hydra.serialization.symbol_sep) )
  (export "hydra.serialization.tab_indent" (func $hydra.serialization.tab_indent) )
  (export "hydra.serialization.tab_indent_double_space" (func $hydra.serialization.tab_indent_double_space) )
  (export "hydra.serialization.tab_indent_single_space" (func $hydra.serialization.tab_indent_single_space) )
  (export "hydra.serialization.unsupported_type" (func $hydra.serialization.unsupported_type) )
  (export "hydra.serialization.unsupported_variant" (func $hydra.serialization.unsupported_variant) )
  (export "hydra.serialization.with_comma" (func $hydra.serialization.with_comma) )
  (export "hydra.serialization.with_semi" (func $hydra.serialization.with_semi) )
  (func $hydra.serialization.angle_braces (result i32)
  i32.const 0 ;; string: "<"
  i32.const 0 ;; string: ">"
)
  (func $hydra.serialization.angle_braces_list (param $style i32) (param $els i32) (result i32)
  local.get $els
  call $hydra.lib.lists.null
  i32.const 0 ;; string: "<>"
  call $hydra.serialization.cst
  call $hydra.serialization.angle_braces
  local.get $style
  local.get $style
  local.get $els
  call $hydra.serialization.comma_sep
  call $hydra.serialization.brackets
  call $hydra.lib.logic.if_else
)
  (func $hydra.serialization.braces_list_adaptive (param $els i32) (result i32)
  (local $inline_list i32)
  i32.const 0
  call $hydra.serialization.inline_style
  local.get $els
  call $hydra.serialization.curly_braces_list
  local.set $inline_list
  local.get $inline_list
  call $hydra.serialization.expression_length
  i32.const 70
  call $hydra.lib.equality.gt
  i32.const 0
  call $hydra.serialization.half_block_style
  local.get $els
  call $hydra.serialization.curly_braces_list
  local.get $inline_list
  call $hydra.lib.logic.if_else
)
  (func $hydra.serialization.bracket_list (param $style i32) (param $els i32) (result i32)
  local.get $els
  call $hydra.lib.lists.null
  i32.const 0 ;; string: "[]"
  call $hydra.serialization.cst
  call $hydra.serialization.square_brackets
  local.get $style
  local.get $style
  local.get $els
  call $hydra.serialization.comma_sep
  call $hydra.serialization.brackets
  call $hydra.lib.logic.if_else
)
  (func $hydra.serialization.bracket_list_adaptive (param $els i32) (result i32)
  (local $inline_list i32)
  call $hydra.serialization.inline_style
  local.get $els
  call $hydra.serialization.bracket_list
  local.set $inline_list
  local.get $inline_list
  call $hydra.serialization.expression_length
  i32.const 70
  call $hydra.lib.equality.gt
  call $hydra.serialization.half_block_style
  local.get $els
  call $hydra.serialization.bracket_list
  local.get $inline_list
  call $hydra.lib.logic.if_else
)
  (func $hydra.serialization.brackets (param $br i32) (param $style i32) (param $e i32) (result i32)
  local.get $br
  local.get $e
  local.get $style
)
  (func $hydra.serialization.comma_sep (result i32)
  i32.const 0 ;; string: ","
  call $hydra.serialization.symbol_sep
)
  (func $hydra.serialization.cst (param $s i32) (result i32)
  local.get $s
  call $hydra.serialization.sym
)
  (func $hydra.serialization.curly_block (param $style i32) (param $e i32) (result i32)
  i32.const 0
  local.get $style
  i32.const 1
  ;; list elements follow
  local.get $e
  call $hydra.serialization.curly_braces_list
)
  (func $hydra.serialization.curly_braces (result i32)
  i32.const 0 ;; string: "{"
  i32.const 0 ;; string: "}"
)
  (func $hydra.serialization.curly_braces_list (param $msymb i32) (param $style i32) (param $els i32) (result i32)
  local.get $els
  call $hydra.lib.lists.null
  i32.const 0 ;; string: "{}"
  call $hydra.serialization.cst
  call $hydra.serialization.curly_braces
  local.get $style
  i32.const 0 ;; string: ","
  local.get $msymb
  call $hydra.lib.maybes.from_maybe
  local.get $style
  local.get $els
  call $hydra.serialization.symbol_sep
  call $hydra.serialization.brackets
  call $hydra.lib.logic.if_else
)
  (func $hydra.serialization.custom_indent (param $idt i32) (param $s i32) (result i32)
  (local $line i32)
  i32.const 0 ;; string: "
"
  local.get $idt
  local.get $line
  call $hydra.lib.strings.cat2
  local.get $s
  call $hydra.lib.strings.lines
  call $hydra.lib.lists.map
  call $hydra.lib.lists.intersperse
  call $hydra.lib.strings.cat
)
  (func $hydra.serialization.custom_indent_block (param $idt i32) (param $els i32) (result i32)
  (local $head i32)
  (local $idt_op i32)
  i32.const 0 ;; string: ""
  call $hydra.serialization.sym
  i32.const 0
  local.get $idt
  i32.const 0
  i32.const 0
  local.set $idt_op
  i32.const 0 ;; string: ""
  call $hydra.serialization.cst
  local.get $els
  call $hydra.lib.lists.length
  i32.const 1
  call $hydra.lib.equality.equal
  local.get $head
  local.get $idt_op
  local.get $head
  i32.const 1
  local.get $els
  call $hydra.lib.lists.drop
  call $hydra.serialization.newline_sep
  call $hydra.serialization.ifx
  call $hydra.lib.logic.if_else
  local.get $els
  call $hydra.lib.lists.safe_head
  call $hydra.lib.maybes.maybe
)
  (func $hydra.serialization.dot_sep (result i32)
  i32.const 0 ;; string: "."
  call $hydra.serialization.sym
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  call $hydra.serialization.sep
)
  (func $hydra.serialization.double_newline_sep (result i32)
  i32.const 0 ;; string: ""
  call $hydra.serialization.sym
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  call $hydra.serialization.sep
)
  (func $hydra.serialization.double_space (result i32)
  i32.const 0 ;; string: "  "
)
  (func $hydra.serialization.expression_length (param $e i32) (result i32)
  (local $base_len i32)
  (local $be i32)
  (local $block_style_length i32)
  (local $bracket_expr_length i32)
  (local $brackets i32)
  (local $brackets_length i32)
  (local $element_lens i32)
  (local $ie i32)
  (local $indent_len i32)
  (local $indented_expression_length i32)
  (local $left_len i32)
  (local $mindent_len i32)
  (local $nl_after_len i32)
  (local $nl_before_len i32)
  (local $num_seps i32)
  (local $oe i32)
  (local $op i32)
  (local $op_expr_length i32)
  (local $op_len i32)
  (local $op_length i32)
  (local $padding i32)
  (local $right_len i32)
  (local $s i32)
  (local $se i32)
  (local $seq_expr_length i32)
  (local $sop_len i32)
  (local $style i32)
  (local $sym_len i32)
  (local $symbol_length i32)
  (local $total_el_len i32)
  (local $ws i32)
  (local $ws_length i32)
  nop
  call $hydra.lib.strings.length
  local.set $symbol_length
  (block $end_ws (result i32)
  (block $double_break
  (block $break_and_indent
  (block $break
  (block $space
  (block $none
  local.get $ws
  br_table $none $space $break $break_and_indent $double_break $double_break
)
  i32.const 0
  br $end_ws
)
  i32.const 1
  br $end_ws
)
  i32.const 10000
  br $end_ws
)
  i32.const 10000
  br $end_ws
)
  i32.const 10000
  br $end_ws
)
  local.set $ws_length
  i32.const 0
  call $hydra.lib.strings.length
  local.get $style
  ;; project field: indent
  call $hydra.lib.maybes.maybe
  local.set $mindent_len
  local.get $style
  ;; project field: newline_before_content
  i32.const 1
  i32.const 0
  call $hydra.lib.logic.if_else
  local.set $nl_before_len
  local.get $style
  ;; project field: newline_after_content
  i32.const 1
  i32.const 0
  call $hydra.lib.logic.if_else
  local.set $nl_after_len
  local.get $mindent_len
  local.get $nl_before_len
  local.get $nl_after_len
  call $hydra.lib.math.add
  call $hydra.lib.math.add
  local.set $block_style_length
  local.get $brackets
  ;; project field: open
  local.get $symbol_length
  local.get $brackets
  ;; project field: close
  local.get $symbol_length
  call $hydra.lib.math.add
  local.set $brackets_length
  local.get $be
  ;; project field: brackets
  local.get $brackets_length
  local.get $be
  ;; project field: enclosed
  call $hydra.serialization.expression_length
  local.get $be
  ;; project field: style
  local.get $block_style_length
  call $hydra.lib.math.add
  call $hydra.lib.math.add
  local.set $bracket_expr_length
  local.get $ie
  ;; project field: expr
  call $hydra.serialization.expression_length
  local.set $base_len
  (block $end_indent_style (result i32)
  (block $subsequent_lines
  (block $all_lines
  local.get $ie
  ;; project field: style
  br_table $all_lines $subsequent_lines $subsequent_lines
)
  local.get $s
  call $hydra.lib.strings.length
  br $end_indent_style
)
  local.get $s
  call $hydra.lib.strings.length
  br $end_indent_style
)
  local.set $indent_len
  local.get $base_len
  local.get $indent_len
  call $hydra.lib.math.add
  local.set $indented_expression_length
  local.get $op
  ;; project field: symbol
  local.get $symbol_length
  local.set $sym_len
  local.get $op
  ;; project field: padding
  local.set $padding
  local.get $padding
  ;; project field: left
  local.get $ws_length
  local.set $left_len
  local.get $padding
  ;; project field: right
  local.get $ws_length
  local.set $right_len
  local.get $sym_len
  local.get $left_len
  local.get $right_len
  call $hydra.lib.math.add
  call $hydra.lib.math.add
  local.set $op_length
  local.get $oe
  ;; project field: op
  local.get $op_length
  local.set $op_len
  local.get $oe
  ;; project field: lhs
  call $hydra.serialization.expression_length
  local.set $left_len
  local.get $oe
  ;; project field: rhs
  call $hydra.serialization.expression_length
  local.set $right_len
  local.get $op_len
  local.get $left_len
  local.get $right_len
  call $hydra.lib.math.add
  call $hydra.lib.math.add
  local.set $op_expr_length
  local.get $se
  ;; project field: op
  local.get $op_length
  local.set $sop_len
  call $hydra.serialization.expression_length
  local.get $se
  ;; project field: elements
  call $hydra.lib.lists.map
  local.set $element_lens
  call $hydra.lib.math.add
  i32.const 0
  local.get $element_lens
  call $hydra.lib.lists.foldl
  local.set $total_el_len
  local.get $se
  ;; project field: elements
  call $hydra.lib.lists.length
  i32.const 1
  call $hydra.lib.math.sub
  local.set $num_seps
  local.get $total_el_len
  local.get $sop_len
  local.get $num_seps
  i32.const 0
  call $hydra.lib.equality.gt
  local.get $num_seps
  i32.const 0
  call $hydra.lib.logic.if_else
  call $hydra.lib.math.mul
  call $hydra.lib.math.add
  local.set $seq_expr_length
  (block $end_expr (result i32)
  (block $seq
  (block $brackets
  (block $op
  (block $indent
  (block $const
  local.get $e
  br_table $const $indent $op $brackets $seq $seq
)
  local.get $s
  local.get $symbol_length
  br $end_expr
)
  local.get $ie
  local.get $indented_expression_length
  br $end_expr
)
  local.get $oe
  local.get $op_expr_length
  br $end_expr
)
  local.get $be
  local.get $bracket_expr_length
  br $end_expr
)
  local.get $se
  local.get $seq_expr_length
  br $end_expr
)
)
  (func $hydra.serialization.full_block_style (result i32)
  call $hydra.serialization.double_space
  i32.const 1
  i32.const 1
)
  (func $hydra.serialization.half_block_style (result i32)
  call $hydra.serialization.double_space
  i32.const 1
  i32.const 0
)
  (func $hydra.serialization.ifx (param $op i32) (param $lhs i32) (param $rhs i32) (result i32)
  local.get $op
  local.get $lhs
  local.get $rhs
)
  (func $hydra.serialization.indent (result i32)
  call $hydra.serialization.double_space
  call $hydra.serialization.custom_indent
)
  (func $hydra.serialization.indent_block (result i32)
  call $hydra.serialization.double_space
  call $hydra.serialization.custom_indent_block
)
  (func $hydra.serialization.indent_subsequent_lines (param $idt i32) (param $e i32) (result i32)
  local.get $idt
  local.get $e
)
  (func $hydra.serialization.infix_ws (param $op i32) (param $l i32) (param $r i32) (result i32)
  i32.const 3
  ;; list elements follow
  local.get $l
  local.get $op
  call $hydra.serialization.cst
  local.get $r
  call $hydra.serialization.space_sep
)
  (func $hydra.serialization.infix_ws_list (param $op i32) (param $opers i32) (result i32)
  (local $e i32)
  (local $fold_fun i32)
  (local $op_expr i32)
  (local $r i32)
  local.get $op
  call $hydra.serialization.cst
  local.set $op_expr
  local.get $e
  call $hydra.lib.lists.null
  i32.const 1
  ;; list elements follow
  local.get $r
  local.get $r
  local.get $op_expr
  local.get $e
  call $hydra.lib.lists.cons
  call $hydra.lib.lists.cons
  call $hydra.lib.logic.if_else
  local.set $fold_fun
  local.get $fold_fun
  i32.const 0
  ;; list elements follow
  local.get $opers
  call $hydra.lib.lists.reverse
  call $hydra.lib.lists.foldl
  call $hydra.serialization.space_sep
)
  (func $hydra.serialization.inline_style (result i32)
  i32.const 0
  i32.const 0
  i32.const 0
)
  (func $hydra.serialization.newline_sep (result i32)
  i32.const 0 ;; string: ""
  call $hydra.serialization.sym
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  call $hydra.serialization.sep
)
  (func $hydra.serialization.no_padding (result i32)
  i32.const 0
  i32.const 0
)
  (func $hydra.serialization.no_sep (result i32)
  i32.const 0 ;; string: ""
  call $hydra.serialization.sym
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  call $hydra.serialization.sep
)
  (func $hydra.serialization.num (param $i i32) (result i32)
  local.get $i
  call $hydra.lib.literals.show_int32
  call $hydra.serialization.cst
)
  (func $hydra.serialization.op (param $s i32) (param $p i32) (param $assoc i32) (result i32)
  local.get $s
  call $hydra.serialization.sym
  i32.const 0
  i32.const 0
  local.get $p
  local.get $assoc
)
  (func $hydra.serialization.or_op (param $newlines i32) (result i32)
  i32.const 0 ;; string: "|"
  call $hydra.serialization.sym
  i32.const 0
  local.get $newlines
  i32.const 0
  i32.const 0
  call $hydra.lib.logic.if_else
  i32.const 0
  i32.const 0
)
  (func $hydra.serialization.or_sep (param $style i32) (param $l i32) (result i32)
  (local $acc i32)
  (local $el i32)
  (local $h i32)
  (local $newlines i32)
  local.get $style
  ;; project field: newline_before_content
  local.set $newlines
  i32.const 0 ;; string: ""
  call $hydra.serialization.cst
  local.get $newlines
  call $hydra.serialization.or_op
  local.get $acc
  local.get $el
  call $hydra.serialization.ifx
  local.get $h
  i32.const 1
  local.get $l
  call $hydra.lib.lists.drop
  call $hydra.lib.lists.foldl
  local.get $l
  call $hydra.lib.lists.safe_head
  call $hydra.lib.maybes.maybe
)
  (func $hydra.serialization.paren_list (param $newlines i32) (param $els i32) (result i32)
  (local $style i32)
  local.get $newlines
  local.get $els
  call $hydra.lib.lists.length
  i32.const 1
  call $hydra.lib.equality.gt
  call $hydra.lib.logic.and
  call $hydra.serialization.half_block_style
  call $hydra.serialization.inline_style
  call $hydra.lib.logic.if_else
  local.set $style
  local.get $els
  call $hydra.lib.lists.null
  i32.const 0 ;; string: "()"
  call $hydra.serialization.cst
  call $hydra.serialization.parentheses
  local.get $style
  local.get $style
  local.get $els
  call $hydra.serialization.comma_sep
  call $hydra.serialization.brackets
  call $hydra.lib.logic.if_else
)
  (func $hydra.serialization.parens (result i32)
  call $hydra.serialization.parentheses
  call $hydra.serialization.inline_style
  call $hydra.serialization.brackets
)
  (func $hydra.serialization.parentheses (result i32)
  i32.const 0 ;; string: "("
  i32.const 0 ;; string: ")"
)
  (func $hydra.serialization.parenthesize (param $exp i32) (result i32)
  (local $a i32)
  (local $assoc i32)
  (local $assoc_left i32)
  (local $assoc_right i32)
  (local $bracket_expr i32)
  (local $comparison i32)
  (local $indent_expr i32)
  (local $lassoc i32)
  (local $lhs i32)
  (local $lhs' i32)
  (local $lhs2 i32)
  (local $lop i32)
  (local $lop_expr i32)
  (local $lprec i32)
  (local $op i32)
  (local $op_expr i32)
  (local $prec i32)
  (local $rassoc i32)
  (local $rhs i32)
  (local $rhs' i32)
  (local $rhs2 i32)
  (local $rop i32)
  (local $rop_expr i32)
  (local $rprec i32)
  (local $seq_expr i32)
  (block $end_associativity (result i32)
  (block $right
  local.get $a
  br_table $right $right
)
  i32.const 0
  br $end_associativity
)
  local.set $assoc_left
  (block $end_associativity (result i32)
  (block $left
  local.get $a
  br_table $left $left
)
  i32.const 0
  br $end_associativity
)
  local.set $assoc_right
  (block $end_expr (result i32)
  (block $op
  (block $seq
  (block $indent
  (block $const
  (block $brackets
  local.get $exp
  br_table $brackets $const $indent $seq $op $op
)
  local.get $bracket_expr
  ;; project field: brackets
  local.get $bracket_expr
  ;; project field: enclosed
  call $hydra.serialization.parenthesize
  local.get $bracket_expr
  ;; project field: style
  br $end_expr
)
  local.get $exp
  br $end_expr
)
  local.get $indent_expr
  ;; project field: style
  local.get $indent_expr
  ;; project field: expr
  call $hydra.serialization.parenthesize
  br $end_expr
)
  local.get $seq_expr
  ;; project field: op
  call $hydra.serialization.parenthesize
  local.get $seq_expr
  ;; project field: elements
  call $hydra.lib.lists.map
  br $end_expr
)
  local.get $op_expr
  ;; project field: op
  local.set $op
  nop
  local.set $prec
  local.get $op
  ;; project field: associativity
  local.set $assoc
  local.get $op_expr
  ;; project field: lhs
  local.set $lhs
  local.get $op_expr
  ;; project field: rhs
  local.set $rhs
  local.get $lhs
  call $hydra.serialization.parenthesize
  local.set $lhs'
  local.get $rhs
  call $hydra.serialization.parenthesize
  local.set $rhs'
  (block $end_expr (result i32)
  (block $op
  local.get $lhs'
  br_table $op $op
)
  local.get $lop_expr
  ;; project field: op
  local.set $lop
  nop
  local.set $lprec
  local.get $lop
  ;; project field: associativity
  local.set $lassoc
  local.get $prec
  local.get $lprec
  call $hydra.lib.equality.compare
  local.set $comparison
  (block $end_comparison (result i32)
  (block $equal_to
  (block $greater_than
  (block $less_than
  local.get $comparison
  br_table $less_than $greater_than $equal_to $equal_to
)
  local.get $lhs'
  br $end_comparison
)
  local.get $lhs'
  call $hydra.serialization.parens
  br $end_comparison
)
  local.get $assoc
  local.get $assoc_left
  local.get $lassoc
  local.get $assoc_left
  call $hydra.lib.logic.and
  local.get $lhs'
  local.get $lhs'
  call $hydra.serialization.parens
  call $hydra.lib.logic.if_else
  br $end_comparison
)
  br $end_expr
)
  local.set $lhs2
  (block $end_expr (result i32)
  (block $op
  local.get $rhs'
  br_table $op $op
)
  local.get $rop_expr
  ;; project field: op
  local.set $rop
  nop
  local.set $rprec
  local.get $rop
  ;; project field: associativity
  local.set $rassoc
  local.get $prec
  local.get $rprec
  call $hydra.lib.equality.compare
  local.set $comparison
  (block $end_comparison (result i32)
  (block $equal_to
  (block $greater_than
  (block $less_than
  local.get $comparison
  br_table $less_than $greater_than $equal_to $equal_to
)
  local.get $rhs'
  br $end_comparison
)
  local.get $rhs'
  call $hydra.serialization.parens
  br $end_comparison
)
  local.get $assoc
  local.get $assoc_right
  local.get $rassoc
  local.get $assoc_right
  call $hydra.lib.logic.and
  local.get $rhs'
  local.get $rhs'
  call $hydra.serialization.parens
  call $hydra.lib.logic.if_else
  br $end_comparison
)
  br $end_expr
)
  local.set $rhs2
  local.get $op
  local.get $lhs2
  local.get $rhs2
  br $end_expr
)
)
  (func $hydra.serialization.prefix (param $p i32) (param $expr i32) (result i32)
  (local $pre_op i32)
  local.get $p
  call $hydra.serialization.sym
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  local.set $pre_op
  local.get $pre_op
  i32.const 0 ;; string: ""
  call $hydra.serialization.cst
  local.get $expr
  call $hydra.serialization.ifx
)
  (func $hydra.serialization.print_expr (param $e i32) (result i32)
  (local $body i32)
  (local $bracket_expr i32)
  (local $brs i32)
  (local $do_indent i32)
  (local $el i32)
  (local $expr i32)
  (local $ibody i32)
  (local $idt i32)
  (local $idt2 i32)
  (local $ilns i32)
  (local $indent_expr i32)
  (local $indent_str i32)
  (local $l i32)
  (local $lhs i32)
  (local $line i32)
  (local $lns i32)
  (local $nl_after i32)
  (local $nl_before i32)
  (local $op i32)
  (local $op_expr i32)
  (local $pad i32)
  (local $padding i32)
  (local $padl i32)
  (local $padr i32)
  (local $pre i32)
  (local $printed_elements i32)
  (local $r i32)
  (local $rhs i32)
  (local $s i32)
  (local $selements i32)
  (local $separator i32)
  (local $seq_expr i32)
  (local $sop i32)
  (local $spadding i32)
  (local $spadl i32)
  (local $spadr i32)
  (local $ssym i32)
  (local $style i32)
  (local $suf i32)
  (local $sym i32)
  (local $ws i32)
  (block $end_ws (result i32)
  (block $double_break
  (block $break_and_indent
  (block $break
  (block $space
  (block $none
  local.get $ws
  br_table $none $space $break $break_and_indent $double_break $double_break
)
  i32.const 0 ;; string: ""
  br $end_ws
)
  i32.const 0 ;; string: " "
  br $end_ws
)
  i32.const 0 ;; string: "
"
  br $end_ws
)
  i32.const 0 ;; string: "
"
  br $end_ws
)
  i32.const 0 ;; string: "

"
  br $end_ws
)
  local.set $pad
  (block $end_ws (result i32)
  (block $break_and_indent
  local.get $ws
  br_table $break_and_indent $break_and_indent
)
  local.get $indent_str
  local.get $s
  call $hydra.serialization.custom_indent
  br $end_ws
)
  local.set $idt
  (block $end_expr (result i32)
  (block $brackets
  (block $op
  (block $seq
  (block $indent
  (block $const
  local.get $e
  br_table $const $indent $seq $op $brackets $brackets
)
  nop
  br $end_expr
)
  local.get $indent_expr
  ;; project field: style
  local.set $style
  local.get $indent_expr
  ;; project field: expr
  local.set $expr
  local.get $expr
  call $hydra.serialization.print_expr
  call $hydra.lib.strings.lines
  local.set $lns
  (block $end_indent_style (result i32)
  (block $subsequent_lines
  (block $all_lines
  local.get $style
  br_table $all_lines $subsequent_lines $subsequent_lines
)
  local.get $idt2
  local.get $line
  call $hydra.lib.strings.cat2
  local.get $lns
  call $hydra.lib.lists.map
  br $end_indent_style
)
  local.get $lns
  call $hydra.lib.lists.length
  i32.const 1
  call $hydra.lib.equality.equal
  local.get $lns
  local.get $lns
  call $hydra.lib.lists.head
  local.get $idt2
  local.get $line
  call $hydra.lib.strings.cat2
  local.get $lns
  call $hydra.lib.lists.tail
  call $hydra.lib.lists.map
  call $hydra.lib.lists.cons
  call $hydra.lib.logic.if_else
  br $end_indent_style
)
  local.set $ilns
  i32.const 0 ;; string: "
"
  local.get $ilns
  call $hydra.lib.strings.intercalate
  br $end_expr
)
  local.get $seq_expr
  ;; project field: op
  local.set $sop
  nop
  local.set $ssym
  local.get $sop
  ;; project field: padding
  local.set $spadding
  local.get $spadding
  ;; project field: left
  local.set $spadl
  local.get $spadding
  ;; project field: right
  local.set $spadr
  local.get $seq_expr
  ;; project field: elements
  local.set $selements
  local.get $spadl
  local.get $pad
  local.get $ssym
  call $hydra.lib.strings.cat2
  local.get $spadr
  local.get $pad
  call $hydra.lib.strings.cat2
  local.set $separator
  local.get $spadr
  local.get $el
  call $hydra.serialization.print_expr
  local.get $idt
  local.get $selements
  call $hydra.lib.lists.map
  local.set $printed_elements
  local.get $separator
  local.get $printed_elements
  call $hydra.lib.strings.intercalate
  br $end_expr
)
  local.get $op_expr
  ;; project field: op
  local.set $op
  nop
  local.set $sym
  local.get $op
  ;; project field: padding
  local.set $padding
  local.get $padding
  ;; project field: left
  local.set $padl
  local.get $padding
  ;; project field: right
  local.set $padr
  local.get $op_expr
  ;; project field: lhs
  local.set $l
  local.get $op_expr
  ;; project field: rhs
  local.set $r
  local.get $padl
  local.get $l
  call $hydra.serialization.print_expr
  local.get $idt
  local.set $lhs
  local.get $padr
  local.get $r
  call $hydra.serialization.print_expr
  local.get $idt
  local.set $rhs
  local.get $lhs
  local.get $padl
  local.get $pad
  call $hydra.lib.strings.cat2
  local.get $sym
  call $hydra.lib.strings.cat2
  local.get $padr
  local.get $pad
  call $hydra.lib.strings.cat2
  local.get $rhs
  call $hydra.lib.strings.cat2
  br $end_expr
)
  local.get $bracket_expr
  ;; project field: brackets
  local.set $brs
  nop
  local.set $l
  nop
  local.set $r
  local.get $bracket_expr
  ;; project field: enclosed
  local.set $e
  local.get $bracket_expr
  ;; project field: style
  local.set $style
  local.get $e
  call $hydra.serialization.print_expr
  local.set $body
  local.get $style
  ;; project field: indent
  local.set $do_indent
  local.get $style
  ;; project field: newline_before_content
  local.set $nl_before
  local.get $style
  ;; project field: newline_after_content
  local.set $nl_after
  local.get $body
  local.get $idt2
  local.get $body
  call $hydra.serialization.custom_indent
  local.get $do_indent
  call $hydra.lib.maybes.maybe
  local.set $ibody
  local.get $nl_before
  i32.const 0 ;; string: "
"
  i32.const 0 ;; string: ""
  call $hydra.lib.logic.if_else
  local.set $pre
  local.get $nl_after
  i32.const 0 ;; string: "
"
  i32.const 0 ;; string: ""
  call $hydra.lib.logic.if_else
  local.set $suf
  local.get $l
  local.get $pre
  call $hydra.lib.strings.cat2
  local.get $ibody
  call $hydra.lib.strings.cat2
  local.get $suf
  call $hydra.lib.strings.cat2
  local.get $r
  call $hydra.lib.strings.cat2
  br $end_expr
)
)
  (func $hydra.serialization.semicolon_sep (result i32)
  i32.const 0 ;; string: ";"
  call $hydra.serialization.inline_style
  call $hydra.serialization.symbol_sep
)
  (func $hydra.serialization.sep (param $op i32) (param $els i32) (result i32)
  (local $acc i32)
  (local $el i32)
  (local $h i32)
  i32.const 0 ;; string: ""
  call $hydra.serialization.cst
  local.get $op
  local.get $acc
  local.get $el
  call $hydra.serialization.ifx
  local.get $h
  i32.const 1
  local.get $els
  call $hydra.lib.lists.drop
  call $hydra.lib.lists.foldl
  local.get $els
  call $hydra.lib.lists.safe_head
  call $hydra.lib.maybes.maybe
)
  (func $hydra.serialization.space_sep (result i32)
  i32.const 0 ;; string: ""
  call $hydra.serialization.sym
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  call $hydra.serialization.sep
)
  (func $hydra.serialization.square_brackets (result i32)
  i32.const 0 ;; string: "["
  i32.const 0 ;; string: "]"
)
  (func $hydra.serialization.structural_sep (param $op i32) (param $els i32) (result i32)
  local.get $els
  call $hydra.lib.lists.null
  i32.const 0 ;; string: ""
  call $hydra.serialization.cst
  local.get $els
  call $hydra.lib.lists.length
  i32.const 1
  call $hydra.lib.equality.equal
  local.get $els
  call $hydra.lib.lists.head
  local.get $op
  local.get $els
  call $hydra.lib.logic.if_else
  call $hydra.lib.logic.if_else
)
  (func $hydra.serialization.structural_space_sep (result i32)
  i32.const 0 ;; string: ""
  call $hydra.serialization.sym
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  call $hydra.serialization.structural_sep
)
  (func $hydra.serialization.suffix (param $s i32) (param $expr i32) (result i32)
  (local $suf_op i32)
  local.get $s
  call $hydra.serialization.sym
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  local.set $suf_op
  local.get $suf_op
  local.get $expr
  i32.const 0 ;; string: ""
  call $hydra.serialization.cst
  call $hydra.serialization.ifx
)
  (func $hydra.serialization.sym (param $s i32) (result i32)
  local.get $s
)
  (func $hydra.serialization.symbol_sep (param $symb i32) (param $style i32) (param $l i32) (result i32)
  (local $acc i32)
  (local $break i32)
  (local $break_count i32)
  (local $comma_op i32)
  (local $el i32)
  (local $h i32)
  (local $x_ i32)
  local.get $x_
  i32.const 2
  ;; list elements follow
  local.get $style
  ;; project field: newline_before_content
  local.get $style
  ;; project field: newline_after_content
  call $hydra.lib.lists.filter
  call $hydra.lib.lists.length
  local.set $break_count
  local.get $break_count
  i32.const 0
  call $hydra.lib.equality.equal
  i32.const 0
  local.get $break_count
  i32.const 1
  call $hydra.lib.equality.equal
  i32.const 0
  i32.const 0
  call $hydra.lib.logic.if_else
  call $hydra.lib.logic.if_else
  local.set $break
  local.get $symb
  call $hydra.serialization.sym
  i32.const 0
  local.get $break
  i32.const 0
  i32.const 0
  local.set $comma_op
  i32.const 0 ;; string: ""
  call $hydra.serialization.cst
  local.get $comma_op
  local.get $acc
  local.get $el
  call $hydra.serialization.ifx
  local.get $h
  i32.const 1
  local.get $l
  call $hydra.lib.lists.drop
  call $hydra.lib.lists.foldl
  local.get $l
  call $hydra.lib.lists.safe_head
  call $hydra.lib.maybes.maybe
)
  (func $hydra.serialization.tab_indent (param $e i32) (result i32)
  i32.const 0 ;; string: "    "
  local.get $e
)
  (func $hydra.serialization.tab_indent_double_space (param $exprs i32) (result i32)
  local.get $exprs
  call $hydra.serialization.double_newline_sep
  call $hydra.serialization.tab_indent
)
  (func $hydra.serialization.tab_indent_single_space (param $exprs i32) (result i32)
  local.get $exprs
  call $hydra.serialization.newline_sep
  call $hydra.serialization.tab_indent
)
  (func $hydra.serialization.unsupported_type (param $label i32) (result i32)
  i32.const 0 ;; string: "["
  local.get $label
  call $hydra.lib.strings.cat2
  i32.const 0 ;; string: "]"
  call $hydra.lib.strings.cat2
  call $hydra.serialization.cst
)
  (func $hydra.serialization.unsupported_variant (param $label i32) (param $obj i32) (result i32)
  i32.const 0 ;; string: "[unsupported "
  local.get $label
  call $hydra.lib.strings.cat2
  i32.const 0 ;; string: ": "
  call $hydra.lib.strings.cat2
  local.get $obj
  call $hydra.lib.literals.show_string
  call $hydra.lib.strings.cat2
  i32.const 0 ;; string: "]"
  call $hydra.lib.strings.cat2
  call $hydra.serialization.cst
)
  (func $hydra.serialization.with_comma (param $e i32) (result i32)
  i32.const 2
  ;; list elements follow
  local.get $e
  i32.const 0 ;; string: ","
  call $hydra.serialization.cst
  call $hydra.serialization.no_sep
)
  (func $hydra.serialization.with_semi (param $e i32) (result i32)
  i32.const 2
  ;; list elements follow
  local.get $e
  i32.const 0 ;; string: ";"
  call $hydra.serialization.cst
  call $hydra.serialization.no_sep
)
)
