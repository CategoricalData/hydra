(module
  (import "hydra.constants" "hydra.constants.warning_auto_generated_file" (func $hydra.constants.warning_auto_generated_file (param i32) (result i32) ) )
  (import "hydra.ext.haskell.operators" "hydra.ext.haskell.operators.app_op" (func $hydra.ext.haskell.operators.app_op (param i32) (result i32) ) )
  (import "hydra.ext.haskell.operators" "hydra.ext.haskell.operators.arrow_op" (func $hydra.ext.haskell.operators.arrow_op (param i32) (result i32) ) )
  (import "hydra.ext.haskell.operators" "hydra.ext.haskell.operators.assert_op" (func $hydra.ext.haskell.operators.assert_op (param i32) (result i32) ) )
  (import "hydra.ext.haskell.operators" "hydra.ext.haskell.operators.define_op" (func $hydra.ext.haskell.operators.define_op (param i32) (result i32) ) )
  (import "hydra.ext.haskell.operators" "hydra.ext.haskell.operators.lambda_op" (func $hydra.ext.haskell.operators.lambda_op (param i32) (result i32) ) )
  (import "hydra.lib.equality" "hydra.lib.equality.gt" (func $hydra.lib.equality.gt (param i32) (result i32) ) )
  (import "hydra.lib.equality" "hydra.lib.equality.lt" (func $hydra.lib.equality.lt (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.concat" (func $hydra.lib.lists.concat (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.concat2" (func $hydra.lib.lists.concat2 (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.cons" (func $hydra.lib.lists.cons (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.null" (func $hydra.lib.lists.null (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_bigint" (func $hydra.lib.literals.show_bigint (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_float32" (func $hydra.lib.literals.show_float32 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_float64" (func $hydra.lib.literals.show_float64 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_int32" (func $hydra.lib.literals.show_int32 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_string" (func $hydra.lib.literals.show_string (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_uint16" (func $hydra.lib.literals.show_uint16 (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.if_else" (func $hydra.lib.logic.if_else (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.cat" (func $hydra.lib.maybes.cat (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.map" (func $hydra.lib.maybes.map (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.maybe" (func $hydra.lib.maybes.maybe (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat" (func $hydra.lib.strings.cat (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat2" (func $hydra.lib.strings.cat2 (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.intercalate" (func $hydra.lib.strings.intercalate (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.lines" (func $hydra.lib.strings.lines (param i32) (result i32) ) )
  (import "hydra.serialization" "hydra.serialization.bracket_list" (func $hydra.serialization.bracket_list (param i32) (result i32) ) )
  (import "hydra.serialization" "hydra.serialization.brackets" (func $hydra.serialization.brackets (param i32) (result i32) ) )
  (import "hydra.serialization" "hydra.serialization.comma_sep" (func $hydra.serialization.comma_sep (param i32) (result i32) ) )
  (import "hydra.serialization" "hydra.serialization.cst" (func $hydra.serialization.cst (param i32) (result i32) ) )
  (import "hydra.serialization" "hydra.serialization.curly_braces" (func $hydra.serialization.curly_braces (param i32) (result i32) ) )
  (import "hydra.serialization" "hydra.serialization.curly_braces_list" (func $hydra.serialization.curly_braces_list (param i32) (result i32) ) )
  (import "hydra.serialization" "hydra.serialization.custom_indent_block" (func $hydra.serialization.custom_indent_block (param i32) (result i32) ) )
  (import "hydra.serialization" "hydra.serialization.double_newline_sep" (func $hydra.serialization.double_newline_sep (param i32) (result i32) ) )
  (import "hydra.serialization" "hydra.serialization.expression_length" (func $hydra.serialization.expression_length (param i32) (result i32) ) )
  (import "hydra.serialization" "hydra.serialization.half_block_style" (func $hydra.serialization.half_block_style (param i32) (result i32) ) )
  (import "hydra.serialization" "hydra.serialization.ifx" (func $hydra.serialization.ifx (param i32) (result i32) ) )
  (import "hydra.serialization" "hydra.serialization.indent_block" (func $hydra.serialization.indent_block (param i32) (result i32) ) )
  (import "hydra.serialization" "hydra.serialization.indent_subsequent_lines" (func $hydra.serialization.indent_subsequent_lines (param i32) (result i32) ) )
  (import "hydra.serialization" "hydra.serialization.inline_style" (func $hydra.serialization.inline_style (param i32) (result i32) ) )
  (import "hydra.serialization" "hydra.serialization.newline_sep" (func $hydra.serialization.newline_sep (param i32) (result i32) ) )
  (import "hydra.serialization" "hydra.serialization.or_sep" (func $hydra.serialization.or_sep (param i32) (result i32) ) )
  (import "hydra.serialization" "hydra.serialization.paren_list" (func $hydra.serialization.paren_list (param i32) (result i32) ) )
  (import "hydra.serialization" "hydra.serialization.parens" (func $hydra.serialization.parens (param i32) (result i32) ) )
  (import "hydra.serialization" "hydra.serialization.parenthesize" (func $hydra.serialization.parenthesize (param i32) (result i32) ) )
  (import "hydra.serialization" "hydra.serialization.prefix" (func $hydra.serialization.prefix (param i32) (result i32) ) )
  (import "hydra.serialization" "hydra.serialization.space_sep" (func $hydra.serialization.space_sep (param i32) (result i32) ) )
  (import "hydra.serialization" "hydra.serialization.structural_space_sep" (func $hydra.serialization.structural_space_sep (param i32) (result i32) ) )
  (import "hydra.serialization" "hydra.serialization.tab_indent" (func $hydra.serialization.tab_indent (param i32) (result i32) ) )
  (memory $memory 1 )
  (export "memory" (memory $memory) )
  (export "hydra.ext.haskell.serde.alternative_to_expr" (func $hydra.ext.haskell.serde.alternative_to_expr) )
  (export "hydra.ext.haskell.serde.application_expression_to_expr" (func $hydra.ext.haskell.serde.application_expression_to_expr) )
  (export "hydra.ext.haskell.serde.application_pattern_to_expr" (func $hydra.ext.haskell.serde.application_pattern_to_expr) )
  (export "hydra.ext.haskell.serde.assertion_to_expr" (func $hydra.ext.haskell.serde.assertion_to_expr) )
  (export "hydra.ext.haskell.serde.case_expression_to_expr" (func $hydra.ext.haskell.serde.case_expression_to_expr) )
  (export "hydra.ext.haskell.serde.case_rhs_to_expr" (func $hydra.ext.haskell.serde.case_rhs_to_expr) )
  (export "hydra.ext.haskell.serde.class_assertion_to_expr" (func $hydra.ext.haskell.serde.class_assertion_to_expr) )
  (export "hydra.ext.haskell.serde.construct_record_expression_to_expr" (func $hydra.ext.haskell.serde.construct_record_expression_to_expr) )
  (export "hydra.ext.haskell.serde.constructor_to_expr" (func $hydra.ext.haskell.serde.constructor_to_expr) )
  (export "hydra.ext.haskell.serde.constructor_with_comments_to_expr" (func $hydra.ext.haskell.serde.constructor_with_comments_to_expr) )
  (export "hydra.ext.haskell.serde.data_or_newtype_to_expr" (func $hydra.ext.haskell.serde.data_or_newtype_to_expr) )
  (export "hydra.ext.haskell.serde.declaration_head_to_expr" (func $hydra.ext.haskell.serde.declaration_head_to_expr) )
  (export "hydra.ext.haskell.serde.declaration_to_expr" (func $hydra.ext.haskell.serde.declaration_to_expr) )
  (export "hydra.ext.haskell.serde.declaration_with_comments_to_expr" (func $hydra.ext.haskell.serde.declaration_with_comments_to_expr) )
  (export "hydra.ext.haskell.serde.expression_to_expr" (func $hydra.ext.haskell.serde.expression_to_expr) )
  (export "hydra.ext.haskell.serde.field_to_expr" (func $hydra.ext.haskell.serde.field_to_expr) )
  (export "hydra.ext.haskell.serde.field_with_comments_to_expr" (func $hydra.ext.haskell.serde.field_with_comments_to_expr) )
  (export "hydra.ext.haskell.serde.if_expression_to_expr" (func $hydra.ext.haskell.serde.if_expression_to_expr) )
  (export "hydra.ext.haskell.serde.import_export_spec_to_expr" (func $hydra.ext.haskell.serde.import_export_spec_to_expr) )
  (export "hydra.ext.haskell.serde.import_to_expr" (func $hydra.ext.haskell.serde.import_to_expr) )
  (export "hydra.ext.haskell.serde.lambda_expression_to_expr" (func $hydra.ext.haskell.serde.lambda_expression_to_expr) )
  (export "hydra.ext.haskell.serde.literal_to_expr" (func $hydra.ext.haskell.serde.literal_to_expr) )
  (export "hydra.ext.haskell.serde.local_binding_to_expr" (func $hydra.ext.haskell.serde.local_binding_to_expr) )
  (export "hydra.ext.haskell.serde.module_head_to_expr" (func $hydra.ext.haskell.serde.module_head_to_expr) )
  (export "hydra.ext.haskell.serde.module_to_expr" (func $hydra.ext.haskell.serde.module_to_expr) )
  (export "hydra.ext.haskell.serde.name_to_expr" (func $hydra.ext.haskell.serde.name_to_expr) )
  (export "hydra.ext.haskell.serde.pattern_to_expr" (func $hydra.ext.haskell.serde.pattern_to_expr) )
  (export "hydra.ext.haskell.serde.right_hand_side_to_expr" (func $hydra.ext.haskell.serde.right_hand_side_to_expr) )
  (export "hydra.ext.haskell.serde.statement_to_expr" (func $hydra.ext.haskell.serde.statement_to_expr) )
  (export "hydra.ext.haskell.serde.to_haskell_comments" (func $hydra.ext.haskell.serde.to_haskell_comments) )
  (export "hydra.ext.haskell.serde.to_simple_comments" (func $hydra.ext.haskell.serde.to_simple_comments) )
  (export "hydra.ext.haskell.serde.type_signature_to_expr" (func $hydra.ext.haskell.serde.type_signature_to_expr) )
  (export "hydra.ext.haskell.serde.type_to_expr" (func $hydra.ext.haskell.serde.type_to_expr) )
  (export "hydra.ext.haskell.serde.value_binding_to_expr" (func $hydra.ext.haskell.serde.value_binding_to_expr) )
  (export "hydra.ext.haskell.serde.variable_to_expr" (func $hydra.ext.haskell.serde.variable_to_expr) )
  (export "hydra.ext.haskell.serde.write_qualified_name" (func $hydra.ext.haskell.serde.write_qualified_name) )
  (func $hydra.ext.haskell.serde.alternative_to_expr (param $alt i32) (result i32)
  i32.const 3
  ;; list elements follow
  ;; project field: pattern
  call $hydra.ext.haskell.serde.pattern_to_expr
  i32.const 0 ;; string: "->"
  call $hydra.serialization.cst
  ;; project field: rhs
  call $hydra.ext.haskell.serde.case_rhs_to_expr
  call $hydra.serialization.structural_space_sep
)
  (func $hydra.ext.haskell.serde.application_expression_to_expr (param $app i32) (result i32)
  call $hydra.ext.haskell.operators.app_op
  ;; project field: function
  call $hydra.ext.haskell.serde.expression_to_expr
  ;; project field: argument
  call $hydra.ext.haskell.serde.expression_to_expr
  call $hydra.serialization.ifx
)
  (func $hydra.ext.haskell.serde.application_pattern_to_expr (param $app_pat i32) (result i32)
  (local $name i32)
  (local $pats i32)
  ;; project field: name
  local.set $name
  ;; project field: args
  local.set $pats
  local.get $name
  call $hydra.ext.haskell.serde.name_to_expr
  call $hydra.ext.haskell.serde.pattern_to_expr
  local.get $pats
  call $hydra.lib.lists.map
  call $hydra.lib.lists.cons
  call $hydra.serialization.space_sep
)
  (func $hydra.ext.haskell.serde.assertion_to_expr (param $sert i32) (result i32)
  (local $cls i32)
  (local $serts i32)
  (block $end_assertion (result i32)
  (block $tuple
  (block $class
  local.get $sert
  br_table $class $tuple $end_assertion
  local.get $cls
  call $hydra.ext.haskell.serde.class_assertion_to_expr
  br $end_assertion
)
  i32.const 0
  call $hydra.ext.haskell.serde.assertion_to_expr
  local.get $serts
  call $hydra.lib.lists.map
  call $hydra.serialization.paren_list
  br $end_assertion
)
)
)
  (func $hydra.ext.haskell.serde.case_expression_to_expr (param $case_expr i32) (result i32)
  (local $alts i32)
  (local $cs i32)
  (local $lhs i32)
  (local $of_op i32)
  (local $rhs i32)
  ;; project field: case
  local.set $cs
  ;; project field: alternatives
  local.set $alts
  i32.const 0 ;; string: "of"
  i32.const 0
  i32.const 0 ;; string: "  "
  i32.const 0
  i32.const 0
  local.set $of_op
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "case"
  call $hydra.serialization.cst
  local.get $cs
  call $hydra.ext.haskell.serde.expression_to_expr
  call $hydra.serialization.space_sep
  local.set $lhs
  call $hydra.ext.haskell.serde.alternative_to_expr
  local.get $alts
  call $hydra.lib.lists.map
  call $hydra.serialization.newline_sep
  local.set $rhs
  local.get $of_op
  local.get $lhs
  local.get $rhs
  call $hydra.serialization.ifx
)
  (func $hydra.ext.haskell.serde.case_rhs_to_expr (param $rhs i32) (result i32)
  nop
  call $hydra.ext.haskell.serde.expression_to_expr
)
  (func $hydra.ext.haskell.serde.class_assertion_to_expr (param $cls_asrt i32) (result i32)
  (local $name i32)
  (local $types i32)
  ;; project field: name
  local.set $name
  ;; project field: types
  local.set $types
  local.get $name
  call $hydra.ext.haskell.serde.name_to_expr
  i32.const 1
  ;; list elements follow
  call $hydra.serialization.half_block_style
  call $hydra.ext.haskell.serde.type_to_expr
  local.get $types
  call $hydra.lib.lists.map
  call $hydra.serialization.comma_sep
  call $hydra.lib.lists.cons
  call $hydra.serialization.space_sep
)
  (func $hydra.ext.haskell.serde.construct_record_expression_to_expr (param $construct_record i32) (result i32)
  (local $body i32)
  (local $fn i32)
  (local $from_update i32)
  (local $name i32)
  (local $updates i32)
  (local $val i32)
  ;; project field: name
  local.set $name
  ;; project field: fields
  local.set $updates
  ;; project field: name
  local.set $fn
  ;; project field: value
  local.set $val
  call $hydra.ext.haskell.operators.define_op
  local.get $fn
  call $hydra.ext.haskell.serde.name_to_expr
  local.get $val
  call $hydra.ext.haskell.serde.expression_to_expr
  call $hydra.serialization.ifx
  local.set $from_update
  call $hydra.serialization.half_block_style
  local.get $from_update
  local.get $updates
  call $hydra.lib.lists.map
  call $hydra.serialization.comma_sep
  local.set $body
  local.get $name
  call $hydra.ext.haskell.serde.name_to_expr
  i32.const 1
  ;; list elements follow
  call $hydra.serialization.curly_braces
  call $hydra.serialization.half_block_style
  local.get $body
  call $hydra.serialization.brackets
  call $hydra.lib.lists.cons
  call $hydra.serialization.space_sep
)
  (func $hydra.ext.haskell.serde.constructor_to_expr (param $cons i32) (result i32)
  (local $fields i32)
  (local $name i32)
  (local $types i32)
  (block $end_constructor (result i32)
  (block $record
  (block $ordinary
  local.get $cons
  br_table $ordinary $record $end_constructor
  ;; project field: name
  local.set $name
  ;; project field: fields
  local.set $types
  local.get $name
  call $hydra.ext.haskell.serde.name_to_expr
  i32.const 1
  ;; list elements follow
  call $hydra.ext.haskell.serde.type_to_expr
  local.get $types
  call $hydra.lib.lists.map
  call $hydra.serialization.space_sep
  call $hydra.lib.lists.cons
  call $hydra.serialization.space_sep
  br $end_constructor
)
  ;; project field: name
  local.set $name
  ;; project field: fields
  local.set $fields
  local.get $name
  call $hydra.ext.haskell.serde.name_to_expr
  i32.const 1
  ;; list elements follow
  i32.const 0
  call $hydra.serialization.half_block_style
  call $hydra.ext.haskell.serde.field_with_comments_to_expr
  local.get $fields
  call $hydra.lib.lists.map
  call $hydra.serialization.curly_braces_list
  call $hydra.lib.lists.cons
  call $hydra.serialization.space_sep
  br $end_constructor
)
)
)
  (func $hydra.ext.haskell.serde.constructor_with_comments_to_expr (param $cons_with_comments i32) (result i32)
  (local $body i32)
  (local $c i32)
  (local $mc i32)
  ;; project field: body
  local.set $body
  ;; project field: comments
  local.set $mc
  local.get $body
  call $hydra.ext.haskell.serde.constructor_to_expr
  local.get $c
  call $hydra.ext.haskell.serde.to_haskell_comments
  call $hydra.serialization.cst
  i32.const 1
  ;; list elements follow
  local.get $body
  call $hydra.ext.haskell.serde.constructor_to_expr
  call $hydra.lib.lists.cons
  call $hydra.serialization.newline_sep
  local.get $mc
  call $hydra.lib.maybes.maybe
)
  (func $hydra.ext.haskell.serde.data_or_newtype_to_expr (param $kw i32) (result i32)
  (block $end_data_or_newtype (result i32)
  (block $newtype
  (block $data
  local.get $kw
  br_table $data $newtype $end_data_or_newtype
  i32.const 0 ;; string: "data"
  call $hydra.serialization.cst
  br $end_data_or_newtype
)
  i32.const 0 ;; string: "newtype"
  call $hydra.serialization.cst
  br $end_data_or_newtype
)
)
)
  (func $hydra.ext.haskell.serde.declaration_head_to_expr (param $hd i32) (result i32)
  (local $fun i32)
  (local $name i32)
  (local $op i32)
  (block $end_declaration_head (result i32)
  (block $simple
  (block $application
  local.get $hd
  br_table $application $simple $end_declaration_head
  ;; project field: function
  local.set $fun
  ;; project field: operand
  local.set $op
  local.get $fun
  call $hydra.ext.haskell.serde.declaration_head_to_expr
  i32.const 1
  ;; list elements follow
  local.get $op
  call $hydra.ext.haskell.serde.variable_to_expr
  call $hydra.lib.lists.cons
  call $hydra.serialization.space_sep
  br $end_declaration_head
)
  local.get $name
  call $hydra.ext.haskell.serde.name_to_expr
  br $end_declaration_head
)
)
)
  (func $hydra.ext.haskell.serde.declaration_to_expr (param $decl i32) (result i32)
  (local $cons i32)
  (local $constructors i32)
  (local $deriv i32)
  (local $deriv_cat i32)
  (local $deriving_clause i32)
  (local $hd i32)
  (local $htype i32)
  (local $kw i32)
  (local $main_parts i32)
  (local $name i32)
  (local $typ i32)
  (local $type_sig i32)
  (local $vb i32)
  (block $end_declaration (result i32)
  (block $typed_binding
  (block $value_binding
  (block $type
  (block $data
  local.get $decl
  br_table $data $type $value_binding $typed_binding $end_declaration
  ;; project field: keyword
  local.set $kw
  ;; project field: head
  local.set $hd
  ;; project field: constructors
  local.set $cons
  ;; project field: deriving
  local.set $deriv
  nop
  local.get $deriv
  call $hydra.lib.lists.map
  call $hydra.lib.lists.concat
  local.set $deriv_cat
  call $hydra.serialization.half_block_style
  call $hydra.ext.haskell.serde.constructor_with_comments_to_expr
  local.get $cons
  call $hydra.lib.lists.map
  call $hydra.serialization.or_sep
  local.set $constructors
  local.get $deriv_cat
  call $hydra.lib.lists.null
  i32.const 0
  ;; list elements follow
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "deriving"
  call $hydra.serialization.cst
  i32.const 1
  ;; list elements follow
  i32.const 0
  call $hydra.ext.haskell.serde.name_to_expr
  local.get $deriv_cat
  call $hydra.lib.lists.map
  call $hydra.serialization.paren_list
  call $hydra.lib.lists.cons
  call $hydra.serialization.space_sep
  call $hydra.lib.logic.if_else
  local.set $deriving_clause
  i32.const 2
  ;; list elements follow
  local.get $kw
  call $hydra.ext.haskell.serde.data_or_newtype_to_expr
  local.get $hd
  call $hydra.ext.haskell.serde.declaration_head_to_expr
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "="
  call $hydra.serialization.cst
  call $hydra.lib.lists.cons
  call $hydra.lib.lists.cons
  call $hydra.serialization.space_sep
  local.get $constructors
  local.set $main_parts
  local.get $main_parts
  local.get $deriving_clause
  call $hydra.lib.lists.concat2
  call $hydra.serialization.indent_block
  br $end_declaration
)
  ;; project field: name
  local.set $hd
  ;; project field: type
  local.set $typ
  i32.const 0 ;; string: "type"
  call $hydra.serialization.cst
  local.get $hd
  call $hydra.ext.haskell.serde.declaration_head_to_expr
  i32.const 0 ;; string: "="
  call $hydra.serialization.cst
  i32.const 1
  ;; list elements follow
  local.get $typ
  call $hydra.ext.haskell.serde.type_to_expr
  call $hydra.lib.lists.cons
  call $hydra.lib.lists.cons
  call $hydra.lib.lists.cons
  call $hydra.serialization.space_sep
  br $end_declaration
)
  local.get $vb
  call $hydra.ext.haskell.serde.value_binding_to_expr
  br $end_declaration
)
  ;; project field: type_signature
  local.set $type_sig
  ;; project field: value_binding
  local.set $vb
  ;; project field: name
  local.set $name
  ;; project field: type
  local.set $htype
  i32.const 3
  ;; list elements follow
  local.get $name
  call $hydra.ext.haskell.serde.name_to_expr
  i32.const 0 ;; string: "::"
  call $hydra.serialization.cst
  local.get $htype
  call $hydra.ext.haskell.serde.type_to_expr
  call $hydra.serialization.structural_space_sep
  i32.const 1
  ;; list elements follow
  local.get $vb
  call $hydra.ext.haskell.serde.value_binding_to_expr
  call $hydra.lib.lists.cons
  call $hydra.serialization.newline_sep
  br $end_declaration
)
)
)
  (func $hydra.ext.haskell.serde.declaration_with_comments_to_expr (param $decl_with_comments i32) (result i32)
  (local $body i32)
  (local $c i32)
  (local $mc i32)
  ;; project field: body
  local.set $body
  ;; project field: comments
  local.set $mc
  local.get $body
  call $hydra.ext.haskell.serde.declaration_to_expr
  local.get $c
  call $hydra.ext.haskell.serde.to_haskell_comments
  call $hydra.serialization.cst
  i32.const 1
  ;; list elements follow
  local.get $body
  call $hydra.ext.haskell.serde.declaration_to_expr
  call $hydra.lib.lists.cons
  call $hydra.serialization.newline_sep
  local.get $mc
  call $hydra.lib.maybes.maybe
)
  (func $hydra.ext.haskell.serde.expression_to_expr (param $expr i32) (result i32)
  (local $app i32)
  (local $binding i32)
  (local $bindings i32)
  (local $cases i32)
  (local $encode_binding i32)
  (local $expr' i32)
  (local $exprs i32)
  (local $ifte i32)
  (local $inner i32)
  (local $lam i32)
  (local $lit i32)
  (local $name i32)
  (local $r i32)
  (local $statements i32)
  (block $end_expression (result i32)
  (block $variable
  (block $tuple
  (block $parens
  (block $list
  (block $let
  (block $lambda
  (block $literal
  (block $if
  (block $do
  (block $construct_record
  (block $case
  (block $application
  local.get $expr
  br_table $application $case $construct_record $do $if $literal $lambda $let $list $parens $tuple $variable $end_expression
  local.get $app
  call $hydra.ext.haskell.serde.application_expression_to_expr
  br $end_expression
)
  local.get $cases
  call $hydra.ext.haskell.serde.case_expression_to_expr
  br $end_expression
)
  local.get $r
  call $hydra.ext.haskell.serde.construct_record_expression_to_expr
  br $end_expression
)
  i32.const 0 ;; string: "do"
  call $hydra.serialization.cst
  call $hydra.ext.haskell.serde.statement_to_expr
  local.get $statements
  call $hydra.lib.lists.map
  call $hydra.lib.lists.cons
  call $hydra.serialization.indent_block
  br $end_expression
)
  local.get $ifte
  call $hydra.ext.haskell.serde.if_expression_to_expr
  br $end_expression
)
  local.get $lit
  call $hydra.ext.haskell.serde.literal_to_expr
  br $end_expression
)
  local.get $lam
  call $hydra.ext.haskell.serde.lambda_expression_to_expr
  call $hydra.serialization.parenthesize
  br $end_expression
)
  ;; project field: bindings
  local.set $bindings
  ;; project field: inner
  local.set $inner
  i32.const 0 ;; string: "    "
  local.get $binding
  call $hydra.ext.haskell.serde.local_binding_to_expr
  call $hydra.serialization.indent_subsequent_lines
  local.set $encode_binding
  i32.const 0 ;; string: ""
  call $hydra.serialization.cst
  i32.const 0 ;; string: "let"
  call $hydra.serialization.cst
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "    "
  local.get $encode_binding
  local.get $bindings
  call $hydra.lib.lists.map
  call $hydra.serialization.custom_indent_block
  call $hydra.lib.lists.cons
  call $hydra.serialization.space_sep
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "in"
  call $hydra.serialization.cst
  i32.const 1
  ;; list elements follow
  local.get $inner
  call $hydra.ext.haskell.serde.expression_to_expr
  call $hydra.lib.lists.cons
  call $hydra.serialization.space_sep
  call $hydra.lib.lists.cons
  call $hydra.lib.lists.cons
  call $hydra.serialization.indent_block
  br $end_expression
)
  call $hydra.serialization.half_block_style
  call $hydra.ext.haskell.serde.expression_to_expr
  local.get $exprs
  call $hydra.lib.lists.map
  call $hydra.serialization.bracket_list
  br $end_expression
)
  local.get $expr'
  call $hydra.ext.haskell.serde.expression_to_expr
  call $hydra.serialization.parenthesize
  br $end_expression
)
  i32.const 0
  call $hydra.ext.haskell.serde.expression_to_expr
  local.get $exprs
  call $hydra.lib.lists.map
  call $hydra.serialization.paren_list
  br $end_expression
)
  local.get $name
  call $hydra.ext.haskell.serde.name_to_expr
  br $end_expression
)
)
)
  (func $hydra.ext.haskell.serde.field_to_expr (param $field i32) (result i32)
  (local $name i32)
  (local $typ i32)
  ;; project field: name
  local.set $name
  ;; project field: type
  local.set $typ
  local.get $name
  call $hydra.ext.haskell.serde.name_to_expr
  i32.const 0 ;; string: "::"
  call $hydra.serialization.cst
  i32.const 1
  ;; list elements follow
  local.get $typ
  call $hydra.ext.haskell.serde.type_to_expr
  call $hydra.lib.lists.cons
  call $hydra.lib.lists.cons
  call $hydra.serialization.space_sep
)
  (func $hydra.ext.haskell.serde.field_with_comments_to_expr (param $field_with_comments i32) (result i32)
  (local $c i32)
  (local $field i32)
  (local $mc i32)
  ;; project field: field
  local.set $field
  ;; project field: comments
  local.set $mc
  local.get $field
  call $hydra.ext.haskell.serde.field_to_expr
  local.get $c
  call $hydra.ext.haskell.serde.to_haskell_comments
  call $hydra.serialization.cst
  i32.const 1
  ;; list elements follow
  local.get $field
  call $hydra.ext.haskell.serde.field_to_expr
  call $hydra.lib.lists.cons
  call $hydra.serialization.newline_sep
  local.get $mc
  call $hydra.lib.maybes.maybe
)
  (func $hydra.ext.haskell.serde.if_expression_to_expr (param $if_expr i32) (result i32)
  (local $body i32)
  (local $eelse i32)
  (local $eif i32)
  (local $ethen i32)
  (local $if_op i32)
  ;; project field: condition
  local.set $eif
  ;; project field: then
  local.set $ethen
  ;; project field: else
  local.set $eelse
  i32.const 0 ;; string: ""
  i32.const 0
  i32.const 0 ;; string: "  "
  i32.const 0
  i32.const 0
  local.set $if_op
  i32.const 0 ;; string: "then"
  call $hydra.serialization.cst
  i32.const 1
  ;; list elements follow
  local.get $ethen
  call $hydra.ext.haskell.serde.expression_to_expr
  call $hydra.lib.lists.cons
  call $hydra.serialization.space_sep
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "else"
  call $hydra.serialization.cst
  i32.const 1
  ;; list elements follow
  local.get $eelse
  call $hydra.ext.haskell.serde.expression_to_expr
  call $hydra.lib.lists.cons
  call $hydra.serialization.space_sep
  call $hydra.lib.lists.cons
  call $hydra.serialization.newline_sep
  local.set $body
  local.get $if_op
  i32.const 0 ;; string: "if"
  call $hydra.serialization.cst
  i32.const 1
  ;; list elements follow
  local.get $eif
  call $hydra.ext.haskell.serde.expression_to_expr
  call $hydra.lib.lists.cons
  call $hydra.serialization.space_sep
  local.get $body
  call $hydra.serialization.ifx
)
  (func $hydra.ext.haskell.serde.import_export_spec_to_expr (param $spec i32) (result i32)
  ;; project field: name
  call $hydra.ext.haskell.serde.name_to_expr
)
  (func $hydra.ext.haskell.serde.import_to_expr (param $import i32) (result i32)
  (local $hiding_sec i32)
  (local $mod i32)
  (local $mod_name i32)
  (local $mspec i32)
  (local $name i32)
  (local $names i32)
  (local $parts i32)
  (local $qual i32)
  (local $spec i32)
  ;; project field: qualified
  local.set $qual
  ;; project field: module
  local.set $mod_name
  ;; project field: as
  local.set $mod
  ;; project field: spec
  local.set $mspec
  nop
  local.set $name
  (block $end_spec_import (result i32)
  (block $hiding
  local.get $spec
  br_table $hiding $end_spec_import
  i32.const 0 ;; string: "hiding "
  call $hydra.serialization.cst
  i32.const 1
  ;; list elements follow
  call $hydra.serialization.inline_style
  call $hydra.ext.haskell.serde.import_export_spec_to_expr
  local.get $names
  call $hydra.lib.lists.map
  call $hydra.serialization.comma_sep
  call $hydra.serialization.parens
  call $hydra.lib.lists.cons
  call $hydra.serialization.space_sep
  br $end_spec_import
)
)
  local.set $hiding_sec
  i32.const 5
  ;; list elements follow
  i32.const 0 ;; string: "import"
  call $hydra.serialization.cst
  local.get $qual
  i32.const 0 ;; string: "qualified"
  call $hydra.serialization.cst
  i32.const 0
  call $hydra.lib.logic.if_else
  local.get $name
  call $hydra.serialization.cst
  i32.const 0 ;; string: "as "
  nop
  call $hydra.lib.strings.cat2
  call $hydra.serialization.cst
  local.get $mod
  call $hydra.lib.maybes.map
  local.get $hiding_sec
  local.get $mspec
  call $hydra.lib.maybes.map
  call $hydra.lib.maybes.cat
  local.set $parts
  local.get $parts
  call $hydra.serialization.space_sep
)
  (func $hydra.ext.haskell.serde.lambda_expression_to_expr (param $lambda_expr i32) (result i32)
  (local $bindings i32)
  (local $body i32)
  (local $head i32)
  (local $inner i32)
  ;; project field: bindings
  local.set $bindings
  ;; project field: inner
  local.set $inner
  call $hydra.ext.haskell.serde.pattern_to_expr
  local.get $bindings
  call $hydra.lib.lists.map
  call $hydra.serialization.space_sep
  local.set $head
  local.get $inner
  call $hydra.ext.haskell.serde.expression_to_expr
  local.set $body
  call $hydra.ext.haskell.operators.lambda_op
  i32.const 0 ;; string: "\"
  local.get $head
  call $hydra.serialization.prefix
  local.get $body
  call $hydra.serialization.ifx
)
  (func $hydra.ext.haskell.serde.literal_to_expr (param $lit i32) (result i32)
  (local $b i32)
  (local $c i32)
  (local $d i32)
  (local $e i32)
  (local $f i32)
  (local $i i32)
  (local $parens_if_neg i32)
  (local $s i32)
  local.get $b
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "("
  local.get $e
  i32.const 0 ;; string: ")"
  call $hydra.lib.strings.cat
  local.get $e
  call $hydra.lib.logic.if_else
  local.set $parens_if_neg
  (block $end_literal (result i32)
  (block $string
  (block $integer
  (block $int
  (block $float
  (block $double
  (block $char
  local.get $lit
  br_table $char $double $float $int $integer $string $end_literal
  local.get $c
  call $hydra.lib.literals.show_uint16
  call $hydra.lib.literals.show_string
  br $end_literal
)
  local.get $d
  f64.const 0.0
  call $hydra.lib.equality.lt
  local.get $d
  call $hydra.lib.literals.show_float64
  local.get $parens_if_neg
  br $end_literal
)
  local.get $f
  f32.const 0.0
  call $hydra.lib.equality.lt
  local.get $f
  call $hydra.lib.literals.show_float32
  local.get $parens_if_neg
  br $end_literal
)
  local.get $i
  i32.const 0
  call $hydra.lib.equality.lt
  local.get $i
  call $hydra.lib.literals.show_int32
  local.get $parens_if_neg
  br $end_literal
)
  local.get $i
  i32.const 0 ;; string: "0:bigint"
  call $hydra.lib.equality.lt
  local.get $i
  call $hydra.lib.literals.show_bigint
  local.get $parens_if_neg
  br $end_literal
)
  local.get $s
  call $hydra.lib.literals.show_string
  br $end_literal
)
)
  call $hydra.serialization.cst
)
  (func $hydra.ext.haskell.serde.local_binding_to_expr (param $binding i32) (result i32)
  (local $ts i32)
  (local $vb i32)
  (block $end_local_binding (result i32)
  (block $value
  (block $signature
  local.get $binding
  br_table $signature $value $end_local_binding
  local.get $ts
  call $hydra.ext.haskell.serde.type_signature_to_expr
  br $end_local_binding
)
  local.get $vb
  call $hydra.ext.haskell.serde.value_binding_to_expr
  br $end_local_binding
)
)
)
  (func $hydra.ext.haskell.serde.module_head_to_expr (param $module_head i32) (result i32)
  (local $c i32)
  (local $head i32)
  (local $mc i32)
  (local $mname i32)
  (local $mod_name i32)
  ;; project field: comments
  local.set $mc
  ;; project field: name
  local.set $mod_name
  nop
  local.set $mname
  i32.const 0 ;; string: "module"
  call $hydra.serialization.cst
  local.get $mname
  call $hydra.serialization.cst
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "where"
  call $hydra.serialization.cst
  call $hydra.lib.lists.cons
  call $hydra.lib.lists.cons
  call $hydra.serialization.space_sep
  local.set $head
  local.get $head
  local.get $c
  call $hydra.ext.haskell.serde.to_haskell_comments
  call $hydra.serialization.cst
  i32.const 0 ;; string: ""
  call $hydra.serialization.cst
  i32.const 1
  ;; list elements follow
  local.get $head
  call $hydra.lib.lists.cons
  call $hydra.lib.lists.cons
  call $hydra.serialization.newline_sep
  local.get $mc
  call $hydra.lib.maybes.maybe
)
  (func $hydra.ext.haskell.serde.module_to_expr (param $module i32) (result i32)
  (local $decl_lines i32)
  (local $decls i32)
  (local $h i32)
  (local $header_line i32)
  (local $import_lines i32)
  (local $imports i32)
  (local $mh i32)
  (local $warning i32)
  ;; project field: head
  local.set $mh
  ;; project field: imports
  local.set $imports
  ;; project field: declarations
  local.set $decls
  i32.const 1
  ;; list elements follow
  call $hydra.constants.warning_auto_generated_file
  call $hydra.ext.haskell.serde.to_simple_comments
  call $hydra.serialization.cst
  local.set $warning
  i32.const 0
  ;; list elements follow
  i32.const 1
  ;; list elements follow
  local.get $h
  call $hydra.ext.haskell.serde.module_head_to_expr
  local.get $mh
  call $hydra.lib.maybes.maybe
  local.set $header_line
  call $hydra.ext.haskell.serde.declaration_with_comments_to_expr
  local.get $decls
  call $hydra.lib.lists.map
  local.set $decl_lines
  local.get $imports
  call $hydra.lib.lists.null
  i32.const 0
  ;; list elements follow
  i32.const 1
  ;; list elements follow
  call $hydra.ext.haskell.serde.import_to_expr
  local.get $imports
  call $hydra.lib.lists.map
  call $hydra.serialization.newline_sep
  call $hydra.lib.logic.if_else
  local.set $import_lines
  i32.const 4
  ;; list elements follow
  local.get $warning
  local.get $header_line
  local.get $import_lines
  local.get $decl_lines
  call $hydra.lib.lists.concat
  call $hydra.serialization.double_newline_sep
)
  (func $hydra.ext.haskell.serde.name_to_expr (param $name i32) (result i32)
  (local $qn i32)
  (block $end_name (result i32)
  (block $parens
  (block $normal
  (block $implicit
  local.get $name
  br_table $implicit $normal $parens $end_name
  i32.const 0 ;; string: "?"
  local.get $qn
  call $hydra.ext.haskell.serde.write_qualified_name
  call $hydra.lib.strings.cat2
  br $end_name
)
  local.get $qn
  call $hydra.ext.haskell.serde.write_qualified_name
  br $end_name
)
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "("
  local.get $qn
  call $hydra.ext.haskell.serde.write_qualified_name
  i32.const 0 ;; string: ")"
  call $hydra.lib.strings.cat
  br $end_name
)
)
  call $hydra.serialization.cst
)
  (func $hydra.ext.haskell.serde.pattern_to_expr (param $pat i32) (result i32)
  (local $app i32)
  (local $lit i32)
  (local $name i32)
  (local $pat' i32)
  (local $pats i32)
  (block $end_pattern (result i32)
  (block $wildcard
  (block $tuple
  (block $parens
  (block $name
  (block $literal
  (block $list
  (block $application
  local.get $pat
  br_table $application $list $literal $name $parens $tuple $wildcard $end_pattern
  local.get $app
  call $hydra.ext.haskell.serde.application_pattern_to_expr
  br $end_pattern
)
  call $hydra.serialization.half_block_style
  call $hydra.ext.haskell.serde.pattern_to_expr
  local.get $pats
  call $hydra.lib.lists.map
  call $hydra.serialization.bracket_list
  br $end_pattern
)
  local.get $lit
  call $hydra.ext.haskell.serde.literal_to_expr
  br $end_pattern
)
  local.get $name
  call $hydra.ext.haskell.serde.name_to_expr
  br $end_pattern
)
  local.get $pat'
  call $hydra.ext.haskell.serde.pattern_to_expr
  call $hydra.serialization.parenthesize
  br $end_pattern
)
  i32.const 0
  call $hydra.ext.haskell.serde.pattern_to_expr
  local.get $pats
  call $hydra.lib.lists.map
  call $hydra.serialization.paren_list
  br $end_pattern
)
  i32.const 0 ;; string: "_"
  call $hydra.serialization.cst
  br $end_pattern
)
)
)
  (func $hydra.ext.haskell.serde.right_hand_side_to_expr (param $rhs i32) (result i32)
  nop
  call $hydra.ext.haskell.serde.expression_to_expr
)
  (func $hydra.ext.haskell.serde.statement_to_expr (param $stmt i32) (result i32)
  nop
  call $hydra.ext.haskell.serde.expression_to_expr
)
  (func $hydra.ext.haskell.serde.to_haskell_comments (param $c i32) (result i32)
  (local $s i32)
  i32.const 0 ;; string: "
"
  i32.const 0 ;; string: "-- | "
  local.get $s
  call $hydra.lib.strings.cat2
  local.get $c
  call $hydra.lib.strings.lines
  call $hydra.lib.lists.map
  call $hydra.lib.strings.intercalate
)
  (func $hydra.ext.haskell.serde.to_simple_comments (param $c i32) (result i32)
  (local $s i32)
  i32.const 0 ;; string: "
"
  i32.const 0 ;; string: "-- "
  local.get $s
  call $hydra.lib.strings.cat2
  local.get $c
  call $hydra.lib.strings.lines
  call $hydra.lib.lists.map
  call $hydra.lib.strings.intercalate
)
  (func $hydra.ext.haskell.serde.type_signature_to_expr (param $type_sig i32) (result i32)
  (local $inline_sig i32)
  (local $name i32)
  (local $name_expr i32)
  (local $typ i32)
  (local $type_expr i32)
  ;; project field: name
  local.set $name
  ;; project field: type
  local.set $typ
  local.get $name
  call $hydra.ext.haskell.serde.name_to_expr
  local.set $name_expr
  local.get $typ
  call $hydra.ext.haskell.serde.type_to_expr
  local.set $type_expr
  i32.const 3
  ;; list elements follow
  local.get $name_expr
  i32.const 0 ;; string: "::"
  call $hydra.serialization.cst
  local.get $type_expr
  call $hydra.serialization.structural_space_sep
  local.set $inline_sig
  local.get $inline_sig
  call $hydra.serialization.expression_length
  i32.const 120
  call $hydra.lib.equality.gt
  i32.const 2
  ;; list elements follow
  i32.const 2
  ;; list elements follow
  local.get $name_expr
  i32.const 0 ;; string: "::"
  call $hydra.serialization.cst
  call $hydra.serialization.space_sep
  local.get $type_expr
  call $hydra.serialization.tab_indent
  call $hydra.serialization.newline_sep
  local.get $inline_sig
  call $hydra.lib.logic.if_else
)
  (func $hydra.ext.haskell.serde.type_to_expr (param $htype i32) (result i32)
  (local $cod i32)
  (local $ctx i32)
  (local $dom i32)
  (local $htype' i32)
  (local $lhs i32)
  (local $name i32)
  (local $rhs i32)
  (local $typ i32)
  (local $types i32)
  (block $end_type (result i32)
  (block $variable
  (block $tuple
  (block $list
  (block $function
  (block $ctx
  (block $application
  local.get $htype
  br_table $application $ctx $function $list $tuple $variable $end_type
  ;; project field: context
  local.set $lhs
  ;; project field: argument
  local.set $rhs
  call $hydra.ext.haskell.operators.app_op
  local.get $lhs
  call $hydra.ext.haskell.serde.type_to_expr
  local.get $rhs
  call $hydra.ext.haskell.serde.type_to_expr
  call $hydra.serialization.ifx
  br $end_type
)
  ;; project field: ctx
  local.set $ctx
  ;; project field: type
  local.set $typ
  call $hydra.ext.haskell.operators.assert_op
  local.get $ctx
  call $hydra.ext.haskell.serde.assertion_to_expr
  local.get $typ
  call $hydra.ext.haskell.serde.type_to_expr
  call $hydra.serialization.ifx
  br $end_type
)
  ;; project field: domain
  local.set $dom
  ;; project field: codomain
  local.set $cod
  call $hydra.ext.haskell.operators.arrow_op
  local.get $dom
  call $hydra.ext.haskell.serde.type_to_expr
  local.get $cod
  call $hydra.ext.haskell.serde.type_to_expr
  call $hydra.serialization.ifx
  br $end_type
)
  call $hydra.serialization.inline_style
  i32.const 1
  ;; list elements follow
  local.get $htype'
  call $hydra.ext.haskell.serde.type_to_expr
  call $hydra.serialization.bracket_list
  br $end_type
)
  i32.const 0
  call $hydra.ext.haskell.serde.type_to_expr
  local.get $types
  call $hydra.lib.lists.map
  call $hydra.serialization.paren_list
  br $end_type
)
  local.get $name
  call $hydra.ext.haskell.serde.name_to_expr
  br $end_type
)
)
)
  (func $hydra.ext.haskell.serde.value_binding_to_expr (param $vb i32) (result i32)
  (local $bindings i32)
  (local $body i32)
  (local $inline_body i32)
  (local $lhs_expr i32)
  (local $local i32)
  (local $pat i32)
  (local $rhs i32)
  (local $rhs_expr i32)
  (block $end_value_binding (result i32)
  (block $simple
  local.get $vb
  br_table $simple $end_value_binding
  ;; project field: pattern
  local.set $pat
  ;; project field: rhs
  local.set $rhs
  ;; project field: local_bindings
  local.set $local
  local.get $pat
  call $hydra.ext.haskell.serde.pattern_to_expr
  local.set $lhs_expr
  local.get $rhs
  call $hydra.ext.haskell.serde.right_hand_side_to_expr
  local.set $rhs_expr
  i32.const 3
  ;; list elements follow
  local.get $lhs_expr
  i32.const 0 ;; string: "="
  call $hydra.serialization.cst
  local.get $rhs_expr
  call $hydra.serialization.structural_space_sep
  local.set $inline_body
  local.get $inline_body
  call $hydra.serialization.expression_length
  i32.const 120
  call $hydra.lib.equality.gt
  i32.const 2
  ;; list elements follow
  i32.const 2
  ;; list elements follow
  local.get $lhs_expr
  i32.const 0 ;; string: "="
  call $hydra.serialization.cst
  call $hydra.serialization.space_sep
  local.get $rhs_expr
  call $hydra.serialization.tab_indent
  call $hydra.serialization.newline_sep
  local.get $inline_body
  call $hydra.lib.logic.if_else
  local.set $body
  local.get $body
  nop
  local.set $bindings
  local.get $body
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "where"
  call $hydra.serialization.cst
  call $hydra.ext.haskell.serde.local_binding_to_expr
  local.get $bindings
  call $hydra.lib.lists.map
  call $hydra.lib.lists.cons
  call $hydra.serialization.indent_block
  call $hydra.lib.lists.cons
  call $hydra.serialization.indent_block
  local.get $local
  call $hydra.lib.maybes.maybe
  br $end_value_binding
)
)
)
  (func $hydra.ext.haskell.serde.variable_to_expr (param $variable i32) (result i32)
  nop
  call $hydra.ext.haskell.serde.name_to_expr
)
  (func $hydra.ext.haskell.serde.write_qualified_name (param $qname i32) (result i32)
  (local $all_parts i32)
  (local $h i32)
  (local $qualifiers i32)
  (local $unqual i32)
  ;; project field: qualifiers
  local.set $qualifiers
  ;; project field: unqualified
  local.set $unqual
  nop
  local.set $h
  local.get $h
  local.get $qualifiers
  call $hydra.lib.lists.map
  i32.const 1
  ;; list elements follow
  local.get $unqual
  local.get $h
  call $hydra.lib.lists.concat2
  local.set $all_parts
  i32.const 0 ;; string: "."
  local.get $all_parts
  call $hydra.lib.strings.intercalate
)
)
