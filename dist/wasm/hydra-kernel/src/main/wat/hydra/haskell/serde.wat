(module
  (import "hydra.lib.equality" "hydra.lib.equality.equal" (func $hydra.lib.equality.equal (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.equality" "hydra.lib.equality.gt" (func $hydra.lib.equality.gt (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.equality" "hydra.lib.equality.lt" (func $hydra.lib.equality.lt (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.concat" (func $hydra.lib.lists.concat (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.concat2" (func $hydra.lib.lists.concat2 (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.cons" (func $hydra.lib.lists.cons (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.null" (func $hydra.lib.lists.null (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_bigint" (func $hydra.lib.literals.show_bigint (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_float32" (func $hydra.lib.literals.show_float32 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_float64" (func $hydra.lib.literals.show_float64 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_int32" (func $hydra.lib.literals.show_int32 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_string" (func $hydra.lib.literals.show_string (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_uint16" (func $hydra.lib.literals.show_uint16 (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.if_else" (func $hydra.lib.logic.if_else (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.cat" (func $hydra.lib.maybes.cat (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.from_maybe" (func $hydra.lib.maybes.from_maybe (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.map" (func $hydra.lib.maybes.map (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.maybe" (func $hydra.lib.maybes.maybe (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat" (func $hydra.lib.strings.cat (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat2" (func $hydra.lib.strings.cat2 (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.intercalate" (func $hydra.lib.strings.intercalate (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.lines" (func $hydra.lib.strings.lines (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.maybe_char_at" (func $hydra.lib.strings.maybe_char_at (param i32) (param i32) (result i32) ) )
  (import "hydra.serialization" "hydra.serialization.bracket_list" (func $hydra.serialization.bracket_list (param i32) (param i32) (result i32) ) )
  (import "hydra.serialization" "hydra.serialization.brackets" (func $hydra.serialization.brackets (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.serialization" "hydra.serialization.comma_sep" (func $hydra.serialization.comma_sep (param i32) (param i32) (result i32) ) )
  (import "hydra.serialization" "hydra.serialization.cst" (func $hydra.serialization.cst (param i32) (result i32) ) )
  (import "hydra.serialization" "hydra.serialization.curly_braces_list" (func $hydra.serialization.curly_braces_list (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.serialization" "hydra.serialization.custom_indent_block" (func $hydra.serialization.custom_indent_block (param i32) (param i32) (result i32) ) )
  (import "hydra.serialization" "hydra.serialization.double_newline_sep" (func $hydra.serialization.double_newline_sep (param i32) (result i32) ) )
  (import "hydra.serialization" "hydra.serialization.expression_length" (func $hydra.serialization.expression_length (param i32) (result i32) ) )
  (import "hydra.serialization" "hydra.serialization.ifx" (func $hydra.serialization.ifx (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.serialization" "hydra.serialization.indent_block" (func $hydra.serialization.indent_block (param i32) (result i32) ) )
  (import "hydra.serialization" "hydra.serialization.indent_subsequent_lines" (func $hydra.serialization.indent_subsequent_lines (param i32) (param i32) (result i32) ) )
  (import "hydra.serialization" "hydra.serialization.newline_sep" (func $hydra.serialization.newline_sep (param i32) (result i32) ) )
  (import "hydra.serialization" "hydra.serialization.or_sep" (func $hydra.serialization.or_sep (param i32) (param i32) (result i32) ) )
  (import "hydra.serialization" "hydra.serialization.paren_list" (func $hydra.serialization.paren_list (param i32) (param i32) (result i32) ) )
  (import "hydra.serialization" "hydra.serialization.parens" (func $hydra.serialization.parens (param i32) (result i32) ) )
  (import "hydra.serialization" "hydra.serialization.parenthesize" (func $hydra.serialization.parenthesize (param i32) (result i32) ) )
  (import "hydra.serialization" "hydra.serialization.prefix" (func $hydra.serialization.prefix (param i32) (param i32) (result i32) ) )
  (import "hydra.serialization" "hydra.serialization.space_sep" (func $hydra.serialization.space_sep (param i32) (result i32) ) )
  (import "hydra.serialization" "hydra.serialization.structural_space_sep" (func $hydra.serialization.structural_space_sep (param i32) (result i32) ) )
  (import "hydra.serialization" "hydra.serialization.tab_indent" (func $hydra.serialization.tab_indent (param i32) (result i32) ) )
  (memory $memory 2 )
  (export "memory" (memory $memory) )
  (data (offset i32.const 1024 ) "\00\00\00\00\01\00\00\00\0a\02\00\00\00\20\20\04\00\00\00\20\20\20\20\01\00\00\00\28\08\00\00\00\28\2d\28\31\2f\30\29\29\05\00\00\00\28\30\2f\30\29\05\00\00\00\28\31\2f\30\29\01\00\00\00\29\03\00\00\00\2d\2d\20\05\00\00\00\2d\2d\20\7c\20\02\00\00\00\2d\3e\09\00\00\00\2d\49\6e\66\69\6e\69\74\79\01\00\00\00\2e\08\00\00\00\30\3a\62\69\67\69\6e\74\02\00\00\00\3a\3a\01\00\00\00\3d\01\00\00\00\3f\08\00\00\00\49\6e\66\69\6e\69\74\79\03\00\00\00\4e\61\4e\01\00\00\00\5c\01\00\00\00\5f\03\00\00\00\61\73\20\04\00\00\00\63\61\73\65\04\00\00\00\64\61\74\61\08\00\00\00\64\65\72\69\76\69\6e\67\02\00\00\00\64\6f\04\00\00\00\65\6c\73\65\07\00\00\00\68\69\64\69\6e\67\20\02\00\00\00\69\66\06\00\00\00\69\6d\70\6f\72\74\02\00\00\00\69\6e\03\00\00\00\6c\65\74\06\00\00\00\6d\6f\64\75\6c\65\07\00\00\00\6e\65\77\74\79\70\65\02\00\00\00\6f\66\09\00\00\00\71\75\61\6c\69\66\69\65\64\04\00\00\00\74\68\65\6e\04\00\00\00\74\79\70\65\05\00\00\00\77\68\65\72\65")
  (global $__bump_ptr (mut i32) i32.const 1344 )
  (export "__bump_ptr" (global $__bump_ptr) )
  (func $__alloc (param $sz i32) (result i32)
  global.get $__bump_ptr
  global.get $__bump_ptr
  local.get $sz
  i32.add
  global.set $__bump_ptr
)
  (export "hydra.haskell.serde.alternative_to_expr" (func $hydra.haskell.serde.alternative_to_expr) )
  (export "hydra.haskell.serde.application_expression_to_expr" (func $hydra.haskell.serde.application_expression_to_expr) )
  (export "hydra.haskell.serde.application_pattern_to_expr" (func $hydra.haskell.serde.application_pattern_to_expr) )
  (export "hydra.haskell.serde.assertion_to_expr" (func $hydra.haskell.serde.assertion_to_expr) )
  (export "hydra.haskell.serde.case_expression_to_expr" (func $hydra.haskell.serde.case_expression_to_expr) )
  (export "hydra.haskell.serde.case_rhs_to_expr" (func $hydra.haskell.serde.case_rhs_to_expr) )
  (export "hydra.haskell.serde.class_assertion_to_expr" (func $hydra.haskell.serde.class_assertion_to_expr) )
  (export "hydra.haskell.serde.construct_record_expression_to_expr" (func $hydra.haskell.serde.construct_record_expression_to_expr) )
  (export "hydra.haskell.serde.constructor_to_expr" (func $hydra.haskell.serde.constructor_to_expr) )
  (export "hydra.haskell.serde.constructor_with_comments_to_expr" (func $hydra.haskell.serde.constructor_with_comments_to_expr) )
  (export "hydra.haskell.serde.data_or_newtype_to_expr" (func $hydra.haskell.serde.data_or_newtype_to_expr) )
  (export "hydra.haskell.serde.declaration_head_to_expr" (func $hydra.haskell.serde.declaration_head_to_expr) )
  (export "hydra.haskell.serde.declaration_to_expr" (func $hydra.haskell.serde.declaration_to_expr) )
  (export "hydra.haskell.serde.declaration_with_comments_to_expr" (func $hydra.haskell.serde.declaration_with_comments_to_expr) )
  (export "hydra.haskell.serde.expression_to_expr" (func $hydra.haskell.serde.expression_to_expr) )
  (export "hydra.haskell.serde.field_to_expr" (func $hydra.haskell.serde.field_to_expr) )
  (export "hydra.haskell.serde.field_with_comments_to_expr" (func $hydra.haskell.serde.field_with_comments_to_expr) )
  (export "hydra.haskell.serde.if_expression_to_expr" (func $hydra.haskell.serde.if_expression_to_expr) )
  (export "hydra.haskell.serde.import_export_spec_to_expr" (func $hydra.haskell.serde.import_export_spec_to_expr) )
  (export "hydra.haskell.serde.import_to_expr" (func $hydra.haskell.serde.import_to_expr) )
  (export "hydra.haskell.serde.lambda_expression_to_expr" (func $hydra.haskell.serde.lambda_expression_to_expr) )
  (export "hydra.haskell.serde.literal_to_expr" (func $hydra.haskell.serde.literal_to_expr) )
  (export "hydra.haskell.serde.local_binding_to_expr" (func $hydra.haskell.serde.local_binding_to_expr) )
  (export "hydra.haskell.serde.module_head_to_expr" (func $hydra.haskell.serde.module_head_to_expr) )
  (export "hydra.haskell.serde.module_to_expr" (func $hydra.haskell.serde.module_to_expr) )
  (export "hydra.haskell.serde.name_to_expr" (func $hydra.haskell.serde.name_to_expr) )
  (export "hydra.haskell.serde.pattern_to_expr" (func $hydra.haskell.serde.pattern_to_expr) )
  (export "hydra.haskell.serde.right_hand_side_to_expr" (func $hydra.haskell.serde.right_hand_side_to_expr) )
  (export "hydra.haskell.serde.statement_to_expr" (func $hydra.haskell.serde.statement_to_expr) )
  (export "hydra.haskell.serde.to_haskell_comments" (func $hydra.haskell.serde.to_haskell_comments) )
  (export "hydra.haskell.serde.to_simple_comments" (func $hydra.haskell.serde.to_simple_comments) )
  (export "hydra.haskell.serde.type_signature_to_expr" (func $hydra.haskell.serde.type_signature_to_expr) )
  (export "hydra.haskell.serde.type_to_expr" (func $hydra.haskell.serde.type_to_expr) )
  (export "hydra.haskell.serde.value_binding_to_expr" (func $hydra.haskell.serde.value_binding_to_expr) )
  (export "hydra.haskell.serde.variable_to_expr" (func $hydra.haskell.serde.variable_to_expr) )
  (export "hydra.haskell.serde.write_qualified_name" (func $hydra.haskell.serde.write_qualified_name) )
  (func $hydra.haskell.serde.alternative_to_expr (param $alt i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  local.get $alt
  i32.load
  call $hydra.haskell.serde.pattern_to_expr
  i32.const 1103
  call $hydra.serialization.cst
  local.get $alt
  i32.load offset=4
  call $hydra.haskell.serde.case_rhs_to_expr
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
  call $hydra.serialization.structural_space_sep
)
  (func $hydra.haskell.serde.application_expression_to_expr (param $app i32) (result i32)
  i32.const 0
  local.get $app
  i32.load
  call $hydra.haskell.serde.expression_to_expr
  local.get $app
  i32.load offset=4
  call $hydra.haskell.serde.expression_to_expr
  call $hydra.serialization.ifx
)
  (func $hydra.haskell.serde.application_pattern_to_expr (param $app_pat i32) (result i32)
  (local $name i32)
  (local $pats i32)
  local.get $app_pat
  i32.load
  local.set $name
  local.get $app_pat
  i32.load offset=4
  local.set $pats
  local.get $name
  call $hydra.haskell.serde.name_to_expr
  i32.const 0
  local.get $pats
  call $hydra.lib.lists.map
  call $hydra.lib.lists.cons
  call $hydra.serialization.space_sep
)
  (func $hydra.haskell.serde.assertion_to_expr (param $sert i32) (result i32)
  (local $__rec_ptr i32)
  (local $cls i32)
  (local $serts i32)
  (local $v i32)
  (block $end_assertion (result i32)
  (block $tuple
  (block $class
  local.get $sert
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $class $tuple $tuple
)
  local.get $v
  drop
  local.get $cls
  call $hydra.haskell.serde.class_assertion_to_expr
  br $end_assertion
)
  local.get $v
  drop
  i32.const 0
  i32.const 0
  local.get $serts
  call $hydra.lib.lists.map
  call $hydra.serialization.paren_list
  br $end_assertion
)
)
  (func $hydra.haskell.serde.case_expression_to_expr (param $case_expr i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $alts i32)
  (local $cs i32)
  (local $lhs i32)
  (local $of_op i32)
  (local $rhs i32)
  local.get $case_expr
  i32.load
  local.set $cs
  local.get $case_expr
  i32.load offset=4
  local.set $alts
  i32.const 1294
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
  i32.const 1033
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
  i32.const 16
  call $__alloc
  local.set $__rec_ptr
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
  local.set $of_op
  i32.const 1191
  call $hydra.serialization.cst
  local.get $cs
  call $hydra.haskell.serde.expression_to_expr
  i32.const 12
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 2
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=8
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.get $__rec_ptr
  call $hydra.serialization.space_sep
  local.set $lhs
  i32.const 0
  local.get $alts
  call $hydra.lib.lists.map
  call $hydra.serialization.newline_sep
  local.set $rhs
  local.get $of_op
  local.get $lhs
  local.get $rhs
  call $hydra.serialization.ifx
)
  (func $hydra.haskell.serde.case_rhs_to_expr (param $rhs i32) (result i32)
  local.get $rhs
  drop
  i32.const 0
  drop
  i32.const 0
  call $hydra.haskell.serde.expression_to_expr
)
  (func $hydra.haskell.serde.class_assertion_to_expr (param $cls_asrt i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $name i32)
  (local $types i32)
  local.get $cls_asrt
  i32.load
  local.set $name
  local.get $cls_asrt
  i32.load offset=4
  local.set $types
  local.get $name
  call $hydra.haskell.serde.name_to_expr
  i32.const 0
  i32.const 0
  local.get $types
  call $hydra.lib.lists.map
  call $hydra.serialization.comma_sep
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
  call $hydra.lib.lists.cons
  call $hydra.serialization.space_sep
)
  (func $hydra.haskell.serde.construct_record_expression_to_expr (param $construct_record i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $body i32)
  (local $fn i32)
  (local $from_update i32)
  (local $name i32)
  (local $update i32)
  (local $updates i32)
  (local $val i32)
  local.get $construct_record
  i32.load
  local.set $name
  local.get $construct_record
  i32.load offset=4
  local.set $updates
  local.get $update
  i32.load
  local.set $fn
  local.get $update
  i32.load offset=4
  local.set $val
  i32.const 0
  local.get $fn
  call $hydra.haskell.serde.name_to_expr
  local.get $val
  call $hydra.haskell.serde.expression_to_expr
  call $hydra.serialization.ifx
  local.set $from_update
  i32.const 0
  local.get $from_update
  local.get $updates
  call $hydra.lib.lists.map
  call $hydra.serialization.comma_sep
  local.set $body
  local.get $name
  call $hydra.haskell.serde.name_to_expr
  i32.const 0
  i32.const 0
  local.get $body
  call $hydra.serialization.brackets
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
  call $hydra.lib.lists.cons
  call $hydra.serialization.space_sep
)
  (func $hydra.haskell.serde.constructor_to_expr (param $cons i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $fields i32)
  (local $name i32)
  (local $ord i32)
  (local $rec i32)
  (local $types i32)
  (local $v i32)
  (block $end_constructor (result i32)
  (block $record
  (block $ordinary
  local.get $cons
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $ordinary $record $record
)
  local.get $v
  drop
  local.get $ord
  i32.load
  local.set $name
  local.get $ord
  i32.load offset=4
  local.set $types
  local.get $name
  call $hydra.haskell.serde.name_to_expr
  i32.const 0
  local.get $types
  call $hydra.lib.lists.map
  call $hydra.serialization.space_sep
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
  call $hydra.lib.lists.cons
  call $hydra.serialization.space_sep
  br $end_constructor
)
  local.get $v
  drop
  local.get $rec
  i32.load
  local.set $name
  local.get $rec
  i32.load offset=4
  local.set $fields
  local.get $name
  call $hydra.haskell.serde.name_to_expr
  i32.const 0
  i32.const 0
  i32.const 0
  local.get $fields
  call $hydra.lib.lists.map
  call $hydra.serialization.curly_braces_list
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
  call $hydra.lib.lists.cons
  call $hydra.serialization.space_sep
  br $end_constructor
)
)
  (func $hydra.haskell.serde.constructor_with_comments_to_expr (param $cons_with_comments i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $body i32)
  (local $c i32)
  (local $mc i32)
  local.get $cons_with_comments
  i32.load
  local.set $body
  local.get $cons_with_comments
  i32.load offset=4
  local.set $mc
  local.get $body
  call $hydra.haskell.serde.constructor_to_expr
  local.get $c
  call $hydra.haskell.serde.to_haskell_comments
  call $hydra.serialization.cst
  local.get $body
  call $hydra.haskell.serde.constructor_to_expr
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
  call $hydra.lib.lists.cons
  call $hydra.serialization.newline_sep
  local.get $mc
  call $hydra.lib.maybes.maybe
)
  (func $hydra.haskell.serde.data_or_newtype_to_expr (param $kw i32) (result i32)
  (local $__rec_ptr i32)
  (local $v i32)
  (block $end_data_or_newtype (result i32)
  (block $newtype
  (block $data
  local.get $kw
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $data $newtype $newtype
)
  local.get $v
  drop
  i32.const 1199
  call $hydra.serialization.cst
  br $end_data_or_newtype
)
  local.get $v
  drop
  i32.const 1283
  call $hydra.serialization.cst
  br $end_data_or_newtype
)
)
  (func $hydra.haskell.serde.declaration_head_to_expr (param $hd i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $app_head i32)
  (local $fun i32)
  (local $name i32)
  (local $op i32)
  (local $v i32)
  (block $end_declaration_head (result i32)
  (block $simple
  (block $application
  local.get $hd
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $application $simple $simple
)
  local.get $v
  drop
  local.get $app_head
  i32.load
  local.set $fun
  local.get $app_head
  i32.load offset=4
  local.set $op
  local.get $fun
  call $hydra.haskell.serde.declaration_head_to_expr
  local.get $op
  call $hydra.haskell.serde.variable_to_expr
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
  call $hydra.lib.lists.cons
  call $hydra.serialization.space_sep
  br $end_declaration_head
)
  local.get $v
  drop
  local.get $name
  call $hydra.haskell.serde.name_to_expr
  br $end_declaration_head
)
)
  (func $hydra.haskell.serde.declaration_to_expr (param $decl i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $cons i32)
  (local $constructors i32)
  (local $data_decl i32)
  (local $deriv i32)
  (local $deriv_cat i32)
  (local $deriving_clause i32)
  (local $hd i32)
  (local $htype i32)
  (local $kw i32)
  (local $main_parts i32)
  (local $name i32)
  (local $typ i32)
  (local $type_decl i32)
  (local $type_sig i32)
  (local $typed_binding i32)
  (local $v i32)
  (local $vb i32)
  (block $end_declaration (result i32)
  (block $typed_binding
  (block $value_binding
  (block $type
  (block $data
  local.get $decl
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $data $type $value_binding $typed_binding $typed_binding
)
  local.get $v
  drop
  local.get $data_decl
  i32.load
  local.set $kw
  local.get $data_decl
  i32.load offset=8
  local.set $hd
  local.get $data_decl
  i32.load offset=12
  local.set $cons
  local.get $data_decl
  i32.load offset=16
  local.set $deriv
  i32.const 0
  local.get $deriv
  call $hydra.lib.lists.map
  call $hydra.lib.lists.concat
  local.set $deriv_cat
  i32.const 0
  i32.const 0
  local.get $cons
  call $hydra.lib.lists.map
  call $hydra.serialization.or_sep
  local.set $constructors
  local.get $deriv_cat
  call $hydra.lib.lists.null
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  i32.const 1207
  call $hydra.serialization.cst
  i32.const 0
  i32.const 0
  local.get $deriv_cat
  call $hydra.lib.lists.map
  call $hydra.serialization.paren_list
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
  call $hydra.lib.lists.cons
  call $hydra.serialization.space_sep
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
  call $hydra.lib.logic.if_else
  local.set $deriving_clause
  local.get $kw
  call $hydra.haskell.serde.data_or_newtype_to_expr
  local.get $hd
  call $hydra.haskell.serde.declaration_head_to_expr
  i32.const 1145
  call $hydra.serialization.cst
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
  call $hydra.lib.lists.cons
  call $hydra.lib.lists.cons
  call $hydra.serialization.space_sep
  local.get $constructors
  i32.const 12
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 2
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=8
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.get $__rec_ptr
  local.set $main_parts
  local.get $main_parts
  local.get $deriving_clause
  call $hydra.lib.lists.concat2
  call $hydra.serialization.indent_block
  br $end_declaration
)
  local.get $v
  drop
  local.get $type_decl
  i32.load
  local.set $hd
  local.get $type_decl
  i32.load offset=4
  local.set $typ
  i32.const 1321
  call $hydra.serialization.cst
  local.get $hd
  call $hydra.haskell.serde.declaration_head_to_expr
  i32.const 1145
  call $hydra.serialization.cst
  local.get $typ
  call $hydra.haskell.serde.type_to_expr
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
  call $hydra.lib.lists.cons
  call $hydra.lib.lists.cons
  call $hydra.lib.lists.cons
  call $hydra.serialization.space_sep
  br $end_declaration
)
  local.get $v
  drop
  local.get $vb
  call $hydra.haskell.serde.value_binding_to_expr
  br $end_declaration
)
  local.get $v
  drop
  local.get $typed_binding
  i32.load
  local.set $type_sig
  local.get $typed_binding
  i32.load offset=4
  local.set $vb
  local.get $type_sig
  i32.load
  local.set $name
  local.get $type_sig
  i32.load offset=4
  local.set $htype
  local.get $name
  call $hydra.haskell.serde.name_to_expr
  i32.const 1139
  call $hydra.serialization.cst
  local.get $htype
  call $hydra.haskell.serde.type_to_expr
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
  call $hydra.serialization.structural_space_sep
  local.get $vb
  call $hydra.haskell.serde.value_binding_to_expr
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
  call $hydra.lib.lists.cons
  call $hydra.serialization.newline_sep
  br $end_declaration
)
)
  (func $hydra.haskell.serde.declaration_with_comments_to_expr (param $decl_with_comments i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $body i32)
  (local $c i32)
  (local $mc i32)
  local.get $decl_with_comments
  i32.load
  local.set $body
  local.get $decl_with_comments
  i32.load offset=4
  local.set $mc
  local.get $body
  call $hydra.haskell.serde.declaration_to_expr
  local.get $c
  call $hydra.haskell.serde.to_haskell_comments
  call $hydra.serialization.cst
  local.get $body
  call $hydra.haskell.serde.declaration_to_expr
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
  call $hydra.lib.lists.cons
  call $hydra.serialization.newline_sep
  local.get $mc
  call $hydra.lib.maybes.maybe
)
  (func $hydra.haskell.serde.expression_to_expr (param $expr i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
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
  (local $let_expr i32)
  (local $lit i32)
  (local $name i32)
  (local $r i32)
  (local $statements i32)
  (local $v i32)
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
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $application $case $construct_record $do $if $literal $lambda $let $list $parens $tuple $variable $variable
)
  local.get $v
  drop
  local.get $app
  call $hydra.haskell.serde.application_expression_to_expr
  br $end_expression
)
  local.get $v
  drop
  local.get $cases
  call $hydra.haskell.serde.case_expression_to_expr
  br $end_expression
)
  local.get $v
  drop
  local.get $r
  call $hydra.haskell.serde.construct_record_expression_to_expr
  br $end_expression
)
  local.get $v
  drop
  i32.const 1219
  call $hydra.serialization.cst
  i32.const 0
  local.get $statements
  call $hydra.lib.lists.map
  call $hydra.lib.lists.cons
  call $hydra.serialization.indent_block
  br $end_expression
)
  local.get $v
  drop
  local.get $ifte
  call $hydra.haskell.serde.if_expression_to_expr
  br $end_expression
)
  local.get $v
  drop
  local.get $lit
  call $hydra.haskell.serde.literal_to_expr
  br $end_expression
)
  local.get $v
  drop
  local.get $lam
  call $hydra.haskell.serde.lambda_expression_to_expr
  call $hydra.serialization.parenthesize
  br $end_expression
)
  local.get $v
  drop
  local.get $let_expr
  i32.load
  local.set $bindings
  local.get $let_expr
  i32.load offset=4
  local.set $inner
  i32.const 1039
  local.get $binding
  call $hydra.haskell.serde.local_binding_to_expr
  call $hydra.serialization.indent_subsequent_lines
  local.set $encode_binding
  i32.const 1024
  call $hydra.serialization.cst
  i32.const 1266
  call $hydra.serialization.cst
  i32.const 1039
  local.get $encode_binding
  local.get $bindings
  call $hydra.lib.lists.map
  call $hydra.serialization.custom_indent_block
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
  call $hydra.lib.lists.cons
  call $hydra.serialization.space_sep
  i32.const 1260
  call $hydra.serialization.cst
  local.get $inner
  call $hydra.haskell.serde.expression_to_expr
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
  call $hydra.lib.lists.cons
  call $hydra.serialization.space_sep
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
  call $hydra.lib.lists.cons
  call $hydra.lib.lists.cons
  call $hydra.serialization.indent_block
  br $end_expression
)
  local.get $v
  drop
  i32.const 0
  i32.const 0
  local.get $exprs
  call $hydra.lib.lists.map
  call $hydra.serialization.bracket_list
  br $end_expression
)
  local.get $v
  drop
  local.get $expr'
  call $hydra.haskell.serde.expression_to_expr
  call $hydra.serialization.parenthesize
  br $end_expression
)
  local.get $v
  drop
  i32.const 0
  i32.const 0
  local.get $exprs
  call $hydra.lib.lists.map
  call $hydra.serialization.paren_list
  br $end_expression
)
  local.get $v
  drop
  local.get $name
  call $hydra.haskell.serde.name_to_expr
  br $end_expression
)
)
  (func $hydra.haskell.serde.field_to_expr (param $field i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $name i32)
  (local $typ i32)
  local.get $field
  i32.load
  local.set $name
  local.get $field
  i32.load offset=4
  local.set $typ
  local.get $name
  call $hydra.haskell.serde.name_to_expr
  i32.const 1139
  call $hydra.serialization.cst
  local.get $typ
  call $hydra.haskell.serde.type_to_expr
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
  call $hydra.lib.lists.cons
  call $hydra.lib.lists.cons
  call $hydra.serialization.space_sep
)
  (func $hydra.haskell.serde.field_with_comments_to_expr (param $field_with_comments i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $c i32)
  (local $field i32)
  (local $mc i32)
  local.get $field_with_comments
  i32.load
  local.set $field
  local.get $field_with_comments
  i32.load offset=4
  local.set $mc
  local.get $field
  call $hydra.haskell.serde.field_to_expr
  local.get $c
  call $hydra.haskell.serde.to_haskell_comments
  call $hydra.serialization.cst
  local.get $field
  call $hydra.haskell.serde.field_to_expr
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
  call $hydra.lib.lists.cons
  call $hydra.serialization.newline_sep
  local.get $mc
  call $hydra.lib.maybes.maybe
)
  (func $hydra.haskell.serde.if_expression_to_expr (param $if_expr i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $body i32)
  (local $eelse i32)
  (local $eif i32)
  (local $ethen i32)
  (local $if_op i32)
  local.get $if_expr
  i32.load
  local.set $eif
  local.get $if_expr
  i32.load offset=4
  local.set $ethen
  local.get $if_expr
  i32.load offset=8
  local.set $eelse
  i32.const 1024
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
  i32.const 1033
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
  i32.const 16
  call $__alloc
  local.set $__rec_ptr
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
  local.set $if_op
  i32.const 1313
  call $hydra.serialization.cst
  local.get $ethen
  call $hydra.haskell.serde.expression_to_expr
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
  call $hydra.lib.lists.cons
  call $hydra.serialization.space_sep
  i32.const 1225
  call $hydra.serialization.cst
  local.get $eelse
  call $hydra.haskell.serde.expression_to_expr
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
  call $hydra.lib.lists.cons
  call $hydra.serialization.space_sep
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
  call $hydra.lib.lists.cons
  call $hydra.serialization.newline_sep
  local.set $body
  local.get $if_op
  i32.const 1244
  call $hydra.serialization.cst
  local.get $eif
  call $hydra.haskell.serde.expression_to_expr
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
  call $hydra.lib.lists.cons
  call $hydra.serialization.space_sep
  local.get $body
  call $hydra.serialization.ifx
)
  (func $hydra.haskell.serde.import_export_spec_to_expr (param $spec i32) (result i32)
  local.get $spec
  i32.load offset=4
  call $hydra.haskell.serde.name_to_expr
)
  (func $hydra.haskell.serde.import_to_expr (param $import i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $hiding_sec i32)
  (local $m i32)
  (local $mod i32)
  (local $mod_name i32)
  (local $mspec i32)
  (local $name i32)
  (local $names i32)
  (local $parts i32)
  (local $qual i32)
  (local $spec i32)
  (local $v i32)
  local.get $import
  i32.load
  local.set $qual
  local.get $import
  i32.load offset=4
  local.set $mod_name
  local.get $import
  i32.load offset=8
  local.set $mod
  local.get $import
  i32.load offset=12
  local.set $mspec
  local.get $mod_name
  drop
  i32.const 0
  drop
  i32.const 0
  local.set $name
  (block $end_spec_import (result i32)
  (block $hiding
  local.get $spec
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $hiding $hiding
)
  local.get $v
  drop
  i32.const 1233
  call $hydra.serialization.cst
  i32.const 0
  i32.const 0
  local.get $names
  call $hydra.lib.lists.map
  call $hydra.serialization.comma_sep
  call $hydra.serialization.parens
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
  call $hydra.lib.lists.cons
  call $hydra.serialization.space_sep
  br $end_spec_import
)
  local.set $hiding_sec
  i32.const 1250
  call $hydra.serialization.cst
  local.get $qual
  i32.const 1300
  call $hydra.serialization.cst
  i32.const 0
  call $hydra.lib.logic.if_else
  local.get $name
  call $hydra.serialization.cst
  i32.const 1184
  local.get $m
  drop
  i32.const 0
  drop
  i32.const 0
  call $hydra.lib.strings.cat2
  call $hydra.serialization.cst
  local.get $mod
  call $hydra.lib.maybes.map
  local.get $hiding_sec
  local.get $mspec
  call $hydra.lib.maybes.map
  i32.const 24
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 5
  i32.store
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
  call $hydra.lib.maybes.cat
  local.set $parts
  local.get $parts
  call $hydra.serialization.space_sep
)
  (func $hydra.haskell.serde.lambda_expression_to_expr (param $lambda_expr i32) (result i32)
  (local $bindings i32)
  (local $body i32)
  (local $head i32)
  (local $inner i32)
  local.get $lambda_expr
  i32.load
  local.set $bindings
  local.get $lambda_expr
  i32.load offset=4
  local.set $inner
  i32.const 0
  local.get $bindings
  call $hydra.lib.lists.map
  call $hydra.serialization.space_sep
  local.set $head
  local.get $inner
  call $hydra.haskell.serde.expression_to_expr
  local.set $body
  i32.const 0
  i32.const 1174
  local.get $head
  call $hydra.serialization.prefix
  local.get $body
  call $hydra.serialization.ifx
)
  (func $hydra.haskell.serde.literal_to_expr (param $lit i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $b i32)
  (local $c i32)
  (local $d i32)
  (local $e i32)
  (local $f i32)
  (local $i i32)
  (local $parens_if_neg i32)
  (local $raw i32)
  (local $s i32)
  (local $show_float i32)
  (local $show_fn i32)
  (local $v i32)
  local.get $b
  i32.const 1047
  local.get $e
  i32.const 1082
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
  local.get $e
  call $hydra.lib.logic.if_else
  local.set $parens_if_neg
  local.get $v
  drop
  local.get $show_fn
  drop
  i32.const 0
  local.set $raw
  local.get $raw
  i32.const 1167
  call $hydra.lib.equality.equal
  i32.const 1064
  local.get $raw
  i32.const 1155
  call $hydra.lib.equality.equal
  i32.const 1073
  local.get $raw
  i32.const 1109
  call $hydra.lib.equality.equal
  i32.const 1052
  i32.const 0
  i32.const 0
  local.get $raw
  call $hydra.lib.strings.maybe_char_at
  call $hydra.lib.maybes.from_maybe
  i32.const 45
  call $hydra.lib.equality.equal
  drop
  local.get $raw
  drop
  local.get $parens_if_neg
  drop
  i32.const 0
  call $hydra.lib.logic.if_else
  call $hydra.lib.logic.if_else
  call $hydra.lib.logic.if_else
  local.set $show_float
  (block $end_literal (result i32)
  (block $string
  (block $integer
  (block $int
  (block $float
  (block $double
  (block $char
  local.get $lit
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $char $double $float $int $integer $string $string
)
  local.get $v
  drop
  local.get $c
  call $hydra.lib.literals.show_uint16
  call $hydra.lib.literals.show_string
  br $end_literal
)
  local.get $v
  drop
  local.get $v
  call $hydra.lib.literals.show_float64
  drop
  local.get $d
  drop
  local.get $show_float
  drop
  i32.const 0
  br $end_literal
)
  local.get $v
  drop
  local.get $v
  call $hydra.lib.literals.show_float32
  drop
  local.get $f
  drop
  local.get $show_float
  drop
  i32.const 0
  br $end_literal
)
  local.get $v
  drop
  local.get $i
  i32.const 0
  call $hydra.lib.equality.lt
  drop
  local.get $i
  call $hydra.lib.literals.show_int32
  drop
  local.get $parens_if_neg
  drop
  i32.const 0
  br $end_literal
)
  local.get $v
  drop
  local.get $i
  i32.const 1127
  call $hydra.lib.equality.lt
  drop
  local.get $i
  call $hydra.lib.literals.show_bigint
  drop
  local.get $parens_if_neg
  drop
  i32.const 0
  br $end_literal
)
  local.get $v
  drop
  local.get $s
  call $hydra.lib.literals.show_string
  br $end_literal
)
  call $hydra.serialization.cst
)
  (func $hydra.haskell.serde.local_binding_to_expr (param $binding i32) (result i32)
  (local $__rec_ptr i32)
  (local $ts i32)
  (local $v i32)
  (local $vb i32)
  (block $end_local_binding (result i32)
  (block $value
  (block $signature
  local.get $binding
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $signature $value $value
)
  local.get $v
  drop
  local.get $ts
  call $hydra.haskell.serde.type_signature_to_expr
  br $end_local_binding
)
  local.get $v
  drop
  local.get $vb
  call $hydra.haskell.serde.value_binding_to_expr
  br $end_local_binding
)
)
  (func $hydra.haskell.serde.module_head_to_expr (param $module_head i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $c i32)
  (local $head i32)
  (local $mc i32)
  (local $mname i32)
  (local $mod_name i32)
  local.get $module_head
  i32.load
  local.set $mc
  local.get $module_head
  i32.load offset=4
  local.set $mod_name
  local.get $mod_name
  drop
  i32.const 0
  drop
  i32.const 0
  local.set $mname
  i32.const 1273
  call $hydra.serialization.cst
  local.get $mname
  call $hydra.serialization.cst
  i32.const 1329
  call $hydra.serialization.cst
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
  call $hydra.lib.lists.cons
  call $hydra.lib.lists.cons
  call $hydra.serialization.space_sep
  local.set $head
  local.get $head
  local.get $c
  call $hydra.haskell.serde.to_haskell_comments
  call $hydra.serialization.cst
  i32.const 1024
  call $hydra.serialization.cst
  local.get $head
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
  call $hydra.lib.lists.cons
  call $hydra.lib.lists.cons
  call $hydra.serialization.newline_sep
  local.get $mc
  call $hydra.lib.maybes.maybe
)
  (func $hydra.haskell.serde.module_to_expr (param $module i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $decl_lines i32)
  (local $decls i32)
  (local $h i32)
  (local $header_line i32)
  (local $import_lines i32)
  (local $imports i32)
  (local $mh i32)
  (local $warning i32)
  local.get $module
  i32.load
  local.set $mh
  local.get $module
  i32.load offset=4
  local.set $imports
  local.get $module
  i32.load offset=8
  local.set $decls
  i32.const 0
  call $hydra.haskell.serde.to_simple_comments
  call $hydra.serialization.cst
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
  local.set $warning
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  local.get $h
  call $hydra.haskell.serde.module_head_to_expr
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
  local.get $mh
  call $hydra.lib.maybes.maybe
  local.set $header_line
  i32.const 0
  local.get $decls
  call $hydra.lib.lists.map
  local.set $decl_lines
  local.get $imports
  call $hydra.lib.lists.null
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  i32.const 0
  local.get $imports
  call $hydra.lib.lists.map
  call $hydra.serialization.newline_sep
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
  call $hydra.lib.logic.if_else
  local.set $import_lines
  local.get $warning
  local.get $header_line
  local.get $import_lines
  local.get $decl_lines
  i32.const 20
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 4
  i32.store
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
  call $hydra.lib.lists.concat
  call $hydra.serialization.double_newline_sep
)
  (func $hydra.haskell.serde.name_to_expr (param $name i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $qn i32)
  (local $v i32)
  (block $end_name (result i32)
  (block $parens
  (block $normal
  (block $implicit
  local.get $name
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $implicit $normal $parens $parens
)
  local.get $v
  drop
  i32.const 1150
  local.get $qn
  call $hydra.haskell.serde.write_qualified_name
  call $hydra.lib.strings.cat2
  br $end_name
)
  local.get $v
  drop
  local.get $qn
  call $hydra.haskell.serde.write_qualified_name
  br $end_name
)
  local.get $v
  drop
  i32.const 1047
  local.get $qn
  call $hydra.haskell.serde.write_qualified_name
  i32.const 1082
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
  br $end_name
)
  call $hydra.serialization.cst
)
  (func $hydra.haskell.serde.pattern_to_expr (param $pat i32) (result i32)
  (local $__rec_ptr i32)
  (local $app i32)
  (local $lit i32)
  (local $name i32)
  (local $pat' i32)
  (local $pats i32)
  (local $v i32)
  (block $end_pattern (result i32)
  (block $wildcard
  (block $tuple
  (block $parens
  (block $name
  (block $literal
  (block $list
  (block $application
  local.get $pat
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $application $list $literal $name $parens $tuple $wildcard $wildcard
)
  local.get $v
  drop
  local.get $app
  call $hydra.haskell.serde.application_pattern_to_expr
  br $end_pattern
)
  local.get $v
  drop
  i32.const 0
  i32.const 0
  local.get $pats
  call $hydra.lib.lists.map
  call $hydra.serialization.bracket_list
  br $end_pattern
)
  local.get $v
  drop
  local.get $lit
  call $hydra.haskell.serde.literal_to_expr
  br $end_pattern
)
  local.get $v
  drop
  local.get $name
  call $hydra.haskell.serde.name_to_expr
  br $end_pattern
)
  local.get $v
  drop
  local.get $pat'
  call $hydra.haskell.serde.pattern_to_expr
  call $hydra.serialization.parenthesize
  br $end_pattern
)
  local.get $v
  drop
  i32.const 0
  i32.const 0
  local.get $pats
  call $hydra.lib.lists.map
  call $hydra.serialization.paren_list
  br $end_pattern
)
  local.get $v
  drop
  i32.const 1179
  call $hydra.serialization.cst
  br $end_pattern
)
)
  (func $hydra.haskell.serde.right_hand_side_to_expr (param $rhs i32) (result i32)
  local.get $rhs
  drop
  i32.const 0
  drop
  i32.const 0
  call $hydra.haskell.serde.expression_to_expr
)
  (func $hydra.haskell.serde.statement_to_expr (param $stmt i32) (result i32)
  local.get $stmt
  drop
  i32.const 0
  drop
  i32.const 0
  call $hydra.haskell.serde.expression_to_expr
)
  (func $hydra.haskell.serde.to_haskell_comments (param $c i32) (result i32)
  (local $s i32)
  i32.const 1028
  i32.const 1094
  local.get $s
  call $hydra.lib.strings.cat2
  local.get $c
  call $hydra.lib.strings.lines
  call $hydra.lib.lists.map
  call $hydra.lib.strings.intercalate
)
  (func $hydra.haskell.serde.to_simple_comments (param $c i32) (result i32)
  (local $s i32)
  i32.const 1028
  i32.const 1087
  local.get $s
  call $hydra.lib.strings.cat2
  local.get $c
  call $hydra.lib.strings.lines
  call $hydra.lib.lists.map
  call $hydra.lib.strings.intercalate
)
  (func $hydra.haskell.serde.type_signature_to_expr (param $type_sig i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $inline_sig i32)
  (local $name i32)
  (local $name_expr i32)
  (local $typ i32)
  (local $type_expr i32)
  local.get $type_sig
  i32.load
  local.set $name
  local.get $type_sig
  i32.load offset=4
  local.set $typ
  local.get $name
  call $hydra.haskell.serde.name_to_expr
  local.set $name_expr
  local.get $typ
  call $hydra.haskell.serde.type_to_expr
  local.set $type_expr
  local.get $name_expr
  i32.const 1139
  call $hydra.serialization.cst
  local.get $type_expr
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
  call $hydra.serialization.structural_space_sep
  local.set $inline_sig
  local.get $inline_sig
  call $hydra.serialization.expression_length
  i32.const 120
  call $hydra.lib.equality.gt
  local.get $name_expr
  i32.const 1139
  call $hydra.serialization.cst
  i32.const 12
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 2
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=8
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.get $__rec_ptr
  call $hydra.serialization.space_sep
  local.get $type_expr
  call $hydra.serialization.tab_indent
  i32.const 12
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 2
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=8
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.get $__rec_ptr
  call $hydra.serialization.newline_sep
  local.get $inline_sig
  call $hydra.lib.logic.if_else
)
  (func $hydra.haskell.serde.type_to_expr (param $htype i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $app_type i32)
  (local $cod i32)
  (local $ctx i32)
  (local $ctx_type i32)
  (local $dom i32)
  (local $fun_type i32)
  (local $htype' i32)
  (local $lhs i32)
  (local $name i32)
  (local $rhs i32)
  (local $typ i32)
  (local $types i32)
  (local $v i32)
  (block $end_type (result i32)
  (block $variable
  (block $tuple
  (block $list
  (block $function
  (block $ctx
  (block $application
  local.get $htype
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $application $ctx $function $list $tuple $variable $variable
)
  local.get $v
  drop
  local.get $app_type
  i32.load
  local.set $lhs
  local.get $app_type
  i32.load offset=4
  local.set $rhs
  i32.const 0
  local.get $lhs
  call $hydra.haskell.serde.type_to_expr
  local.get $rhs
  call $hydra.haskell.serde.type_to_expr
  call $hydra.serialization.ifx
  br $end_type
)
  local.get $v
  drop
  local.get $ctx_type
  i32.load
  local.set $ctx
  local.get $ctx_type
  i32.load offset=4
  local.set $typ
  i32.const 0
  local.get $ctx
  call $hydra.haskell.serde.assertion_to_expr
  local.get $typ
  call $hydra.haskell.serde.type_to_expr
  call $hydra.serialization.ifx
  br $end_type
)
  local.get $v
  drop
  local.get $fun_type
  i32.load
  local.set $dom
  local.get $fun_type
  i32.load offset=4
  local.set $cod
  i32.const 0
  local.get $dom
  call $hydra.haskell.serde.type_to_expr
  local.get $cod
  call $hydra.haskell.serde.type_to_expr
  call $hydra.serialization.ifx
  br $end_type
)
  local.get $v
  drop
  i32.const 0
  local.get $htype'
  call $hydra.haskell.serde.type_to_expr
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
  call $hydra.serialization.bracket_list
  br $end_type
)
  local.get $v
  drop
  i32.const 0
  i32.const 0
  local.get $types
  call $hydra.lib.lists.map
  call $hydra.serialization.paren_list
  br $end_type
)
  local.get $v
  drop
  local.get $name
  call $hydra.haskell.serde.name_to_expr
  br $end_type
)
)
  (func $hydra.haskell.serde.value_binding_to_expr (param $vb i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $bindings i32)
  (local $body i32)
  (local $inline_body i32)
  (local $lhs_expr i32)
  (local $local i32)
  (local $local_bindings i32)
  (local $pat i32)
  (local $rhs i32)
  (local $rhs_expr i32)
  (local $simple_v_b i32)
  (local $v i32)
  (block $end_value_binding (result i32)
  (block $simple
  local.get $vb
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $simple $simple
)
  local.get $v
  drop
  local.get $simple_v_b
  i32.load
  local.set $pat
  local.get $simple_v_b
  i32.load offset=4
  local.set $rhs
  local.get $simple_v_b
  i32.load offset=8
  local.set $local
  local.get $pat
  call $hydra.haskell.serde.pattern_to_expr
  local.set $lhs_expr
  local.get $rhs
  call $hydra.haskell.serde.right_hand_side_to_expr
  local.set $rhs_expr
  local.get $lhs_expr
  i32.const 1145
  call $hydra.serialization.cst
  local.get $rhs_expr
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
  call $hydra.serialization.structural_space_sep
  local.set $inline_body
  local.get $inline_body
  call $hydra.serialization.expression_length
  i32.const 120
  call $hydra.lib.equality.gt
  local.get $lhs_expr
  i32.const 1145
  call $hydra.serialization.cst
  i32.const 12
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 2
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=8
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.get $__rec_ptr
  call $hydra.serialization.space_sep
  local.get $rhs_expr
  call $hydra.serialization.tab_indent
  i32.const 12
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 2
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=8
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.get $__rec_ptr
  call $hydra.serialization.newline_sep
  local.get $inline_body
  call $hydra.lib.logic.if_else
  local.set $body
  local.get $body
  local.get $local_bindings
  drop
  i32.const 0
  drop
  i32.const 0
  local.set $bindings
  local.get $body
  i32.const 1329
  call $hydra.serialization.cst
  i32.const 0
  local.get $bindings
  call $hydra.lib.lists.map
  call $hydra.lib.lists.cons
  call $hydra.serialization.indent_block
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
  call $hydra.lib.lists.cons
  call $hydra.serialization.indent_block
  local.get $local
  call $hydra.lib.maybes.maybe
  br $end_value_binding
)
)
  (func $hydra.haskell.serde.variable_to_expr (param $variable i32) (result i32)
  local.get $variable
  drop
  i32.const 0
  drop
  i32.const 0
  call $hydra.haskell.serde.name_to_expr
)
  (func $hydra.haskell.serde.write_qualified_name (param $qname i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $all_parts i32)
  (local $h i32)
  (local $name_part i32)
  (local $qualifiers i32)
  (local $unqual i32)
  local.get $qname
  i32.load
  local.set $qualifiers
  local.get $qname
  i32.load offset=4
  local.set $unqual
  local.get $name_part
  drop
  i32.const 0
  drop
  i32.const 0
  local.set $h
  local.get $h
  local.get $qualifiers
  call $hydra.lib.lists.map
  local.get $unqual
  drop
  local.get $h
  drop
  i32.const 0
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
  call $hydra.lib.lists.concat2
  local.set $all_parts
  i32.const 1122
  local.get $all_parts
  call $hydra.lib.strings.intercalate
)
)
