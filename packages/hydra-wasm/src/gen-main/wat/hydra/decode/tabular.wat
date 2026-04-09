(module
  (import "hydra.decode.core" "hydra.decode.core.type" (func $hydra.decode.core.type (param i32) (result i32) ) )
  (import "hydra.decode.relational" "hydra.decode.relational.column_name" (func $hydra.decode.relational.column_name (param i32) (result i32) ) )
  (import "hydra.decode.relational" "hydra.decode.relational.relation_name" (func $hydra.decode.relational.relation_name (param i32) (result i32) ) )
  (import "hydra.extract.core" "hydra.extract.core.decode_list" (func $hydra.extract.core.decode_list (param i32) (result i32) ) )
  (import "hydra.extract.core" "hydra.extract.core.decode_maybe" (func $hydra.extract.core.decode_maybe (param i32) (result i32) ) )
  (import "hydra.extract.core" "hydra.extract.core.require_field" (func $hydra.extract.core.require_field (param i32) (result i32) ) )
  (import "hydra.extract.core" "hydra.extract.core.to_field_map" (func $hydra.extract.core.to_field_map (param i32) (result i32) ) )
  (import "hydra.lexical" "hydra.lexical.strip_and_dereference_term_either" (func $hydra.lexical.strip_and_dereference_term_either (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.bind" (func $hydra.lib.eithers.bind (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.either" (func $hydra.lib.eithers.either (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map" (func $hydra.lib.eithers.map (param i32) (result i32) ) )
  (memory $memory 1 )
  (export "memory" (memory $memory) )
  (export "hydra.decode.tabular.column_type" (func $hydra.decode.tabular.column_type) )
  (export "hydra.decode.tabular.data_row" (func $hydra.decode.tabular.data_row) )
  (export "hydra.decode.tabular.header_row" (func $hydra.decode.tabular.header_row) )
  (export "hydra.decode.tabular.table" (func $hydra.decode.tabular.table) )
  (export "hydra.decode.tabular.table_type" (func $hydra.decode.tabular.table_type) )
  (func $hydra.decode.tabular.column_type (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_map i32)
  (local $field_name i32)
  (local $field_type i32)
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
  i32.const 0 ;; string: "name"
  call $hydra.decode.relational.column_name
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "type"
  call $hydra.decode.core.type
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_name
  local.get $field_type
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.tabular.data_row (param $v i32) (param $cx i32) (param $raw i32) (result i32)
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
  local.get $v
  call $hydra.extract.core.decode_maybe
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
  (func $hydra.decode.tabular.header_row (param $cx i32) (param $raw i32) (result i32)
  (local $b i32)
  (local $cx2 i32)
  (local $err i32)
  (local $raw2 i32)
  (local $s i32)
  (local $stripped i32)
  (local $stripped2 i32)
  (local $v i32)
  (local $wrapped_term i32)
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $wrap
  local.get $stripped
  br_table $wrap $wrap
)
  local.get $b
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
  (func $hydra.decode.tabular.table (param $v i32) (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_data i32)
  (local $field_header i32)
  (local $field_map i32)
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
  i32.const 0 ;; string: "header"
  call $hydra.decode.tabular.header_row
  call $hydra.extract.core.decode_maybe
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "data"
  local.get $v
  call $hydra.decode.tabular.data_row
  call $hydra.extract.core.decode_list
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_header
  local.get $field_data
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.tabular.table_type (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_columns i32)
  (local $field_map i32)
  (local $field_name i32)
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
  i32.const 0 ;; string: "name"
  call $hydra.decode.relational.relation_name
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "columns"
  call $hydra.decode.tabular.column_type
  call $hydra.extract.core.decode_list
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_name
  local.get $field_columns
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
)
