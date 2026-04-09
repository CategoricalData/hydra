(module
  (import "hydra.extract.core" "hydra.extract.core.decode_list" (func $hydra.extract.core.decode_list (param i32) (result i32) ) )
  (import "hydra.extract.core" "hydra.extract.core.decode_map" (func $hydra.extract.core.decode_map (param i32) (result i32) ) )
  (import "hydra.extract.core" "hydra.extract.core.decode_set" (func $hydra.extract.core.decode_set (param i32) (result i32) ) )
  (import "hydra.extract.core" "hydra.extract.core.require_field" (func $hydra.extract.core.require_field (param i32) (result i32) ) )
  (import "hydra.extract.core" "hydra.extract.core.to_field_map" (func $hydra.extract.core.to_field_map (param i32) (result i32) ) )
  (import "hydra.lexical" "hydra.lexical.strip_and_dereference_term_either" (func $hydra.lexical.strip_and_dereference_term_either (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.bind" (func $hydra.lib.eithers.bind (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.either" (func $hydra.lib.eithers.either (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map" (func $hydra.lib.eithers.map (param i32) (result i32) ) )
  (memory $memory 1 )
  (export "memory" (memory $memory) )
  (export "hydra.decode.relational.column_name" (func $hydra.decode.relational.column_name) )
  (export "hydra.decode.relational.column_schema" (func $hydra.decode.relational.column_schema) )
  (export "hydra.decode.relational.foreign_key" (func $hydra.decode.relational.foreign_key) )
  (export "hydra.decode.relational.primary_key" (func $hydra.decode.relational.primary_key) )
  (export "hydra.decode.relational.relation" (func $hydra.decode.relational.relation) )
  (export "hydra.decode.relational.relation_name" (func $hydra.decode.relational.relation_name) )
  (export "hydra.decode.relational.relation_schema" (func $hydra.decode.relational.relation_schema) )
  (export "hydra.decode.relational.relationship" (func $hydra.decode.relational.relationship) )
  (export "hydra.decode.relational.row" (func $hydra.decode.relational.row) )
  (func $hydra.decode.relational.column_name (param $cx i32) (param $raw i32) (result i32)
  (local $b i32)
  (local $cx2 i32)
  (local $err i32)
  (local $raw2 i32)
  (local $s i32)
  (local $stripped i32)
  (local $stripped2 i32)
  (local $v i32)
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
  call $hydra.lib.eithers.map
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.relational.column_schema (param $t i32) (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_domain i32)
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
  call $hydra.decode.relational.column_name
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "domain"
  local.get $t
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_name
  local.get $field_domain
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.relational.foreign_key (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_foreign_relation i32)
  (local $field_keys i32)
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
  i32.const 0 ;; string: "foreignRelation"
  call $hydra.decode.relational.relation_name
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "keys"
  call $hydra.decode.relational.column_name
  call $hydra.decode.relational.column_name
  call $hydra.extract.core.decode_map
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_foreign_relation
  local.get $field_keys
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.relational.primary_key (param $cx i32) (param $raw i32) (result i32)
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
  call $hydra.decode.relational.column_name
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
  (func $hydra.decode.relational.relation (param $v i32) (param $cx i32) (param $raw i32) (result i32)
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
  call $hydra.decode.relational.row
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
  (func $hydra.decode.relational.relation_name (param $cx i32) (param $raw i32) (result i32)
  (local $b i32)
  (local $cx2 i32)
  (local $err i32)
  (local $raw2 i32)
  (local $s i32)
  (local $stripped i32)
  (local $stripped2 i32)
  (local $v i32)
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
  call $hydra.lib.eithers.map
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.relational.relation_schema (param $t i32) (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_columns i32)
  (local $field_foreign_keys i32)
  (local $field_map i32)
  (local $field_name i32)
  (local $field_primary_keys i32)
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
  local.get $t
  call $hydra.decode.relational.column_schema
  call $hydra.extract.core.decode_list
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "primaryKeys"
  call $hydra.decode.relational.primary_key
  call $hydra.extract.core.decode_list
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "foreignKeys"
  call $hydra.decode.relational.foreign_key
  call $hydra.extract.core.decode_list
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_name
  local.get $field_columns
  local.get $field_primary_keys
  local.get $field_foreign_keys
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.relational.relationship (param $v i32) (param $cx i32) (param $raw i32) (result i32)
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
  call $hydra.decode.relational.column_name
  local.get $v
  call $hydra.extract.core.decode_map
  local.get $cx
  local.get $wrapped_term
  ;; project field: body
  call $hydra.extract.core.decode_set
  call $hydra.lib.eithers.map
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.relational.row (param $v i32) (param $cx i32) (param $raw i32) (result i32)
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
)
