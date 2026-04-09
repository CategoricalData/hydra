(module
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.bimap" (func $hydra.lib.maps.bimap (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.map" (func $hydra.lib.sets.map (param i32) (result i32) ) )
  (memory $memory 1 )
  (export "memory" (memory $memory) )
  (export "hydra.encode.relational.column_name" (func $hydra.encode.relational.column_name) )
  (export "hydra.encode.relational.column_schema" (func $hydra.encode.relational.column_schema) )
  (export "hydra.encode.relational.foreign_key" (func $hydra.encode.relational.foreign_key) )
  (export "hydra.encode.relational.primary_key" (func $hydra.encode.relational.primary_key) )
  (export "hydra.encode.relational.relation" (func $hydra.encode.relational.relation) )
  (export "hydra.encode.relational.relation_name" (func $hydra.encode.relational.relation_name) )
  (export "hydra.encode.relational.relation_schema" (func $hydra.encode.relational.relation_schema) )
  (export "hydra.encode.relational.relationship" (func $hydra.encode.relational.relationship) )
  (export "hydra.encode.relational.row" (func $hydra.encode.relational.row) )
  (func $hydra.encode.relational.column_name (param $x i32) (result i32)
  (local $x2 i32)
  i32.const 0 ;; string: "hydra.relational.ColumnName"
  local.get $x2
)
  (func $hydra.encode.relational.column_schema (param $t i32) (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.relational.ColumnSchema"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "name"
  local.get $x
  ;; project field: name
  call $hydra.encode.relational.column_name
  i32.const 0 ;; string: "domain"
  local.get $x
  ;; project field: domain
  local.get $t
)
  (func $hydra.encode.relational.foreign_key (param $x i32) (result i32)
  (local $m i32)
  i32.const 0 ;; string: "hydra.relational.ForeignKey"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "foreignRelation"
  local.get $x
  ;; project field: foreign_relation
  call $hydra.encode.relational.relation_name
  i32.const 0 ;; string: "keys"
  call $hydra.encode.relational.column_name
  call $hydra.encode.relational.column_name
  local.get $m
  call $hydra.lib.maps.bimap
)
  (func $hydra.encode.relational.primary_key (param $x i32) (result i32)
  (local $xs i32)
  i32.const 0 ;; string: "hydra.relational.PrimaryKey"
  call $hydra.encode.relational.column_name
  local.get $xs
  call $hydra.lib.lists.map
)
  (func $hydra.encode.relational.relation (param $v i32) (param $x i32) (result i32)
  (local $xs i32)
  i32.const 0 ;; string: "hydra.relational.Relation"
  local.get $v
  call $hydra.encode.relational.row
  local.get $xs
  call $hydra.lib.lists.map
)
  (func $hydra.encode.relational.relation_name (param $x i32) (result i32)
  (local $x2 i32)
  i32.const 0 ;; string: "hydra.relational.RelationName"
  local.get $x2
)
  (func $hydra.encode.relational.relation_schema (param $t i32) (param $x i32) (result i32)
  (local $xs i32)
  i32.const 0 ;; string: "hydra.relational.RelationSchema"
  i32.const 4
  ;; list elements follow
  i32.const 0 ;; string: "name"
  local.get $x
  ;; project field: name
  call $hydra.encode.relational.relation_name
  i32.const 0 ;; string: "columns"
  local.get $t
  call $hydra.encode.relational.column_schema
  local.get $xs
  call $hydra.lib.lists.map
  i32.const 0 ;; string: "primaryKeys"
  call $hydra.encode.relational.primary_key
  local.get $xs
  call $hydra.lib.lists.map
  i32.const 0 ;; string: "foreignKeys"
  call $hydra.encode.relational.foreign_key
  local.get $xs
  call $hydra.lib.lists.map
)
  (func $hydra.encode.relational.relationship (param $v i32) (param $x i32) (result i32)
  (local $m i32)
  (local $s i32)
  i32.const 0 ;; string: "hydra.relational.Relationship"
  call $hydra.encode.relational.column_name
  local.get $v
  local.get $m
  call $hydra.lib.maps.bimap
  local.get $s
  call $hydra.lib.sets.map
)
  (func $hydra.encode.relational.row (param $v i32) (param $x i32) (result i32)
  (local $xs i32)
  i32.const 0 ;; string: "hydra.relational.Row"
  local.get $v
  local.get $xs
  call $hydra.lib.lists.map
)
)
