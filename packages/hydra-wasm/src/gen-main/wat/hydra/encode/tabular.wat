(module
  (import "hydra.encode.core" "hydra.encode.core.type" (func $hydra.encode.core.type (param i32) (result i32) ) )
  (import "hydra.encode.relational" "hydra.encode.relational.column_name" (func $hydra.encode.relational.column_name (param i32) (result i32) ) )
  (import "hydra.encode.relational" "hydra.encode.relational.relation_name" (func $hydra.encode.relational.relation_name (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.map" (func $hydra.lib.maybes.map (param i32) (result i32) ) )
  (memory $memory 1 )
  (export "memory" (memory $memory) )
  (export "hydra.encode.tabular.column_type" (func $hydra.encode.tabular.column_type) )
  (export "hydra.encode.tabular.data_row" (func $hydra.encode.tabular.data_row) )
  (export "hydra.encode.tabular.header_row" (func $hydra.encode.tabular.header_row) )
  (export "hydra.encode.tabular.table" (func $hydra.encode.tabular.table) )
  (export "hydra.encode.tabular.table_type" (func $hydra.encode.tabular.table_type) )
  (func $hydra.encode.tabular.column_type (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.tabular.ColumnType"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "name"
  local.get $x
  ;; project field: name
  call $hydra.encode.relational.column_name
  i32.const 0 ;; string: "type"
  local.get $x
  ;; project field: type
  call $hydra.encode.core.type
)
  (func $hydra.encode.tabular.data_row (param $v i32) (param $x i32) (result i32)
  (local $opt i32)
  (local $xs i32)
  i32.const 0 ;; string: "hydra.tabular.DataRow"
  local.get $v
  local.get $opt
  call $hydra.lib.maybes.map
  local.get $xs
  call $hydra.lib.lists.map
)
  (func $hydra.encode.tabular.header_row (param $x i32) (result i32)
  (local $x2 i32)
  (local $xs i32)
  i32.const 0 ;; string: "hydra.tabular.HeaderRow"
  local.get $x2
  local.get $xs
  call $hydra.lib.lists.map
)
  (func $hydra.encode.tabular.table (param $v i32) (param $x i32) (result i32)
  (local $opt i32)
  (local $xs i32)
  i32.const 0 ;; string: "hydra.tabular.Table"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "header"
  call $hydra.encode.tabular.header_row
  local.get $opt
  call $hydra.lib.maybes.map
  i32.const 0 ;; string: "data"
  local.get $v
  call $hydra.encode.tabular.data_row
  local.get $xs
  call $hydra.lib.lists.map
)
  (func $hydra.encode.tabular.table_type (param $x i32) (result i32)
  (local $xs i32)
  i32.const 0 ;; string: "hydra.tabular.TableType"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "name"
  local.get $x
  ;; project field: name
  call $hydra.encode.relational.relation_name
  i32.const 0 ;; string: "columns"
  call $hydra.encode.tabular.column_type
  local.get $xs
  call $hydra.lib.lists.map
)
)
