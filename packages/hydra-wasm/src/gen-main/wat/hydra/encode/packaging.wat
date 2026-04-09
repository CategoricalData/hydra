(module
  (import "hydra.encode.core" "hydra.encode.core.name" (func $hydra.encode.core.name (param i32) (result i32) ) )
  (import "hydra.encode.core" "hydra.encode.core.term" (func $hydra.encode.core.term (param i32) (result i32) ) )
  (import "hydra.encode.core" "hydra.encode.core.type_scheme" (func $hydra.encode.core.type_scheme (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.bimap" (func $hydra.lib.maps.bimap (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.map" (func $hydra.lib.maybes.map (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.bimap" (func $hydra.lib.pairs.bimap (param i32) (result i32) ) )
  (memory $memory 1 )
  (export "memory" (memory $memory) )
  (export "hydra.encode.packaging.definition" (func $hydra.encode.packaging.definition) )
  (export "hydra.encode.packaging.file_extension" (func $hydra.encode.packaging.file_extension) )
  (export "hydra.encode.packaging.module" (func $hydra.encode.packaging.module) )
  (export "hydra.encode.packaging.namespace" (func $hydra.encode.packaging.namespace) )
  (export "hydra.encode.packaging.namespaces" (func $hydra.encode.packaging.namespaces) )
  (export "hydra.encode.packaging.package" (func $hydra.encode.packaging.package) )
  (export "hydra.encode.packaging.package_name" (func $hydra.encode.packaging.package_name) )
  (export "hydra.encode.packaging.qualified_name" (func $hydra.encode.packaging.qualified_name) )
  (export "hydra.encode.packaging.term_definition" (func $hydra.encode.packaging.term_definition) )
  (export "hydra.encode.packaging.type_definition" (func $hydra.encode.packaging.type_definition) )
  (func $hydra.encode.packaging.definition (param $arg_0 i32) (result i32)
  (local $y i32)
  (block $end_definition (result i32)
  (block $type
  (block $term
  local.get $arg_0
  br_table $term $type $type
)
  i32.const 0 ;; string: "hydra.packaging.Definition"
  i32.const 0 ;; string: "term"
  local.get $y
  call $hydra.encode.packaging.term_definition
  br $end_definition
)
  i32.const 0 ;; string: "hydra.packaging.Definition"
  i32.const 0 ;; string: "type"
  local.get $y
  call $hydra.encode.packaging.type_definition
  br $end_definition
)
)
  (func $hydra.encode.packaging.file_extension (param $x i32) (result i32)
  (local $x2 i32)
  i32.const 0 ;; string: "hydra.packaging.FileExtension"
  local.get $x2
)
  (func $hydra.encode.packaging.module (param $x i32) (result i32)
  (local $opt i32)
  (local $x2 i32)
  (local $xs i32)
  i32.const 0 ;; string: "hydra.packaging.Module"
  i32.const 5
  ;; list elements follow
  i32.const 0 ;; string: "namespace"
  local.get $x
  ;; project field: namespace
  call $hydra.encode.packaging.namespace
  i32.const 0 ;; string: "definitions"
  call $hydra.encode.packaging.definition
  local.get $xs
  call $hydra.lib.lists.map
  i32.const 0 ;; string: "termDependencies"
  call $hydra.encode.packaging.namespace
  local.get $xs
  call $hydra.lib.lists.map
  i32.const 0 ;; string: "typeDependencies"
  call $hydra.encode.packaging.namespace
  local.get $xs
  call $hydra.lib.lists.map
  i32.const 0 ;; string: "description"
  local.get $x2
  local.get $opt
  call $hydra.lib.maybes.map
)
  (func $hydra.encode.packaging.namespace (param $x i32) (result i32)
  (local $x2 i32)
  i32.const 0 ;; string: "hydra.packaging.Namespace"
  local.get $x2
)
  (func $hydra.encode.packaging.namespaces (param $n i32) (param $x i32) (result i32)
  (local $m i32)
  (local $p i32)
  i32.const 0 ;; string: "hydra.packaging.Namespaces"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "focus"
  call $hydra.encode.packaging.namespace
  local.get $n
  local.get $p
  call $hydra.lib.pairs.bimap
  i32.const 0 ;; string: "mapping"
  call $hydra.encode.packaging.namespace
  local.get $n
  local.get $m
  call $hydra.lib.maps.bimap
)
  (func $hydra.encode.packaging.package (param $x i32) (result i32)
  (local $opt i32)
  (local $x2 i32)
  (local $xs i32)
  i32.const 0 ;; string: "hydra.packaging.Package"
  i32.const 4
  ;; list elements follow
  i32.const 0 ;; string: "name"
  local.get $x
  ;; project field: name
  call $hydra.encode.packaging.package_name
  i32.const 0 ;; string: "modules"
  call $hydra.encode.packaging.module
  local.get $xs
  call $hydra.lib.lists.map
  i32.const 0 ;; string: "dependencies"
  call $hydra.encode.packaging.package_name
  local.get $xs
  call $hydra.lib.lists.map
  i32.const 0 ;; string: "description"
  local.get $x2
  local.get $opt
  call $hydra.lib.maybes.map
)
  (func $hydra.encode.packaging.package_name (param $x i32) (result i32)
  (local $x2 i32)
  i32.const 0 ;; string: "hydra.packaging.PackageName"
  local.get $x2
)
  (func $hydra.encode.packaging.qualified_name (param $x i32) (result i32)
  (local $opt i32)
  (local $x2 i32)
  i32.const 0 ;; string: "hydra.packaging.QualifiedName"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "namespace"
  call $hydra.encode.packaging.namespace
  local.get $opt
  call $hydra.lib.maybes.map
  i32.const 0 ;; string: "local"
  local.get $x2
)
  (func $hydra.encode.packaging.term_definition (param $x i32) (result i32)
  (local $opt i32)
  i32.const 0 ;; string: "hydra.packaging.TermDefinition"
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "name"
  local.get $x
  ;; project field: name
  call $hydra.encode.core.name
  i32.const 0 ;; string: "term"
  local.get $x
  ;; project field: term
  call $hydra.encode.core.term
  i32.const 0 ;; string: "type"
  call $hydra.encode.core.type_scheme
  local.get $opt
  call $hydra.lib.maybes.map
)
  (func $hydra.encode.packaging.type_definition (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.packaging.TypeDefinition"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "name"
  local.get $x
  ;; project field: name
  call $hydra.encode.core.name
  i32.const 0 ;; string: "type"
  local.get $x
  ;; project field: type
  call $hydra.encode.core.type_scheme
)
)
