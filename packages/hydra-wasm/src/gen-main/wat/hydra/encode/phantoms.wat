(module
  (import "hydra.encode.core" "hydra.encode.core.name" (func $hydra.encode.core.name (param i32) (result i32) ) )
  (import "hydra.encode.core" "hydra.encode.core.term" (func $hydra.encode.core.term (param i32) (result i32) ) )
  (memory $memory 1 )
  (export "memory" (memory $memory) )
  (export "hydra.encode.phantoms.t_binding" (func $hydra.encode.phantoms.t_binding) )
  (export "hydra.encode.phantoms.t_term" (func $hydra.encode.phantoms.t_term) )
  (export "hydra.encode.phantoms.t_term_definition" (func $hydra.encode.phantoms.t_term_definition) )
  (func $hydra.encode.phantoms.t_binding (param $a i32) (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.phantoms.TBinding"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "name"
  local.get $x
  ;; project field: name
  call $hydra.encode.core.name
  i32.const 0 ;; string: "term"
  local.get $a
  local.get $x
  ;; project field: term
  call $hydra.encode.phantoms.t_term
)
  (func $hydra.encode.phantoms.t_term (param $a i32) (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.phantoms.TTerm"
  nop
  call $hydra.encode.core.term
)
  (func $hydra.encode.phantoms.t_term_definition (param $a i32) (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.phantoms.TTermDefinition"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "name"
  local.get $x
  ;; project field: name
  call $hydra.encode.core.name
  i32.const 0 ;; string: "term"
  local.get $a
  local.get $x
  ;; project field: term
  call $hydra.encode.phantoms.t_term
)
)
