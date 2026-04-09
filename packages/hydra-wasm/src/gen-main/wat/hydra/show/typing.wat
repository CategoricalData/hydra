(module
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.to_list" (func $hydra.lib.maps.to_list (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.second" (func $hydra.lib.pairs.second (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat" (func $hydra.lib.strings.cat (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.intercalate" (func $hydra.lib.strings.intercalate (param i32) (result i32) ) )
  (import "hydra.show.core" "hydra.show.core.type" (func $hydra.show.core.type (param i32) (result i32) ) )
  (memory $memory 1 )
  (export "memory" (memory $memory) )
  (export "hydra.show.typing.type_constraint" (func $hydra.show.typing.type_constraint) )
  (export "hydra.show.typing.type_subst" (func $hydra.show.typing.type_subst) )
  (func $hydra.show.typing.type_constraint (param $tc i32) (result i32)
  (local $ltyp i32)
  (local $rtyp i32)
  local.get $tc
  ;; project field: left
  local.set $ltyp
  local.get $tc
  ;; project field: right
  local.set $rtyp
  i32.const 3
  ;; list elements follow
  local.get $ltyp
  call $hydra.show.core.type
  i32.const 0 ;; string: "≡"
  local.get $rtyp
  call $hydra.show.core.type
  call $hydra.lib.strings.cat
)
  (func $hydra.show.typing.type_subst (param $ts i32) (result i32)
  (local $name i32)
  (local $pair i32)
  (local $pair_strs i32)
  (local $pairs i32)
  (local $show_pair i32)
  (local $subst i32)
  (local $typ i32)
  nop
  local.set $subst
  local.get $subst
  call $hydra.lib.maps.to_list
  local.set $pairs
  nop
  local.set $name
  local.get $pair
  call $hydra.lib.pairs.second
  local.set $typ
  i32.const 3
  ;; list elements follow
  local.get $name
  i32.const 0 ;; string: "↦"
  local.get $typ
  call $hydra.show.core.type
  call $hydra.lib.strings.cat
  local.set $show_pair
  local.get $show_pair
  local.get $pairs
  call $hydra.lib.lists.map
  local.set $pair_strs
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "{"
  i32.const 0 ;; string: ","
  local.get $pair_strs
  call $hydra.lib.strings.intercalate
  i32.const 0 ;; string: "}"
  call $hydra.lib.strings.cat
)
)
