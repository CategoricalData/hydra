(module
  (memory $memory 2 )
  (export "memory" (memory $memory) )
  (data (offset i32.const 1024 ) "\0a\00\00\00\50\61\73\63\61\6c\43\61\73\65\10\00\00\00\55\50\50\45\52\5f\53\4e\41\4b\45\5f\43\41\53\45\09\00\00\00\63\61\6d\65\6c\43\61\73\65\07\00\00\00\65\71\75\61\6c\54\6f\0b\00\00\00\67\72\65\61\74\65\72\54\68\61\6e\08\00\00\00\6c\65\73\73\54\68\61\6e\10\00\00\00\6c\6f\77\65\72\5f\73\6e\61\6b\65\5f\63\61\73\65")
  (global $__bump_ptr (mut i32) i32.const 1136 )
  (export "__bump_ptr" (global $__bump_ptr) )
  (func $__alloc (param $sz i32) (result i32)
  global.get $__bump_ptr
  global.get $__bump_ptr
  local.get $sz
  i32.add
  global.set $__bump_ptr
)
  (export "hydra.show.util.case_convention" (func $hydra.show.util.case_convention) )
  (export "hydra.show.util.comparison" (func $hydra.show.util.comparison) )
  (func $hydra.show.util.case_convention (param $c i32) (result i32)
  (local $__rec_ptr i32)
  (local $v i32)
  (block $end_case_convention (result i32)
  (block $pascal
  (block $camel
  (block $upper_snake
  (block $lower_snake
  local.get $c
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $lower_snake $upper_snake $camel $pascal $pascal
)
  local.get $v
  drop
  i32.const 1109
  br $end_case_convention
)
  local.get $v
  drop
  i32.const 1038
  br $end_case_convention
)
  local.get $v
  drop
  i32.const 1058
  br $end_case_convention
)
  local.get $v
  drop
  i32.const 1024
  br $end_case_convention
)
)
  (func $hydra.show.util.comparison (param $c i32) (result i32)
  (local $__rec_ptr i32)
  (local $v i32)
  (block $end_comparison (result i32)
  (block $greater_than
  (block $equal_to
  (block $less_than
  local.get $c
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $less_than $equal_to $greater_than $greater_than
)
  local.get $v
  drop
  i32.const 1097
  br $end_comparison
)
  local.get $v
  drop
  i32.const 1071
  br $end_comparison
)
  local.get $v
  drop
  i32.const 1082
  br $end_comparison
)
)
)
