(module
  (import "hydra.lib.math" "hydra.lib.math.negate" (func $hydra.lib.math.negate (param i32) (result i32) ) )
  (import "hydra.serialization" "hydra.serialization.op" (func $hydra.serialization.op (param i32) (param i32) (param i32) (result i32) ) )
  (memory $memory 2 )
  (export "memory" (memory $memory) )
  (data (offset i32.const 1024 ) "\00\00\00\00\02\00\00\00\21\21\01\00\00\00\24\02\00\00\00\26\26\01\00\00\00\2a\01\00\00\00\2b\02\00\00\00\2b\2b\01\00\00\00\2d\02\00\00\00\2d\3e\01\00\00\00\2e\01\00\00\00\2f\02\00\00\00\2f\3d\01\00\00\00\3a\02\00\00\00\3a\3a\01\00\00\00\3c\03\00\00\00\3c\24\3e\03\00\00\00\3c\2a\3e\02\00\00\00\3c\3e\01\00\00\00\3d\02\00\00\00\3d\3d\02\00\00\00\3d\3e\01\00\00\00\3e\02\00\00\00\3e\3d\03\00\00\00\3e\3e\3d\05\00\00\00\60\64\69\76\60\06\00\00\00\60\65\6c\65\6d\60\05\00\00\00\60\6d\6f\64\60\09\00\00\00\60\6e\6f\74\45\6c\65\6d\60\06\00\00\00\60\71\75\6f\74\60\05\00\00\00\60\72\65\6d\60\02\00\00\00\7c\7c")
  (global $__bump_ptr (mut i32) i32.const 1232 )
  (export "__bump_ptr" (global $__bump_ptr) )
  (func $__alloc (param $sz i32) (result i32)
  global.get $__bump_ptr
  global.get $__bump_ptr
  local.get $sz
  i32.add
  global.set $__bump_ptr
)
  (export "hydra.haskell.operators.and_op" (func $hydra.haskell.operators.and_op) )
  (export "hydra.haskell.operators.ap_op" (func $hydra.haskell.operators.ap_op) )
  (export "hydra.haskell.operators.app_op" (func $hydra.haskell.operators.app_op) )
  (export "hydra.haskell.operators.apply_op" (func $hydra.haskell.operators.apply_op) )
  (export "hydra.haskell.operators.arrow_op" (func $hydra.haskell.operators.arrow_op) )
  (export "hydra.haskell.operators.assert_op" (func $hydra.haskell.operators.assert_op) )
  (export "hydra.haskell.operators.bind_op" (func $hydra.haskell.operators.bind_op) )
  (export "hydra.haskell.operators.case_op" (func $hydra.haskell.operators.case_op) )
  (export "hydra.haskell.operators.compose_op" (func $hydra.haskell.operators.compose_op) )
  (export "hydra.haskell.operators.concat_op" (func $hydra.haskell.operators.concat_op) )
  (export "hydra.haskell.operators.cons_op" (func $hydra.haskell.operators.cons_op) )
  (export "hydra.haskell.operators.define_op" (func $hydra.haskell.operators.define_op) )
  (export "hydra.haskell.operators.diamond_op" (func $hydra.haskell.operators.diamond_op) )
  (export "hydra.haskell.operators.div_op" (func $hydra.haskell.operators.div_op) )
  (export "hydra.haskell.operators.divide_op" (func $hydra.haskell.operators.divide_op) )
  (export "hydra.haskell.operators.elem_op" (func $hydra.haskell.operators.elem_op) )
  (export "hydra.haskell.operators.equal_op" (func $hydra.haskell.operators.equal_op) )
  (export "hydra.haskell.operators.fmap_op" (func $hydra.haskell.operators.fmap_op) )
  (export "hydra.haskell.operators.gt_op" (func $hydra.haskell.operators.gt_op) )
  (export "hydra.haskell.operators.gte_op" (func $hydra.haskell.operators.gte_op) )
  (export "hydra.haskell.operators.index_op" (func $hydra.haskell.operators.index_op) )
  (export "hydra.haskell.operators.lambda_op" (func $hydra.haskell.operators.lambda_op) )
  (export "hydra.haskell.operators.lt_op" (func $hydra.haskell.operators.lt_op) )
  (export "hydra.haskell.operators.lte_op" (func $hydra.haskell.operators.lte_op) )
  (export "hydra.haskell.operators.minus_op" (func $hydra.haskell.operators.minus_op) )
  (export "hydra.haskell.operators.mod_op" (func $hydra.haskell.operators.mod_op) )
  (export "hydra.haskell.operators.mult_op" (func $hydra.haskell.operators.mult_op) )
  (export "hydra.haskell.operators.neq_op" (func $hydra.haskell.operators.neq_op) )
  (export "hydra.haskell.operators.not_elem_op" (func $hydra.haskell.operators.not_elem_op) )
  (export "hydra.haskell.operators.or_op" (func $hydra.haskell.operators.or_op) )
  (export "hydra.haskell.operators.plus_op" (func $hydra.haskell.operators.plus_op) )
  (export "hydra.haskell.operators.quot_op" (func $hydra.haskell.operators.quot_op) )
  (export "hydra.haskell.operators.rem_op" (func $hydra.haskell.operators.rem_op) )
  (export "hydra.haskell.operators.type_op" (func $hydra.haskell.operators.type_op) )
  (func $hydra.haskell.operators.and_op (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 1039
  i32.const 3
  i32.const 2
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
  call $hydra.serialization.op
)
  (func $hydra.haskell.operators.ap_op (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 1111
  i32.const 4
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
  call $hydra.serialization.op
)
  (func $hydra.haskell.operators.app_op (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
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
)
  (func $hydra.haskell.operators.apply_op (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 1034
  i32.const 0
  i32.const 2
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
  call $hydra.serialization.op
)
  (func $hydra.haskell.operators.arrow_op (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 1066
  i32.const 1
  call $hydra.lib.math.negate
  i32.const 2
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
  call $hydra.serialization.op
)
  (func $hydra.haskell.operators.assert_op (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 1135
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
  call $hydra.serialization.op
)
  (func $hydra.haskell.operators.bind_op (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 1152
  i32.const 1
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
  call $hydra.serialization.op
)
  (func $hydra.haskell.operators.case_op (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 1066
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
  call $hydra.serialization.op
)
  (func $hydra.haskell.operators.compose_op (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 1072
  i32.const 9
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
  call $hydra.serialization.op
)
  (func $hydra.haskell.operators.concat_op (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 1055
  i32.const 5
  i32.const 2
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
  call $hydra.serialization.op
)
  (func $hydra.haskell.operators.cons_op (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 1088
  i32.const 5
  i32.const 2
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
  call $hydra.serialization.op
)
  (func $hydra.haskell.operators.define_op (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 1124
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
  call $hydra.serialization.op
)
  (func $hydra.haskell.operators.diamond_op (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 1118
  i32.const 6
  i32.const 2
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
  call $hydra.serialization.op
)
  (func $hydra.haskell.operators.div_op (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 1159
  i32.const 7
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
  call $hydra.serialization.op
)
  (func $hydra.haskell.operators.divide_op (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 1077
  i32.const 7
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
  call $hydra.serialization.op
)
  (func $hydra.haskell.operators.elem_op (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 1168
  i32.const 4
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
  call $hydra.serialization.op
)
  (func $hydra.haskell.operators.equal_op (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 1129
  i32.const 4
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
  call $hydra.serialization.op
)
  (func $hydra.haskell.operators.fmap_op (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 1104
  i32.const 4
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
  call $hydra.serialization.op
)
  (func $hydra.haskell.operators.gt_op (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 1141
  i32.const 4
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
  call $hydra.serialization.op
)
  (func $hydra.haskell.operators.gte_op (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 1146
  i32.const 4
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
  call $hydra.serialization.op
)
  (func $hydra.haskell.operators.index_op (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 1028
  i32.const 9
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
  call $hydra.serialization.op
)
  (func $hydra.haskell.operators.lambda_op (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 1066
  i32.const 1
  call $hydra.lib.math.negate
  i32.const 2
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
  call $hydra.serialization.op
)
  (func $hydra.haskell.operators.lt_op (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 1099
  i32.const 4
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
  call $hydra.serialization.op
)
  (func $hydra.haskell.operators.lte_op (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 1146
  i32.const 4
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
  call $hydra.serialization.op
)
  (func $hydra.haskell.operators.minus_op (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 1061
  i32.const 6
  i32.const 3
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
  call $hydra.serialization.op
)
  (func $hydra.haskell.operators.mod_op (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 1178
  i32.const 7
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
  call $hydra.serialization.op
)
  (func $hydra.haskell.operators.mult_op (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 1045
  i32.const 7
  i32.const 3
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
  call $hydra.serialization.op
)
  (func $hydra.haskell.operators.neq_op (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 1082
  i32.const 4
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
  call $hydra.serialization.op
)
  (func $hydra.haskell.operators.not_elem_op (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 1187
  i32.const 4
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
  call $hydra.serialization.op
)
  (func $hydra.haskell.operators.or_op (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 1219
  i32.const 2
  i32.const 2
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
  call $hydra.serialization.op
)
  (func $hydra.haskell.operators.plus_op (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 1050
  i32.const 6
  i32.const 3
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
  call $hydra.serialization.op
)
  (func $hydra.haskell.operators.quot_op (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 1200
  i32.const 7
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
  call $hydra.serialization.op
)
  (func $hydra.haskell.operators.rem_op (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 1210
  i32.const 7
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
  call $hydra.serialization.op
)
  (func $hydra.haskell.operators.type_op (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 1093
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
  call $hydra.serialization.op
)
)
