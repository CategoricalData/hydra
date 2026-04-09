(module
  (import "hydra.lib.math" "hydra.lib.math.negate" (func $hydra.lib.math.negate (param i32) (result i32) ) )
  (import "hydra.serialization" "hydra.serialization.op" (func $hydra.serialization.op (param i32) (result i32) ) )
  (memory $memory 1 )
  (export "memory" (memory $memory) )
  (export "hydra.ext.haskell.operators.and_op" (func $hydra.ext.haskell.operators.and_op) )
  (export "hydra.ext.haskell.operators.ap_op" (func $hydra.ext.haskell.operators.ap_op) )
  (export "hydra.ext.haskell.operators.app_op" (func $hydra.ext.haskell.operators.app_op) )
  (export "hydra.ext.haskell.operators.apply_op" (func $hydra.ext.haskell.operators.apply_op) )
  (export "hydra.ext.haskell.operators.arrow_op" (func $hydra.ext.haskell.operators.arrow_op) )
  (export "hydra.ext.haskell.operators.assert_op" (func $hydra.ext.haskell.operators.assert_op) )
  (export "hydra.ext.haskell.operators.bind_op" (func $hydra.ext.haskell.operators.bind_op) )
  (export "hydra.ext.haskell.operators.case_op" (func $hydra.ext.haskell.operators.case_op) )
  (export "hydra.ext.haskell.operators.compose_op" (func $hydra.ext.haskell.operators.compose_op) )
  (export "hydra.ext.haskell.operators.concat_op" (func $hydra.ext.haskell.operators.concat_op) )
  (export "hydra.ext.haskell.operators.cons_op" (func $hydra.ext.haskell.operators.cons_op) )
  (export "hydra.ext.haskell.operators.define_op" (func $hydra.ext.haskell.operators.define_op) )
  (export "hydra.ext.haskell.operators.diamond_op" (func $hydra.ext.haskell.operators.diamond_op) )
  (export "hydra.ext.haskell.operators.div_op" (func $hydra.ext.haskell.operators.div_op) )
  (export "hydra.ext.haskell.operators.divide_op" (func $hydra.ext.haskell.operators.divide_op) )
  (export "hydra.ext.haskell.operators.elem_op" (func $hydra.ext.haskell.operators.elem_op) )
  (export "hydra.ext.haskell.operators.equal_op" (func $hydra.ext.haskell.operators.equal_op) )
  (export "hydra.ext.haskell.operators.fmap_op" (func $hydra.ext.haskell.operators.fmap_op) )
  (export "hydra.ext.haskell.operators.gt_op" (func $hydra.ext.haskell.operators.gt_op) )
  (export "hydra.ext.haskell.operators.gte_op" (func $hydra.ext.haskell.operators.gte_op) )
  (export "hydra.ext.haskell.operators.index_op" (func $hydra.ext.haskell.operators.index_op) )
  (export "hydra.ext.haskell.operators.lambda_op" (func $hydra.ext.haskell.operators.lambda_op) )
  (export "hydra.ext.haskell.operators.lt_op" (func $hydra.ext.haskell.operators.lt_op) )
  (export "hydra.ext.haskell.operators.lte_op" (func $hydra.ext.haskell.operators.lte_op) )
  (export "hydra.ext.haskell.operators.minus_op" (func $hydra.ext.haskell.operators.minus_op) )
  (export "hydra.ext.haskell.operators.mod_op" (func $hydra.ext.haskell.operators.mod_op) )
  (export "hydra.ext.haskell.operators.mult_op" (func $hydra.ext.haskell.operators.mult_op) )
  (export "hydra.ext.haskell.operators.neq_op" (func $hydra.ext.haskell.operators.neq_op) )
  (export "hydra.ext.haskell.operators.not_elem_op" (func $hydra.ext.haskell.operators.not_elem_op) )
  (export "hydra.ext.haskell.operators.or_op" (func $hydra.ext.haskell.operators.or_op) )
  (export "hydra.ext.haskell.operators.plus_op" (func $hydra.ext.haskell.operators.plus_op) )
  (export "hydra.ext.haskell.operators.quot_op" (func $hydra.ext.haskell.operators.quot_op) )
  (export "hydra.ext.haskell.operators.rem_op" (func $hydra.ext.haskell.operators.rem_op) )
  (export "hydra.ext.haskell.operators.type_op" (func $hydra.ext.haskell.operators.type_op) )
  (func $hydra.ext.haskell.operators.and_op (result i32)
  i32.const 0 ;; string: "&&"
  i32.const 3
  i32.const 0
  call $hydra.serialization.op
)
  (func $hydra.ext.haskell.operators.ap_op (result i32)
  i32.const 0 ;; string: "<*>"
  i32.const 4
  i32.const 0
  call $hydra.serialization.op
)
  (func $hydra.ext.haskell.operators.app_op (result i32)
  i32.const 0 ;; string: ""
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
)
  (func $hydra.ext.haskell.operators.apply_op (result i32)
  i32.const 0 ;; string: "$"
  i32.const 0
  i32.const 0
  call $hydra.serialization.op
)
  (func $hydra.ext.haskell.operators.arrow_op (result i32)
  i32.const 0 ;; string: "->"
  i32.const 1
  call $hydra.lib.math.negate
  i32.const 0
  call $hydra.serialization.op
)
  (func $hydra.ext.haskell.operators.assert_op (result i32)
  i32.const 0 ;; string: "=>"
  i32.const 0
  i32.const 0
  call $hydra.serialization.op
)
  (func $hydra.ext.haskell.operators.bind_op (result i32)
  i32.const 0 ;; string: ">>="
  i32.const 1
  i32.const 0
  call $hydra.serialization.op
)
  (func $hydra.ext.haskell.operators.case_op (result i32)
  i32.const 0 ;; string: "->"
  i32.const 0
  i32.const 0
  call $hydra.serialization.op
)
  (func $hydra.ext.haskell.operators.compose_op (result i32)
  i32.const 0 ;; string: "."
  i32.const 9
  i32.const 0
  call $hydra.serialization.op
)
  (func $hydra.ext.haskell.operators.concat_op (result i32)
  i32.const 0 ;; string: "++"
  i32.const 5
  i32.const 0
  call $hydra.serialization.op
)
  (func $hydra.ext.haskell.operators.cons_op (result i32)
  i32.const 0 ;; string: ":"
  i32.const 5
  i32.const 0
  call $hydra.serialization.op
)
  (func $hydra.ext.haskell.operators.define_op (result i32)
  i32.const 0 ;; string: "="
  i32.const 0
  i32.const 0
  call $hydra.serialization.op
)
  (func $hydra.ext.haskell.operators.diamond_op (result i32)
  i32.const 0 ;; string: "<>"
  i32.const 6
  i32.const 0
  call $hydra.serialization.op
)
  (func $hydra.ext.haskell.operators.div_op (result i32)
  i32.const 0 ;; string: "`div`"
  i32.const 7
  i32.const 0
  call $hydra.serialization.op
)
  (func $hydra.ext.haskell.operators.divide_op (result i32)
  i32.const 0 ;; string: "/"
  i32.const 7
  i32.const 0
  call $hydra.serialization.op
)
  (func $hydra.ext.haskell.operators.elem_op (result i32)
  i32.const 0 ;; string: "`elem`"
  i32.const 4
  i32.const 0
  call $hydra.serialization.op
)
  (func $hydra.ext.haskell.operators.equal_op (result i32)
  i32.const 0 ;; string: "=="
  i32.const 4
  i32.const 0
  call $hydra.serialization.op
)
  (func $hydra.ext.haskell.operators.fmap_op (result i32)
  i32.const 0 ;; string: "<$>"
  i32.const 4
  i32.const 0
  call $hydra.serialization.op
)
  (func $hydra.ext.haskell.operators.gt_op (result i32)
  i32.const 0 ;; string: ">"
  i32.const 4
  i32.const 0
  call $hydra.serialization.op
)
  (func $hydra.ext.haskell.operators.gte_op (result i32)
  i32.const 0 ;; string: ">="
  i32.const 4
  i32.const 0
  call $hydra.serialization.op
)
  (func $hydra.ext.haskell.operators.index_op (result i32)
  i32.const 0 ;; string: "!!"
  i32.const 9
  i32.const 0
  call $hydra.serialization.op
)
  (func $hydra.ext.haskell.operators.lambda_op (result i32)
  i32.const 0 ;; string: "->"
  i32.const 1
  call $hydra.lib.math.negate
  i32.const 0
  call $hydra.serialization.op
)
  (func $hydra.ext.haskell.operators.lt_op (result i32)
  i32.const 0 ;; string: "<"
  i32.const 4
  i32.const 0
  call $hydra.serialization.op
)
  (func $hydra.ext.haskell.operators.lte_op (result i32)
  i32.const 0 ;; string: ">="
  i32.const 4
  i32.const 0
  call $hydra.serialization.op
)
  (func $hydra.ext.haskell.operators.minus_op (result i32)
  i32.const 0 ;; string: "-"
  i32.const 6
  i32.const 0
  call $hydra.serialization.op
)
  (func $hydra.ext.haskell.operators.mod_op (result i32)
  i32.const 0 ;; string: "`mod`"
  i32.const 7
  i32.const 0
  call $hydra.serialization.op
)
  (func $hydra.ext.haskell.operators.mult_op (result i32)
  i32.const 0 ;; string: "*"
  i32.const 7
  i32.const 0
  call $hydra.serialization.op
)
  (func $hydra.ext.haskell.operators.neq_op (result i32)
  i32.const 0 ;; string: "/="
  i32.const 4
  i32.const 0
  call $hydra.serialization.op
)
  (func $hydra.ext.haskell.operators.not_elem_op (result i32)
  i32.const 0 ;; string: "`notElem`"
  i32.const 4
  i32.const 0
  call $hydra.serialization.op
)
  (func $hydra.ext.haskell.operators.or_op (result i32)
  i32.const 0 ;; string: "||"
  i32.const 2
  i32.const 0
  call $hydra.serialization.op
)
  (func $hydra.ext.haskell.operators.plus_op (result i32)
  i32.const 0 ;; string: "+"
  i32.const 6
  i32.const 0
  call $hydra.serialization.op
)
  (func $hydra.ext.haskell.operators.quot_op (result i32)
  i32.const 0 ;; string: "`quot`"
  i32.const 7
  i32.const 0
  call $hydra.serialization.op
)
  (func $hydra.ext.haskell.operators.rem_op (result i32)
  i32.const 0 ;; string: "`rem`"
  i32.const 7
  i32.const 0
  call $hydra.serialization.op
)
  (func $hydra.ext.haskell.operators.type_op (result i32)
  i32.const 0 ;; string: "::"
  i32.const 0
  i32.const 0
  call $hydra.serialization.op
)
)
