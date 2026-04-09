(module
  (memory $memory 1 )
  (export "memory" (memory $memory) )
  (export "hydra.show.util.case_convention" (func $hydra.show.util.case_convention) )
  (export "hydra.show.util.comparison" (func $hydra.show.util.comparison) )
  (func $hydra.show.util.case_convention (param $c i32) (result i32)
  (block $end_case_convention (result i32)
  (block $pascal
  (block $camel
  (block $upper_snake
  (block $lower_snake
  local.get $c
  br_table $lower_snake $upper_snake $camel $pascal $pascal
)
  i32.const 0 ;; string: "lower_snake_case"
  br $end_case_convention
)
  i32.const 0 ;; string: "UPPER_SNAKE_CASE"
  br $end_case_convention
)
  i32.const 0 ;; string: "camelCase"
  br $end_case_convention
)
  i32.const 0 ;; string: "PascalCase"
  br $end_case_convention
)
)
  (func $hydra.show.util.comparison (param $c i32) (result i32)
  (block $end_comparison (result i32)
  (block $greater_than
  (block $equal_to
  (block $less_than
  local.get $c
  br_table $less_than $equal_to $greater_than $greater_than
)
  i32.const 0 ;; string: "lessThan"
  br $end_comparison
)
  i32.const 0 ;; string: "equalTo"
  br $end_comparison
)
  i32.const 0 ;; string: "greaterThan"
  br $end_comparison
)
)
)
