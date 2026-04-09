(module
  (memory $memory 1 )
  (export "memory" (memory $memory) )
  (export "hydra.encode.util.case_convention" (func $hydra.encode.util.case_convention) )
  (export "hydra.encode.util.comparison" (func $hydra.encode.util.comparison) )
  (export "hydra.encode.util.precision" (func $hydra.encode.util.precision) )
  (func $hydra.encode.util.case_convention (param $arg_0 i32) (result i32)
  (block $end_case_convention (result i32)
  (block $upper_snake
  (block $lower_snake
  (block $pascal
  (block $camel
  local.get $arg_0
  br_table $camel $pascal $lower_snake $upper_snake $upper_snake
)
  i32.const 0 ;; string: "hydra.util.CaseConvention"
  i32.const 0 ;; string: "camel"
  i32.const 0
  br $end_case_convention
)
  i32.const 0 ;; string: "hydra.util.CaseConvention"
  i32.const 0 ;; string: "pascal"
  i32.const 0
  br $end_case_convention
)
  i32.const 0 ;; string: "hydra.util.CaseConvention"
  i32.const 0 ;; string: "lowerSnake"
  i32.const 0
  br $end_case_convention
)
  i32.const 0 ;; string: "hydra.util.CaseConvention"
  i32.const 0 ;; string: "upperSnake"
  i32.const 0
  br $end_case_convention
)
)
  (func $hydra.encode.util.comparison (param $arg_0 i32) (result i32)
  (block $end_comparison (result i32)
  (block $greater_than
  (block $equal_to
  (block $less_than
  local.get $arg_0
  br_table $less_than $equal_to $greater_than $greater_than
)
  i32.const 0 ;; string: "hydra.util.Comparison"
  i32.const 0 ;; string: "lessThan"
  i32.const 0
  br $end_comparison
)
  i32.const 0 ;; string: "hydra.util.Comparison"
  i32.const 0 ;; string: "equalTo"
  i32.const 0
  br $end_comparison
)
  i32.const 0 ;; string: "hydra.util.Comparison"
  i32.const 0 ;; string: "greaterThan"
  i32.const 0
  br $end_comparison
)
)
  (func $hydra.encode.util.precision (param $arg_0 i32) (result i32)
  (local $x i32)
  (block $end_precision (result i32)
  (block $bits
  (block $arbitrary
  local.get $arg_0
  br_table $arbitrary $bits $bits
)
  i32.const 0 ;; string: "hydra.util.Precision"
  i32.const 0 ;; string: "arbitrary"
  i32.const 0
  br $end_precision
)
  i32.const 0 ;; string: "hydra.util.Precision"
  i32.const 0 ;; string: "bits"
  local.get $x
  br $end_precision
)
)
)
