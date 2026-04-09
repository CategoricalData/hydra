(module
  (memory $memory 1 )
  (export "memory" (memory $memory) )
  (export "hydra.encode.parsing.parse_error" (func $hydra.encode.parsing.parse_error) )
  (export "hydra.encode.parsing.parse_result" (func $hydra.encode.parsing.parse_result) )
  (export "hydra.encode.parsing.parse_success" (func $hydra.encode.parsing.parse_success) )
  (func $hydra.encode.parsing.parse_error (param $x i32) (result i32)
  (local $x2 i32)
  i32.const 0 ;; string: "hydra.parsing.ParseError"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "message"
  local.get $x2
  i32.const 0 ;; string: "remainder"
  local.get $x2
)
  (func $hydra.encode.parsing.parse_result (param $a i32) (param $arg_0 i32) (result i32)
  (local $y i32)
  (block $end_parse_result (result i32)
  (block $failure
  (block $success
  local.get $a
  br_table $success $failure $failure
)
  i32.const 0 ;; string: "hydra.parsing.ParseResult"
  i32.const 0 ;; string: "success"
  local.get $a
  local.get $y
  call $hydra.encode.parsing.parse_success
  br $end_parse_result
)
  i32.const 0 ;; string: "hydra.parsing.ParseResult"
  i32.const 0 ;; string: "failure"
  local.get $y
  call $hydra.encode.parsing.parse_error
  br $end_parse_result
)
)
  (func $hydra.encode.parsing.parse_success (param $a i32) (param $x i32) (result i32)
  (local $x2 i32)
  i32.const 0 ;; string: "hydra.parsing.ParseSuccess"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "value"
  local.get $x
  ;; project field: value
  local.get $a
  i32.const 0 ;; string: "remainder"
  local.get $x2
)
)
