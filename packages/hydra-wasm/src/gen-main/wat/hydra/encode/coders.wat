(module
  (memory $memory 1 )
  (export "memory" (memory $memory) )
  (export "hydra.encode.coders.coder_direction" (func $hydra.encode.coders.coder_direction) )
  (export "hydra.encode.coders.language_name" (func $hydra.encode.coders.language_name) )
  (export "hydra.encode.coders.traversal_order" (func $hydra.encode.coders.traversal_order) )
  (func $hydra.encode.coders.coder_direction (param $arg_0 i32) (result i32)
  (block $end_coder_direction (result i32)
  (block $decode
  (block $encode
  local.get $arg_0
  br_table $encode $decode $decode
)
  i32.const 0 ;; string: "hydra.coders.CoderDirection"
  i32.const 0 ;; string: "encode"
  i32.const 0
  br $end_coder_direction
)
  i32.const 0 ;; string: "hydra.coders.CoderDirection"
  i32.const 0 ;; string: "decode"
  i32.const 0
  br $end_coder_direction
)
)
  (func $hydra.encode.coders.language_name (param $x i32) (result i32)
  (local $x2 i32)
  i32.const 0 ;; string: "hydra.coders.LanguageName"
  local.get $x2
)
  (func $hydra.encode.coders.traversal_order (param $arg_0 i32) (result i32)
  (block $end_traversal_order (result i32)
  (block $post
  (block $pre
  local.get $arg_0
  br_table $pre $post $post
)
  i32.const 0 ;; string: "hydra.coders.TraversalOrder"
  i32.const 0 ;; string: "pre"
  i32.const 0
  br $end_traversal_order
)
  i32.const 0 ;; string: "hydra.coders.TraversalOrder"
  i32.const 0 ;; string: "post"
  i32.const 0
  br $end_traversal_order
)
)
)
