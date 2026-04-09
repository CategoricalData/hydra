(module
  (memory $memory 1 )
  (export "memory" (memory $memory) )
  (export "hydra.encode.classes.type_class" (func $hydra.encode.classes.type_class) )
  (func $hydra.encode.classes.type_class (param $arg_0 i32) (result i32)
  (block $end_type_class (result i32)
  (block $ordering
  (block $equality
  local.get $arg_0
  br_table $equality $ordering $ordering
)
  i32.const 0 ;; string: "hydra.classes.TypeClass"
  i32.const 0 ;; string: "equality"
  i32.const 0
  br $end_type_class
)
  i32.const 0 ;; string: "hydra.classes.TypeClass"
  i32.const 0 ;; string: "ordering"
  i32.const 0
  br $end_type_class
)
)
)
