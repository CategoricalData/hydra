(module
  (import "hydra.lib.lists" "hydra.lib.lists.cons" (func $hydra.lib.lists.cons (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.math" "hydra.lib.math.add" (func $hydra.lib.math.add (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.math" "hydra.lib.math.sub" (func $hydra.lib.math.sub (param i32) (param i32) (result i32) ) )
  (memory $memory 2 )
  (export "memory" (memory $memory) )
  (data (offset i32.const 1024 ) "")
  (global $__bump_ptr (mut i32) i32.const 1024 )
  (export "__bump_ptr" (global $__bump_ptr) )
  (func $__alloc (param $sz i32) (result i32)
  global.get $__bump_ptr
  global.get $__bump_ptr
  local.get $sz
  i32.add
  global.set $__bump_ptr
)
  (export "hydra.arity.primitive_arity" (func $hydra.arity.primitive_arity) )
  (export "hydra.arity.term_arity" (func $hydra.arity.term_arity) )
  (export "hydra.arity.type_arity" (func $hydra.arity.type_arity) )
  (export "hydra.arity.type_scheme_arity" (func $hydra.arity.type_scheme_arity) )
  (export "hydra.arity.uncurry_type" (func $hydra.arity.uncurry_type) )
  (func $hydra.arity.primitive_arity (param $arg_ i32) (result i32)
  (local $arg_2 i32)
  local.get $arg_
  i32.load offset=4
  drop
  local.get $arg_2
  i32.load offset=4
  call $hydra.arity.type_arity
)
  (func $hydra.arity.term_arity (param $arg_0 i32) (result i32)
  (local $__rec_ptr i32)
  (local $arg_ i32)
  (local $arg_2 i32)
  (local $i i32)
  (local $v i32)
  (local $xapp i32)
  (block $end_term (result i32)
  (block $unwrap
  (block $project
  (block $lambda
  (block $cases
  (block $application
  local.get $arg_0
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $application $cases $lambda $project $unwrap $unwrap
)
  local.get $v
  drop
  local.get $arg_
  i32.load
  drop
  local.get $arg_2
  call $hydra.arity.term_arity
  drop
  local.get $xapp
  i32.const 1
  call $hydra.lib.math.sub
  br $end_term
)
  local.get $v
  drop
  i32.const 1
  br $end_term
)
  local.get $v
  drop
  local.get $arg_
  drop
  local.get $arg_2
  i32.load offset=8
  call $hydra.arity.term_arity
  drop
  i32.const 1
  local.get $i
  call $hydra.lib.math.add
  br $end_term
)
  local.get $v
  drop
  i32.const 1
  br $end_term
)
  local.get $v
  drop
  i32.const 1
  br $end_term
)
)
  (func $hydra.arity.type_arity (param $arg_0 i32) (result i32)
  (local $__rec_ptr i32)
  (local $arg_ i32)
  (local $f i32)
  (local $v i32)
  (block $end_type (result i32)
  (block $function
  (block $forall
  (block $application
  (block $annotated
  local.get $arg_0
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $annotated $application $forall $function $function
)
  local.get $v
  drop
  local.get $arg_
  i32.load
  call $hydra.arity.type_arity
  br $end_type
)
  local.get $v
  drop
  local.get $arg_
  i32.load
  call $hydra.arity.type_arity
  br $end_type
)
  local.get $v
  drop
  local.get $arg_
  i32.load offset=4
  call $hydra.arity.type_arity
  br $end_type
)
  local.get $v
  drop
  i32.const 1
  local.get $f
  drop
  local.get $arg_
  i32.load offset=4
  call $hydra.arity.type_arity
  call $hydra.lib.math.add
  br $end_type
)
)
  (func $hydra.arity.type_scheme_arity (param $arg_ i32) (result i32)
  local.get $arg_
  i32.load offset=4
  call $hydra.arity.type_arity
)
  (func $hydra.arity.uncurry_type (param $t i32) (result i32)
  (local $__rec_ptr i32)
  (local $arg_ i32)
  (local $ft i32)
  (local $v i32)
  (block $end_type (result i32)
  (block $function
  (block $forall
  (block $application
  (block $annotated
  local.get $t
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $annotated $application $forall $function $function
)
  local.get $v
  drop
  local.get $arg_
  i32.load
  call $hydra.arity.uncurry_type
  br $end_type
)
  local.get $v
  drop
  local.get $arg_
  i32.load
  call $hydra.arity.uncurry_type
  br $end_type
)
  local.get $v
  drop
  local.get $arg_
  i32.load offset=4
  call $hydra.arity.uncurry_type
  br $end_type
)
  local.get $v
  drop
  local.get $ft
  i32.load
  local.get $ft
  drop
  local.get $arg_
  i32.load offset=4
  call $hydra.arity.uncurry_type
  call $hydra.lib.lists.cons
  br $end_type
)
)
)
