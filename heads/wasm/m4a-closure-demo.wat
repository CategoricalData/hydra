;; M4a closure mechanism demo.
;;
;; Hand-written WAT proving the end-to-end shape:
;;   - A closure value is a pointer to an 8-byte record: [table_idx:i32][env:i32]
;;   - Applying a closure: load its table index + env, push env and args, call_indirect
;;   - The called function's signature is always (env:i32, args...) -> i32
;;
;; This file is NOT coder-generated. It demonstrates the pattern for the JS
;; harness and shapes the future coder extension (M4b).
;;
;; Closure #0 in the table: (\env arg -> env + arg)
;; Exported apply_closure(clo_ptr, arg) validates the mechanism:
;;   load [clo_ptr+0] as table_idx, [clo_ptr+4] as env, push env, push arg,
;;   call_indirect (type $__closure_sig).
;;
;; Also exports __alloc (bump allocator) so JS can build closure values.

(module
  (memory $memory 1)
  (export "memory" (memory $memory))

  (global $__bump_ptr (mut i32) (i32.const 1024))
  (export "__bump_ptr" (global $__bump_ptr))

  (func $__alloc (param $sz i32) (result i32)
    global.get $__bump_ptr
    global.get $__bump_ptr
    local.get $sz
    i32.add
    global.set $__bump_ptr)
  (export "__alloc" (func $__alloc))

  ;; Closure signature: (env, arg) -> result. All closures use this shape for M4a.
  (type $__closure_sig (func (param i32) (param i32) (result i32)))

  ;; Concrete closure body: returns env + arg. Signature matches $__closure_sig.
  (func $__closure_add_env_arg (param $env i32) (param $arg i32) (result i32)
    local.get $env
    local.get $arg
    i32.add)

  ;; Function table for closures. Size 1 (one entry) for now.
  (table $__closure_table 1 funcref)

  ;; Populate table index 0 with $__closure_add_env_arg.
  (elem (i32.const 0) $__closure_add_env_arg)

  ;; apply_closure: (clo_ptr, arg) -> closure(env, arg)
  (func $apply_closure (param $clo_ptr i32) (param $arg i32) (result i32)
    ;; Push env (offset 4) onto the stack
    local.get $clo_ptr
    i32.load offset=4
    ;; Push real arg
    local.get $arg
    ;; Push table index (offset 0) onto the stack
    local.get $clo_ptr
    i32.load
    ;; Dispatch
    call_indirect (type $__closure_sig))
  (export "apply_closure" (func $apply_closure))
)
