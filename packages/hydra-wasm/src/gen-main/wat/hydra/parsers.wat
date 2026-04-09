(module
  (import "hydra.lib.equality" "hydra.lib.equality.equal" (func $hydra.lib.equality.equal (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.cons" (func $hydra.lib.lists.cons (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.drop" (func $hydra.lib.lists.drop (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.foldl" (func $hydra.lib.lists.foldl (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.length" (func $hydra.lib.lists.length (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.safe_head" (func $hydra.lib.lists.safe_head (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.take" (func $hydra.lib.lists.take (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.if_else" (func $hydra.lib.logic.if_else (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.maybe" (func $hydra.lib.maybes.maybe (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.pure" (func $hydra.lib.maybes.pure (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat2" (func $hydra.lib.strings.cat2 (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.from_list" (func $hydra.lib.strings.from_list (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.to_list" (func $hydra.lib.strings.to_list (param i32) (result i32) ) )
  (memory $memory 1 )
  (export "memory" (memory $memory) )
  (export "hydra.parsers.alt" (func $hydra.parsers.alt) )
  (export "hydra.parsers.any_char" (func $hydra.parsers.any_char) )
  (export "hydra.parsers.apply" (func $hydra.parsers.apply) )
  (export "hydra.parsers.between" (func $hydra.parsers.between) )
  (export "hydra.parsers.bind" (func $hydra.parsers.bind) )
  (export "hydra.parsers.char" (func $hydra.parsers.char) )
  (export "hydra.parsers.choice" (func $hydra.parsers.choice) )
  (export "hydra.parsers.eof" (func $hydra.parsers.eof) )
  (export "hydra.parsers.fail" (func $hydra.parsers.fail) )
  (export "hydra.parsers.lazy" (func $hydra.parsers.lazy) )
  (export "hydra.parsers.many" (func $hydra.parsers.many) )
  (export "hydra.parsers.map" (func $hydra.parsers.map) )
  (export "hydra.parsers.optional" (func $hydra.parsers.optional) )
  (export "hydra.parsers.pure" (func $hydra.parsers.pure) )
  (export "hydra.parsers.run_parser" (func $hydra.parsers.run_parser) )
  (export "hydra.parsers.satisfy" (func $hydra.parsers.satisfy) )
  (export "hydra.parsers.sep_by" (func $hydra.parsers.sep_by) )
  (export "hydra.parsers.sep_by1" (func $hydra.parsers.sep_by1) )
  (export "hydra.parsers.some" (func $hydra.parsers.some) )
  (export "hydra.parsers.string" (func $hydra.parsers.string) )
  (func $hydra.parsers.alt (param $p1 i32) (param $p2 i32) (result i32)
  (local $e i32)
  (local $input i32)
  (local $parse i32)
  (local $s i32)
  (block $end_parse_result (result i32)
  (block $failure
  (block $success
  nop
  br_table $success $failure $failure
)
  local.get $s
  br $end_parse_result
)
  local.get $e
  ;; project field: remainder
  local.get $input
  call $hydra.lib.equality.equal
  nop
  local.get $e
  call $hydra.lib.logic.if_else
  br $end_parse_result
)
  local.set $parse
  local.get $parse
)
  (func $hydra.parsers.any_char (result i32)
  i32.const 1
  call $hydra.parsers.satisfy
)
  (func $hydra.parsers.apply (param $pf i32) (param $pa i32) (result i32)
  (local $e i32)
  (local $parse i32)
  (local $sa i32)
  (local $sf i32)
  (block $end_parse_result (result i32)
  (block $failure
  (block $success
  nop
  br_table $success $failure $failure
)
  (block $end_parse_result (result i32)
  (block $failure
  (block $success
  nop
  br_table $success $failure $failure
)
  local.get $sf
  local.get $sa
  ;; project field: value
  ;; project field: value
  local.get $sa
  ;; project field: remainder
  br $end_parse_result
)
  local.get $e
  br $end_parse_result
)
  br $end_parse_result
)
  local.get $e
  br $end_parse_result
)
  local.set $parse
  local.get $parse
)
  (func $hydra.parsers.between (param $open i32) (param $close i32) (param $p i32) (result i32)
  (local $x i32)
  local.get $open
  local.get $p
  local.get $close
  local.get $x
  call $hydra.parsers.pure
  call $hydra.parsers.bind
  call $hydra.parsers.bind
  call $hydra.parsers.bind
)
  (func $hydra.parsers.bind (param $pa i32) (param $f i32) (result i32)
  (local $e i32)
  (local $parse i32)
  (block $end_parse_result (result i32)
  (block $failure
  (block $success
  nop
  br_table $success $failure $failure
)
  nop
  br $end_parse_result
)
  local.get $e
  br $end_parse_result
)
  local.set $parse
  local.get $parse
)
  (func $hydra.parsers.char (param $c i32) (result i32)
  (local $x i32)
  local.get $x
  local.get $c
  call $hydra.lib.equality.equal
  call $hydra.parsers.satisfy
)
  (func $hydra.parsers.choice (param $ps i32) (result i32)
  call $hydra.parsers.alt
  i32.const 0 ;; string: "no choice matched"
  call $hydra.parsers.fail
  local.get $ps
  call $hydra.lib.lists.foldl
)
  (func $hydra.parsers.eof (result i32)
  (local $input i32)
  local.get $input
  i32.const 0 ;; string: ""
  call $hydra.lib.equality.equal
  i32.const 0 ;; string: ""
  i32.const 0 ;; string: "expected end of input"
  local.get $input
  call $hydra.lib.logic.if_else
)
  (func $hydra.parsers.fail (param $msg i32) (result i32)
  (local $input i32)
  local.get $msg
  local.get $input
)
  (func $hydra.parsers.lazy (param $f i32) (result i32)
  nop
)
  (func $hydra.parsers.many (param $p i32) (result i32)
  local.get $p
  call $hydra.parsers.some
  i32.const 0
  ;; list elements follow
  call $hydra.parsers.pure
  call $hydra.parsers.alt
)
  (func $hydra.parsers.map (param $f i32) (param $pa i32) (result i32)
  (local $e i32)
  (local $parse i32)
  (local $s i32)
  (block $end_parse_result (result i32)
  (block $failure
  (block $success
  nop
  br_table $success $failure $failure
)
  local.get $s
  ;; project field: value
  local.get $f
  local.get $s
  ;; project field: remainder
  br $end_parse_result
)
  local.get $e
  br $end_parse_result
)
  local.set $parse
  local.get $parse
)
  (func $hydra.parsers.optional (param $p i32) (result i32)
  call $hydra.lib.maybes.pure
  local.get $p
  call $hydra.parsers.map
  i32.const 0
  call $hydra.parsers.pure
  call $hydra.parsers.alt
)
  (func $hydra.parsers.pure (param $a i32) (result i32)
  (local $input i32)
  local.get $a
  local.get $input
)
  (func $hydra.parsers.run_parser (param $p i32) (param $input i32) (result i32)
  nop
)
  (func $hydra.parsers.satisfy (param $pred i32) (result i32)
  (local $c i32)
  (local $codes i32)
  (local $input i32)
  (local $parse i32)
  (local $rest i32)
  local.get $input
  call $hydra.lib.strings.to_list
  local.set $codes
  i32.const 0 ;; string: "unexpected end of input"
  local.get $input
  i32.const 1
  local.get $codes
  call $hydra.lib.lists.drop
  call $hydra.lib.strings.from_list
  local.set $rest
  local.get $c
  local.get $pred
  local.get $c
  local.get $rest
  i32.const 0 ;; string: "character did not satisfy predicate"
  local.get $input
  call $hydra.lib.logic.if_else
  local.get $codes
  call $hydra.lib.lists.safe_head
  call $hydra.lib.maybes.maybe
  local.set $parse
  local.get $parse
)
  (func $hydra.parsers.sep_by (param $p i32) (param $sep i32) (result i32)
  local.get $p
  local.get $sep
  call $hydra.parsers.sep_by1
  i32.const 0
  ;; list elements follow
  call $hydra.parsers.pure
  call $hydra.parsers.alt
)
  (func $hydra.parsers.sep_by1 (param $p i32) (param $sep i32) (result i32)
  (local $x i32)
  (local $xs i32)
  local.get $p
  local.get $sep
  local.get $p
  call $hydra.parsers.bind
  call $hydra.parsers.many
  local.get $x
  local.get $xs
  call $hydra.lib.lists.cons
  call $hydra.parsers.pure
  call $hydra.parsers.bind
  call $hydra.parsers.bind
)
  (func $hydra.parsers.some (param $p i32) (result i32)
  (local $x i32)
  (local $xs i32)
  local.get $p
  local.get $p
  call $hydra.parsers.many
  local.get $x
  local.get $xs
  call $hydra.lib.lists.cons
  call $hydra.parsers.pure
  call $hydra.parsers.bind
  call $hydra.parsers.bind
)
  (func $hydra.parsers.string (param $str i32) (result i32)
  (local $input i32)
  (local $input_codes i32)
  (local $input_prefix i32)
  (local $str_codes i32)
  (local $str_len i32)
  local.get $str
  call $hydra.lib.strings.to_list
  local.set $str_codes
  local.get $input
  call $hydra.lib.strings.to_list
  local.set $input_codes
  local.get $str_codes
  call $hydra.lib.lists.length
  local.set $str_len
  local.get $str_len
  local.get $input_codes
  call $hydra.lib.lists.take
  local.set $input_prefix
  local.get $str_codes
  local.get $input_prefix
  call $hydra.lib.equality.equal
  local.get $str
  local.get $str_len
  local.get $input_codes
  call $hydra.lib.lists.drop
  call $hydra.lib.strings.from_list
  i32.const 0 ;; string: "expected: "
  local.get $str
  call $hydra.lib.strings.cat2
  local.get $input
  call $hydra.lib.logic.if_else
)
)
