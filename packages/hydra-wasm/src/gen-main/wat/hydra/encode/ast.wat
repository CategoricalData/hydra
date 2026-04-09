(module
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.map" (func $hydra.lib.maybes.map (param i32) (result i32) ) )
  (memory $memory 1 )
  (export "memory" (memory $memory) )
  (export "hydra.encode.ast.associativity" (func $hydra.encode.ast.associativity) )
  (export "hydra.encode.ast.block_style" (func $hydra.encode.ast.block_style) )
  (export "hydra.encode.ast.bracket_expr" (func $hydra.encode.ast.bracket_expr) )
  (export "hydra.encode.ast.brackets" (func $hydra.encode.ast.brackets) )
  (export "hydra.encode.ast.expr" (func $hydra.encode.ast.expr) )
  (export "hydra.encode.ast.indent_style" (func $hydra.encode.ast.indent_style) )
  (export "hydra.encode.ast.indented_expression" (func $hydra.encode.ast.indented_expression) )
  (export "hydra.encode.ast.op" (func $hydra.encode.ast.op) )
  (export "hydra.encode.ast.op_expr" (func $hydra.encode.ast.op_expr) )
  (export "hydra.encode.ast.padding" (func $hydra.encode.ast.padding) )
  (export "hydra.encode.ast.precedence" (func $hydra.encode.ast.precedence) )
  (export "hydra.encode.ast.seq_expr" (func $hydra.encode.ast.seq_expr) )
  (export "hydra.encode.ast.symbol" (func $hydra.encode.ast.symbol) )
  (export "hydra.encode.ast.ws" (func $hydra.encode.ast.ws) )
  (func $hydra.encode.ast.associativity (param $arg_0 i32) (result i32)
  (block $end_associativity (result i32)
  (block $both
  (block $right
  (block $left
  (block $none
  local.get $arg_0
  br_table $none $left $right $both $both
)
  i32.const 0 ;; string: "hydra.ast.Associativity"
  i32.const 0 ;; string: "none"
  i32.const 0
  br $end_associativity
)
  i32.const 0 ;; string: "hydra.ast.Associativity"
  i32.const 0 ;; string: "left"
  i32.const 0
  br $end_associativity
)
  i32.const 0 ;; string: "hydra.ast.Associativity"
  i32.const 0 ;; string: "right"
  i32.const 0
  br $end_associativity
)
  i32.const 0 ;; string: "hydra.ast.Associativity"
  i32.const 0 ;; string: "both"
  i32.const 0
  br $end_associativity
)
)
  (func $hydra.encode.ast.block_style (param $x i32) (result i32)
  (local $opt i32)
  (local $x2 i32)
  i32.const 0 ;; string: "hydra.ast.BlockStyle"
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "indent"
  local.get $x2
  local.get $opt
  call $hydra.lib.maybes.map
  i32.const 0 ;; string: "newlineBeforeContent"
  local.get $x2
  i32.const 0 ;; string: "newlineAfterContent"
  local.get $x2
)
  (func $hydra.encode.ast.bracket_expr (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.ast.BracketExpr"
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "brackets"
  local.get $x
  ;; project field: brackets
  call $hydra.encode.ast.brackets
  i32.const 0 ;; string: "enclosed"
  local.get $x
  ;; project field: enclosed
  call $hydra.encode.ast.expr
  i32.const 0 ;; string: "style"
  local.get $x
  ;; project field: style
  call $hydra.encode.ast.block_style
)
  (func $hydra.encode.ast.brackets (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.ast.Brackets"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "open"
  local.get $x
  ;; project field: open
  call $hydra.encode.ast.symbol
  i32.const 0 ;; string: "close"
  local.get $x
  ;; project field: close
  call $hydra.encode.ast.symbol
)
  (func $hydra.encode.ast.expr (param $arg_0 i32) (result i32)
  (local $y i32)
  (block $end_expr (result i32)
  (block $seq
  (block $brackets
  (block $op
  (block $indent
  (block $const
  local.get $arg_0
  br_table $const $indent $op $brackets $seq $seq
)
  i32.const 0 ;; string: "hydra.ast.Expr"
  i32.const 0 ;; string: "const"
  local.get $y
  call $hydra.encode.ast.symbol
  br $end_expr
)
  i32.const 0 ;; string: "hydra.ast.Expr"
  i32.const 0 ;; string: "indent"
  local.get $y
  call $hydra.encode.ast.indented_expression
  br $end_expr
)
  i32.const 0 ;; string: "hydra.ast.Expr"
  i32.const 0 ;; string: "op"
  local.get $y
  call $hydra.encode.ast.op_expr
  br $end_expr
)
  i32.const 0 ;; string: "hydra.ast.Expr"
  i32.const 0 ;; string: "brackets"
  local.get $y
  call $hydra.encode.ast.bracket_expr
  br $end_expr
)
  i32.const 0 ;; string: "hydra.ast.Expr"
  i32.const 0 ;; string: "seq"
  local.get $y
  call $hydra.encode.ast.seq_expr
  br $end_expr
)
)
  (func $hydra.encode.ast.indent_style (param $arg_0 i32) (result i32)
  (local $x i32)
  (block $end_indent_style (result i32)
  (block $subsequent_lines
  (block $all_lines
  local.get $arg_0
  br_table $all_lines $subsequent_lines $subsequent_lines
)
  i32.const 0 ;; string: "hydra.ast.IndentStyle"
  i32.const 0 ;; string: "allLines"
  local.get $x
  br $end_indent_style
)
  i32.const 0 ;; string: "hydra.ast.IndentStyle"
  i32.const 0 ;; string: "subsequentLines"
  local.get $x
  br $end_indent_style
)
)
  (func $hydra.encode.ast.indented_expression (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.ast.IndentedExpression"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "style"
  local.get $x
  ;; project field: style
  call $hydra.encode.ast.indent_style
  i32.const 0 ;; string: "expr"
  local.get $x
  ;; project field: expr
  call $hydra.encode.ast.expr
)
  (func $hydra.encode.ast.op (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.ast.Op"
  i32.const 4
  ;; list elements follow
  i32.const 0 ;; string: "symbol"
  local.get $x
  ;; project field: symbol
  call $hydra.encode.ast.symbol
  i32.const 0 ;; string: "padding"
  local.get $x
  ;; project field: padding
  call $hydra.encode.ast.padding
  i32.const 0 ;; string: "precedence"
  local.get $x
  ;; project field: precedence
  call $hydra.encode.ast.precedence
  i32.const 0 ;; string: "associativity"
  local.get $x
  ;; project field: associativity
  call $hydra.encode.ast.associativity
)
  (func $hydra.encode.ast.op_expr (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.ast.OpExpr"
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "op"
  local.get $x
  ;; project field: op
  call $hydra.encode.ast.op
  i32.const 0 ;; string: "lhs"
  local.get $x
  ;; project field: lhs
  call $hydra.encode.ast.expr
  i32.const 0 ;; string: "rhs"
  local.get $x
  ;; project field: rhs
  call $hydra.encode.ast.expr
)
  (func $hydra.encode.ast.padding (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.ast.Padding"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "left"
  local.get $x
  ;; project field: left
  call $hydra.encode.ast.ws
  i32.const 0 ;; string: "right"
  local.get $x
  ;; project field: right
  call $hydra.encode.ast.ws
)
  (func $hydra.encode.ast.precedence (param $x i32) (result i32)
  (local $x2 i32)
  i32.const 0 ;; string: "hydra.ast.Precedence"
  local.get $x2
)
  (func $hydra.encode.ast.seq_expr (param $x i32) (result i32)
  (local $xs i32)
  i32.const 0 ;; string: "hydra.ast.SeqExpr"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "op"
  local.get $x
  ;; project field: op
  call $hydra.encode.ast.op
  i32.const 0 ;; string: "elements"
  call $hydra.encode.ast.expr
  local.get $xs
  call $hydra.lib.lists.map
)
  (func $hydra.encode.ast.symbol (param $x i32) (result i32)
  (local $x2 i32)
  i32.const 0 ;; string: "hydra.ast.Symbol"
  local.get $x2
)
  (func $hydra.encode.ast.ws (param $arg_0 i32) (result i32)
  (local $x i32)
  (block $end_ws (result i32)
  (block $double_break
  (block $break_and_indent
  (block $break
  (block $space
  (block $none
  local.get $arg_0
  br_table $none $space $break $break_and_indent $double_break $double_break
)
  i32.const 0 ;; string: "hydra.ast.Ws"
  i32.const 0 ;; string: "none"
  i32.const 0
  br $end_ws
)
  i32.const 0 ;; string: "hydra.ast.Ws"
  i32.const 0 ;; string: "space"
  i32.const 0
  br $end_ws
)
  i32.const 0 ;; string: "hydra.ast.Ws"
  i32.const 0 ;; string: "break"
  i32.const 0
  br $end_ws
)
  i32.const 0 ;; string: "hydra.ast.Ws"
  i32.const 0 ;; string: "breakAndIndent"
  local.get $x
  br $end_ws
)
  i32.const 0 ;; string: "hydra.ast.Ws"
  i32.const 0 ;; string: "doubleBreak"
  i32.const 0
  br $end_ws
)
)
)
