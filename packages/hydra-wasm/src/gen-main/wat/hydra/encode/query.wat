(module
  (import "hydra.encode.core" "hydra.encode.core.name" (func $hydra.encode.core.name (param i32) (result i32) ) )
  (import "hydra.encode.core" "hydra.encode.core.projection" (func $hydra.encode.core.projection (param i32) (result i32) ) )
  (import "hydra.encode.core" "hydra.encode.core.term" (func $hydra.encode.core.term (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.map" (func $hydra.lib.maybes.map (param i32) (result i32) ) )
  (memory $memory 1 )
  (export "memory" (memory $memory) )
  (export "hydra.encode.query.comparison_constraint" (func $hydra.encode.query.comparison_constraint) )
  (export "hydra.encode.query.edge" (func $hydra.encode.query.edge) )
  (export "hydra.encode.query.graph_pattern" (func $hydra.encode.query.graph_pattern) )
  (export "hydra.encode.query.node" (func $hydra.encode.query.node) )
  (export "hydra.encode.query.path" (func $hydra.encode.query.path) )
  (export "hydra.encode.query.path_equation" (func $hydra.encode.query.path_equation) )
  (export "hydra.encode.query.pattern" (func $hydra.encode.query.pattern) )
  (export "hydra.encode.query.pattern_implication" (func $hydra.encode.query.pattern_implication) )
  (export "hydra.encode.query.query" (func $hydra.encode.query.query) )
  (export "hydra.encode.query.range" (func $hydra.encode.query.range) )
  (export "hydra.encode.query.regex_quantifier" (func $hydra.encode.query.regex_quantifier) )
  (export "hydra.encode.query.regex_sequence" (func $hydra.encode.query.regex_sequence) )
  (export "hydra.encode.query.step" (func $hydra.encode.query.step) )
  (export "hydra.encode.query.triple_pattern" (func $hydra.encode.query.triple_pattern) )
  (export "hydra.encode.query.variable" (func $hydra.encode.query.variable) )
  (func $hydra.encode.query.comparison_constraint (param $arg_0 i32) (result i32)
  (block $end_comparison_constraint (result i32)
  (block $greater_than_or_equal
  (block $less_than_or_equal
  (block $greater_than
  (block $less_than
  (block $not_equal
  (block $equal
  local.get $arg_0
  br_table $equal $not_equal $less_than $greater_than $less_than_or_equal $greater_than_or_equal $greater_than_or_equal
)
  i32.const 0 ;; string: "hydra.query.ComparisonConstraint"
  i32.const 0 ;; string: "equal"
  i32.const 0
  br $end_comparison_constraint
)
  i32.const 0 ;; string: "hydra.query.ComparisonConstraint"
  i32.const 0 ;; string: "notEqual"
  i32.const 0
  br $end_comparison_constraint
)
  i32.const 0 ;; string: "hydra.query.ComparisonConstraint"
  i32.const 0 ;; string: "lessThan"
  i32.const 0
  br $end_comparison_constraint
)
  i32.const 0 ;; string: "hydra.query.ComparisonConstraint"
  i32.const 0 ;; string: "greaterThan"
  i32.const 0
  br $end_comparison_constraint
)
  i32.const 0 ;; string: "hydra.query.ComparisonConstraint"
  i32.const 0 ;; string: "lessThanOrEqual"
  i32.const 0
  br $end_comparison_constraint
)
  i32.const 0 ;; string: "hydra.query.ComparisonConstraint"
  i32.const 0 ;; string: "greaterThanOrEqual"
  i32.const 0
  br $end_comparison_constraint
)
)
  (func $hydra.encode.query.edge (param $x i32) (result i32)
  (local $opt i32)
  i32.const 0 ;; string: "hydra.query.Edge"
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "type"
  local.get $x
  ;; project field: type
  call $hydra.encode.core.name
  i32.const 0 ;; string: "out"
  call $hydra.encode.core.name
  local.get $opt
  call $hydra.lib.maybes.map
  i32.const 0 ;; string: "in"
  call $hydra.encode.core.name
  local.get $opt
  call $hydra.lib.maybes.map
)
  (func $hydra.encode.query.graph_pattern (param $x i32) (result i32)
  (local $xs i32)
  i32.const 0 ;; string: "hydra.query.GraphPattern"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "graph"
  local.get $x
  ;; project field: graph
  call $hydra.encode.core.name
  i32.const 0 ;; string: "patterns"
  call $hydra.encode.query.pattern
  local.get $xs
  call $hydra.lib.lists.map
)
  (func $hydra.encode.query.node (param $arg_0 i32) (result i32)
  (local $y i32)
  (block $end_node (result i32)
  (block $wildcard
  (block $variable
  (block $term
  local.get $arg_0
  br_table $term $variable $wildcard $wildcard
)
  i32.const 0 ;; string: "hydra.query.Node"
  i32.const 0 ;; string: "term"
  local.get $y
  call $hydra.encode.core.term
  br $end_node
)
  i32.const 0 ;; string: "hydra.query.Node"
  i32.const 0 ;; string: "variable"
  local.get $y
  call $hydra.encode.query.variable
  br $end_node
)
  i32.const 0 ;; string: "hydra.query.Node"
  i32.const 0 ;; string: "wildcard"
  i32.const 0
  br $end_node
)
)
  (func $hydra.encode.query.path (param $arg_0 i32) (result i32)
  (local $y i32)
  (block $end_path (result i32)
  (block $inverse
  (block $regex
  (block $step
  local.get $arg_0
  br_table $step $regex $inverse $inverse
)
  i32.const 0 ;; string: "hydra.query.Path"
  i32.const 0 ;; string: "step"
  local.get $y
  call $hydra.encode.query.step
  br $end_path
)
  i32.const 0 ;; string: "hydra.query.Path"
  i32.const 0 ;; string: "regex"
  local.get $y
  call $hydra.encode.query.regex_sequence
  br $end_path
)
  i32.const 0 ;; string: "hydra.query.Path"
  i32.const 0 ;; string: "inverse"
  local.get $y
  call $hydra.encode.query.path
  br $end_path
)
)
  (func $hydra.encode.query.path_equation (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.query.PathEquation"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "left"
  local.get $x
  ;; project field: left
  call $hydra.encode.query.path
  i32.const 0 ;; string: "right"
  local.get $x
  ;; project field: right
  call $hydra.encode.query.path
)
  (func $hydra.encode.query.pattern (param $arg_0 i32) (result i32)
  (local $xs i32)
  (local $y i32)
  (block $end_pattern (result i32)
  (block $graph
  (block $disjunction
  (block $conjunction
  (block $negation
  (block $triple
  local.get $arg_0
  br_table $triple $negation $conjunction $disjunction $graph $graph
)
  i32.const 0 ;; string: "hydra.query.Pattern"
  i32.const 0 ;; string: "triple"
  local.get $y
  call $hydra.encode.query.triple_pattern
  br $end_pattern
)
  i32.const 0 ;; string: "hydra.query.Pattern"
  i32.const 0 ;; string: "negation"
  local.get $y
  call $hydra.encode.query.pattern
  br $end_pattern
)
  i32.const 0 ;; string: "hydra.query.Pattern"
  i32.const 0 ;; string: "conjunction"
  call $hydra.encode.query.pattern
  local.get $xs
  call $hydra.lib.lists.map
  br $end_pattern
)
  i32.const 0 ;; string: "hydra.query.Pattern"
  i32.const 0 ;; string: "disjunction"
  call $hydra.encode.query.pattern
  local.get $xs
  call $hydra.lib.lists.map
  br $end_pattern
)
  i32.const 0 ;; string: "hydra.query.Pattern"
  i32.const 0 ;; string: "graph"
  local.get $y
  call $hydra.encode.query.graph_pattern
  br $end_pattern
)
)
  (func $hydra.encode.query.pattern_implication (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.query.PatternImplication"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "antecedent"
  local.get $x
  ;; project field: antecedent
  call $hydra.encode.query.pattern
  i32.const 0 ;; string: "consequent"
  local.get $x
  ;; project field: consequent
  call $hydra.encode.query.pattern
)
  (func $hydra.encode.query.query (param $x i32) (result i32)
  (local $xs i32)
  i32.const 0 ;; string: "hydra.query.Query"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "variables"
  call $hydra.encode.query.variable
  local.get $xs
  call $hydra.lib.lists.map
  i32.const 0 ;; string: "patterns"
  call $hydra.encode.query.pattern
  local.get $xs
  call $hydra.lib.lists.map
)
  (func $hydra.encode.query.range (param $x i32) (result i32)
  (local $x2 i32)
  i32.const 0 ;; string: "hydra.query.Range"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "min"
  local.get $x2
  i32.const 0 ;; string: "max"
  local.get $x2
)
  (func $hydra.encode.query.regex_quantifier (param $arg_0 i32) (result i32)
  (local $x i32)
  (local $y i32)
  (block $end_regex_quantifier (result i32)
  (block $range
  (block $at_least
  (block $exactly
  (block $one_or_more
  (block $zero_or_more
  (block $zero_or_one
  (block $one
  local.get $arg_0
  br_table $one $zero_or_one $zero_or_more $one_or_more $exactly $at_least $range $range
)
  i32.const 0 ;; string: "hydra.query.RegexQuantifier"
  i32.const 0 ;; string: "one"
  i32.const 0
  br $end_regex_quantifier
)
  i32.const 0 ;; string: "hydra.query.RegexQuantifier"
  i32.const 0 ;; string: "zeroOrOne"
  i32.const 0
  br $end_regex_quantifier
)
  i32.const 0 ;; string: "hydra.query.RegexQuantifier"
  i32.const 0 ;; string: "zeroOrMore"
  i32.const 0
  br $end_regex_quantifier
)
  i32.const 0 ;; string: "hydra.query.RegexQuantifier"
  i32.const 0 ;; string: "oneOrMore"
  i32.const 0
  br $end_regex_quantifier
)
  i32.const 0 ;; string: "hydra.query.RegexQuantifier"
  i32.const 0 ;; string: "exactly"
  local.get $x
  br $end_regex_quantifier
)
  i32.const 0 ;; string: "hydra.query.RegexQuantifier"
  i32.const 0 ;; string: "atLeast"
  local.get $x
  br $end_regex_quantifier
)
  i32.const 0 ;; string: "hydra.query.RegexQuantifier"
  i32.const 0 ;; string: "range"
  local.get $y
  call $hydra.encode.query.range
  br $end_regex_quantifier
)
)
  (func $hydra.encode.query.regex_sequence (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.query.RegexSequence"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "path"
  local.get $x
  ;; project field: path
  call $hydra.encode.query.path
  i32.const 0 ;; string: "quantifier"
  local.get $x
  ;; project field: quantifier
  call $hydra.encode.query.regex_quantifier
)
  (func $hydra.encode.query.step (param $arg_0 i32) (result i32)
  (local $y i32)
  (block $end_step (result i32)
  (block $compare
  (block $project
  (block $edge
  local.get $arg_0
  br_table $edge $project $compare $compare
)
  i32.const 0 ;; string: "hydra.query.Step"
  i32.const 0 ;; string: "edge"
  local.get $y
  call $hydra.encode.query.edge
  br $end_step
)
  i32.const 0 ;; string: "hydra.query.Step"
  i32.const 0 ;; string: "project"
  local.get $y
  call $hydra.encode.core.projection
  br $end_step
)
  i32.const 0 ;; string: "hydra.query.Step"
  i32.const 0 ;; string: "compare"
  local.get $y
  call $hydra.encode.query.comparison_constraint
  br $end_step
)
)
  (func $hydra.encode.query.triple_pattern (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.query.TriplePattern"
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "subject"
  local.get $x
  ;; project field: subject
  call $hydra.encode.query.node
  i32.const 0 ;; string: "predicate"
  local.get $x
  ;; project field: predicate
  call $hydra.encode.query.path
  i32.const 0 ;; string: "object"
  local.get $x
  ;; project field: object
  call $hydra.encode.query.node
)
  (func $hydra.encode.query.variable (param $x i32) (result i32)
  (local $x2 i32)
  i32.const 0 ;; string: "hydra.query.Variable"
  local.get $x2
)
)
