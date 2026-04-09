(module
  (memory $memory 1 )
  (export "memory" (memory $memory) )
  (export "hydra.encode.variants.elimination_variant" (func $hydra.encode.variants.elimination_variant) )
  (export "hydra.encode.variants.function_variant" (func $hydra.encode.variants.function_variant) )
  (export "hydra.encode.variants.literal_variant" (func $hydra.encode.variants.literal_variant) )
  (export "hydra.encode.variants.term_variant" (func $hydra.encode.variants.term_variant) )
  (export "hydra.encode.variants.type_variant" (func $hydra.encode.variants.type_variant) )
  (func $hydra.encode.variants.elimination_variant (param $arg_0 i32) (result i32)
  (block $end_elimination_variant (result i32)
  (block $wrap
  (block $union
  (block $record
  local.get $arg_0
  br_table $record $union $wrap $wrap
)
  i32.const 0 ;; string: "hydra.variants.EliminationVariant"
  i32.const 0 ;; string: "record"
  i32.const 0
  br $end_elimination_variant
)
  i32.const 0 ;; string: "hydra.variants.EliminationVariant"
  i32.const 0 ;; string: "union"
  i32.const 0
  br $end_elimination_variant
)
  i32.const 0 ;; string: "hydra.variants.EliminationVariant"
  i32.const 0 ;; string: "wrap"
  i32.const 0
  br $end_elimination_variant
)
)
  (func $hydra.encode.variants.function_variant (param $arg_0 i32) (result i32)
  (block $end_function_variant (result i32)
  (block $lambda
  (block $elimination
  local.get $arg_0
  br_table $elimination $lambda $lambda
)
  i32.const 0 ;; string: "hydra.variants.FunctionVariant"
  i32.const 0 ;; string: "elimination"
  i32.const 0
  br $end_function_variant
)
  i32.const 0 ;; string: "hydra.variants.FunctionVariant"
  i32.const 0 ;; string: "lambda"
  i32.const 0
  br $end_function_variant
)
)
  (func $hydra.encode.variants.literal_variant (param $arg_0 i32) (result i32)
  (block $end_literal_variant (result i32)
  (block $string
  (block $integer
  (block $float
  (block $boolean
  (block $binary
  local.get $arg_0
  br_table $binary $boolean $float $integer $string $string
)
  i32.const 0 ;; string: "hydra.variants.LiteralVariant"
  i32.const 0 ;; string: "binary"
  i32.const 0
  br $end_literal_variant
)
  i32.const 0 ;; string: "hydra.variants.LiteralVariant"
  i32.const 0 ;; string: "boolean"
  i32.const 0
  br $end_literal_variant
)
  i32.const 0 ;; string: "hydra.variants.LiteralVariant"
  i32.const 0 ;; string: "float"
  i32.const 0
  br $end_literal_variant
)
  i32.const 0 ;; string: "hydra.variants.LiteralVariant"
  i32.const 0 ;; string: "integer"
  i32.const 0
  br $end_literal_variant
)
  i32.const 0 ;; string: "hydra.variants.LiteralVariant"
  i32.const 0 ;; string: "string"
  i32.const 0
  br $end_literal_variant
)
)
  (func $hydra.encode.variants.term_variant (param $arg_0 i32) (result i32)
  (block $end_term_variant (result i32)
  (block $wrap
  (block $variable
  (block $unit
  (block $union
  (block $type_lambda
  (block $type_application
  (block $set
  (block $record
  (block $pair
  (block $maybe
  (block $map
  (block $literal
  (block $list
  (block $let
  (block $function
  (block $either
  (block $application
  (block $annotated
  local.get $arg_0
  br_table $annotated $application $either $function $let $list $literal $map $maybe $pair $record $set $type_application $type_lambda $union $unit $variable $wrap $wrap
)
  i32.const 0 ;; string: "hydra.variants.TermVariant"
  i32.const 0 ;; string: "annotated"
  i32.const 0
  br $end_term_variant
)
  i32.const 0 ;; string: "hydra.variants.TermVariant"
  i32.const 0 ;; string: "application"
  i32.const 0
  br $end_term_variant
)
  i32.const 0 ;; string: "hydra.variants.TermVariant"
  i32.const 0 ;; string: "either"
  i32.const 0
  br $end_term_variant
)
  i32.const 0 ;; string: "hydra.variants.TermVariant"
  i32.const 0 ;; string: "function"
  i32.const 0
  br $end_term_variant
)
  i32.const 0 ;; string: "hydra.variants.TermVariant"
  i32.const 0 ;; string: "let"
  i32.const 0
  br $end_term_variant
)
  i32.const 0 ;; string: "hydra.variants.TermVariant"
  i32.const 0 ;; string: "list"
  i32.const 0
  br $end_term_variant
)
  i32.const 0 ;; string: "hydra.variants.TermVariant"
  i32.const 0 ;; string: "literal"
  i32.const 0
  br $end_term_variant
)
  i32.const 0 ;; string: "hydra.variants.TermVariant"
  i32.const 0 ;; string: "map"
  i32.const 0
  br $end_term_variant
)
  i32.const 0 ;; string: "hydra.variants.TermVariant"
  i32.const 0 ;; string: "maybe"
  i32.const 0
  br $end_term_variant
)
  i32.const 0 ;; string: "hydra.variants.TermVariant"
  i32.const 0 ;; string: "pair"
  i32.const 0
  br $end_term_variant
)
  i32.const 0 ;; string: "hydra.variants.TermVariant"
  i32.const 0 ;; string: "record"
  i32.const 0
  br $end_term_variant
)
  i32.const 0 ;; string: "hydra.variants.TermVariant"
  i32.const 0 ;; string: "set"
  i32.const 0
  br $end_term_variant
)
  i32.const 0 ;; string: "hydra.variants.TermVariant"
  i32.const 0 ;; string: "typeApplication"
  i32.const 0
  br $end_term_variant
)
  i32.const 0 ;; string: "hydra.variants.TermVariant"
  i32.const 0 ;; string: "typeLambda"
  i32.const 0
  br $end_term_variant
)
  i32.const 0 ;; string: "hydra.variants.TermVariant"
  i32.const 0 ;; string: "union"
  i32.const 0
  br $end_term_variant
)
  i32.const 0 ;; string: "hydra.variants.TermVariant"
  i32.const 0 ;; string: "unit"
  i32.const 0
  br $end_term_variant
)
  i32.const 0 ;; string: "hydra.variants.TermVariant"
  i32.const 0 ;; string: "variable"
  i32.const 0
  br $end_term_variant
)
  i32.const 0 ;; string: "hydra.variants.TermVariant"
  i32.const 0 ;; string: "wrap"
  i32.const 0
  br $end_term_variant
)
)
  (func $hydra.encode.variants.type_variant (param $arg_0 i32) (result i32)
  (block $end_type_variant (result i32)
  (block $wrap
  (block $void
  (block $variable
  (block $unit
  (block $union
  (block $set
  (block $record
  (block $pair
  (block $maybe
  (block $map
  (block $literal
  (block $list
  (block $function
  (block $forall
  (block $either
  (block $application
  (block $annotated
  local.get $arg_0
  br_table $annotated $application $either $forall $function $list $literal $map $maybe $pair $record $set $union $unit $variable $void $wrap $wrap
)
  i32.const 0 ;; string: "hydra.variants.TypeVariant"
  i32.const 0 ;; string: "annotated"
  i32.const 0
  br $end_type_variant
)
  i32.const 0 ;; string: "hydra.variants.TypeVariant"
  i32.const 0 ;; string: "application"
  i32.const 0
  br $end_type_variant
)
  i32.const 0 ;; string: "hydra.variants.TypeVariant"
  i32.const 0 ;; string: "either"
  i32.const 0
  br $end_type_variant
)
  i32.const 0 ;; string: "hydra.variants.TypeVariant"
  i32.const 0 ;; string: "forall"
  i32.const 0
  br $end_type_variant
)
  i32.const 0 ;; string: "hydra.variants.TypeVariant"
  i32.const 0 ;; string: "function"
  i32.const 0
  br $end_type_variant
)
  i32.const 0 ;; string: "hydra.variants.TypeVariant"
  i32.const 0 ;; string: "list"
  i32.const 0
  br $end_type_variant
)
  i32.const 0 ;; string: "hydra.variants.TypeVariant"
  i32.const 0 ;; string: "literal"
  i32.const 0
  br $end_type_variant
)
  i32.const 0 ;; string: "hydra.variants.TypeVariant"
  i32.const 0 ;; string: "map"
  i32.const 0
  br $end_type_variant
)
  i32.const 0 ;; string: "hydra.variants.TypeVariant"
  i32.const 0 ;; string: "maybe"
  i32.const 0
  br $end_type_variant
)
  i32.const 0 ;; string: "hydra.variants.TypeVariant"
  i32.const 0 ;; string: "pair"
  i32.const 0
  br $end_type_variant
)
  i32.const 0 ;; string: "hydra.variants.TypeVariant"
  i32.const 0 ;; string: "record"
  i32.const 0
  br $end_type_variant
)
  i32.const 0 ;; string: "hydra.variants.TypeVariant"
  i32.const 0 ;; string: "set"
  i32.const 0
  br $end_type_variant
)
  i32.const 0 ;; string: "hydra.variants.TypeVariant"
  i32.const 0 ;; string: "union"
  i32.const 0
  br $end_type_variant
)
  i32.const 0 ;; string: "hydra.variants.TypeVariant"
  i32.const 0 ;; string: "unit"
  i32.const 0
  br $end_type_variant
)
  i32.const 0 ;; string: "hydra.variants.TypeVariant"
  i32.const 0 ;; string: "variable"
  i32.const 0
  br $end_type_variant
)
  i32.const 0 ;; string: "hydra.variants.TypeVariant"
  i32.const 0 ;; string: "void"
  i32.const 0
  br $end_type_variant
)
  i32.const 0 ;; string: "hydra.variants.TypeVariant"
  i32.const 0 ;; string: "wrap"
  i32.const 0
  br $end_type_variant
)
)
)
