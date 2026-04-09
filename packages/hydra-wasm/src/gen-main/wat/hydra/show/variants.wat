(module
  (memory $memory 1 )
  (export "memory" (memory $memory) )
  (export "hydra.show.variants.term_variant" (func $hydra.show.variants.term_variant) )
  (export "hydra.show.variants.type_variant" (func $hydra.show.variants.type_variant) )
  (func $hydra.show.variants.term_variant (param $arg_0 i32) (result i32)
  (block $end_term_variant (result i32)
  (block $wrap
  (block $variable
  (block $unit
  (block $union
  (block $type_application
  (block $type_lambda
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
  br_table $annotated $application $either $function $let $list $literal $map $maybe $pair $record $set $type_lambda $type_application $union $unit $variable $wrap $wrap
)
  i32.const 0 ;; string: "annotated"
  br $end_term_variant
)
  i32.const 0 ;; string: "application"
  br $end_term_variant
)
  i32.const 0 ;; string: "either"
  br $end_term_variant
)
  i32.const 0 ;; string: "function"
  br $end_term_variant
)
  i32.const 0 ;; string: "let"
  br $end_term_variant
)
  i32.const 0 ;; string: "list"
  br $end_term_variant
)
  i32.const 0 ;; string: "literal"
  br $end_term_variant
)
  i32.const 0 ;; string: "map"
  br $end_term_variant
)
  i32.const 0 ;; string: "maybe"
  br $end_term_variant
)
  i32.const 0 ;; string: "pair"
  br $end_term_variant
)
  i32.const 0 ;; string: "record"
  br $end_term_variant
)
  i32.const 0 ;; string: "set"
  br $end_term_variant
)
  i32.const 0 ;; string: "typeLambda"
  br $end_term_variant
)
  i32.const 0 ;; string: "typeApplication"
  br $end_term_variant
)
  i32.const 0 ;; string: "union"
  br $end_term_variant
)
  i32.const 0 ;; string: "unit"
  br $end_term_variant
)
  i32.const 0 ;; string: "variable"
  br $end_term_variant
)
  i32.const 0 ;; string: "wrap"
  br $end_term_variant
)
)
  (func $hydra.show.variants.type_variant (param $arg_0 i32) (result i32)
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
  i32.const 0 ;; string: "annotated"
  br $end_type_variant
)
  i32.const 0 ;; string: "application"
  br $end_type_variant
)
  i32.const 0 ;; string: "either"
  br $end_type_variant
)
  i32.const 0 ;; string: "forall"
  br $end_type_variant
)
  i32.const 0 ;; string: "function"
  br $end_type_variant
)
  i32.const 0 ;; string: "list"
  br $end_type_variant
)
  i32.const 0 ;; string: "literal"
  br $end_type_variant
)
  i32.const 0 ;; string: "map"
  br $end_type_variant
)
  i32.const 0 ;; string: "maybe"
  br $end_type_variant
)
  i32.const 0 ;; string: "pair"
  br $end_type_variant
)
  i32.const 0 ;; string: "record"
  br $end_type_variant
)
  i32.const 0 ;; string: "set"
  br $end_type_variant
)
  i32.const 0 ;; string: "union"
  br $end_type_variant
)
  i32.const 0 ;; string: "unit"
  br $end_type_variant
)
  i32.const 0 ;; string: "variable"
  br $end_type_variant
)
  i32.const 0 ;; string: "void"
  br $end_type_variant
)
  i32.const 0 ;; string: "wrap"
  br $end_type_variant
)
)
)
