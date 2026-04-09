(module
  (import "hydra.lib.lists" "hydra.lib.lists.concat2" (func $hydra.lib.lists.concat2 (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.from_list" (func $hydra.lib.sets.from_list (param i32) (result i32) ) )
  (memory $memory 1 )
  (export "memory" (memory $memory) )
  (export "hydra.ext.haskell.language.haskell_language" (func $hydra.ext.haskell.language.haskell_language) )
  (export "hydra.ext.haskell.language.reserved_words" (func $hydra.ext.haskell.language.reserved_words) )
  (func $hydra.ext.haskell.language.haskell_language (result i32)
  (local $elimination_variants i32)
  (local $float_types i32)
  (local $function_variants i32)
  (local $integer_types i32)
  (local $literal_variants i32)
  (local $term_variants i32)
  (local $type_predicate i32)
  (local $type_variants i32)
  i32.const 3
  ;; list elements follow
  i32.const 0
  i32.const 0
  i32.const 0
  call $hydra.lib.sets.from_list
  local.set $elimination_variants
  i32.const 5
  ;; list elements follow
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  call $hydra.lib.sets.from_list
  local.set $literal_variants
  i32.const 2
  ;; list elements follow
  i32.const 0
  i32.const 0
  call $hydra.lib.sets.from_list
  local.set $float_types
  i32.const 2
  ;; list elements follow
  i32.const 0
  i32.const 0
  call $hydra.lib.sets.from_list
  local.set $function_variants
  i32.const 5
  ;; list elements follow
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  call $hydra.lib.sets.from_list
  local.set $integer_types
  i32.const 16
  ;; list elements follow
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  call $hydra.lib.sets.from_list
  local.set $term_variants
  i32.const 17
  ;; list elements follow
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  call $hydra.lib.sets.from_list
  local.set $type_variants
  i32.const 1
  local.set $type_predicate
  i32.const 0 ;; string: "hydra.ext.haskell"
  local.get $elimination_variants
  local.get $literal_variants
  local.get $float_types
  local.get $function_variants
  local.get $integer_types
  local.get $term_variants
  local.get $type_variants
  local.get $type_predicate
)
  (func $hydra.ext.haskell.language.reserved_words (result i32)
  (local $keyword_symbols i32)
  (local $reserved_symbols i32)
  i32.const 23
  ;; list elements follow
  i32.const 0 ;; string: "case"
  i32.const 0 ;; string: "class"
  i32.const 0 ;; string: "data"
  i32.const 0 ;; string: "default"
  i32.const 0 ;; string: "deriving"
  i32.const 0 ;; string: "do"
  i32.const 0 ;; string: "else"
  i32.const 0 ;; string: "forall"
  i32.const 0 ;; string: "foreign"
  i32.const 0 ;; string: "if"
  i32.const 0 ;; string: "import"
  i32.const 0 ;; string: "in"
  i32.const 0 ;; string: "infix"
  i32.const 0 ;; string: "infixl"
  i32.const 0 ;; string: "infixr"
  i32.const 0 ;; string: "instance"
  i32.const 0 ;; string: "let"
  i32.const 0 ;; string: "module"
  i32.const 0 ;; string: "newtype"
  i32.const 0 ;; string: "of"
  i32.const 0 ;; string: "then"
  i32.const 0 ;; string: "type"
  i32.const 0 ;; string: "where"
  local.set $keyword_symbols
  i32.const 13
  ;; list elements follow
  i32.const 0 ;; string: "Bool"
  i32.const 0 ;; string: "Double"
  i32.const 0 ;; string: "False"
  i32.const 0 ;; string: "Float"
  i32.const 0 ;; string: "Int"
  i32.const 0 ;; string: "Integer"
  i32.const 0 ;; string: "Just"
  i32.const 0 ;; string: "Maybe"
  i32.const 0 ;; string: "Nothing"
  i32.const 0 ;; string: "Ord"
  i32.const 0 ;; string: "Show"
  i32.const 0 ;; string: "String"
  i32.const 0 ;; string: "True"
  local.set $reserved_symbols
  local.get $keyword_symbols
  local.get $reserved_symbols
  call $hydra.lib.lists.concat2
  call $hydra.lib.sets.from_list
)
)
