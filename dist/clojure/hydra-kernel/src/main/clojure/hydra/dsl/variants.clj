(ns hydra.dsl.variants
  (:require [hydra.core :refer :all] [hydra.phantoms :refer :all]
))

(declare hydra_dsl_variants_elimination_variant_record hydra_dsl_variants_elimination_variant_union hydra_dsl_variants_elimination_variant_wrap hydra_dsl_variants_function_variant_elimination hydra_dsl_variants_function_variant_lambda hydra_dsl_variants_literal_variant_binary hydra_dsl_variants_literal_variant_boolean hydra_dsl_variants_literal_variant_decimal hydra_dsl_variants_literal_variant_float hydra_dsl_variants_literal_variant_integer hydra_dsl_variants_literal_variant_string hydra_dsl_variants_term_variant_annotated hydra_dsl_variants_term_variant_application hydra_dsl_variants_term_variant_cases hydra_dsl_variants_term_variant_either hydra_dsl_variants_term_variant_inject hydra_dsl_variants_term_variant_lambda hydra_dsl_variants_term_variant_let hydra_dsl_variants_term_variant_list hydra_dsl_variants_term_variant_literal hydra_dsl_variants_term_variant_map hydra_dsl_variants_term_variant_maybe hydra_dsl_variants_term_variant_pair hydra_dsl_variants_term_variant_project hydra_dsl_variants_term_variant_record hydra_dsl_variants_term_variant_set hydra_dsl_variants_term_variant_type_application hydra_dsl_variants_term_variant_type_lambda hydra_dsl_variants_term_variant_unit hydra_dsl_variants_term_variant_unwrap hydra_dsl_variants_term_variant_variable hydra_dsl_variants_term_variant_wrap hydra_dsl_variants_type_variant_annotated hydra_dsl_variants_type_variant_application hydra_dsl_variants_type_variant_either hydra_dsl_variants_type_variant_forall hydra_dsl_variants_type_variant_function hydra_dsl_variants_type_variant_list hydra_dsl_variants_type_variant_literal hydra_dsl_variants_type_variant_map hydra_dsl_variants_type_variant_maybe hydra_dsl_variants_type_variant_pair hydra_dsl_variants_type_variant_record hydra_dsl_variants_type_variant_set hydra_dsl_variants_type_variant_union hydra_dsl_variants_type_variant_unit hydra_dsl_variants_type_variant_variable hydra_dsl_variants_type_variant_void hydra_dsl_variants_type_variant_wrap)

(def hydra_dsl_variants_elimination_variant_record (list :inject (->hydra_core_injection "hydra.variants.EliminationVariant" (->hydra_core_field "record" (list :unit nil)))))

(def hydra_dsl_variants_elimination_variant_union (list :inject (->hydra_core_injection "hydra.variants.EliminationVariant" (->hydra_core_field "union" (list :unit nil)))))

(def hydra_dsl_variants_elimination_variant_wrap (list :inject (->hydra_core_injection "hydra.variants.EliminationVariant" (->hydra_core_field "wrap" (list :unit nil)))))

(def hydra_dsl_variants_function_variant_elimination (list :inject (->hydra_core_injection "hydra.variants.FunctionVariant" (->hydra_core_field "elimination" (list :unit nil)))))

(def hydra_dsl_variants_function_variant_lambda (list :inject (->hydra_core_injection "hydra.variants.FunctionVariant" (->hydra_core_field "lambda" (list :unit nil)))))

(def hydra_dsl_variants_literal_variant_binary (list :inject (->hydra_core_injection "hydra.variants.LiteralVariant" (->hydra_core_field "binary" (list :unit nil)))))

(def hydra_dsl_variants_literal_variant_boolean (list :inject (->hydra_core_injection "hydra.variants.LiteralVariant" (->hydra_core_field "boolean" (list :unit nil)))))

(def hydra_dsl_variants_literal_variant_decimal (list :inject (->hydra_core_injection "hydra.variants.LiteralVariant" (->hydra_core_field "decimal" (list :unit nil)))))

(def hydra_dsl_variants_literal_variant_float (list :inject (->hydra_core_injection "hydra.variants.LiteralVariant" (->hydra_core_field "float" (list :unit nil)))))

(def hydra_dsl_variants_literal_variant_integer (list :inject (->hydra_core_injection "hydra.variants.LiteralVariant" (->hydra_core_field "integer" (list :unit nil)))))

(def hydra_dsl_variants_literal_variant_string (list :inject (->hydra_core_injection "hydra.variants.LiteralVariant" (->hydra_core_field "string" (list :unit nil)))))

(def hydra_dsl_variants_term_variant_annotated (list :inject (->hydra_core_injection "hydra.variants.TermVariant" (->hydra_core_field "annotated" (list :unit nil)))))

(def hydra_dsl_variants_term_variant_application (list :inject (->hydra_core_injection "hydra.variants.TermVariant" (->hydra_core_field "application" (list :unit nil)))))

(def hydra_dsl_variants_term_variant_cases (list :inject (->hydra_core_injection "hydra.variants.TermVariant" (->hydra_core_field "cases" (list :unit nil)))))

(def hydra_dsl_variants_term_variant_either (list :inject (->hydra_core_injection "hydra.variants.TermVariant" (->hydra_core_field "either" (list :unit nil)))))

(def hydra_dsl_variants_term_variant_inject (list :inject (->hydra_core_injection "hydra.variants.TermVariant" (->hydra_core_field "inject" (list :unit nil)))))

(def hydra_dsl_variants_term_variant_lambda (list :inject (->hydra_core_injection "hydra.variants.TermVariant" (->hydra_core_field "lambda" (list :unit nil)))))

(def hydra_dsl_variants_term_variant_let (list :inject (->hydra_core_injection "hydra.variants.TermVariant" (->hydra_core_field "let" (list :unit nil)))))

(def hydra_dsl_variants_term_variant_list (list :inject (->hydra_core_injection "hydra.variants.TermVariant" (->hydra_core_field "list" (list :unit nil)))))

(def hydra_dsl_variants_term_variant_literal (list :inject (->hydra_core_injection "hydra.variants.TermVariant" (->hydra_core_field "literal" (list :unit nil)))))

(def hydra_dsl_variants_term_variant_map (list :inject (->hydra_core_injection "hydra.variants.TermVariant" (->hydra_core_field "map" (list :unit nil)))))

(def hydra_dsl_variants_term_variant_maybe (list :inject (->hydra_core_injection "hydra.variants.TermVariant" (->hydra_core_field "maybe" (list :unit nil)))))

(def hydra_dsl_variants_term_variant_pair (list :inject (->hydra_core_injection "hydra.variants.TermVariant" (->hydra_core_field "pair" (list :unit nil)))))

(def hydra_dsl_variants_term_variant_project (list :inject (->hydra_core_injection "hydra.variants.TermVariant" (->hydra_core_field "project" (list :unit nil)))))

(def hydra_dsl_variants_term_variant_record (list :inject (->hydra_core_injection "hydra.variants.TermVariant" (->hydra_core_field "record" (list :unit nil)))))

(def hydra_dsl_variants_term_variant_set (list :inject (->hydra_core_injection "hydra.variants.TermVariant" (->hydra_core_field "set" (list :unit nil)))))

(def hydra_dsl_variants_term_variant_type_application (list :inject (->hydra_core_injection "hydra.variants.TermVariant" (->hydra_core_field "typeApplication" (list :unit nil)))))

(def hydra_dsl_variants_term_variant_type_lambda (list :inject (->hydra_core_injection "hydra.variants.TermVariant" (->hydra_core_field "typeLambda" (list :unit nil)))))

(def hydra_dsl_variants_term_variant_unit (list :inject (->hydra_core_injection "hydra.variants.TermVariant" (->hydra_core_field "unit" (list :unit nil)))))

(def hydra_dsl_variants_term_variant_unwrap (list :inject (->hydra_core_injection "hydra.variants.TermVariant" (->hydra_core_field "unwrap" (list :unit nil)))))

(def hydra_dsl_variants_term_variant_variable (list :inject (->hydra_core_injection "hydra.variants.TermVariant" (->hydra_core_field "variable" (list :unit nil)))))

(def hydra_dsl_variants_term_variant_wrap (list :inject (->hydra_core_injection "hydra.variants.TermVariant" (->hydra_core_field "wrap" (list :unit nil)))))

(def hydra_dsl_variants_type_variant_annotated (list :inject (->hydra_core_injection "hydra.variants.TypeVariant" (->hydra_core_field "annotated" (list :unit nil)))))

(def hydra_dsl_variants_type_variant_application (list :inject (->hydra_core_injection "hydra.variants.TypeVariant" (->hydra_core_field "application" (list :unit nil)))))

(def hydra_dsl_variants_type_variant_either (list :inject (->hydra_core_injection "hydra.variants.TypeVariant" (->hydra_core_field "either" (list :unit nil)))))

(def hydra_dsl_variants_type_variant_forall (list :inject (->hydra_core_injection "hydra.variants.TypeVariant" (->hydra_core_field "forall" (list :unit nil)))))

(def hydra_dsl_variants_type_variant_function (list :inject (->hydra_core_injection "hydra.variants.TypeVariant" (->hydra_core_field "function" (list :unit nil)))))

(def hydra_dsl_variants_type_variant_list (list :inject (->hydra_core_injection "hydra.variants.TypeVariant" (->hydra_core_field "list" (list :unit nil)))))

(def hydra_dsl_variants_type_variant_literal (list :inject (->hydra_core_injection "hydra.variants.TypeVariant" (->hydra_core_field "literal" (list :unit nil)))))

(def hydra_dsl_variants_type_variant_map (list :inject (->hydra_core_injection "hydra.variants.TypeVariant" (->hydra_core_field "map" (list :unit nil)))))

(def hydra_dsl_variants_type_variant_maybe (list :inject (->hydra_core_injection "hydra.variants.TypeVariant" (->hydra_core_field "maybe" (list :unit nil)))))

(def hydra_dsl_variants_type_variant_pair (list :inject (->hydra_core_injection "hydra.variants.TypeVariant" (->hydra_core_field "pair" (list :unit nil)))))

(def hydra_dsl_variants_type_variant_record (list :inject (->hydra_core_injection "hydra.variants.TypeVariant" (->hydra_core_field "record" (list :unit nil)))))

(def hydra_dsl_variants_type_variant_set (list :inject (->hydra_core_injection "hydra.variants.TypeVariant" (->hydra_core_field "set" (list :unit nil)))))

(def hydra_dsl_variants_type_variant_union (list :inject (->hydra_core_injection "hydra.variants.TypeVariant" (->hydra_core_field "union" (list :unit nil)))))

(def hydra_dsl_variants_type_variant_unit (list :inject (->hydra_core_injection "hydra.variants.TypeVariant" (->hydra_core_field "unit" (list :unit nil)))))

(def hydra_dsl_variants_type_variant_variable (list :inject (->hydra_core_injection "hydra.variants.TypeVariant" (->hydra_core_field "variable" (list :unit nil)))))

(def hydra_dsl_variants_type_variant_void (list :inject (->hydra_core_injection "hydra.variants.TypeVariant" (->hydra_core_field "void" (list :unit nil)))))

(def hydra_dsl_variants_type_variant_wrap (list :inject (->hydra_core_injection "hydra.variants.TypeVariant" (->hydra_core_field "wrap" (list :unit nil)))))
