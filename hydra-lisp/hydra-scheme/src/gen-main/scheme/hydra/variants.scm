(define-library (hydra variants)
(export hydra_variants_elimination_variant-variants hydra_variants_function_variant-variants hydra_variants_literal_variant-variants hydra_variants_term_variant-variants hydra_variants_type_variant-variants)
(import (scheme base))
(begin
(define hydra_variants_elimination_variant-variants (list 'record 'union 'wrap))
(define hydra_variants_function_variant-variants (list 'elimination 'lambda))
(define hydra_variants_literal_variant-variants (list 'binary 'boolean 'float 'integer 'string))
(define hydra_variants_term_variant-variants (list 'annotated 'application 'either 'function 'let 'list 'literal 'map 'maybe 'pair 'record 'set 'type_application 'type_lambda 'union 'unit 'variable 'wrap))
(define hydra_variants_type_variant-variants (list 'annotated 'application 'either 'forall 'function 'list 'literal 'map 'maybe 'pair 'record 'set 'union 'unit 'variable 'void 'wrap))))
