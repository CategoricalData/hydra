(defpackage :hydra.coders
(:use :cl :hydra.compute :hydra.core :hydra.graph :hydra.variants)
(:export :make-hydra_coders_adapter_context :hydra_coders_adapter_context? :hydra_coders_adapter_context-graph :hydra_coders_adapter_context-language :hydra_coders_adapter_context-adapters :hydra_coders_coder_direction-variants :make-hydra_coders_language :hydra_coders_language? :hydra_coders_language-name :hydra_coders_language-constraints :make-hydra_coders_language_constraints :hydra_coders_language_constraints? :hydra_coders_language_constraints-elimination_variants :hydra_coders_language_constraints-literal_variants :hydra_coders_language_constraints-float_types :hydra_coders_language_constraints-function_variants :hydra_coders_language_constraints-integer_types :hydra_coders_language_constraints-term_variants :hydra_coders_language_constraints-type_variants :hydra_coders_language_constraints-types :make-hydra_coders_language_name :hydra_coders_language_name? :hydra_coders_language_name-value :hydra_coders_traversal_order-variants))

(in-package :hydra.coders)

(cl:defstruct hydra_coders_adapter_context graph language adapters)

(cl:defvar hydra_coders_coder_direction-variants (cl:list :encode :decode))

(cl:defstruct hydra_coders_language name constraints)

(cl:defstruct hydra_coders_language_constraints elimination_variants literal_variants float_types function_variants integer_types term_variants type_variants types)

(cl:defstruct hydra_coders_language_name value)

(cl:defvar hydra_coders_traversal_order-variants (cl:list :pre :post))
