(defpackage :hydra.errors
(:use :cl :hydra.core :hydra.error.checking :hydra.error.core)
(:export :make-hydra_errors_decoding_error :hydra_errors_decoding_error? :hydra_errors_decoding_error-value :hydra_errors_error-variants :make-hydra_errors_other_error :hydra_errors_other_error? :hydra_errors_other_error-value :make-hydra_errors_unification_error :hydra_errors_unification_error? :hydra_errors_unification_error-left_type :hydra_errors_unification_error-right_type :hydra_errors_unification_error-message))

(in-package :hydra.errors)

(cl:defstruct hydra_errors_decoding_error value)

(cl:defvar hydra_errors_error-variants (cl:list :checking :decoding :duplicate_binding :duplicate_field :other :undefined_field :undefined_term_variable :untyped_term_variable :unexpected_term_variant :unexpected_type_variant :unification))

(cl:defstruct hydra_errors_other_error value)

(cl:defstruct hydra_errors_unification_error left_type right_type message)
