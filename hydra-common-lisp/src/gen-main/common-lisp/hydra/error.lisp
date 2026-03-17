(defpackage :hydra.error
(:use :cl :hydra.core)
(:export :make-hydra_error_decoding_error :hydra_error_decoding_error? :hydra_error_decoding_error-value :hydra_error_error-variants :make-hydra_error_other_error :hydra_error_other_error? :hydra_error_other_error-value :make-hydra_error_unification_error :hydra_error_unification_error? :hydra_error_unification_error-left_type :hydra_error_unification_error-right_type :hydra_error_unification_error-message))

(in-package :hydra.error)

(cl:defstruct hydra_error_decoding_error value)

(cl:defvar hydra_error_error-variants (cl:list :decoding :other :unification))

(cl:defstruct hydra_error_other_error value)

(cl:defstruct hydra_error_unification_error left_type right_type message)
