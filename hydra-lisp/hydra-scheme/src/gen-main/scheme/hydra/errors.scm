(define-library (hydra errors)
(export make-hydra_errors_decoding_error hydra_errors_decoding_error? hydra_errors_decoding_error-value hydra_errors_error-variants make-hydra_errors_other_error hydra_errors_other_error? hydra_errors_other_error-value make-hydra_errors_unification_error hydra_errors_unification_error? hydra_errors_unification_error-left_type hydra_errors_unification_error-right_type hydra_errors_unification_error-message)
(import (scheme base) (hydra core) (hydra error checking) (hydra error core))
(begin
(define-record-type hydra_errors_decoding_error (make-hydra_errors_decoding_error value) hydra_errors_decoding_error? (value hydra_errors_decoding_error-value))
(define hydra_errors_error-variants (list 'checking 'decoding 'duplicate_binding 'duplicate_field 'other 'undefined_field 'undefined_term 'undefined_type 'unexpected_term_variant 'unexpected_type_variant 'unification))
(define-record-type hydra_errors_other_error (make-hydra_errors_other_error value) hydra_errors_other_error? (value hydra_errors_other_error-value))
(define-record-type hydra_errors_unification_error (make-hydra_errors_unification_error left_type right_type message) hydra_errors_unification_error? (left_type hydra_errors_unification_error-left_type) (right_type hydra_errors_unification_error-right_type) (message hydra_errors_unification_error-message))))
