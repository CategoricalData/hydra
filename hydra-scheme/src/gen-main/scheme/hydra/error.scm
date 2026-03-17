(define-library (hydra error)
(export make-hydra_error_decoding_error hydra_error_decoding_error? hydra_error_decoding_error-value hydra_error_error-variants make-hydra_error_other_error hydra_error_other_error? hydra_error_other_error-value make-hydra_error_unification_error hydra_error_unification_error? hydra_error_unification_error-left_type hydra_error_unification_error-right_type hydra_error_unification_error-message)
(import (scheme base) (hydra core))
(begin
(define-record-type hydra_error_decoding_error (make-hydra_error_decoding_error value) hydra_error_decoding_error? (value hydra_error_decoding_error-value))
(define hydra_error_error-variants (list 'decoding 'other 'unification))
(define-record-type hydra_error_other_error (make-hydra_error_other_error value) hydra_error_other_error? (value hydra_error_other_error-value))
(define-record-type hydra_error_unification_error (make-hydra_error_unification_error left_type right_type message) hydra_error_unification_error? (left_type hydra_error_unification_error-left_type) (right_type hydra_error_unification_error-right_type) (message hydra_error_unification_error-message))))
