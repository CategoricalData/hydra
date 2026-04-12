(define-library (hydra parsing)
(export make-hydra_parsing_parse_error hydra_parsing_parse_error? hydra_parsing_parse_error-message hydra_parsing_parse_error-remainder hydra_parsing_parse_result-variants make-hydra_parsing_parse_success hydra_parsing_parse_success? hydra_parsing_parse_success-value hydra_parsing_parse_success-remainder make-hydra_parsing_parser hydra_parsing_parser? hydra_parsing_parser-value)
(import (scheme base))
(begin
(define-record-type hydra_parsing_parse_error (make-hydra_parsing_parse_error message remainder) hydra_parsing_parse_error? (message hydra_parsing_parse_error-message) (remainder hydra_parsing_parse_error-remainder))
(define hydra_parsing_parse_result-variants (list 'success 'failure))
(define-record-type hydra_parsing_parse_success (make-hydra_parsing_parse_success value remainder) hydra_parsing_parse_success? (value hydra_parsing_parse_success-value) (remainder hydra_parsing_parse_success-remainder))
(define-record-type hydra_parsing_parser (make-hydra_parsing_parser value) hydra_parsing_parser? (value hydra_parsing_parser-value))))
