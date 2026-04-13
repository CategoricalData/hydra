(defpackage :hydra.parsing
(:use :cl)
(:export :make-hydra_parsing_parse_error :hydra_parsing_parse_error? :hydra_parsing_parse_error-message :hydra_parsing_parse_error-remainder :hydra_parsing_parse_result-variants :make-hydra_parsing_parse_success :hydra_parsing_parse_success? :hydra_parsing_parse_success-value :hydra_parsing_parse_success-remainder :make-hydra_parsing_parser :hydra_parsing_parser? :hydra_parsing_parser-value))

(in-package :hydra.parsing)

(cl:defstruct hydra_parsing_parse_error message remainder)

(cl:defvar hydra_parsing_parse_result-variants (cl:list :success :failure))

(cl:defstruct hydra_parsing_parse_success value remainder)

(cl:defstruct hydra_parsing_parser value)
