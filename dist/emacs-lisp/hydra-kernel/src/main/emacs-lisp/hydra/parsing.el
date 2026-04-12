(require 'cl-lib)

(cl-defstruct hydra_parsing_parse_error message remainder)

(defvar hydra_parsing_parse_result-variants (list :success :failure))

(cl-defstruct hydra_parsing_parse_success value remainder)

(cl-defstruct hydra_parsing_parser value)

(provide 'hydra.parsing)
