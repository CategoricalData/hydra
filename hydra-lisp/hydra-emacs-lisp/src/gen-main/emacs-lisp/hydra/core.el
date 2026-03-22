(require 'cl-lib)

(cl-defstruct hydra_core_annotated_term body annotation)

(cl-defstruct hydra_core_annotated_type body annotation)

(cl-defstruct hydra_core_application function argument)

(cl-defstruct hydra_core_application_type function argument)

(cl-defstruct hydra_core_binding name term type)

(cl-defstruct hydra_core_case_statement type_name default cases)

(cl-defstruct hydra_core_either_type left right)

(cl-defstruct hydra_core_pair_type first second)

(defvar hydra_core_elimination-variants (list :record :union :wrap))

(cl-defstruct hydra_core_field name term)

(cl-defstruct hydra_core_field_type name type)

(defvar hydra_core_float_type-variants (list :bigfloat :float32 :float64))

(defvar hydra_core_float_value-variants (list :bigfloat :float32 :float64))

(cl-defstruct hydra_core_forall_type parameter body)

(defvar hydra_core_function-variants (list :elimination :lambda :primitive))

(cl-defstruct hydra_core_function_type domain codomain)

(cl-defstruct hydra_core_injection type_name field)

(defvar hydra_core_integer_type-variants (list :bigint :int8 :int16 :int32 :int64 :uint8 :uint16 :uint32 :uint64))

(defvar hydra_core_integer_value-variants (list :bigint :int8 :int16 :int32 :int64 :uint8 :uint16 :uint32 :uint64))

(cl-defstruct hydra_core_lambda parameter domain body)

(cl-defstruct hydra_core_let bindings body)

(defvar hydra_core_literal-variants (list :binary :boolean :float :integer :string))

(defvar hydra_core_literal_type-variants (list :binary :boolean :float :integer :string))

(cl-defstruct hydra_core_map_type keys values)

(cl-defstruct hydra_core_name value)

(cl-defstruct hydra_core_projection type_name field)

(cl-defstruct hydra_core_record type_name fields)

(defvar hydra_core_term-variants (list :annotated :application :either :function :let :list :literal :map :maybe :pair :record :set :type_application :type_lambda :union :unit :variable :wrap))

(defvar hydra_core_type-variants (list :annotated :application :either :forall :function :list :literal :map :maybe :pair :record :set :union :unit :variable :void :wrap))

(cl-defstruct hydra_core_type_application_term body type)

(cl-defstruct hydra_core_type_lambda parameter body)

(cl-defstruct hydra_core_type_scheme variables type constraints)

(cl-defstruct hydra_core_type_variable_metadata classes)

(cl-defstruct hydra_core_wrapped_term type_name body)

(provide 'hydra.core)
