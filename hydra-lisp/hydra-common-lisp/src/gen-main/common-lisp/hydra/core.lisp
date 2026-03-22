(defpackage :hydra.core
(:use :cl)
(:export :make-hydra_core_annotated_term :hydra_core_annotated_term? :hydra_core_annotated_term-body :hydra_core_annotated_term-annotation :make-hydra_core_annotated_type :hydra_core_annotated_type? :hydra_core_annotated_type-body :hydra_core_annotated_type-annotation :make-hydra_core_application :hydra_core_application? :hydra_core_application-function :hydra_core_application-argument :make-hydra_core_application_type :hydra_core_application_type? :hydra_core_application_type-function :hydra_core_application_type-argument :make-hydra_core_binding :hydra_core_binding? :hydra_core_binding-name :hydra_core_binding-term :hydra_core_binding-type :make-hydra_core_case_statement :hydra_core_case_statement? :hydra_core_case_statement-type_name :hydra_core_case_statement-default :hydra_core_case_statement-cases :make-hydra_core_either_type :hydra_core_either_type? :hydra_core_either_type-left :hydra_core_either_type-right :make-hydra_core_pair_type :hydra_core_pair_type? :hydra_core_pair_type-first :hydra_core_pair_type-second :hydra_core_elimination-variants :make-hydra_core_field :hydra_core_field? :hydra_core_field-name :hydra_core_field-term :make-hydra_core_field_type :hydra_core_field_type? :hydra_core_field_type-name :hydra_core_field_type-type :hydra_core_float_type-variants :hydra_core_float_value-variants :make-hydra_core_forall_type :hydra_core_forall_type? :hydra_core_forall_type-parameter :hydra_core_forall_type-body :hydra_core_function-variants :make-hydra_core_function_type :hydra_core_function_type? :hydra_core_function_type-domain :hydra_core_function_type-codomain :make-hydra_core_injection :hydra_core_injection? :hydra_core_injection-type_name :hydra_core_injection-field :hydra_core_integer_type-variants :hydra_core_integer_value-variants :make-hydra_core_lambda :hydra_core_lambda? :hydra_core_lambda-parameter :hydra_core_lambda-domain :hydra_core_lambda-body :make-hydra_core_let :hydra_core_let? :hydra_core_let-bindings :hydra_core_let-body :hydra_core_literal-variants :hydra_core_literal_type-variants :make-hydra_core_map_type :hydra_core_map_type? :hydra_core_map_type-keys :hydra_core_map_type-values :make-hydra_core_name :hydra_core_name? :hydra_core_name-value :make-hydra_core_projection :hydra_core_projection? :hydra_core_projection-type_name :hydra_core_projection-field :make-hydra_core_record :hydra_core_record? :hydra_core_record-type_name :hydra_core_record-fields :hydra_core_term-variants :hydra_core_type-variants :make-hydra_core_type_application_term :hydra_core_type_application_term? :hydra_core_type_application_term-body :hydra_core_type_application_term-type :make-hydra_core_type_lambda :hydra_core_type_lambda? :hydra_core_type_lambda-parameter :hydra_core_type_lambda-body :make-hydra_core_type_scheme :hydra_core_type_scheme? :hydra_core_type_scheme-variables :hydra_core_type_scheme-type :hydra_core_type_scheme-constraints :make-hydra_core_type_variable_metadata :hydra_core_type_variable_metadata? :hydra_core_type_variable_metadata-classes :make-hydra_core_wrapped_term :hydra_core_wrapped_term? :hydra_core_wrapped_term-type_name :hydra_core_wrapped_term-body))

(in-package :hydra.core)

(cl:defstruct hydra_core_annotated_term body annotation)

(cl:defstruct hydra_core_annotated_type body annotation)

(cl:defstruct hydra_core_application function argument)

(cl:defstruct hydra_core_application_type function argument)

(cl:defstruct hydra_core_binding name term type)

(cl:defstruct hydra_core_case_statement type_name default cases)

(cl:defstruct hydra_core_either_type left right)

(cl:defstruct hydra_core_pair_type first second)

(cl:defvar hydra_core_elimination-variants (cl:list :record :union :wrap))

(cl:defstruct hydra_core_field name term)

(cl:defstruct hydra_core_field_type name type)

(cl:defvar hydra_core_float_type-variants (cl:list :bigfloat :float32 :float64))

(cl:defvar hydra_core_float_value-variants (cl:list :bigfloat :float32 :float64))

(cl:defstruct hydra_core_forall_type parameter body)

(cl:defvar hydra_core_function-variants (cl:list :elimination :lambda :primitive))

(cl:defstruct hydra_core_function_type domain codomain)

(cl:defstruct hydra_core_injection type_name field)

(cl:defvar hydra_core_integer_type-variants (cl:list :bigint :int8 :int16 :int32 :int64 :uint8 :uint16 :uint32 :uint64))

(cl:defvar hydra_core_integer_value-variants (cl:list :bigint :int8 :int16 :int32 :int64 :uint8 :uint16 :uint32 :uint64))

(cl:defstruct hydra_core_lambda parameter domain body)

(cl:defstruct hydra_core_let bindings body)

(cl:defvar hydra_core_literal-variants (cl:list :binary :boolean :float :integer :string))

(cl:defvar hydra_core_literal_type-variants (cl:list :binary :boolean :float :integer :string))

(cl:defstruct hydra_core_map_type keys values)

(cl:defstruct hydra_core_name value)

(cl:defstruct hydra_core_projection type_name field)

(cl:defstruct hydra_core_record type_name fields)

(cl:defvar hydra_core_term-variants (cl:list :annotated :application :either :function :let :list :literal :map :maybe :pair :record :set :type_application :type_lambda :union :unit :variable :wrap))

(cl:defvar hydra_core_type-variants (cl:list :annotated :application :either :forall :function :list :literal :map :maybe :pair :record :set :union :unit :variable :void :wrap))

(cl:defstruct hydra_core_type_application_term body type)

(cl:defstruct hydra_core_type_lambda parameter body)

(cl:defstruct hydra_core_type_scheme variables type constraints)

(cl:defstruct hydra_core_type_variable_metadata classes)

(cl:defstruct hydra_core_wrapped_term type_name body)
