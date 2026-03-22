(ns hydra.core)

(declare hydra_core_elimination-variants hydra_core_float_type-variants hydra_core_float_value-variants hydra_core_function-variants hydra_core_integer_type-variants hydra_core_integer_value-variants hydra_core_literal-variants hydra_core_literal_type-variants hydra_core_term-variants hydra_core_type-variants)

(defrecord hydra_core_annotated_term [body annotation])
(defn make-hydra_core_annotated_term [body annotation] (->hydra_core_annotated_term body annotation))

(defrecord hydra_core_annotated_type [body annotation])
(defn make-hydra_core_annotated_type [body annotation] (->hydra_core_annotated_type body annotation))

(defrecord hydra_core_application [function argument])
(defn make-hydra_core_application [function argument] (->hydra_core_application function argument))

(defrecord hydra_core_application_type [function argument])
(defn make-hydra_core_application_type [function argument] (->hydra_core_application_type function argument))

(defrecord hydra_core_binding [name term type])
(defn make-hydra_core_binding [name term type] (->hydra_core_binding name term type))

(defrecord hydra_core_case_statement [type_name default cases])
(defn make-hydra_core_case_statement [type_name default cases] (->hydra_core_case_statement type_name default cases))

(defrecord hydra_core_either_type [left right])
(defn make-hydra_core_either_type [left right] (->hydra_core_either_type left right))

(defrecord hydra_core_pair_type [first second])
(defn make-hydra_core_pair_type [first second] (->hydra_core_pair_type first second))

(def hydra_core_elimination-variants (list :record :union :wrap))

(defrecord hydra_core_field [name term])
(defn make-hydra_core_field [name term] (->hydra_core_field name term))

(defrecord hydra_core_field_type [name type])
(defn make-hydra_core_field_type [name type] (->hydra_core_field_type name type))

(def hydra_core_float_type-variants (list :bigfloat :float32 :float64))

(def hydra_core_float_value-variants (list :bigfloat :float32 :float64))

(defrecord hydra_core_forall_type [parameter body])
(defn make-hydra_core_forall_type [parameter body] (->hydra_core_forall_type parameter body))

(def hydra_core_function-variants (list :elimination :lambda :primitive))

(defrecord hydra_core_function_type [domain codomain])
(defn make-hydra_core_function_type [domain codomain] (->hydra_core_function_type domain codomain))

(defrecord hydra_core_injection [type_name field])
(defn make-hydra_core_injection [type_name field] (->hydra_core_injection type_name field))

(def hydra_core_integer_type-variants (list :bigint :int8 :int16 :int32 :int64 :uint8 :uint16 :uint32 :uint64))

(def hydra_core_integer_value-variants (list :bigint :int8 :int16 :int32 :int64 :uint8 :uint16 :uint32 :uint64))

(defrecord hydra_core_lambda [parameter domain body])
(defn make-hydra_core_lambda [parameter domain body] (->hydra_core_lambda parameter domain body))

(defrecord hydra_core_let [bindings body])
(defn make-hydra_core_let [bindings body] (->hydra_core_let bindings body))

(def hydra_core_literal-variants (list :binary :boolean :float :integer :string))

(def hydra_core_literal_type-variants (list :binary :boolean :float :integer :string))

(defrecord hydra_core_map_type [keys values])
(defn make-hydra_core_map_type [keys values] (->hydra_core_map_type keys values))

(defrecord hydra_core_name [value])
(defn make-hydra_core_name [value] (->hydra_core_name value))

(defrecord hydra_core_projection [type_name field])
(defn make-hydra_core_projection [type_name field] (->hydra_core_projection type_name field))

(defrecord hydra_core_record [type_name fields])
(defn make-hydra_core_record [type_name fields] (->hydra_core_record type_name fields))

(def hydra_core_term-variants (list :annotated :application :either :function :let :list :literal :map :maybe :pair :record :set :type_application :type_lambda :union :unit :variable :wrap))

(def hydra_core_type-variants (list :annotated :application :either :forall :function :list :literal :map :maybe :pair :record :set :union :unit :variable :void :wrap))

(defrecord hydra_core_type_application_term [body type])
(defn make-hydra_core_type_application_term [body type] (->hydra_core_type_application_term body type))

(defrecord hydra_core_type_lambda [parameter body])
(defn make-hydra_core_type_lambda [parameter body] (->hydra_core_type_lambda parameter body))

(defrecord hydra_core_type_scheme [variables type constraints])
(defn make-hydra_core_type_scheme [variables type constraints] (->hydra_core_type_scheme variables type constraints))

(defrecord hydra_core_type_variable_metadata [classes])
(defn make-hydra_core_type_variable_metadata [classes] (->hydra_core_type_variable_metadata classes))

(defrecord hydra_core_wrapped_term [type_name body])
(defn make-hydra_core_wrapped_term [type_name body] (->hydra_core_wrapped_term type_name body))
