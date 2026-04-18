(ns hydra.azure.dtld)

(declare hydra_azure_dtld_command_type-variants hydra_azure_dtld_integer_or_string-variants hydra_azure_dtld_interface_contents-variants hydra_azure_dtld_schema-variants hydra_azure_dtld_schema_complex-variants hydra_azure_dtld_schema_interface_type-variants hydra_azure_dtld_schema_primitive-variants)

(defrecord hydra_azure_dtld_command [type name id comment description display_name command_type request response])
(defn make-hydra_azure_dtld_command [type name id comment description display_name command_type request response] (->hydra_azure_dtld_command type name id comment description display_name command_type request response))

(defrecord hydra_azure_dtld_command_payload [name schema id comment description display_name])
(defn make-hydra_azure_dtld_command_payload [name schema id comment description display_name] (->hydra_azure_dtld_command_payload name schema id comment description display_name))

(def hydra_azure_dtld_command_type-variants (list :synchronous :asynchronous))

(defrecord hydra_azure_dtld_component [type name schema id comment description display_name])
(defn make-hydra_azure_dtld_component [type name schema id comment description display_name] (->hydra_azure_dtld_component type name schema id comment description display_name))

(defrecord hydra_azure_dtld_dtmi [value])
(defn make-hydra_azure_dtld_dtmi [value] (->hydra_azure_dtld_dtmi value))

(defrecord hydra_azure_dtld_enum_value [name enum_value id comment description display_name])
(defn make-hydra_azure_dtld_enum_value [name enum_value id comment description display_name] (->hydra_azure_dtld_enum_value name enum_value id comment description display_name))

(defrecord hydra_azure_dtld_field [name schema id comment description display_name])
(defn make-hydra_azure_dtld_field [name schema id comment description display_name] (->hydra_azure_dtld_field name schema id comment description display_name))

(def hydra_azure_dtld_integer_or_string-variants (list :integer :string))

(defrecord hydra_azure_dtld_interface [id type context comment contents description display_name extends schemas])
(defn make-hydra_azure_dtld_interface [id type context comment contents description display_name extends schemas] (->hydra_azure_dtld_interface id type context comment contents description display_name extends schemas))

(def hydra_azure_dtld_interface_contents-variants (list :command :component :property :relationship :telemetry))

(defrecord hydra_azure_dtld_iri [value])
(defn make-hydra_azure_dtld_iri [value] (->hydra_azure_dtld_iri value))

(defrecord hydra_azure_dtld_map_key [name schema id comment description display_name])
(defn make-hydra_azure_dtld_map_key [name schema id comment description display_name] (->hydra_azure_dtld_map_key name schema id comment description display_name))

(defrecord hydra_azure_dtld_map_value [name schema id comment description display_name])
(defn make-hydra_azure_dtld_map_value [name schema id comment description display_name] (->hydra_azure_dtld_map_value name schema id comment description display_name))

(defrecord hydra_azure_dtld_property [type name schema id comment description display_name unit writable])
(defn make-hydra_azure_dtld_property [type name schema id comment description display_name unit writable] (->hydra_azure_dtld_property type name schema id comment description display_name unit writable))

(defrecord hydra_azure_dtld_relationship [type name id comment description display_name max_multiplicity min_multiplicity properties target writable])
(defn make-hydra_azure_dtld_relationship [type name id comment description display_name max_multiplicity min_multiplicity properties target writable] (->hydra_azure_dtld_relationship type name id comment description display_name max_multiplicity min_multiplicity properties target writable))

(def hydra_azure_dtld_schema-variants (list :primitive :complex))

(defrecord hydra_azure_dtld_schema_array [type element_schema id comment description display_name])
(defn make-hydra_azure_dtld_schema_array [type element_schema id comment description display_name] (->hydra_azure_dtld_schema_array type element_schema id comment description display_name))

(def hydra_azure_dtld_schema_complex-variants (list :array :enum :map :object))

(defrecord hydra_azure_dtld_schema_enum [type enum_values value_schema id comment description display_name])
(defn make-hydra_azure_dtld_schema_enum [type enum_values value_schema id comment description display_name] (->hydra_azure_dtld_schema_enum type enum_values value_schema id comment description display_name))

(defrecord hydra_azure_dtld_schema_interface [id type comment description display_name])
(defn make-hydra_azure_dtld_schema_interface [id type comment description display_name] (->hydra_azure_dtld_schema_interface id type comment description display_name))

(def hydra_azure_dtld_schema_interface_type-variants (list :array :enum :map :object))

(defrecord hydra_azure_dtld_schema_map [type map_key map_value id comment description display_name])
(defn make-hydra_azure_dtld_schema_map [type map_key map_value id comment description display_name] (->hydra_azure_dtld_schema_map type map_key map_value id comment description display_name))

(defrecord hydra_azure_dtld_schema_object [type fields id comment description display_name])
(defn make-hydra_azure_dtld_schema_object [type fields id comment description display_name] (->hydra_azure_dtld_schema_object type fields id comment description display_name))

(def hydra_azure_dtld_schema_primitive-variants (list :boolean :date :date_time :double :duration :float :integer :long :string :time))

(defrecord hydra_azure_dtld_telemetry [type name schema id comment description display_name unit])
(defn make-hydra_azure_dtld_telemetry [type name schema id comment description display_name unit] (->hydra_azure_dtld_telemetry type name schema id comment description display_name unit))

(defrecord hydra_azure_dtld_unit [value])
(defn make-hydra_azure_dtld_unit [value] (->hydra_azure_dtld_unit value))
