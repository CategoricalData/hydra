(ns hydra.protobuf.proto3)

(declare hydra_protobuf_proto3_definition-variants hydra_protobuf_proto3_field_type-variants hydra_protobuf_proto3_scalar_type-variants hydra_protobuf_proto3_simple_type-variants hydra_protobuf_proto3_value-variants)

(def hydra_protobuf_proto3_definition-variants (list :enum :message))

(defrecord hydra_protobuf_proto3_enum_definition [name values options])
(defn make-hydra_protobuf_proto3_enum_definition [name values options] (->hydra_protobuf_proto3_enum_definition name values options))

(defrecord hydra_protobuf_proto3_enum_value [name number options])
(defn make-hydra_protobuf_proto3_enum_value [name number options] (->hydra_protobuf_proto3_enum_value name number options))

(defrecord hydra_protobuf_proto3_enum_value_name [value])
(defn make-hydra_protobuf_proto3_enum_value_name [value] (->hydra_protobuf_proto3_enum_value_name value))

(defrecord hydra_protobuf_proto3_field [name json_name type number options])
(defn make-hydra_protobuf_proto3_field [name json_name type number options] (->hydra_protobuf_proto3_field name json_name type number options))

(defrecord hydra_protobuf_proto3_field_name [value])
(defn make-hydra_protobuf_proto3_field_name [value] (->hydra_protobuf_proto3_field_name value))

(def hydra_protobuf_proto3_field_type-variants (list :map :oneof :repeated :simple))

(defrecord hydra_protobuf_proto3_file_reference [value])
(defn make-hydra_protobuf_proto3_file_reference [value] (->hydra_protobuf_proto3_file_reference value))

(defrecord hydra_protobuf_proto3_map_type [keys values])
(defn make-hydra_protobuf_proto3_map_type [keys values] (->hydra_protobuf_proto3_map_type keys values))

(defrecord hydra_protobuf_proto3_message_definition [name fields options])
(defn make-hydra_protobuf_proto3_message_definition [name fields options] (->hydra_protobuf_proto3_message_definition name fields options))

(defrecord hydra_protobuf_proto3_option [name value])
(defn make-hydra_protobuf_proto3_option [name value] (->hydra_protobuf_proto3_option name value))

(defrecord hydra_protobuf_proto3_package_name [value])
(defn make-hydra_protobuf_proto3_package_name [value] (->hydra_protobuf_proto3_package_name value))

(defrecord hydra_protobuf_proto3_proto_file [package imports types options])
(defn make-hydra_protobuf_proto3_proto_file [package imports types options] (->hydra_protobuf_proto3_proto_file package imports types options))

(def hydra_protobuf_proto3_scalar_type-variants (list :bool :bytes :double :fixed32 :fixed64 :float :int32 :int64 :sfixed32 :sfixed64 :sint32 :sint64 :string :uint32 :uint64))

(def hydra_protobuf_proto3_simple_type-variants (list :reference :scalar))

(defrecord hydra_protobuf_proto3_type_name [value])
(defn make-hydra_protobuf_proto3_type_name [value] (->hydra_protobuf_proto3_type_name value))

(defrecord hydra_protobuf_proto3_type_reference [value])
(defn make-hydra_protobuf_proto3_type_reference [value] (->hydra_protobuf_proto3_type_reference value))

(def hydra_protobuf_proto3_value-variants (list :boolean :string))
