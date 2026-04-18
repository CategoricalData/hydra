(ns hydra.delta.parquet)

(declare hydra_delta_parquet_base_primitive_type-variants hydra_delta_parquet_data_type-variants)

(defrecord hydra_delta_parquet_array_type [element_type contains_null])
(defn make-hydra_delta_parquet_array_type [element_type contains_null] (->hydra_delta_parquet_array_type element_type contains_null))

(def hydra_delta_parquet_base_primitive_type-variants (list :binary :boolean :byte :date :double :float :integer :long :short :string :timestamp))

(def hydra_delta_parquet_data_type-variants (list :array :base :decimal :map :struct))

(defrecord hydra_delta_parquet_decimal_type [precision scale])
(defn make-hydra_delta_parquet_decimal_type [precision scale] (->hydra_delta_parquet_decimal_type precision scale))

(defrecord hydra_delta_parquet_map_type [key_type value_type value_contains_null])
(defn make-hydra_delta_parquet_map_type [key_type value_type value_contains_null] (->hydra_delta_parquet_map_type key_type value_type value_contains_null))

(defrecord hydra_delta_parquet_struct_field [name data_type nullable])
(defn make-hydra_delta_parquet_struct_field [name data_type nullable] (->hydra_delta_parquet_struct_field name data_type nullable))

(defrecord hydra_delta_parquet_struct_type [fields])
(defn make-hydra_delta_parquet_struct_type [fields] (->hydra_delta_parquet_struct_type fields))
