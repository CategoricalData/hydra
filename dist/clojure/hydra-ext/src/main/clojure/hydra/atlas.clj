(ns hydra.atlas
  (:require [hydra.xml.schema :refer :all]
))

(declare hydra_atlas_atlas_attribute_def_cardinality-variants hydra_atlas_atlas_attribute_def_index_type-variants hydra_atlas_type_category-variants)

(defrecord hydra_atlas_atlas_attribute_def [name type_name is_optional cardinality values_min_count values_max_count is_unique is_indexable include_in_notification default_value description search_weight index_type constraints options display_name])
(defn make-hydra_atlas_atlas_attribute_def [name type_name is_optional cardinality values_min_count values_max_count is_unique is_indexable include_in_notification default_value description search_weight index_type constraints options display_name] (->hydra_atlas_atlas_attribute_def name type_name is_optional cardinality values_min_count values_max_count is_unique is_indexable include_in_notification default_value description search_weight index_type constraints options display_name))

(def hydra_atlas_atlas_attribute_def_cardinality-variants (list :single :list :set))

(def hydra_atlas_atlas_attribute_def_index_type-variants (list :default :string))

(defrecord hydra_atlas_atlas_base_type_def [category guid created_by updated_by create_time update_time version name description type_version service_type options])
(defn make-hydra_atlas_atlas_base_type_def [category guid created_by updated_by create_time update_time version name description type_version service_type options] (->hydra_atlas_atlas_base_type_def category guid created_by updated_by create_time update_time version name description type_version service_type options))

(defrecord hydra_atlas_atlas_constraint_def [type params])
(defn make-hydra_atlas_atlas_constraint_def [type params] (->hydra_atlas_atlas_constraint_def type params))

(defrecord hydra_atlas_atlas_entity_def [as_atlas_struct super_types sub_types relationship_attribute_defs business_attribute_defs])
(defn make-hydra_atlas_atlas_entity_def [as_atlas_struct super_types sub_types relationship_attribute_defs business_attribute_defs] (->hydra_atlas_atlas_entity_def as_atlas_struct super_types sub_types relationship_attribute_defs business_attribute_defs))

(defrecord hydra_atlas_atlas_relationship_attribute_def [as_atlas_attribute relationship_type_name is_legacy_attribute])
(defn make-hydra_atlas_atlas_relationship_attribute_def [as_atlas_attribute relationship_type_name is_legacy_attribute] (->hydra_atlas_atlas_relationship_attribute_def as_atlas_attribute relationship_type_name is_legacy_attribute))

(defrecord hydra_atlas_atlas_struct_def [as_atlas_base_type attribute_defs])
(defn make-hydra_atlas_atlas_struct_def [as_atlas_base_type attribute_defs] (->hydra_atlas_atlas_struct_def as_atlas_base_type attribute_defs))

(def hydra_atlas_type_category-variants (list :primitive :object_id_type :enum :struct :classification :entity :array :map :relationship :business_metadata))
