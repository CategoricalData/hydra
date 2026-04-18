(ns hydra.stac.items
  (:require [hydra.geojson.model :refer :all] [hydra.iana.linkrelations :refer :all]
))

(declare hydra_stac_items_relation_type-variants hydra_stac_items_role-variants hydra_stac_items_stac_relation_type-variants)

(defrecord hydra_stac_items_asset [href title description type roles])
(defn make-hydra_stac_items_asset [href title description type roles] (->hydra_stac_items_asset href title description type roles))

(defrecord hydra_stac_items_item [feature stac_version stac_extensions links assets collection])
(defn make-hydra_stac_items_item [feature stac_version stac_extensions links assets collection] (->hydra_stac_items_item feature stac_version stac_extensions links assets collection))

(defrecord hydra_stac_items_link [href rel type title])
(defn make-hydra_stac_items_link [href rel type title] (->hydra_stac_items_link href rel type title))

(defrecord hydra_stac_items_media_type [value])
(defn make-hydra_stac_items_media_type [value] (->hydra_stac_items_media_type value))

(def hydra_stac_items_relation_type-variants (list :iana :stac :other))

(def hydra_stac_items_role-variants (list :thumbnail :overview :data :metadata :other))

(def hydra_stac_items_stac_relation_type-variants (list :self :root :parent :collection :derived_from))

(defrecord hydra_stac_items_stac_version [value])
(defn make-hydra_stac_items_stac_version [value] (->hydra_stac_items_stac_version value))

(defrecord hydra_stac_items_uri [value])
(defn make-hydra_stac_items_uri [value] (->hydra_stac_items_uri value))

(defrecord hydra_stac_items_url [value])
(defn make-hydra_stac_items_url [value] (->hydra_stac_items_url value))
