(ns hydra.geojson.model
  (:require [hydra.json.model :refer :all]
))

(declare hydra_geojson_model_geometry-variants hydra_geojson_model_id-variants hydra_geojson_model_object-variants)

(defrecord hydra_geojson_model_bounding_box [value])
(defn make-hydra_geojson_model_bounding_box [value] (->hydra_geojson_model_bounding_box value))

(defrecord hydra_geojson_model_coordinate_range [min max])
(defn make-hydra_geojson_model_coordinate_range [min max] (->hydra_geojson_model_coordinate_range min max))

(defrecord hydra_geojson_model_feature [geometry properties id bbox])
(defn make-hydra_geojson_model_feature [geometry properties id bbox] (->hydra_geojson_model_feature geometry properties id bbox))

(defrecord hydra_geojson_model_feature_collection [features bbox])
(defn make-hydra_geojson_model_feature_collection [features bbox] (->hydra_geojson_model_feature_collection features bbox))

(defrecord hydra_geojson_model_geometry_collection [geometries bbox])
(defn make-hydra_geojson_model_geometry_collection [geometries bbox] (->hydra_geojson_model_geometry_collection geometries bbox))

(def hydra_geojson_model_geometry-variants (list :point :multi_point :line_string :multi_line_string :polygon :multi_polygon :geometry_collection))

(def hydra_geojson_model_id-variants (list :number :string))

(defrecord hydra_geojson_model_line_string [coordinates bbox])
(defn make-hydra_geojson_model_line_string [coordinates bbox] (->hydra_geojson_model_line_string coordinates bbox))

(defrecord hydra_geojson_model_multi_line_string [coordinates bbox])
(defn make-hydra_geojson_model_multi_line_string [coordinates bbox] (->hydra_geojson_model_multi_line_string coordinates bbox))

(defrecord hydra_geojson_model_multi_point [coordinates bbox])
(defn make-hydra_geojson_model_multi_point [coordinates bbox] (->hydra_geojson_model_multi_point coordinates bbox))

(defrecord hydra_geojson_model_multi_polygon [coordinates bbox])
(defn make-hydra_geojson_model_multi_polygon [coordinates bbox] (->hydra_geojson_model_multi_polygon coordinates bbox))

(def hydra_geojson_model_object-variants (list :geometry :feature :feature_collection))

(defrecord hydra_geojson_model_point [coordinates bbox])
(defn make-hydra_geojson_model_point [coordinates bbox] (->hydra_geojson_model_point coordinates bbox))

(defrecord hydra_geojson_model_polygon [coordinates bbox])
(defn make-hydra_geojson_model_polygon [coordinates bbox] (->hydra_geojson_model_polygon coordinates bbox))

(defrecord hydra_geojson_model_position [latitude longitude altitude])
(defn make-hydra_geojson_model_position [latitude longitude altitude] (->hydra_geojson_model_position latitude longitude altitude))
