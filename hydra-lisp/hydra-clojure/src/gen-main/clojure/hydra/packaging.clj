(ns hydra.packaging
  (:require [hydra.module :refer :all]
))

(defrecord hydra_packaging_package [name modules dependencies description])
(defn make-hydra_packaging_package [name modules dependencies description] (->hydra_packaging_package name modules dependencies description))

(defrecord hydra_packaging_package_name [value])
(defn make-hydra_packaging_package_name [value] (->hydra_packaging_package_name value))
