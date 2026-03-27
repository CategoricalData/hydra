(ns hydra.lib.maps
  (:require [hydra.lib.equality :refer [generic-compare]]
            [hydra.lib.maybes :refer [maybe-nothing?]]))

;; Maps are Clojure hash maps for O(1) amortized lookup/insert/delete.
;; toList produces sorted output (via generic-compare on keys) for determinism.

(defn- to-hash-map
  "Ensure we have a hash map. Converts legacy alist format if needed."
  [m]
  (cond
    (nil? m) {}
    (map? m) m
    ;; Legacy: sequence of (key value) pairs
    (sequential? m)
      (reduce (fn [acc entry]
                (assoc acc (first entry) (second entry)))
              {}
              m)
    :else {}))

;; alter :: (Maybe v -> Maybe v) -> k -> Map k v -> Map k v
(def hydra_lib_maps_alter
  "Alter a value at a key using a function."
  (fn [f] (fn [k] (fn [m]
    (let [hm (to-hash-map m)
          old-maybe (if (contains? hm k)
                      (list :just (get hm k))
                      (list :nothing))
          new-maybe (f old-maybe)]
      (if (maybe-nothing? new-maybe)
        (dissoc hm k)
        (let [v (cond
                  (and (sequential? new-maybe) (= (first new-maybe) :just)) (second new-maybe)
                  (and (sequential? new-maybe) (= (first new-maybe) :maybe)) (second new-maybe)
                  :else new-maybe)]
          (assoc hm k v))))))))

;; bimap :: (k1 -> k2) -> (v1 -> v2) -> Map k1 v1 -> Map k2 v2
(def hydra_lib_maps_bimap
  "Map a function over the keys and values of a map."
  (fn [fk] (fn [fv] (fn [m]
    (reduce-kv (fn [acc k v]
                 (assoc acc (fk k) (fv v)))
               {}
               (to-hash-map m))))))

;; delete :: k -> Map k v -> Map k v
(def hydra_lib_maps_delete
  "Remove a key from a map."
  (fn [k] (fn [m] (dissoc (to-hash-map m) k))))

;; elems :: Map k v -> [v]
(def hydra_lib_maps_elems
  "Get the values of a map."
  (fn [m] (vals (to-hash-map m))))

;; empty :: Map k v
(def hydra_lib_maps_empty
  "Create an empty map."
  {})

;; filter :: (v -> Bool) -> Map k v -> Map k v
(def hydra_lib_maps_filter
  "Filter a map based on values."
  (fn [pred_] (fn [m]
    (reduce-kv (fn [acc k v]
                 (if (pred_ v) (assoc acc k v) acc))
               {}
               (to-hash-map m)))))

;; filter_with_key :: (k -> v -> Bool) -> Map k v -> Map k v
(def hydra_lib_maps_filter_with_key
  "Filter a map based on key-value pairs."
  (fn [pred_] (fn [m]
    (reduce-kv (fn [acc k v]
                 (if ((pred_ k) v) (assoc acc k v) acc))
               {}
               (to-hash-map m)))))

;; find_with_default :: v -> k -> Map k v -> v
(def hydra_lib_maps_find_with_default
  "Lookup a value with a default."
  (fn [def_] (fn [k] (fn [m]
    (get (to-hash-map m) k def_)))))

;; from_list :: [Pair k v] -> Map k v
(def hydra_lib_maps_from_list
  "Create a map from a list of key-value pairs."
  (fn [pairs]
    (reduce (fn [acc entry]
              (assoc acc (first entry) (second entry)))
            {}
            pairs)))

;; insert :: k -> v -> Map k v -> Map k v
(def hydra_lib_maps_insert
  "Insert a key-value pair into a map."
  (fn [k] (fn [v] (fn [m] (assoc (to-hash-map m) k v)))))

;; keys :: Map k v -> [k]
(def hydra_lib_maps_keys
  "Get the keys of a map."
  (fn [m] (clojure.core/keys (to-hash-map m))))

;; lookup :: k -> Map k v -> Maybe v
(def hydra_lib_maps_lookup
  "Lookup a value in a map."
  (fn [k] (fn [m]
    (let [hm (to-hash-map m)]
      (if (contains? hm k)
        (list :just (get hm k))
        (list :nothing))))))

;; map :: (v1 -> v2) -> Map k v1 -> Map k v2
(def hydra_lib_maps_map
  "Map a function over a map."
  (fn [f] (fn [m]
    (reduce-kv (fn [acc k v]
                 (assoc acc k (f v)))
               {}
               (to-hash-map m)))))

;; map_keys :: (k1 -> k2) -> Map k1 v -> Map k2 v
(def hydra_lib_maps_map_keys
  "Map a function over the keys of a map."
  (fn [f] (fn [m]
    (reduce-kv (fn [acc k v]
                 (assoc acc (f k) v))
               {}
               (to-hash-map m)))))

;; member :: k -> Map k v -> Bool
(def hydra_lib_maps_member
  "Check if a key is present in a map."
  (fn [k] (fn [m] (contains? (to-hash-map m) k))))

;; null :: Map k v -> Bool
(def hydra_lib_maps_null
  "Check if a map is empty."
  (fn [m] (or (nil? m) (empty? m))))

;; singleton :: k -> v -> Map k v
(def hydra_lib_maps_singleton
  "Create a map with a single key-value pair."
  (fn [k] (fn [v] {k v})))

;; size :: Map k v -> Int
(def hydra_lib_maps_size
  "Get the size of a map."
  (fn [m] (count m)))

;; to_list :: Map k v -> [Pair k v]
;; Sort by key for deterministic output
(def hydra_lib_maps_to_list
  "Convert a map to a list of key-value pairs."
  (fn [m]
    (let [hm (to-hash-map m)]
      (map (fn [entry]
             (if (instance? java.util.Map$Entry entry)
               (list (key entry) (val entry))
               (list (first entry) (second entry))))
           (sort-by (fn [e] (if (instance? java.util.Map$Entry e) (key e) (first e)))
                    generic-compare
                    (seq hm))))))

;; union :: Map k v -> Map k v -> Map k v (left-biased)
(def hydra_lib_maps_union
  "Union two maps, with the first taking precedence."
  (fn [m1] (fn [m2]
    ;; merge m2 into m1; m1 takes precedence (left-biased)
    (merge (to-hash-map m2) (to-hash-map m1)))))
