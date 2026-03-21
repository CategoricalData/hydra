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
  (fn [fk] (fn [fv] (fn [m]
    (reduce-kv (fn [acc k v]
                 (assoc acc (fk k) (fv v)))
               {}
               (to-hash-map m))))))

;; delete :: k -> Map k v -> Map k v
(def hydra_lib_maps_delete
  (fn [k] (fn [m] (dissoc (to-hash-map m) k))))

;; elems :: Map k v -> [v]
(def hydra_lib_maps_elems
  (fn [m] (vals (to-hash-map m))))

;; empty :: Map k v
(def hydra_lib_maps_empty {})

;; filter :: (v -> Bool) -> Map k v -> Map k v
(def hydra_lib_maps_filter
  (fn [pred_] (fn [m]
    (reduce-kv (fn [acc k v]
                 (if (pred_ v) (assoc acc k v) acc))
               {}
               (to-hash-map m)))))

;; filter_with_key :: (k -> v -> Bool) -> Map k v -> Map k v
(def hydra_lib_maps_filter_with_key
  (fn [pred_] (fn [m]
    (reduce-kv (fn [acc k v]
                 (if ((pred_ k) v) (assoc acc k v) acc))
               {}
               (to-hash-map m)))))

;; find_with_default :: v -> k -> Map k v -> v
(def hydra_lib_maps_find_with_default
  (fn [def_] (fn [k] (fn [m]
    (get (to-hash-map m) k def_)))))

;; from_list :: [Pair k v] -> Map k v
(def hydra_lib_maps_from_list
  (fn [pairs]
    (reduce (fn [acc entry]
              (assoc acc (first entry) (second entry)))
            {}
            pairs)))

;; insert :: k -> v -> Map k v -> Map k v
(def hydra_lib_maps_insert
  (fn [k] (fn [v] (fn [m] (assoc (to-hash-map m) k v)))))

;; keys :: Map k v -> [k]
(def hydra_lib_maps_keys
  (fn [m] (clojure.core/keys (to-hash-map m))))

;; lookup :: k -> Map k v -> Maybe v
(def hydra_lib_maps_lookup
  (fn [k] (fn [m]
    (let [hm (to-hash-map m)]
      (if (contains? hm k)
        (list :just (get hm k))
        (list :nothing))))))

;; map :: (v1 -> v2) -> Map k v1 -> Map k v2
(def hydra_lib_maps_map
  (fn [f] (fn [m]
    (reduce-kv (fn [acc k v]
                 (assoc acc k (f v)))
               {}
               (to-hash-map m)))))

;; map_keys :: (k1 -> k2) -> Map k1 v -> Map k2 v
(def hydra_lib_maps_map_keys
  (fn [f] (fn [m]
    (reduce-kv (fn [acc k v]
                 (assoc acc (f k) v))
               {}
               (to-hash-map m)))))

;; member :: k -> Map k v -> Bool
(def hydra_lib_maps_member
  (fn [k] (fn [m] (contains? (to-hash-map m) k))))

;; null :: Map k v -> Bool
(def hydra_lib_maps_null
  (fn [m] (or (nil? m) (empty? m))))

;; singleton :: k -> v -> Map k v
(def hydra_lib_maps_singleton
  (fn [k] (fn [v] {k v})))

;; size :: Map k v -> Int
(def hydra_lib_maps_size
  (fn [m] (count m)))

;; to_list :: Map k v -> [Pair k v]
;; Sort by key for deterministic output
(def hydra_lib_maps_to_list
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
  (fn [m1] (fn [m2]
    ;; merge m2 into m1; m1 takes precedence (left-biased)
    (merge (to-hash-map m2) (to-hash-map m1)))))
