(ns hydra.lib.maps
  (:require [hydra.lib.equality :refer [generic-compare]]
            [hydra.lib.maybes :refer [maybe-nothing?]]))

;; Maps are sorted association lists: ((k1 v1) (k2 v2) ...)
;; Each entry is a 2-element list (list key value)

(defn alist-lookup [key alist]
  (loop [rest_ (seq alist)]
    (cond
      (nil? rest_) nil
      (= key (first (first rest_))) (first rest_)
      :else (recur (next rest_)))))

(defn alist-insert [key val_ alist]
  (cond
    (empty? alist) (list (list key val_))
    (= (generic-compare key (first (first alist))) 0)
    (cons (list key val_) (rest alist))
    (< (generic-compare key (first (first alist))) 0)
    (cons (list key val_) alist)
    :else (cons (first alist) (alist-insert key val_ (rest alist)))))

(defn alist-delete [key alist]
  (cond
    (empty? alist) ()
    (= key (first (first alist))) (rest alist)
    :else (cons (first alist) (alist-delete key (rest alist)))))

;; alter :: (Maybe v -> Maybe v) -> k -> Map k v -> Map k v
(def hydra_lib_maps_alter
  (fn [f] (fn [k] (fn [m]
    (let [existing (alist-lookup k m)
          old-maybe (if existing
                      (list :just (second existing))
                      (list :nothing))
          new-maybe (f old-maybe)]
      (if (maybe-nothing? new-maybe)
        (alist-delete k m)
        (let [v (cond
                  (and (sequential? new-maybe) (= (first new-maybe) :just)) (second new-maybe)
                  (and (sequential? new-maybe) (= (first new-maybe) :maybe)) (second new-maybe)
                  :else new-maybe)]
          (alist-insert k v m))))))))

;; bimap :: (k1 -> k2) -> (v1 -> v2) -> Map k1 v1 -> Map k2 v2
(def hydra_lib_maps_bimap
  (fn [fk] (fn [fv] (fn [m]
    (loop [rest_ (seq m) result ()]
      (if (nil? rest_)
        result
        (let [entry (first rest_)]
          (recur (next rest_)
                 (alist-insert (fk (first entry)) (fv (second entry)) result)))))))))

;; delete :: k -> Map k v -> Map k v
(def hydra_lib_maps_delete
  (fn [k] (fn [m] (alist-delete k m))))

;; elems :: Map k v -> [v]
(def hydra_lib_maps_elems
  (fn [m] (map second m)))

;; empty :: Map k v
(def hydra_lib_maps_empty ())

;; filter :: (v -> Bool) -> Map k v -> Map k v
(def hydra_lib_maps_filter
  (fn [pred_] (fn [m]
    (filter (fn [entry] (pred_ (second entry))) m))))

;; filter_with_key :: (k -> v -> Bool) -> Map k v -> Map k v
(def hydra_lib_maps_filter_with_key
  (fn [pred_] (fn [m]
    (filter (fn [entry] ((pred_ (first entry)) (second entry))) m))))

;; find_with_default :: v -> k -> Map k v -> v
(def hydra_lib_maps_find_with_default
  (fn [def_] (fn [k] (fn [m]
    (let [entry (alist-lookup k m)]
      (if entry (second entry) def_))))))

;; from_list :: [Pair k v] -> Map k v
(def hydra_lib_maps_from_list
  (fn [pairs]
    (loop [rest_ (seq pairs) acc ()]
      (if (nil? rest_)
        acc
        (let [p (first rest_)]
          (recur (next rest_)
                 (alist-insert (first p) (second p) acc)))))))

;; insert :: k -> v -> Map k v -> Map k v
(def hydra_lib_maps_insert
  (fn [k] (fn [v] (fn [m] (alist-insert k v m)))))

;; keys :: Map k v -> [k]
(def hydra_lib_maps_keys
  (fn [m] (map first m)))

;; lookup :: k -> Map k v -> Maybe v
(def hydra_lib_maps_lookup
  (fn [k] (fn [m]
    (let [entry (alist-lookup k m)]
      (if entry
        (list :just (second entry))
        (list :nothing))))))

;; map :: (v1 -> v2) -> Map k v1 -> Map k v2
(def hydra_lib_maps_map
  (fn [f] (fn [m]
    (map (fn [entry] (list (first entry) (f (second entry)))) m))))

;; map_keys :: (k1 -> k2) -> Map k1 v -> Map k2 v
(def hydra_lib_maps_map_keys
  (fn [f] (fn [m]
    (loop [rest_ (seq m) result ()]
      (if (nil? rest_)
        result
        (let [entry (first rest_)]
          (recur (next rest_)
                 (alist-insert (f (first entry)) (second entry) result))))))))

;; member :: k -> Map k v -> Bool
(def hydra_lib_maps_member
  (fn [k] (fn [m] (if (alist-lookup k m) true false))))

;; null :: Map k v -> Bool
(def hydra_lib_maps_null
  (fn [m] (empty? m)))

;; singleton :: k -> v -> Map k v
(def hydra_lib_maps_singleton
  (fn [k] (fn [v] (list (list k v)))))

;; size :: Map k v -> Int
(def hydra_lib_maps_size
  (fn [m] (count m)))

;; to_list :: Map k v -> [Pair k v]
(def hydra_lib_maps_to_list
  (fn [m] (map (fn [entry] (list (first entry) (second entry))) m)))

;; union :: Map k v -> Map k v -> Map k v (left-biased)
(def hydra_lib_maps_union
  (fn [m1] (fn [m2]
    (loop [rest_ (seq m2) acc m1]
      (if (nil? rest_)
        acc
        (let [k (first (first rest_)) v (second (first rest_))]
          (if (alist-lookup k acc)
            (recur (next rest_) acc)
            (recur (next rest_) (alist-insert k v acc)))))))))
