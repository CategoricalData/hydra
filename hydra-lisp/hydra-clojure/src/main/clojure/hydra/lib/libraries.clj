(ns hydra.lib.libraries
  (:require [hydra.dsl.prims :as p]
            [hydra.lib.chars :as chars]
            [hydra.lib.eithers :as eithers]
            [hydra.lib.equality :as equality]
            [hydra.lib.lists :as lists]
            [hydra.lib.literals :as literals]
            [hydra.lib.logic :as logic]
            [hydra.lib.maps :as maps]
            [hydra.lib.math :as math]
            [hydra.lib.maybes :as maybes]
            [hydra.lib.pairs :as pairs]
            [hydra.lib.sets :as sets]
            [hydra.lib.strings :as strings]))

(defn- qname [ns local] (str ns "." local))

(defn fun
  "A TermCoder for function types using beta reduction to bridge
   term-level functions to native functions."
  [dom cod]
  (p/tc-function-with-reduce
   (fn [cx g t]
     ;; Lazy require to avoid circular dependency
     (let [reduce-fn (deref (or (resolve 'hydra_reduction_reduce_term)
                                (ns-resolve 'hydra.reduction 'hydra_reduction_reduce_term)))]
       ((((reduce-fn cx) g) true) t)))
   dom cod))

;; ============================================================
;; Chars
;; ============================================================

(defn register-chars []
  (let [ns_ "hydra.lib.chars"]
    {(qname ns_ "isAlphaNum") (p/prim1 (qname ns_ "isAlphaNum") chars/hydra_lib_chars_is_alpha_num [] (p/tc-int32) (p/tc-boolean))
     (qname ns_ "isLower")    (p/prim1 (qname ns_ "isLower")    chars/hydra_lib_chars_is_lower    [] (p/tc-int32) (p/tc-boolean))
     (qname ns_ "isSpace")    (p/prim1 (qname ns_ "isSpace")    chars/hydra_lib_chars_is_space    [] (p/tc-int32) (p/tc-boolean))
     (qname ns_ "isUpper")    (p/prim1 (qname ns_ "isUpper")    chars/hydra_lib_chars_is_upper    [] (p/tc-int32) (p/tc-boolean))
     (qname ns_ "toLower")    (p/prim1 (qname ns_ "toLower")    chars/hydra_lib_chars_to_lower    [] (p/tc-int32) (p/tc-int32))
     (qname ns_ "toUpper")    (p/prim1 (qname ns_ "toUpper")    chars/hydra_lib_chars_to_upper    [] (p/tc-int32) (p/tc-int32))}))

;; ============================================================
;; Equality
;; ============================================================

(defn register-equality []
  (let [ns_ "hydra.lib.equality"
        x (p/tc-variable "x")
        ord-x {"x" ["ordering"]}
        eq-x {"x" ["equality"]}]
    {(qname ns_ "compare")  (p/prim2 (qname ns_ "compare")  (fn [a b] ((equality/hydra_lib_equality_compare a) b))  [] x x (p/tc-comparison) ord-x)
     (qname ns_ "equal")    (p/prim2 (qname ns_ "equal")    (fn [a b] ((equality/hydra_lib_equality_equal a) b))    [] x x (p/tc-boolean) eq-x)
     (qname ns_ "identity") (p/prim1 (qname ns_ "identity") identity                             [] x x)
     (qname ns_ "gt")       (p/prim2 (qname ns_ "gt")       (fn [a b] ((equality/hydra_lib_equality_gt a) b))       [] x x (p/tc-boolean) ord-x)
     (qname ns_ "gte")      (p/prim2 (qname ns_ "gte")      (fn [a b] ((equality/hydra_lib_equality_gte a) b))      [] x x (p/tc-boolean) ord-x)
     (qname ns_ "lt")       (p/prim2 (qname ns_ "lt")       (fn [a b] ((equality/hydra_lib_equality_lt a) b))       [] x x (p/tc-boolean) ord-x)
     (qname ns_ "lte")      (p/prim2 (qname ns_ "lte")      (fn [a b] ((equality/hydra_lib_equality_lte a) b))      [] x x (p/tc-boolean) ord-x)
     (qname ns_ "max")      (p/prim2 (qname ns_ "max")      (fn [a b] ((equality/hydra_lib_equality_max a) b))      [] x x x ord-x)
     (qname ns_ "min")      (p/prim2 (qname ns_ "min")      (fn [a b] ((equality/hydra_lib_equality_min a) b))      [] x x x ord-x)}))

;; ============================================================
;; Eithers
;; ============================================================

(defn register-eithers []
  (let [ns_ "hydra.lib.eithers"
        x (p/tc-variable "x")
        y (p/tc-variable "y")
        z (p/tc-variable "z")
        w (p/tc-variable "w")]
    {(qname ns_ "bind")    (p/prim2 (qname ns_ "bind")
                                     (fn [e f] ((eithers/hydra_lib_eithers_bind e) f))
                                     [] (p/tc-either x y) (fun y (p/tc-either x z)) (p/tc-either x z))
     (qname ns_ "bimap")   (p/prim3 (qname ns_ "bimap")
                                     (fn [f g e] (((eithers/hydra_lib_eithers_bimap f) g) e))
                                     [] (fun x z) (fun y w) (p/tc-either x y) (p/tc-either z w))
     (qname ns_ "either")  (p/prim3 (qname ns_ "either")
                                     (fn [f g e] (((eithers/hydra_lib_eithers_either f) g) e))
                                     [] (fun x z) (fun y z) (p/tc-either x y) z)
     (qname ns_ "foldl")   (p/prim3 (qname ns_ "foldl")
                                     (fn [f init xs] (((eithers/hydra_lib_eithers_foldl f) init) xs))
                                     [] (fun x (fun y (p/tc-either z x))) x (p/tc-list y) (p/tc-either z x))
     (qname ns_ "fromLeft")  (p/prim2 (qname ns_ "fromLeft")
                                       (fn [dflt e] ((eithers/hydra_lib_eithers_from_left dflt) e))
                                       [] x (p/tc-either x y) x)
     (qname ns_ "fromRight") (p/prim2 (qname ns_ "fromRight")
                                       (fn [dflt e] ((eithers/hydra_lib_eithers_from_right dflt) e))
                                       [] y (p/tc-either x y) y)
     (qname ns_ "isLeft")  (p/prim1 (qname ns_ "isLeft")  eithers/hydra_lib_eithers_is_left  [] (p/tc-either x y) (p/tc-boolean))
     (qname ns_ "isRight") (p/prim1 (qname ns_ "isRight") eithers/hydra_lib_eithers_is_right [] (p/tc-either x y) (p/tc-boolean))
     (qname ns_ "lefts")   (p/prim1 (qname ns_ "lefts")   eithers/hydra_lib_eithers_lefts   [] (p/tc-list (p/tc-either x y)) (p/tc-list x))
     (qname ns_ "map")     (p/prim2 (qname ns_ "map")
                                     (fn [f e] ((eithers/hydra_lib_eithers_map f) e))
                                     [] (fun x y) (p/tc-either z x) (p/tc-either z y))
     (qname ns_ "mapList") (p/prim2 (qname ns_ "mapList")
                                     (fn [f xs] ((eithers/hydra_lib_eithers_map_list f) xs))
                                     [] (fun x (p/tc-either z y)) (p/tc-list x) (p/tc-either z (p/tc-list y)))
     (qname ns_ "mapMaybe") (p/prim2 (qname ns_ "mapMaybe")
                                      (fn [f mx] ((eithers/hydra_lib_eithers_map_maybe f) mx))
                                      [] (fun x (p/tc-either z y)) (p/tc-optional x) (p/tc-either z (p/tc-optional y)))
     (qname ns_ "mapSet")  (p/prim2 (qname ns_ "mapSet")
                                     (fn [f s] ((eithers/hydra_lib_eithers_map_set f) s))
                                     [] (fun x (p/tc-either z y)) (p/tc-set x) (p/tc-either z (p/tc-set y)))
     (qname ns_ "partitionEithers") (p/prim1 (qname ns_ "partitionEithers")
                                              eithers/hydra_lib_eithers_partition_eithers
                                              [] (p/tc-list (p/tc-either x y)) (p/tc-pair (p/tc-list x) (p/tc-list y)))
     (qname ns_ "rights")  (p/prim1 (qname ns_ "rights")  eithers/hydra_lib_eithers_rights  [] (p/tc-list (p/tc-either x y)) (p/tc-list y))}))

;; ============================================================
;; Lists
;; ============================================================

(defn register-lists []
  (let [ns_ "hydra.lib.lists"
        a (p/tc-variable "a")
        b (p/tc-variable "b")
        c (p/tc-variable "c")]
    {(qname ns_ "apply")      (p/prim2 (qname ns_ "apply")
                                        (fn [fs xs] ((lists/hydra_lib_lists_apply fs) xs))
                                        [] (p/tc-list (fun a b)) (p/tc-list a) (p/tc-list b))
     (qname ns_ "at")         (p/prim2 (qname ns_ "at")
                                        (fn [n xs] ((lists/hydra_lib_lists_at n) xs))
                                        [] (p/tc-int32) (p/tc-list a) a)
     (qname ns_ "bind")       (p/prim2 (qname ns_ "bind")
                                        (fn [xs f] ((lists/hydra_lib_lists_bind xs) f))
                                        [] (p/tc-list a) (fun a (p/tc-list b)) (p/tc-list b))
     (qname ns_ "concat")     (p/prim1 (qname ns_ "concat")     lists/hydra_lib_lists_concat     [] (p/tc-list (p/tc-list a)) (p/tc-list a))
     (qname ns_ "concat2")    (p/prim2 (qname ns_ "concat2")
                                        (fn [xs ys] ((lists/hydra_lib_lists_concat2 xs) ys))
                                        [] (p/tc-list a) (p/tc-list a) (p/tc-list a))
     (qname ns_ "cons")       (p/prim2 (qname ns_ "cons")
                                        (fn [x xs] ((lists/hydra_lib_lists_cons x) xs))
                                        [] a (p/tc-list a) (p/tc-list a))
     (qname ns_ "drop")       (p/prim2 (qname ns_ "drop")
                                        (fn [n xs] ((lists/hydra_lib_lists_drop n) xs))
                                        [] (p/tc-int32) (p/tc-list a) (p/tc-list a))
     (qname ns_ "dropWhile")  (p/prim2 (qname ns_ "dropWhile")
                                        (fn [f xs] ((lists/hydra_lib_lists_drop_while f) xs))
                                        [] (fun a (p/tc-boolean)) (p/tc-list a) (p/tc-list a))
     (qname ns_ "elem")       (p/prim2 (qname ns_ "elem")
                                        (fn [x xs] ((lists/hydra_lib_lists_elem x) xs))
                                        [] a (p/tc-list a) (p/tc-boolean) {"a" ["equality"]})
     (qname ns_ "filter")     (p/prim2 (qname ns_ "filter")
                                        (fn [f xs] ((lists/hydra_lib_lists_filter f) xs))
                                        [] (fun a (p/tc-boolean)) (p/tc-list a) (p/tc-list a))
     (qname ns_ "find")       (p/prim2 (qname ns_ "find")
                                        (fn [f xs] ((lists/hydra_lib_lists_find f) xs))
                                        [] (fun a (p/tc-boolean)) (p/tc-list a) (p/tc-optional a))
     (qname ns_ "foldl")      (p/prim3 (qname ns_ "foldl")
                                        (fn [f init xs] (((lists/hydra_lib_lists_foldl (fn [acc] (fn [el] ((f acc) el)))) init) xs))
                                        [] (fun b (fun a b)) b (p/tc-list a) b)
     (qname ns_ "foldr")      (p/prim3 (qname ns_ "foldr")
                                        (fn [f init xs] (((lists/hydra_lib_lists_foldr (fn [el] (fn [acc] ((f el) acc)))) init) xs))
                                        [] (fun a (fun b b)) b (p/tc-list a) b)
     (qname ns_ "group")      (p/prim1 (qname ns_ "group")      lists/hydra_lib_lists_group      [] (p/tc-list a) (p/tc-list (p/tc-list a)) {"a" ["equality"]})
     (qname ns_ "head")       (p/prim1 (qname ns_ "head")       lists/hydra_lib_lists_head       [] (p/tc-list a) a)
     (qname ns_ "init")       (p/prim1 (qname ns_ "init")       lists/hydra_lib_lists_init       [] (p/tc-list a) (p/tc-list a))
     (qname ns_ "intercalate") (p/prim2 (qname ns_ "intercalate")
                                         (fn [sep xss] ((lists/hydra_lib_lists_intercalate sep) xss))
                                         [] (p/tc-list a) (p/tc-list (p/tc-list a)) (p/tc-list a))
     (qname ns_ "intersperse") (p/prim2 (qname ns_ "intersperse")
                                         (fn [sep xs] ((lists/hydra_lib_lists_intersperse sep) xs))
                                         [] a (p/tc-list a) (p/tc-list a))
     (qname ns_ "last")       (p/prim1 (qname ns_ "last")       lists/hydra_lib_lists_last       [] (p/tc-list a) a)
     (qname ns_ "length")     (p/prim1 (qname ns_ "length")     lists/hydra_lib_lists_length     [] (p/tc-list a) (p/tc-int32))
     (qname ns_ "map")        (p/prim2 (qname ns_ "map")
                                        (fn [f xs] ((lists/hydra_lib_lists_map f) xs))
                                        [] (fun a b) (p/tc-list a) (p/tc-list b))
     (qname ns_ "nub")        (p/prim1 (qname ns_ "nub")        lists/hydra_lib_lists_nub        [] (p/tc-list a) (p/tc-list a) {"a" ["equality"]})
     (qname ns_ "null")       (p/prim1 (qname ns_ "null")       lists/hydra_lib_lists_null       [] (p/tc-list a) (p/tc-boolean))
     (qname ns_ "partition")   (p/prim2 (qname ns_ "partition")
                                         (fn [f xs] ((lists/hydra_lib_lists_partition f) xs))
                                         [] (fun a (p/tc-boolean)) (p/tc-list a) (p/tc-pair (p/tc-list a) (p/tc-list a)))
     (qname ns_ "pure")       (p/prim1 (qname ns_ "pure")       lists/hydra_lib_lists_pure       [] a (p/tc-list a))
     (qname ns_ "replicate")  (p/prim2 (qname ns_ "replicate")
                                        (fn [n x] ((lists/hydra_lib_lists_replicate n) x))
                                        [] (p/tc-int32) a (p/tc-list a))
     (qname ns_ "reverse")    (p/prim1 (qname ns_ "reverse")    lists/hydra_lib_lists_reverse    [] (p/tc-list a) (p/tc-list a))
     (qname ns_ "safeHead")   (p/prim1 (qname ns_ "safeHead")   lists/hydra_lib_lists_safe_head  [] (p/tc-list a) (p/tc-optional a))
     (qname ns_ "singleton")  (p/prim1 (qname ns_ "singleton")  lists/hydra_lib_lists_singleton  [] a (p/tc-list a))
     (qname ns_ "sort")       (p/prim1 (qname ns_ "sort")       lists/hydra_lib_lists_sort       [] (p/tc-list a) (p/tc-list a) {"a" ["ordering"]})
     (qname ns_ "sortOn")     (p/prim2 (qname ns_ "sortOn")
                                        (fn [f xs] ((lists/hydra_lib_lists_sort_on f) xs))
                                        [] (fun a b) (p/tc-list a) (p/tc-list a))
     (qname ns_ "span")       (p/prim2 (qname ns_ "span")
                                        (fn [f xs] ((lists/hydra_lib_lists_span f) xs))
                                        [] (fun a (p/tc-boolean)) (p/tc-list a) (p/tc-pair (p/tc-list a) (p/tc-list a)))
     (qname ns_ "tail")       (p/prim1 (qname ns_ "tail")       lists/hydra_lib_lists_tail       [] (p/tc-list a) (p/tc-list a))
     (qname ns_ "take")       (p/prim2 (qname ns_ "take")
                                        (fn [n xs] ((lists/hydra_lib_lists_take n) xs))
                                        [] (p/tc-int32) (p/tc-list a) (p/tc-list a))
     (qname ns_ "transpose")  (p/prim1 (qname ns_ "transpose")  lists/hydra_lib_lists_transpose  [] (p/tc-list (p/tc-list a)) (p/tc-list (p/tc-list a)))
     (qname ns_ "zip")        (p/prim2 (qname ns_ "zip")
                                        (fn [xs ys] ((lists/hydra_lib_lists_zip xs) ys))
                                        [] (p/tc-list a) (p/tc-list b) (p/tc-list (p/tc-pair a b)))
     (qname ns_ "zipWith")    (p/prim3 (qname ns_ "zipWith")
                                        (fn [f xs ys] (((lists/hydra_lib_lists_zip_with (fn [a] (fn [b] ((f a) b)))) xs) ys))
                                        [] (fun a (fun b c)) (p/tc-list a) (p/tc-list b) (p/tc-list c))}))

;; ============================================================
;; Logic
;; ============================================================

(defn register-logic []
  (let [ns_ "hydra.lib.logic"
        a (p/tc-variable "a")]
    {(qname ns_ "and")    (p/prim2 (qname ns_ "and")
                                    (fn [a b] ((logic/hydra_lib_logic_and a) b))
                                    [] (p/tc-boolean) (p/tc-boolean) (p/tc-boolean))
     (qname ns_ "ifElse") (p/prim3 (qname ns_ "ifElse")
                                    (fn [cond t f] (((logic/hydra_lib_logic_if_else cond) t) f))
                                    [] (p/tc-boolean) a a a)
     (qname ns_ "not")    (p/prim1 (qname ns_ "not")    logic/hydra_lib_logic_not [] (p/tc-boolean) (p/tc-boolean))
     (qname ns_ "or")     (p/prim2 (qname ns_ "or")
                                    (fn [a b] ((logic/hydra_lib_logic_or a) b))
                                    [] (p/tc-boolean) (p/tc-boolean) (p/tc-boolean))}))

;; ============================================================
;; Maps
;; ============================================================

(defn register-maps []
  (let [ns_ "hydra.lib.maps"
        k  (p/tc-variable "k")
        k1 (p/tc-variable "k1")
        k2 (p/tc-variable "k2")
        v  (p/tc-variable "v")
        v1 (p/tc-variable "v1")
        v2 (p/tc-variable "v2")
        map-kv (p/tc-map k v)
        ;; All map primitives have ordering constraint on key type(s)
        ord-k {"k" ["ordering"]}
        ord-k1k2 {"k1" ["ordering"] "k2" ["ordering"]}]
    {(qname ns_ "alter")          (p/prim3 (qname ns_ "alter")
                                            (fn [f key m] (((maps/hydra_lib_maps_alter f) key) m))
                                            [] (fun (p/tc-optional v) (p/tc-optional v)) k map-kv map-kv ord-k)
     (qname ns_ "bimap")          (p/prim3 (qname ns_ "bimap")
                                            (fn [fk fv m] (((maps/hydra_lib_maps_bimap fk) fv) m))
                                            [] (fun k1 k2) (fun v1 v2) (p/tc-map k1 v1) (p/tc-map k2 v2) ord-k1k2)
     (qname ns_ "delete")         (p/prim2 (qname ns_ "delete")
                                            (fn [key m] ((maps/hydra_lib_maps_delete key) m))
                                            [] k map-kv map-kv ord-k)
     (qname ns_ "elems")          (p/prim1 (qname ns_ "elems")  maps/hydra_lib_maps_elems  [] map-kv (p/tc-list v) ord-k)
     (qname ns_ "empty")          (p/prim0 (qname ns_ "empty")  (fn [] maps/hydra_lib_maps_empty)  [] map-kv ord-k)
     (qname ns_ "filter")         (p/prim2 (qname ns_ "filter")
                                            (fn [f m] ((maps/hydra_lib_maps_filter f) m))
                                            [] (fun v (p/tc-boolean)) map-kv map-kv ord-k)
     (qname ns_ "filterWithKey")  (p/prim2 (qname ns_ "filterWithKey")
                                            (fn [f m] ((maps/hydra_lib_maps_filter_with_key f) m))
                                            [] (fun k (fun v (p/tc-boolean))) map-kv map-kv ord-k)
     (qname ns_ "findWithDefault") (p/prim3 (qname ns_ "findWithDefault")
                                             (fn [dflt key m] (((maps/hydra_lib_maps_find_with_default dflt) key) m))
                                             [] v k map-kv v ord-k)
     (qname ns_ "fromList")       (p/prim1 (qname ns_ "fromList") maps/hydra_lib_maps_from_list [] (p/tc-list (p/tc-pair k v)) map-kv ord-k)
     (qname ns_ "insert")         (p/prim3 (qname ns_ "insert")
                                            (fn [key val m] (((maps/hydra_lib_maps_insert key) val) m))
                                            [] k v map-kv map-kv ord-k)
     (qname ns_ "keys")           (p/prim1 (qname ns_ "keys")   maps/hydra_lib_maps_keys   [] map-kv (p/tc-list k) ord-k)
     (qname ns_ "lookup")         (p/prim2 (qname ns_ "lookup")
                                            (fn [key m] ((maps/hydra_lib_maps_lookup key) m))
                                            [] k map-kv (p/tc-optional v) ord-k)
     (qname ns_ "map")            (p/prim2 (qname ns_ "map")
                                            (fn [f m] ((maps/hydra_lib_maps_map f) m))
                                            [] (fun v1 v2) (p/tc-map k v1) (p/tc-map k v2) ord-k)
     (qname ns_ "mapKeys")        (p/prim2 (qname ns_ "mapKeys")
                                            (fn [f m] ((maps/hydra_lib_maps_map_keys f) m))
                                            [] (fun k1 k2) (p/tc-map k1 v) (p/tc-map k2 v) ord-k1k2)
     (qname ns_ "member")         (p/prim2 (qname ns_ "member")
                                            (fn [key m] ((maps/hydra_lib_maps_member key) m))
                                            [] k map-kv (p/tc-boolean) ord-k)
     (qname ns_ "null")           (p/prim1 (qname ns_ "null")   maps/hydra_lib_maps_null   [] map-kv (p/tc-boolean) ord-k)
     (qname ns_ "singleton")      (p/prim2 (qname ns_ "singleton")
                                            (fn [key val] ((maps/hydra_lib_maps_singleton key) val))
                                            [] k v map-kv ord-k)
     (qname ns_ "size")           (p/prim1 (qname ns_ "size")   maps/hydra_lib_maps_size   [] map-kv (p/tc-int32) ord-k)
     (qname ns_ "toList")         (p/prim1 (qname ns_ "toList") maps/hydra_lib_maps_to_list [] map-kv (p/tc-list (p/tc-pair k v)) ord-k)
     (qname ns_ "union")          (p/prim2 (qname ns_ "union")
                                            (fn [m1 m2] ((maps/hydra_lib_maps_union m1) m2))
                                            [] map-kv map-kv map-kv ord-k)}))

;; ============================================================
;; Math
;; ============================================================

(defn register-math []
  (let [ns_ "hydra.lib.math"
        i32 (p/tc-int32)
        f32 (p/tc-float32)
        f64 (p/tc-float64)
        bf  (p/tc-bigfloat)
        bi  (p/tc-bigint)
        b   (p/tc-boolean)]
    (merge
     ;; Int32 primitives
     {(qname ns_ "abs")    (p/prim1 (qname ns_ "abs")    math/hydra_lib_math_abs    [] i32 i32)
      (qname ns_ "add")    (p/prim2 (qname ns_ "add")    (fn [a b] ((math/hydra_lib_math_add a) b))    [] i32 i32 i32)
      (qname ns_ "div")    (p/prim2 (qname ns_ "div")    (fn [a b] ((math/hydra_lib_math_div a) b))    [] i32 i32 i32)
      (qname ns_ "even")   (p/prim1 (qname ns_ "even")   math/hydra_lib_math_even   [] i32 b)
      (qname ns_ "mod")    (p/prim2 (qname ns_ "mod")    (fn [a b] ((math/hydra_lib_math_mod a) b))    [] i32 i32 i32)
      (qname ns_ "mul")    (p/prim2 (qname ns_ "mul")    (fn [a b] ((math/hydra_lib_math_mul a) b))    [] i32 i32 i32)
      (qname ns_ "negate") (p/prim1 (qname ns_ "negate") math/hydra_lib_math_negate [] i32 i32)
      (qname ns_ "odd")    (p/prim1 (qname ns_ "odd")    math/hydra_lib_math_odd    [] i32 b)
      (qname ns_ "pred")   (p/prim1 (qname ns_ "pred")   math/hydra_lib_math_pred   [] i32 i32)
      (qname ns_ "range")  (p/prim2 (qname ns_ "range")  (fn [a b] ((math/hydra_lib_math_range a) b))  [] i32 i32 (p/tc-list i32))
      (qname ns_ "rem")    (p/prim2 (qname ns_ "rem")    (fn [a b] ((math/hydra_lib_math_rem a) b))    [] i32 i32 i32)
      (qname ns_ "signum") (p/prim1 (qname ns_ "signum") math/hydra_lib_math_signum [] i32 i32)
      (qname ns_ "sub")    (p/prim2 (qname ns_ "sub")    (fn [a b] ((math/hydra_lib_math_sub a) b))    [] i32 i32 i32)
      (qname ns_ "succ")   (p/prim1 (qname ns_ "succ")   math/hydra_lib_math_succ   [] i32 i32)
      (qname ns_ "max")    (p/prim2 (qname ns_ "max")    (fn [a b] ((math/hydra_lib_math_max a) b))    [] i32 i32 i32)
      (qname ns_ "min")    (p/prim2 (qname ns_ "min")    (fn [a b] ((math/hydra_lib_math_min a) b))    [] i32 i32 i32)}
     ;; Float64 primitives
     {(qname ns_ "acos")     (p/prim1 (qname ns_ "acos")     math/hydra_lib_math_acos     [] f64 f64)
      (qname ns_ "acosh")    (p/prim1 (qname ns_ "acosh")    math/hydra_lib_math_acosh    [] f64 f64)
      (qname ns_ "asin")     (p/prim1 (qname ns_ "asin")     math/hydra_lib_math_asin     [] f64 f64)
      (qname ns_ "asinh")    (p/prim1 (qname ns_ "asinh")    math/hydra_lib_math_asinh    [] f64 f64)
      (qname ns_ "atan")     (p/prim1 (qname ns_ "atan")     math/hydra_lib_math_atan     [] f64 f64)
      (qname ns_ "atan2")    (p/prim2 (qname ns_ "atan2")    (fn [a b] ((math/hydra_lib_math_atan2 a) b)) [] f64 f64 f64)
      (qname ns_ "atanh")    (p/prim1 (qname ns_ "atanh")    math/hydra_lib_math_atanh    [] f64 f64)
      (qname ns_ "ceiling")  (p/prim1 (qname ns_ "ceiling")  math/hydra_lib_math_ceiling  [] f64 bi)
      (qname ns_ "cos")      (p/prim1 (qname ns_ "cos")      math/hydra_lib_math_cos      [] f64 f64)
      (qname ns_ "cosh")     (p/prim1 (qname ns_ "cosh")     math/hydra_lib_math_cosh     [] f64 f64)
      (qname ns_ "e")        (p/prim0 (qname ns_ "e")        (fn [] math/hydra_lib_math_e)        [] f64)
      (qname ns_ "exp")      (p/prim1 (qname ns_ "exp")      math/hydra_lib_math_exp      [] f64 f64)
      (qname ns_ "floor")    (p/prim1 (qname ns_ "floor")    math/hydra_lib_math_floor    [] f64 bi)
      (qname ns_ "log")      (p/prim1 (qname ns_ "log")      math/hydra_lib_math_log      [] f64 f64)
      (qname ns_ "logBase")  (p/prim2 (qname ns_ "logBase")  (fn [a b] ((math/hydra_lib_math_log_base a) b)) [] f64 f64 f64)
      (qname ns_ "pi")       (p/prim0 (qname ns_ "pi")       (fn [] math/hydra_lib_math_pi)       [] f64)
      (qname ns_ "pow")      (p/prim2 (qname ns_ "pow")      (fn [a b] ((math/hydra_lib_math_pow a) b)) [] f64 f64 f64)
      (qname ns_ "round")    (p/prim1 (qname ns_ "round")    math/hydra_lib_math_round    [] f64 bi)
      (qname ns_ "roundBigfloat") (p/prim2 (qname ns_ "roundBigfloat")
                                            (fn [n x] ((math/hydra_lib_math_round_bigfloat n) x))
                                            [] i32 bf bf)
      (qname ns_ "roundFloat32")  (p/prim2 (qname ns_ "roundFloat32")
                                            (fn [n x] ((math/hydra_lib_math_round_float32 n) x))
                                            [] i32 f32 f32)
      (qname ns_ "roundFloat64")  (p/prim2 (qname ns_ "roundFloat64")
                                            (fn [n x] ((math/hydra_lib_math_round_float64 n) x))
                                            [] i32 f64 f64)
      (qname ns_ "sin")      (p/prim1 (qname ns_ "sin")      math/hydra_lib_math_sin      [] f64 f64)
      (qname ns_ "sinh")     (p/prim1 (qname ns_ "sinh")     math/hydra_lib_math_sinh     [] f64 f64)
      (qname ns_ "sqrt")     (p/prim1 (qname ns_ "sqrt")     math/hydra_lib_math_sqrt     [] f64 f64)
      (qname ns_ "tan")      (p/prim1 (qname ns_ "tan")      math/hydra_lib_math_tan      [] f64 f64)
      (qname ns_ "tanh")     (p/prim1 (qname ns_ "tanh")     math/hydra_lib_math_tanh     [] f64 f64)
      (qname ns_ "truncate") (p/prim1 (qname ns_ "truncate") math/hydra_lib_math_truncate [] f64 bi)})))

;; ============================================================
;; Maybes
;; ============================================================

(defn register-maybes []
  (let [ns_ "hydra.lib.maybes"
        a (p/tc-variable "a")
        b (p/tc-variable "b")
        c (p/tc-variable "c")]
    {(qname ns_ "apply")    (p/prim2 (qname ns_ "apply")
                                      (fn [mf mx] ((maybes/hydra_lib_maybes_apply mf) mx))
                                      [] (p/tc-optional (fun a b)) (p/tc-optional a) (p/tc-optional b))
     (qname ns_ "bind")     (p/prim2 (qname ns_ "bind")
                                      (fn [mx f] ((maybes/hydra_lib_maybes_bind mx) f))
                                      [] (p/tc-optional a) (fun a (p/tc-optional b)) (p/tc-optional b))
     (qname ns_ "cases")    (p/prim3 (qname ns_ "cases")
                                      (fn [mx dflt f] (((maybes/hydra_lib_maybes_cases mx) dflt) f))
                                      [] (p/tc-optional a) b (fun a b) b)
     (qname ns_ "cat")      (p/prim1 (qname ns_ "cat")      maybes/hydra_lib_maybes_cat      [] (p/tc-list (p/tc-optional a)) (p/tc-list a))
     (qname ns_ "compose")  (p/prim3 (qname ns_ "compose")
                                      (fn [f g x] (((maybes/hydra_lib_maybes_compose f) g) x))
                                      [] (fun a (p/tc-optional b)) (fun b (p/tc-optional c)) a (p/tc-optional c))
     (qname ns_ "fromJust") (p/prim1 (qname ns_ "fromJust") maybes/hydra_lib_maybes_from_just [] (p/tc-optional a) a)
     (qname ns_ "fromMaybe") (p/prim2 (qname ns_ "fromMaybe")
                                       (fn [dflt mx] ((maybes/hydra_lib_maybes_from_maybe dflt) mx))
                                       [] a (p/tc-optional a) a)
     (qname ns_ "isJust")    (p/prim1 (qname ns_ "isJust")    maybes/hydra_lib_maybes_is_just    [] (p/tc-optional a) (p/tc-boolean))
     (qname ns_ "isNothing") (p/prim1 (qname ns_ "isNothing") maybes/hydra_lib_maybes_is_nothing [] (p/tc-optional a) (p/tc-boolean))
     (qname ns_ "map")       (p/prim2 (qname ns_ "map")
                                       (fn [f mx] ((maybes/hydra_lib_maybes_map f) mx))
                                       [] (fun a b) (p/tc-optional a) (p/tc-optional b))
     (qname ns_ "mapMaybe")  (p/prim2 (qname ns_ "mapMaybe")
                                       (fn [f xs] ((maybes/hydra_lib_maybes_map_maybe f) xs))
                                       [] (fun a (p/tc-optional b)) (p/tc-list a) (p/tc-list b))
     (qname ns_ "maybe")     (p/prim3 (qname ns_ "maybe")
                                       (fn [dflt f mx] (((maybes/hydra_lib_maybes_maybe dflt) f) mx))
                                       [] b (fun a b) (p/tc-optional a) b)
     (qname ns_ "pure")      (p/prim1 (qname ns_ "pure")      maybes/hydra_lib_maybes_pure      [] a (p/tc-optional a))
     (qname ns_ "toList")    (p/prim1 (qname ns_ "toList")    maybes/hydra_lib_maybes_to_list   [] (p/tc-optional a) (p/tc-list a))}))

;; ============================================================
;; Pairs
;; ============================================================

(defn register-pairs []
  (let [ns_ "hydra.lib.pairs"
        a (p/tc-variable "a")
        b (p/tc-variable "b")
        c (p/tc-variable "c")
        d (p/tc-variable "d")]
    {(qname ns_ "bimap")  (p/prim3 (qname ns_ "bimap")
                                    (fn [f g p] (((pairs/hydra_lib_pairs_bimap f) g) p))
                                    [] (fun a c) (fun b d) (p/tc-pair a b) (p/tc-pair c d))
     (qname ns_ "first")  (p/prim1 (qname ns_ "first")  pairs/hydra_lib_pairs_first  [] (p/tc-pair a b) a)
     (qname ns_ "second") (p/prim1 (qname ns_ "second") pairs/hydra_lib_pairs_second [] (p/tc-pair a b) b)}))

;; ============================================================
;; Sets
;; ============================================================

(defn register-sets []
  (let [ns_ "hydra.lib.sets"
        a (p/tc-variable "a")
        b (p/tc-variable "b")
        ;; All set primitives have ordering constraint on element type
        ord-a {"a" ["ordering"]}
        ord-ab {"a" ["ordering"] "b" ["ordering"]}]
    {(qname ns_ "delete")       (p/prim2 (qname ns_ "delete")
                                          (fn [x s] ((sets/hydra_lib_sets_delete x) s))
                                          [] a (p/tc-set a) (p/tc-set a) ord-a)
     (qname ns_ "difference")   (p/prim2 (qname ns_ "difference")
                                          (fn [s1 s2] ((sets/hydra_lib_sets_difference s1) s2))
                                          [] (p/tc-set a) (p/tc-set a) (p/tc-set a) ord-a)
     (qname ns_ "empty")        (p/prim0 (qname ns_ "empty")   (fn [] sets/hydra_lib_sets_empty)   [] (p/tc-set a) ord-a)
     (qname ns_ "fromList")     (p/prim1 (qname ns_ "fromList") sets/hydra_lib_sets_from_list [] (p/tc-list a) (p/tc-set a) ord-a)
     (qname ns_ "insert")       (p/prim2 (qname ns_ "insert")
                                          (fn [x s] ((sets/hydra_lib_sets_insert x) s))
                                          [] a (p/tc-set a) (p/tc-set a) ord-a)
     (qname ns_ "intersection") (p/prim2 (qname ns_ "intersection")
                                          (fn [s1 s2] ((sets/hydra_lib_sets_intersection s1) s2))
                                          [] (p/tc-set a) (p/tc-set a) (p/tc-set a) ord-a)
     (qname ns_ "map")          (p/prim2 (qname ns_ "map")
                                          (fn [f s] ((sets/hydra_lib_sets_map f) s))
                                          [] (fun a b) (p/tc-set a) (p/tc-set b) ord-ab)
     (qname ns_ "member")       (p/prim2 (qname ns_ "member")
                                          (fn [x s] ((sets/hydra_lib_sets_member x) s))
                                          [] a (p/tc-set a) (p/tc-boolean) ord-a)
     (qname ns_ "null")         (p/prim1 (qname ns_ "null")     sets/hydra_lib_sets_null     [] (p/tc-set a) (p/tc-boolean) ord-a)
     (qname ns_ "singleton")    (p/prim1 (qname ns_ "singleton") sets/hydra_lib_sets_singleton [] a (p/tc-set a) ord-a)
     (qname ns_ "size")         (p/prim1 (qname ns_ "size")     sets/hydra_lib_sets_size     [] (p/tc-set a) (p/tc-int32) ord-a)
     (qname ns_ "toList")       (p/prim1 (qname ns_ "toList")   sets/hydra_lib_sets_to_list  [] (p/tc-set a) (p/tc-list a) ord-a)
     (qname ns_ "union")        (p/prim2 (qname ns_ "union")
                                          (fn [s1 s2] ((sets/hydra_lib_sets_union s1) s2))
                                          [] (p/tc-set a) (p/tc-set a) (p/tc-set a) ord-a)
     (qname ns_ "unions")       (p/prim1 (qname ns_ "unions")   sets/hydra_lib_sets_unions   [] (p/tc-list (p/tc-set a)) (p/tc-set a) ord-a)}))

;; ============================================================
;; Strings
;; ============================================================

(defn register-strings []
  (let [ns_ "hydra.lib.strings"
        s (p/tc-string)
        i (p/tc-int32)
        b (p/tc-boolean)]
    {(qname ns_ "cat")         (p/prim1 (qname ns_ "cat")         strings/hydra_lib_strings_cat         [] (p/tc-list s) s)
     (qname ns_ "cat2")        (p/prim2 (qname ns_ "cat2")
                                         (fn [a b] ((strings/hydra_lib_strings_cat2 a) b))
                                         [] s s s)
     (qname ns_ "charAt")      (p/prim2 (qname ns_ "charAt")
                                         (fn [n str] ((strings/hydra_lib_strings_char_at n) str))
                                         [] i s i)
     (qname ns_ "fromList")    (p/prim1 (qname ns_ "fromList")    strings/hydra_lib_strings_from_list    [] (p/tc-list i) s)
     (qname ns_ "intercalate") (p/prim2 (qname ns_ "intercalate")
                                         (fn [sep ss] ((strings/hydra_lib_strings_intercalate sep) ss))
                                         [] s (p/tc-list s) s)
     (qname ns_ "length")      (p/prim1 (qname ns_ "length")      strings/hydra_lib_strings_length      [] s i)
     (qname ns_ "lines")       (p/prim1 (qname ns_ "lines")       strings/hydra_lib_strings_lines       [] s (p/tc-list s))
     (qname ns_ "null")        (p/prim1 (qname ns_ "null")        strings/hydra_lib_strings_null        [] s b)
     (qname ns_ "splitOn")     (p/prim2 (qname ns_ "splitOn")
                                         (fn [sep str] ((strings/hydra_lib_strings_split_on sep) str))
                                         [] s s (p/tc-list s))
     (qname ns_ "toList")      (p/prim1 (qname ns_ "toList")      strings/hydra_lib_strings_to_list     [] s (p/tc-list i))
     (qname ns_ "toLower")     (p/prim1 (qname ns_ "toLower")     strings/hydra_lib_strings_to_lower    [] s s)
     (qname ns_ "toUpper")     (p/prim1 (qname ns_ "toUpper")     strings/hydra_lib_strings_to_upper    [] s s)
     (qname ns_ "unlines")     (p/prim1 (qname ns_ "unlines")     strings/hydra_lib_strings_unlines     [] (p/tc-list s) s)}))

;; ============================================================
;; Literals
;; ============================================================

(defn register-literals []
  (let [ns_ "hydra.lib.literals"
        bf  (p/tc-bigfloat)
        bi  (p/tc-bigint)
        f32 (p/tc-float32)
        f64 (p/tc-float64)
        i8  (p/tc-int8)
        i16 (p/tc-int16)
        i32 (p/tc-int32)
        i64 (p/tc-int64)
        u8  (p/tc-uint8)
        u16 (p/tc-uint16)
        u32 (p/tc-uint32)
        u64 (p/tc-uint64)
        s   (p/tc-string)
        b   (p/tc-boolean)
        bin (p/tc-binary)]
    (merge
     ;; Conversions
     {(qname ns_ "bigfloatToBigint")   (p/prim1 (qname ns_ "bigfloatToBigint")   literals/hydra_lib_literals_bigfloat_to_bigint   [] bf bi)
      (qname ns_ "bigfloatToFloat32")  (p/prim1 (qname ns_ "bigfloatToFloat32")  literals/hydra_lib_literals_bigfloat_to_float32  [] bf f32)
      (qname ns_ "bigfloatToFloat64")  (p/prim1 (qname ns_ "bigfloatToFloat64")  literals/hydra_lib_literals_bigfloat_to_float64  [] bf f64)
      (qname ns_ "bigintToBigfloat")   (p/prim1 (qname ns_ "bigintToBigfloat")   literals/hydra_lib_literals_bigint_to_bigfloat   [] bi bf)
      (qname ns_ "bigintToInt8")       (p/prim1 (qname ns_ "bigintToInt8")       literals/hydra_lib_literals_bigint_to_int8       [] bi i8)
      (qname ns_ "bigintToInt16")      (p/prim1 (qname ns_ "bigintToInt16")      literals/hydra_lib_literals_bigint_to_int16      [] bi i16)
      (qname ns_ "bigintToInt32")      (p/prim1 (qname ns_ "bigintToInt32")      literals/hydra_lib_literals_bigint_to_int32      [] bi i32)
      (qname ns_ "bigintToInt64")      (p/prim1 (qname ns_ "bigintToInt64")      literals/hydra_lib_literals_bigint_to_int64      [] bi i64)
      (qname ns_ "bigintToUint8")      (p/prim1 (qname ns_ "bigintToUint8")      literals/hydra_lib_literals_bigint_to_uint8      [] bi u8)
      (qname ns_ "bigintToUint16")     (p/prim1 (qname ns_ "bigintToUint16")     literals/hydra_lib_literals_bigint_to_uint16     [] bi u16)
      (qname ns_ "bigintToUint32")     (p/prim1 (qname ns_ "bigintToUint32")     literals/hydra_lib_literals_bigint_to_uint32     [] bi u32)
      (qname ns_ "bigintToUint64")     (p/prim1 (qname ns_ "bigintToUint64")     literals/hydra_lib_literals_bigint_to_uint64     [] bi u64)
      (qname ns_ "binaryToBytes")      (p/prim1 (qname ns_ "binaryToBytes")      literals/hydra_lib_literals_binary_to_bytes      [] bin (p/tc-list i32))
      (qname ns_ "binaryToString")     (p/prim1 (qname ns_ "binaryToString")     literals/hydra_lib_literals_binary_to_string     [] bin s)
      (qname ns_ "float32ToBigfloat")  (p/prim1 (qname ns_ "float32ToBigfloat")  literals/hydra_lib_literals_float32_to_bigfloat  [] f32 bf)
      (qname ns_ "float64ToBigfloat")  (p/prim1 (qname ns_ "float64ToBigfloat")  literals/hydra_lib_literals_float64_to_bigfloat  [] f64 bf)
      (qname ns_ "int8ToBigint")       (p/prim1 (qname ns_ "int8ToBigint")       literals/hydra_lib_literals_int8_to_bigint       [] i8 bi)
      (qname ns_ "int16ToBigint")      (p/prim1 (qname ns_ "int16ToBigint")      literals/hydra_lib_literals_int16_to_bigint      [] i16 bi)
      (qname ns_ "int32ToBigint")      (p/prim1 (qname ns_ "int32ToBigint")      literals/hydra_lib_literals_int32_to_bigint      [] i32 bi)
      (qname ns_ "int64ToBigint")      (p/prim1 (qname ns_ "int64ToBigint")      literals/hydra_lib_literals_int64_to_bigint      [] i64 bi)
      (qname ns_ "uint8ToBigint")      (p/prim1 (qname ns_ "uint8ToBigint")      literals/hydra_lib_literals_uint8_to_bigint      [] u8 bi)
      (qname ns_ "uint16ToBigint")     (p/prim1 (qname ns_ "uint16ToBigint")     literals/hydra_lib_literals_uint16_to_bigint     [] u16 bi)
      (qname ns_ "uint32ToBigint")     (p/prim1 (qname ns_ "uint32ToBigint")     literals/hydra_lib_literals_uint32_to_bigint     [] u32 bi)
      (qname ns_ "uint64ToBigint")     (p/prim1 (qname ns_ "uint64ToBigint")     literals/hydra_lib_literals_uint64_to_bigint     [] u64 bi)
      (qname ns_ "stringToBinary")     (p/prim1 (qname ns_ "stringToBinary")     literals/hydra_lib_literals_string_to_binary     [] s bin)}
     ;; Read primitives
     {(qname ns_ "readBigfloat") (p/prim1 (qname ns_ "readBigfloat") literals/hydra_lib_literals_read_bigfloat [] s (p/tc-optional bf))
      (qname ns_ "readBigint")   (p/prim1 (qname ns_ "readBigint")   literals/hydra_lib_literals_read_bigint   [] s (p/tc-optional bi))
      (qname ns_ "readBoolean")  (p/prim1 (qname ns_ "readBoolean")  literals/hydra_lib_literals_read_boolean  [] s (p/tc-optional b))
      (qname ns_ "readFloat32")  (p/prim1 (qname ns_ "readFloat32")  literals/hydra_lib_literals_read_float32  [] s (p/tc-optional f32))
      (qname ns_ "readFloat64")  (p/prim1 (qname ns_ "readFloat64")  literals/hydra_lib_literals_read_float64  [] s (p/tc-optional f64))
      (qname ns_ "readInt8")     (p/prim1 (qname ns_ "readInt8")     literals/hydra_lib_literals_read_int8     [] s (p/tc-optional i8))
      (qname ns_ "readInt16")    (p/prim1 (qname ns_ "readInt16")    literals/hydra_lib_literals_read_int16    [] s (p/tc-optional i16))
      (qname ns_ "readInt32")    (p/prim1 (qname ns_ "readInt32")    literals/hydra_lib_literals_read_int32    [] s (p/tc-optional i32))
      (qname ns_ "readInt64")    (p/prim1 (qname ns_ "readInt64")    literals/hydra_lib_literals_read_int64    [] s (p/tc-optional i64))
      (qname ns_ "readString")   (p/prim1 (qname ns_ "readString")   literals/hydra_lib_literals_read_string   [] s (p/tc-optional s))
      (qname ns_ "readUint8")    (p/prim1 (qname ns_ "readUint8")    literals/hydra_lib_literals_read_uint8    [] s (p/tc-optional u8))
      (qname ns_ "readUint16")   (p/prim1 (qname ns_ "readUint16")   literals/hydra_lib_literals_read_uint16   [] s (p/tc-optional u16))
      (qname ns_ "readUint32")   (p/prim1 (qname ns_ "readUint32")   literals/hydra_lib_literals_read_uint32   [] s (p/tc-optional u32))
      (qname ns_ "readUint64")   (p/prim1 (qname ns_ "readUint64")   literals/hydra_lib_literals_read_uint64   [] s (p/tc-optional u64))}
     ;; Show primitives
     {(qname ns_ "showBigfloat") (p/prim1 (qname ns_ "showBigfloat") literals/hydra_lib_literals_show_bigfloat [] bf s)
      (qname ns_ "showBigint")   (p/prim1 (qname ns_ "showBigint")   literals/hydra_lib_literals_show_bigint   [] bi s)
      (qname ns_ "showBoolean")  (p/prim1 (qname ns_ "showBoolean")  literals/hydra_lib_literals_show_boolean  [] b s)
      (qname ns_ "showFloat32")  (p/prim1 (qname ns_ "showFloat32")  literals/hydra_lib_literals_show_float32  [] f32 s)
      (qname ns_ "showFloat64")  (p/prim1 (qname ns_ "showFloat64")  literals/hydra_lib_literals_show_float64  [] f64 s)
      (qname ns_ "showInt8")     (p/prim1 (qname ns_ "showInt8")     literals/hydra_lib_literals_show_int8     [] i8 s)
      (qname ns_ "showInt16")    (p/prim1 (qname ns_ "showInt16")    literals/hydra_lib_literals_show_int16    [] i16 s)
      (qname ns_ "showInt32")    (p/prim1 (qname ns_ "showInt32")    literals/hydra_lib_literals_show_int32    [] i32 s)
      (qname ns_ "showInt64")    (p/prim1 (qname ns_ "showInt64")    literals/hydra_lib_literals_show_int64    [] i64 s)
      (qname ns_ "showUint8")    (p/prim1 (qname ns_ "showUint8")    literals/hydra_lib_literals_show_uint8    [] u8 s)
      (qname ns_ "showUint16")   (p/prim1 (qname ns_ "showUint16")   literals/hydra_lib_literals_show_uint16   [] u16 s)
      (qname ns_ "showUint32")   (p/prim1 (qname ns_ "showUint32")   literals/hydra_lib_literals_show_uint32   [] u32 s)
      (qname ns_ "showUint64")   (p/prim1 (qname ns_ "showUint64")   literals/hydra_lib_literals_show_uint64   [] u64 s)
      (qname ns_ "showString")   (p/prim1 (qname ns_ "showString")   literals/hydra_lib_literals_show_string   [] s s)})))

;; ============================================================
;; ============================================================
;; Annotations (term-level functions registered as primitives)
;; ============================================================

(defn- term-maybe-to-native
  "Convert a term-level maybe (:maybe val_or_nil) to native maybe (:just val) / (:nothing)"
  [m]
  (cond
    (nil? m) (list :nothing)
    (not (sequential? m)) (list :just m)
    (= (first m) :nothing) (list :nothing)
    (= (first m) :just) m
    (= (first m) :maybe)
    (let [inner (second m)]
      (if (or (nil? inner)
              (and (sequential? inner) (= (first inner) :nothing)))
        (list :nothing)
        (list :just inner)))
    :else (list :just m)))

(defn- native-maybe-to-term
  "Convert a native maybe (:just val) / (:nothing) to term-level (:maybe val_or_nil)"
  [m]
  (cond
    (nil? m) (list :maybe nil)
    (not (sequential? m)) (list :maybe m)
    (= (first m) :just) (list :maybe (second m))
    (= (first m) :nothing) (list :maybe nil)
    :else (list :maybe m)))

(defn register-annotations []
  (let [t (p/tc-term)]
    {;; setTermAnnotation :: Name -> Maybe Term -> Term -> Term
     "hydra.annotations.setTermAnnotation"
     (p/prim3 "hydra.annotations.setTermAnnotation"
              (fn [key val term]
                (let [native-maybe (term-maybe-to-native val)]
                  ((((deref (or (resolve 'hydra_annotations_set_term_annotation) (ns-resolve 'hydra.annotations 'hydra_annotations_set_term_annotation))) key) native-maybe) term)))
              [] t t t t)
     ;; getTermAnnotation :: Name -> Term -> Maybe Term
     "hydra.annotations.getTermAnnotation"
     (p/prim2 "hydra.annotations.getTermAnnotation"
              (fn [key term]
                (native-maybe-to-term
                 (((deref (or (resolve 'hydra_annotations_get_term_annotation) (ns-resolve 'hydra.annotations 'hydra_annotations_get_term_annotation))) key) term)))
              [] t t t)
     ;; setTermDescription :: Maybe String -> Term -> Term
     "hydra.annotations.setTermDescription"
     (p/prim2 "hydra.annotations.setTermDescription"
              (fn [d term]
                (let [native-d (term-maybe-to-native d)
                      ;; Extract string from (:literal (:string "...")) wrapper if present
                      native-str-d (cond
                                     (= (first native-d) :nothing) (list :nothing)
                                     :else (let [inner (second native-d)]
                                             (if (and (sequential? inner) (= (first inner) :literal)
                                                      (sequential? (second inner)) (= (first (second inner)) :string))
                                               (list :just (second (second inner)))
                                               native-d)))]
                  (((deref (or (resolve 'hydra_annotations_set_term_description) (ns-resolve 'hydra.annotations 'hydra_annotations_set_term_description))) native-str-d) term)))
              [] t t t)
     ;; getTermDescription :: Context -> Graph -> Term -> Either (Maybe String)
     "hydra.annotations.getTermDescription"
     (p/prim3 "hydra.annotations.getTermDescription"
              (fn [cx graph term]
                (let [result ((((deref (or (resolve 'hydra_annotations_get_term_description) (ns-resolve 'hydra.annotations 'hydra_annotations_get_term_description))) cx) graph) term)]
                  ;; Result is Either Error (Maybe String)
                  ;; Convert to term-level: (:either (:right (:maybe (:literal (:string "...")))))
                  (if (= (first result) :left)
                    (list :either result)
                    (let [maybe-str (second result)
                          term-maybe (cond
                                       (nil? maybe-str) (list :maybe nil)
                                       (= (first maybe-str) :nothing) (list :maybe nil)
                                       (= (first maybe-str) :just)
                                       (list :maybe (list :literal (list :string (second maybe-str))))
                                       :else (list :maybe maybe-str))]
                      (list :either (list :right term-maybe))))))
              [] t t t t)}))

;; Standard library: all primitives combined
;; ============================================================

(defn standard-library
  "Returns a map from primitive name (string) to Primitive record."
  []
  (merge
   (register-chars)
   (register-equality)
   (register-eithers)
   (register-lists)
   (register-literals)
   (register-logic)
   (register-maps)
   (register-math)
   (register-maybes)
   (register-pairs)
   (register-sets)
   (register-strings)
   (register-annotations)))
