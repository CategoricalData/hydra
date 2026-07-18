(ns hydra.overlay.clojure.libraries
  (:require [hydra.overlay.clojure.dsl.prims :as p]
            [hydra.overlay.clojure.lib.chars :as chars]
            [hydra.overlay.clojure.lib.effects :as effects]
            [hydra.overlay.clojure.lib.eithers :as eithers]
            [hydra.overlay.clojure.lib.equality :as equality]
            [hydra.overlay.clojure.lib.files :as files]
            [hydra.overlay.clojure.lib.hashing :as hashing]
            [hydra.overlay.clojure.lib.lists :as lists]
            [hydra.overlay.clojure.lib.literals :as literals]
            [hydra.overlay.clojure.lib.logic :as logic]
            [hydra.overlay.clojure.lib.maps :as maps]
            [hydra.overlay.clojure.lib.math :as math]
            [hydra.overlay.clojure.lib.optionals :as optionals]
            [hydra.overlay.clojure.lib.pairs :as pairs]
            [hydra.overlay.clojure.lib.regex :as regex]
            [hydra.overlay.clojure.lib.sets :as sets]
            [hydra.overlay.clojure.lib.strings :as strings]
            [hydra.overlay.clojure.lib.system :as system]
            [hydra.overlay.clojure.lib.text :as text]))

(defn- prim-name
  "Derive a primitive's canonical name from its generated hydra.lib.* PrimitiveDefinition var
   (the single source of truth for primitive names, #473). `defsym` is the fully-qualified def var,
   e.g. 'hydra.lib.chars/hydra_lib_chars_is_alpha_num. Uses requiring-resolve so the def-module is
   loaded lazily on first use — after load-gen-main! has loaded the kernel deps it transitively needs,
   which is the documented precondition for loading this registry."
  [defsym]
  (:name @(requiring-resolve defsym)))

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
  (let []
    {(prim-name 'hydra.lib.chars/hydra_lib_chars_is_alpha_num) (p/prim1 (prim-name 'hydra.lib.chars/hydra_lib_chars_is_alpha_num) chars/hydra_lib_chars_is_alpha_num [] (p/tc-int32) (p/tc-boolean))
     (prim-name 'hydra.lib.chars/hydra_lib_chars_is_lower)    (p/prim1 (prim-name 'hydra.lib.chars/hydra_lib_chars_is_lower)    chars/hydra_lib_chars_is_lower    [] (p/tc-int32) (p/tc-boolean))
     (prim-name 'hydra.lib.chars/hydra_lib_chars_is_space)    (p/prim1 (prim-name 'hydra.lib.chars/hydra_lib_chars_is_space)    chars/hydra_lib_chars_is_space    [] (p/tc-int32) (p/tc-boolean))
     (prim-name 'hydra.lib.chars/hydra_lib_chars_is_upper)    (p/prim1 (prim-name 'hydra.lib.chars/hydra_lib_chars_is_upper)    chars/hydra_lib_chars_is_upper    [] (p/tc-int32) (p/tc-boolean))
     (prim-name 'hydra.lib.chars/hydra_lib_chars_to_lower)    (p/prim1 (prim-name 'hydra.lib.chars/hydra_lib_chars_to_lower)    chars/hydra_lib_chars_to_lower    [] (p/tc-int32) (p/tc-int32))
     (prim-name 'hydra.lib.chars/hydra_lib_chars_to_upper)    (p/prim1 (prim-name 'hydra.lib.chars/hydra_lib_chars_to_upper)    chars/hydra_lib_chars_to_upper    [] (p/tc-int32) (p/tc-int32))}))

;; ============================================================
;; Eithers
;; ============================================================

(defn register-eithers []
  (let [
        x (p/tc-variable "x")
        y (p/tc-variable "y")
        z (p/tc-variable "z")
        w (p/tc-variable "w")]
    {(prim-name 'hydra.lib.eithers/hydra_lib_eithers_bind)    (p/prim2 (prim-name 'hydra.lib.eithers/hydra_lib_eithers_bind)
                                     (fn [e f] ((eithers/hydra_lib_eithers_bind e) f))
                                     [] (p/tc-either x y) (fun y (p/tc-either x z)) (p/tc-either x z))
     ;; BUG #438: explicit variable order (see mapList comment below).
     (prim-name 'hydra.lib.eithers/hydra_lib_eithers_bimap)   (p/prim3 (prim-name 'hydra.lib.eithers/hydra_lib_eithers_bimap)
                                     (fn [f g e] (((eithers/hydra_lib_eithers_bimap f) g) e))
                                     ["x" "y" "z" "w"] (fun x z) (fun y w) (p/tc-either x y) (p/tc-either z w))
     (prim-name 'hydra.lib.eithers/hydra_lib_eithers_either)  (p/prim3 (prim-name 'hydra.lib.eithers/hydra_lib_eithers_either)
                                     (fn [f g e] (((eithers/hydra_lib_eithers_either f) g) e))
                                     ["x" "y" "z"] (fun x z) (fun y z) (p/tc-either x y) z)
     (prim-name 'hydra.lib.eithers/hydra_lib_eithers_foldl)   (p/prim3 (prim-name 'hydra.lib.eithers/hydra_lib_eithers_foldl)
                                     (fn [f init xs] (((eithers/hydra_lib_eithers_foldl f) init) xs))
                                     [] (fun x (fun y (p/tc-either z x))) x (p/tc-list y) (p/tc-either z x))
     (prim-name 'hydra.lib.eithers/hydra_lib_eithers_from_left)  (p/lazy-args [0] (p/prim2 (prim-name 'hydra.lib.eithers/hydra_lib_eithers_from_left)
                                       (fn [dflt e] ((eithers/hydra_lib_eithers_from_left dflt) e))
                                       [] x (p/tc-either x y) x))
     (prim-name 'hydra.lib.eithers/hydra_lib_eithers_from_right) (p/lazy-args [0] (p/prim2 (prim-name 'hydra.lib.eithers/hydra_lib_eithers_from_right)
                                       (fn [dflt e] ((eithers/hydra_lib_eithers_from_right dflt) e))
                                       ["x" "y"] y (p/tc-either x y) y))
     (prim-name 'hydra.lib.eithers/hydra_lib_eithers_is_left)  (p/prim1 (prim-name 'hydra.lib.eithers/hydra_lib_eithers_is_left)  eithers/hydra_lib_eithers_is_left  [] (p/tc-either x y) (p/tc-boolean))
     (prim-name 'hydra.lib.eithers/hydra_lib_eithers_is_right) (p/prim1 (prim-name 'hydra.lib.eithers/hydra_lib_eithers_is_right) eithers/hydra_lib_eithers_is_right [] (p/tc-either x y) (p/tc-boolean))
     (prim-name 'hydra.lib.eithers/hydra_lib_eithers_lefts)   (p/prim1 (prim-name 'hydra.lib.eithers/hydra_lib_eithers_lefts)   eithers/hydra_lib_eithers_lefts   [] (p/tc-list (p/tc-either x y)) (p/tc-list x))
     (prim-name 'hydra.lib.eithers/hydra_lib_eithers_map)     (p/prim2 (prim-name 'hydra.lib.eithers/hydra_lib_eithers_map)
                                     (fn [f e] ((eithers/hydra_lib_eithers_map f) e))
                                     [] (fun x y) (p/tc-either z x) (p/tc-either z y))
     ;; BUG #438: explicit ["x" "y" "z"] order. With auto-detect (empty []),
     ;; build-type-scheme would walk (x -> Either z y) and produce [x, z, y]
     ;; — the order in which type variables first appear in the body. The
     ;; kernel's hand-written sig forces [x, y, z] (see hydra-kernel/Sources/
     ;; Kernel/Lib/Eithers.hs mapListSig). The two are isomorphic as type
     ;; schemes but applyTypeArgumentsToType peels foralls left-to-right, so
     ;; the variable ORDER determines which fresh type argument lands in
     ;; which body position. Without the explicit list, instantiation puts
     ;; the error-type fresh var where the success-type fresh var should go
     ;; — visible as a (Binding -> Either<Unit, Error>) vs
     ;; (Binding -> Either<Error, Unit>) swap in type-check errors during
     ;; clojure-to-java codegen of mapList/mapMaybe/mapSet over forBinding
     ;; (`Binding -> Either Error ()`). Mirrors the TS host fix e8a0abf254.
     (prim-name 'hydra.lib.eithers/hydra_lib_eithers_map_list) (p/prim2 (prim-name 'hydra.lib.eithers/hydra_lib_eithers_map_list)
                                     (fn [f xs] ((eithers/hydra_lib_eithers_map_list f) xs))
                                     ["x" "y" "z"] (fun x (p/tc-either z y)) (p/tc-list x) (p/tc-either z (p/tc-list y)))
     (prim-name 'hydra.lib.eithers/hydra_lib_eithers_map_optional) (p/prim2 (prim-name 'hydra.lib.eithers/hydra_lib_eithers_map_optional)
                                      (fn [f mx] ((eithers/hydra_lib_eithers_map_optional f) mx))
                                      ["x" "y" "z"] (fun x (p/tc-either z y)) (p/tc-optional x) (p/tc-either z (p/tc-optional y)))
     (prim-name 'hydra.lib.eithers/hydra_lib_eithers_map_set)  (p/prim2 (prim-name 'hydra.lib.eithers/hydra_lib_eithers_map_set)
                                     (fn [f s] ((eithers/hydra_lib_eithers_map_set f) s))
                                     ["x" "y" "z"] (fun x (p/tc-either z y)) (p/tc-set x) (p/tc-either z (p/tc-set y)))
     (prim-name 'hydra.lib.eithers/hydra_lib_eithers_partition_eithers) (p/prim1 (prim-name 'hydra.lib.eithers/hydra_lib_eithers_partition_eithers)
                                              eithers/hydra_lib_eithers_partition_eithers
                                              [] (p/tc-list (p/tc-either x y)) (p/tc-pair (p/tc-list x) (p/tc-list y)))
     (prim-name 'hydra.lib.eithers/hydra_lib_eithers_rights)  (p/prim1 (prim-name 'hydra.lib.eithers/hydra_lib_eithers_rights)  eithers/hydra_lib_eithers_rights  [] (p/tc-list (p/tc-either x y)) (p/tc-list y))}))

;; ============================================================
;; Equality
;; ============================================================

(defn register-equality []
  (let [
        x (p/tc-variable "x")
        ord-x {"x" ["ordering"]}
        eq-x {"x" ["equality"]}]
    {(prim-name 'hydra.lib.equality/hydra_lib_equality_compare)  (p/prim2 (prim-name 'hydra.lib.equality/hydra_lib_equality_compare)  (fn [a b] ((equality/hydra_lib_equality_compare a) b))  [] x x (p/tc-comparison) ord-x)
     (prim-name 'hydra.lib.equality/hydra_lib_equality_equal)    (p/prim2 (prim-name 'hydra.lib.equality/hydra_lib_equality_equal)    (fn [a b] ((equality/hydra_lib_equality_equal a) b))    [] x x (p/tc-boolean) eq-x)
     (prim-name 'hydra.lib.equality/hydra_lib_equality_gt)       (p/prim2 (prim-name 'hydra.lib.equality/hydra_lib_equality_gt)       (fn [a b] ((equality/hydra_lib_equality_gt a) b))       [] x x (p/tc-boolean) ord-x)
     (prim-name 'hydra.lib.equality/hydra_lib_equality_gte)      (p/prim2 (prim-name 'hydra.lib.equality/hydra_lib_equality_gte)      (fn [a b] ((equality/hydra_lib_equality_gte a) b))      [] x x (p/tc-boolean) ord-x)
     (prim-name 'hydra.lib.equality/hydra_lib_equality_identity) (p/prim1 (prim-name 'hydra.lib.equality/hydra_lib_equality_identity) identity                             [] x x)
     (prim-name 'hydra.lib.equality/hydra_lib_equality_lt)       (p/prim2 (prim-name 'hydra.lib.equality/hydra_lib_equality_lt)       (fn [a b] ((equality/hydra_lib_equality_lt a) b))       [] x x (p/tc-boolean) ord-x)
     (prim-name 'hydra.lib.equality/hydra_lib_equality_lte)      (p/prim2 (prim-name 'hydra.lib.equality/hydra_lib_equality_lte)      (fn [a b] ((equality/hydra_lib_equality_lte a) b))      [] x x (p/tc-boolean) ord-x)
     (prim-name 'hydra.lib.equality/hydra_lib_equality_max)      (p/prim2 (prim-name 'hydra.lib.equality/hydra_lib_equality_max)      (fn [a b] ((equality/hydra_lib_equality_max a) b))      [] x x x ord-x)
     (prim-name 'hydra.lib.equality/hydra_lib_equality_min)      (p/prim2 (prim-name 'hydra.lib.equality/hydra_lib_equality_min)      (fn [a b] ((equality/hydra_lib_equality_min a) b))      [] x x x ord-x)}))

;; ============================================================
;; Lists
;; ============================================================

(defn register-lists []
  (let [
        a (p/tc-variable "a")
        b (p/tc-variable "b")
        c (p/tc-variable "c")]
    {(prim-name 'hydra.lib.lists/hydra_lib_lists_apply)      (p/prim2 (prim-name 'hydra.lib.lists/hydra_lib_lists_apply)
                                        (fn [fs xs] ((lists/hydra_lib_lists_apply fs) xs))
                                        [] (p/tc-list (fun a b)) (p/tc-list a) (p/tc-list b))
     (prim-name 'hydra.lib.lists/hydra_lib_lists_bind)       (p/prim2 (prim-name 'hydra.lib.lists/hydra_lib_lists_bind)
                                        (fn [xs f] ((lists/hydra_lib_lists_bind xs) f))
                                        [] (p/tc-list a) (fun a (p/tc-list b)) (p/tc-list b))
     (prim-name 'hydra.lib.lists/hydra_lib_lists_concat)     (p/prim1 (prim-name 'hydra.lib.lists/hydra_lib_lists_concat)     lists/hydra_lib_lists_concat     [] (p/tc-list (p/tc-list a)) (p/tc-list a))
     (prim-name 'hydra.lib.lists/hydra_lib_lists_concat2)    (p/prim2 (prim-name 'hydra.lib.lists/hydra_lib_lists_concat2)
                                        (fn [xs ys] ((lists/hydra_lib_lists_concat2 xs) ys))
                                        [] (p/tc-list a) (p/tc-list a) (p/tc-list a))
     (prim-name 'hydra.lib.lists/hydra_lib_lists_cons)       (p/prim2 (prim-name 'hydra.lib.lists/hydra_lib_lists_cons)
                                        (fn [x xs] ((lists/hydra_lib_lists_cons x) xs))
                                        [] a (p/tc-list a) (p/tc-list a))
     (prim-name 'hydra.lib.lists/hydra_lib_lists_drop)       (p/prim2 (prim-name 'hydra.lib.lists/hydra_lib_lists_drop)
                                        (fn [n xs] ((lists/hydra_lib_lists_drop n) xs))
                                        [] (p/tc-int32) (p/tc-list a) (p/tc-list a))
     (prim-name 'hydra.lib.lists/hydra_lib_lists_drop_while)  (p/prim2 (prim-name 'hydra.lib.lists/hydra_lib_lists_drop_while)
                                        (fn [f xs] ((lists/hydra_lib_lists_drop_while f) xs))
                                        [] (fun a (p/tc-boolean)) (p/tc-list a) (p/tc-list a))
     (prim-name 'hydra.lib.lists/hydra_lib_lists_elem)       (p/prim2 (prim-name 'hydra.lib.lists/hydra_lib_lists_elem)
                                        (fn [x xs] ((lists/hydra_lib_lists_elem x) xs))
                                        [] a (p/tc-list a) (p/tc-boolean) {"a" ["equality"]})
     (prim-name 'hydra.lib.lists/hydra_lib_lists_filter)     (p/prim2 (prim-name 'hydra.lib.lists/hydra_lib_lists_filter)
                                        (fn [f xs] ((lists/hydra_lib_lists_filter f) xs))
                                        [] (fun a (p/tc-boolean)) (p/tc-list a) (p/tc-list a))
     (prim-name 'hydra.lib.lists/hydra_lib_lists_find)       (p/prim2 (prim-name 'hydra.lib.lists/hydra_lib_lists_find)
                                        (fn [f xs] ((lists/hydra_lib_lists_find f) xs))
                                        [] (fun a (p/tc-boolean)) (p/tc-list a) (p/tc-optional a))
     (prim-name 'hydra.lib.lists/hydra_lib_lists_foldl)      (p/prim3 (prim-name 'hydra.lib.lists/hydra_lib_lists_foldl)
                                        (fn [f init xs] (((lists/hydra_lib_lists_foldl (fn [acc] (fn [el] ((f acc) el)))) init) xs))
                                        [] (fun b (fun a b)) b (p/tc-list a) b)
     (prim-name 'hydra.lib.lists/hydra_lib_lists_foldr)      (p/prim3 (prim-name 'hydra.lib.lists/hydra_lib_lists_foldr)
                                        (fn [f init xs] (((lists/hydra_lib_lists_foldr (fn [el] (fn [acc] ((f el) acc)))) init) xs))
                                        [] (fun a (fun b b)) b (p/tc-list a) b)
     (prim-name 'hydra.lib.lists/hydra_lib_lists_group)      (p/prim1 (prim-name 'hydra.lib.lists/hydra_lib_lists_group)      lists/hydra_lib_lists_group      [] (p/tc-list a) (p/tc-list (p/tc-list a)) {"a" ["equality"]})
     (prim-name 'hydra.lib.lists/hydra_lib_lists_intercalate) (p/prim2 (prim-name 'hydra.lib.lists/hydra_lib_lists_intercalate)
                                         (fn [sep xss] ((lists/hydra_lib_lists_intercalate sep) xss))
                                         [] (p/tc-list a) (p/tc-list (p/tc-list a)) (p/tc-list a))
     (prim-name 'hydra.lib.lists/hydra_lib_lists_intersperse) (p/prim2 (prim-name 'hydra.lib.lists/hydra_lib_lists_intersperse)
                                         (fn [sep xs] ((lists/hydra_lib_lists_intersperse sep) xs))
                                         [] a (p/tc-list a) (p/tc-list a))
     (prim-name 'hydra.lib.lists/hydra_lib_lists_length)     (p/prim1 (prim-name 'hydra.lib.lists/hydra_lib_lists_length)     lists/hydra_lib_lists_length     [] (p/tc-list a) (p/tc-int32))
     (prim-name 'hydra.lib.lists/hydra_lib_lists_map)        (p/prim2 (prim-name 'hydra.lib.lists/hydra_lib_lists_map)
                                        (fn [f xs] ((lists/hydra_lib_lists_map f) xs))
                                        [] (fun a b) (p/tc-list a) (p/tc-list b))
     (prim-name 'hydra.lib.lists/hydra_lib_lists_maybe_at)    (p/prim2 (prim-name 'hydra.lib.lists/hydra_lib_lists_maybe_at)
                                        (fn [n xs] ((lists/hydra_lib_lists_maybe_at n) xs))
                                        [] (p/tc-int32) (p/tc-list a) (p/tc-optional a))
     (prim-name 'hydra.lib.lists/hydra_lib_lists_maybe_head)  (p/prim1 (prim-name 'hydra.lib.lists/hydra_lib_lists_maybe_head)  lists/hydra_lib_lists_maybe_head [] (p/tc-list a) (p/tc-optional a))
     (prim-name 'hydra.lib.lists/hydra_lib_lists_maybe_init)  (p/prim1 (prim-name 'hydra.lib.lists/hydra_lib_lists_maybe_init)  lists/hydra_lib_lists_maybe_init [] (p/tc-list a) (p/tc-optional (p/tc-list a)))
     (prim-name 'hydra.lib.lists/hydra_lib_lists_maybe_last)  (p/prim1 (prim-name 'hydra.lib.lists/hydra_lib_lists_maybe_last)  lists/hydra_lib_lists_maybe_last [] (p/tc-list a) (p/tc-optional a))
     (prim-name 'hydra.lib.lists/hydra_lib_lists_maybe_tail)  (p/prim1 (prim-name 'hydra.lib.lists/hydra_lib_lists_maybe_tail)  lists/hydra_lib_lists_maybe_tail [] (p/tc-list a) (p/tc-optional (p/tc-list a)))
     (prim-name 'hydra.lib.lists/hydra_lib_lists_nub)        (p/prim1 (prim-name 'hydra.lib.lists/hydra_lib_lists_nub)        lists/hydra_lib_lists_nub        [] (p/tc-list a) (p/tc-list a) {"a" ["equality"]})
     (prim-name 'hydra.lib.lists/hydra_lib_lists_null)       (p/prim1 (prim-name 'hydra.lib.lists/hydra_lib_lists_null)       lists/hydra_lib_lists_null       [] (p/tc-list a) (p/tc-boolean))
     (prim-name 'hydra.lib.lists/hydra_lib_lists_partition)   (p/prim2 (prim-name 'hydra.lib.lists/hydra_lib_lists_partition)
                                         (fn [f xs] ((lists/hydra_lib_lists_partition f) xs))
                                         [] (fun a (p/tc-boolean)) (p/tc-list a) (p/tc-pair (p/tc-list a) (p/tc-list a)))
     (prim-name 'hydra.lib.lists/hydra_lib_lists_pure)       (p/prim1 (prim-name 'hydra.lib.lists/hydra_lib_lists_pure)       lists/hydra_lib_lists_pure       [] a (p/tc-list a))
     (prim-name 'hydra.lib.lists/hydra_lib_lists_replicate)  (p/prim2 (prim-name 'hydra.lib.lists/hydra_lib_lists_replicate)
                                        (fn [n x] ((lists/hydra_lib_lists_replicate n) x))
                                        [] (p/tc-int32) a (p/tc-list a))
     (prim-name 'hydra.lib.lists/hydra_lib_lists_reverse)    (p/prim1 (prim-name 'hydra.lib.lists/hydra_lib_lists_reverse)    lists/hydra_lib_lists_reverse    [] (p/tc-list a) (p/tc-list a))
     (prim-name 'hydra.lib.lists/hydra_lib_lists_singleton)  (p/prim1 (prim-name 'hydra.lib.lists/hydra_lib_lists_singleton)  lists/hydra_lib_lists_singleton  [] a (p/tc-list a))
     (prim-name 'hydra.lib.lists/hydra_lib_lists_sort)       (p/prim1 (prim-name 'hydra.lib.lists/hydra_lib_lists_sort)       lists/hydra_lib_lists_sort       [] (p/tc-list a) (p/tc-list a) {"a" ["ordering"]})
     (prim-name 'hydra.lib.lists/hydra_lib_lists_sort_on)     (p/prim2 (prim-name 'hydra.lib.lists/hydra_lib_lists_sort_on)
                                        (fn [f xs] ((lists/hydra_lib_lists_sort_on f) xs))
                                        [] (fun a b) (p/tc-list a) (p/tc-list a))
     (prim-name 'hydra.lib.lists/hydra_lib_lists_span)       (p/prim2 (prim-name 'hydra.lib.lists/hydra_lib_lists_span)
                                        (fn [f xs] ((lists/hydra_lib_lists_span f) xs))
                                        [] (fun a (p/tc-boolean)) (p/tc-list a) (p/tc-pair (p/tc-list a) (p/tc-list a)))
     (prim-name 'hydra.lib.lists/hydra_lib_lists_take)       (p/prim2 (prim-name 'hydra.lib.lists/hydra_lib_lists_take)
                                        (fn [n xs] ((lists/hydra_lib_lists_take n) xs))
                                        [] (p/tc-int32) (p/tc-list a) (p/tc-list a))
     (prim-name 'hydra.lib.lists/hydra_lib_lists_transpose)  (p/prim1 (prim-name 'hydra.lib.lists/hydra_lib_lists_transpose)  lists/hydra_lib_lists_transpose  [] (p/tc-list (p/tc-list a)) (p/tc-list (p/tc-list a)))
     (prim-name 'hydra.lib.lists/hydra_lib_lists_uncons)     (p/prim1 (prim-name 'hydra.lib.lists/hydra_lib_lists_uncons)     lists/hydra_lib_lists_uncons     [] (p/tc-list a) (p/tc-optional (p/tc-pair a (p/tc-list a))))
     (prim-name 'hydra.lib.lists/hydra_lib_lists_zip)        (p/prim2 (prim-name 'hydra.lib.lists/hydra_lib_lists_zip)
                                        (fn [xs ys] ((lists/hydra_lib_lists_zip xs) ys))
                                        [] (p/tc-list a) (p/tc-list b) (p/tc-list (p/tc-pair a b)))
     (prim-name 'hydra.lib.lists/hydra_lib_lists_zip_with)    (p/prim3 (prim-name 'hydra.lib.lists/hydra_lib_lists_zip_with)
                                        (fn [f xs ys] (((lists/hydra_lib_lists_zip_with (fn [a] (fn [b] ((f a) b)))) xs) ys))
                                        [] (fun a (fun b c)) (p/tc-list a) (p/tc-list b) (p/tc-list c))}))

;; ============================================================
;; Logic
;; ============================================================

(defn register-logic []
  (let [
        a (p/tc-variable "a")]
    {(prim-name 'hydra.lib.logic/hydra_lib_logic_and)    (p/prim2 (prim-name 'hydra.lib.logic/hydra_lib_logic_and)
                                    (fn [a b] ((logic/hydra_lib_logic_and a) b))
                                    [] (p/tc-boolean) (p/tc-boolean) (p/tc-boolean))
     (prim-name 'hydra.lib.logic/hydra_lib_logic_if_else) (p/lazy-args [1 2] (p/prim3 (prim-name 'hydra.lib.logic/hydra_lib_logic_if_else)
                                    (fn [cond t f] (((logic/hydra_lib_logic_if_else cond) t) f))
                                    [] (p/tc-boolean) a a a))
     (prim-name 'hydra.lib.logic/hydra_lib_logic_not)    (p/prim1 (prim-name 'hydra.lib.logic/hydra_lib_logic_not)    logic/hydra_lib_logic_not [] (p/tc-boolean) (p/tc-boolean))
     (prim-name 'hydra.lib.logic/hydra_lib_logic_or)     (p/prim2 (prim-name 'hydra.lib.logic/hydra_lib_logic_or)
                                    (fn [a b] ((logic/hydra_lib_logic_or a) b))
                                    [] (p/tc-boolean) (p/tc-boolean) (p/tc-boolean))}))

;; ============================================================
;; Maps
;; ============================================================

(defn register-maps []
  (let [
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
    {(prim-name 'hydra.lib.maps/hydra_lib_maps_alter)          (p/prim3 (prim-name 'hydra.lib.maps/hydra_lib_maps_alter)
                                            (fn [f key m] (((maps/hydra_lib_maps_alter f) key) m))
                                            [] (fun (p/tc-optional v) (p/tc-optional v)) k map-kv map-kv ord-k)
     (prim-name 'hydra.lib.maps/hydra_lib_maps_bimap)          (p/prim3 (prim-name 'hydra.lib.maps/hydra_lib_maps_bimap)
                                            (fn [fk fv m] (((maps/hydra_lib_maps_bimap fk) fv) m))
                                            [] (fun k1 k2) (fun v1 v2) (p/tc-map k1 v1) (p/tc-map k2 v2) ord-k1k2)
     (prim-name 'hydra.lib.maps/hydra_lib_maps_delete)         (p/prim2 (prim-name 'hydra.lib.maps/hydra_lib_maps_delete)
                                            (fn [key m] ((maps/hydra_lib_maps_delete key) m))
                                            [] k map-kv map-kv ord-k)
     (prim-name 'hydra.lib.maps/hydra_lib_maps_elems)          (p/prim1 (prim-name 'hydra.lib.maps/hydra_lib_maps_elems)  maps/hydra_lib_maps_elems  [] map-kv (p/tc-list v) ord-k)
     (prim-name 'hydra.lib.maps/hydra_lib_maps_empty)          (p/prim0 (prim-name 'hydra.lib.maps/hydra_lib_maps_empty)  (fn [] maps/hydra_lib_maps_empty)  [] map-kv ord-k)
     (prim-name 'hydra.lib.maps/hydra_lib_maps_filter)         (p/prim2 (prim-name 'hydra.lib.maps/hydra_lib_maps_filter)
                                            (fn [f m] ((maps/hydra_lib_maps_filter f) m))
                                            [] (fun v (p/tc-boolean)) map-kv map-kv ord-k)
     (prim-name 'hydra.lib.maps/hydra_lib_maps_filter_with_key)  (p/prim2 (prim-name 'hydra.lib.maps/hydra_lib_maps_filter_with_key)
                                            (fn [f m] ((maps/hydra_lib_maps_filter_with_key f) m))
                                            [] (fun k (fun v (p/tc-boolean))) map-kv map-kv ord-k)
     (prim-name 'hydra.lib.maps/hydra_lib_maps_find_with_default) (p/lazy-args [0] (p/prim3 (prim-name 'hydra.lib.maps/hydra_lib_maps_find_with_default)
                                             (fn [dflt key m] (((maps/hydra_lib_maps_find_with_default dflt) key) m))
                                             [] v k map-kv v ord-k))
     (prim-name 'hydra.lib.maps/hydra_lib_maps_from_list)       (p/prim1 (prim-name 'hydra.lib.maps/hydra_lib_maps_from_list) maps/hydra_lib_maps_from_list [] (p/tc-list (p/tc-pair k v)) map-kv ord-k)
     (prim-name 'hydra.lib.maps/hydra_lib_maps_insert)         (p/prim3 (prim-name 'hydra.lib.maps/hydra_lib_maps_insert)
                                            (fn [key val m] (((maps/hydra_lib_maps_insert key) val) m))
                                            [] k v map-kv map-kv ord-k)
     (prim-name 'hydra.lib.maps/hydra_lib_maps_keys)           (p/prim1 (prim-name 'hydra.lib.maps/hydra_lib_maps_keys)   maps/hydra_lib_maps_keys   [] map-kv (p/tc-list k) ord-k)
     (prim-name 'hydra.lib.maps/hydra_lib_maps_lookup)         (p/prim2 (prim-name 'hydra.lib.maps/hydra_lib_maps_lookup)
                                            (fn [key m] ((maps/hydra_lib_maps_lookup key) m))
                                            [] k map-kv (p/tc-optional v) ord-k)
     (prim-name 'hydra.lib.maps/hydra_lib_maps_map)            (p/prim2 (prim-name 'hydra.lib.maps/hydra_lib_maps_map)
                                            (fn [f m] ((maps/hydra_lib_maps_map f) m))
                                            [] (fun v1 v2) (p/tc-map k v1) (p/tc-map k v2) ord-k)
     (prim-name 'hydra.lib.maps/hydra_lib_maps_map_keys)        (p/prim2 (prim-name 'hydra.lib.maps/hydra_lib_maps_map_keys)
                                            (fn [f m] ((maps/hydra_lib_maps_map_keys f) m))
                                            [] (fun k1 k2) (p/tc-map k1 v) (p/tc-map k2 v) ord-k1k2)
     (prim-name 'hydra.lib.maps/hydra_lib_maps_member)         (p/prim2 (prim-name 'hydra.lib.maps/hydra_lib_maps_member)
                                            (fn [key m] ((maps/hydra_lib_maps_member key) m))
                                            [] k map-kv (p/tc-boolean) ord-k)
     (prim-name 'hydra.lib.maps/hydra_lib_maps_null)           (p/prim1 (prim-name 'hydra.lib.maps/hydra_lib_maps_null)   maps/hydra_lib_maps_null   [] map-kv (p/tc-boolean) ord-k)
     (prim-name 'hydra.lib.maps/hydra_lib_maps_singleton)      (p/prim2 (prim-name 'hydra.lib.maps/hydra_lib_maps_singleton)
                                            (fn [key val] ((maps/hydra_lib_maps_singleton key) val))
                                            [] k v map-kv ord-k)
     (prim-name 'hydra.lib.maps/hydra_lib_maps_size)           (p/prim1 (prim-name 'hydra.lib.maps/hydra_lib_maps_size)   maps/hydra_lib_maps_size   [] map-kv (p/tc-int32) ord-k)
     (prim-name 'hydra.lib.maps/hydra_lib_maps_to_list)         (p/prim1 (prim-name 'hydra.lib.maps/hydra_lib_maps_to_list) maps/hydra_lib_maps_to_list [] map-kv (p/tc-list (p/tc-pair k v)) ord-k)
     (prim-name 'hydra.lib.maps/hydra_lib_maps_union)          (p/prim2 (prim-name 'hydra.lib.maps/hydra_lib_maps_union)
                                            (fn [m1 m2] ((maps/hydra_lib_maps_union m1) m2))
                                            [] map-kv map-kv map-kv ord-k)}))

;; ============================================================
;; Math
;; ============================================================

(defn register-math []
  (let [
        i32 (p/tc-int32)
        f32 (p/tc-float32)
        f64 (p/tc-float64)
        bi  (p/tc-bigint)
        b   (p/tc-boolean)]
    (merge
     ;; Int32 primitives
     {(prim-name 'hydra.lib.math/hydra_lib_math_abs)    (p/prim1 (prim-name 'hydra.lib.math/hydra_lib_math_abs)    math/hydra_lib_math_abs    [] i32 i32)
      (prim-name 'hydra.lib.math/hydra_lib_math_add)    (p/prim2 (prim-name 'hydra.lib.math/hydra_lib_math_add)    (fn [a b] ((math/hydra_lib_math_add a) b))    [] i32 i32 i32)
      (prim-name 'hydra.lib.math/hydra_lib_math_even)   (p/prim1 (prim-name 'hydra.lib.math/hydra_lib_math_even)   math/hydra_lib_math_even   [] i32 b)
      (prim-name 'hydra.lib.math/hydra_lib_math_mul)    (p/prim2 (prim-name 'hydra.lib.math/hydra_lib_math_mul)    (fn [a b] ((math/hydra_lib_math_mul a) b))    [] i32 i32 i32)
      (prim-name 'hydra.lib.math/hydra_lib_math_negate) (p/prim1 (prim-name 'hydra.lib.math/hydra_lib_math_negate) math/hydra_lib_math_negate [] i32 i32)
      (prim-name 'hydra.lib.math/hydra_lib_math_odd)    (p/prim1 (prim-name 'hydra.lib.math/hydra_lib_math_odd)    math/hydra_lib_math_odd    [] i32 b)
      (prim-name 'hydra.lib.math/hydra_lib_math_range)  (p/prim2 (prim-name 'hydra.lib.math/hydra_lib_math_range)  (fn [a b] ((math/hydra_lib_math_range a) b))  [] i32 i32 (p/tc-list i32))
      (prim-name 'hydra.lib.math/hydra_lib_math_signum) (p/prim1 (prim-name 'hydra.lib.math/hydra_lib_math_signum) math/hydra_lib_math_signum [] i32 i32)
      (prim-name 'hydra.lib.math/hydra_lib_math_sub)    (p/prim2 (prim-name 'hydra.lib.math/hydra_lib_math_sub)    (fn [a b] ((math/hydra_lib_math_sub a) b))    [] i32 i32 i32)
      (prim-name 'hydra.lib.math/hydra_lib_math_max)    (p/prim2 (prim-name 'hydra.lib.math/hydra_lib_math_max)    (fn [a b] ((math/hydra_lib_math_max a) b))    [] i32 i32 i32)
      (prim-name 'hydra.lib.math/hydra_lib_math_maybe_div)  (p/prim2 (prim-name 'hydra.lib.math/hydra_lib_math_maybe_div)  (fn [a b] ((math/hydra_lib_math_maybe_div a) b))  [] i32 i32 (p/tc-optional i32))
      (prim-name 'hydra.lib.math/hydra_lib_math_maybe_mod)  (p/prim2 (prim-name 'hydra.lib.math/hydra_lib_math_maybe_mod)  (fn [a b] ((math/hydra_lib_math_maybe_mod a) b))  [] i32 i32 (p/tc-optional i32))
      (prim-name 'hydra.lib.math/hydra_lib_math_maybe_pred) (p/prim1 (prim-name 'hydra.lib.math/hydra_lib_math_maybe_pred) math/hydra_lib_math_maybe_pred [] i32 (p/tc-optional i32))
      (prim-name 'hydra.lib.math/hydra_lib_math_maybe_rem)  (p/prim2 (prim-name 'hydra.lib.math/hydra_lib_math_maybe_rem)  (fn [a b] ((math/hydra_lib_math_maybe_rem a) b))  [] i32 i32 (p/tc-optional i32))
      (prim-name 'hydra.lib.math/hydra_lib_math_maybe_succ) (p/prim1 (prim-name 'hydra.lib.math/hydra_lib_math_maybe_succ) math/hydra_lib_math_maybe_succ [] i32 (p/tc-optional i32))
      (prim-name 'hydra.lib.math/hydra_lib_math_min)    (p/prim2 (prim-name 'hydra.lib.math/hydra_lib_math_min)    (fn [a b] ((math/hydra_lib_math_min a) b))    [] i32 i32 i32)}
     ;; Float64 primitives
     {(prim-name 'hydra.lib.math/hydra_lib_math_acos)     (p/prim1 (prim-name 'hydra.lib.math/hydra_lib_math_acos)     math/hydra_lib_math_acos     [] f64 f64)
      (prim-name 'hydra.lib.math/hydra_lib_math_acosh)    (p/prim1 (prim-name 'hydra.lib.math/hydra_lib_math_acosh)    math/hydra_lib_math_acosh    [] f64 f64)
      (prim-name 'hydra.lib.math/hydra_lib_math_add_float64) (p/prim2 (prim-name 'hydra.lib.math/hydra_lib_math_add_float64) (fn [a b] ((math/hydra_lib_math_add_float64 a) b)) [] f64 f64 f64)
      (prim-name 'hydra.lib.math/hydra_lib_math_asin)     (p/prim1 (prim-name 'hydra.lib.math/hydra_lib_math_asin)     math/hydra_lib_math_asin     [] f64 f64)
      (prim-name 'hydra.lib.math/hydra_lib_math_asinh)    (p/prim1 (prim-name 'hydra.lib.math/hydra_lib_math_asinh)    math/hydra_lib_math_asinh    [] f64 f64)
      (prim-name 'hydra.lib.math/hydra_lib_math_atan)     (p/prim1 (prim-name 'hydra.lib.math/hydra_lib_math_atan)     math/hydra_lib_math_atan     [] f64 f64)
      (prim-name 'hydra.lib.math/hydra_lib_math_atan2)    (p/prim2 (prim-name 'hydra.lib.math/hydra_lib_math_atan2)    (fn [a b] ((math/hydra_lib_math_atan2 a) b)) [] f64 f64 f64)
      (prim-name 'hydra.lib.math/hydra_lib_math_atanh)    (p/prim1 (prim-name 'hydra.lib.math/hydra_lib_math_atanh)    math/hydra_lib_math_atanh    [] f64 f64)
      (prim-name 'hydra.lib.math/hydra_lib_math_ceiling)  (p/prim1 (prim-name 'hydra.lib.math/hydra_lib_math_ceiling)  math/hydra_lib_math_ceiling  [] f64 f64)
      (prim-name 'hydra.lib.math/hydra_lib_math_cos)      (p/prim1 (prim-name 'hydra.lib.math/hydra_lib_math_cos)      math/hydra_lib_math_cos      [] f64 f64)
      (prim-name 'hydra.lib.math/hydra_lib_math_cosh)     (p/prim1 (prim-name 'hydra.lib.math/hydra_lib_math_cosh)     math/hydra_lib_math_cosh     [] f64 f64)
      (prim-name 'hydra.lib.math/hydra_lib_math_e)        (p/prim0 (prim-name 'hydra.lib.math/hydra_lib_math_e)        (fn [] math/hydra_lib_math_e)        [] f64)
      (prim-name 'hydra.lib.math/hydra_lib_math_exp)      (p/prim1 (prim-name 'hydra.lib.math/hydra_lib_math_exp)      math/hydra_lib_math_exp      [] f64 f64)
      (prim-name 'hydra.lib.math/hydra_lib_math_floor)    (p/prim1 (prim-name 'hydra.lib.math/hydra_lib_math_floor)    math/hydra_lib_math_floor    [] f64 f64)
      (prim-name 'hydra.lib.math/hydra_lib_math_log)      (p/prim1 (prim-name 'hydra.lib.math/hydra_lib_math_log)      math/hydra_lib_math_log      [] f64 f64)
      (prim-name 'hydra.lib.math/hydra_lib_math_log_base)  (p/prim2 (prim-name 'hydra.lib.math/hydra_lib_math_log_base)  (fn [a b] ((math/hydra_lib_math_log_base a) b)) [] f64 f64 f64)
      (prim-name 'hydra.lib.math/hydra_lib_math_mul_float64) (p/prim2 (prim-name 'hydra.lib.math/hydra_lib_math_mul_float64) (fn [a b] ((math/hydra_lib_math_mul_float64 a) b)) [] f64 f64 f64)
      (prim-name 'hydra.lib.math/hydra_lib_math_negate_float64) (p/prim1 (prim-name 'hydra.lib.math/hydra_lib_math_negate_float64) math/hydra_lib_math_negate_float64 [] f64 f64)
      (prim-name 'hydra.lib.math/hydra_lib_math_pi)       (p/prim0 (prim-name 'hydra.lib.math/hydra_lib_math_pi)       (fn [] math/hydra_lib_math_pi)       [] f64)
      (prim-name 'hydra.lib.math/hydra_lib_math_pow)      (p/prim2 (prim-name 'hydra.lib.math/hydra_lib_math_pow)      (fn [a b] ((math/hydra_lib_math_pow a) b)) [] f64 f64 f64)
      (prim-name 'hydra.lib.math/hydra_lib_math_round)    (p/prim1 (prim-name 'hydra.lib.math/hydra_lib_math_round)    math/hydra_lib_math_round    [] f64 f64)
      (prim-name 'hydra.lib.math/hydra_lib_math_round_float32)  (p/prim2 (prim-name 'hydra.lib.math/hydra_lib_math_round_float32)
                                            (fn [n x] ((math/hydra_lib_math_round_float32 n) x))
                                            [] i32 f32 f32)
      (prim-name 'hydra.lib.math/hydra_lib_math_round_float64)  (p/prim2 (prim-name 'hydra.lib.math/hydra_lib_math_round_float64)
                                            (fn [n x] ((math/hydra_lib_math_round_float64 n) x))
                                            [] i32 f64 f64)
      (prim-name 'hydra.lib.math/hydra_lib_math_sin)      (p/prim1 (prim-name 'hydra.lib.math/hydra_lib_math_sin)      math/hydra_lib_math_sin      [] f64 f64)
      (prim-name 'hydra.lib.math/hydra_lib_math_sinh)     (p/prim1 (prim-name 'hydra.lib.math/hydra_lib_math_sinh)     math/hydra_lib_math_sinh     [] f64 f64)
      (prim-name 'hydra.lib.math/hydra_lib_math_sqrt)     (p/prim1 (prim-name 'hydra.lib.math/hydra_lib_math_sqrt)     math/hydra_lib_math_sqrt     [] f64 f64)
      (prim-name 'hydra.lib.math/hydra_lib_math_sub_float64) (p/prim2 (prim-name 'hydra.lib.math/hydra_lib_math_sub_float64) (fn [a b] ((math/hydra_lib_math_sub_float64 a) b)) [] f64 f64 f64)
      (prim-name 'hydra.lib.math/hydra_lib_math_tan)      (p/prim1 (prim-name 'hydra.lib.math/hydra_lib_math_tan)      math/hydra_lib_math_tan      [] f64 f64)
      (prim-name 'hydra.lib.math/hydra_lib_math_tanh)     (p/prim1 (prim-name 'hydra.lib.math/hydra_lib_math_tanh)     math/hydra_lib_math_tanh     [] f64 f64)
      (prim-name 'hydra.lib.math/hydra_lib_math_truncate) (p/prim1 (prim-name 'hydra.lib.math/hydra_lib_math_truncate) math/hydra_lib_math_truncate [] f64 f64)})))

;; ============================================================
;; Maybes
;; ============================================================

(defn register-optionals []
  (let [
        a (p/tc-variable "a")
        b (p/tc-variable "b")
        c (p/tc-variable "c")]
    {(prim-name 'hydra.lib.optionals/hydra_lib_optionals_apply)    (p/prim2 (prim-name 'hydra.lib.optionals/hydra_lib_optionals_apply)
                                      (fn [mf mx] ((optionals/hydra_lib_optionals_apply mf) mx))
                                      [] (p/tc-optional (fun a b)) (p/tc-optional a) (p/tc-optional b))
     (prim-name 'hydra.lib.optionals/hydra_lib_optionals_bind)     (p/prim2 (prim-name 'hydra.lib.optionals/hydra_lib_optionals_bind)
                                      (fn [mx f] ((optionals/hydra_lib_optionals_bind mx) f))
                                      [] (p/tc-optional a) (fun a (p/tc-optional b)) (p/tc-optional b))
     (prim-name 'hydra.lib.optionals/hydra_lib_optionals_cases)    (p/lazy-args [1] (p/prim3 (prim-name 'hydra.lib.optionals/hydra_lib_optionals_cases)
                                      (fn [mx dflt f] (((optionals/hydra_lib_optionals_cases mx) dflt) f))
                                      [] (p/tc-optional a) b (fun a b) b))
     (prim-name 'hydra.lib.optionals/hydra_lib_optionals_cat)      (p/prim1 (prim-name 'hydra.lib.optionals/hydra_lib_optionals_cat)      optionals/hydra_lib_optionals_cat      [] (p/tc-list (p/tc-optional a)) (p/tc-list a))
     (prim-name 'hydra.lib.optionals/hydra_lib_optionals_compose)  (p/prim3 (prim-name 'hydra.lib.optionals/hydra_lib_optionals_compose)
                                      (fn [f g x] (((optionals/hydra_lib_optionals_compose f) g) x))
                                      [] (fun a (p/tc-optional b)) (fun b (p/tc-optional c)) a (p/tc-optional c))
     (prim-name 'hydra.lib.optionals/hydra_lib_optionals_from_optional) (p/lazy-args [0] (p/prim2 (prim-name 'hydra.lib.optionals/hydra_lib_optionals_from_optional)
                                       (fn [dflt mx] ((optionals/hydra_lib_optionals_from_optional dflt) mx))
                                       [] a (p/tc-optional a) a))
     (prim-name 'hydra.lib.optionals/hydra_lib_optionals_is_given)    (p/prim1 (prim-name 'hydra.lib.optionals/hydra_lib_optionals_is_given)    optionals/hydra_lib_optionals_is_given    [] (p/tc-optional a) (p/tc-boolean))
     (prim-name 'hydra.lib.optionals/hydra_lib_optionals_is_none) (p/prim1 (prim-name 'hydra.lib.optionals/hydra_lib_optionals_is_none) optionals/hydra_lib_optionals_is_none [] (p/tc-optional a) (p/tc-boolean))
     (prim-name 'hydra.lib.optionals/hydra_lib_optionals_map)       (p/prim2 (prim-name 'hydra.lib.optionals/hydra_lib_optionals_map)
                                       (fn [f mx] ((optionals/hydra_lib_optionals_map f) mx))
                                       [] (fun a b) (p/tc-optional a) (p/tc-optional b))
     (prim-name 'hydra.lib.optionals/hydra_lib_optionals_map_optional)  (p/prim2 (prim-name 'hydra.lib.optionals/hydra_lib_optionals_map_optional)
                                       (fn [f xs] ((optionals/hydra_lib_optionals_map_optional f) xs))
                                       [] (fun a (p/tc-optional b)) (p/tc-list a) (p/tc-list b))
     (prim-name 'hydra.lib.optionals/hydra_lib_optionals_pure)      (p/prim1 (prim-name 'hydra.lib.optionals/hydra_lib_optionals_pure)      optionals/hydra_lib_optionals_pure      [] a (p/tc-optional a))
     (prim-name 'hydra.lib.optionals/hydra_lib_optionals_to_list)    (p/prim1 (prim-name 'hydra.lib.optionals/hydra_lib_optionals_to_list)    optionals/hydra_lib_optionals_to_list   [] (p/tc-optional a) (p/tc-list a))}))

;; ============================================================
;; Pairs
;; ============================================================

(defn register-pairs []
  (let [
        a (p/tc-variable "a")
        b (p/tc-variable "b")
        c (p/tc-variable "c")
        d (p/tc-variable "d")]
    {;; BUG #438: explicit ["a" "b" "c" "d"] order (kernel's
     ;; Pairs.bimapSig declares it that way, but auto-detect from body
     ;; (a -> c) -> (b -> d) -> ... yields [a, c, b, d] which swaps
     ;; arguments in inferred Function<A,B> casts at call sites.
     (prim-name 'hydra.lib.pairs/hydra_lib_pairs_bimap)  (p/prim3 (prim-name 'hydra.lib.pairs/hydra_lib_pairs_bimap)
                                    (fn [f g p] (((pairs/hydra_lib_pairs_bimap f) g) p))
                                    ["a" "b" "c" "d"] (fun a c) (fun b d) (p/tc-pair a b) (p/tc-pair c d))
     (prim-name 'hydra.lib.pairs/hydra_lib_pairs_first)  (p/prim1 (prim-name 'hydra.lib.pairs/hydra_lib_pairs_first)  pairs/hydra_lib_pairs_first  [] (p/tc-pair a b) a)
     (prim-name 'hydra.lib.pairs/hydra_lib_pairs_second) (p/prim1 (prim-name 'hydra.lib.pairs/hydra_lib_pairs_second) pairs/hydra_lib_pairs_second [] (p/tc-pair a b) b)}))

;; ============================================================
;; Sets
;; ============================================================

(defn register-sets []
  (let [
        a (p/tc-variable "a")
        b (p/tc-variable "b")
        ;; All set primitives have ordering constraint on element type
        ord-a {"a" ["ordering"]}
        ord-ab {"a" ["ordering"] "b" ["ordering"]}]
    {(prim-name 'hydra.lib.sets/hydra_lib_sets_delete)       (p/prim2 (prim-name 'hydra.lib.sets/hydra_lib_sets_delete)
                                          (fn [x s] ((sets/hydra_lib_sets_delete x) s))
                                          [] a (p/tc-set a) (p/tc-set a) ord-a)
     (prim-name 'hydra.lib.sets/hydra_lib_sets_difference)   (p/prim2 (prim-name 'hydra.lib.sets/hydra_lib_sets_difference)
                                          (fn [s1 s2] ((sets/hydra_lib_sets_difference s1) s2))
                                          [] (p/tc-set a) (p/tc-set a) (p/tc-set a) ord-a)
     (prim-name 'hydra.lib.sets/hydra_lib_sets_empty)        (p/prim0 (prim-name 'hydra.lib.sets/hydra_lib_sets_empty)   (fn [] sets/hydra_lib_sets_empty)   [] (p/tc-set a) ord-a)
     (prim-name 'hydra.lib.sets/hydra_lib_sets_from_list)     (p/prim1 (prim-name 'hydra.lib.sets/hydra_lib_sets_from_list) sets/hydra_lib_sets_from_list [] (p/tc-list a) (p/tc-set a) ord-a)
     (prim-name 'hydra.lib.sets/hydra_lib_sets_insert)       (p/prim2 (prim-name 'hydra.lib.sets/hydra_lib_sets_insert)
                                          (fn [x s] ((sets/hydra_lib_sets_insert x) s))
                                          [] a (p/tc-set a) (p/tc-set a) ord-a)
     (prim-name 'hydra.lib.sets/hydra_lib_sets_intersection) (p/prim2 (prim-name 'hydra.lib.sets/hydra_lib_sets_intersection)
                                          (fn [s1 s2] ((sets/hydra_lib_sets_intersection s1) s2))
                                          [] (p/tc-set a) (p/tc-set a) (p/tc-set a) ord-a)
     (prim-name 'hydra.lib.sets/hydra_lib_sets_map)          (p/prim2 (prim-name 'hydra.lib.sets/hydra_lib_sets_map)
                                          (fn [f s] ((sets/hydra_lib_sets_map f) s))
                                          [] (fun a b) (p/tc-set a) (p/tc-set b) ord-ab)
     (prim-name 'hydra.lib.sets/hydra_lib_sets_member)       (p/prim2 (prim-name 'hydra.lib.sets/hydra_lib_sets_member)
                                          (fn [x s] ((sets/hydra_lib_sets_member x) s))
                                          [] a (p/tc-set a) (p/tc-boolean) ord-a)
     (prim-name 'hydra.lib.sets/hydra_lib_sets_null)         (p/prim1 (prim-name 'hydra.lib.sets/hydra_lib_sets_null)     sets/hydra_lib_sets_null     [] (p/tc-set a) (p/tc-boolean) ord-a)
     (prim-name 'hydra.lib.sets/hydra_lib_sets_singleton)    (p/prim1 (prim-name 'hydra.lib.sets/hydra_lib_sets_singleton) sets/hydra_lib_sets_singleton [] a (p/tc-set a) ord-a)
     (prim-name 'hydra.lib.sets/hydra_lib_sets_size)         (p/prim1 (prim-name 'hydra.lib.sets/hydra_lib_sets_size)     sets/hydra_lib_sets_size     [] (p/tc-set a) (p/tc-int32) ord-a)
     (prim-name 'hydra.lib.sets/hydra_lib_sets_to_list)       (p/prim1 (prim-name 'hydra.lib.sets/hydra_lib_sets_to_list)   sets/hydra_lib_sets_to_list  [] (p/tc-set a) (p/tc-list a) ord-a)
     (prim-name 'hydra.lib.sets/hydra_lib_sets_union)        (p/prim2 (prim-name 'hydra.lib.sets/hydra_lib_sets_union)
                                          (fn [s1 s2] ((sets/hydra_lib_sets_union s1) s2))
                                          [] (p/tc-set a) (p/tc-set a) (p/tc-set a) ord-a)
     (prim-name 'hydra.lib.sets/hydra_lib_sets_unions)       (p/prim1 (prim-name 'hydra.lib.sets/hydra_lib_sets_unions)   sets/hydra_lib_sets_unions   [] (p/tc-list (p/tc-set a)) (p/tc-set a) ord-a)}))

;; ============================================================
;; Regex
;; ============================================================

(defn register-regex []
  (let [
        s (p/tc-string)
        b (p/tc-boolean)]
    {(prim-name 'hydra.lib.regex/hydra_lib_regex_find)       (p/prim2 (prim-name 'hydra.lib.regex/hydra_lib_regex_find)
                                        (fn [pat input] ((regex/hydra_lib_regex_find pat) input))
                                        [] s s (p/tc-optional s))
     (prim-name 'hydra.lib.regex/hydra_lib_regex_find_all)    (p/prim2 (prim-name 'hydra.lib.regex/hydra_lib_regex_find_all)
                                        (fn [pat input] ((regex/hydra_lib_regex_find_all pat) input))
                                        [] s s (p/tc-list s))
     (prim-name 'hydra.lib.regex/hydra_lib_regex_matches)    (p/prim2 (prim-name 'hydra.lib.regex/hydra_lib_regex_matches)
                                        (fn [pat input] ((regex/hydra_lib_regex_matches pat) input))
                                        [] s s b)
     (prim-name 'hydra.lib.regex/hydra_lib_regex_replace)    (p/prim3 (prim-name 'hydra.lib.regex/hydra_lib_regex_replace)
                                        (fn [pat repl input] (((regex/hydra_lib_regex_replace pat) repl) input))
                                        [] s s s s)
     (prim-name 'hydra.lib.regex/hydra_lib_regex_replace_all) (p/prim3 (prim-name 'hydra.lib.regex/hydra_lib_regex_replace_all)
                                        (fn [pat repl input] (((regex/hydra_lib_regex_replace_all pat) repl) input))
                                        [] s s s s)
     (prim-name 'hydra.lib.regex/hydra_lib_regex_split)      (p/prim2 (prim-name 'hydra.lib.regex/hydra_lib_regex_split)
                                        (fn [pat input] ((regex/hydra_lib_regex_split pat) input))
                                        [] s s (p/tc-list s))}))

;; ============================================================
;; Strings
;; ============================================================

(defn register-strings []
  (let [
        s (p/tc-string)
        i (p/tc-int32)
        b (p/tc-boolean)]
    {(prim-name 'hydra.lib.strings/hydra_lib_strings_cat)         (p/prim1 (prim-name 'hydra.lib.strings/hydra_lib_strings_cat)         strings/hydra_lib_strings_cat         [] (p/tc-list s) s)
     (prim-name 'hydra.lib.strings/hydra_lib_strings_cat2)        (p/prim2 (prim-name 'hydra.lib.strings/hydra_lib_strings_cat2)
                                         (fn [a b] ((strings/hydra_lib_strings_cat2 a) b))
                                         [] s s s)
     (prim-name 'hydra.lib.strings/hydra_lib_strings_from_list)    (p/prim1 (prim-name 'hydra.lib.strings/hydra_lib_strings_from_list)    strings/hydra_lib_strings_from_list    [] (p/tc-list i) s)
     (prim-name 'hydra.lib.strings/hydra_lib_strings_intercalate) (p/prim2 (prim-name 'hydra.lib.strings/hydra_lib_strings_intercalate)
                                         (fn [sep ss] ((strings/hydra_lib_strings_intercalate sep) ss))
                                         [] s (p/tc-list s) s)
     (prim-name 'hydra.lib.strings/hydra_lib_strings_length)      (p/prim1 (prim-name 'hydra.lib.strings/hydra_lib_strings_length)      strings/hydra_lib_strings_length      [] s i)
     (prim-name 'hydra.lib.strings/hydra_lib_strings_lines)       (p/prim1 (prim-name 'hydra.lib.strings/hydra_lib_strings_lines)       strings/hydra_lib_strings_lines       [] s (p/tc-list s))
     (prim-name 'hydra.lib.strings/hydra_lib_strings_maybe_char_at) (p/prim2 (prim-name 'hydra.lib.strings/hydra_lib_strings_maybe_char_at)
                                         (fn [n str] ((strings/hydra_lib_strings_maybe_char_at n) str))
                                         [] i s (p/tc-optional i))
     (prim-name 'hydra.lib.strings/hydra_lib_strings_null)        (p/prim1 (prim-name 'hydra.lib.strings/hydra_lib_strings_null)        strings/hydra_lib_strings_null        [] s b)
     (prim-name 'hydra.lib.strings/hydra_lib_strings_split_on)     (p/prim2 (prim-name 'hydra.lib.strings/hydra_lib_strings_split_on)
                                         (fn [sep str] ((strings/hydra_lib_strings_split_on sep) str))
                                         [] s s (p/tc-list s))
     (prim-name 'hydra.lib.strings/hydra_lib_strings_to_list)      (p/prim1 (prim-name 'hydra.lib.strings/hydra_lib_strings_to_list)      strings/hydra_lib_strings_to_list     [] s (p/tc-list i))
     (prim-name 'hydra.lib.strings/hydra_lib_strings_to_lower)     (p/prim1 (prim-name 'hydra.lib.strings/hydra_lib_strings_to_lower)     strings/hydra_lib_strings_to_lower    [] s s)
     (prim-name 'hydra.lib.strings/hydra_lib_strings_to_upper)     (p/prim1 (prim-name 'hydra.lib.strings/hydra_lib_strings_to_upper)     strings/hydra_lib_strings_to_upper    [] s s)
     (prim-name 'hydra.lib.strings/hydra_lib_strings_unlines)     (p/prim1 (prim-name 'hydra.lib.strings/hydra_lib_strings_unlines)     strings/hydra_lib_strings_unlines     [] (p/tc-list s) s)}))

;; ============================================================
;; Literals
;; ============================================================

(defn register-literals []
  (let [
        bi  (p/tc-bigint)
        dec (p/tc-decimal)
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
     {(prim-name 'hydra.lib.literals/hydra_lib_literals_bigint_to_decimal)    (p/prim1 (prim-name 'hydra.lib.literals/hydra_lib_literals_bigint_to_decimal)    literals/hydra_lib_literals_bigint_to_decimal    [] bi dec)
      (prim-name 'hydra.lib.literals/hydra_lib_literals_bigint_to_int8)       (p/prim1 (prim-name 'hydra.lib.literals/hydra_lib_literals_bigint_to_int8)       literals/hydra_lib_literals_bigint_to_int8       [] bi i8)
      (prim-name 'hydra.lib.literals/hydra_lib_literals_bigint_to_int16)      (p/prim1 (prim-name 'hydra.lib.literals/hydra_lib_literals_bigint_to_int16)      literals/hydra_lib_literals_bigint_to_int16      [] bi i16)
      (prim-name 'hydra.lib.literals/hydra_lib_literals_bigint_to_int32)      (p/prim1 (prim-name 'hydra.lib.literals/hydra_lib_literals_bigint_to_int32)      literals/hydra_lib_literals_bigint_to_int32      [] bi i32)
      (prim-name 'hydra.lib.literals/hydra_lib_literals_bigint_to_int64)      (p/prim1 (prim-name 'hydra.lib.literals/hydra_lib_literals_bigint_to_int64)      literals/hydra_lib_literals_bigint_to_int64      [] bi i64)
      (prim-name 'hydra.lib.literals/hydra_lib_literals_bigint_to_uint8)      (p/prim1 (prim-name 'hydra.lib.literals/hydra_lib_literals_bigint_to_uint8)      literals/hydra_lib_literals_bigint_to_uint8      [] bi u8)
      (prim-name 'hydra.lib.literals/hydra_lib_literals_bigint_to_uint16)     (p/prim1 (prim-name 'hydra.lib.literals/hydra_lib_literals_bigint_to_uint16)     literals/hydra_lib_literals_bigint_to_uint16     [] bi u16)
      (prim-name 'hydra.lib.literals/hydra_lib_literals_bigint_to_uint32)     (p/prim1 (prim-name 'hydra.lib.literals/hydra_lib_literals_bigint_to_uint32)     literals/hydra_lib_literals_bigint_to_uint32     [] bi u32)
      (prim-name 'hydra.lib.literals/hydra_lib_literals_bigint_to_uint64)     (p/prim1 (prim-name 'hydra.lib.literals/hydra_lib_literals_bigint_to_uint64)     literals/hydra_lib_literals_bigint_to_uint64     [] bi u64)
      (prim-name 'hydra.lib.literals/hydra_lib_literals_binary_to_bytes)      (p/prim1 (prim-name 'hydra.lib.literals/hydra_lib_literals_binary_to_bytes)      literals/hydra_lib_literals_binary_to_bytes      [] bin (p/tc-list i32))
      (prim-name 'hydra.lib.literals/hydra_lib_literals_binary_to_string)     (p/prim1 (prim-name 'hydra.lib.literals/hydra_lib_literals_binary_to_string)     literals/hydra_lib_literals_binary_to_string     [] bin s)
      (prim-name 'hydra.lib.literals/hydra_lib_literals_decimal_to_bigint)    (p/prim1 (prim-name 'hydra.lib.literals/hydra_lib_literals_decimal_to_bigint)    literals/hydra_lib_literals_decimal_to_bigint    [] dec bi)
      (prim-name 'hydra.lib.literals/hydra_lib_literals_decimal_to_float32)   (p/prim1 (prim-name 'hydra.lib.literals/hydra_lib_literals_decimal_to_float32)   literals/hydra_lib_literals_decimal_to_float32   [] dec f32)
      (prim-name 'hydra.lib.literals/hydra_lib_literals_decimal_to_float64)   (p/prim1 (prim-name 'hydra.lib.literals/hydra_lib_literals_decimal_to_float64)   literals/hydra_lib_literals_decimal_to_float64   [] dec f64)
      (prim-name 'hydra.lib.literals/hydra_lib_literals_float32_to_decimal)   (p/prim1 (prim-name 'hydra.lib.literals/hydra_lib_literals_float32_to_decimal)   literals/hydra_lib_literals_float32_to_decimal   [] f32 dec)
      (prim-name 'hydra.lib.literals/hydra_lib_literals_float32_to_float64)   (p/prim1 (prim-name 'hydra.lib.literals/hydra_lib_literals_float32_to_float64)   literals/hydra_lib_literals_float32_to_float64   [] f32 f64)
      (prim-name 'hydra.lib.literals/hydra_lib_literals_float64_to_decimal)   (p/prim1 (prim-name 'hydra.lib.literals/hydra_lib_literals_float64_to_decimal)   literals/hydra_lib_literals_float64_to_decimal   [] f64 dec)
      (prim-name 'hydra.lib.literals/hydra_lib_literals_float64_to_float32)   (p/prim1 (prim-name 'hydra.lib.literals/hydra_lib_literals_float64_to_float32)   literals/hydra_lib_literals_float64_to_float32   [] f64 f32)
      (prim-name 'hydra.lib.literals/hydra_lib_literals_int8_to_bigint)       (p/prim1 (prim-name 'hydra.lib.literals/hydra_lib_literals_int8_to_bigint)       literals/hydra_lib_literals_int8_to_bigint       [] i8 bi)
      (prim-name 'hydra.lib.literals/hydra_lib_literals_int16_to_bigint)      (p/prim1 (prim-name 'hydra.lib.literals/hydra_lib_literals_int16_to_bigint)      literals/hydra_lib_literals_int16_to_bigint      [] i16 bi)
      (prim-name 'hydra.lib.literals/hydra_lib_literals_int32_to_bigint)      (p/prim1 (prim-name 'hydra.lib.literals/hydra_lib_literals_int32_to_bigint)      literals/hydra_lib_literals_int32_to_bigint      [] i32 bi)
      (prim-name 'hydra.lib.literals/hydra_lib_literals_int64_to_bigint)      (p/prim1 (prim-name 'hydra.lib.literals/hydra_lib_literals_int64_to_bigint)      literals/hydra_lib_literals_int64_to_bigint      [] i64 bi)
      (prim-name 'hydra.lib.literals/hydra_lib_literals_uint8_to_bigint)      (p/prim1 (prim-name 'hydra.lib.literals/hydra_lib_literals_uint8_to_bigint)      literals/hydra_lib_literals_uint8_to_bigint      [] u8 bi)
      (prim-name 'hydra.lib.literals/hydra_lib_literals_uint16_to_bigint)     (p/prim1 (prim-name 'hydra.lib.literals/hydra_lib_literals_uint16_to_bigint)     literals/hydra_lib_literals_uint16_to_bigint     [] u16 bi)
      (prim-name 'hydra.lib.literals/hydra_lib_literals_uint32_to_bigint)     (p/prim1 (prim-name 'hydra.lib.literals/hydra_lib_literals_uint32_to_bigint)     literals/hydra_lib_literals_uint32_to_bigint     [] u32 bi)
      (prim-name 'hydra.lib.literals/hydra_lib_literals_uint64_to_bigint)     (p/prim1 (prim-name 'hydra.lib.literals/hydra_lib_literals_uint64_to_bigint)     literals/hydra_lib_literals_uint64_to_bigint     [] u64 bi)
      (prim-name 'hydra.lib.literals/hydra_lib_literals_string_to_binary)     (p/prim1 (prim-name 'hydra.lib.literals/hydra_lib_literals_string_to_binary)     literals/hydra_lib_literals_string_to_binary     [] s bin)}
     ;; Read primitives
     {(prim-name 'hydra.lib.literals/hydra_lib_literals_read_bigint)   (p/prim1 (prim-name 'hydra.lib.literals/hydra_lib_literals_read_bigint)   literals/hydra_lib_literals_read_bigint   [] s (p/tc-optional bi))
      (prim-name 'hydra.lib.literals/hydra_lib_literals_read_boolean)  (p/prim1 (prim-name 'hydra.lib.literals/hydra_lib_literals_read_boolean)  literals/hydra_lib_literals_read_boolean  [] s (p/tc-optional b))
      (prim-name 'hydra.lib.literals/hydra_lib_literals_read_decimal)  (p/prim1 (prim-name 'hydra.lib.literals/hydra_lib_literals_read_decimal)  literals/hydra_lib_literals_read_decimal  [] s (p/tc-optional dec))
      (prim-name 'hydra.lib.literals/hydra_lib_literals_read_float32)  (p/prim1 (prim-name 'hydra.lib.literals/hydra_lib_literals_read_float32)  literals/hydra_lib_literals_read_float32  [] s (p/tc-optional f32))
      (prim-name 'hydra.lib.literals/hydra_lib_literals_read_float64)  (p/prim1 (prim-name 'hydra.lib.literals/hydra_lib_literals_read_float64)  literals/hydra_lib_literals_read_float64  [] s (p/tc-optional f64))
      (prim-name 'hydra.lib.literals/hydra_lib_literals_read_int8)     (p/prim1 (prim-name 'hydra.lib.literals/hydra_lib_literals_read_int8)     literals/hydra_lib_literals_read_int8     [] s (p/tc-optional i8))
      (prim-name 'hydra.lib.literals/hydra_lib_literals_read_int16)    (p/prim1 (prim-name 'hydra.lib.literals/hydra_lib_literals_read_int16)    literals/hydra_lib_literals_read_int16    [] s (p/tc-optional i16))
      (prim-name 'hydra.lib.literals/hydra_lib_literals_read_int32)    (p/prim1 (prim-name 'hydra.lib.literals/hydra_lib_literals_read_int32)    literals/hydra_lib_literals_read_int32    [] s (p/tc-optional i32))
      (prim-name 'hydra.lib.literals/hydra_lib_literals_read_int64)    (p/prim1 (prim-name 'hydra.lib.literals/hydra_lib_literals_read_int64)    literals/hydra_lib_literals_read_int64    [] s (p/tc-optional i64))
      (prim-name 'hydra.lib.literals/hydra_lib_literals_read_string)   (p/prim1 (prim-name 'hydra.lib.literals/hydra_lib_literals_read_string)   literals/hydra_lib_literals_read_string   [] s (p/tc-optional s))
      (prim-name 'hydra.lib.literals/hydra_lib_literals_read_uint8)    (p/prim1 (prim-name 'hydra.lib.literals/hydra_lib_literals_read_uint8)    literals/hydra_lib_literals_read_uint8    [] s (p/tc-optional u8))
      (prim-name 'hydra.lib.literals/hydra_lib_literals_read_uint16)   (p/prim1 (prim-name 'hydra.lib.literals/hydra_lib_literals_read_uint16)   literals/hydra_lib_literals_read_uint16   [] s (p/tc-optional u16))
      (prim-name 'hydra.lib.literals/hydra_lib_literals_read_uint32)   (p/prim1 (prim-name 'hydra.lib.literals/hydra_lib_literals_read_uint32)   literals/hydra_lib_literals_read_uint32   [] s (p/tc-optional u32))
      (prim-name 'hydra.lib.literals/hydra_lib_literals_read_uint64)   (p/prim1 (prim-name 'hydra.lib.literals/hydra_lib_literals_read_uint64)   literals/hydra_lib_literals_read_uint64   [] s (p/tc-optional u64))}
     ;; Show primitives
     {(prim-name 'hydra.lib.literals/hydra_lib_literals_show_bigint)   (p/prim1 (prim-name 'hydra.lib.literals/hydra_lib_literals_show_bigint)   literals/hydra_lib_literals_show_bigint   [] bi s)
      (prim-name 'hydra.lib.literals/hydra_lib_literals_show_boolean)  (p/prim1 (prim-name 'hydra.lib.literals/hydra_lib_literals_show_boolean)  literals/hydra_lib_literals_show_boolean  [] b s)
      (prim-name 'hydra.lib.literals/hydra_lib_literals_show_decimal)  (p/prim1 (prim-name 'hydra.lib.literals/hydra_lib_literals_show_decimal)  literals/hydra_lib_literals_show_decimal  [] dec s)
      (prim-name 'hydra.lib.literals/hydra_lib_literals_show_float32)  (p/prim1 (prim-name 'hydra.lib.literals/hydra_lib_literals_show_float32)  literals/hydra_lib_literals_show_float32  [] f32 s)
      (prim-name 'hydra.lib.literals/hydra_lib_literals_show_float64)  (p/prim1 (prim-name 'hydra.lib.literals/hydra_lib_literals_show_float64)  literals/hydra_lib_literals_show_float64  [] f64 s)
      (prim-name 'hydra.lib.literals/hydra_lib_literals_show_int8)     (p/prim1 (prim-name 'hydra.lib.literals/hydra_lib_literals_show_int8)     literals/hydra_lib_literals_show_int8     [] i8 s)
      (prim-name 'hydra.lib.literals/hydra_lib_literals_show_int16)    (p/prim1 (prim-name 'hydra.lib.literals/hydra_lib_literals_show_int16)    literals/hydra_lib_literals_show_int16    [] i16 s)
      (prim-name 'hydra.lib.literals/hydra_lib_literals_show_int32)    (p/prim1 (prim-name 'hydra.lib.literals/hydra_lib_literals_show_int32)    literals/hydra_lib_literals_show_int32    [] i32 s)
      (prim-name 'hydra.lib.literals/hydra_lib_literals_show_int64)    (p/prim1 (prim-name 'hydra.lib.literals/hydra_lib_literals_show_int64)    literals/hydra_lib_literals_show_int64    [] i64 s)
      (prim-name 'hydra.lib.literals/hydra_lib_literals_show_uint8)    (p/prim1 (prim-name 'hydra.lib.literals/hydra_lib_literals_show_uint8)    literals/hydra_lib_literals_show_uint8    [] u8 s)
      (prim-name 'hydra.lib.literals/hydra_lib_literals_show_uint16)   (p/prim1 (prim-name 'hydra.lib.literals/hydra_lib_literals_show_uint16)   literals/hydra_lib_literals_show_uint16   [] u16 s)
      (prim-name 'hydra.lib.literals/hydra_lib_literals_show_uint32)   (p/prim1 (prim-name 'hydra.lib.literals/hydra_lib_literals_show_uint32)   literals/hydra_lib_literals_show_uint32   [] u32 s)
      (prim-name 'hydra.lib.literals/hydra_lib_literals_show_uint64)   (p/prim1 (prim-name 'hydra.lib.literals/hydra_lib_literals_show_uint64)   literals/hydra_lib_literals_show_uint64   [] u64 s)
      (prim-name 'hydra.lib.literals/hydra_lib_literals_show_string)   (p/prim1 (prim-name 'hydra.lib.literals/hydra_lib_literals_show_string)   literals/hydra_lib_literals_show_string   [] s s)})))

;; ============================================================
;; Effects (#494)
;; ============================================================
;;
;; effect<t> is transparent in Clojure (effect<t> = t). These are registered so the inference
;; graph can resolve the hydra.lib.effects.* names; their type schemes match the kernel
;; signatures exactly (note pure is x -> effect<x>, including the function arrow). The real
;; evaluation happens through the relocated hydra.overlay.clojure.lib.effects runtime, reached via the
;; bootstrap redirect; the impls wired here are consistent with that runtime.

(defn register-effects []
  (let [x (p/tc-variable "x")
        y (p/tc-variable "y")
        z (p/tc-variable "z")
        eff (fn [c] (p/tc-effect c))]
    {(prim-name 'hydra.lib.effects/hydra_lib_effects_apply) (p/prim2 (prim-name 'hydra.lib.effects/hydra_lib_effects_apply)
                                     (fn [ef ex] ((effects/hydra_lib_effects_apply ef) ex))
                                     ["x" "y"] (eff (p/tc-function x y)) (eff x) (eff y))
     (prim-name 'hydra.lib.effects/hydra_lib_effects_bind) (p/prim2 (prim-name 'hydra.lib.effects/hydra_lib_effects_bind)
                                     (fn [a f] ((effects/hydra_lib_effects_bind a) f))
                                     ["x" "y"] (eff x) (p/tc-function x (eff y)) (eff y))
     (prim-name 'hydra.lib.effects/hydra_lib_effects_compose) (p/prim3 (prim-name 'hydra.lib.effects/hydra_lib_effects_compose)
                                     (fn [f g x_] (((effects/hydra_lib_effects_compose f) g) x_))
                                     ["x" "y" "z"] (p/tc-function x (eff y)) (p/tc-function y (eff z)) x (eff z))
     (prim-name 'hydra.lib.effects/hydra_lib_effects_foldl) (p/prim3 (prim-name 'hydra.lib.effects/hydra_lib_effects_foldl)
                                     (fn [f acc xs] (((effects/hydra_lib_effects_foldl f) acc) xs))
                                     ["x" "y"] (p/tc-function x (p/tc-function y (eff x))) x (p/tc-list y) (eff x))
     (prim-name 'hydra.lib.effects/hydra_lib_effects_map) (p/prim2 (prim-name 'hydra.lib.effects/hydra_lib_effects_map)
                                     (fn [f a] ((effects/hydra_lib_effects_map f) a))
                                     ["x" "y"] (p/tc-function x y) (eff x) (eff y))
     (prim-name 'hydra.lib.effects/hydra_lib_effects_map_list) (p/prim2 (prim-name 'hydra.lib.effects/hydra_lib_effects_map_list)
                                     (fn [f xs] ((effects/hydra_lib_effects_map_list f) xs))
                                     ["x" "y"] (p/tc-function x (eff y)) (p/tc-list x) (eff (p/tc-list y)))
     (prim-name 'hydra.lib.effects/hydra_lib_effects_map_optional) (p/prim2 (prim-name 'hydra.lib.effects/hydra_lib_effects_map_optional)
                                     (fn [f m] ((effects/hydra_lib_effects_map_optional f) m))
                                     ["x" "y"] (p/tc-function x (eff y)) (p/tc-optional x) (eff (p/tc-optional y)))
     (prim-name 'hydra.lib.effects/hydra_lib_effects_pure) (p/prim1 (prim-name 'hydra.lib.effects/hydra_lib_effects_pure)
                                     effects/hydra_lib_effects_pure
                                     ["x"] x (eff x))}))

;; ============================================================
;; Files (#494)
;; ============================================================
;;
;; FilePath and FileError are nominal kernel types (referenced by name). unit maps to nil,
;; binary to a byte-array, either to the (list :left/:right) representation. As with effects,
;; the type schemes are registered for inference name-resolution; the real I/O happens in
;; hydra.overlay.clojure.lib.files, reached via the bootstrap redirect.

(defn register-files []
  (let [bool (p/tc-boolean)
        bin (p/tc-binary)
        fp (p/tc-named "hydra.file.FilePath")
        ferr (p/tc-named "hydra.error.file.FileError")
        fstat (p/tc-named "hydra.file.FileStatus")
        unit (p/tc-unit)
        eff (fn [c] (p/tc-effect c))]
    {(prim-name 'hydra.lib.files/hydra_lib_files_append_file) (p/prim2 (prim-name 'hydra.lib.files/hydra_lib_files_append_file)
                                     (fn [path contents] ((files/hydra_lib_files_append_file path) contents))
                                     [] fp bin (eff (p/tc-either ferr unit)))
     (prim-name 'hydra.lib.files/hydra_lib_files_copy) (p/prim3 (prim-name 'hydra.lib.files/hydra_lib_files_copy)
                                     (fn [recursive source destination] (((files/hydra_lib_files_copy recursive) source) destination))
                                     [] bool fp fp (eff (p/tc-either ferr unit)))
     (prim-name 'hydra.lib.files/hydra_lib_files_create_directory) (p/prim2 (prim-name 'hydra.lib.files/hydra_lib_files_create_directory)
                                     (fn [recursive path] ((files/hydra_lib_files_create_directory recursive) path))
                                     [] bool fp (eff (p/tc-either ferr unit)))
     (prim-name 'hydra.lib.files/hydra_lib_files_exists) (p/prim1 (prim-name 'hydra.lib.files/hydra_lib_files_exists)
                                     files/hydra_lib_files_exists
                                     [] fp (eff (p/tc-either ferr bool)))
     (prim-name 'hydra.lib.files/hydra_lib_files_list_directory) (p/prim1 (prim-name 'hydra.lib.files/hydra_lib_files_list_directory)
                                     files/hydra_lib_files_list_directory
                                     [] fp (eff (p/tc-either ferr (p/tc-list fp))))
     (prim-name 'hydra.lib.files/hydra_lib_files_read_file) (p/prim1 (prim-name 'hydra.lib.files/hydra_lib_files_read_file)
                                     files/hydra_lib_files_read_file
                                     [] fp (eff (p/tc-either ferr bin)))
     (prim-name 'hydra.lib.files/hydra_lib_files_remove_directory) (p/prim2 (prim-name 'hydra.lib.files/hydra_lib_files_remove_directory)
                                     (fn [recursive path] ((files/hydra_lib_files_remove_directory recursive) path))
                                     [] bool fp (eff (p/tc-either ferr unit)))
     (prim-name 'hydra.lib.files/hydra_lib_files_remove_file) (p/prim1 (prim-name 'hydra.lib.files/hydra_lib_files_remove_file)
                                     files/hydra_lib_files_remove_file
                                     [] fp (eff (p/tc-either ferr unit)))
     (prim-name 'hydra.lib.files/hydra_lib_files_rename) (p/prim2 (prim-name 'hydra.lib.files/hydra_lib_files_rename)
                                     (fn [source destination] ((files/hydra_lib_files_rename source) destination))
                                     [] fp fp (eff (p/tc-either ferr unit)))
     (prim-name 'hydra.lib.files/hydra_lib_files_status) (p/prim1 (prim-name 'hydra.lib.files/hydra_lib_files_status)
                                     files/hydra_lib_files_status
                                     [] fp (eff (p/tc-either ferr fstat)))
     (prim-name 'hydra.lib.files/hydra_lib_files_write_file) (p/prim2 (prim-name 'hydra.lib.files/hydra_lib_files_write_file)
                                     (fn [path contents] ((files/hydra_lib_files_write_file path) contents))
                                     [] fp bin (eff (p/tc-either ferr unit)))}))

;; ============================================================
;; Text (#494)
;; ============================================================
;;
;; UTF-8 codecs bridging Hydra strings and raw bytes. decodeUtf8 :: binary -> either<string, string>
;; (Left message on invalid UTF-8); encodeUtf8 :: string -> binary (total).

(defn register-hashing []
  (let [s (p/tc-string)
        bin (p/tc-binary)]
    {(prim-name 'hydra.lib.hashing/hydra_lib_hashing_sha256) (p/prim1 (prim-name 'hydra.lib.hashing/hydra_lib_hashing_sha256)
                                     hashing/hydra_lib_hashing_sha256
                                     [] bin bin)
     (prim-name 'hydra.lib.hashing/hydra_lib_hashing_sha256_hex) (p/prim1 (prim-name 'hydra.lib.hashing/hydra_lib_hashing_sha256_hex)
                                     hashing/hydra_lib_hashing_sha256_hex
                                     [] bin s)}))

(defn register-text []
  (let [s (p/tc-string)
        bin (p/tc-binary)]
    {(prim-name 'hydra.lib.text/hydra_lib_text_decode_utf8) (p/prim1 (prim-name 'hydra.lib.text/hydra_lib_text_decode_utf8)
                                     text/hydra_lib_text_decode_utf8
                                     [] bin (p/tc-either s s))
     (prim-name 'hydra.lib.text/hydra_lib_text_encode_utf8) (p/prim1 (prim-name 'hydra.lib.text/hydra_lib_text_encode_utf8)
                                     text/hydra_lib_text_encode_utf8
                                     [] s bin)}))

;; ============================================================
;; System (#498)
;; ============================================================
;;
;; The hydra.lib.system.* effectful primitives (execute/exit/getEnvironment/...) whose real I/O lives
;; in hydra.overlay.clojure.lib.system, reached via the bootstrap redirect. Nominal kernel types
;; (Command, ProcessResult, SystemError, EnvironmentVariable, StatusCode, Timespec, FilePath) are
;; referenced by name; the type schemes are registered for inference name-resolution. Without this,
;; cross-host generation of hydra.test.lib.system fails with "Unknown variable:
;; hydra.lib.system.getEnvironment" (the def-module data is present but the primitive is unregistered).
(defn register-system []
  (let [s (p/tc-string)
        unit (p/tc-unit)
        bin (p/tc-binary)
        eff (fn [c] (p/tc-effect c))
        cmd (p/tc-named "hydra.system.Command")
        procResult (p/tc-named "hydra.system.ProcessResult")
        sysErr (p/tc-named "hydra.error.system.SystemError")
        envVar (p/tc-named "hydra.system.EnvironmentVariable")
        statusCode (p/tc-named "hydra.system.StatusCode")
        timespec (p/tc-named "hydra.time.Timespec")
        filePath (p/tc-named "hydra.file.FilePath")]
    {(prim-name 'hydra.lib.system/hydra_lib_system_execute) (p/prim1 (prim-name 'hydra.lib.system/hydra_lib_system_execute)
                                     system/hydra_lib_system_execute
                                     [] cmd (eff (p/tc-either sysErr procResult)))
     (prim-name 'hydra.lib.system/hydra_lib_system_exit) (p/prim1 (prim-name 'hydra.lib.system/hydra_lib_system_exit)
                                     system/hydra_lib_system_exit
                                     [] statusCode (eff unit))
     (prim-name 'hydra.lib.system/hydra_lib_system_get_environment) (p/prim0 (prim-name 'hydra.lib.system/hydra_lib_system_get_environment)
                                     (fn [] system/hydra_lib_system_get_environment)
                                     [] (eff (p/tc-map envVar s)))
     (prim-name 'hydra.lib.system/hydra_lib_system_get_environment_variable) (p/prim1 (prim-name 'hydra.lib.system/hydra_lib_system_get_environment_variable)
                                     system/hydra_lib_system_get_environment_variable
                                     [] envVar (eff (p/tc-optional s)))
     (prim-name 'hydra.lib.system/hydra_lib_system_get_time) (p/prim0 (prim-name 'hydra.lib.system/hydra_lib_system_get_time)
                                     (fn [] system/hydra_lib_system_get_time)
                                     [] (eff timespec))
     (prim-name 'hydra.lib.system/hydra_lib_system_get_working_directory) (p/prim0 (prim-name 'hydra.lib.system/hydra_lib_system_get_working_directory)
                                     (fn [] system/hydra_lib_system_get_working_directory)
                                     [] (eff (p/tc-either sysErr filePath)))
     (prim-name 'hydra.lib.system/hydra_lib_system_read_stdin) (p/prim0 (prim-name 'hydra.lib.system/hydra_lib_system_read_stdin)
                                     (fn [] system/hydra_lib_system_read_stdin)
                                     [] (eff (p/tc-either sysErr bin)))
     (prim-name 'hydra.lib.system/hydra_lib_system_write_stderr) (p/prim1 (prim-name 'hydra.lib.system/hydra_lib_system_write_stderr)
                                     system/hydra_lib_system_write_stderr
                                     [] bin (eff (p/tc-either sysErr unit)))
     (prim-name 'hydra.lib.system/hydra_lib_system_write_stdout) (p/prim1 (prim-name 'hydra.lib.system/hydra_lib_system_write_stdout)
                                     system/hydra_lib_system_write_stdout
                                     [] bin (eff (p/tc-either sysErr unit)))}))

;; ============================================================
;; ============================================================
;; Annotations (term-level functions registered as primitives)
;; ============================================================

(defn- term-maybe-to-native
  "Convert a term-level maybe (:optional val_or_nil) to native maybe (:given val) / (:none)"
  [m]
  (cond
    (nil? m) (list :none)
    (not (sequential? m)) (list :given m)
    (= (first m) :none) (list :none)
    (= (first m) :given) m
    (= (first m) :optional)
    (let [inner (second m)]
      (cond
        (or (nil? inner)
            (and (sequential? inner) (= (first inner) :none)))
        (list :none)
        (and (sequential? inner) (= (first inner) :given))
        inner  ;; (:given val) — already in native format
        :else
        (list :given inner)))
    :else (list :given m)))

(defn- native-maybe-to-term
  "Convert a native maybe (:given val) / (:none) to term-level (:optional val_or_nil)"
  [m]
  (cond
    (nil? m) (list :optional nil)
    (not (sequential? m)) (list :optional m)
    (= (first m) :given) (list :optional (second m))
    (= (first m) :none) (list :optional nil)
    :else (list :optional m)))

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
                                     (= (first native-d) :none) (list :none)
                                     :else (let [inner (second native-d)]
                                             (if (and (sequential? inner) (= (first inner) :literal)
                                                      (sequential? (second inner)) (= (first (second inner)) :string))
                                               (list :given (second (second inner)))
                                               native-d)))]
                  (((deref (or (resolve 'hydra_annotations_set_term_description) (ns-resolve 'hydra.annotations 'hydra_annotations_set_term_description))) native-str-d) term)))
              [] t t t)
     ;; getTermDescription :: InferenceContext -> Graph -> Term -> Either (Maybe String)
     "hydra.annotations.getTermDescription"
     (p/prim3 "hydra.annotations.getTermDescription"
              (fn [cx graph term]
                (let [result ((((deref (or (resolve 'hydra_annotations_get_term_description) (ns-resolve 'hydra.annotations 'hydra_annotations_get_term_description))) cx) graph) term)]
                  ;; Result is Either Error (Maybe String)
                  ;; Convert to term-level: (:either (:right (:optional (:literal (:string "...")))))
                  (if (= (first result) :left)
                    (list :either result)
                    (let [maybe-str (second result)
                          term-maybe (cond
                                       (nil? maybe-str) (list :optional nil)
                                       (= (first maybe-str) :none) (list :optional nil)
                                       (= (first maybe-str) :given)
                                       (list :optional (list :literal (list :string (second maybe-str))))
                                       :else (list :optional maybe-str))]
                      (list :either (list :right term-maybe))))))
              [] t t t t)}))

;; Standard library: all primitives combined
;; ============================================================

(defn standard-library
  "Returns a map from primitive name (string) to Primitive record."
  []
  (merge
   (register-chars)
   (register-effects)
   (register-eithers)
   (register-equality)
   (register-files)
   (register-hashing)
   (register-lists)
   (register-literals)
   (register-logic)
   (register-maps)
   (register-math)
   (register-optionals)
   (register-pairs)
   (register-regex)
   (register-sets)
   (register-strings)
   (register-system)
   (register-text)
   (register-annotations)))
