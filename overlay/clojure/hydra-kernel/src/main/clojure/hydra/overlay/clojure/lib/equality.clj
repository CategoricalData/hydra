(ns hydra.overlay.clojure.lib.equality
  (:require [hydra.overlay.clojure.lib.ordering :refer [generic-compare]]))

;; equal :: a -> a -> Bool
;; Delegates to generic-compare (hydra.overlay.clojure.lib.ordering) -- the
;; established pattern already used by maps.clj/sets.clj/lists.clj/eithers.clj
;; -- rather than a separate implementation, per
;; docs/specification/ordering-and-equality.md: equal a b holds exactly when
;; compare a b is equalTo.
;;
;; This also fixes a bug where a bare top-level (:literal (:decimal ...)) term
;; got the correct scale-sensitive comparison (via a dedicated decimal-term?
;; check, using BigDecimal .equals instead of Clojure's scale-blind numeric-
;; tower `=`), but a decimal NESTED inside a record/map (e.g. a Field's term,
;; or a map value) fell straight through to native `=`, which recurses
;; structurally but compares any BigDecimal leaf it reaches with scale-blind
;; equality ((= 1.10M 1.1M) => true) -- silently wrong for exactly the values
;; docs/specification/ordering-and-equality.md's decimal scale-distinctness
;; provision exists to distinguish. generic-compare's decimal branch is
;; unconditional (used at every recursion depth, not just the top level), so
;; delegating to it is correct at any nesting depth.
(def hydra_overlay_clojure_lib_equality_equal
  "Check if two values are equal."
  (fn [a] (fn [b] (zero? (generic-compare a b)))))
