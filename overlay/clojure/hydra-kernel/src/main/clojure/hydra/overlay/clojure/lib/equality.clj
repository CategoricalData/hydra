(ns hydra.overlay.clojure.lib.equality)

;; A Literal.decimal wrapped as a Hydra term, as `equal`'s generic x/x TermCoder (which just
;; passes terms through unchanged -- see prims.clj's tc-variable) actually receives it:
;; (:literal (:decimal <BigDecimal>)), not a bare BigDecimal.
(defn- decimal-term? [t]
  (and (sequential? t) (= (first t) :literal)
       (let [lit (second t)]
         (and (sequential? lit) (= (first lit) :decimal)))))

(defn- decimal-term-value [t] (second (second t)))

;; equal :: a -> a -> Bool
(def hydra_overlay_clojure_lib_equality_equal
  "Check if two values are equal."
  (fn [a] (fn [b]
    ;; Hydra decimal equality is scale-distinct (docs/specification/ordering-and-equality.md):
    ;; 1.10 and 1.1 are the same number but distinct, unequal values (coefficient AND scale
    ;; must agree). Clojure's own `=` on BigDecimal (and on the (:literal (:decimal ...)) term
    ;; wrapping it, since = recurses structurally) is numeric-tower equality (scale-blind:
    ;; (= 1.10M 1.1M) is true), the opposite of what's needed -- use BigDecimal's .equals
    ;; (scale-sensitive) instead. A decimal is never compared against a float/double in practice
    ;; (#727 -- Clojure is the only Lisp dialect with a real decimal representation, and
    ;; float/decimal are distinct Hydra literal types never adapted into each other for a
    ;; language that supports both), so no cross-type coercion is needed here.
    (if (and (decimal-term? a) (decimal-term? b))
      (.equals ^java.math.BigDecimal (decimal-term-value a) (decimal-term-value b))
      (= a b)))))
