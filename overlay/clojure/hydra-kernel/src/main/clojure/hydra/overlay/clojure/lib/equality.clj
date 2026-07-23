(ns hydra.overlay.clojure.lib.equality)

;; equal :: a -> a -> Bool
(def hydra_lib_equality_equal
  "Check if two values are equal."
  (fn [a] (fn [b]
    ;; Hydra decimal equality is numerical. Java BigDecimal's .equals is scale-sensitive
    ;; ("0.0" != "0"), and Clojure's = returns false when one operand is a Double and the
    ;; other is a BigDecimal even when numerically equal. Coerce both sides to BigDecimal
    ;; when either is a BigDecimal/Double and compare by value.
    (cond
      (and (instance? java.math.BigDecimal a) (instance? java.math.BigDecimal b))
        (zero? (.compareTo ^java.math.BigDecimal a ^java.math.BigDecimal b))
      (and (instance? java.math.BigDecimal a) (or (instance? Double b) (instance? Float b)))
        (zero? (.compareTo ^java.math.BigDecimal a (bigdec b)))
      (and (instance? java.math.BigDecimal b) (or (instance? Double a) (instance? Float a)))
        (zero? (.compareTo (bigdec a) ^java.math.BigDecimal b))
      :else
        (= a b)))))
