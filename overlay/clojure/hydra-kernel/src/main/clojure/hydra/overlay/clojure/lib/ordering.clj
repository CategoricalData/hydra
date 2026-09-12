(ns hydra.overlay.clojure.lib.ordering)

;; Declared-variant order for each hydra.core union family, transcribed from
;; the generated hydra/core.clj (hydra_core_term-variants, hydra_core_type-
;; variants, etc.), which in turn come from the DSL declaration order
;; (packages/hydra-kernel/.../Sources/Kernel/Types/Core.hs). Mirrors Python's
;; _VARIANT_ORDER table (overlay/python/.../util/_compare.py) -- the
;; established #718 precedent hand-authors variant order for kernel types
;; only; non-kernel (user-schema) unions fall back to a still-deterministic
;; but non-declared-order tag comparison (see generic-compare's keyword
;; branch below). Keyed by family name (not by tag alone): a union value's
;; tag keyword alone carries no runtime type identity, and tags collide
;; across unrelated unions repo-wide (:literal, :map, :unit, etc.), so a
;; single flat tag->ordinal table would be unsound. Hydra's static typing
;; guarantees compare/equal are only ever called on same-typed values
;; (docs/specification/ordering-and-equality.md), so it is safe to resolve
;; the family from BOTH sides' tags and require them to agree.
(def ^:private variant-order
  {"term" [:annotated :application :cases :either :lambda :let :list
           :literal :map :optional :pair :project :record :set
           :type_lambda :type_application :inject :unit :unwrap
           :variable :wrap]
   "type" [:annotated :application :effect :either :forall :function
           :list :literal :map :optional :pair :record :set :union
           :unit :variable :void :wrap]
   "literal" [:binary :boolean :decimal :float :integer :string]
   "integer" [:bigint :int8 :int16 :int32 :int64 :uint8 :uint16 :uint32 :uint64]
   "float" [:float32 :float64]})

;; If tags A and B both belong to the SAME known family, return
;; [ordinal-a ordinal-b]; otherwise nil (unknown family, or a family
;; mismatch that should not arise under Hydra's static typing -- callers
;; fall back to a deterministic tag compare in that case).
(defn- variant-ordinals [tag-a tag-b]
  (some (fn [variants]
          (let [pa (.indexOf ^java.util.List variants tag-a)
                pb (.indexOf ^java.util.List variants tag-b)]
            (when (and (not= pa -1) (not= pb -1)) [pa pb])))
        (vals variant-order)))

;; Compare two tag keywords: declared-variant order when both resolve to the
;; same known kernel family, else a deterministic (not print-based) keyword
;; compare -- the same scope limitation #718 carries on every host but Java.
(defn- compare-tags [ta tb]
  (if (= ta tb)
    0
    (let [ordinals (variant-ordinals ta tb)]
      (if ordinals
        (compare (first ordinals) (second ordinals))
        (compare ta tb)))))

;; Compare two strings by Unicode code point rather than UTF-16 code unit
;; (native String/compareTo): a code-unit compare misorders an astral
;; character (code point > 0xFFFF, a surrogate pair) relative to a BMP
;; private-use character (U+E000-FFFF, a single unit).
(defn- compare-strings-by-code-point [a b]
  (let [ca (.codePoints ^String a) cb (.codePoints ^String b)
        ia (.iterator ca) ib (.iterator cb)]
    (loop []
      (cond
        (and (not (.hasNext ia)) (not (.hasNext ib))) 0
        (not (.hasNext ia)) -1
        (not (.hasNext ib)) 1
        :else
        (let [x (.nextInt ia) y (.nextInt ib)]
          (if (= x y) (recur) (compare x y)))))))

(defn generic-compare
  "Generic comparison function for arbitrary Clojure values, returning -1, 0, or 1."
  [a b]
  (cond
    (identical? a b) 0
    (nil? a) (if (nil? b) 0 -1)
    (nil? b) 1
    ;; Hydra decimal ordering is by numeric value first, with numerically-equal decimals of
    ;; different scale tiebroken by scale ascending (docs/specification/ordering-and-equality.md:
    ;; 1.1 < 1.10 < 1.100). Clojure's `compare` on BigDecimal is numeric-value-only (scale-blind),
    ;; so add the scale tiebreak explicitly, mirroring Java's Comparing.compareDecimals.
    (and (instance? java.math.BigDecimal a) (instance? java.math.BigDecimal b))
    (let [c (compare a b)]
      (if (not= c 0) c (compare (.scale ^java.math.BigDecimal a) (.scale ^java.math.BigDecimal b))))
    ;; Floats: Clojure's `compare` already treats NaN as equal to itself (matching
    ;; IEEE 754 extended totalOrder, docs/specification/ordering-and-equality.md),
    ;; but treats -0.0 and +0.0 as equal (native Java Double.compare disagrees:
    ;; -0.0 < +0.0). Delegate to Double/compare, which gets both right.
    (and (instance? Double a) (instance? Double b)) (Double/compare a b)
    (and (instance? Float a) (instance? Float b)) (Float/compare a b)
    (and (number? a) (number? b)) (compare a b)
    ;; Native String/compare compares UTF-16 code units, which misorders an
    ;; astral character (code point > 0xFFFF, a surrogate pair) relative to a
    ;; BMP private-use character (U+E000-FFFF, a single unit): compare by code
    ;; point instead.
    (and (string? a) (string? b)) (compare-strings-by-code-point a b)
    (and (keyword? a) (keyword? b)) (compare-tags a b)
    (and (boolean? a) (boolean? b)) (compare a b)
    (and (char? a) (char? b)) (compare (int a) (int b))
    ;; Records (defrecord instances): compare by type name first, then fields
    ;; in DECLARATION order -- (keys record) on a defrecord instance already
    ;; preserves declaration order (verified: defrecord fields are backed by
    ;; a fixed-order struct-map), so no separate field-order table is needed
    ;; here (unlike the union-tag case above); do NOT `sort` the keys, which
    ;; would silently re-order them alphabetically.
    (instance? clojure.lang.IRecord a)
    (if (instance? clojure.lang.IRecord b)
      (let [ta (type a) tb (type b)]
        (if (= ta tb)
          (loop [ks (seq (keys a))]
            (if (nil? ks) 0
              (let [k (first ks)
                    c (generic-compare (get a k) (get b k))]
                (if (not= c 0) c (recur (next ks))))))
          (compare (str ta) (str tb))))
      (compare (str (type a)) (str (type b))))
    (map? a)
    (if (map? b)
      (let [ca (count a) cb (count b)]
        (if (not= ca cb)
          (compare ca cb)
          (loop [ra (seq a) rb (seq b)]
            (cond
              (nil? ra) 0
              :else
              (let [ea (first ra) eb (first rb)
                    ck (generic-compare (key ea) (key eb))]
                (if (not= ck 0) ck
                  (let [cv (generic-compare (val ea) (val eb))]
                    (if (not= cv 0) cv
                      (recur (next ra) (next rb))))))))))
      (compare (str (type a)) (str (type b))))
    ;; Lists (including the (tag payload...) union encoding, whose arity
    ;; varies by variant) and the tuple encoding of Pair: lexicographic,
    ;; shorter-is-prefix-less (docs/specification/ordering-and-equality.md),
    ;; NOT length-first -- comparing lengths before elements was wrong (it
    ;; would rank a (:unit nil) 2-element union payload as "less than" any
    ;; 3-element variant's payload regardless of the tag itself, since
    ;; :unit's own tag ordinal is never reached).
    (and (sequential? a) (sequential? b))
    (loop [ra (seq a) rb (seq b)]
      (cond
        (and (nil? ra) (nil? rb)) 0
        (nil? ra) -1
        (nil? rb) 1
        :else
        (let [c (generic-compare (clojure.core/first ra) (clojure.core/first rb))]
          (if (not= c 0) c (recur (next ra) (next rb))))))
    :else (compare (pr-str a) (pr-str b))))

;; compare :: a -> a -> Comparison
;; Returns a Comparison union: (list :less_than nil), (list :equal_to nil), (list :greater_than nil)
(def hydra_overlay_clojure_lib_ordering_compare
  "Compare two values and return a Comparison."
  (fn [a] (fn [b]
    (let [c (generic-compare a b)]
      (cond
        (neg? c) (list :less_than nil)
        (zero? c) (list :equal_to nil)
        :else (list :greater_than nil))))))

;; gt :: a -> a -> Bool
(def hydra_overlay_clojure_lib_ordering_gt
  "Check if first value is greater than second."
  (fn [a] (fn [b] (> (generic-compare a b) 0))))

;; gte :: a -> a -> Bool
(def hydra_overlay_clojure_lib_ordering_gte
  "Check if first value is greater than or equal to second."
  (fn [a] (fn [b] (>= (generic-compare a b) 0))))

;; lt :: a -> a -> Bool
(def hydra_overlay_clojure_lib_ordering_lt
  "Check if first value is less than second."
  (fn [a] (fn [b] (< (generic-compare a b) 0))))

;; lte :: a -> a -> Bool
(def hydra_overlay_clojure_lib_ordering_lte
  "Check if first value is less than or equal to second."
  (fn [a] (fn [b] (<= (generic-compare a b) 0))))

;; max :: a -> a -> a
(def hydra_overlay_clojure_lib_ordering_max
  "Return the maximum of two values."
  (fn [a] (fn [b] (if (>= (generic-compare a b) 0) a b))))

;; min :: a -> a -> a
(def hydra_overlay_clojure_lib_ordering_min
  "Return the minimum of two values."
  (fn [a] (fn [b] (if (<= (generic-compare a b) 0) a b))))
