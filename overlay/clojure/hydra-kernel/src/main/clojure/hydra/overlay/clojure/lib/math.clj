(ns hydra.overlay.clojure.lib.math)

;; Constraint-polymorphic ('numeric') dispatch for add/sub/mul/negate.
;;
;; These vars serve two distinct call contracts, both real:
;;
;;  1. The interpreter path: registered as `hydra.lib.math.add` etc. via `p/prim2`/`p/prim1` with
;;     a 'numeric' class constraint and identity (Term) coders (tc-variable) in libraries.clj's
;;     register-math, so on that path the compute fn receives raw Terms -- list-encoded, e.g.
;;     (:literal (:integer (:int32 42))) -- and must dispatch on the operand's literal variant.
;;  2. The generated-kernel-code path: this SAME def is :refer :all-imported directly by Hydra's
;;     own self-hosted Clojure source (e.g. hydra.names, hydra.lexical, hydra.rewriting) and
;;     called as ordinary curried arithmetic over raw native numbers (Long, Double, BigInt, ...),
;;     the same way user Clojure code would call +/-/* directly -- no Term involved at all.
;;
;; This mirrors the Haskell host's numericBinary/numericUnary (Hydra.Overlay.Haskell.Lib.Math) and
;; Java's NumericDispatch, except Java splits the two contracts onto two differently-typed methods
;; on one class (implementation() vs a generic apply()); Clojure has one flat function namespace,
;; so both contracts are dispatched here by argument shape instead: a Term literal is a sequential
;; (a list/seq), a native number is not. No typeclass mechanism is consulted at runtime -- the host
;; has none. Type inference guarantees both operands of a binary op share one numeric type, so the
;; Term-path dispatch keys on the first operand and requires the second to match; a mismatch or a
;; non-numeric operand is an internal invariant violation and fails loudly.
;;
;; Fixed-width integer variants are narrowed back to the source width (two's-complement
;; wraparound, mirroring Java's NumericDispatch.rewrapInteger); bigint is arbitrary precision.
;; The native-value path needs no narrowing: Clojure's own numeric tower (auto-promoting +'/-'/*'
;; or plain +/-/* on the native types already in play) already gives the right per-type behavior.

(def ^:private int-width-bits
  {:int8 8 :int16 16 :int32 32 :int64 64
   :uint8 8 :uint16 16 :uint32 32 :uint64 64})

(def ^:private unsigned-int-widths #{:uint8 :uint16 :uint32 :uint64})

(defn- wrap-int [width-tag r]
  (if (= width-tag :bigint)
    r
    ;; bit-shift-left operates on primitive longs and silently wraps at 64 bits (e.g.
    ;; (bit-shift-left 1 64) => 1, not 2^64), which is wrong for the int64/uint64 widths here;
    ;; use BigInteger's arbitrary-precision shiftLeft instead.
    (let [bits (int-width-bits width-tag)
          m (bigint (.shiftLeft (biginteger 1) bits))]
      (if (contains? unsigned-int-widths width-tag)
        (mod r m)
        (let [w (mod r m)]
          (if (>= w (/ m 2)) (- w m) w))))))

;; Clojure's `(float x)` coercion THROWS ArithmeticException for any double outside float32's
;; finite range (including actual infinities), instead of saturating to +-Infinity per IEEE 754 --
;; e.g. (float 6.8e38) or (float ##Inf) both throw "Value out of range for float". `.floatValue`
;; (widening-narrowing via the boxed Double's own method) correctly saturates instead.
(defn- to-float32 [^double r]
  (.floatValue ^Double (Double/valueOf r)))

(defn- numeric-literal [op-name t]
  (if (and (sequential? t) (= (first t) :literal))
    (second t)
    (throw (ex-info (str "hydra.lib.math." op-name ": expected a numeric literal term") {}))))

(defn- numeric-binary-term [op-name int-op float-op x y]
  (let [lx (numeric-literal op-name x)
        ly (numeric-literal op-name y)
        lx-kind (first lx)
        ly-kind (first ly)]
    (when (not= lx-kind ly-kind)
      (throw (ex-info (str "hydra.lib.math." op-name ": operands are not the same numeric kind") {})))
    (cond
      (= lx-kind :integer)
      (let [vx (second lx) vy (second ly)
            vx-tag (first vx) vy-tag (first vy)]
        (when (not= vx-tag vy-tag)
          (throw (ex-info (str "hydra.lib.math." op-name ": integer operands differ in precision") {})))
        (list :literal (list :integer (list vx-tag (wrap-int vx-tag (int-op (second vx) (second vy)))))))
      (= lx-kind :float)
      (let [vx (second lx) vy (second ly)
            vx-tag (first vx) vy-tag (first vy)]
        (when (not= vx-tag vy-tag)
          (throw (ex-info (str "hydra.lib.math." op-name ": float operands differ in precision") {})))
        (let [r (float-op (second vx) (second vy))]
          (list :literal (list :float (list vx-tag (if (= vx-tag :float32) (to-float32 r) (double r)))))))
      :else (throw (ex-info (str "hydra.lib.math." op-name ": operand is not numeric") {})))))

(defn- numeric-unary-term [op-name int-op float-op x]
  (let [lx (numeric-literal op-name x)
        lx-kind (first lx)]
    (cond
      (= lx-kind :integer)
      (let [vx (second lx) vx-tag (first vx)]
        (list :literal (list :integer (list vx-tag (wrap-int vx-tag (int-op (second vx)))))))
      (= lx-kind :float)
      (let [vx (second lx) vx-tag (first vx)
            r (float-op (second vx))]
        (list :literal (list :float (list vx-tag (if (= vx-tag :float32) (to-float32 r) (double r))))))
      :else (throw (ex-info (str "hydra.lib.math." op-name ": operand is not numeric") {})))))

(defn- numeric-binary [op-name int-op float-op]
  (fn [x] (fn [y]
    (if (sequential? x)
      (numeric-binary-term op-name int-op float-op x y)
      (int-op x y)))))

(defn- numeric-unary [op-name int-op float-op]
  (fn [x]
    (if (sequential? x)
      (numeric-unary-term op-name int-op float-op x)
      (int-op x))))

;; --- Constraint-polymorphic ('integral') dispatch for div/mod/rem/even/odd ---
;;
;; div/mod are floor-based (sign follows the divisor); rem is truncated (sign follows the
;; dividend) -- mirroring the Haskell/Java/Python/Scala/TypeScript hosts' div/mod vs rem split.
;; All three guard the zero-divisor case (returning :none) before computing. The (minBound, -1)
;; boundary needs an explicit wrap-to-minBound on div only; mod/rem have no overflow there.
;; Both call contracts (Term-path vs native-value path) are supported, same dispatch-by-shape
;; convention as numeric-binary/numeric-unary above.

(defn- floor-div [a b]
  (let [q (quot a b)]
    (if (and (not= (rem a b) 0) (not= (< a 0) (< b 0))) (dec q) q)))

(defn- floor-mod [a b]
  (let [r (rem a b)]
    (if (and (not= r 0) (not= (< r 0) (< b 0))) (+ r b) r)))

(defn- integral-literal [op-name t]
  (if (and (sequential? t) (= (first t) :literal) (= (first (second t)) :integer))
    (second (second t))
    (throw (ex-info (str "hydra.lib.math." op-name ": expected an integer literal term") {}))))

(defn- integral-binary-term [op-name int-op wrap-min-boundary-on-div x y]
  (let [vx (integral-literal op-name x)
        vy (integral-literal op-name y)
        vx-tag (first vx) vy-tag (first vy)]
    (when (not= vx-tag vy-tag)
      (throw (ex-info (str "hydra.lib.math." op-name ": integer operands differ in precision") {})))
    (let [a (second vx) b (second vy)]
      (if (= b 0)
        (list :none)
        (let [signed (contains? int-width-bits vx-tag)
              r (if (and wrap-min-boundary-on-div signed (not= vx-tag :bigint))
                  (let [bits (int-width-bits vx-tag)
                        min-bound (- (bigint (.shiftLeft (biginteger 1) (dec bits))))]
                    (if (and (= a min-bound) (= b -1)) min-bound (int-op a b)))
                  (int-op a b))]
          (list :given (list :literal (list :integer (list vx-tag (wrap-int vx-tag r))))))))))

(defn- integral-binary-native [op-name int-op wrap-min-boundary-on-div a b]
  (if (= b 0)
    (list :none)
    (list :given (int-op a b))))

(defn- integral-binary [op-name int-op wrap-min-boundary-on-div]
  (fn [x] (fn [y]
    (if (sequential? x)
      (integral-binary-term op-name int-op wrap-min-boundary-on-div x y)
      (integral-binary-native op-name int-op wrap-min-boundary-on-div x y)))))

(defn- integral-to-bigint [v]
  (let [tag (first v) n (second v)]
    (if (= tag :uint8) (bit-and n 0xff) n)))

(defn- even-or-odd [op-name want-even]
  (fn [x]
    (let [n (if (sequential? x) (integral-to-bigint (integral-literal op-name x)) x)]
      (= (even? n) want-even))))

;; --- Constraint-polymorphic ('fractional') dispatch for divide ---
;;
;; float32/float64 only, IEEE-total (division by zero yields ±Infinity/NaN, not an exception --
;; the JVM's own `/` on doubles/floats already gives these sentinels for free).

(defn- divide-term [x y]
  (let [lx (numeric-literal "divide" x)
        ly (numeric-literal "divide" y)]
    (when (or (not= (first lx) :float) (not= (first ly) :float))
      (throw (ex-info "hydra.lib.math.divide: operands are not the same fractional kind" {})))
    (let [vx (second lx) vy (second ly)
          vx-tag (first vx) vy-tag (first vy)]
      (when (not= vx-tag vy-tag)
        (throw (ex-info "hydra.lib.math.divide: float operands differ in precision" {})))
      (let [r (/ (double (second vx)) (double (second vy)))]
        (list :literal (list :float (list vx-tag (if (= vx-tag :float32) (to-float32 r) r))))))))

(defn- divide-dispatch [x]
  (fn [y]
    (if (sequential? x)
      (divide-term x y)
      (/ (double x) (double y)))))

;; abs :: numeric x => x -> x
;; Uses -' (auto-promoting subtract, like negate below) rather than unchecked -: at
;; Long/MIN_VALUE, -a overflows a primitive long (Math/negateExact throws "long overflow"); -'
;; promotes to BigInteger instead, which wrap-int then correctly narrows back to minBound.
(def hydra_overlay_clojure_lib_math_abs
  "Constraint-polymorphic absolute value over any type with a 'numeric' instance."
  (numeric-unary "abs" (fn [a] (if (< a 0) (-' a) a)) (fn [a] (Math/abs (double a)))))

;; acos :: Double -> Double
(def hydra_overlay_clojure_lib_math_acos
  "Return the arc cosine of x in radians."
  (fn [x] (Math/acos x)))

;; acosh :: Double -> Double
(def hydra_overlay_clojure_lib_math_acosh
  "Return the inverse hyperbolic cosine of x."
  (fn [x] (Math/log (+ x (Math/sqrt (- (* x x) 1.0))))))

;; add :: numeric x => x -> x -> x
(def hydra_overlay_clojure_lib_math_add
  "Constraint-polymorphic addition over any type with a 'numeric' instance."
  (numeric-binary "add" +' +))

;; addFloat64 :: Double -> Double -> Double
(def hydra_overlay_clojure_lib_math_add_float64
  "Add two Float64 numbers."
  (fn [a] (fn [b] (+ (double a) (double b)))))

;; asin :: Double -> Double
(def hydra_overlay_clojure_lib_math_asin
  "Return the arc sine of x in radians."
  (fn [x] (Math/asin x)))

;; asinh :: Double -> Double
(def hydra_overlay_clojure_lib_math_asinh
  "Return the inverse hyperbolic sine of x."
  ;; Special-case infinities: asinh(±Inf) = ±Inf (naive formula gives NaN for -Inf).
  (fn [x]
    (if (Double/isInfinite x)
      x
      (Math/log (+ x (Math/sqrt (+ (* x x) 1.0)))))))

;; atan :: Double -> Double
(def hydra_overlay_clojure_lib_math_atan
  "Return the arc tangent of x in radians."
  (fn [x] (Math/atan x)))

;; atan2 :: Double -> Double -> Double
(def hydra_overlay_clojure_lib_math_atan2
  "Return the arc tangent of y/x in radians, using signs to determine quadrant."
  ;; Match Haskell: atan2 returns NaN when both arguments are infinite
  ;; (Java's Math.atan2 returns ±pi/4 or ±3pi/4 in these cases).
  (fn [y] (fn [x]
    (if (and (Double/isInfinite y) (Double/isInfinite x))
      Double/NaN
      (Math/atan2 y x)))))

;; atanh :: Double -> Double
(def hydra_overlay_clojure_lib_math_atanh
  "Return the inverse hyperbolic tangent of x."
  (fn [x] (* 0.5 (Math/log (/ (+ 1.0 x) (- 1.0 x))))))

;; ceiling :: Double -> Double
;; DIVERGENCE FROM HASKELL: Hydra returns a float, not an integer, so that
;; NaN/Inf propagate naturally per IEEE 754.
(def hydra_overlay_clojure_lib_math_ceiling
  "Return the ceiling of x as a float."
  (fn [x]
    (if (or (Double/isNaN x) (Double/isInfinite x))
      x
      (Math/ceil x))))

;; cos :: Double -> Double
(def hydra_overlay_clojure_lib_math_cos
  "Return the cosine of x radians."
  (fn [x] (Math/cos x)))

;; cosh :: Double -> Double
(def hydra_overlay_clojure_lib_math_cosh
  "Return the hyperbolic cosine of x."
  (fn [x] (Math/cosh x)))

;; e :: Double
(def hydra_overlay_clojure_lib_math_e
  "Euler's number (e = 2.71828...)."
  Math/E)

;; even :: integral x => x -> Bool
(def hydra_overlay_clojure_lib_math_even
  "Check if an integer is even (constraint-polymorphic over any 'integral' type)."
  (even-or-odd "even" true))

;; exp :: Double -> Double
(def hydra_overlay_clojure_lib_math_exp
  "Return e raised to the power x."
  (fn [x] (Math/exp x)))

;; floor :: Double -> Double
;; DIVERGENCE FROM HASKELL: returns a float, not an integer (see ceiling).
(def hydra_overlay_clojure_lib_math_floor
  "Return the floor of x as a float."
  (fn [x]
    (if (or (Double/isNaN x) (Double/isInfinite x))
      x
      (Math/floor x))))

;; log :: Double -> Double
(def hydra_overlay_clojure_lib_math_log
  "Return the natural logarithm of x."
  (fn [x] (Math/log x)))

;; logBase :: Double -> Double -> Double
(def hydra_overlay_clojure_lib_math_logBase
  "Return the logarithm of x to the given base."
  (fn [base] (fn [x] (/ (Math/log x) (Math/log base)))))
(def hydra_overlay_clojure_lib_math_log_base
  "Return the logarithm of x to the given base."
  hydra_overlay_clojure_lib_math_logBase)

;; div :: integral x => x -> x -> optional x
(def hydra_overlay_clojure_lib_math_div
  "Constraint-polymorphic floor division over any type with an 'integral' instance."
  (integral-binary "div" floor-div true))

;; mod :: integral x => x -> x -> optional x
(def hydra_overlay_clojure_lib_math_mod
  "Constraint-polymorphic floor modulus over any type with an 'integral' instance."
  (integral-binary "mod" floor-mod false))

;; rem :: integral x => x -> x -> optional x
(def hydra_overlay_clojure_lib_math_rem
  "Constraint-polymorphic truncated remainder over any type with an 'integral' instance."
  (integral-binary "rem" rem false))

;; divide :: fractional x => x -> x -> x
(def hydra_overlay_clojure_lib_math_divide
  "Constraint-polymorphic IEEE-total division over any type with a 'fractional' instance."
  divide-dispatch)

;; mul :: numeric x => x -> x -> x
(def hydra_overlay_clojure_lib_math_mul
  "Constraint-polymorphic multiplication over any type with a 'numeric' instance."
  (numeric-binary "mul" *' *))

;; mulFloat64 :: Double -> Double -> Double
(def hydra_overlay_clojure_lib_math_mul_float64
  "Multiply two Float64 numbers."
  (fn [a] (fn [b] (* (double a) (double b)))))

;; negate :: numeric x => x -> x
(def hydra_overlay_clojure_lib_math_negate
  "Constraint-polymorphic negation over any type with a 'numeric' instance."
  (numeric-unary "negate" -' -))

;; negateFloat64 :: Double -> Double
(def hydra_overlay_clojure_lib_math_negate_float64
  "Negate a Float64 number."
  (fn [n] (- (double n))))

;; odd :: integral x => x -> Bool
(def hydra_overlay_clojure_lib_math_odd
  "Check if an integer is odd (constraint-polymorphic over any 'integral' type)."
  (even-or-odd "odd" false))

;; pi :: Double
(def hydra_overlay_clojure_lib_math_pi
  "Pi (= 3.14159...)."
  Math/PI)

;; pow :: Double -> Double -> Double
(def hydra_overlay_clojure_lib_math_pow
  "Return x raised to the power y."
  (fn [base] (fn [exp_] (Math/pow base exp_))))

;; range :: Int -> Int -> [Int]  (half-open, [start, end))
(def hydra_overlay_clojure_lib_math_range
  "Generate a half-open range of values from start (inclusive) to end (exclusive)."
  (fn [start_] (fn [end_]
    (range start_ end_))))

;; round :: Double -> Double (Haskell-style round half to even)
;; DIVERGENCE FROM HASKELL: returns a float, not an integer (see ceiling).
(def hydra_overlay_clojure_lib_math_round
  "Return x rounded to the nearest integer, as a float."
  (fn [x]
    (if (or (Double/isNaN x) (Double/isInfinite x))
      x
      (double (.setScale (BigDecimal/valueOf (double x)) 0 java.math.RoundingMode/HALF_EVEN)))))

;; roundFloat32 :: Int -> Float -> Float
;; Returns NaN/Inf inputs unchanged (no rounding is possible).
(def hydra_overlay_clojure_lib_math_round_float32
  "Round a float32 to n significant digits."
  (fn [n] (fn [x]
    (let [dx (double x)]
      (cond
        (Double/isNaN dx) x
        (Double/isInfinite dx) x
        (== dx 0.0) (float 0.0)
        :else
          (let [factor (Math/pow 10.0 (- n 1 (Math/floor (Math/log10 (Math/abs dx)))))]
            (float (/ (Math/round (* dx factor)) factor))))))))

;; roundFloat64 :: Int -> Double -> Double
;; Returns NaN/Inf inputs unchanged (no rounding is possible).
(def hydra_overlay_clojure_lib_math_round_float64
  "Round a float64 to n significant digits."
  (fn [n] (fn [x]
    (cond
      (Double/isNaN x) x
      (Double/isInfinite x) x
      (== x 0.0) 0.0
      :else
        (let [factor (Math/pow 10.0 (- n 1 (Math/floor (Math/log10 (Math/abs x)))))]
          (/ (Math/round (* x factor)) factor))))))

;; signum :: numeric x => x -> x
(def hydra_overlay_clojure_lib_math_signum
  "Constraint-polymorphic sign function over any type with a 'numeric' instance.
  Returns -1/0/1 for integers; for floats, preserves the sign of zero (signum(-0.0) = -0.0)
  and propagates NaN, matching Math/signum."
  (numeric-unary "signum" (fn [a] (cond (pos? a) 1 (neg? a) -1 :else 0)) (fn [a] (Math/signum (double a)))))

;; sin :: Double -> Double
(def hydra_overlay_clojure_lib_math_sin
  "Return the sine of x radians."
  (fn [x] (Math/sin x)))

;; sinh :: Double -> Double
(def hydra_overlay_clojure_lib_math_sinh
  "Return the hyperbolic sine of x."
  (fn [x] (Math/sinh x)))

;; sqrt :: Double -> Double
(def hydra_overlay_clojure_lib_math_sqrt
  "Return the square root of x."
  (fn [x] (Math/sqrt x)))

;; sub :: numeric x => x -> x -> x
(def hydra_overlay_clojure_lib_math_sub
  "Constraint-polymorphic subtraction over any type with a 'numeric' instance."
  (numeric-binary "sub" -' -))

;; subFloat64 :: Double -> Double -> Double
(def hydra_overlay_clojure_lib_math_sub_float64
  "Subtract two Float64 numbers."
  (fn [a] (fn [b] (- (double a) (double b)))))


;; tan :: Double -> Double
(def hydra_overlay_clojure_lib_math_tan
  "Return the tangent of x radians."
  (fn [x] (Math/tan x)))

;; tanh :: Double -> Double
(def hydra_overlay_clojure_lib_math_tanh
  "Return the hyperbolic tangent of x."
  (fn [x] (Math/tanh x)))

;; truncate :: Double -> Double
;; DIVERGENCE FROM HASKELL: returns a float, not an integer (see ceiling).
(def hydra_overlay_clojure_lib_math_truncate
  "Return x truncated (towards zero), as a float."
  (fn [x]
    (if (or (Double/isNaN x) (Double/isInfinite x))
      x
      (double (long x)))))
