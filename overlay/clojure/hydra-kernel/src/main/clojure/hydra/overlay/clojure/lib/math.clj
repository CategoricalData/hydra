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
          (list :literal (list :float (list vx-tag (if (= vx-tag :float32) (float r) (double r)))))))
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
        (list :literal (list :float (list vx-tag (if (= vx-tag :float32) (float r) (double r))))))
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

;; abs :: Int -> Int
(def hydra_lib_math_abs
  "Return the absolute value."
  (fn [n] (Math/abs (long n))))

;; acos :: Double -> Double
(def hydra_lib_math_acos
  "Return the arc cosine of x in radians."
  (fn [x] (Math/acos x)))

;; acosh :: Double -> Double
(def hydra_lib_math_acosh
  "Return the inverse hyperbolic cosine of x."
  (fn [x] (Math/log (+ x (Math/sqrt (- (* x x) 1.0))))))

;; add :: numeric x => x -> x -> x
(def hydra_lib_math_add
  "Constraint-polymorphic addition over any type with a 'numeric' instance."
  (numeric-binary "add" +' +))

;; addFloat64 :: Double -> Double -> Double
(def hydra_lib_math_add_float64
  "Add two Float64 numbers."
  (fn [a] (fn [b] (+ (double a) (double b)))))

;; asin :: Double -> Double
(def hydra_lib_math_asin
  "Return the arc sine of x in radians."
  (fn [x] (Math/asin x)))

;; asinh :: Double -> Double
(def hydra_lib_math_asinh
  "Return the inverse hyperbolic sine of x."
  ;; Special-case infinities: asinh(±Inf) = ±Inf (naive formula gives NaN for -Inf).
  (fn [x]
    (if (Double/isInfinite x)
      x
      (Math/log (+ x (Math/sqrt (+ (* x x) 1.0)))))))

;; atan :: Double -> Double
(def hydra_lib_math_atan
  "Return the arc tangent of x in radians."
  (fn [x] (Math/atan x)))

;; atan2 :: Double -> Double -> Double
(def hydra_lib_math_atan2
  "Return the arc tangent of y/x in radians, using signs to determine quadrant."
  ;; Match Haskell: atan2 returns NaN when both arguments are infinite
  ;; (Java's Math.atan2 returns ±pi/4 or ±3pi/4 in these cases).
  (fn [y] (fn [x]
    (if (and (Double/isInfinite y) (Double/isInfinite x))
      Double/NaN
      (Math/atan2 y x)))))

;; atanh :: Double -> Double
(def hydra_lib_math_atanh
  "Return the inverse hyperbolic tangent of x."
  (fn [x] (* 0.5 (Math/log (/ (+ 1.0 x) (- 1.0 x))))))

;; ceiling :: Double -> Double
;; DIVERGENCE FROM HASKELL: Hydra returns a float, not an integer, so that
;; NaN/Inf propagate naturally per IEEE 754.
(def hydra_lib_math_ceiling
  "Return the ceiling of x as a float."
  (fn [x]
    (if (or (Double/isNaN x) (Double/isInfinite x))
      x
      (Math/ceil x))))

;; cos :: Double -> Double
(def hydra_lib_math_cos
  "Return the cosine of x radians."
  (fn [x] (Math/cos x)))

;; cosh :: Double -> Double
(def hydra_lib_math_cosh
  "Return the hyperbolic cosine of x."
  (fn [x] (Math/cosh x)))

;; e :: Double
(def hydra_lib_math_e
  "Euler's number (e = 2.71828...)."
  Math/E)

;; even :: Int -> Bool
(def hydra_lib_math_even
  "Check if an integer is even."
  (fn [n] (even? n)))

;; exp :: Double -> Double
(def hydra_lib_math_exp
  "Return e raised to the power x."
  (fn [x] (Math/exp x)))

;; floor :: Double -> Double
;; DIVERGENCE FROM HASKELL: returns a float, not an integer (see ceiling).
(def hydra_lib_math_floor
  "Return the floor of x as a float."
  (fn [x]
    (if (or (Double/isNaN x) (Double/isInfinite x))
      x
      (Math/floor x))))

;; log :: Double -> Double
(def hydra_lib_math_log
  "Return the natural logarithm of x."
  (fn [x] (Math/log x)))

;; logBase :: Double -> Double -> Double
(def hydra_lib_math_logBase
  "Return the logarithm of x to the given base."
  (fn [base] (fn [x] (/ (Math/log x) (Math/log base)))))
(def hydra_lib_math_log_base
  "Return the logarithm of x to the given base."
  hydra_lib_math_logBase)

;; div :: Int -> Int -> Maybe Int
(def hydra_lib_math_div
  "Divide two integers, returning Nothing if the divisor is zero."
  (fn [a] (fn [b]
    (if (= b 0)
      (list :none)
      (list :given (Math/floorDiv (long a) (long b)))))))

;; mod :: Int -> Int -> Maybe Int
(def hydra_lib_math_mod
  "Mathematical modulo, returning Nothing if the divisor is zero."
  (fn [a] (fn [b]
    (if (= b 0)
      (list :none)
      (list :given (Math/floorMod (long a) (long b)))))))

;; rem :: Int -> Int -> Maybe Int
(def hydra_lib_math_rem
  "Integer remainder, returning Nothing if the divisor is zero."
  (fn [a] (fn [b]
    (if (= b 0)
      (list :none)
      (list :given (rem a b))))))

;; mul :: numeric x => x -> x -> x
(def hydra_lib_math_mul
  "Constraint-polymorphic multiplication over any type with a 'numeric' instance."
  (numeric-binary "mul" *' *))

;; mulFloat64 :: Double -> Double -> Double
(def hydra_lib_math_mul_float64
  "Multiply two Float64 numbers."
  (fn [a] (fn [b] (* (double a) (double b)))))

;; negate :: numeric x => x -> x
(def hydra_lib_math_negate
  "Constraint-polymorphic negation over any type with a 'numeric' instance."
  (numeric-unary "negate" -' -))

;; negateFloat64 :: Double -> Double
(def hydra_lib_math_negate_float64
  "Negate a Float64 number."
  (fn [n] (- (double n))))

;; odd :: Int -> Bool
(def hydra_lib_math_odd
  "Check if an integer is odd."
  (fn [n] (odd? n)))

;; pi :: Double
(def hydra_lib_math_pi
  "Pi (= 3.14159...)."
  Math/PI)

;; pow :: Double -> Double -> Double
(def hydra_lib_math_pow
  "Return x raised to the power y."
  (fn [base] (fn [exp_] (Math/pow base exp_))))

;; range :: Int -> Int -> [Int]  (inclusive both ends)
(def hydra_lib_math_range
  "Generate a range of values from start to end (inclusive)."
  (fn [start_] (fn [end_]
    (range start_ (inc end_)))))

;; round :: Double -> Double (Haskell-style round half to even)
;; DIVERGENCE FROM HASKELL: returns a float, not an integer (see ceiling).
(def hydra_lib_math_round
  "Return x rounded to the nearest integer, as a float."
  (fn [x]
    (if (or (Double/isNaN x) (Double/isInfinite x))
      x
      (double (.setScale (BigDecimal/valueOf (double x)) 0 java.math.RoundingMode/HALF_EVEN)))))

;; roundFloat32 :: Int -> Float -> Float
;; Returns NaN/Inf inputs unchanged (no rounding is possible).
(def hydra_lib_math_round_float32
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
(def hydra_lib_math_round_float64
  "Round a float64 to n significant digits."
  (fn [n] (fn [x]
    (cond
      (Double/isNaN x) x
      (Double/isInfinite x) x
      (== x 0.0) 0.0
      :else
        (let [factor (Math/pow 10.0 (- n 1 (Math/floor (Math/log10 (Math/abs x)))))]
          (/ (Math/round (* x factor)) factor))))))

;; signum :: Int -> Int
(def hydra_lib_math_signum
  "Return the sign of a number (-1, 0, or 1)."
  (fn [n] (cond (pos? n) 1 (neg? n) -1 :else 0)))

;; sin :: Double -> Double
(def hydra_lib_math_sin
  "Return the sine of x radians."
  (fn [x] (Math/sin x)))

;; sinh :: Double -> Double
(def hydra_lib_math_sinh
  "Return the hyperbolic sine of x."
  (fn [x] (Math/sinh x)))

;; sqrt :: Double -> Double
(def hydra_lib_math_sqrt
  "Return the square root of x."
  (fn [x] (Math/sqrt x)))

;; sub :: numeric x => x -> x -> x
(def hydra_lib_math_sub
  "Constraint-polymorphic subtraction over any type with a 'numeric' instance."
  (numeric-binary "sub" -' -))

;; subFloat64 :: Double -> Double -> Double
(def hydra_lib_math_sub_float64
  "Subtract two Float64 numbers."
  (fn [a] (fn [b] (- (double a) (double b)))))


;; tan :: Double -> Double
(def hydra_lib_math_tan
  "Return the tangent of x radians."
  (fn [x] (Math/tan x)))

;; tanh :: Double -> Double
(def hydra_lib_math_tanh
  "Return the hyperbolic tangent of x."
  (fn [x] (Math/tanh x)))

;; truncate :: Double -> Double
;; DIVERGENCE FROM HASKELL: returns a float, not an integer (see ceiling).
(def hydra_lib_math_truncate
  "Return x truncated (towards zero), as a float."
  (fn [x]
    (if (or (Double/isNaN x) (Double/isInfinite x))
      x
      (double (long x)))))
