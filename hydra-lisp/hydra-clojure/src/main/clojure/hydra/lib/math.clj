(ns hydra.lib.math)

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

;; add :: Int -> Int -> Int
(def hydra_lib_math_add
  "Add two numbers."
  (fn [a] (fn [b] (+ a b))))

;; asin :: Double -> Double
(def hydra_lib_math_asin
  "Return the arc sine of x in radians."
  (fn [x] (Math/asin x)))

;; asinh :: Double -> Double
(def hydra_lib_math_asinh
  "Return the inverse hyperbolic sine of x."
  (fn [x] (Math/log (+ x (Math/sqrt (+ (* x x) 1.0))))))

;; atan :: Double -> Double
(def hydra_lib_math_atan
  "Return the arc tangent of x in radians."
  (fn [x] (Math/atan x)))

;; atan2 :: Double -> Double -> Double
(def hydra_lib_math_atan2
  "Return the arc tangent of y/x in radians, using signs to determine quadrant."
  (fn [y] (fn [x] (Math/atan2 y x))))

;; atanh :: Double -> Double
(def hydra_lib_math_atanh
  "Return the inverse hyperbolic tangent of x."
  (fn [x] (* 0.5 (Math/log (/ (+ 1.0 x) (- 1.0 x))))))

;; ceiling :: Double -> BigInt
(def hydra_lib_math_ceiling
  "Return the ceiling of x as an integer."
  (fn [x] (long (Math/ceil x))))

;; cos :: Double -> Double
(def hydra_lib_math_cos
  "Return the cosine of x radians."
  (fn [x] (Math/cos x)))

;; cosh :: Double -> Double
(def hydra_lib_math_cosh
  "Return the hyperbolic cosine of x."
  (fn [x] (Math/cosh x)))

;; div :: Int -> Int -> Int  (floor division)
(def hydra_lib_math_div
  "Divide two integers using integer division."
  (fn [a] (fn [b] (Math/floorDiv (long a) (long b)))))

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

;; floor :: Double -> BigInt
(def hydra_lib_math_floor
  "Return the floor of x as an integer."
  (fn [x] (long (Math/floor x))))

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

;; max :: Int -> Int -> Int
(def hydra_lib_math_max
  "Return the maximum of two values."
  (fn [a] (fn [b] (max a b))))

;; maybe_div :: Int -> Int -> Maybe Int
(def hydra_lib_math_maybe_div
  "Divide two integers, returning Nothing if the divisor is zero."
  (fn [a] (fn [b]
    (if (= b 0)
      (list :nothing)
      (list :just (Math/floorDiv (long a) (long b)))))))

;; maybe_mod :: Int -> Int -> Maybe Int
(def hydra_lib_math_maybe_mod
  "Mathematical modulo, returning Nothing if the divisor is zero."
  (fn [a] (fn [b]
    (if (= b 0)
      (list :nothing)
      (list :just (Math/floorMod (long a) (long b)))))))

;; maybe_pred :: Int -> Maybe Int
(def hydra_lib_math_maybe_pred
  "Return the predecessor, returning Nothing if x is minBound."
  (fn [n]
    (if (= n -2147483648)
      (list :nothing)
      (list :just (dec n)))))

;; maybe_rem :: Int -> Int -> Maybe Int
(def hydra_lib_math_maybe_rem
  "Integer remainder, returning Nothing if the divisor is zero."
  (fn [a] (fn [b]
    (if (= b 0)
      (list :nothing)
      (list :just (rem a b))))))

;; maybe_succ :: Int -> Maybe Int
(def hydra_lib_math_maybe_succ
  "Return the successor, returning Nothing if x is maxBound."
  (fn [n]
    (if (= n 2147483647)
      (list :nothing)
      (list :just (inc n)))))

;; min :: Int -> Int -> Int
(def hydra_lib_math_min
  "Return the minimum of two values."
  (fn [a] (fn [b] (min a b))))

;; mod :: Int -> Int -> Int  (floor mod)
(def hydra_lib_math_mod
  "Mathematical modulo."
  (fn [a] (fn [b] (Math/floorMod (long a) (long b)))))

;; mul :: Int -> Int -> Int
(def hydra_lib_math_mul
  "Multiply two numbers."
  (fn [a] (fn [b] (* a b))))

;; negate :: Int -> Int
(def hydra_lib_math_negate
  "Negate a number."
  (fn [n] (- n)))

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

;; pred :: Int -> Int
(def hydra_lib_math_pred
  "Return the predecessor (x - 1)."
  (fn [n] (dec n)))

;; range :: Int -> Int -> [Int]  (inclusive both ends)
(def hydra_lib_math_range
  "Generate a range of values from start to end (inclusive)."
  (fn [start_] (fn [end_]
    (range start_ (inc end_)))))

;; rem :: Int -> Int -> Int  (truncating remainder)
(def hydra_lib_math_rem
  "Integer remainder."
  (fn [a] (fn [b] (rem a b))))

;; round :: Double -> BigInt (Haskell-style round half to even)
(def hydra_lib_math_round
  "Return x rounded to the nearest integer."
  (fn [x]
    (long (.setScale (BigDecimal/valueOf (double x)) 0 java.math.RoundingMode/HALF_EVEN))))

;; roundFloat32 :: Int -> Float -> Float
(def hydra_lib_math_round_float32
  "Round a float32 to n significant digits."
  (fn [n] (fn [x]
    (if (== x 0.0) (float 0.0)
      (let [factor (Math/pow 10.0 (- n 1 (Math/floor (Math/log10 (Math/abs (double x))))))]
        (float (/ (Math/round (* (double x) factor)) factor)))))))

;; roundFloat64 :: Int -> Double -> Double
(def hydra_lib_math_round_float64
  "Round a float64 to n significant digits."
  (fn [n] (fn [x]
    (if (== x 0.0) 0.0
      (let [factor (Math/pow 10.0 (- n 1 (Math/floor (Math/log10 (Math/abs x)))))]
        (/ (Math/round (* x factor)) factor))))))

;; roundBigfloat :: Int -> Double -> Double  (alias for roundFloat64)
(def hydra_lib_math_round_bigfloat
  "Round a bigfloat to n significant digits."
  hydra_lib_math_round_float64)

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

;; sub :: Int -> Int -> Int
(def hydra_lib_math_sub
  "Subtract two numbers."
  (fn [a] (fn [b] (- a b))))

;; succ :: Int -> Int
(def hydra_lib_math_succ
  "Return the successor (x + 1)."
  (fn [n] (inc n)))

;; tan :: Double -> Double
(def hydra_lib_math_tan
  "Return the tangent of x radians."
  (fn [x] (Math/tan x)))

;; tanh :: Double -> Double
(def hydra_lib_math_tanh
  "Return the hyperbolic tangent of x."
  (fn [x] (Math/tanh x)))

;; truncate :: Double -> BigInt
(def hydra_lib_math_truncate
  "Return x truncated to an integer (towards zero)."
  (fn [x] (long x)))
