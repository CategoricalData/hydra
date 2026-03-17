(ns hydra.lib.math)

;; abs :: Int -> Int
(def hydra_lib_math_abs
  (fn [n] (Math/abs (long n))))

;; add :: Int -> Int -> Int
(def hydra_lib_math_add
  (fn [a] (fn [b] (+ a b))))

;; div :: Int -> Int -> Int  (floor division)
(def hydra_lib_math_div
  (fn [a] (fn [b] (Math/floorDiv (long a) (long b)))))

;; even :: Int -> Bool
(def hydra_lib_math_even
  (fn [n] (even? n)))

;; max :: Int -> Int -> Int
(def hydra_lib_math_max
  (fn [a] (fn [b] (max a b))))

;; min :: Int -> Int -> Int
(def hydra_lib_math_min
  (fn [a] (fn [b] (min a b))))

;; mod :: Int -> Int -> Int  (floor mod)
(def hydra_lib_math_mod
  (fn [a] (fn [b] (Math/floorMod (long a) (long b)))))

;; mul :: Int -> Int -> Int
(def hydra_lib_math_mul
  (fn [a] (fn [b] (* a b))))

;; negate :: Int -> Int
(def hydra_lib_math_negate
  (fn [n] (- n)))

;; odd :: Int -> Bool
(def hydra_lib_math_odd
  (fn [n] (odd? n)))

;; pred :: Int -> Int
(def hydra_lib_math_pred
  (fn [n] (dec n)))

;; range :: Int -> Int -> [Int]  (inclusive both ends)
(def hydra_lib_math_range
  (fn [start_] (fn [end_]
    (range start_ (inc end_)))))

;; rem :: Int -> Int -> Int  (truncating remainder)
(def hydra_lib_math_rem
  (fn [a] (fn [b] (rem a b))))

;; signum :: Int -> Int
(def hydra_lib_math_signum
  (fn [n] (cond (pos? n) 1 (neg? n) -1 :else 0)))

;; sub :: Int -> Int -> Int
(def hydra_lib_math_sub
  (fn [a] (fn [b] (- a b))))

;; succ :: Int -> Int
(def hydra_lib_math_succ
  (fn [n] (inc n)))

;; e :: Double
(def hydra_lib_math_e Math/E)

;; pi :: Double
(def hydra_lib_math_pi Math/PI)

;; sin :: Double -> Double
(def hydra_lib_math_sin (fn [x] (Math/sin x)))

;; cos :: Double -> Double
(def hydra_lib_math_cos (fn [x] (Math/cos x)))

;; tan :: Double -> Double
(def hydra_lib_math_tan (fn [x] (Math/tan x)))

;; asin :: Double -> Double
(def hydra_lib_math_asin (fn [x] (Math/asin x)))

;; acos :: Double -> Double
(def hydra_lib_math_acos (fn [x] (Math/acos x)))

;; atan :: Double -> Double
(def hydra_lib_math_atan (fn [x] (Math/atan x)))

;; atan2 :: Double -> Double -> Double
(def hydra_lib_math_atan2 (fn [y] (fn [x] (Math/atan2 y x))))

;; sinh :: Double -> Double
(def hydra_lib_math_sinh (fn [x] (Math/sinh x)))

;; cosh :: Double -> Double
(def hydra_lib_math_cosh (fn [x] (Math/cosh x)))

;; tanh :: Double -> Double
(def hydra_lib_math_tanh (fn [x] (Math/tanh x)))

;; asinh :: Double -> Double
(def hydra_lib_math_asinh (fn [x] (Math/log (+ x (Math/sqrt (+ (* x x) 1.0))))))

;; acosh :: Double -> Double
(def hydra_lib_math_acosh (fn [x] (Math/log (+ x (Math/sqrt (- (* x x) 1.0))))))

;; atanh :: Double -> Double
(def hydra_lib_math_atanh (fn [x] (* 0.5 (Math/log (/ (+ 1.0 x) (- 1.0 x))))))

;; exp :: Double -> Double
(def hydra_lib_math_exp (fn [x] (Math/exp x)))

;; log :: Double -> Double
(def hydra_lib_math_log (fn [x] (Math/log x)))

;; logBase :: Double -> Double -> Double
(def hydra_lib_math_logBase (fn [base] (fn [x] (/ (Math/log x) (Math/log base)))))
(def hydra_lib_math_log_base hydra_lib_math_logBase)

;; pow :: Double -> Double -> Double
(def hydra_lib_math_pow (fn [base] (fn [exp_] (Math/pow base exp_))))

;; sqrt :: Double -> Double
(def hydra_lib_math_sqrt (fn [x] (Math/sqrt x)))

;; ceiling :: Double -> BigInt
(def hydra_lib_math_ceiling (fn [x] (long (Math/ceil x))))

;; floor :: Double -> BigInt
(def hydra_lib_math_floor (fn [x] (long (Math/floor x))))

;; round :: Double -> BigInt (Haskell-style round half to even)
(def hydra_lib_math_round
  (fn [x]
    (long (.setScale (BigDecimal/valueOf (double x)) 0 java.math.RoundingMode/HALF_EVEN))))

;; truncate :: Double -> BigInt
(def hydra_lib_math_truncate (fn [x] (long x)))

;; roundFloat64 :: Int -> Double -> Double
(def hydra_lib_math_round_float64
  (fn [n] (fn [x]
    (if (== x 0.0) 0.0
      (let [factor (Math/pow 10.0 (- n 1 (Math/floor (Math/log10 (Math/abs x)))))]
        (/ (Math/round (* x factor)) factor))))))

;; roundFloat32 :: Int -> Float -> Float
(def hydra_lib_math_round_float32
  (fn [n] (fn [x]
    (if (== x 0.0) (float 0.0)
      (let [factor (Math/pow 10.0 (- n 1 (Math/floor (Math/log10 (Math/abs (double x))))))]
        (float (/ (Math/round (* (double x) factor)) factor)))))))

;; roundBigfloat :: Int -> Double -> Double  (alias for roundFloat64)
(def hydra_lib_math_round_bigfloat hydra_lib_math_round_float64)
