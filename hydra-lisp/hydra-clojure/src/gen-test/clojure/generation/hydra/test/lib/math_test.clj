;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.math primitives

(ns generation.hydra.test.lib.math-test
  (:require [clojure.test :refer :all]))

;; abs

(deftest test-abs-negpositive

  (is (= 5

         (hydra_lib_math_abs 5))))

(deftest test-abs-negnegative

  (is (= 5

         (hydra_lib_math_abs -5))))

(deftest test-abs-negzero

  (is (= 0

         (hydra_lib_math_abs 0))))

;; add

(deftest test-add-negpositive-numbers

  (is (= 8

         ((hydra_lib_math_add 3) 5))))

(deftest test-add-negnegative-numbers

  (is (= -8

         ((hydra_lib_math_add -3) -5))))

(deftest test-add-negmixed-sign

  (is (= 7

         ((hydra_lib_math_add 10) -3))))

(deftest test-add-negwith-zero

  (is (= 42

         ((hydra_lib_math_add 42) 0))))

;; div

(deftest test-div-negexact-division

  (is (= 5

         ((hydra_lib_math_div 10) 2))))

(deftest test-div-negtruncates-toward-negative-infinity

  (is (= 3

         ((hydra_lib_math_div 10) 3))))

(deftest test-div-negnegative-dividend

  (is (= -4

         ((hydra_lib_math_div -10) 3))))

(deftest test-div-negnegative-divisor

  (is (= -4

         ((hydra_lib_math_div 10) -3))))

;; even

(deftest test-even-negeven-positive

  (is (= true

         (hydra_lib_math_even 4))))

(deftest test-even-negodd-positive

  (is (= false

         (hydra_lib_math_even 5))))

(deftest test-even-negeven-negative

  (is (= true

         (hydra_lib_math_even -4))))

(deftest test-even-negodd-negative

  (is (= false

         (hydra_lib_math_even -5))))

(deftest test-even-negzero

  (is (= true

         (hydra_lib_math_even 0))))

;; max

(deftest test-max-negfirst-is-larger

  (is (= 10

         ((hydra_lib_math_max 10) 5))))

(deftest test-max-negsecond-is-larger

  (is (= 10

         ((hydra_lib_math_max 5) 10))))

(deftest test-max-negequal-values

  (is (= 7

         ((hydra_lib_math_max 7) 7))))

(deftest test-max-negnegative-numbers

  (is (= -3

         ((hydra_lib_math_max -3) -5))))

(deftest test-max-negmixed-sign

  (is (= 5

         ((hydra_lib_math_max -5) 5))))

(deftest test-max-negwith-zero

  (is (= 42

         ((hydra_lib_math_max 0) 42))))

;; min

(deftest test-min-negfirst-is-smaller

  (is (= 5

         ((hydra_lib_math_min 5) 10))))

(deftest test-min-negsecond-is-smaller

  (is (= 5

         ((hydra_lib_math_min 10) 5))))

(deftest test-min-negequal-values

  (is (= 7

         ((hydra_lib_math_min 7) 7))))

(deftest test-min-negnegative-numbers

  (is (= -5

         ((hydra_lib_math_min -3) -5))))

(deftest test-min-negmixed-sign

  (is (= -5

         ((hydra_lib_math_min -5) 5))))

(deftest test-min-negwith-zero

  (is (= 0

         ((hydra_lib_math_min 0) 42))))

;; mod

(deftest test-mod-negbasic-modulo

  (is (= 1

         ((hydra_lib_math_mod 10) 3))))

(deftest test-mod-negexact-division

  (is (= 0

         ((hydra_lib_math_mod 10) 2))))

(deftest test-mod-negnegative-dividend

  (is (= 2

         ((hydra_lib_math_mod -10) 3))))

(deftest test-mod-negnegative-divisor

  (is (= -2

         ((hydra_lib_math_mod 10) -3))))

;; mul

(deftest test-mul-negpositive-numbers

  (is (= 15

         ((hydra_lib_math_mul 3) 5))))

(deftest test-mul-negnegative-numbers

  (is (= 15

         ((hydra_lib_math_mul -3) -5))))

(deftest test-mul-negmixed-sign

  (is (= -15

         ((hydra_lib_math_mul 3) -5))))

(deftest test-mul-negwith-zero

  (is (= 0

         ((hydra_lib_math_mul 42) 0))))

(deftest test-mul-negwith-one

  (is (= 42

         ((hydra_lib_math_mul 42) 1))))

;; negate

(deftest test-negate-negpositive

  (is (= -5

         (hydra_lib_math_negate 5))))

(deftest test-negate-negnegative

  (is (= 5

         (hydra_lib_math_negate -5))))

(deftest test-negate-negzero

  (is (= 0

         (hydra_lib_math_negate 0))))

;; odd

(deftest test-odd-negodd-positive

  (is (= true

         (hydra_lib_math_odd 5))))

(deftest test-odd-negeven-positive

  (is (= false

         (hydra_lib_math_odd 4))))

(deftest test-odd-negodd-negative

  (is (= true

         (hydra_lib_math_odd -5))))

(deftest test-odd-negeven-negative

  (is (= false

         (hydra_lib_math_odd -4))))

(deftest test-odd-negzero

  (is (= false

         (hydra_lib_math_odd 0))))

;; pred

(deftest test-pred-negpositive

  (is (= 4

         (hydra_lib_math_pred 5))))

(deftest test-pred-negzero

  (is (= -1

         (hydra_lib_math_pred 0))))

(deftest test-pred-negnegative

  (is (= -6

         (hydra_lib_math_pred -5))))

;; range

(deftest test-range-negascending-range

  (is (= (list 1 2 3 4 5)

         ((hydra_lib_math_range 1) 5))))

(deftest test-range-negsingle-element

  (is (= (list 5)

         ((hydra_lib_math_range 5) 5))))

(deftest test-range-negtwo-elements

  (is (= (list 3 4)

         ((hydra_lib_math_range 3) 4))))

(deftest test-range-negnegative-start

  (is (= (list -2 -1 0 1 2)

         ((hydra_lib_math_range -2) 2))))

;; rem

(deftest test-rem-negbasic-remainder

  (is (= 1

         ((hydra_lib_math_rem 10) 3))))

(deftest test-rem-negexact-division

  (is (= 0

         ((hydra_lib_math_rem 10) 2))))

(deftest test-rem-negnegative-dividend

  (is (= -1

         ((hydra_lib_math_rem -10) 3))))

(deftest test-rem-negnegative-divisor

  (is (= 1

         ((hydra_lib_math_rem 10) -3))))

;; signum

(deftest test-signum-negpositive

  (is (= 1

         (hydra_lib_math_signum 5))))

(deftest test-signum-negnegative

  (is (= -1

         (hydra_lib_math_signum -5))))

(deftest test-signum-negzero

  (is (= 0

         (hydra_lib_math_signum 0))))

;; sub

(deftest test-sub-negpositive-numbers

  (is (= 7

         ((hydra_lib_math_sub 10) 3))))

(deftest test-sub-negnegative-numbers

  (is (= -7

         ((hydra_lib_math_sub -10) -3))))

(deftest test-sub-negmixed-sign

  (is (= 13

         ((hydra_lib_math_sub 10) -3))))

(deftest test-sub-negwith-zero

  (is (= 42

         ((hydra_lib_math_sub 42) 0))))

;; succ

(deftest test-succ-negpositive

  (is (= 6

         (hydra_lib_math_succ 5))))

(deftest test-succ-negzero

  (is (= 1

         (hydra_lib_math_succ 0))))

(deftest test-succ-negnegative

  (is (= -4

         (hydra_lib_math_succ -5))))

;; e

(deftest test-e-negeuler-s-number

  (is (= 2.718281828459045

         hydra_lib_math_e)))

;; pi

(deftest test-pi-negpi-constant

  (is (= 3.141592653589793

         hydra_lib_math_pi)))

;; sin

(deftest test-sin-negsin-0

  (is (= 0.0

         (hydra_lib_math_sin 0.0))))

(deftest test-sin-negsin-pi-div2

  (is (= 1.0

         (hydra_lib_math_sin 1.5707963267948966))))

(deftest test-sin-negsin-pi

  (is (= 1.2246467991473532e-16

         (hydra_lib_math_sin 3.141592653589793))))

(deftest test-sin-negsin-1

  (is (= 0.841470984808

         ((hydra_lib_math_round_float64 12) (hydra_lib_math_sin 1.0)))))

(deftest test-sin-negsin-0-dot5

  (is (= 0.479425538604

         ((hydra_lib_math_round_float64 12) (hydra_lib_math_sin 0.5)))))

;; cos

(deftest test-cos-negcos-0

  (is (= 1.0

         (hydra_lib_math_cos 0.0))))

(deftest test-cos-negcos-pi-div2

  (is (= 6.123233995736766e-17

         (hydra_lib_math_cos 1.5707963267948966))))

(deftest test-cos-negcos-pi

  (is (= -1.0

         (hydra_lib_math_cos 3.141592653589793))))

(deftest test-cos-negcos-1

  (is (= 0.540302305868

         ((hydra_lib_math_round_float64 12) (hydra_lib_math_cos 1.0)))))

(deftest test-cos-negcos-0-dot5

  (is (= 0.87758256189

         ((hydra_lib_math_round_float64 12) (hydra_lib_math_cos 0.5)))))

;; tan

(deftest test-tan-negtan-0

  (is (= 0.0

         (hydra_lib_math_tan 0.0))))

(deftest test-tan-negtan-pi-div4

  (is (= 0.9999999999999999

         (hydra_lib_math_tan 0.7853981633974483))))

(deftest test-tan-negtan-1

  (is (= 1.55740772465

         ((hydra_lib_math_round_float64 12) (hydra_lib_math_tan 1.0)))))

(deftest test-tan-negtan-0-dot5

  (is (= 0.546302489844

         ((hydra_lib_math_round_float64 12) (hydra_lib_math_tan 0.5)))))

;; asin

(deftest test-asin-negasin-0

  (is (= 0.0

         (hydra_lib_math_asin 0.0))))

(deftest test-asin-negasin-1

  (is (= 1.5707963267948966

         (hydra_lib_math_asin 1.0))))

(deftest test-asin-negasin--neg1

  (is (= -1.5707963267948966

         (hydra_lib_math_asin -1.0))))

(deftest test-asin-negasin-0-dot5

  (is (= 0.523598775598

         ((hydra_lib_math_round_float64 12) (hydra_lib_math_asin 0.5)))))

;; acos

(deftest test-acos-negacos-1

  (is (= 0.0

         (hydra_lib_math_acos 1.0))))

(deftest test-acos-negacos-0

  (is (= 1.5707963267948966

         (hydra_lib_math_acos 0.0))))

(deftest test-acos-negacos--neg1

  (is (= 3.141592653589793

         (hydra_lib_math_acos -1.0))))

(deftest test-acos-negacos-0-dot5

  (is (= 1.0471975512

         ((hydra_lib_math_round_float64 12) (hydra_lib_math_acos 0.5)))))

;; atan

(deftest test-atan-negatan-0

  (is (= 0.0

         (hydra_lib_math_atan 0.0))))

(deftest test-atan-negatan-1

  (is (= 0.7853981633974483

         (hydra_lib_math_atan 1.0))))

(deftest test-atan-negatan-0-dot5

  (is (= 0.463647609001

         ((hydra_lib_math_round_float64 12) (hydra_lib_math_atan 0.5)))))

;; atan2

(deftest test-atan2-negatan2-1-1

  (is (= 0.7853981633974483

         ((hydra_lib_math_atan2 1.0) 1.0))))

(deftest test-atan2-negatan2-1-0

  (is (= 1.5707963267948966

         ((hydra_lib_math_atan2 1.0) 0.0))))

(deftest test-atan2-negatan2-0-1

  (is (= 0.0

         ((hydra_lib_math_atan2 0.0) 1.0))))

(deftest test-atan2-negatan2-3-4

  (is (= 0.643501108793

         ((hydra_lib_math_round_float64 12) ((hydra_lib_math_atan2 3.0) 4.0)))))

;; sinh

(deftest test-sinh-negsinh-0

  (is (= 0.0

         (hydra_lib_math_sinh 0.0))))

(deftest test-sinh-negsinh-1

  (is (= 1.1752011936438014

         (hydra_lib_math_sinh 1.0))))

(deftest test-sinh-negsinh-2

  (is (= 3.62686040785

         ((hydra_lib_math_round_float64 12) (hydra_lib_math_sinh 2.0)))))

;; cosh

(deftest test-cosh-negcosh-0

  (is (= 1.0

         (hydra_lib_math_cosh 0.0))))

(deftest test-cosh-negcosh-1

  (is (= 1.54308063482

         ((hydra_lib_math_round_float64 12) (hydra_lib_math_cosh 1.0)))))

(deftest test-cosh-negcosh-2

  (is (= 3.76219569108

         ((hydra_lib_math_round_float64 12) (hydra_lib_math_cosh 2.0)))))

;; tanh

(deftest test-tanh-negtanh-0

  (is (= 0.0

         (hydra_lib_math_tanh 0.0))))

(deftest test-tanh-negtanh-1

  (is (= 0.7615941559557649

         (hydra_lib_math_tanh 1.0))))

(deftest test-tanh-negtanh-0-dot5

  (is (= 0.46211715726

         ((hydra_lib_math_round_float64 12) (hydra_lib_math_tanh 0.5)))))

;; asinh

(deftest test-asinh-negasinh-0

  (is (= 0.0

         (hydra_lib_math_asinh 0.0))))

(deftest test-asinh-negasinh-1

  (is (= 0.88137358702

         ((hydra_lib_math_round_float64 12) (hydra_lib_math_asinh 1.0)))))

(deftest test-asinh-negasinh-0-dot5

  (is (= 0.48121182506

         ((hydra_lib_math_round_float64 12) (hydra_lib_math_asinh 0.5)))))

;; acosh

(deftest test-acosh-negacosh-1

  (is (= 0.0

         (hydra_lib_math_acosh 1.0))))

(deftest test-acosh-negacosh-2

  (is (= 1.31695789692

         ((hydra_lib_math_round_float64 12) (hydra_lib_math_acosh 2.0)))))

(deftest test-acosh-negacosh-3

  (is (= 1.76274717404

         ((hydra_lib_math_round_float64 12) (hydra_lib_math_acosh 3.0)))))

;; atanh

(deftest test-atanh-negatanh-0

  (is (= 0.0

         (hydra_lib_math_atanh 0.0))))

(deftest test-atanh-negatanh-0-dot5

  (is (= 0.549306144334

         ((hydra_lib_math_round_float64 12) (hydra_lib_math_atanh 0.5)))))

(deftest test-atanh-negatanh-0-dot1

  (is (= 0.100335347731

         ((hydra_lib_math_round_float64 12) (hydra_lib_math_atanh 0.1)))))

;; exp

(deftest test-exp-negexp-0

  (is (= 1.0

         (hydra_lib_math_exp 0.0))))

(deftest test-exp-negexp-1

  (is (= 2.71828182846

         ((hydra_lib_math_round_float64 12) (hydra_lib_math_exp 1.0)))))

(deftest test-exp-negexp--neg1

  (is (= 0.367879441171

         ((hydra_lib_math_round_float64 12) (hydra_lib_math_exp -1.0)))))

(deftest test-exp-negexp-2

  (is (= 7.38905609893

         ((hydra_lib_math_round_float64 12) (hydra_lib_math_exp 2.0)))))

(deftest test-exp-negexp-0-dot5

  (is (= 1.6487212707

         ((hydra_lib_math_round_float64 12) (hydra_lib_math_exp 0.5)))))

;; log

(deftest test-log-neglog-1

  (is (= 0.0

         (hydra_lib_math_log 1.0))))

(deftest test-log-neglog-e

  (is (= 1.0

         (hydra_lib_math_log 2.718281828459045))))

(deftest test-log-neglog-2

  (is (= 0.69314718056

         ((hydra_lib_math_round_float64 12) (hydra_lib_math_log 2.0)))))

(deftest test-log-neglog-10

  (is (= 2.30258509299

         ((hydra_lib_math_round_float64 12) (hydra_lib_math_log 10.0)))))

;; logBase

(deftest test-logbase-neglog10-1

  (is (= 0.0

         ((hydra_lib_math_log_base 10.0) 1.0))))

(deftest test-logbase-neglog10-10

  (is (= 1.0

         ((hydra_lib_math_log_base 10.0) 10.0))))

(deftest test-logbase-neglog10-100

  (is (= 2.0

         ((hydra_lib_math_log_base 10.0) 100.0))))

(deftest test-logbase-neglog2-8

  (is (= 3.0

         ((hydra_lib_math_log_base 2.0) 8.0))))

(deftest test-logbase-neglog2-10

  (is (= 3.32192809489

         ((hydra_lib_math_round_float64 12) ((hydra_lib_math_log_base 2.0) 10.0)))))

;; pow

(deftest test-pow-neg2-3

  (is (= 8.0

         ((hydra_lib_math_pow 2.0) 3.0))))

(deftest test-pow-neg10-0

  (is (= 1.0

         ((hydra_lib_math_pow 10.0) 0.0))))

(deftest test-pow-neg2--neg1

  (is (= 0.5

         ((hydra_lib_math_pow 2.0) -1.0))))

(deftest test-pow-neg2-0-dot5

  (is (= 1.41421356237

         ((hydra_lib_math_round_float64 12) ((hydra_lib_math_pow 2.0) 0.5)))))

;; sqrt

(deftest test-sqrt-negsqrt-4

  (is (= 2.0

         (hydra_lib_math_sqrt 4.0))))

(deftest test-sqrt-negsqrt-9

  (is (= 3.0

         (hydra_lib_math_sqrt 9.0))))

(deftest test-sqrt-negsqrt-2

  (is (= 1.4142135623730951

         (hydra_lib_math_sqrt 2.0))))

(deftest test-sqrt-negsqrt-0

  (is (= 0.0

         (hydra_lib_math_sqrt 0.0))))

(deftest test-sqrt-negsqrt-3

  (is (= 1.73205080757

         ((hydra_lib_math_round_float64 12) (hydra_lib_math_sqrt 3.0)))))

;; ceiling

(deftest test-ceiling-negceiling-3-dot2

  (is (= 4

         (hydra_lib_math_ceiling 3.2))))

(deftest test-ceiling-negceiling-3-dot0

  (is (= 3

         (hydra_lib_math_ceiling 3.0))))

(deftest test-ceiling-negceiling--neg3-dot2

  (is (= -3

         (hydra_lib_math_ceiling -3.2))))

(deftest test-ceiling-negceiling--neg3-dot0

  (is (= -3

         (hydra_lib_math_ceiling -3.0))))

;; floor

(deftest test-floor-negfloor-3-dot8

  (is (= 3

         (hydra_lib_math_floor 3.8))))

(deftest test-floor-negfloor-3-dot0

  (is (= 3

         (hydra_lib_math_floor 3.0))))

(deftest test-floor-negfloor--neg3-dot2

  (is (= -4

         (hydra_lib_math_floor -3.2))))

(deftest test-floor-negfloor--neg3-dot0

  (is (= -3

         (hydra_lib_math_floor -3.0))))

;; round

(deftest test-round-neground-3-dot4

  (is (= 3

         (hydra_lib_math_round 3.4))))

(deftest test-round-neground-3-dot5

  (is (= 4

         (hydra_lib_math_round 3.5))))

(deftest test-round-neground-3-dot6

  (is (= 4

         (hydra_lib_math_round 3.6))))

(deftest test-round-neground--neg3-dot4

  (is (= -3

         (hydra_lib_math_round -3.4))))

(deftest test-round-neground--neg3-dot5

  (is (= -4

         (hydra_lib_math_round -3.5))))

;; roundBigfloat

(deftest test-roundbigfloat-negzero

  (is (= 0.0

         ((hydra_lib_math_round_bigfloat 5) 0.0))))

(deftest test-roundbigfloat-neground-pi-to-4-digits

  (is (= 3.142

         ((hydra_lib_math_round_bigfloat 4) 3.141592653589793))))

(deftest test-roundbigfloat-neground-1234-dot5-to-3-digits

  (is (= 1230.0

         ((hydra_lib_math_round_bigfloat 3) 1234.5))))

(deftest test-roundbigfloat-neground-0-dot001234-to-2-digits

  (is (= 1.2e-3

         ((hydra_lib_math_round_bigfloat 2) 1.234e-3))))

(deftest test-roundbigfloat-negnegative

  (is (= -1230.0

         ((hydra_lib_math_round_bigfloat 3) -1234.5))))

;; roundFloat32

(deftest test-roundfloat32-negzero

  (is (= 0.0

         ((hydra_lib_math_round_float32 5) 0.0))))

(deftest test-roundfloat32-neground-pi-to-4-digits

  (is (= 3.1419999599456787

         ((hydra_lib_math_round_float32 4) 3.1415927410125732))))

(deftest test-roundfloat32-neground-1234-dot5-to-3-digits

  (is (= 1230.0

         ((hydra_lib_math_round_float32 3) 1234.5))))

(deftest test-roundfloat32-negnegative

  (is (= -1230.0

         ((hydra_lib_math_round_float32 3) -1234.5))))

;; roundFloat64

(deftest test-roundfloat64-negzero

  (is (= 0.0

         ((hydra_lib_math_round_float64 5) 0.0))))

(deftest test-roundfloat64-neground-pi-to-4-digits

  (is (= 3.142

         ((hydra_lib_math_round_float64 4) 3.141592653589793))))

(deftest test-roundfloat64-neground-pi-to-10-digits

  (is (= 3.141592654

         ((hydra_lib_math_round_float64 10) 3.141592653589793))))

(deftest test-roundfloat64-neground-1234-dot5-to-3-digits

  (is (= 1230.0

         ((hydra_lib_math_round_float64 3) 1234.5))))

(deftest test-roundfloat64-neground-0-dot001234-to-2-digits

  (is (= 1.2e-3

         ((hydra_lib_math_round_float64 2) 1.234e-3))))

(deftest test-roundfloat64-negnegative

  (is (= -1230.0

         ((hydra_lib_math_round_float64 3) -1234.5))))

(deftest test-roundfloat64-neground-1-digit

  (is (= 10.0

         ((hydra_lib_math_round_float64 1) 9.876))))

;; truncate

(deftest test-truncate-negtruncate-3-dot8

  (is (= 3

         (hydra_lib_math_truncate 3.8))))

(deftest test-truncate-negtruncate-3-dot2

  (is (= 3

         (hydra_lib_math_truncate 3.2))))

(deftest test-truncate-negtruncate--neg3-dot8

  (is (= -3

         (hydra_lib_math_truncate -3.8))))

(deftest test-truncate-negtruncate--neg3-dot2

  (is (= -3

         (hydra_lib_math_truncate -3.2))))
