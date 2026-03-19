;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.math primitives

(ns generation.hydra.test.lib.math-test
  (:require [clojure.test :refer :all]))

;; abs

(deftest test-math-negabs-negpositive

  (is (= 5

         (hydra_lib_math_abs 5))))

(deftest test-math-negabs-negnegative

  (is (= 5

         (hydra_lib_math_abs -5))))

(deftest test-math-negabs-negzero

  (is (= 0

         (hydra_lib_math_abs 0))))

;; add

(deftest test-math-negadd-negpositive-numbers

  (is (= 8

         ((hydra_lib_math_add 3) 5))))

(deftest test-math-negadd-negnegative-numbers

  (is (= -8

         ((hydra_lib_math_add -3) -5))))

(deftest test-math-negadd-negmixed-sign

  (is (= 7

         ((hydra_lib_math_add 10) -3))))

(deftest test-math-negadd-negwith-zero

  (is (= 42

         ((hydra_lib_math_add 42) 0))))

;; div

(deftest test-math-negdiv-negexact-division

  (is (= 5

         ((hydra_lib_math_div 10) 2))))

(deftest test-math-negdiv-negtruncates-toward-negative-infinity

  (is (= 3

         ((hydra_lib_math_div 10) 3))))

(deftest test-math-negdiv-negnegative-dividend

  (is (= -4

         ((hydra_lib_math_div -10) 3))))

(deftest test-math-negdiv-negnegative-divisor

  (is (= -4

         ((hydra_lib_math_div 10) -3))))

;; even

(deftest test-math-negeven-negeven-positive

  (is (= true

         (hydra_lib_math_even 4))))

(deftest test-math-negeven-negodd-positive

  (is (= false

         (hydra_lib_math_even 5))))

(deftest test-math-negeven-negeven-negative

  (is (= true

         (hydra_lib_math_even -4))))

(deftest test-math-negeven-negodd-negative

  (is (= false

         (hydra_lib_math_even -5))))

(deftest test-math-negeven-negzero

  (is (= true

         (hydra_lib_math_even 0))))

;; max

(deftest test-math-negmax-negfirst-is-larger

  (is (= 10

         ((hydra_lib_math_max 10) 5))))

(deftest test-math-negmax-negsecond-is-larger

  (is (= 10

         ((hydra_lib_math_max 5) 10))))

(deftest test-math-negmax-negequal-values

  (is (= 7

         ((hydra_lib_math_max 7) 7))))

(deftest test-math-negmax-negnegative-numbers

  (is (= -3

         ((hydra_lib_math_max -3) -5))))

(deftest test-math-negmax-negmixed-sign

  (is (= 5

         ((hydra_lib_math_max -5) 5))))

(deftest test-math-negmax-negwith-zero

  (is (= 42

         ((hydra_lib_math_max 0) 42))))

;; min

(deftest test-math-negmin-negfirst-is-smaller

  (is (= 5

         ((hydra_lib_math_min 5) 10))))

(deftest test-math-negmin-negsecond-is-smaller

  (is (= 5

         ((hydra_lib_math_min 10) 5))))

(deftest test-math-negmin-negequal-values

  (is (= 7

         ((hydra_lib_math_min 7) 7))))

(deftest test-math-negmin-negnegative-numbers

  (is (= -5

         ((hydra_lib_math_min -3) -5))))

(deftest test-math-negmin-negmixed-sign

  (is (= -5

         ((hydra_lib_math_min -5) 5))))

(deftest test-math-negmin-negwith-zero

  (is (= 0

         ((hydra_lib_math_min 0) 42))))

;; mod

(deftest test-math-negmod-negbasic-modulo

  (is (= 1

         ((hydra_lib_math_mod 10) 3))))

(deftest test-math-negmod-negexact-division

  (is (= 0

         ((hydra_lib_math_mod 10) 2))))

(deftest test-math-negmod-negnegative-dividend

  (is (= 2

         ((hydra_lib_math_mod -10) 3))))

(deftest test-math-negmod-negnegative-divisor

  (is (= -2

         ((hydra_lib_math_mod 10) -3))))

;; mul

(deftest test-math-negmul-negpositive-numbers

  (is (= 15

         ((hydra_lib_math_mul 3) 5))))

(deftest test-math-negmul-negnegative-numbers

  (is (= 15

         ((hydra_lib_math_mul -3) -5))))

(deftest test-math-negmul-negmixed-sign

  (is (= -15

         ((hydra_lib_math_mul 3) -5))))

(deftest test-math-negmul-negwith-zero

  (is (= 0

         ((hydra_lib_math_mul 42) 0))))

(deftest test-math-negmul-negwith-one

  (is (= 42

         ((hydra_lib_math_mul 42) 1))))

;; negate

(deftest test-math-negnegate-negpositive

  (is (= -5

         (hydra_lib_math_negate 5))))

(deftest test-math-negnegate-negnegative

  (is (= 5

         (hydra_lib_math_negate -5))))

(deftest test-math-negnegate-negzero

  (is (= 0

         (hydra_lib_math_negate 0))))

;; odd

(deftest test-math-negodd-negodd-positive

  (is (= true

         (hydra_lib_math_odd 5))))

(deftest test-math-negodd-negeven-positive

  (is (= false

         (hydra_lib_math_odd 4))))

(deftest test-math-negodd-negodd-negative

  (is (= true

         (hydra_lib_math_odd -5))))

(deftest test-math-negodd-negeven-negative

  (is (= false

         (hydra_lib_math_odd -4))))

(deftest test-math-negodd-negzero

  (is (= false

         (hydra_lib_math_odd 0))))

;; pred

(deftest test-math-negpred-negpositive

  (is (= 4

         (hydra_lib_math_pred 5))))

(deftest test-math-negpred-negzero

  (is (= -1

         (hydra_lib_math_pred 0))))

(deftest test-math-negpred-negnegative

  (is (= -6

         (hydra_lib_math_pred -5))))

;; range

(deftest test-math-negrange-negascending-range

  (is (= (list 1 2 3 4 5)

         ((hydra_lib_math_range 1) 5))))

(deftest test-math-negrange-negsingle-element

  (is (= (list 5)

         ((hydra_lib_math_range 5) 5))))

(deftest test-math-negrange-negtwo-elements

  (is (= (list 3 4)

         ((hydra_lib_math_range 3) 4))))

(deftest test-math-negrange-negnegative-start

  (is (= (list -2 -1 0 1 2)

         ((hydra_lib_math_range -2) 2))))

;; rem

(deftest test-math-negrem-negbasic-remainder

  (is (= 1

         ((hydra_lib_math_rem 10) 3))))

(deftest test-math-negrem-negexact-division

  (is (= 0

         ((hydra_lib_math_rem 10) 2))))

(deftest test-math-negrem-negnegative-dividend

  (is (= -1

         ((hydra_lib_math_rem -10) 3))))

(deftest test-math-negrem-negnegative-divisor

  (is (= 1

         ((hydra_lib_math_rem 10) -3))))

;; signum

(deftest test-math-negsignum-negpositive

  (is (= 1

         (hydra_lib_math_signum 5))))

(deftest test-math-negsignum-negnegative

  (is (= -1

         (hydra_lib_math_signum -5))))

(deftest test-math-negsignum-negzero

  (is (= 0

         (hydra_lib_math_signum 0))))

;; sub

(deftest test-math-negsub-negpositive-numbers

  (is (= 7

         ((hydra_lib_math_sub 10) 3))))

(deftest test-math-negsub-negnegative-numbers

  (is (= -7

         ((hydra_lib_math_sub -10) -3))))

(deftest test-math-negsub-negmixed-sign

  (is (= 13

         ((hydra_lib_math_sub 10) -3))))

(deftest test-math-negsub-negwith-zero

  (is (= 42

         ((hydra_lib_math_sub 42) 0))))

;; succ

(deftest test-math-negsucc-negpositive

  (is (= 6

         (hydra_lib_math_succ 5))))

(deftest test-math-negsucc-negzero

  (is (= 1

         (hydra_lib_math_succ 0))))

(deftest test-math-negsucc-negnegative

  (is (= -4

         (hydra_lib_math_succ -5))))

;; e

(deftest test-math-nege-negeuler-s-number

  (is (= 2.718281828459045

         hydra_lib_math_e)))

;; pi

(deftest test-math-negpi-negpi-constant

  (is (= 3.141592653589793

         hydra_lib_math_pi)))

;; sin

(deftest test-math-negsin-negsin-0

  (is (= 0.0

         (hydra_lib_math_sin 0.0))))

(deftest test-math-negsin-negsin-pi-div2

  (is (= 1.0

         (hydra_lib_math_sin 1.5707963267948966))))

(deftest test-math-negsin-negsin-pi

  (is (= 1.2246467991473532e-16

         (hydra_lib_math_sin 3.141592653589793))))

(deftest test-math-negsin-negsin-1

  (is (= 0.841470984808

         ((hydra_lib_math_round_float64 12) (hydra_lib_math_sin 1.0)))))

(deftest test-math-negsin-negsin-0-dot5

  (is (= 0.479425538604

         ((hydra_lib_math_round_float64 12) (hydra_lib_math_sin 0.5)))))

;; cos

(deftest test-math-negcos-negcos-0

  (is (= 1.0

         (hydra_lib_math_cos 0.0))))

(deftest test-math-negcos-negcos-pi-div2

  (is (= 6.123233995736766e-17

         (hydra_lib_math_cos 1.5707963267948966))))

(deftest test-math-negcos-negcos-pi

  (is (= -1.0

         (hydra_lib_math_cos 3.141592653589793))))

(deftest test-math-negcos-negcos-1

  (is (= 0.540302305868

         ((hydra_lib_math_round_float64 12) (hydra_lib_math_cos 1.0)))))

(deftest test-math-negcos-negcos-0-dot5

  (is (= 0.87758256189

         ((hydra_lib_math_round_float64 12) (hydra_lib_math_cos 0.5)))))

;; tan

(deftest test-math-negtan-negtan-0

  (is (= 0.0

         (hydra_lib_math_tan 0.0))))

(deftest test-math-negtan-negtan-pi-div4

  (is (= 0.9999999999999999

         (hydra_lib_math_tan 0.7853981633974483))))

(deftest test-math-negtan-negtan-1

  (is (= 1.55740772465

         ((hydra_lib_math_round_float64 12) (hydra_lib_math_tan 1.0)))))

(deftest test-math-negtan-negtan-0-dot5

  (is (= 0.546302489844

         ((hydra_lib_math_round_float64 12) (hydra_lib_math_tan 0.5)))))

;; asin

(deftest test-math-negasin-negasin-0

  (is (= 0.0

         (hydra_lib_math_asin 0.0))))

(deftest test-math-negasin-negasin-1

  (is (= 1.5707963267948966

         (hydra_lib_math_asin 1.0))))

(deftest test-math-negasin-negasin--neg1

  (is (= -1.5707963267948966

         (hydra_lib_math_asin -1.0))))

(deftest test-math-negasin-negasin-0-dot5

  (is (= 0.523598775598

         ((hydra_lib_math_round_float64 12) (hydra_lib_math_asin 0.5)))))

;; acos

(deftest test-math-negacos-negacos-1

  (is (= 0.0

         (hydra_lib_math_acos 1.0))))

(deftest test-math-negacos-negacos-0

  (is (= 1.5707963267948966

         (hydra_lib_math_acos 0.0))))

(deftest test-math-negacos-negacos--neg1

  (is (= 3.141592653589793

         (hydra_lib_math_acos -1.0))))

(deftest test-math-negacos-negacos-0-dot5

  (is (= 1.0471975512

         ((hydra_lib_math_round_float64 12) (hydra_lib_math_acos 0.5)))))

;; atan

(deftest test-math-negatan-negatan-0

  (is (= 0.0

         (hydra_lib_math_atan 0.0))))

(deftest test-math-negatan-negatan-1

  (is (= 0.7853981633974483

         (hydra_lib_math_atan 1.0))))

(deftest test-math-negatan-negatan-0-dot5

  (is (= 0.463647609001

         ((hydra_lib_math_round_float64 12) (hydra_lib_math_atan 0.5)))))

;; atan2

(deftest test-math-negatan2-negatan2-1-1

  (is (= 0.7853981633974483

         ((hydra_lib_math_atan2 1.0) 1.0))))

(deftest test-math-negatan2-negatan2-1-0

  (is (= 1.5707963267948966

         ((hydra_lib_math_atan2 1.0) 0.0))))

(deftest test-math-negatan2-negatan2-0-1

  (is (= 0.0

         ((hydra_lib_math_atan2 0.0) 1.0))))

(deftest test-math-negatan2-negatan2-3-4

  (is (= 0.643501108793

         ((hydra_lib_math_round_float64 12) ((hydra_lib_math_atan2 3.0) 4.0)))))

;; sinh

(deftest test-math-negsinh-negsinh-0

  (is (= 0.0

         (hydra_lib_math_sinh 0.0))))

(deftest test-math-negsinh-negsinh-1

  (is (= 1.1752011936438014

         (hydra_lib_math_sinh 1.0))))

(deftest test-math-negsinh-negsinh-2

  (is (= 3.62686040785

         ((hydra_lib_math_round_float64 12) (hydra_lib_math_sinh 2.0)))))

;; cosh

(deftest test-math-negcosh-negcosh-0

  (is (= 1.0

         (hydra_lib_math_cosh 0.0))))

(deftest test-math-negcosh-negcosh-1

  (is (= 1.54308063482

         ((hydra_lib_math_round_float64 12) (hydra_lib_math_cosh 1.0)))))

(deftest test-math-negcosh-negcosh-2

  (is (= 3.76219569108

         ((hydra_lib_math_round_float64 12) (hydra_lib_math_cosh 2.0)))))

;; tanh

(deftest test-math-negtanh-negtanh-0

  (is (= 0.0

         (hydra_lib_math_tanh 0.0))))

(deftest test-math-negtanh-negtanh-1

  (is (= 0.7615941559557649

         (hydra_lib_math_tanh 1.0))))

(deftest test-math-negtanh-negtanh-0-dot5

  (is (= 0.46211715726

         ((hydra_lib_math_round_float64 12) (hydra_lib_math_tanh 0.5)))))

;; asinh

(deftest test-math-negasinh-negasinh-0

  (is (= 0.0

         (hydra_lib_math_asinh 0.0))))

(deftest test-math-negasinh-negasinh-1

  (is (= 0.88137358702

         ((hydra_lib_math_round_float64 12) (hydra_lib_math_asinh 1.0)))))

(deftest test-math-negasinh-negasinh-0-dot5

  (is (= 0.48121182506

         ((hydra_lib_math_round_float64 12) (hydra_lib_math_asinh 0.5)))))

;; acosh

(deftest test-math-negacosh-negacosh-1

  (is (= 0.0

         (hydra_lib_math_acosh 1.0))))

(deftest test-math-negacosh-negacosh-2

  (is (= 1.31695789692

         ((hydra_lib_math_round_float64 12) (hydra_lib_math_acosh 2.0)))))

(deftest test-math-negacosh-negacosh-3

  (is (= 1.76274717404

         ((hydra_lib_math_round_float64 12) (hydra_lib_math_acosh 3.0)))))

;; atanh

(deftest test-math-negatanh-negatanh-0

  (is (= 0.0

         (hydra_lib_math_atanh 0.0))))

(deftest test-math-negatanh-negatanh-0-dot5

  (is (= 0.549306144334

         ((hydra_lib_math_round_float64 12) (hydra_lib_math_atanh 0.5)))))

(deftest test-math-negatanh-negatanh-0-dot1

  (is (= 0.100335347731

         ((hydra_lib_math_round_float64 12) (hydra_lib_math_atanh 0.1)))))

;; exp

(deftest test-math-negexp-negexp-0

  (is (= 1.0

         (hydra_lib_math_exp 0.0))))

(deftest test-math-negexp-negexp-1

  (is (= 2.71828182846

         ((hydra_lib_math_round_float64 12) (hydra_lib_math_exp 1.0)))))

(deftest test-math-negexp-negexp--neg1

  (is (= 0.367879441171

         ((hydra_lib_math_round_float64 12) (hydra_lib_math_exp -1.0)))))

(deftest test-math-negexp-negexp-2

  (is (= 7.38905609893

         ((hydra_lib_math_round_float64 12) (hydra_lib_math_exp 2.0)))))

(deftest test-math-negexp-negexp-0-dot5

  (is (= 1.6487212707

         ((hydra_lib_math_round_float64 12) (hydra_lib_math_exp 0.5)))))

;; log

(deftest test-math-neglog-neglog-1

  (is (= 0.0

         (hydra_lib_math_log 1.0))))

(deftest test-math-neglog-neglog-e

  (is (= 1.0

         (hydra_lib_math_log 2.718281828459045))))

(deftest test-math-neglog-neglog-2

  (is (= 0.69314718056

         ((hydra_lib_math_round_float64 12) (hydra_lib_math_log 2.0)))))

(deftest test-math-neglog-neglog-10

  (is (= 2.30258509299

         ((hydra_lib_math_round_float64 12) (hydra_lib_math_log 10.0)))))

;; logBase

(deftest test-math-neglogbase-neglog10-1

  (is (= 0.0

         ((hydra_lib_math_log_base 10.0) 1.0))))

(deftest test-math-neglogbase-neglog10-10

  (is (= 1.0

         ((hydra_lib_math_log_base 10.0) 10.0))))

(deftest test-math-neglogbase-neglog10-100

  (is (= 2.0

         ((hydra_lib_math_log_base 10.0) 100.0))))

(deftest test-math-neglogbase-neglog2-8

  (is (= 3.0

         ((hydra_lib_math_log_base 2.0) 8.0))))

(deftest test-math-neglogbase-neglog2-10

  (is (= 3.32192809489

         ((hydra_lib_math_round_float64 12) ((hydra_lib_math_log_base 2.0) 10.0)))))

;; pow

(deftest test-math-negpow-neg2-3

  (is (= 8.0

         ((hydra_lib_math_pow 2.0) 3.0))))

(deftest test-math-negpow-neg10-0

  (is (= 1.0

         ((hydra_lib_math_pow 10.0) 0.0))))

(deftest test-math-negpow-neg2--neg1

  (is (= 0.5

         ((hydra_lib_math_pow 2.0) -1.0))))

(deftest test-math-negpow-neg2-0-dot5

  (is (= 1.41421356237

         ((hydra_lib_math_round_float64 12) ((hydra_lib_math_pow 2.0) 0.5)))))

;; sqrt

(deftest test-math-negsqrt-negsqrt-4

  (is (= 2.0

         (hydra_lib_math_sqrt 4.0))))

(deftest test-math-negsqrt-negsqrt-9

  (is (= 3.0

         (hydra_lib_math_sqrt 9.0))))

(deftest test-math-negsqrt-negsqrt-2

  (is (= 1.4142135623730951

         (hydra_lib_math_sqrt 2.0))))

(deftest test-math-negsqrt-negsqrt-0

  (is (= 0.0

         (hydra_lib_math_sqrt 0.0))))

(deftest test-math-negsqrt-negsqrt-3

  (is (= 1.73205080757

         ((hydra_lib_math_round_float64 12) (hydra_lib_math_sqrt 3.0)))))

;; ceiling

(deftest test-math-negceiling-negceiling-3-dot2

  (is (= 4

         (hydra_lib_math_ceiling 3.2))))

(deftest test-math-negceiling-negceiling-3-dot0

  (is (= 3

         (hydra_lib_math_ceiling 3.0))))

(deftest test-math-negceiling-negceiling--neg3-dot2

  (is (= -3

         (hydra_lib_math_ceiling -3.2))))

(deftest test-math-negceiling-negceiling--neg3-dot0

  (is (= -3

         (hydra_lib_math_ceiling -3.0))))

;; floor

(deftest test-math-negfloor-negfloor-3-dot8

  (is (= 3

         (hydra_lib_math_floor 3.8))))

(deftest test-math-negfloor-negfloor-3-dot0

  (is (= 3

         (hydra_lib_math_floor 3.0))))

(deftest test-math-negfloor-negfloor--neg3-dot2

  (is (= -4

         (hydra_lib_math_floor -3.2))))

(deftest test-math-negfloor-negfloor--neg3-dot0

  (is (= -3

         (hydra_lib_math_floor -3.0))))

;; round

(deftest test-math-neground-neground-3-dot4

  (is (= 3

         (hydra_lib_math_round 3.4))))

(deftest test-math-neground-neground-3-dot5

  (is (= 4

         (hydra_lib_math_round 3.5))))

(deftest test-math-neground-neground-3-dot6

  (is (= 4

         (hydra_lib_math_round 3.6))))

(deftest test-math-neground-neground--neg3-dot4

  (is (= -3

         (hydra_lib_math_round -3.4))))

(deftest test-math-neground-neground--neg3-dot5

  (is (= -4

         (hydra_lib_math_round -3.5))))

;; roundBigfloat

(deftest test-math-negroundbigfloat-negzero

  (is (= 0.0

         ((hydra_lib_math_round_bigfloat 5) 0.0))))

(deftest test-math-negroundbigfloat-neground-pi-to-4-digits

  (is (= 3.142

         ((hydra_lib_math_round_bigfloat 4) 3.141592653589793))))

(deftest test-math-negroundbigfloat-neground-1234-dot5-to-3-digits

  (is (= 1230.0

         ((hydra_lib_math_round_bigfloat 3) 1234.5))))

(deftest test-math-negroundbigfloat-neground-0-dot001234-to-2-digits

  (is (= 1.2e-3

         ((hydra_lib_math_round_bigfloat 2) 1.234e-3))))

(deftest test-math-negroundbigfloat-negnegative

  (is (= -1230.0

         ((hydra_lib_math_round_bigfloat 3) -1234.5))))

;; roundFloat32

(deftest test-math-negroundfloat32-negzero

  (is (= 0.0

         ((hydra_lib_math_round_float32 5) 0.0))))

(deftest test-math-negroundfloat32-neground-pi-to-4-digits

  (is (= 3.1419999599456787

         ((hydra_lib_math_round_float32 4) 3.1415927410125732))))

(deftest test-math-negroundfloat32-neground-1234-dot5-to-3-digits

  (is (= 1230.0

         ((hydra_lib_math_round_float32 3) 1234.5))))

(deftest test-math-negroundfloat32-negnegative

  (is (= -1230.0

         ((hydra_lib_math_round_float32 3) -1234.5))))

;; roundFloat64

(deftest test-math-negroundfloat64-negzero

  (is (= 0.0

         ((hydra_lib_math_round_float64 5) 0.0))))

(deftest test-math-negroundfloat64-neground-pi-to-4-digits

  (is (= 3.142

         ((hydra_lib_math_round_float64 4) 3.141592653589793))))

(deftest test-math-negroundfloat64-neground-pi-to-10-digits

  (is (= 3.141592654

         ((hydra_lib_math_round_float64 10) 3.141592653589793))))

(deftest test-math-negroundfloat64-neground-1234-dot5-to-3-digits

  (is (= 1230.0

         ((hydra_lib_math_round_float64 3) 1234.5))))

(deftest test-math-negroundfloat64-neground-0-dot001234-to-2-digits

  (is (= 1.2e-3

         ((hydra_lib_math_round_float64 2) 1.234e-3))))

(deftest test-math-negroundfloat64-negnegative

  (is (= -1230.0

         ((hydra_lib_math_round_float64 3) -1234.5))))

(deftest test-math-negroundfloat64-neground-1-digit

  (is (= 10.0

         ((hydra_lib_math_round_float64 1) 9.876))))

;; truncate

(deftest test-math-negtruncate-negtruncate-3-dot8

  (is (= 3

         (hydra_lib_math_truncate 3.8))))

(deftest test-math-negtruncate-negtruncate-3-dot2

  (is (= 3

         (hydra_lib_math_truncate 3.2))))

(deftest test-math-negtruncate-negtruncate--neg3-dot8

  (is (= -3

         (hydra_lib_math_truncate -3.8))))

(deftest test-math-negtruncate-negtruncate--neg3-dot2

  (is (= -3

         (hydra_lib_math_truncate -3.2))))
