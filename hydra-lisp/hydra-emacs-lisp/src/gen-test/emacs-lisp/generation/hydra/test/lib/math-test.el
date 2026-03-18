;;; Note: this is an automatically generated file. Do not edit.
;;; hydra.lib.math primitives

(require 'ert)

;; abs

(ert-deftest test-abs-negpositive ()

  (should (equal 5 (hydra_lib_math_abs 5))))

(ert-deftest test-abs-negnegative ()

  (should (equal 5 (hydra_lib_math_abs -5))))

(ert-deftest test-abs-negzero ()

  (should (equal 0 (hydra_lib_math_abs 0))))

;; add

(ert-deftest test-add-negpositive-numbers ()

  (should (equal 8 ((hydra_lib_math_add 3) 5))))

(ert-deftest test-add-negnegative-numbers ()

  (should (equal -8 ((hydra_lib_math_add -3) -5))))

(ert-deftest test-add-negmixed-sign ()

  (should (equal 7 ((hydra_lib_math_add 10) -3))))

(ert-deftest test-add-negwith-zero ()

  (should (equal 42 ((hydra_lib_math_add 42) 0))))

;; div

(ert-deftest test-div-negexact-division ()

  (should (equal 5 ((hydra_lib_math_div 10) 2))))

(ert-deftest test-div-negtruncates-toward-negative-infinity ()

  (should (equal 3 ((hydra_lib_math_div 10) 3))))

(ert-deftest test-div-negnegative-dividend ()

  (should (equal -4 ((hydra_lib_math_div -10) 3))))

(ert-deftest test-div-negnegative-divisor ()

  (should (equal -4 ((hydra_lib_math_div 10) -3))))

;; even

(ert-deftest test-even-negeven-positive ()

  (should (equal t (hydra_lib_math_even 4))))

(ert-deftest test-even-negodd-positive ()

  (should (equal nil (hydra_lib_math_even 5))))

(ert-deftest test-even-negeven-negative ()

  (should (equal t (hydra_lib_math_even -4))))

(ert-deftest test-even-negodd-negative ()

  (should (equal nil (hydra_lib_math_even -5))))

(ert-deftest test-even-negzero ()

  (should (equal t (hydra_lib_math_even 0))))

;; max

(ert-deftest test-max-negfirst-is-larger ()

  (should (equal 10 ((hydra_lib_math_max 10) 5))))

(ert-deftest test-max-negsecond-is-larger ()

  (should (equal 10 ((hydra_lib_math_max 5) 10))))

(ert-deftest test-max-negequal-values ()

  (should (equal 7 ((hydra_lib_math_max 7) 7))))

(ert-deftest test-max-negnegative-numbers ()

  (should (equal -3 ((hydra_lib_math_max -3) -5))))

(ert-deftest test-max-negmixed-sign ()

  (should (equal 5 ((hydra_lib_math_max -5) 5))))

(ert-deftest test-max-negwith-zero ()

  (should (equal 42 ((hydra_lib_math_max 0) 42))))

;; min

(ert-deftest test-min-negfirst-is-smaller ()

  (should (equal 5 ((hydra_lib_math_min 5) 10))))

(ert-deftest test-min-negsecond-is-smaller ()

  (should (equal 5 ((hydra_lib_math_min 10) 5))))

(ert-deftest test-min-negequal-values ()

  (should (equal 7 ((hydra_lib_math_min 7) 7))))

(ert-deftest test-min-negnegative-numbers ()

  (should (equal -5 ((hydra_lib_math_min -3) -5))))

(ert-deftest test-min-negmixed-sign ()

  (should (equal -5 ((hydra_lib_math_min -5) 5))))

(ert-deftest test-min-negwith-zero ()

  (should (equal 0 ((hydra_lib_math_min 0) 42))))

;; mod

(ert-deftest test-mod-negbasic-modulo ()

  (should (equal 1 ((hydra_lib_math_mod 10) 3))))

(ert-deftest test-mod-negexact-division ()

  (should (equal 0 ((hydra_lib_math_mod 10) 2))))

(ert-deftest test-mod-negnegative-dividend ()

  (should (equal 2 ((hydra_lib_math_mod -10) 3))))

(ert-deftest test-mod-negnegative-divisor ()

  (should (equal -2 ((hydra_lib_math_mod 10) -3))))

;; mul

(ert-deftest test-mul-negpositive-numbers ()

  (should (equal 15 ((hydra_lib_math_mul 3) 5))))

(ert-deftest test-mul-negnegative-numbers ()

  (should (equal 15 ((hydra_lib_math_mul -3) -5))))

(ert-deftest test-mul-negmixed-sign ()

  (should (equal -15 ((hydra_lib_math_mul 3) -5))))

(ert-deftest test-mul-negwith-zero ()

  (should (equal 0 ((hydra_lib_math_mul 42) 0))))

(ert-deftest test-mul-negwith-one ()

  (should (equal 42 ((hydra_lib_math_mul 42) 1))))

;; negate

(ert-deftest test-negate-negpositive ()

  (should (equal -5 (hydra_lib_math_negate 5))))

(ert-deftest test-negate-negnegative ()

  (should (equal 5 (hydra_lib_math_negate -5))))

(ert-deftest test-negate-negzero ()

  (should (equal 0 (hydra_lib_math_negate 0))))

;; odd

(ert-deftest test-odd-negodd-positive ()

  (should (equal t (hydra_lib_math_odd 5))))

(ert-deftest test-odd-negeven-positive ()

  (should (equal nil (hydra_lib_math_odd 4))))

(ert-deftest test-odd-negodd-negative ()

  (should (equal t (hydra_lib_math_odd -5))))

(ert-deftest test-odd-negeven-negative ()

  (should (equal nil (hydra_lib_math_odd -4))))

(ert-deftest test-odd-negzero ()

  (should (equal nil (hydra_lib_math_odd 0))))

;; pred

(ert-deftest test-pred-negpositive ()

  (should (equal 4 (hydra_lib_math_pred 5))))

(ert-deftest test-pred-negzero ()

  (should (equal -1 (hydra_lib_math_pred 0))))

(ert-deftest test-pred-negnegative ()

  (should (equal -6 (hydra_lib_math_pred -5))))

;; range

(ert-deftest test-range-negascending-range ()

  (should (equal (list 1 2 3 4 5) ((hydra_lib_math_range 1) 5))))

(ert-deftest test-range-negsingle-element ()

  (should (equal (list 5) ((hydra_lib_math_range 5) 5))))

(ert-deftest test-range-negtwo-elements ()

  (should (equal (list 3 4) ((hydra_lib_math_range 3) 4))))

(ert-deftest test-range-negnegative-start ()

  (should (equal (list -2 -1 0 1 2) ((hydra_lib_math_range -2) 2))))

;; rem

(ert-deftest test-rem-negbasic-remainder ()

  (should (equal 1 ((hydra_lib_math_rem 10) 3))))

(ert-deftest test-rem-negexact-division ()

  (should (equal 0 ((hydra_lib_math_rem 10) 2))))

(ert-deftest test-rem-negnegative-dividend ()

  (should (equal -1 ((hydra_lib_math_rem -10) 3))))

(ert-deftest test-rem-negnegative-divisor ()

  (should (equal 1 ((hydra_lib_math_rem 10) -3))))

;; signum

(ert-deftest test-signum-negpositive ()

  (should (equal 1 (hydra_lib_math_signum 5))))

(ert-deftest test-signum-negnegative ()

  (should (equal -1 (hydra_lib_math_signum -5))))

(ert-deftest test-signum-negzero ()

  (should (equal 0 (hydra_lib_math_signum 0))))

;; sub

(ert-deftest test-sub-negpositive-numbers ()

  (should (equal 7 ((hydra_lib_math_sub 10) 3))))

(ert-deftest test-sub-negnegative-numbers ()

  (should (equal -7 ((hydra_lib_math_sub -10) -3))))

(ert-deftest test-sub-negmixed-sign ()

  (should (equal 13 ((hydra_lib_math_sub 10) -3))))

(ert-deftest test-sub-negwith-zero ()

  (should (equal 42 ((hydra_lib_math_sub 42) 0))))

;; succ

(ert-deftest test-succ-negpositive ()

  (should (equal 6 (hydra_lib_math_succ 5))))

(ert-deftest test-succ-negzero ()

  (should (equal 1 (hydra_lib_math_succ 0))))

(ert-deftest test-succ-negnegative ()

  (should (equal -4 (hydra_lib_math_succ -5))))

;; e

(ert-deftest test-e-negeuler-s-number ()

  (should (equal 2.718281828459045 hydra_lib_math_e)))

;; pi

(ert-deftest test-pi-negpi-constant ()

  (should (equal 3.141592653589793 hydra_lib_math_pi)))

;; sin

(ert-deftest test-sin-negsin-0 ()

  (should (equal 0.0 (hydra_lib_math_sin 0.0))))

(ert-deftest test-sin-negsin-pi-div2 ()

  (should (equal 1.0 (hydra_lib_math_sin 1.5707963267948966))))

(ert-deftest test-sin-negsin-pi ()

  (should (equal 1.2246467991473532e-16 (hydra_lib_math_sin 3.141592653589793))))

(ert-deftest test-sin-negsin-1 ()

  (should (equal 0.841470984808 ((hydra_lib_math_round_float64 12) (hydra_lib_math_sin 1.0)))))

(ert-deftest test-sin-negsin-0-dot5 ()

  (should (equal 0.479425538604 ((hydra_lib_math_round_float64 12) (hydra_lib_math_sin 0.5)))))

;; cos

(ert-deftest test-cos-negcos-0 ()

  (should (equal 1.0 (hydra_lib_math_cos 0.0))))

(ert-deftest test-cos-negcos-pi-div2 ()

  (should (equal 6.123233995736766e-17 (hydra_lib_math_cos 1.5707963267948966))))

(ert-deftest test-cos-negcos-pi ()

  (should (equal -1.0 (hydra_lib_math_cos 3.141592653589793))))

(ert-deftest test-cos-negcos-1 ()

  (should (equal 0.540302305868 ((hydra_lib_math_round_float64 12) (hydra_lib_math_cos 1.0)))))

(ert-deftest test-cos-negcos-0-dot5 ()

  (should (equal 0.87758256189 ((hydra_lib_math_round_float64 12) (hydra_lib_math_cos 0.5)))))

;; tan

(ert-deftest test-tan-negtan-0 ()

  (should (equal 0.0 (hydra_lib_math_tan 0.0))))

(ert-deftest test-tan-negtan-pi-div4 ()

  (should (equal 0.9999999999999999 (hydra_lib_math_tan 0.7853981633974483))))

(ert-deftest test-tan-negtan-1 ()

  (should (equal 1.55740772465 ((hydra_lib_math_round_float64 12) (hydra_lib_math_tan 1.0)))))

(ert-deftest test-tan-negtan-0-dot5 ()

  (should (equal 0.546302489844 ((hydra_lib_math_round_float64 12) (hydra_lib_math_tan 0.5)))))

;; asin

(ert-deftest test-asin-negasin-0 ()

  (should (equal 0.0 (hydra_lib_math_asin 0.0))))

(ert-deftest test-asin-negasin-1 ()

  (should (equal 1.5707963267948966 (hydra_lib_math_asin 1.0))))

(ert-deftest test-asin-negasin--neg1 ()

  (should (equal -1.5707963267948966 (hydra_lib_math_asin -1.0))))

(ert-deftest test-asin-negasin-0-dot5 ()

  (should (equal 0.523598775598 ((hydra_lib_math_round_float64 12) (hydra_lib_math_asin 0.5)))))

;; acos

(ert-deftest test-acos-negacos-1 ()

  (should (equal 0.0 (hydra_lib_math_acos 1.0))))

(ert-deftest test-acos-negacos-0 ()

  (should (equal 1.5707963267948966 (hydra_lib_math_acos 0.0))))

(ert-deftest test-acos-negacos--neg1 ()

  (should (equal 3.141592653589793 (hydra_lib_math_acos -1.0))))

(ert-deftest test-acos-negacos-0-dot5 ()

  (should (equal 1.0471975512 ((hydra_lib_math_round_float64 12) (hydra_lib_math_acos 0.5)))))

;; atan

(ert-deftest test-atan-negatan-0 ()

  (should (equal 0.0 (hydra_lib_math_atan 0.0))))

(ert-deftest test-atan-negatan-1 ()

  (should (equal 0.7853981633974483 (hydra_lib_math_atan 1.0))))

(ert-deftest test-atan-negatan-0-dot5 ()

  (should (equal 0.463647609001 ((hydra_lib_math_round_float64 12) (hydra_lib_math_atan 0.5)))))

;; atan2

(ert-deftest test-atan2-negatan2-1-1 ()

  (should (equal 0.7853981633974483 ((hydra_lib_math_atan2 1.0) 1.0))))

(ert-deftest test-atan2-negatan2-1-0 ()

  (should (equal 1.5707963267948966 ((hydra_lib_math_atan2 1.0) 0.0))))

(ert-deftest test-atan2-negatan2-0-1 ()

  (should (equal 0.0 ((hydra_lib_math_atan2 0.0) 1.0))))

(ert-deftest test-atan2-negatan2-3-4 ()

  (should (equal 0.643501108793 ((hydra_lib_math_round_float64 12) ((hydra_lib_math_atan2 3.0) 4.0)))))

;; sinh

(ert-deftest test-sinh-negsinh-0 ()

  (should (equal 0.0 (hydra_lib_math_sinh 0.0))))

(ert-deftest test-sinh-negsinh-1 ()

  (should (equal 1.1752011936438014 (hydra_lib_math_sinh 1.0))))

(ert-deftest test-sinh-negsinh-2 ()

  (should (equal 3.62686040785 ((hydra_lib_math_round_float64 12) (hydra_lib_math_sinh 2.0)))))

;; cosh

(ert-deftest test-cosh-negcosh-0 ()

  (should (equal 1.0 (hydra_lib_math_cosh 0.0))))

(ert-deftest test-cosh-negcosh-1 ()

  (should (equal 1.5430806348152437 (hydra_lib_math_cosh 1.0))))

(ert-deftest test-cosh-negcosh-2 ()

  (should (equal 3.76219569108 ((hydra_lib_math_round_float64 12) (hydra_lib_math_cosh 2.0)))))

;; tanh

(ert-deftest test-tanh-negtanh-0 ()

  (should (equal 0.0 (hydra_lib_math_tanh 0.0))))

(ert-deftest test-tanh-negtanh-1 ()

  (should (equal 0.7615941559557649 (hydra_lib_math_tanh 1.0))))

(ert-deftest test-tanh-negtanh-0-dot5 ()

  (should (equal 0.46211715726 ((hydra_lib_math_round_float64 12) (hydra_lib_math_tanh 0.5)))))

;; asinh

(ert-deftest test-asinh-negasinh-0 ()

  (should (equal 0.0 (hydra_lib_math_asinh 0.0))))

(ert-deftest test-asinh-negasinh-1 ()

  (should (equal 0.88137358702 ((hydra_lib_math_round_float64 12) (hydra_lib_math_asinh 1.0)))))

(ert-deftest test-asinh-negasinh-0-dot5 ()

  (should (equal 0.48121182506 ((hydra_lib_math_round_float64 12) (hydra_lib_math_asinh 0.5)))))

;; acosh

(ert-deftest test-acosh-negacosh-1 ()

  (should (equal 0.0 (hydra_lib_math_acosh 1.0))))

(ert-deftest test-acosh-negacosh-2 ()

  (should (equal 1.31695789692 ((hydra_lib_math_round_float64 12) (hydra_lib_math_acosh 2.0)))))

(ert-deftest test-acosh-negacosh-3 ()

  (should (equal 1.76274717404 ((hydra_lib_math_round_float64 12) (hydra_lib_math_acosh 3.0)))))

;; atanh

(ert-deftest test-atanh-negatanh-0 ()

  (should (equal 0.0 (hydra_lib_math_atanh 0.0))))

(ert-deftest test-atanh-negatanh-0-dot5 ()

  (should (equal 0.549306144334 ((hydra_lib_math_round_float64 12) (hydra_lib_math_atanh 0.5)))))

(ert-deftest test-atanh-negatanh-0-dot1 ()

  (should (equal 0.100335347731 ((hydra_lib_math_round_float64 12) (hydra_lib_math_atanh 0.1)))))

;; exp

(ert-deftest test-exp-negexp-0 ()

  (should (equal 1.0 (hydra_lib_math_exp 0.0))))

(ert-deftest test-exp-negexp-1 ()

  (should (equal 2.718281828459045 (hydra_lib_math_exp 1.0))))

(ert-deftest test-exp-negexp--neg1 ()

  (should (equal 0.36787944117144233 (hydra_lib_math_exp -1.0))))

(ert-deftest test-exp-negexp-2 ()

  (should (equal 7.38905609893 ((hydra_lib_math_round_float64 12) (hydra_lib_math_exp 2.0)))))

(ert-deftest test-exp-negexp-0-dot5 ()

  (should (equal 1.6487212707 ((hydra_lib_math_round_float64 12) (hydra_lib_math_exp 0.5)))))

;; log

(ert-deftest test-log-neglog-1 ()

  (should (equal 0.0 (hydra_lib_math_log 1.0))))

(ert-deftest test-log-neglog-e ()

  (should (equal 1.0 (hydra_lib_math_log 2.718281828459045))))

(ert-deftest test-log-neglog-2 ()

  (should (equal 0.69314718056 ((hydra_lib_math_round_float64 12) (hydra_lib_math_log 2.0)))))

(ert-deftest test-log-neglog-10 ()

  (should (equal 2.30258509299 ((hydra_lib_math_round_float64 12) (hydra_lib_math_log 10.0)))))

;; logBase

(ert-deftest test-logbase-neglog10-1 ()

  (should (equal 0.0 ((hydra_lib_math_log_base 10.0) 1.0))))

(ert-deftest test-logbase-neglog10-10 ()

  (should (equal 1.0 ((hydra_lib_math_log_base 10.0) 10.0))))

(ert-deftest test-logbase-neglog10-100 ()

  (should (equal 2.0 ((hydra_lib_math_log_base 10.0) 100.0))))

(ert-deftest test-logbase-neglog2-8 ()

  (should (equal 3.0 ((hydra_lib_math_log_base 2.0) 8.0))))

(ert-deftest test-logbase-neglog2-10 ()

  (should (equal 3.32192809489 ((hydra_lib_math_round_float64 12) ((hydra_lib_math_log_base 2.0) 10.0)))))

;; pow

(ert-deftest test-pow-neg2-3 ()

  (should (equal 8.0 ((hydra_lib_math_pow 2.0) 3.0))))

(ert-deftest test-pow-neg10-0 ()

  (should (equal 1.0 ((hydra_lib_math_pow 10.0) 0.0))))

(ert-deftest test-pow-neg2--neg1 ()

  (should (equal 0.5 ((hydra_lib_math_pow 2.0) -1.0))))

(ert-deftest test-pow-neg2-0-dot5 ()

  (should (equal 1.41421356237 ((hydra_lib_math_round_float64 12) ((hydra_lib_math_pow 2.0) 0.5)))))

;; sqrt

(ert-deftest test-sqrt-negsqrt-4 ()

  (should (equal 2.0 (hydra_lib_math_sqrt 4.0))))

(ert-deftest test-sqrt-negsqrt-9 ()

  (should (equal 3.0 (hydra_lib_math_sqrt 9.0))))

(ert-deftest test-sqrt-negsqrt-2 ()

  (should (equal 1.4142135623730951 (hydra_lib_math_sqrt 2.0))))

(ert-deftest test-sqrt-negsqrt-0 ()

  (should (equal 0.0 (hydra_lib_math_sqrt 0.0))))

(ert-deftest test-sqrt-negsqrt-3 ()

  (should (equal 1.73205080757 ((hydra_lib_math_round_float64 12) (hydra_lib_math_sqrt 3.0)))))

;; ceiling

(ert-deftest test-ceiling-negceiling-3-dot2 ()

  (should (equal 4 (hydra_lib_math_ceiling 3.2))))

(ert-deftest test-ceiling-negceiling-3-dot0 ()

  (should (equal 3 (hydra_lib_math_ceiling 3.0))))

(ert-deftest test-ceiling-negceiling--neg3-dot2 ()

  (should (equal -3 (hydra_lib_math_ceiling -3.2))))

(ert-deftest test-ceiling-negceiling--neg3-dot0 ()

  (should (equal -3 (hydra_lib_math_ceiling -3.0))))

;; floor

(ert-deftest test-floor-negfloor-3-dot8 ()

  (should (equal 3 (hydra_lib_math_floor 3.8))))

(ert-deftest test-floor-negfloor-3-dot0 ()

  (should (equal 3 (hydra_lib_math_floor 3.0))))

(ert-deftest test-floor-negfloor--neg3-dot2 ()

  (should (equal -4 (hydra_lib_math_floor -3.2))))

(ert-deftest test-floor-negfloor--neg3-dot0 ()

  (should (equal -3 (hydra_lib_math_floor -3.0))))

;; round

(ert-deftest test-round-neground-3-dot4 ()

  (should (equal 3 (hydra_lib_math_round 3.4))))

(ert-deftest test-round-neground-3-dot5 ()

  (should (equal 4 (hydra_lib_math_round 3.5))))

(ert-deftest test-round-neground-3-dot6 ()

  (should (equal 4 (hydra_lib_math_round 3.6))))

(ert-deftest test-round-neground--neg3-dot4 ()

  (should (equal -3 (hydra_lib_math_round -3.4))))

(ert-deftest test-round-neground--neg3-dot5 ()

  (should (equal -4 (hydra_lib_math_round -3.5))))

;; roundBigfloat

(ert-deftest test-roundbigfloat-negzero ()

  (should (equal 0.0 ((hydra_lib_math_round_bigfloat 5) 0.0))))

(ert-deftest test-roundbigfloat-neground-pi-to-4-digits ()

  (should (equal 3.142 ((hydra_lib_math_round_bigfloat 4) 3.141592653589793))))

(ert-deftest test-roundbigfloat-neground-1234-dot5-to-3-digits ()

  (should (equal 1230.0 ((hydra_lib_math_round_bigfloat 3) 1234.5))))

(ert-deftest test-roundbigfloat-neground-0-dot001234-to-2-digits ()

  (should (equal 1.2e-3 ((hydra_lib_math_round_bigfloat 2) 1.234e-3))))

(ert-deftest test-roundbigfloat-negnegative ()

  (should (equal -1230.0 ((hydra_lib_math_round_bigfloat 3) -1234.5))))

;; roundFloat32

(ert-deftest test-roundfloat32-negzero ()

  (should (equal 0.0 ((hydra_lib_math_round_float32 5) 0.0))))

(ert-deftest test-roundfloat32-neground-pi-to-4-digits ()

  (should (equal 3.1419999599456787 ((hydra_lib_math_round_float32 4) 3.1415927410125732))))

(ert-deftest test-roundfloat32-neground-1234-dot5-to-3-digits ()

  (should (equal 1230.0 ((hydra_lib_math_round_float32 3) 1234.5))))

(ert-deftest test-roundfloat32-negnegative ()

  (should (equal -1230.0 ((hydra_lib_math_round_float32 3) -1234.5))))

;; roundFloat64

(ert-deftest test-roundfloat64-negzero ()

  (should (equal 0.0 ((hydra_lib_math_round_float64 5) 0.0))))

(ert-deftest test-roundfloat64-neground-pi-to-4-digits ()

  (should (equal 3.142 ((hydra_lib_math_round_float64 4) 3.141592653589793))))

(ert-deftest test-roundfloat64-neground-pi-to-10-digits ()

  (should (equal 3.141592654 ((hydra_lib_math_round_float64 10) 3.141592653589793))))

(ert-deftest test-roundfloat64-neground-1234-dot5-to-3-digits ()

  (should (equal 1230.0 ((hydra_lib_math_round_float64 3) 1234.5))))

(ert-deftest test-roundfloat64-neground-0-dot001234-to-2-digits ()

  (should (equal 1.2e-3 ((hydra_lib_math_round_float64 2) 1.234e-3))))

(ert-deftest test-roundfloat64-negnegative ()

  (should (equal -1230.0 ((hydra_lib_math_round_float64 3) -1234.5))))

(ert-deftest test-roundfloat64-neground-1-digit ()

  (should (equal 10.0 ((hydra_lib_math_round_float64 1) 9.876))))

;; truncate

(ert-deftest test-truncate-negtruncate-3-dot8 ()

  (should (equal 3 (hydra_lib_math_truncate 3.8))))

(ert-deftest test-truncate-negtruncate-3-dot2 ()

  (should (equal 3 (hydra_lib_math_truncate 3.2))))

(ert-deftest test-truncate-negtruncate--neg3-dot8 ()

  (should (equal -3 (hydra_lib_math_truncate -3.8))))

(ert-deftest test-truncate-negtruncate--neg3-dot2 ()

  (should (equal -3 (hydra_lib_math_truncate -3.2))))
