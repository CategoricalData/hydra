;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.math primitives

;; abs

(defun test-abs-negpositive ()

  (assert (equal 5 (hydra_lib_math_abs 5))))

(defun test-abs-negnegative ()

  (assert (equal 5 (hydra_lib_math_abs -5))))

(defun test-abs-negzero ()

  (assert (equal 0 (hydra_lib_math_abs 0))))

;; add

(defun test-add-negpositive-numbers ()

  (assert (equal 8 ((hydra_lib_math_add 3) 5))))

(defun test-add-negnegative-numbers ()

  (assert (equal -8 ((hydra_lib_math_add -3) -5))))

(defun test-add-negmixed-sign ()

  (assert (equal 7 ((hydra_lib_math_add 10) -3))))

(defun test-add-negwith-zero ()

  (assert (equal 42 ((hydra_lib_math_add 42) 0))))

;; div

(defun test-div-negexact-division ()

  (assert (equal 5 ((hydra_lib_math_div 10) 2))))

(defun test-div-negtruncates-toward-negative-infinity ()

  (assert (equal 3 ((hydra_lib_math_div 10) 3))))

(defun test-div-negnegative-dividend ()

  (assert (equal -4 ((hydra_lib_math_div -10) 3))))

(defun test-div-negnegative-divisor ()

  (assert (equal -4 ((hydra_lib_math_div 10) -3))))

;; even

(defun test-even-negeven-positive ()

  (assert (equal cl:t (hydra_lib_math_even 4))))

(defun test-even-negodd-positive ()

  (assert (equal cl:nil (hydra_lib_math_even 5))))

(defun test-even-negeven-negative ()

  (assert (equal cl:t (hydra_lib_math_even -4))))

(defun test-even-negodd-negative ()

  (assert (equal cl:nil (hydra_lib_math_even -5))))

(defun test-even-negzero ()

  (assert (equal cl:t (hydra_lib_math_even 0))))

;; max

(defun test-max-negfirst-is-larger ()

  (assert (equal 10 ((hydra_lib_math_max 10) 5))))

(defun test-max-negsecond-is-larger ()

  (assert (equal 10 ((hydra_lib_math_max 5) 10))))

(defun test-max-negequal-values ()

  (assert (equal 7 ((hydra_lib_math_max 7) 7))))

(defun test-max-negnegative-numbers ()

  (assert (equal -3 ((hydra_lib_math_max -3) -5))))

(defun test-max-negmixed-sign ()

  (assert (equal 5 ((hydra_lib_math_max -5) 5))))

(defun test-max-negwith-zero ()

  (assert (equal 42 ((hydra_lib_math_max 0) 42))))

;; min

(defun test-min-negfirst-is-smaller ()

  (assert (equal 5 ((hydra_lib_math_min 5) 10))))

(defun test-min-negsecond-is-smaller ()

  (assert (equal 5 ((hydra_lib_math_min 10) 5))))

(defun test-min-negequal-values ()

  (assert (equal 7 ((hydra_lib_math_min 7) 7))))

(defun test-min-negnegative-numbers ()

  (assert (equal -5 ((hydra_lib_math_min -3) -5))))

(defun test-min-negmixed-sign ()

  (assert (equal -5 ((hydra_lib_math_min -5) 5))))

(defun test-min-negwith-zero ()

  (assert (equal 0 ((hydra_lib_math_min 0) 42))))

;; mod

(defun test-mod-negbasic-modulo ()

  (assert (equal 1 ((hydra_lib_math_mod 10) 3))))

(defun test-mod-negexact-division ()

  (assert (equal 0 ((hydra_lib_math_mod 10) 2))))

(defun test-mod-negnegative-dividend ()

  (assert (equal 2 ((hydra_lib_math_mod -10) 3))))

(defun test-mod-negnegative-divisor ()

  (assert (equal -2 ((hydra_lib_math_mod 10) -3))))

;; mul

(defun test-mul-negpositive-numbers ()

  (assert (equal 15 ((hydra_lib_math_mul 3) 5))))

(defun test-mul-negnegative-numbers ()

  (assert (equal 15 ((hydra_lib_math_mul -3) -5))))

(defun test-mul-negmixed-sign ()

  (assert (equal -15 ((hydra_lib_math_mul 3) -5))))

(defun test-mul-negwith-zero ()

  (assert (equal 0 ((hydra_lib_math_mul 42) 0))))

(defun test-mul-negwith-one ()

  (assert (equal 42 ((hydra_lib_math_mul 42) 1))))

;; negate

(defun test-negate-negpositive ()

  (assert (equal -5 (hydra_lib_math_negate 5))))

(defun test-negate-negnegative ()

  (assert (equal 5 (hydra_lib_math_negate -5))))

(defun test-negate-negzero ()

  (assert (equal 0 (hydra_lib_math_negate 0))))

;; odd

(defun test-odd-negodd-positive ()

  (assert (equal cl:t (hydra_lib_math_odd 5))))

(defun test-odd-negeven-positive ()

  (assert (equal cl:nil (hydra_lib_math_odd 4))))

(defun test-odd-negodd-negative ()

  (assert (equal cl:t (hydra_lib_math_odd -5))))

(defun test-odd-negeven-negative ()

  (assert (equal cl:nil (hydra_lib_math_odd -4))))

(defun test-odd-negzero ()

  (assert (equal cl:nil (hydra_lib_math_odd 0))))

;; pred

(defun test-pred-negpositive ()

  (assert (equal 4 (hydra_lib_math_pred 5))))

(defun test-pred-negzero ()

  (assert (equal -1 (hydra_lib_math_pred 0))))

(defun test-pred-negnegative ()

  (assert (equal -6 (hydra_lib_math_pred -5))))

;; range

(defun test-range-negascending-range ()

  (assert (equal (list 1 2 3 4 5) ((hydra_lib_math_range 1) 5))))

(defun test-range-negsingle-element ()

  (assert (equal (list 5) ((hydra_lib_math_range 5) 5))))

(defun test-range-negtwo-elements ()

  (assert (equal (list 3 4) ((hydra_lib_math_range 3) 4))))

(defun test-range-negnegative-start ()

  (assert (equal (list -2 -1 0 1 2) ((hydra_lib_math_range -2) 2))))

;; rem

(defun test-rem-negbasic-remainder ()

  (assert (equal 1 ((hydra_lib_math_rem 10) 3))))

(defun test-rem-negexact-division ()

  (assert (equal 0 ((hydra_lib_math_rem 10) 2))))

(defun test-rem-negnegative-dividend ()

  (assert (equal -1 ((hydra_lib_math_rem -10) 3))))

(defun test-rem-negnegative-divisor ()

  (assert (equal 1 ((hydra_lib_math_rem 10) -3))))

;; signum

(defun test-signum-negpositive ()

  (assert (equal 1 (hydra_lib_math_signum 5))))

(defun test-signum-negnegative ()

  (assert (equal -1 (hydra_lib_math_signum -5))))

(defun test-signum-negzero ()

  (assert (equal 0 (hydra_lib_math_signum 0))))

;; sub

(defun test-sub-negpositive-numbers ()

  (assert (equal 7 ((hydra_lib_math_sub 10) 3))))

(defun test-sub-negnegative-numbers ()

  (assert (equal -7 ((hydra_lib_math_sub -10) -3))))

(defun test-sub-negmixed-sign ()

  (assert (equal 13 ((hydra_lib_math_sub 10) -3))))

(defun test-sub-negwith-zero ()

  (assert (equal 42 ((hydra_lib_math_sub 42) 0))))

;; succ

(defun test-succ-negpositive ()

  (assert (equal 6 (hydra_lib_math_succ 5))))

(defun test-succ-negzero ()

  (assert (equal 1 (hydra_lib_math_succ 0))))

(defun test-succ-negnegative ()

  (assert (equal -4 (hydra_lib_math_succ -5))))

;; e

(defun test-e-negeuler-s-number ()

  (assert (equal 2.718281828459045 hydra_lib_math_e)))

;; pi

(defun test-pi-negpi-constant ()

  (assert (equal 3.141592653589793 hydra_lib_math_pi)))

;; sin

(defun test-sin-negsin-0 ()

  (assert (equal 0.0 (hydra_lib_math_sin 0.0))))

(defun test-sin-negsin-pi-div2 ()

  (assert (equal 1.0 (hydra_lib_math_sin 1.5707963267948966))))

(defun test-sin-negsin-pi ()

  (assert (equal 1.2246467991473532e-16 (hydra_lib_math_sin 3.141592653589793))))

(defun test-sin-negsin-1 ()

  (assert (equal 0.841470984808 ((hydra_lib_math_round_float64 12) (hydra_lib_math_sin 1.0)))))

(defun test-sin-negsin-0-dot5 ()

  (assert (equal 0.479425538604 ((hydra_lib_math_round_float64 12) (hydra_lib_math_sin 0.5)))))

;; cos

(defun test-cos-negcos-0 ()

  (assert (equal 1.0 (hydra_lib_math_cos 0.0))))

(defun test-cos-negcos-pi-div2 ()

  (assert (equal 6.123233995736766e-17 (hydra_lib_math_cos 1.5707963267948966))))

(defun test-cos-negcos-pi ()

  (assert (equal -1.0 (hydra_lib_math_cos 3.141592653589793))))

(defun test-cos-negcos-1 ()

  (assert (equal 0.540302305868 ((hydra_lib_math_round_float64 12) (hydra_lib_math_cos 1.0)))))

(defun test-cos-negcos-0-dot5 ()

  (assert (equal 0.87758256189 ((hydra_lib_math_round_float64 12) (hydra_lib_math_cos 0.5)))))

;; tan

(defun test-tan-negtan-0 ()

  (assert (equal 0.0 (hydra_lib_math_tan 0.0))))

(defun test-tan-negtan-pi-div4 ()

  (assert (equal 0.9999999999999999 (hydra_lib_math_tan 0.7853981633974483))))

(defun test-tan-negtan-1 ()

  (assert (equal 1.55740772465 ((hydra_lib_math_round_float64 12) (hydra_lib_math_tan 1.0)))))

(defun test-tan-negtan-0-dot5 ()

  (assert (equal 0.546302489844 ((hydra_lib_math_round_float64 12) (hydra_lib_math_tan 0.5)))))

;; asin

(defun test-asin-negasin-0 ()

  (assert (equal 0.0 (hydra_lib_math_asin 0.0))))

(defun test-asin-negasin-1 ()

  (assert (equal 1.5707963267948966 (hydra_lib_math_asin 1.0))))

(defun test-asin-negasin--neg1 ()

  (assert (equal -1.5707963267948966 (hydra_lib_math_asin -1.0))))

(defun test-asin-negasin-0-dot5 ()

  (assert (equal 0.523598775598 ((hydra_lib_math_round_float64 12) (hydra_lib_math_asin 0.5)))))

;; acos

(defun test-acos-negacos-1 ()

  (assert (equal 0.0 (hydra_lib_math_acos 1.0))))

(defun test-acos-negacos-0 ()

  (assert (equal 1.5707963267948966 (hydra_lib_math_acos 0.0))))

(defun test-acos-negacos--neg1 ()

  (assert (equal 3.141592653589793 (hydra_lib_math_acos -1.0))))

(defun test-acos-negacos-0-dot5 ()

  (assert (equal 1.0471975512 ((hydra_lib_math_round_float64 12) (hydra_lib_math_acos 0.5)))))

;; atan

(defun test-atan-negatan-0 ()

  (assert (equal 0.0 (hydra_lib_math_atan 0.0))))

(defun test-atan-negatan-1 ()

  (assert (equal 0.7853981633974483 (hydra_lib_math_atan 1.0))))

(defun test-atan-negatan-0-dot5 ()

  (assert (equal 0.463647609001 ((hydra_lib_math_round_float64 12) (hydra_lib_math_atan 0.5)))))

;; atan2

(defun test-atan2-negatan2-1-1 ()

  (assert (equal 0.7853981633974483 ((hydra_lib_math_atan2 1.0) 1.0))))

(defun test-atan2-negatan2-1-0 ()

  (assert (equal 1.5707963267948966 ((hydra_lib_math_atan2 1.0) 0.0))))

(defun test-atan2-negatan2-0-1 ()

  (assert (equal 0.0 ((hydra_lib_math_atan2 0.0) 1.0))))

(defun test-atan2-negatan2-3-4 ()

  (assert (equal 0.643501108793 ((hydra_lib_math_round_float64 12) ((hydra_lib_math_atan2 3.0) 4.0)))))

;; sinh

(defun test-sinh-negsinh-0 ()

  (assert (equal 0.0 (hydra_lib_math_sinh 0.0))))

(defun test-sinh-negsinh-1 ()

  (assert (equal 1.1752011936438014 (hydra_lib_math_sinh 1.0))))

(defun test-sinh-negsinh-2 ()

  (assert (equal 3.62686040785 ((hydra_lib_math_round_float64 12) (hydra_lib_math_sinh 2.0)))))

;; cosh

(defun test-cosh-negcosh-0 ()

  (assert (equal 1.0 (hydra_lib_math_cosh 0.0))))

(defun test-cosh-negcosh-1 ()

  (assert (equal 1.54308063482 ((hydra_lib_math_round_float64 12) (hydra_lib_math_cosh 1.0)))))

(defun test-cosh-negcosh-2 ()

  (assert (equal 3.76219569108 ((hydra_lib_math_round_float64 12) (hydra_lib_math_cosh 2.0)))))

;; tanh

(defun test-tanh-negtanh-0 ()

  (assert (equal 0.0 (hydra_lib_math_tanh 0.0))))

(defun test-tanh-negtanh-1 ()

  (assert (equal 0.7615941559557649 (hydra_lib_math_tanh 1.0))))

(defun test-tanh-negtanh-0-dot5 ()

  (assert (equal 0.46211715726 ((hydra_lib_math_round_float64 12) (hydra_lib_math_tanh 0.5)))))

;; asinh

(defun test-asinh-negasinh-0 ()

  (assert (equal 0.0 (hydra_lib_math_asinh 0.0))))

(defun test-asinh-negasinh-1 ()

  (assert (equal 0.88137358702 ((hydra_lib_math_round_float64 12) (hydra_lib_math_asinh 1.0)))))

(defun test-asinh-negasinh-0-dot5 ()

  (assert (equal 0.48121182506 ((hydra_lib_math_round_float64 12) (hydra_lib_math_asinh 0.5)))))

;; acosh

(defun test-acosh-negacosh-1 ()

  (assert (equal 0.0 (hydra_lib_math_acosh 1.0))))

(defun test-acosh-negacosh-2 ()

  (assert (equal 1.31695789692 ((hydra_lib_math_round_float64 12) (hydra_lib_math_acosh 2.0)))))

(defun test-acosh-negacosh-3 ()

  (assert (equal 1.76274717404 ((hydra_lib_math_round_float64 12) (hydra_lib_math_acosh 3.0)))))

;; atanh

(defun test-atanh-negatanh-0 ()

  (assert (equal 0.0 (hydra_lib_math_atanh 0.0))))

(defun test-atanh-negatanh-0-dot5 ()

  (assert (equal 0.549306144334 ((hydra_lib_math_round_float64 12) (hydra_lib_math_atanh 0.5)))))

(defun test-atanh-negatanh-0-dot1 ()

  (assert (equal 0.100335347731 ((hydra_lib_math_round_float64 12) (hydra_lib_math_atanh 0.1)))))

;; exp

(defun test-exp-negexp-0 ()

  (assert (equal 1.0 (hydra_lib_math_exp 0.0))))

(defun test-exp-negexp-1 ()

  (assert (equal 2.71828182846 ((hydra_lib_math_round_float64 12) (hydra_lib_math_exp 1.0)))))

(defun test-exp-negexp--neg1 ()

  (assert (equal 0.367879441171 ((hydra_lib_math_round_float64 12) (hydra_lib_math_exp -1.0)))))

(defun test-exp-negexp-2 ()

  (assert (equal 7.38905609893 ((hydra_lib_math_round_float64 12) (hydra_lib_math_exp 2.0)))))

(defun test-exp-negexp-0-dot5 ()

  (assert (equal 1.6487212707 ((hydra_lib_math_round_float64 12) (hydra_lib_math_exp 0.5)))))

;; log

(defun test-log-neglog-1 ()

  (assert (equal 0.0 (hydra_lib_math_log 1.0))))

(defun test-log-neglog-e ()

  (assert (equal 1.0 (hydra_lib_math_log 2.718281828459045))))

(defun test-log-neglog-2 ()

  (assert (equal 0.69314718056 ((hydra_lib_math_round_float64 12) (hydra_lib_math_log 2.0)))))

(defun test-log-neglog-10 ()

  (assert (equal 2.30258509299 ((hydra_lib_math_round_float64 12) (hydra_lib_math_log 10.0)))))

;; logBase

(defun test-logbase-neglog10-1 ()

  (assert (equal 0.0 ((hydra_lib_math_log_base 10.0) 1.0))))

(defun test-logbase-neglog10-10 ()

  (assert (equal 1.0 ((hydra_lib_math_log_base 10.0) 10.0))))

(defun test-logbase-neglog10-100 ()

  (assert (equal 2.0 ((hydra_lib_math_log_base 10.0) 100.0))))

(defun test-logbase-neglog2-8 ()

  (assert (equal 3.0 ((hydra_lib_math_log_base 2.0) 8.0))))

(defun test-logbase-neglog2-10 ()

  (assert (equal 3.32192809489 ((hydra_lib_math_round_float64 12) ((hydra_lib_math_log_base 2.0) 10.0)))))

;; pow

(defun test-pow-neg2-3 ()

  (assert (equal 8.0 ((hydra_lib_math_pow 2.0) 3.0))))

(defun test-pow-neg10-0 ()

  (assert (equal 1.0 ((hydra_lib_math_pow 10.0) 0.0))))

(defun test-pow-neg2--neg1 ()

  (assert (equal 0.5 ((hydra_lib_math_pow 2.0) -1.0))))

(defun test-pow-neg2-0-dot5 ()

  (assert (equal 1.41421356237 ((hydra_lib_math_round_float64 12) ((hydra_lib_math_pow 2.0) 0.5)))))

;; sqrt

(defun test-sqrt-negsqrt-4 ()

  (assert (equal 2.0 (hydra_lib_math_sqrt 4.0))))

(defun test-sqrt-negsqrt-9 ()

  (assert (equal 3.0 (hydra_lib_math_sqrt 9.0))))

(defun test-sqrt-negsqrt-2 ()

  (assert (equal 1.4142135623730951 (hydra_lib_math_sqrt 2.0))))

(defun test-sqrt-negsqrt-0 ()

  (assert (equal 0.0 (hydra_lib_math_sqrt 0.0))))

(defun test-sqrt-negsqrt-3 ()

  (assert (equal 1.73205080757 ((hydra_lib_math_round_float64 12) (hydra_lib_math_sqrt 3.0)))))

;; ceiling

(defun test-ceiling-negceiling-3-dot2 ()

  (assert (equal 4 (hydra_lib_math_ceiling 3.2))))

(defun test-ceiling-negceiling-3-dot0 ()

  (assert (equal 3 (hydra_lib_math_ceiling 3.0))))

(defun test-ceiling-negceiling--neg3-dot2 ()

  (assert (equal -3 (hydra_lib_math_ceiling -3.2))))

(defun test-ceiling-negceiling--neg3-dot0 ()

  (assert (equal -3 (hydra_lib_math_ceiling -3.0))))

;; floor

(defun test-floor-negfloor-3-dot8 ()

  (assert (equal 3 (hydra_lib_math_floor 3.8))))

(defun test-floor-negfloor-3-dot0 ()

  (assert (equal 3 (hydra_lib_math_floor 3.0))))

(defun test-floor-negfloor--neg3-dot2 ()

  (assert (equal -4 (hydra_lib_math_floor -3.2))))

(defun test-floor-negfloor--neg3-dot0 ()

  (assert (equal -3 (hydra_lib_math_floor -3.0))))

;; round

(defun test-round-neground-3-dot4 ()

  (assert (equal 3 (hydra_lib_math_round 3.4))))

(defun test-round-neground-3-dot5 ()

  (assert (equal 4 (hydra_lib_math_round 3.5))))

(defun test-round-neground-3-dot6 ()

  (assert (equal 4 (hydra_lib_math_round 3.6))))

(defun test-round-neground--neg3-dot4 ()

  (assert (equal -3 (hydra_lib_math_round -3.4))))

(defun test-round-neground--neg3-dot5 ()

  (assert (equal -4 (hydra_lib_math_round -3.5))))

;; roundBigfloat

(defun test-roundbigfloat-negzero ()

  (assert (equal 0.0 ((hydra_lib_math_round_bigfloat 5) 0.0))))

(defun test-roundbigfloat-neground-pi-to-4-digits ()

  (assert (equal 3.142 ((hydra_lib_math_round_bigfloat 4) 3.141592653589793))))

(defun test-roundbigfloat-neground-1234-dot5-to-3-digits ()

  (assert (equal 1230.0 ((hydra_lib_math_round_bigfloat 3) 1234.5))))

(defun test-roundbigfloat-neground-0-dot001234-to-2-digits ()

  (assert (equal 1.2e-3 ((hydra_lib_math_round_bigfloat 2) 1.234e-3))))

(defun test-roundbigfloat-negnegative ()

  (assert (equal -1230.0 ((hydra_lib_math_round_bigfloat 3) -1234.5))))

;; roundFloat32

(defun test-roundfloat32-negzero ()

  (assert (equal 0.0 ((hydra_lib_math_round_float32 5) 0.0))))

(defun test-roundfloat32-neground-pi-to-4-digits ()

  (assert (equal 3.1419999599456787 ((hydra_lib_math_round_float32 4) 3.1415927410125732))))

(defun test-roundfloat32-neground-1234-dot5-to-3-digits ()

  (assert (equal 1230.0 ((hydra_lib_math_round_float32 3) 1234.5))))

(defun test-roundfloat32-negnegative ()

  (assert (equal -1230.0 ((hydra_lib_math_round_float32 3) -1234.5))))

;; roundFloat64

(defun test-roundfloat64-negzero ()

  (assert (equal 0.0 ((hydra_lib_math_round_float64 5) 0.0))))

(defun test-roundfloat64-neground-pi-to-4-digits ()

  (assert (equal 3.142 ((hydra_lib_math_round_float64 4) 3.141592653589793))))

(defun test-roundfloat64-neground-pi-to-10-digits ()

  (assert (equal 3.141592654 ((hydra_lib_math_round_float64 10) 3.141592653589793))))

(defun test-roundfloat64-neground-1234-dot5-to-3-digits ()

  (assert (equal 1230.0 ((hydra_lib_math_round_float64 3) 1234.5))))

(defun test-roundfloat64-neground-0-dot001234-to-2-digits ()

  (assert (equal 1.2e-3 ((hydra_lib_math_round_float64 2) 1.234e-3))))

(defun test-roundfloat64-negnegative ()

  (assert (equal -1230.0 ((hydra_lib_math_round_float64 3) -1234.5))))

(defun test-roundfloat64-neground-1-digit ()

  (assert (equal 10.0 ((hydra_lib_math_round_float64 1) 9.876))))

;; truncate

(defun test-truncate-negtruncate-3-dot8 ()

  (assert (equal 3 (hydra_lib_math_truncate 3.8))))

(defun test-truncate-negtruncate-3-dot2 ()

  (assert (equal 3 (hydra_lib_math_truncate 3.2))))

(defun test-truncate-negtruncate--neg3-dot8 ()

  (assert (equal -3 (hydra_lib_math_truncate -3.8))))

(defun test-truncate-negtruncate--neg3-dot2 ()

  (assert (equal -3 (hydra_lib_math_truncate -3.2))))
