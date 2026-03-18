;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.math primitives

(import (scheme base))

;; abs

(define (test-abs-negpositive)

  (assert (equal? 5 (hydra_lib_math_abs 5))))

(define (test-abs-negnegative)

  (assert (equal? 5 (hydra_lib_math_abs -5))))

(define (test-abs-negzero)

  (assert (equal? 0 (hydra_lib_math_abs 0))))

;; add

(define (test-add-negpositive-numbers)

  (assert (equal? 8 ((hydra_lib_math_add 3) 5))))

(define (test-add-negnegative-numbers)

  (assert (equal? -8 ((hydra_lib_math_add -3) -5))))

(define (test-add-negmixed-sign)

  (assert (equal? 7 ((hydra_lib_math_add 10) -3))))

(define (test-add-negwith-zero)

  (assert (equal? 42 ((hydra_lib_math_add 42) 0))))

;; div

(define (test-div-negexact-division)

  (assert (equal? 5 ((hydra_lib_math_div 10) 2))))

(define (test-div-negtruncates-toward-negative-infinity)

  (assert (equal? 3 ((hydra_lib_math_div 10) 3))))

(define (test-div-negnegative-dividend)

  (assert (equal? -4 ((hydra_lib_math_div -10) 3))))

(define (test-div-negnegative-divisor)

  (assert (equal? -4 ((hydra_lib_math_div 10) -3))))

;; even

(define (test-even-negeven-positive)

  (assert (equal? #t (hydra_lib_math_even 4))))

(define (test-even-negodd-positive)

  (assert (equal? #f (hydra_lib_math_even 5))))

(define (test-even-negeven-negative)

  (assert (equal? #t (hydra_lib_math_even -4))))

(define (test-even-negodd-negative)

  (assert (equal? #f (hydra_lib_math_even -5))))

(define (test-even-negzero)

  (assert (equal? #t (hydra_lib_math_even 0))))

;; max

(define (test-max-negfirst-is-larger)

  (assert (equal? 10 ((hydra_lib_math_max 10) 5))))

(define (test-max-negsecond-is-larger)

  (assert (equal? 10 ((hydra_lib_math_max 5) 10))))

(define (test-max-negequal-values)

  (assert (equal? 7 ((hydra_lib_math_max 7) 7))))

(define (test-max-negnegative-numbers)

  (assert (equal? -3 ((hydra_lib_math_max -3) -5))))

(define (test-max-negmixed-sign)

  (assert (equal? 5 ((hydra_lib_math_max -5) 5))))

(define (test-max-negwith-zero)

  (assert (equal? 42 ((hydra_lib_math_max 0) 42))))

;; min

(define (test-min-negfirst-is-smaller)

  (assert (equal? 5 ((hydra_lib_math_min 5) 10))))

(define (test-min-negsecond-is-smaller)

  (assert (equal? 5 ((hydra_lib_math_min 10) 5))))

(define (test-min-negequal-values)

  (assert (equal? 7 ((hydra_lib_math_min 7) 7))))

(define (test-min-negnegative-numbers)

  (assert (equal? -5 ((hydra_lib_math_min -3) -5))))

(define (test-min-negmixed-sign)

  (assert (equal? -5 ((hydra_lib_math_min -5) 5))))

(define (test-min-negwith-zero)

  (assert (equal? 0 ((hydra_lib_math_min 0) 42))))

;; mod

(define (test-mod-negbasic-modulo)

  (assert (equal? 1 ((hydra_lib_math_mod 10) 3))))

(define (test-mod-negexact-division)

  (assert (equal? 0 ((hydra_lib_math_mod 10) 2))))

(define (test-mod-negnegative-dividend)

  (assert (equal? 2 ((hydra_lib_math_mod -10) 3))))

(define (test-mod-negnegative-divisor)

  (assert (equal? -2 ((hydra_lib_math_mod 10) -3))))

;; mul

(define (test-mul-negpositive-numbers)

  (assert (equal? 15 ((hydra_lib_math_mul 3) 5))))

(define (test-mul-negnegative-numbers)

  (assert (equal? 15 ((hydra_lib_math_mul -3) -5))))

(define (test-mul-negmixed-sign)

  (assert (equal? -15 ((hydra_lib_math_mul 3) -5))))

(define (test-mul-negwith-zero)

  (assert (equal? 0 ((hydra_lib_math_mul 42) 0))))

(define (test-mul-negwith-one)

  (assert (equal? 42 ((hydra_lib_math_mul 42) 1))))

;; negate

(define (test-negate-negpositive)

  (assert (equal? -5 (hydra_lib_math_negate 5))))

(define (test-negate-negnegative)

  (assert (equal? 5 (hydra_lib_math_negate -5))))

(define (test-negate-negzero)

  (assert (equal? 0 (hydra_lib_math_negate 0))))

;; odd

(define (test-odd-negodd-positive)

  (assert (equal? #t (hydra_lib_math_odd 5))))

(define (test-odd-negeven-positive)

  (assert (equal? #f (hydra_lib_math_odd 4))))

(define (test-odd-negodd-negative)

  (assert (equal? #t (hydra_lib_math_odd -5))))

(define (test-odd-negeven-negative)

  (assert (equal? #f (hydra_lib_math_odd -4))))

(define (test-odd-negzero)

  (assert (equal? #f (hydra_lib_math_odd 0))))

;; pred

(define (test-pred-negpositive)

  (assert (equal? 4 (hydra_lib_math_pred 5))))

(define (test-pred-negzero)

  (assert (equal? -1 (hydra_lib_math_pred 0))))

(define (test-pred-negnegative)

  (assert (equal? -6 (hydra_lib_math_pred -5))))

;; range

(define (test-range-negascending-range)

  (assert (equal? (list 1 2 3 4 5) ((hydra_lib_math_range 1) 5))))

(define (test-range-negsingle-element)

  (assert (equal? (list 5) ((hydra_lib_math_range 5) 5))))

(define (test-range-negtwo-elements)

  (assert (equal? (list 3 4) ((hydra_lib_math_range 3) 4))))

(define (test-range-negnegative-start)

  (assert (equal? (list -2 -1 0 1 2) ((hydra_lib_math_range -2) 2))))

;; rem

(define (test-rem-negbasic-remainder)

  (assert (equal? 1 ((hydra_lib_math_rem 10) 3))))

(define (test-rem-negexact-division)

  (assert (equal? 0 ((hydra_lib_math_rem 10) 2))))

(define (test-rem-negnegative-dividend)

  (assert (equal? -1 ((hydra_lib_math_rem -10) 3))))

(define (test-rem-negnegative-divisor)

  (assert (equal? 1 ((hydra_lib_math_rem 10) -3))))

;; signum

(define (test-signum-negpositive)

  (assert (equal? 1 (hydra_lib_math_signum 5))))

(define (test-signum-negnegative)

  (assert (equal? -1 (hydra_lib_math_signum -5))))

(define (test-signum-negzero)

  (assert (equal? 0 (hydra_lib_math_signum 0))))

;; sub

(define (test-sub-negpositive-numbers)

  (assert (equal? 7 ((hydra_lib_math_sub 10) 3))))

(define (test-sub-negnegative-numbers)

  (assert (equal? -7 ((hydra_lib_math_sub -10) -3))))

(define (test-sub-negmixed-sign)

  (assert (equal? 13 ((hydra_lib_math_sub 10) -3))))

(define (test-sub-negwith-zero)

  (assert (equal? 42 ((hydra_lib_math_sub 42) 0))))

;; succ

(define (test-succ-negpositive)

  (assert (equal? 6 (hydra_lib_math_succ 5))))

(define (test-succ-negzero)

  (assert (equal? 1 (hydra_lib_math_succ 0))))

(define (test-succ-negnegative)

  (assert (equal? -4 (hydra_lib_math_succ -5))))

;; e

(define (test-e-negeuler-s-number)

  (assert (equal? 2.718281828459045 hydra_lib_math_e)))

;; pi

(define (test-pi-negpi-constant)

  (assert (equal? 3.141592653589793 hydra_lib_math_pi)))

;; sin

(define (test-sin-negsin-0)

  (assert (equal? 0.0 (hydra_lib_math_sin 0.0))))

(define (test-sin-negsin-pi-div2)

  (assert (equal? 1.0 (hydra_lib_math_sin 1.5707963267948966))))

(define (test-sin-negsin-pi)

  (assert (equal? 1.2246467991473532e-16 (hydra_lib_math_sin 3.141592653589793))))

(define (test-sin-negsin-1)

  (assert (equal? 0.841470984808 ((hydra_lib_math_round_float64 12) (hydra_lib_math_sin 1.0)))))

(define (test-sin-negsin-0-dot5)

  (assert (equal? 0.479425538604 ((hydra_lib_math_round_float64 12) (hydra_lib_math_sin 0.5)))))

;; cos

(define (test-cos-negcos-0)

  (assert (equal? 1.0 (hydra_lib_math_cos 0.0))))

(define (test-cos-negcos-pi-div2)

  (assert (equal? 6.123233995736766e-17 (hydra_lib_math_cos 1.5707963267948966))))

(define (test-cos-negcos-pi)

  (assert (equal? -1.0 (hydra_lib_math_cos 3.141592653589793))))

(define (test-cos-negcos-1)

  (assert (equal? 0.540302305868 ((hydra_lib_math_round_float64 12) (hydra_lib_math_cos 1.0)))))

(define (test-cos-negcos-0-dot5)

  (assert (equal? 0.87758256189 ((hydra_lib_math_round_float64 12) (hydra_lib_math_cos 0.5)))))

;; tan

(define (test-tan-negtan-0)

  (assert (equal? 0.0 (hydra_lib_math_tan 0.0))))

(define (test-tan-negtan-pi-div4)

  (assert (equal? 0.9999999999999999 (hydra_lib_math_tan 0.7853981633974483))))

(define (test-tan-negtan-1)

  (assert (equal? 1.55740772465 ((hydra_lib_math_round_float64 12) (hydra_lib_math_tan 1.0)))))

(define (test-tan-negtan-0-dot5)

  (assert (equal? 0.546302489844 ((hydra_lib_math_round_float64 12) (hydra_lib_math_tan 0.5)))))

;; asin

(define (test-asin-negasin-0)

  (assert (equal? 0.0 (hydra_lib_math_asin 0.0))))

(define (test-asin-negasin-1)

  (assert (equal? 1.5707963267948966 (hydra_lib_math_asin 1.0))))

(define (test-asin-negasin--neg1)

  (assert (equal? -1.5707963267948966 (hydra_lib_math_asin -1.0))))

(define (test-asin-negasin-0-dot5)

  (assert (equal? 0.523598775598 ((hydra_lib_math_round_float64 12) (hydra_lib_math_asin 0.5)))))

;; acos

(define (test-acos-negacos-1)

  (assert (equal? 0.0 (hydra_lib_math_acos 1.0))))

(define (test-acos-negacos-0)

  (assert (equal? 1.5707963267948966 (hydra_lib_math_acos 0.0))))

(define (test-acos-negacos--neg1)

  (assert (equal? 3.141592653589793 (hydra_lib_math_acos -1.0))))

(define (test-acos-negacos-0-dot5)

  (assert (equal? 1.0471975512 ((hydra_lib_math_round_float64 12) (hydra_lib_math_acos 0.5)))))

;; atan

(define (test-atan-negatan-0)

  (assert (equal? 0.0 (hydra_lib_math_atan 0.0))))

(define (test-atan-negatan-1)

  (assert (equal? 0.7853981633974483 (hydra_lib_math_atan 1.0))))

(define (test-atan-negatan-0-dot5)

  (assert (equal? 0.463647609001 ((hydra_lib_math_round_float64 12) (hydra_lib_math_atan 0.5)))))

;; atan2

(define (test-atan2-negatan2-1-1)

  (assert (equal? 0.7853981633974483 ((hydra_lib_math_atan2 1.0) 1.0))))

(define (test-atan2-negatan2-1-0)

  (assert (equal? 1.5707963267948966 ((hydra_lib_math_atan2 1.0) 0.0))))

(define (test-atan2-negatan2-0-1)

  (assert (equal? 0.0 ((hydra_lib_math_atan2 0.0) 1.0))))

(define (test-atan2-negatan2-3-4)

  (assert (equal? 0.643501108793 ((hydra_lib_math_round_float64 12) ((hydra_lib_math_atan2 3.0) 4.0)))))

;; sinh

(define (test-sinh-negsinh-0)

  (assert (equal? 0.0 (hydra_lib_math_sinh 0.0))))

(define (test-sinh-negsinh-1)

  (assert (equal? 1.1752011936438014 (hydra_lib_math_sinh 1.0))))

(define (test-sinh-negsinh-2)

  (assert (equal? 3.62686040785 ((hydra_lib_math_round_float64 12) (hydra_lib_math_sinh 2.0)))))

;; cosh

(define (test-cosh-negcosh-0)

  (assert (equal? 1.0 (hydra_lib_math_cosh 0.0))))

(define (test-cosh-negcosh-1)

  (assert (equal? 1.5430806348152437 (hydra_lib_math_cosh 1.0))))

(define (test-cosh-negcosh-2)

  (assert (equal? 3.76219569108 ((hydra_lib_math_round_float64 12) (hydra_lib_math_cosh 2.0)))))

;; tanh

(define (test-tanh-negtanh-0)

  (assert (equal? 0.0 (hydra_lib_math_tanh 0.0))))

(define (test-tanh-negtanh-1)

  (assert (equal? 0.7615941559557649 (hydra_lib_math_tanh 1.0))))

(define (test-tanh-negtanh-0-dot5)

  (assert (equal? 0.46211715726 ((hydra_lib_math_round_float64 12) (hydra_lib_math_tanh 0.5)))))

;; asinh

(define (test-asinh-negasinh-0)

  (assert (equal? 0.0 (hydra_lib_math_asinh 0.0))))

(define (test-asinh-negasinh-1)

  (assert (equal? 0.88137358702 ((hydra_lib_math_round_float64 12) (hydra_lib_math_asinh 1.0)))))

(define (test-asinh-negasinh-0-dot5)

  (assert (equal? 0.48121182506 ((hydra_lib_math_round_float64 12) (hydra_lib_math_asinh 0.5)))))

;; acosh

(define (test-acosh-negacosh-1)

  (assert (equal? 0.0 (hydra_lib_math_acosh 1.0))))

(define (test-acosh-negacosh-2)

  (assert (equal? 1.31695789692 ((hydra_lib_math_round_float64 12) (hydra_lib_math_acosh 2.0)))))

(define (test-acosh-negacosh-3)

  (assert (equal? 1.76274717404 ((hydra_lib_math_round_float64 12) (hydra_lib_math_acosh 3.0)))))

;; atanh

(define (test-atanh-negatanh-0)

  (assert (equal? 0.0 (hydra_lib_math_atanh 0.0))))

(define (test-atanh-negatanh-0-dot5)

  (assert (equal? 0.549306144334 ((hydra_lib_math_round_float64 12) (hydra_lib_math_atanh 0.5)))))

(define (test-atanh-negatanh-0-dot1)

  (assert (equal? 0.100335347731 ((hydra_lib_math_round_float64 12) (hydra_lib_math_atanh 0.1)))))

;; exp

(define (test-exp-negexp-0)

  (assert (equal? 1.0 (hydra_lib_math_exp 0.0))))

(define (test-exp-negexp-1)

  (assert (equal? 2.718281828459045 (hydra_lib_math_exp 1.0))))

(define (test-exp-negexp--neg1)

  (assert (equal? 0.36787944117144233 (hydra_lib_math_exp -1.0))))

(define (test-exp-negexp-2)

  (assert (equal? 7.38905609893 ((hydra_lib_math_round_float64 12) (hydra_lib_math_exp 2.0)))))

(define (test-exp-negexp-0-dot5)

  (assert (equal? 1.6487212707 ((hydra_lib_math_round_float64 12) (hydra_lib_math_exp 0.5)))))

;; log

(define (test-log-neglog-1)

  (assert (equal? 0.0 (hydra_lib_math_log 1.0))))

(define (test-log-neglog-e)

  (assert (equal? 1.0 (hydra_lib_math_log 2.718281828459045))))

(define (test-log-neglog-2)

  (assert (equal? 0.69314718056 ((hydra_lib_math_round_float64 12) (hydra_lib_math_log 2.0)))))

(define (test-log-neglog-10)

  (assert (equal? 2.30258509299 ((hydra_lib_math_round_float64 12) (hydra_lib_math_log 10.0)))))

;; logBase

(define (test-logbase-neglog10-1)

  (assert (equal? 0.0 ((hydra_lib_math_log_base 10.0) 1.0))))

(define (test-logbase-neglog10-10)

  (assert (equal? 1.0 ((hydra_lib_math_log_base 10.0) 10.0))))

(define (test-logbase-neglog10-100)

  (assert (equal? 2.0 ((hydra_lib_math_log_base 10.0) 100.0))))

(define (test-logbase-neglog2-8)

  (assert (equal? 3.0 ((hydra_lib_math_log_base 2.0) 8.0))))

(define (test-logbase-neglog2-10)

  (assert (equal? 3.32192809489 ((hydra_lib_math_round_float64 12) ((hydra_lib_math_log_base 2.0) 10.0)))))

;; pow

(define (test-pow-neg2-3)

  (assert (equal? 8.0 ((hydra_lib_math_pow 2.0) 3.0))))

(define (test-pow-neg10-0)

  (assert (equal? 1.0 ((hydra_lib_math_pow 10.0) 0.0))))

(define (test-pow-neg2--neg1)

  (assert (equal? 0.5 ((hydra_lib_math_pow 2.0) -1.0))))

(define (test-pow-neg2-0-dot5)

  (assert (equal? 1.41421356237 ((hydra_lib_math_round_float64 12) ((hydra_lib_math_pow 2.0) 0.5)))))

;; sqrt

(define (test-sqrt-negsqrt-4)

  (assert (equal? 2.0 (hydra_lib_math_sqrt 4.0))))

(define (test-sqrt-negsqrt-9)

  (assert (equal? 3.0 (hydra_lib_math_sqrt 9.0))))

(define (test-sqrt-negsqrt-2)

  (assert (equal? 1.4142135623730951 (hydra_lib_math_sqrt 2.0))))

(define (test-sqrt-negsqrt-0)

  (assert (equal? 0.0 (hydra_lib_math_sqrt 0.0))))

(define (test-sqrt-negsqrt-3)

  (assert (equal? 1.73205080757 ((hydra_lib_math_round_float64 12) (hydra_lib_math_sqrt 3.0)))))

;; ceiling

(define (test-ceiling-negceiling-3-dot2)

  (assert (equal? 4 (hydra_lib_math_ceiling 3.2))))

(define (test-ceiling-negceiling-3-dot0)

  (assert (equal? 3 (hydra_lib_math_ceiling 3.0))))

(define (test-ceiling-negceiling--neg3-dot2)

  (assert (equal? -3 (hydra_lib_math_ceiling -3.2))))

(define (test-ceiling-negceiling--neg3-dot0)

  (assert (equal? -3 (hydra_lib_math_ceiling -3.0))))

;; floor

(define (test-floor-negfloor-3-dot8)

  (assert (equal? 3 (hydra_lib_math_floor 3.8))))

(define (test-floor-negfloor-3-dot0)

  (assert (equal? 3 (hydra_lib_math_floor 3.0))))

(define (test-floor-negfloor--neg3-dot2)

  (assert (equal? -4 (hydra_lib_math_floor -3.2))))

(define (test-floor-negfloor--neg3-dot0)

  (assert (equal? -3 (hydra_lib_math_floor -3.0))))

;; round

(define (test-round-neground-3-dot4)

  (assert (equal? 3 (hydra_lib_math_round 3.4))))

(define (test-round-neground-3-dot5)

  (assert (equal? 4 (hydra_lib_math_round 3.5))))

(define (test-round-neground-3-dot6)

  (assert (equal? 4 (hydra_lib_math_round 3.6))))

(define (test-round-neground--neg3-dot4)

  (assert (equal? -3 (hydra_lib_math_round -3.4))))

(define (test-round-neground--neg3-dot5)

  (assert (equal? -4 (hydra_lib_math_round -3.5))))

;; roundBigfloat

(define (test-roundbigfloat-negzero)

  (assert (equal? 0.0 ((hydra_lib_math_round_bigfloat 5) 0.0))))

(define (test-roundbigfloat-neground-pi-to-4-digits)

  (assert (equal? 3.142 ((hydra_lib_math_round_bigfloat 4) 3.141592653589793))))

(define (test-roundbigfloat-neground-1234-dot5-to-3-digits)

  (assert (equal? 1230.0 ((hydra_lib_math_round_bigfloat 3) 1234.5))))

(define (test-roundbigfloat-neground-0-dot001234-to-2-digits)

  (assert (equal? 1.2e-3 ((hydra_lib_math_round_bigfloat 2) 1.234e-3))))

(define (test-roundbigfloat-negnegative)

  (assert (equal? -1230.0 ((hydra_lib_math_round_bigfloat 3) -1234.5))))

;; roundFloat32

(define (test-roundfloat32-negzero)

  (assert (equal? 0.0 ((hydra_lib_math_round_float32 5) 0.0))))

(define (test-roundfloat32-neground-pi-to-4-digits)

  (assert (equal? 3.1419999599456787 ((hydra_lib_math_round_float32 4) 3.1415927410125732))))

(define (test-roundfloat32-neground-1234-dot5-to-3-digits)

  (assert (equal? 1230.0 ((hydra_lib_math_round_float32 3) 1234.5))))

(define (test-roundfloat32-negnegative)

  (assert (equal? -1230.0 ((hydra_lib_math_round_float32 3) -1234.5))))

;; roundFloat64

(define (test-roundfloat64-negzero)

  (assert (equal? 0.0 ((hydra_lib_math_round_float64 5) 0.0))))

(define (test-roundfloat64-neground-pi-to-4-digits)

  (assert (equal? 3.142 ((hydra_lib_math_round_float64 4) 3.141592653589793))))

(define (test-roundfloat64-neground-pi-to-10-digits)

  (assert (equal? 3.141592654 ((hydra_lib_math_round_float64 10) 3.141592653589793))))

(define (test-roundfloat64-neground-1234-dot5-to-3-digits)

  (assert (equal? 1230.0 ((hydra_lib_math_round_float64 3) 1234.5))))

(define (test-roundfloat64-neground-0-dot001234-to-2-digits)

  (assert (equal? 1.2e-3 ((hydra_lib_math_round_float64 2) 1.234e-3))))

(define (test-roundfloat64-negnegative)

  (assert (equal? -1230.0 ((hydra_lib_math_round_float64 3) -1234.5))))

(define (test-roundfloat64-neground-1-digit)

  (assert (equal? 10.0 ((hydra_lib_math_round_float64 1) 9.876))))

;; truncate

(define (test-truncate-negtruncate-3-dot8)

  (assert (equal? 3 (hydra_lib_math_truncate 3.8))))

(define (test-truncate-negtruncate-3-dot2)

  (assert (equal? 3 (hydra_lib_math_truncate 3.2))))

(define (test-truncate-negtruncate--neg3-dot8)

  (assert (equal? -3 (hydra_lib_math_truncate -3.8))))

(define (test-truncate-negtruncate--neg3-dot2)

  (assert (equal? -3 (hydra_lib_math_truncate -3.2))))
