;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.math primitives

(import (scheme base))

;; abs

(define (test-math-negabs-negpositive)

  (assert (equal? 5 (hydra_lib_math_abs 5))))

(define (test-math-negabs-negnegative)

  (assert (equal? 5 (hydra_lib_math_abs -5))))

(define (test-math-negabs-negzero)

  (assert (equal? 0 (hydra_lib_math_abs 0))))

;; add

(define (test-math-negadd-negpositive-numbers)

  (assert (equal? 8 ((hydra_lib_math_add 3) 5))))

(define (test-math-negadd-negnegative-numbers)

  (assert (equal? -8 ((hydra_lib_math_add -3) -5))))

(define (test-math-negadd-negmixed-sign)

  (assert (equal? 7 ((hydra_lib_math_add 10) -3))))

(define (test-math-negadd-negwith-zero)

  (assert (equal? 42 ((hydra_lib_math_add 42) 0))))

;; div

(define (test-math-negdiv-negexact-division)

  (assert (equal? 5 ((hydra_lib_math_div 10) 2))))

(define (test-math-negdiv-negtruncates-toward-negative-infinity)

  (assert (equal? 3 ((hydra_lib_math_div 10) 3))))

(define (test-math-negdiv-negnegative-dividend)

  (assert (equal? -4 ((hydra_lib_math_div -10) 3))))

(define (test-math-negdiv-negnegative-divisor)

  (assert (equal? -4 ((hydra_lib_math_div 10) -3))))

;; even

(define (test-math-negeven-negeven-positive)

  (assert (equal? #t (hydra_lib_math_even 4))))

(define (test-math-negeven-negodd-positive)

  (assert (equal? #f (hydra_lib_math_even 5))))

(define (test-math-negeven-negeven-negative)

  (assert (equal? #t (hydra_lib_math_even -4))))

(define (test-math-negeven-negodd-negative)

  (assert (equal? #f (hydra_lib_math_even -5))))

(define (test-math-negeven-negzero)

  (assert (equal? #t (hydra_lib_math_even 0))))

;; max

(define (test-math-negmax-negfirst-is-larger)

  (assert (equal? 10 ((hydra_lib_math_max 10) 5))))

(define (test-math-negmax-negsecond-is-larger)

  (assert (equal? 10 ((hydra_lib_math_max 5) 10))))

(define (test-math-negmax-negequal-values)

  (assert (equal? 7 ((hydra_lib_math_max 7) 7))))

(define (test-math-negmax-negnegative-numbers)

  (assert (equal? -3 ((hydra_lib_math_max -3) -5))))

(define (test-math-negmax-negmixed-sign)

  (assert (equal? 5 ((hydra_lib_math_max -5) 5))))

(define (test-math-negmax-negwith-zero)

  (assert (equal? 42 ((hydra_lib_math_max 0) 42))))

;; min

(define (test-math-negmin-negfirst-is-smaller)

  (assert (equal? 5 ((hydra_lib_math_min 5) 10))))

(define (test-math-negmin-negsecond-is-smaller)

  (assert (equal? 5 ((hydra_lib_math_min 10) 5))))

(define (test-math-negmin-negequal-values)

  (assert (equal? 7 ((hydra_lib_math_min 7) 7))))

(define (test-math-negmin-negnegative-numbers)

  (assert (equal? -5 ((hydra_lib_math_min -3) -5))))

(define (test-math-negmin-negmixed-sign)

  (assert (equal? -5 ((hydra_lib_math_min -5) 5))))

(define (test-math-negmin-negwith-zero)

  (assert (equal? 0 ((hydra_lib_math_min 0) 42))))

;; mod

(define (test-math-negmod-negbasic-modulo)

  (assert (equal? 1 ((hydra_lib_math_mod 10) 3))))

(define (test-math-negmod-negexact-division)

  (assert (equal? 0 ((hydra_lib_math_mod 10) 2))))

(define (test-math-negmod-negnegative-dividend)

  (assert (equal? 2 ((hydra_lib_math_mod -10) 3))))

(define (test-math-negmod-negnegative-divisor)

  (assert (equal? -2 ((hydra_lib_math_mod 10) -3))))

;; mul

(define (test-math-negmul-negpositive-numbers)

  (assert (equal? 15 ((hydra_lib_math_mul 3) 5))))

(define (test-math-negmul-negnegative-numbers)

  (assert (equal? 15 ((hydra_lib_math_mul -3) -5))))

(define (test-math-negmul-negmixed-sign)

  (assert (equal? -15 ((hydra_lib_math_mul 3) -5))))

(define (test-math-negmul-negwith-zero)

  (assert (equal? 0 ((hydra_lib_math_mul 42) 0))))

(define (test-math-negmul-negwith-one)

  (assert (equal? 42 ((hydra_lib_math_mul 42) 1))))

;; negate

(define (test-math-negnegate-negpositive)

  (assert (equal? -5 (hydra_lib_math_negate 5))))

(define (test-math-negnegate-negnegative)

  (assert (equal? 5 (hydra_lib_math_negate -5))))

(define (test-math-negnegate-negzero)

  (assert (equal? 0 (hydra_lib_math_negate 0))))

;; odd

(define (test-math-negodd-negodd-positive)

  (assert (equal? #t (hydra_lib_math_odd 5))))

(define (test-math-negodd-negeven-positive)

  (assert (equal? #f (hydra_lib_math_odd 4))))

(define (test-math-negodd-negodd-negative)

  (assert (equal? #t (hydra_lib_math_odd -5))))

(define (test-math-negodd-negeven-negative)

  (assert (equal? #f (hydra_lib_math_odd -4))))

(define (test-math-negodd-negzero)

  (assert (equal? #f (hydra_lib_math_odd 0))))

;; pred

(define (test-math-negpred-negpositive)

  (assert (equal? 4 (hydra_lib_math_pred 5))))

(define (test-math-negpred-negzero)

  (assert (equal? -1 (hydra_lib_math_pred 0))))

(define (test-math-negpred-negnegative)

  (assert (equal? -6 (hydra_lib_math_pred -5))))

;; range

(define (test-math-negrange-negascending-range)

  (assert (equal? (list 1 2 3 4 5) ((hydra_lib_math_range 1) 5))))

(define (test-math-negrange-negsingle-element)

  (assert (equal? (list 5) ((hydra_lib_math_range 5) 5))))

(define (test-math-negrange-negtwo-elements)

  (assert (equal? (list 3 4) ((hydra_lib_math_range 3) 4))))

(define (test-math-negrange-negnegative-start)

  (assert (equal? (list -2 -1 0 1 2) ((hydra_lib_math_range -2) 2))))

;; rem

(define (test-math-negrem-negbasic-remainder)

  (assert (equal? 1 ((hydra_lib_math_rem 10) 3))))

(define (test-math-negrem-negexact-division)

  (assert (equal? 0 ((hydra_lib_math_rem 10) 2))))

(define (test-math-negrem-negnegative-dividend)

  (assert (equal? -1 ((hydra_lib_math_rem -10) 3))))

(define (test-math-negrem-negnegative-divisor)

  (assert (equal? 1 ((hydra_lib_math_rem 10) -3))))

;; signum

(define (test-math-negsignum-negpositive)

  (assert (equal? 1 (hydra_lib_math_signum 5))))

(define (test-math-negsignum-negnegative)

  (assert (equal? -1 (hydra_lib_math_signum -5))))

(define (test-math-negsignum-negzero)

  (assert (equal? 0 (hydra_lib_math_signum 0))))

;; sub

(define (test-math-negsub-negpositive-numbers)

  (assert (equal? 7 ((hydra_lib_math_sub 10) 3))))

(define (test-math-negsub-negnegative-numbers)

  (assert (equal? -7 ((hydra_lib_math_sub -10) -3))))

(define (test-math-negsub-negmixed-sign)

  (assert (equal? 13 ((hydra_lib_math_sub 10) -3))))

(define (test-math-negsub-negwith-zero)

  (assert (equal? 42 ((hydra_lib_math_sub 42) 0))))

;; succ

(define (test-math-negsucc-negpositive)

  (assert (equal? 6 (hydra_lib_math_succ 5))))

(define (test-math-negsucc-negzero)

  (assert (equal? 1 (hydra_lib_math_succ 0))))

(define (test-math-negsucc-negnegative)

  (assert (equal? -4 (hydra_lib_math_succ -5))))

;; e

(define (test-math-nege-negeuler-s-number)

  (assert (equal? 2.71828182846 ((hydra_lib_math_round_float64 12) hydra_lib_math_e))))

;; pi

(define (test-math-negpi-negpi-constant)

  (assert (equal? 3.14159265359 ((hydra_lib_math_round_float64 12) hydra_lib_math_pi))))

;; sin

(define (test-math-negsin-negsin-0)

  (assert (equal? 0.0 (hydra_lib_math_sin 0.0))))

(define (test-math-negsin-negsin-pi-div2)

  (assert (equal? 1.0 ((hydra_lib_math_round_float64 12) (hydra_lib_math_sin 1.5707963267948966)))))

(define (test-math-negsin-negsin-pi)

  (assert (equal? 1.22464679915e-16 ((hydra_lib_math_round_float64 12) (hydra_lib_math_sin 3.141592653589793)))))

(define (test-math-negsin-negsin-1)

  (assert (equal? 0.841470984808 ((hydra_lib_math_round_float64 12) (hydra_lib_math_sin 1.0)))))

(define (test-math-negsin-negsin-0-dot5)

  (assert (equal? 0.479425538604 ((hydra_lib_math_round_float64 12) (hydra_lib_math_sin 0.5)))))

;; cos

(define (test-math-negcos-negcos-0)

  (assert (equal? 1.0 (hydra_lib_math_cos 0.0))))

(define (test-math-negcos-negcos-pi-div2)

  (assert (equal? 6.12323399574e-17 ((hydra_lib_math_round_float64 12) (hydra_lib_math_cos 1.5707963267948966)))))

(define (test-math-negcos-negcos-pi)

  (assert (equal? -1.0 (hydra_lib_math_cos 3.141592653589793))))

(define (test-math-negcos-negcos-1)

  (assert (equal? 0.540302305868 ((hydra_lib_math_round_float64 12) (hydra_lib_math_cos 1.0)))))

(define (test-math-negcos-negcos-0-dot5)

  (assert (equal? 0.87758256189 ((hydra_lib_math_round_float64 12) (hydra_lib_math_cos 0.5)))))

;; tan

(define (test-math-negtan-negtan-0)

  (assert (equal? 0.0 (hydra_lib_math_tan 0.0))))

(define (test-math-negtan-negtan-pi-div4)

  (assert (equal? 1.0 ((hydra_lib_math_round_float64 12) (hydra_lib_math_tan 0.7853981633974483)))))

(define (test-math-negtan-negtan-1)

  (assert (equal? 1.55740772465 ((hydra_lib_math_round_float64 12) (hydra_lib_math_tan 1.0)))))

(define (test-math-negtan-negtan-0-dot5)

  (assert (equal? 0.546302489844 ((hydra_lib_math_round_float64 12) (hydra_lib_math_tan 0.5)))))

;; asin

(define (test-math-negasin-negasin-0)

  (assert (equal? 0.0 (hydra_lib_math_asin 0.0))))

(define (test-math-negasin-negasin-1)

  (assert (equal? 1.57079632679 ((hydra_lib_math_round_float64 12) (hydra_lib_math_asin 1.0)))))

(define (test-math-negasin-negasin--neg1)

  (assert (equal? -1.57079632679 ((hydra_lib_math_round_float64 12) (hydra_lib_math_asin -1.0)))))

(define (test-math-negasin-negasin-0-dot5)

  (assert (equal? 0.523598775598 ((hydra_lib_math_round_float64 12) (hydra_lib_math_asin 0.5)))))

;; acos

(define (test-math-negacos-negacos-1)

  (assert (equal? 0.0 (hydra_lib_math_acos 1.0))))

(define (test-math-negacos-negacos-0)

  (assert (equal? 1.57079632679 ((hydra_lib_math_round_float64 12) (hydra_lib_math_acos 0.0)))))

(define (test-math-negacos-negacos--neg1)

  (assert (equal? 3.14159265359 ((hydra_lib_math_round_float64 12) (hydra_lib_math_acos -1.0)))))

(define (test-math-negacos-negacos-0-dot5)

  (assert (equal? 1.0471975512 ((hydra_lib_math_round_float64 12) (hydra_lib_math_acos 0.5)))))

;; atan

(define (test-math-negatan-negatan-0)

  (assert (equal? 0.0 (hydra_lib_math_atan 0.0))))

(define (test-math-negatan-negatan-1)

  (assert (equal? 0.785398163397 ((hydra_lib_math_round_float64 12) (hydra_lib_math_atan 1.0)))))

(define (test-math-negatan-negatan-0-dot5)

  (assert (equal? 0.463647609001 ((hydra_lib_math_round_float64 12) (hydra_lib_math_atan 0.5)))))

;; atan2

(define (test-math-negatan2-negatan2-1-1)

  (assert (equal? 0.785398163397 ((hydra_lib_math_round_float64 12) ((hydra_lib_math_atan2 1.0) 1.0)))))

(define (test-math-negatan2-negatan2-1-0)

  (assert (equal? 1.57079632679 ((hydra_lib_math_round_float64 12) ((hydra_lib_math_atan2 1.0) 0.0)))))

(define (test-math-negatan2-negatan2-0-1)

  (assert (equal? 0.0 ((hydra_lib_math_atan2 0.0) 1.0))))

(define (test-math-negatan2-negatan2-3-4)

  (assert (equal? 0.643501108793 ((hydra_lib_math_round_float64 12) ((hydra_lib_math_atan2 3.0) 4.0)))))

;; sinh

(define (test-math-negsinh-negsinh-0)

  (assert (equal? 0.0 (hydra_lib_math_sinh 0.0))))

(define (test-math-negsinh-negsinh-1)

  (assert (equal? 1.17520119364 ((hydra_lib_math_round_float64 12) (hydra_lib_math_sinh 1.0)))))

(define (test-math-negsinh-negsinh-2)

  (assert (equal? 3.62686040785 ((hydra_lib_math_round_float64 12) (hydra_lib_math_sinh 2.0)))))

;; cosh

(define (test-math-negcosh-negcosh-0)

  (assert (equal? 1.0 (hydra_lib_math_cosh 0.0))))

(define (test-math-negcosh-negcosh-1)

  (assert (equal? 1.54308063482 ((hydra_lib_math_round_float64 12) (hydra_lib_math_cosh 1.0)))))

(define (test-math-negcosh-negcosh-2)

  (assert (equal? 3.76219569108 ((hydra_lib_math_round_float64 12) (hydra_lib_math_cosh 2.0)))))

;; tanh

(define (test-math-negtanh-negtanh-0)

  (assert (equal? 0.0 (hydra_lib_math_tanh 0.0))))

(define (test-math-negtanh-negtanh-1)

  (assert (equal? 0.761594155956 ((hydra_lib_math_round_float64 12) (hydra_lib_math_tanh 1.0)))))

(define (test-math-negtanh-negtanh-0-dot5)

  (assert (equal? 0.46211715726 ((hydra_lib_math_round_float64 12) (hydra_lib_math_tanh 0.5)))))

;; asinh

(define (test-math-negasinh-negasinh-0)

  (assert (equal? 0.0 (hydra_lib_math_asinh 0.0))))

(define (test-math-negasinh-negasinh-1)

  (assert (equal? 0.88137358702 ((hydra_lib_math_round_float64 12) (hydra_lib_math_asinh 1.0)))))

(define (test-math-negasinh-negasinh-0-dot5)

  (assert (equal? 0.48121182506 ((hydra_lib_math_round_float64 12) (hydra_lib_math_asinh 0.5)))))

;; acosh

(define (test-math-negacosh-negacosh-1)

  (assert (equal? 0.0 (hydra_lib_math_acosh 1.0))))

(define (test-math-negacosh-negacosh-2)

  (assert (equal? 1.31695789692 ((hydra_lib_math_round_float64 12) (hydra_lib_math_acosh 2.0)))))

(define (test-math-negacosh-negacosh-3)

  (assert (equal? 1.76274717404 ((hydra_lib_math_round_float64 12) (hydra_lib_math_acosh 3.0)))))

;; atanh

(define (test-math-negatanh-negatanh-0)

  (assert (equal? 0.0 (hydra_lib_math_atanh 0.0))))

(define (test-math-negatanh-negatanh-0-dot5)

  (assert (equal? 0.549306144334 ((hydra_lib_math_round_float64 12) (hydra_lib_math_atanh 0.5)))))

(define (test-math-negatanh-negatanh-0-dot1)

  (assert (equal? 0.100335347731 ((hydra_lib_math_round_float64 12) (hydra_lib_math_atanh 0.1)))))

;; exp

(define (test-math-negexp-negexp-0)

  (assert (equal? 1.0 (hydra_lib_math_exp 0.0))))

(define (test-math-negexp-negexp-1)

  (assert (equal? 2.71828182846 ((hydra_lib_math_round_float64 12) (hydra_lib_math_exp 1.0)))))

(define (test-math-negexp-negexp--neg1)

  (assert (equal? 0.367879441171 ((hydra_lib_math_round_float64 12) (hydra_lib_math_exp -1.0)))))

(define (test-math-negexp-negexp-2)

  (assert (equal? 7.38905609893 ((hydra_lib_math_round_float64 12) (hydra_lib_math_exp 2.0)))))

(define (test-math-negexp-negexp-0-dot5)

  (assert (equal? 1.6487212707 ((hydra_lib_math_round_float64 12) (hydra_lib_math_exp 0.5)))))

;; log

(define (test-math-neglog-neglog-1)

  (assert (equal? 0.0 (hydra_lib_math_log 1.0))))

(define (test-math-neglog-neglog-e)

  (assert (equal? 1.0 ((hydra_lib_math_round_float64 12) (hydra_lib_math_log 2.718281828459045)))))

(define (test-math-neglog-neglog-2)

  (assert (equal? 0.69314718056 ((hydra_lib_math_round_float64 12) (hydra_lib_math_log 2.0)))))

(define (test-math-neglog-neglog-10)

  (assert (equal? 2.30258509299 ((hydra_lib_math_round_float64 12) (hydra_lib_math_log 10.0)))))

;; logBase

(define (test-math-neglogbase-neglog10-1)

  (assert (equal? 0.0 ((hydra_lib_math_log_base 10.0) 1.0))))

(define (test-math-neglogbase-neglog10-10)

  (assert (equal? 1.0 ((hydra_lib_math_log_base 10.0) 10.0))))

(define (test-math-neglogbase-neglog10-100)

  (assert (equal? 2.0 ((hydra_lib_math_log_base 10.0) 100.0))))

(define (test-math-neglogbase-neglog2-8)

  (assert (equal? 3.0 ((hydra_lib_math_log_base 2.0) 8.0))))

(define (test-math-neglogbase-neglog2-10)

  (assert (equal? 3.32192809489 ((hydra_lib_math_round_float64 12) ((hydra_lib_math_log_base 2.0) 10.0)))))

;; pow

(define (test-math-negpow-neg2-3)

  (assert (equal? 8.0 ((hydra_lib_math_pow 2.0) 3.0))))

(define (test-math-negpow-neg10-0)

  (assert (equal? 1.0 ((hydra_lib_math_pow 10.0) 0.0))))

(define (test-math-negpow-neg2--neg1)

  (assert (equal? 0.5 ((hydra_lib_math_pow 2.0) -1.0))))

(define (test-math-negpow-neg2-0-dot5)

  (assert (equal? 1.41421356237 ((hydra_lib_math_round_float64 12) ((hydra_lib_math_pow 2.0) 0.5)))))

;; sqrt

(define (test-math-negsqrt-negsqrt-4)

  (assert (equal? 2.0 (hydra_lib_math_sqrt 4.0))))

(define (test-math-negsqrt-negsqrt-9)

  (assert (equal? 3.0 (hydra_lib_math_sqrt 9.0))))

(define (test-math-negsqrt-negsqrt-2)

  (assert (equal? 1.4142135623730951 (hydra_lib_math_sqrt 2.0))))

(define (test-math-negsqrt-negsqrt-0)

  (assert (equal? 0.0 (hydra_lib_math_sqrt 0.0))))

(define (test-math-negsqrt-negsqrt-3)

  (assert (equal? 1.73205080757 ((hydra_lib_math_round_float64 12) (hydra_lib_math_sqrt 3.0)))))

;; ceiling

(define (test-math-negceiling-negceiling-3-dot2)

  (assert (equal? 4 (hydra_lib_math_ceiling 3.2))))

(define (test-math-negceiling-negceiling-3-dot0)

  (assert (equal? 3 (hydra_lib_math_ceiling 3.0))))

(define (test-math-negceiling-negceiling--neg3-dot2)

  (assert (equal? -3 (hydra_lib_math_ceiling -3.2))))

(define (test-math-negceiling-negceiling--neg3-dot0)

  (assert (equal? -3 (hydra_lib_math_ceiling -3.0))))

;; floor

(define (test-math-negfloor-negfloor-3-dot8)

  (assert (equal? 3 (hydra_lib_math_floor 3.8))))

(define (test-math-negfloor-negfloor-3-dot0)

  (assert (equal? 3 (hydra_lib_math_floor 3.0))))

(define (test-math-negfloor-negfloor--neg3-dot2)

  (assert (equal? -4 (hydra_lib_math_floor -3.2))))

(define (test-math-negfloor-negfloor--neg3-dot0)

  (assert (equal? -3 (hydra_lib_math_floor -3.0))))

;; round

(define (test-math-neground-neground-3-dot4)

  (assert (equal? 3 (hydra_lib_math_round 3.4))))

(define (test-math-neground-neground-3-dot5)

  (assert (equal? 4 (hydra_lib_math_round 3.5))))

(define (test-math-neground-neground-3-dot6)

  (assert (equal? 4 (hydra_lib_math_round 3.6))))

(define (test-math-neground-neground--neg3-dot4)

  (assert (equal? -3 (hydra_lib_math_round -3.4))))

(define (test-math-neground-neground--neg3-dot5)

  (assert (equal? -4 (hydra_lib_math_round -3.5))))

;; roundBigfloat

(define (test-math-negroundbigfloat-negzero)

  (assert (equal? 0.0 ((hydra_lib_math_round_bigfloat 5) 0.0))))

(define (test-math-negroundbigfloat-neground-pi-to-4-digits)

  (assert (equal? 3.142 ((hydra_lib_math_round_bigfloat 4) 3.141592653589793))))

(define (test-math-negroundbigfloat-neground-1234-dot5-to-3-digits)

  (assert (equal? 1230.0 ((hydra_lib_math_round_bigfloat 3) 1234.5))))

(define (test-math-negroundbigfloat-neground-0-dot001234-to-2-digits)

  (assert (equal? 1.2e-3 ((hydra_lib_math_round_bigfloat 2) 1.234e-3))))

(define (test-math-negroundbigfloat-negnegative)

  (assert (equal? -1230.0 ((hydra_lib_math_round_bigfloat 3) -1234.5))))

;; roundFloat32

(define (test-math-negroundfloat32-negzero)

  (assert (equal? 0.0 ((hydra_lib_math_round_float32 5) 0.0))))

(define (test-math-negroundfloat32-neground-pi-to-4-digits)

  (assert (equal? 3.1419999599456787 ((hydra_lib_math_round_float32 4) 3.1415927410125732))))

(define (test-math-negroundfloat32-neground-1234-dot5-to-3-digits)

  (assert (equal? 1230.0 ((hydra_lib_math_round_float32 3) 1234.5))))

(define (test-math-negroundfloat32-negnegative)

  (assert (equal? -1230.0 ((hydra_lib_math_round_float32 3) -1234.5))))

;; roundFloat64

(define (test-math-negroundfloat64-negzero)

  (assert (equal? 0.0 ((hydra_lib_math_round_float64 5) 0.0))))

(define (test-math-negroundfloat64-neground-pi-to-4-digits)

  (assert (equal? 3.142 ((hydra_lib_math_round_float64 4) 3.141592653589793))))

(define (test-math-negroundfloat64-neground-pi-to-10-digits)

  (assert (equal? 3.141592654 ((hydra_lib_math_round_float64 10) 3.141592653589793))))

(define (test-math-negroundfloat64-neground-1234-dot5-to-3-digits)

  (assert (equal? 1230.0 ((hydra_lib_math_round_float64 3) 1234.5))))

(define (test-math-negroundfloat64-neground-0-dot001234-to-2-digits)

  (assert (equal? 1.2e-3 ((hydra_lib_math_round_float64 2) 1.234e-3))))

(define (test-math-negroundfloat64-negnegative)

  (assert (equal? -1230.0 ((hydra_lib_math_round_float64 3) -1234.5))))

(define (test-math-negroundfloat64-neground-1-digit)

  (assert (equal? 10.0 ((hydra_lib_math_round_float64 1) 9.876))))

;; truncate

(define (test-math-negtruncate-negtruncate-3-dot8)

  (assert (equal? 3 (hydra_lib_math_truncate 3.8))))

(define (test-math-negtruncate-negtruncate-3-dot2)

  (assert (equal? 3 (hydra_lib_math_truncate 3.2))))

(define (test-math-negtruncate-negtruncate--neg3-dot8)

  (assert (equal? -3 (hydra_lib_math_truncate -3.8))))

(define (test-math-negtruncate-negtruncate--neg3-dot2)

  (assert (equal? -3 (hydra_lib_math_truncate -3.2))))
