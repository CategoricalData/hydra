;;; Note: this is an automatically generated file. Do not edit. -*- lexical-binding: t; coding: utf-8 -*-
;;; hydra.lib.math primitives

(require 'ert)

;; abs

(ert-deftest test-math-negabs-negpositive ()

  (should (equal 5 (funcall hydra_lib_math_abs 5))))

(ert-deftest test-math-negabs-negnegative ()

  (should (equal 5 (funcall hydra_lib_math_abs -5))))

(ert-deftest test-math-negabs-negzero ()

  (should (equal 0 (funcall hydra_lib_math_abs 0))))

;; add

(ert-deftest test-math-negadd-negpositive-numbers ()

  (should (equal 8 (funcall (funcall hydra_lib_math_add 3) 5))))

(ert-deftest test-math-negadd-negnegative-numbers ()

  (should (equal -8 (funcall (funcall hydra_lib_math_add -3) -5))))

(ert-deftest test-math-negadd-negmixed-sign ()

  (should (equal 7 (funcall (funcall hydra_lib_math_add 10) -3))))

(ert-deftest test-math-negadd-negwith-zero ()

  (should (equal 42 (funcall (funcall hydra_lib_math_add 42) 0))))

;; div

(ert-deftest test-math-negdiv-negexact-division ()

  (should (equal 5 (funcall (funcall hydra_lib_math_div 10) 2))))

(ert-deftest test-math-negdiv-negtruncates-toward-negative-infinity ()

  (should (equal 3 (funcall (funcall hydra_lib_math_div 10) 3))))

(ert-deftest test-math-negdiv-negnegative-dividend ()

  (should (equal -4 (funcall (funcall hydra_lib_math_div -10) 3))))

(ert-deftest test-math-negdiv-negnegative-divisor ()

  (should (equal -4 (funcall (funcall hydra_lib_math_div 10) -3))))

;; even

(ert-deftest test-math-negeven-negeven-positive ()

  (should (equal t (funcall hydra_lib_math_even 4))))

(ert-deftest test-math-negeven-negodd-positive ()

  (should (equal nil (funcall hydra_lib_math_even 5))))

(ert-deftest test-math-negeven-negeven-negative ()

  (should (equal t (funcall hydra_lib_math_even -4))))

(ert-deftest test-math-negeven-negodd-negative ()

  (should (equal nil (funcall hydra_lib_math_even -5))))

(ert-deftest test-math-negeven-negzero ()

  (should (equal t (funcall hydra_lib_math_even 0))))

;; max

(ert-deftest test-math-negmax-negfirst-is-larger ()

  (should (equal 10 (funcall (funcall hydra_lib_math_max 10) 5))))

(ert-deftest test-math-negmax-negsecond-is-larger ()

  (should (equal 10 (funcall (funcall hydra_lib_math_max 5) 10))))

(ert-deftest test-math-negmax-negequal-values ()

  (should (equal 7 (funcall (funcall hydra_lib_math_max 7) 7))))

(ert-deftest test-math-negmax-negnegative-numbers ()

  (should (equal -3 (funcall (funcall hydra_lib_math_max -3) -5))))

(ert-deftest test-math-negmax-negmixed-sign ()

  (should (equal 5 (funcall (funcall hydra_lib_math_max -5) 5))))

(ert-deftest test-math-negmax-negwith-zero ()

  (should (equal 42 (funcall (funcall hydra_lib_math_max 0) 42))))

;; min

(ert-deftest test-math-negmin-negfirst-is-smaller ()

  (should (equal 5 (funcall (funcall hydra_lib_math_min 5) 10))))

(ert-deftest test-math-negmin-negsecond-is-smaller ()

  (should (equal 5 (funcall (funcall hydra_lib_math_min 10) 5))))

(ert-deftest test-math-negmin-negequal-values ()

  (should (equal 7 (funcall (funcall hydra_lib_math_min 7) 7))))

(ert-deftest test-math-negmin-negnegative-numbers ()

  (should (equal -5 (funcall (funcall hydra_lib_math_min -3) -5))))

(ert-deftest test-math-negmin-negmixed-sign ()

  (should (equal -5 (funcall (funcall hydra_lib_math_min -5) 5))))

(ert-deftest test-math-negmin-negwith-zero ()

  (should (equal 0 (funcall (funcall hydra_lib_math_min 0) 42))))

;; mod

(ert-deftest test-math-negmod-negbasic-modulo ()

  (should (equal 1 (funcall (funcall hydra_lib_math_mod 10) 3))))

(ert-deftest test-math-negmod-negexact-division ()

  (should (equal 0 (funcall (funcall hydra_lib_math_mod 10) 2))))

(ert-deftest test-math-negmod-negnegative-dividend ()

  (should (equal 2 (funcall (funcall hydra_lib_math_mod -10) 3))))

(ert-deftest test-math-negmod-negnegative-divisor ()

  (should (equal -2 (funcall (funcall hydra_lib_math_mod 10) -3))))

;; mul

(ert-deftest test-math-negmul-negpositive-numbers ()

  (should (equal 15 (funcall (funcall hydra_lib_math_mul 3) 5))))

(ert-deftest test-math-negmul-negnegative-numbers ()

  (should (equal 15 (funcall (funcall hydra_lib_math_mul -3) -5))))

(ert-deftest test-math-negmul-negmixed-sign ()

  (should (equal -15 (funcall (funcall hydra_lib_math_mul 3) -5))))

(ert-deftest test-math-negmul-negwith-zero ()

  (should (equal 0 (funcall (funcall hydra_lib_math_mul 42) 0))))

(ert-deftest test-math-negmul-negwith-one ()

  (should (equal 42 (funcall (funcall hydra_lib_math_mul 42) 1))))

;; negate

(ert-deftest test-math-negnegate-negpositive ()

  (should (equal -5 (funcall hydra_lib_math_negate 5))))

(ert-deftest test-math-negnegate-negnegative ()

  (should (equal 5 (funcall hydra_lib_math_negate -5))))

(ert-deftest test-math-negnegate-negzero ()

  (should (equal 0 (funcall hydra_lib_math_negate 0))))

;; odd

(ert-deftest test-math-negodd-negodd-positive ()

  (should (equal t (funcall hydra_lib_math_odd 5))))

(ert-deftest test-math-negodd-negeven-positive ()

  (should (equal nil (funcall hydra_lib_math_odd 4))))

(ert-deftest test-math-negodd-negodd-negative ()

  (should (equal t (funcall hydra_lib_math_odd -5))))

(ert-deftest test-math-negodd-negeven-negative ()

  (should (equal nil (funcall hydra_lib_math_odd -4))))

(ert-deftest test-math-negodd-negzero ()

  (should (equal nil (funcall hydra_lib_math_odd 0))))

;; pred

(ert-deftest test-math-negpred-negpositive ()

  (should (equal 4 (funcall hydra_lib_math_pred 5))))

(ert-deftest test-math-negpred-negzero ()

  (should (equal -1 (funcall hydra_lib_math_pred 0))))

(ert-deftest test-math-negpred-negnegative ()

  (should (equal -6 (funcall hydra_lib_math_pred -5))))

;; range

(ert-deftest test-math-negrange-negascending-range ()

  (should (equal (list 1 2 3 4 5) (funcall (funcall hydra_lib_math_range 1) 5))))

(ert-deftest test-math-negrange-negsingle-element ()

  (should (equal (list 5) (funcall (funcall hydra_lib_math_range 5) 5))))

(ert-deftest test-math-negrange-negtwo-elements ()

  (should (equal (list 3 4) (funcall (funcall hydra_lib_math_range 3) 4))))

(ert-deftest test-math-negrange-negnegative-start ()

  (should (equal (list -2 -1 0 1 2) (funcall (funcall hydra_lib_math_range -2) 2))))

;; rem

(ert-deftest test-math-negrem-negbasic-remainder ()

  (should (equal 1 (funcall (funcall hydra_lib_math_rem 10) 3))))

(ert-deftest test-math-negrem-negexact-division ()

  (should (equal 0 (funcall (funcall hydra_lib_math_rem 10) 2))))

(ert-deftest test-math-negrem-negnegative-dividend ()

  (should (equal -1 (funcall (funcall hydra_lib_math_rem -10) 3))))

(ert-deftest test-math-negrem-negnegative-divisor ()

  (should (equal 1 (funcall (funcall hydra_lib_math_rem 10) -3))))

;; signum

(ert-deftest test-math-negsignum-negpositive ()

  (should (equal 1 (funcall hydra_lib_math_signum 5))))

(ert-deftest test-math-negsignum-negnegative ()

  (should (equal -1 (funcall hydra_lib_math_signum -5))))

(ert-deftest test-math-negsignum-negzero ()

  (should (equal 0 (funcall hydra_lib_math_signum 0))))

;; sub

(ert-deftest test-math-negsub-negpositive-numbers ()

  (should (equal 7 (funcall (funcall hydra_lib_math_sub 10) 3))))

(ert-deftest test-math-negsub-negnegative-numbers ()

  (should (equal -7 (funcall (funcall hydra_lib_math_sub -10) -3))))

(ert-deftest test-math-negsub-negmixed-sign ()

  (should (equal 13 (funcall (funcall hydra_lib_math_sub 10) -3))))

(ert-deftest test-math-negsub-negwith-zero ()

  (should (equal 42 (funcall (funcall hydra_lib_math_sub 42) 0))))

;; succ

(ert-deftest test-math-negsucc-negpositive ()

  (should (equal 6 (funcall hydra_lib_math_succ 5))))

(ert-deftest test-math-negsucc-negzero ()

  (should (equal 1 (funcall hydra_lib_math_succ 0))))

(ert-deftest test-math-negsucc-negnegative ()

  (should (equal -4 (funcall hydra_lib_math_succ -5))))

;; e

(ert-deftest test-math-nege-negeuler-s-number ()

  (should (equal 2.71828182846 (funcall (funcall hydra_lib_math_round_float64 12) hydra_lib_math_e))))

;; pi

(ert-deftest test-math-negpi-negpi-constant ()

  (should (equal 3.14159265359 (funcall (funcall hydra_lib_math_round_float64 12) hydra_lib_math_pi))))

;; sin

(ert-deftest test-math-negsin-negsin-0 ()

  (should (equal 0.0 (funcall hydra_lib_math_sin 0.0))))

(ert-deftest test-math-negsin-negsin-pi-div2 ()

  (should (equal 1.0 (funcall (funcall hydra_lib_math_round_float64 12) (funcall hydra_lib_math_sin 1.5707963267948966)))))

(ert-deftest test-math-negsin-negsin-pi ()

  (should (equal 1.22464679915e-16 (funcall (funcall hydra_lib_math_round_float64 12) (funcall hydra_lib_math_sin 3.141592653589793)))))

(ert-deftest test-math-negsin-negsin-1 ()

  (should (equal 0.841470984808 (funcall (funcall hydra_lib_math_round_float64 12) (funcall hydra_lib_math_sin 1.0)))))

(ert-deftest test-math-negsin-negsin-0-dot5 ()

  (should (equal 0.479425538604 (funcall (funcall hydra_lib_math_round_float64 12) (funcall hydra_lib_math_sin 0.5)))))

;; cos

(ert-deftest test-math-negcos-negcos-0 ()

  (should (equal 1.0 (funcall hydra_lib_math_cos 0.0))))

(ert-deftest test-math-negcos-negcos-pi-div2 ()

  (should (equal 6.12323399574e-17 (funcall (funcall hydra_lib_math_round_float64 12) (funcall hydra_lib_math_cos 1.5707963267948966)))))

(ert-deftest test-math-negcos-negcos-pi ()

  (should (equal -1.0 (funcall hydra_lib_math_cos 3.141592653589793))))

(ert-deftest test-math-negcos-negcos-1 ()

  (should (equal 0.540302305868 (funcall (funcall hydra_lib_math_round_float64 12) (funcall hydra_lib_math_cos 1.0)))))

(ert-deftest test-math-negcos-negcos-0-dot5 ()

  (should (equal 0.87758256189 (funcall (funcall hydra_lib_math_round_float64 12) (funcall hydra_lib_math_cos 0.5)))))

;; tan

(ert-deftest test-math-negtan-negtan-0 ()

  (should (equal 0.0 (funcall hydra_lib_math_tan 0.0))))

(ert-deftest test-math-negtan-negtan-pi-div4 ()

  (should (equal 1.0 (funcall (funcall hydra_lib_math_round_float64 12) (funcall hydra_lib_math_tan 0.7853981633974483)))))

(ert-deftest test-math-negtan-negtan-1 ()

  (should (equal 1.55740772465 (funcall (funcall hydra_lib_math_round_float64 12) (funcall hydra_lib_math_tan 1.0)))))

(ert-deftest test-math-negtan-negtan-0-dot5 ()

  (should (equal 0.546302489844 (funcall (funcall hydra_lib_math_round_float64 12) (funcall hydra_lib_math_tan 0.5)))))

;; asin

(ert-deftest test-math-negasin-negasin-0 ()

  (should (equal 0.0 (funcall hydra_lib_math_asin 0.0))))

(ert-deftest test-math-negasin-negasin-1 ()

  (should (equal 1.57079632679 (funcall (funcall hydra_lib_math_round_float64 12) (funcall hydra_lib_math_asin 1.0)))))

(ert-deftest test-math-negasin-negasin--neg1 ()

  (should (equal -1.57079632679 (funcall (funcall hydra_lib_math_round_float64 12) (funcall hydra_lib_math_asin -1.0)))))

(ert-deftest test-math-negasin-negasin-0-dot5 ()

  (should (equal 0.523598775598 (funcall (funcall hydra_lib_math_round_float64 12) (funcall hydra_lib_math_asin 0.5)))))

;; acos

(ert-deftest test-math-negacos-negacos-1 ()

  (should (equal 0.0 (funcall hydra_lib_math_acos 1.0))))

(ert-deftest test-math-negacos-negacos-0 ()

  (should (equal 1.57079632679 (funcall (funcall hydra_lib_math_round_float64 12) (funcall hydra_lib_math_acos 0.0)))))

(ert-deftest test-math-negacos-negacos--neg1 ()

  (should (equal 3.14159265359 (funcall (funcall hydra_lib_math_round_float64 12) (funcall hydra_lib_math_acos -1.0)))))

(ert-deftest test-math-negacos-negacos-0-dot5 ()

  (should (equal 1.0471975512 (funcall (funcall hydra_lib_math_round_float64 12) (funcall hydra_lib_math_acos 0.5)))))

;; atan

(ert-deftest test-math-negatan-negatan-0 ()

  (should (equal 0.0 (funcall hydra_lib_math_atan 0.0))))

(ert-deftest test-math-negatan-negatan-1 ()

  (should (equal 0.785398163397 (funcall (funcall hydra_lib_math_round_float64 12) (funcall hydra_lib_math_atan 1.0)))))

(ert-deftest test-math-negatan-negatan-0-dot5 ()

  (should (equal 0.463647609001 (funcall (funcall hydra_lib_math_round_float64 12) (funcall hydra_lib_math_atan 0.5)))))

;; atan2

(ert-deftest test-math-negatan2-negatan2-1-1 ()

  (should (equal 0.785398163397 (funcall (funcall hydra_lib_math_round_float64 12) (funcall (funcall hydra_lib_math_atan2 1.0) 1.0)))))

(ert-deftest test-math-negatan2-negatan2-1-0 ()

  (should (equal 1.57079632679 (funcall (funcall hydra_lib_math_round_float64 12) (funcall (funcall hydra_lib_math_atan2 1.0) 0.0)))))

(ert-deftest test-math-negatan2-negatan2-0-1 ()

  (should (equal 0.0 (funcall (funcall hydra_lib_math_atan2 0.0) 1.0))))

(ert-deftest test-math-negatan2-negatan2-3-4 ()

  (should (equal 0.643501108793 (funcall (funcall hydra_lib_math_round_float64 12) (funcall (funcall hydra_lib_math_atan2 3.0) 4.0)))))

;; sinh

(ert-deftest test-math-negsinh-negsinh-0 ()

  (should (equal 0.0 (funcall hydra_lib_math_sinh 0.0))))

(ert-deftest test-math-negsinh-negsinh-1 ()

  (should (equal 1.17520119364 (funcall (funcall hydra_lib_math_round_float64 12) (funcall hydra_lib_math_sinh 1.0)))))

(ert-deftest test-math-negsinh-negsinh-2 ()

  (should (equal 3.62686040785 (funcall (funcall hydra_lib_math_round_float64 12) (funcall hydra_lib_math_sinh 2.0)))))

;; cosh

(ert-deftest test-math-negcosh-negcosh-0 ()

  (should (equal 1.0 (funcall hydra_lib_math_cosh 0.0))))

(ert-deftest test-math-negcosh-negcosh-1 ()

  (should (equal 1.54308063482 (funcall (funcall hydra_lib_math_round_float64 12) (funcall hydra_lib_math_cosh 1.0)))))

(ert-deftest test-math-negcosh-negcosh-2 ()

  (should (equal 3.76219569108 (funcall (funcall hydra_lib_math_round_float64 12) (funcall hydra_lib_math_cosh 2.0)))))

;; tanh

(ert-deftest test-math-negtanh-negtanh-0 ()

  (should (equal 0.0 (funcall hydra_lib_math_tanh 0.0))))

(ert-deftest test-math-negtanh-negtanh-1 ()

  (should (equal 0.761594155956 (funcall (funcall hydra_lib_math_round_float64 12) (funcall hydra_lib_math_tanh 1.0)))))

(ert-deftest test-math-negtanh-negtanh-0-dot5 ()

  (should (equal 0.46211715726 (funcall (funcall hydra_lib_math_round_float64 12) (funcall hydra_lib_math_tanh 0.5)))))

;; asinh

(ert-deftest test-math-negasinh-negasinh-0 ()

  (should (equal 0.0 (funcall hydra_lib_math_asinh 0.0))))

(ert-deftest test-math-negasinh-negasinh-1 ()

  (should (equal 0.88137358702 (funcall (funcall hydra_lib_math_round_float64 12) (funcall hydra_lib_math_asinh 1.0)))))

(ert-deftest test-math-negasinh-negasinh-0-dot5 ()

  (should (equal 0.48121182506 (funcall (funcall hydra_lib_math_round_float64 12) (funcall hydra_lib_math_asinh 0.5)))))

;; acosh

(ert-deftest test-math-negacosh-negacosh-1 ()

  (should (equal 0.0 (funcall hydra_lib_math_acosh 1.0))))

(ert-deftest test-math-negacosh-negacosh-2 ()

  (should (equal 1.31695789692 (funcall (funcall hydra_lib_math_round_float64 12) (funcall hydra_lib_math_acosh 2.0)))))

(ert-deftest test-math-negacosh-negacosh-3 ()

  (should (equal 1.76274717404 (funcall (funcall hydra_lib_math_round_float64 12) (funcall hydra_lib_math_acosh 3.0)))))

;; atanh

(ert-deftest test-math-negatanh-negatanh-0 ()

  (should (equal 0.0 (funcall hydra_lib_math_atanh 0.0))))

(ert-deftest test-math-negatanh-negatanh-0-dot5 ()

  (should (equal 0.549306144334 (funcall (funcall hydra_lib_math_round_float64 12) (funcall hydra_lib_math_atanh 0.5)))))

(ert-deftest test-math-negatanh-negatanh-0-dot1 ()

  (should (equal 0.100335347731 (funcall (funcall hydra_lib_math_round_float64 12) (funcall hydra_lib_math_atanh 0.1)))))

;; exp

(ert-deftest test-math-negexp-negexp-0 ()

  (should (equal 1.0 (funcall hydra_lib_math_exp 0.0))))

(ert-deftest test-math-negexp-negexp-1 ()

  (should (equal 2.71828182846 (funcall (funcall hydra_lib_math_round_float64 12) (funcall hydra_lib_math_exp 1.0)))))

(ert-deftest test-math-negexp-negexp--neg1 ()

  (should (equal 0.367879441171 (funcall (funcall hydra_lib_math_round_float64 12) (funcall hydra_lib_math_exp -1.0)))))

(ert-deftest test-math-negexp-negexp-2 ()

  (should (equal 7.38905609893 (funcall (funcall hydra_lib_math_round_float64 12) (funcall hydra_lib_math_exp 2.0)))))

(ert-deftest test-math-negexp-negexp-0-dot5 ()

  (should (equal 1.6487212707 (funcall (funcall hydra_lib_math_round_float64 12) (funcall hydra_lib_math_exp 0.5)))))

;; log

(ert-deftest test-math-neglog-neglog-1 ()

  (should (equal 0.0 (funcall hydra_lib_math_log 1.0))))

(ert-deftest test-math-neglog-neglog-e ()

  (should (equal 1.0 (funcall (funcall hydra_lib_math_round_float64 12) (funcall hydra_lib_math_log 2.718281828459045)))))

(ert-deftest test-math-neglog-neglog-2 ()

  (should (equal 0.69314718056 (funcall (funcall hydra_lib_math_round_float64 12) (funcall hydra_lib_math_log 2.0)))))

(ert-deftest test-math-neglog-neglog-10 ()

  (should (equal 2.30258509299 (funcall (funcall hydra_lib_math_round_float64 12) (funcall hydra_lib_math_log 10.0)))))

;; logBase

(ert-deftest test-math-neglogbase-neglog10-1 ()

  (should (equal 0.0 (funcall (funcall hydra_lib_math_log_base 10.0) 1.0))))

(ert-deftest test-math-neglogbase-neglog10-10 ()

  (should (equal 1.0 (funcall (funcall hydra_lib_math_log_base 10.0) 10.0))))

(ert-deftest test-math-neglogbase-neglog10-100 ()

  (should (equal 2.0 (funcall (funcall hydra_lib_math_log_base 10.0) 100.0))))

(ert-deftest test-math-neglogbase-neglog2-8 ()

  (should (equal 3.0 (funcall (funcall hydra_lib_math_log_base 2.0) 8.0))))

(ert-deftest test-math-neglogbase-neglog2-10 ()

  (should (equal 3.32192809489 (funcall (funcall hydra_lib_math_round_float64 12) (funcall (funcall hydra_lib_math_log_base 2.0) 10.0)))))

;; pow

(ert-deftest test-math-negpow-neg2-3 ()

  (should (equal 8.0 (funcall (funcall hydra_lib_math_pow 2.0) 3.0))))

(ert-deftest test-math-negpow-neg10-0 ()

  (should (equal 1.0 (funcall (funcall hydra_lib_math_pow 10.0) 0.0))))

(ert-deftest test-math-negpow-neg2--neg1 ()

  (should (equal 0.5 (funcall (funcall hydra_lib_math_pow 2.0) -1.0))))

(ert-deftest test-math-negpow-neg2-0-dot5 ()

  (should (equal 1.41421356237 (funcall (funcall hydra_lib_math_round_float64 12) (funcall (funcall hydra_lib_math_pow 2.0) 0.5)))))

;; sqrt

(ert-deftest test-math-negsqrt-negsqrt-4 ()

  (should (equal 2.0 (funcall hydra_lib_math_sqrt 4.0))))

(ert-deftest test-math-negsqrt-negsqrt-9 ()

  (should (equal 3.0 (funcall hydra_lib_math_sqrt 9.0))))

(ert-deftest test-math-negsqrt-negsqrt-2 ()

  (should (equal 1.4142135623730951 (funcall hydra_lib_math_sqrt 2.0))))

(ert-deftest test-math-negsqrt-negsqrt-0 ()

  (should (equal 0.0 (funcall hydra_lib_math_sqrt 0.0))))

(ert-deftest test-math-negsqrt-negsqrt-3 ()

  (should (equal 1.73205080757 (funcall (funcall hydra_lib_math_round_float64 12) (funcall hydra_lib_math_sqrt 3.0)))))

;; ceiling

(ert-deftest test-math-negceiling-negceiling-3-dot2 ()

  (should (equal 4 (funcall hydra_lib_math_ceiling 3.2))))

(ert-deftest test-math-negceiling-negceiling-3-dot0 ()

  (should (equal 3 (funcall hydra_lib_math_ceiling 3.0))))

(ert-deftest test-math-negceiling-negceiling--neg3-dot2 ()

  (should (equal -3 (funcall hydra_lib_math_ceiling -3.2))))

(ert-deftest test-math-negceiling-negceiling--neg3-dot0 ()

  (should (equal -3 (funcall hydra_lib_math_ceiling -3.0))))

;; floor

(ert-deftest test-math-negfloor-negfloor-3-dot8 ()

  (should (equal 3 (funcall hydra_lib_math_floor 3.8))))

(ert-deftest test-math-negfloor-negfloor-3-dot0 ()

  (should (equal 3 (funcall hydra_lib_math_floor 3.0))))

(ert-deftest test-math-negfloor-negfloor--neg3-dot2 ()

  (should (equal -4 (funcall hydra_lib_math_floor -3.2))))

(ert-deftest test-math-negfloor-negfloor--neg3-dot0 ()

  (should (equal -3 (funcall hydra_lib_math_floor -3.0))))

;; round

(ert-deftest test-math-neground-neground-3-dot4 ()

  (should (equal 3 (funcall hydra_lib_math_round 3.4))))

(ert-deftest test-math-neground-neground-3-dot5 ()

  (should (equal 4 (funcall hydra_lib_math_round 3.5))))

(ert-deftest test-math-neground-neground-3-dot6 ()

  (should (equal 4 (funcall hydra_lib_math_round 3.6))))

(ert-deftest test-math-neground-neground--neg3-dot4 ()

  (should (equal -3 (funcall hydra_lib_math_round -3.4))))

(ert-deftest test-math-neground-neground--neg3-dot5 ()

  (should (equal -4 (funcall hydra_lib_math_round -3.5))))

;; roundBigfloat

(ert-deftest test-math-negroundbigfloat-negzero ()

  (should (equal 0.0 (funcall (funcall hydra_lib_math_round_bigfloat 5) 0.0))))

(ert-deftest test-math-negroundbigfloat-neground-pi-to-4-digits ()

  (should (equal 3.142 (funcall (funcall hydra_lib_math_round_bigfloat 4) 3.141592653589793))))

(ert-deftest test-math-negroundbigfloat-neground-1234-dot5-to-3-digits ()

  (should (equal 1230.0 (funcall (funcall hydra_lib_math_round_bigfloat 3) 1234.5))))

(ert-deftest test-math-negroundbigfloat-neground-0-dot001234-to-2-digits ()

  (should (equal 1.2e-3 (funcall (funcall hydra_lib_math_round_bigfloat 2) 1.234e-3))))

(ert-deftest test-math-negroundbigfloat-negnegative ()

  (should (equal -1230.0 (funcall (funcall hydra_lib_math_round_bigfloat 3) -1234.5))))

;; roundFloat32

(ert-deftest test-math-negroundfloat32-negzero ()

  (should (equal 0.0 (funcall (funcall hydra_lib_math_round_float32 5) 0.0))))

(ert-deftest test-math-negroundfloat32-neground-pi-to-4-digits ()

  (should (equal 3.1419999599456787 (funcall (funcall hydra_lib_math_round_float32 4) 3.1415927410125732))))

(ert-deftest test-math-negroundfloat32-neground-1234-dot5-to-3-digits ()

  (should (equal 1230.0 (funcall (funcall hydra_lib_math_round_float32 3) 1234.5))))

(ert-deftest test-math-negroundfloat32-negnegative ()

  (should (equal -1230.0 (funcall (funcall hydra_lib_math_round_float32 3) -1234.5))))

;; roundFloat64

(ert-deftest test-math-negroundfloat64-negzero ()

  (should (equal 0.0 (funcall (funcall hydra_lib_math_round_float64 5) 0.0))))

(ert-deftest test-math-negroundfloat64-neground-pi-to-4-digits ()

  (should (equal 3.142 (funcall (funcall hydra_lib_math_round_float64 4) 3.141592653589793))))

(ert-deftest test-math-negroundfloat64-neground-pi-to-10-digits ()

  (should (equal 3.141592654 (funcall (funcall hydra_lib_math_round_float64 10) 3.141592653589793))))

(ert-deftest test-math-negroundfloat64-neground-1234-dot5-to-3-digits ()

  (should (equal 1230.0 (funcall (funcall hydra_lib_math_round_float64 3) 1234.5))))

(ert-deftest test-math-negroundfloat64-neground-0-dot001234-to-2-digits ()

  (should (equal 1.2e-3 (funcall (funcall hydra_lib_math_round_float64 2) 1.234e-3))))

(ert-deftest test-math-negroundfloat64-negnegative ()

  (should (equal -1230.0 (funcall (funcall hydra_lib_math_round_float64 3) -1234.5))))

(ert-deftest test-math-negroundfloat64-neground-1-digit ()

  (should (equal 10.0 (funcall (funcall hydra_lib_math_round_float64 1) 9.876))))

;; truncate

(ert-deftest test-math-negtruncate-negtruncate-3-dot8 ()

  (should (equal 3 (funcall hydra_lib_math_truncate 3.8))))

(ert-deftest test-math-negtruncate-negtruncate-3-dot2 ()

  (should (equal 3 (funcall hydra_lib_math_truncate 3.2))))

(ert-deftest test-math-negtruncate-negtruncate--neg3-dot8 ()

  (should (equal -3 (funcall hydra_lib_math_truncate -3.8))))

(ert-deftest test-math-negtruncate-negtruncate--neg3-dot2 ()

  (should (equal -3 (funcall hydra_lib_math_truncate -3.2))))
