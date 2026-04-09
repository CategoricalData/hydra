;;; Note: this is an automatically generated file. Do not edit. -*- lexical-binding: t; coding: utf-8 -*-
;;; hydra.lib.math primitives

(require 'ert)

;; abs

(ert-deftest test-math-negabs-negpositive ()

  (should (equal 5:int32 5:int32)))

(ert-deftest test-math-negabs-negnegative ()

  (should (equal 5:int32 5:int32)))

(ert-deftest test-math-negabs-negzero ()

  (should (equal 0:int32 0:int32)))

;; add

(ert-deftest test-math-negadd-negpositive-numbers ()

  (should (equal 8:int32 8:int32)))

(ert-deftest test-math-negadd-negnegative-numbers ()

  (should (equal -8:int32 -8:int32)))

(ert-deftest test-math-negadd-negmixed-sign ()

  (should (equal 7:int32 7:int32)))

(ert-deftest test-math-negadd-negwith-zero ()

  (should (equal 42:int32 42:int32)))

;; div

(ert-deftest test-math-negdiv-negexact-division ()

  (should (equal 5:int32 5:int32)))

(ert-deftest test-math-negdiv-negtruncates-toward-negative-infinity ()

  (should (equal 3:int32 3:int32)))

(ert-deftest test-math-negdiv-negnegative-dividend ()

  (should (equal -4:int32 -4:int32)))

(ert-deftest test-math-negdiv-negnegative-divisor ()

  (should (equal -4:int32 -4:int32)))

;; even

(ert-deftest test-math-negeven-negeven-positive ()

  (should (equal true true)))

(ert-deftest test-math-negeven-negodd-positive ()

  (should (equal false false)))

(ert-deftest test-math-negeven-negeven-negative ()

  (should (equal true true)))

(ert-deftest test-math-negeven-negodd-negative ()

  (should (equal false false)))

(ert-deftest test-math-negeven-negzero ()

  (should (equal true true)))

;; max

(ert-deftest test-math-negmax-negfirst-is-larger ()

  (should (equal 10:int32 10:int32)))

(ert-deftest test-math-negmax-negsecond-is-larger ()

  (should (equal 10:int32 10:int32)))

(ert-deftest test-math-negmax-negequal-values ()

  (should (equal 7:int32 7:int32)))

(ert-deftest test-math-negmax-negnegative-numbers ()

  (should (equal -3:int32 -3:int32)))

(ert-deftest test-math-negmax-negmixed-sign ()

  (should (equal 5:int32 5:int32)))

(ert-deftest test-math-negmax-negwith-zero ()

  (should (equal 42:int32 42:int32)))

;; maybeDiv

(ert-deftest test-math-negmaybediv-negbasic-division ()

  (should (equal just(3:int32) just(3:int32))))

(ert-deftest test-math-negmaybediv-negexact-division ()

  (should (equal just(5:int32) just(5:int32))))

(ert-deftest test-math-negmaybediv-negdivision-by-zero ()

  (should (equal nothing nothing)))

(ert-deftest test-math-negmaybediv-negzero-divided ()

  (should (equal just(0:int32) just(0:int32))))

(ert-deftest test-math-negmaybediv-negnegative-dividend ()

  (should (equal just(-4:int32) just(-4:int32))))

(ert-deftest test-math-negmaybediv-negnegative-divisor ()

  (should (equal just(-4:int32) just(-4:int32))))

;; min

(ert-deftest test-math-negmin-negfirst-is-smaller ()

  (should (equal 5:int32 5:int32)))

(ert-deftest test-math-negmin-negsecond-is-smaller ()

  (should (equal 5:int32 5:int32)))

(ert-deftest test-math-negmin-negequal-values ()

  (should (equal 7:int32 7:int32)))

(ert-deftest test-math-negmin-negnegative-numbers ()

  (should (equal -5:int32 -5:int32)))

(ert-deftest test-math-negmin-negmixed-sign ()

  (should (equal -5:int32 -5:int32)))

(ert-deftest test-math-negmin-negwith-zero ()

  (should (equal 0:int32 0:int32)))

;; maybeMod

(ert-deftest test-math-negmaybemod-negbasic-modulo ()

  (should (equal just(1:int32) just(1:int32))))

(ert-deftest test-math-negmaybemod-negexact-division ()

  (should (equal just(0:int32) just(0:int32))))

(ert-deftest test-math-negmaybemod-negdivision-by-zero ()

  (should (equal nothing nothing)))

(ert-deftest test-math-negmaybemod-negnegative-dividend ()

  (should (equal just(2:int32) just(2:int32))))

(ert-deftest test-math-negmaybemod-negnegative-divisor ()

  (should (equal just(-2:int32) just(-2:int32))))

;; mod

(ert-deftest test-math-negmod-negbasic-modulo ()

  (should (equal 1:int32 1:int32)))

(ert-deftest test-math-negmod-negexact-division ()

  (should (equal 0:int32 0:int32)))

(ert-deftest test-math-negmod-negnegative-dividend ()

  (should (equal 2:int32 2:int32)))

(ert-deftest test-math-negmod-negnegative-divisor ()

  (should (equal -2:int32 -2:int32)))

;; mul

(ert-deftest test-math-negmul-negpositive-numbers ()

  (should (equal 15:int32 15:int32)))

(ert-deftest test-math-negmul-negnegative-numbers ()

  (should (equal 15:int32 15:int32)))

(ert-deftest test-math-negmul-negmixed-sign ()

  (should (equal -15:int32 -15:int32)))

(ert-deftest test-math-negmul-negwith-zero ()

  (should (equal 0:int32 0:int32)))

(ert-deftest test-math-negmul-negwith-one ()

  (should (equal 42:int32 42:int32)))

;; negate

(ert-deftest test-math-negnegate-negpositive ()

  (should (equal -5:int32 -5:int32)))

(ert-deftest test-math-negnegate-negnegative ()

  (should (equal 5:int32 5:int32)))

(ert-deftest test-math-negnegate-negzero ()

  (should (equal 0:int32 0:int32)))

;; odd

(ert-deftest test-math-negodd-negodd-positive ()

  (should (equal true true)))

(ert-deftest test-math-negodd-negeven-positive ()

  (should (equal false false)))

(ert-deftest test-math-negodd-negodd-negative ()

  (should (equal true true)))

(ert-deftest test-math-negodd-negeven-negative ()

  (should (equal false false)))

(ert-deftest test-math-negodd-negzero ()

  (should (equal false false)))

;; maybePred

(ert-deftest test-math-negmaybepred-negpositive ()

  (should (equal just(4:int32) just(4:int32))))

(ert-deftest test-math-negmaybepred-negzero ()

  (should (equal just(-1:int32) just(-1:int32))))

(ert-deftest test-math-negmaybepred-negnegative ()

  (should (equal just(-6:int32) just(-6:int32))))

(ert-deftest test-math-negmaybepred-negminbound ()

  (should (equal nothing nothing)))

;; pred

(ert-deftest test-math-negpred-negpositive ()

  (should (equal 4:int32 4:int32)))

(ert-deftest test-math-negpred-negzero ()

  (should (equal -1:int32 -1:int32)))

(ert-deftest test-math-negpred-negnegative ()

  (should (equal -6:int32 -6:int32)))

;; range

(ert-deftest test-math-negrange-negascending-range ()

  (should (equal [1:int32, 2:int32, 3:int32, 4:int32, 5:int32] [1:int32, 2:int32, 3:int32, 4:int32, 5:int32])))

(ert-deftest test-math-negrange-negsingle-element ()

  (should (equal [5:int32] [5:int32])))

(ert-deftest test-math-negrange-negtwo-elements ()

  (should (equal [3:int32, 4:int32] [3:int32, 4:int32])))

(ert-deftest test-math-negrange-negnegative-start ()

  (should (equal [-2:int32, -1:int32, 0:int32, 1:int32, 2:int32] [-2:int32, -1:int32, 0:int32, 1:int32, 2:int32])))

;; maybeRem

(ert-deftest test-math-negmayberem-negbasic-remainder ()

  (should (equal just(1:int32) just(1:int32))))

(ert-deftest test-math-negmayberem-negexact-division ()

  (should (equal just(0:int32) just(0:int32))))

(ert-deftest test-math-negmayberem-negdivision-by-zero ()

  (should (equal nothing nothing)))

(ert-deftest test-math-negmayberem-negnegative-dividend ()

  (should (equal just(-1:int32) just(-1:int32))))

(ert-deftest test-math-negmayberem-negnegative-divisor ()

  (should (equal just(1:int32) just(1:int32))))

;; rem

(ert-deftest test-math-negrem-negbasic-remainder ()

  (should (equal 1:int32 1:int32)))

(ert-deftest test-math-negrem-negexact-division ()

  (should (equal 0:int32 0:int32)))

(ert-deftest test-math-negrem-negnegative-dividend ()

  (should (equal -1:int32 -1:int32)))

(ert-deftest test-math-negrem-negnegative-divisor ()

  (should (equal 1:int32 1:int32)))

;; signum

(ert-deftest test-math-negsignum-negpositive ()

  (should (equal 1:int32 1:int32)))

(ert-deftest test-math-negsignum-negnegative ()

  (should (equal -1:int32 -1:int32)))

(ert-deftest test-math-negsignum-negzero ()

  (should (equal 0:int32 0:int32)))

;; sub

(ert-deftest test-math-negsub-negpositive-numbers ()

  (should (equal 7:int32 7:int32)))

(ert-deftest test-math-negsub-negnegative-numbers ()

  (should (equal -7:int32 -7:int32)))

(ert-deftest test-math-negsub-negmixed-sign ()

  (should (equal 13:int32 13:int32)))

(ert-deftest test-math-negsub-negwith-zero ()

  (should (equal 42:int32 42:int32)))

;; maybeSucc

(ert-deftest test-math-negmaybesucc-negpositive ()

  (should (equal just(6:int32) just(6:int32))))

(ert-deftest test-math-negmaybesucc-negzero ()

  (should (equal just(1:int32) just(1:int32))))

(ert-deftest test-math-negmaybesucc-negnegative ()

  (should (equal just(-4:int32) just(-4:int32))))

(ert-deftest test-math-negmaybesucc-negmaxbound ()

  (should (equal nothing nothing)))

;; succ

(ert-deftest test-math-negsucc-negpositive ()

  (should (equal 6:int32 6:int32)))

(ert-deftest test-math-negsucc-negzero ()

  (should (equal 1:int32 1:int32)))

(ert-deftest test-math-negsucc-negnegative ()

  (should (equal -4:int32 -4:int32)))

;; e

(ert-deftest test-math-nege-negeuler-s-number ()

  (should (equal 2.71828182846:float64 2.71828182846:float64)))

;; pi

(ert-deftest test-math-negpi-negpi-constant ()

  (should (equal 3.14159265359:float64 3.14159265359:float64)))

;; sin

(ert-deftest test-math-negsin-negsin-0 ()

  (should (equal 0.0:float64 0.0:float64)))

(ert-deftest test-math-negsin-negsin-pi-div2 ()

  (should (equal 1.0:float64 1.0:float64)))

(ert-deftest test-math-negsin-negsin-pi ()

  (should (equal 1.22464679915e-16:float64 1.22464679915e-16:float64)))

(ert-deftest test-math-negsin-negsin-1 ()

  (should (equal 0.841470984808:float64 0.841470984808:float64)))

(ert-deftest test-math-negsin-negsin-0-dot5 ()

  (should (equal 0.479425538604:float64 0.479425538604:float64)))

(ert-deftest test-math-negsin-negsin-nan ()

  (should (equal NaN:float64 NaN:float64)))

(ert-deftest test-math-negsin-negsin--plusinf ()

  (should (equal NaN:float64 NaN:float64)))

(ert-deftest test-math-negsin-negsin--neginf ()

  (should (equal NaN:float64 NaN:float64)))

;; cos

(ert-deftest test-math-negcos-negcos-0 ()

  (should (equal 1.0:float64 1.0:float64)))

(ert-deftest test-math-negcos-negcos-pi-div2 ()

  (should (equal 6.12323399574e-17:float64 6.12323399574e-17:float64)))

(ert-deftest test-math-negcos-negcos-pi ()

  (should (equal -1.0:float64 -1.0:float64)))

(ert-deftest test-math-negcos-negcos-1 ()

  (should (equal 0.540302305868:float64 0.540302305868:float64)))

(ert-deftest test-math-negcos-negcos-0-dot5 ()

  (should (equal 0.87758256189:float64 0.87758256189:float64)))

(ert-deftest test-math-negcos-negcos-nan ()

  (should (equal NaN:float64 NaN:float64)))

(ert-deftest test-math-negcos-negcos--plusinf ()

  (should (equal NaN:float64 NaN:float64)))

(ert-deftest test-math-negcos-negcos--neginf ()

  (should (equal NaN:float64 NaN:float64)))

;; tan

(ert-deftest test-math-negtan-negtan-0 ()

  (should (equal 0.0:float64 0.0:float64)))

(ert-deftest test-math-negtan-negtan-pi-div4 ()

  (should (equal 1.0:float64 1.0:float64)))

(ert-deftest test-math-negtan-negtan-1 ()

  (should (equal 1.55740772465:float64 1.55740772465:float64)))

(ert-deftest test-math-negtan-negtan-0-dot5 ()

  (should (equal 0.546302489844:float64 0.546302489844:float64)))

(ert-deftest test-math-negtan-negtan-nan ()

  (should (equal NaN:float64 NaN:float64)))

(ert-deftest test-math-negtan-negtan--plusinf ()

  (should (equal NaN:float64 NaN:float64)))

(ert-deftest test-math-negtan-negtan--neginf ()

  (should (equal NaN:float64 NaN:float64)))

;; asin

(ert-deftest test-math-negasin-negasin-0 ()

  (should (equal 0.0:float64 0.0:float64)))

(ert-deftest test-math-negasin-negasin-1 ()

  (should (equal 1.57079632679:float64 1.57079632679:float64)))

(ert-deftest test-math-negasin-negasin--neg1 ()

  (should (equal -1.57079632679:float64 -1.57079632679:float64)))

(ert-deftest test-math-negasin-negasin-0-dot5 ()

  (should (equal 0.523598775598:float64 0.523598775598:float64)))

(ert-deftest test-math-negasin-negasin-below-domain ()

  (should (equal NaN:float64 NaN:float64)))

(ert-deftest test-math-negasin-negasin-above-domain ()

  (should (equal NaN:float64 NaN:float64)))

(ert-deftest test-math-negasin-negasin-nan ()

  (should (equal NaN:float64 NaN:float64)))

(ert-deftest test-math-negasin-negasin--plusinf ()

  (should (equal NaN:float64 NaN:float64)))

(ert-deftest test-math-negasin-negasin--neginf ()

  (should (equal NaN:float64 NaN:float64)))

;; acos

(ert-deftest test-math-negacos-negacos-1 ()

  (should (equal 0.0:float64 0.0:float64)))

(ert-deftest test-math-negacos-negacos-0 ()

  (should (equal 1.57079632679:float64 1.57079632679:float64)))

(ert-deftest test-math-negacos-negacos--neg1 ()

  (should (equal 3.14159265359:float64 3.14159265359:float64)))

(ert-deftest test-math-negacos-negacos-0-dot5 ()

  (should (equal 1.0471975512:float64 1.0471975512:float64)))

(ert-deftest test-math-negacos-negacos-below-domain ()

  (should (equal NaN:float64 NaN:float64)))

(ert-deftest test-math-negacos-negacos-above-domain ()

  (should (equal NaN:float64 NaN:float64)))

(ert-deftest test-math-negacos-negacos-nan ()

  (should (equal NaN:float64 NaN:float64)))

(ert-deftest test-math-negacos-negacos--plusinf ()

  (should (equal NaN:float64 NaN:float64)))

(ert-deftest test-math-negacos-negacos--neginf ()

  (should (equal NaN:float64 NaN:float64)))

;; atan

(ert-deftest test-math-negatan-negatan-0 ()

  (should (equal 0.0:float64 0.0:float64)))

(ert-deftest test-math-negatan-negatan-1 ()

  (should (equal 0.785398163397:float64 0.785398163397:float64)))

(ert-deftest test-math-negatan-negatan-0-dot5 ()

  (should (equal 0.463647609001:float64 0.463647609001:float64)))

(ert-deftest test-math-negatan-negatan-nan ()

  (should (equal NaN:float64 NaN:float64)))

(ert-deftest test-math-negatan-negatan--plusinf ()

  (should (equal 1.57079632679:float64 1.57079632679:float64)))

(ert-deftest test-math-negatan-negatan--neginf ()

  (should (equal -1.57079632679:float64 -1.57079632679:float64)))

;; atan2

(ert-deftest test-math-negatan2-negatan2-1-1 ()

  (should (equal 0.785398163397:float64 0.785398163397:float64)))

(ert-deftest test-math-negatan2-negatan2-1-0 ()

  (should (equal 1.57079632679:float64 1.57079632679:float64)))

(ert-deftest test-math-negatan2-negatan2-0-1 ()

  (should (equal 0.0:float64 0.0:float64)))

(ert-deftest test-math-negatan2-negatan2-3-4 ()

  (should (equal 0.643501108793:float64 0.643501108793:float64)))

(ert-deftest test-math-negatan2-negatan2-nan-1 ()

  (should (equal NaN:float64 NaN:float64)))

(ert-deftest test-math-negatan2-negatan2--plusinf-1 ()

  (should (equal 1.57079632679:float64 1.57079632679:float64)))

(ert-deftest test-math-negatan2-negatan2--neginf-1 ()

  (should (equal -1.57079632679:float64 -1.57079632679:float64)))

(ert-deftest test-math-negatan2-negatan2-1-nan ()

  (should (equal NaN:float64 NaN:float64)))

(ert-deftest test-math-negatan2-negatan2-1--plusinf ()

  (should (equal 0.0:float64 0.0:float64)))

(ert-deftest test-math-negatan2-negatan2-1--neginf ()

  (should (equal 3.14159265359:float64 3.14159265359:float64)))

(ert-deftest test-math-negatan2-negatan2--plusinf--plusinf ()

  (should (equal NaN:float64 NaN:float64)))

(ert-deftest test-math-negatan2-negatan2--plusinf--neginf ()

  (should (equal NaN:float64 NaN:float64)))

(ert-deftest test-math-negatan2-negatan2--neginf--plusinf ()

  (should (equal NaN:float64 NaN:float64)))

(ert-deftest test-math-negatan2-negatan2--neginf--neginf ()

  (should (equal NaN:float64 NaN:float64)))

;; sinh

(ert-deftest test-math-negsinh-negsinh-0 ()

  (should (equal 0.0:float64 0.0:float64)))

(ert-deftest test-math-negsinh-negsinh-1 ()

  (should (equal 1.17520119364:float64 1.17520119364:float64)))

(ert-deftest test-math-negsinh-negsinh-2 ()

  (should (equal 3.62686040785:float64 3.62686040785:float64)))

(ert-deftest test-math-negsinh-negsinh-nan ()

  (should (equal NaN:float64 NaN:float64)))

(ert-deftest test-math-negsinh-negsinh--plusinf ()

  (should (equal Infinity:float64 Infinity:float64)))

(ert-deftest test-math-negsinh-negsinh--neginf ()

  (should (equal -Infinity:float64 -Infinity:float64)))

;; cosh

(ert-deftest test-math-negcosh-negcosh-0 ()

  (should (equal 1.0:float64 1.0:float64)))

(ert-deftest test-math-negcosh-negcosh-1 ()

  (should (equal 1.54308063482:float64 1.54308063482:float64)))

(ert-deftest test-math-negcosh-negcosh-2 ()

  (should (equal 3.76219569108:float64 3.76219569108:float64)))

(ert-deftest test-math-negcosh-negcosh-nan ()

  (should (equal NaN:float64 NaN:float64)))

(ert-deftest test-math-negcosh-negcosh--plusinf ()

  (should (equal Infinity:float64 Infinity:float64)))

(ert-deftest test-math-negcosh-negcosh--neginf ()

  (should (equal Infinity:float64 Infinity:float64)))

;; tanh

(ert-deftest test-math-negtanh-negtanh-0 ()

  (should (equal 0.0:float64 0.0:float64)))

(ert-deftest test-math-negtanh-negtanh-1 ()

  (should (equal 0.761594155956:float64 0.761594155956:float64)))

(ert-deftest test-math-negtanh-negtanh-0-dot5 ()

  (should (equal 0.46211715726:float64 0.46211715726:float64)))

(ert-deftest test-math-negtanh-negtanh-nan ()

  (should (equal NaN:float64 NaN:float64)))

(ert-deftest test-math-negtanh-negtanh--plusinf ()

  (should (equal 1.0:float64 1.0:float64)))

(ert-deftest test-math-negtanh-negtanh--neginf ()

  (should (equal -1.0:float64 -1.0:float64)))

;; asinh

(ert-deftest test-math-negasinh-negasinh-0 ()

  (should (equal 0.0:float64 0.0:float64)))

(ert-deftest test-math-negasinh-negasinh-1 ()

  (should (equal 0.88137358702:float64 0.88137358702:float64)))

(ert-deftest test-math-negasinh-negasinh-0-dot5 ()

  (should (equal 0.48121182506:float64 0.48121182506:float64)))

(ert-deftest test-math-negasinh-negasinh-nan ()

  (should (equal NaN:float64 NaN:float64)))

(ert-deftest test-math-negasinh-negasinh--plusinf ()

  (should (equal Infinity:float64 Infinity:float64)))

(ert-deftest test-math-negasinh-negasinh--neginf ()

  (should (equal -Infinity:float64 -Infinity:float64)))

;; acosh

(ert-deftest test-math-negacosh-negacosh-1 ()

  (should (equal 0.0:float64 0.0:float64)))

(ert-deftest test-math-negacosh-negacosh-2 ()

  (should (equal 1.31695789692:float64 1.31695789692:float64)))

(ert-deftest test-math-negacosh-negacosh-3 ()

  (should (equal 1.76274717404:float64 1.76274717404:float64)))

(ert-deftest test-math-negacosh-negacosh-below-domain ()

  (should (equal NaN:float64 NaN:float64)))

(ert-deftest test-math-negacosh-negacosh-negative ()

  (should (equal NaN:float64 NaN:float64)))

(ert-deftest test-math-negacosh-negacosh-nan ()

  (should (equal NaN:float64 NaN:float64)))

(ert-deftest test-math-negacosh-negacosh--plusinf ()

  (should (equal Infinity:float64 Infinity:float64)))

(ert-deftest test-math-negacosh-negacosh--neginf ()

  (should (equal NaN:float64 NaN:float64)))

;; atanh

(ert-deftest test-math-negatanh-negatanh-0 ()

  (should (equal 0.0:float64 0.0:float64)))

(ert-deftest test-math-negatanh-negatanh-0-dot5 ()

  (should (equal 0.549306144334:float64 0.549306144334:float64)))

(ert-deftest test-math-negatanh-negatanh-0-dot1 ()

  (should (equal 0.100335347731:float64 0.100335347731:float64)))

(ert-deftest test-math-negatanh-negatanh-upper-boundary ()

  (should (equal Infinity:float64 Infinity:float64)))

(ert-deftest test-math-negatanh-negatanh-lower-boundary ()

  (should (equal -Infinity:float64 -Infinity:float64)))

(ert-deftest test-math-negatanh-negatanh-above-domain ()

  (should (equal NaN:float64 NaN:float64)))

(ert-deftest test-math-negatanh-negatanh-below-domain ()

  (should (equal NaN:float64 NaN:float64)))

(ert-deftest test-math-negatanh-negatanh-nan ()

  (should (equal NaN:float64 NaN:float64)))

(ert-deftest test-math-negatanh-negatanh--plusinf ()

  (should (equal NaN:float64 NaN:float64)))

(ert-deftest test-math-negatanh-negatanh--neginf ()

  (should (equal NaN:float64 NaN:float64)))

;; exp

(ert-deftest test-math-negexp-negexp-0 ()

  (should (equal 1.0:float64 1.0:float64)))

(ert-deftest test-math-negexp-negexp-1 ()

  (should (equal 2.71828182846:float64 2.71828182846:float64)))

(ert-deftest test-math-negexp-negexp--neg1 ()

  (should (equal 0.367879441171:float64 0.367879441171:float64)))

(ert-deftest test-math-negexp-negexp-2 ()

  (should (equal 7.38905609893:float64 7.38905609893:float64)))

(ert-deftest test-math-negexp-negexp-0-dot5 ()

  (should (equal 1.6487212707:float64 1.6487212707:float64)))

(ert-deftest test-math-negexp-negexp-nan ()

  (should (equal NaN:float64 NaN:float64)))

(ert-deftest test-math-negexp-negexp--plusinf ()

  (should (equal Infinity:float64 Infinity:float64)))

(ert-deftest test-math-negexp-negexp--neginf ()

  (should (equal 0.0:float64 0.0:float64)))

;; log

(ert-deftest test-math-neglog-neglog-1 ()

  (should (equal 0.0:float64 0.0:float64)))

(ert-deftest test-math-neglog-neglog-e ()

  (should (equal 1.0:float64 1.0:float64)))

(ert-deftest test-math-neglog-neglog-2 ()

  (should (equal 0.69314718056:float64 0.69314718056:float64)))

(ert-deftest test-math-neglog-neglog-10 ()

  (should (equal 2.30258509299:float64 2.30258509299:float64)))

(ert-deftest test-math-neglog-neglog-0 ()

  (should (equal -Infinity:float64 -Infinity:float64)))

(ert-deftest test-math-neglog-neglog-negative ()

  (should (equal NaN:float64 NaN:float64)))

(ert-deftest test-math-neglog-neglog-nan ()

  (should (equal NaN:float64 NaN:float64)))

(ert-deftest test-math-neglog-neglog--plusinf ()

  (should (equal Infinity:float64 Infinity:float64)))

(ert-deftest test-math-neglog-neglog--neginf ()

  (should (equal NaN:float64 NaN:float64)))

;; logBase

(ert-deftest test-math-neglogbase-neglog10-1 ()

  (should (equal 0.0:float64 0.0:float64)))

(ert-deftest test-math-neglogbase-neglog10-10 ()

  (should (equal 1.0:float64 1.0:float64)))

(ert-deftest test-math-neglogbase-neglog10-100 ()

  (should (equal 2.0:float64 2.0:float64)))

(ert-deftest test-math-neglogbase-neglog2-8 ()

  (should (equal 3.0:float64 3.0:float64)))

(ert-deftest test-math-neglogbase-neglog2-10 ()

  (should (equal 3.32192809489:float64 3.32192809489:float64)))

(ert-deftest test-math-neglogbase-neglogbase-10-0 ()

  (should (equal -Infinity:float64 -Infinity:float64)))

(ert-deftest test-math-neglogbase-neglogbase-10-negative ()

  (should (equal NaN:float64 NaN:float64)))

(ert-deftest test-math-neglogbase-neglogbase-negative-10 ()

  (should (equal NaN:float64 NaN:float64)))

(ert-deftest test-math-neglogbase-neglogbase-10-nan ()

  (should (equal NaN:float64 NaN:float64)))

(ert-deftest test-math-neglogbase-neglogbase-10--plusinf ()

  (should (equal Infinity:float64 Infinity:float64)))

(ert-deftest test-math-neglogbase-neglogbase-10--neginf ()

  (should (equal NaN:float64 NaN:float64)))

(ert-deftest test-math-neglogbase-neglogbase-nan-10 ()

  (should (equal NaN:float64 NaN:float64)))

(ert-deftest test-math-neglogbase-neglogbase--plusinf-10 ()

  (should (equal 0.0:float64 0.0:float64)))

(ert-deftest test-math-neglogbase-neglogbase--neginf-10 ()

  (should (equal NaN:float64 NaN:float64)))

;; pow

(ert-deftest test-math-negpow-neg2-3 ()

  (should (equal 8.0:float64 8.0:float64)))

(ert-deftest test-math-negpow-neg10-0 ()

  (should (equal 1.0:float64 1.0:float64)))

(ert-deftest test-math-negpow-neg2--neg1 ()

  (should (equal 0.5:float64 0.5:float64)))

(ert-deftest test-math-negpow-neg2-0-dot5 ()

  (should (equal 1.41421356237:float64 1.41421356237:float64)))

(ert-deftest test-math-negpow-neg0-0 ()

  (should (equal 1.0:float64 1.0:float64)))

(ert-deftest test-math-negpow-neg0--neg1 ()

  (should (equal Infinity:float64 Infinity:float64)))

(ert-deftest test-math-negpow-neg--neg1-0-dot5 ()

  (should (equal NaN:float64 NaN:float64)))

(ert-deftest test-math-negpow-negnan-2 ()

  (should (equal NaN:float64 NaN:float64)))

(ert-deftest test-math-negpow-neg-plusinf-2 ()

  (should (equal Infinity:float64 Infinity:float64)))

(ert-deftest test-math-negpow-neg-neginf-2 ()

  (should (equal Infinity:float64 Infinity:float64)))

(ert-deftest test-math-negpow-neg-plusinf--neg1 ()

  (should (equal 0.0:float64 0.0:float64)))

(ert-deftest test-math-negpow-neg2-nan ()

  (should (equal NaN:float64 NaN:float64)))

(ert-deftest test-math-negpow-neg2--plusinf ()

  (should (equal Infinity:float64 Infinity:float64)))

(ert-deftest test-math-negpow-neg2--neginf ()

  (should (equal 0.0:float64 0.0:float64)))

;; sqrt

(ert-deftest test-math-negsqrt-negsqrt-4 ()

  (should (equal 2.0:float64 2.0:float64)))

(ert-deftest test-math-negsqrt-negsqrt-9 ()

  (should (equal 3.0:float64 3.0:float64)))

(ert-deftest test-math-negsqrt-negsqrt-2 ()

  (should (equal 1.4142135623730951:float64 1.4142135623730951:float64)))

(ert-deftest test-math-negsqrt-negsqrt-0 ()

  (should (equal 0.0:float64 0.0:float64)))

(ert-deftest test-math-negsqrt-negsqrt-3 ()

  (should (equal 1.73205080757:float64 1.73205080757:float64)))

(ert-deftest test-math-negsqrt-negsqrt-negative ()

  (should (equal NaN:float64 NaN:float64)))

(ert-deftest test-math-negsqrt-negsqrt-nan ()

  (should (equal NaN:float64 NaN:float64)))

(ert-deftest test-math-negsqrt-negsqrt--plusinf ()

  (should (equal Infinity:float64 Infinity:float64)))

(ert-deftest test-math-negsqrt-negsqrt--neginf ()

  (should (equal NaN:float64 NaN:float64)))

;; ceiling

(ert-deftest test-math-negceiling-negceiling-3-dot2 ()

  (should (equal 4.0:float64 4.0:float64)))

(ert-deftest test-math-negceiling-negceiling-3-dot0 ()

  (should (equal 3.0:float64 3.0:float64)))

(ert-deftest test-math-negceiling-negceiling--neg3-dot2 ()

  (should (equal -3.0:float64 -3.0:float64)))

(ert-deftest test-math-negceiling-negceiling--neg3-dot0 ()

  (should (equal -3.0:float64 -3.0:float64)))

(ert-deftest test-math-negceiling-negceiling-nan ()

  (should (equal NaN:float64 NaN:float64)))

(ert-deftest test-math-negceiling-negceiling--plusinf ()

  (should (equal Infinity:float64 Infinity:float64)))

(ert-deftest test-math-negceiling-negceiling--neginf ()

  (should (equal -Infinity:float64 -Infinity:float64)))

;; floor

(ert-deftest test-math-negfloor-negfloor-3-dot8 ()

  (should (equal 3.0:float64 3.0:float64)))

(ert-deftest test-math-negfloor-negfloor-3-dot0 ()

  (should (equal 3.0:float64 3.0:float64)))

(ert-deftest test-math-negfloor-negfloor--neg3-dot2 ()

  (should (equal -4.0:float64 -4.0:float64)))

(ert-deftest test-math-negfloor-negfloor--neg3-dot0 ()

  (should (equal -3.0:float64 -3.0:float64)))

(ert-deftest test-math-negfloor-negfloor-nan ()

  (should (equal NaN:float64 NaN:float64)))

(ert-deftest test-math-negfloor-negfloor--plusinf ()

  (should (equal Infinity:float64 Infinity:float64)))

(ert-deftest test-math-negfloor-negfloor--neginf ()

  (should (equal -Infinity:float64 -Infinity:float64)))

;; round

(ert-deftest test-math-neground-neground-3-dot4 ()

  (should (equal 3.0:float64 3.0:float64)))

(ert-deftest test-math-neground-neground-3-dot5 ()

  (should (equal 4.0:float64 4.0:float64)))

(ert-deftest test-math-neground-neground-3-dot6 ()

  (should (equal 4.0:float64 4.0:float64)))

(ert-deftest test-math-neground-neground--neg3-dot4 ()

  (should (equal -3.0:float64 -3.0:float64)))

(ert-deftest test-math-neground-neground--neg3-dot5 ()

  (should (equal -4.0:float64 -4.0:float64)))

(ert-deftest test-math-neground-neground-nan ()

  (should (equal NaN:float64 NaN:float64)))

(ert-deftest test-math-neground-neground--plusinf ()

  (should (equal Infinity:float64 Infinity:float64)))

(ert-deftest test-math-neground-neground--neginf ()

  (should (equal -Infinity:float64 -Infinity:float64)))

;; roundBigfloat

(ert-deftest test-math-negroundbigfloat-negzero ()

  (should (equal 0.0:bigfloat 0.0:bigfloat)))

(ert-deftest test-math-negroundbigfloat-neground-pi-to-4-digits ()

  (should (equal 3.142:bigfloat 3.142:bigfloat)))

(ert-deftest test-math-negroundbigfloat-neground-1234-dot5-to-3-digits ()

  (should (equal 1230.0:bigfloat 1230.0:bigfloat)))

(ert-deftest test-math-negroundbigfloat-neground-0-dot001234-to-2-digits ()

  (should (equal 1.2e-3:bigfloat 1.2e-3:bigfloat)))

(ert-deftest test-math-negroundbigfloat-negnegative ()

  (should (equal -1230.0:bigfloat -1230.0:bigfloat)))

;; roundFloat32

(ert-deftest test-math-negroundfloat32-negzero ()

  (should (equal 0.0:float32 0.0:float32)))

(ert-deftest test-math-negroundfloat32-neground-pi-to-4-digits ()

  (should (equal 3.142:float32 3.142:float32)))

(ert-deftest test-math-negroundfloat32-neground-1234-dot5-to-3-digits ()

  (should (equal 1230.0:float32 1230.0:float32)))

(ert-deftest test-math-negroundfloat32-negnegative ()

  (should (equal -1230.0:float32 -1230.0:float32)))

(ert-deftest test-math-negroundfloat32-negnan ()

  (should (equal NaN:float32 NaN:float32)))

(ert-deftest test-math-negroundfloat32-neg-plusinf ()

  (should (equal Infinity:float32 Infinity:float32)))

(ert-deftest test-math-negroundfloat32-neg-neginf ()

  (should (equal -Infinity:float32 -Infinity:float32)))

;; roundFloat64

(ert-deftest test-math-negroundfloat64-negzero ()

  (should (equal 0.0:float64 0.0:float64)))

(ert-deftest test-math-negroundfloat64-neground-pi-to-4-digits ()

  (should (equal 3.142:float64 3.142:float64)))

(ert-deftest test-math-negroundfloat64-neground-pi-to-10-digits ()

  (should (equal 3.141592654:float64 3.141592654:float64)))

(ert-deftest test-math-negroundfloat64-neground-1234-dot5-to-3-digits ()

  (should (equal 1230.0:float64 1230.0:float64)))

(ert-deftest test-math-negroundfloat64-neground-0-dot001234-to-2-digits ()

  (should (equal 1.2e-3:float64 1.2e-3:float64)))

(ert-deftest test-math-negroundfloat64-negnegative ()

  (should (equal -1230.0:float64 -1230.0:float64)))

(ert-deftest test-math-negroundfloat64-neground-1-digit ()

  (should (equal 10.0:float64 10.0:float64)))

(ert-deftest test-math-negroundfloat64-negnan ()

  (should (equal NaN:float64 NaN:float64)))

(ert-deftest test-math-negroundfloat64-neg-plusinf ()

  (should (equal Infinity:float64 Infinity:float64)))

(ert-deftest test-math-negroundfloat64-neg-neginf ()

  (should (equal -Infinity:float64 -Infinity:float64)))

;; truncate

(ert-deftest test-math-negtruncate-negtruncate-3-dot8 ()

  (should (equal 3.0:float64 3.0:float64)))

(ert-deftest test-math-negtruncate-negtruncate-3-dot2 ()

  (should (equal 3.0:float64 3.0:float64)))

(ert-deftest test-math-negtruncate-negtruncate--neg3-dot8 ()

  (should (equal -3.0:float64 -3.0:float64)))

(ert-deftest test-math-negtruncate-negtruncate--neg3-dot2 ()

  (should (equal -3.0:float64 -3.0:float64)))

(ert-deftest test-math-negtruncate-negtruncate-nan ()

  (should (equal NaN:float64 NaN:float64)))

(ert-deftest test-math-negtruncate-negtruncate--plusinf ()

  (should (equal Infinity:float64 Infinity:float64)))

(ert-deftest test-math-negtruncate-negtruncate--neginf ()

  (should (equal -Infinity:float64 -Infinity:float64)))
