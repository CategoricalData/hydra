;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.math primitives

;; abs

(defun test-math-negabs-negpositive ()

  (assert (equal 5:int32 5:int32)))

(defun test-math-negabs-negnegative ()

  (assert (equal 5:int32 5:int32)))

(defun test-math-negabs-negzero ()

  (assert (equal 0:int32 0:int32)))

;; add

(defun test-math-negadd-negpositive-numbers ()

  (assert (equal 8:int32 8:int32)))

(defun test-math-negadd-negnegative-numbers ()

  (assert (equal -8:int32 -8:int32)))

(defun test-math-negadd-negmixed-sign ()

  (assert (equal 7:int32 7:int32)))

(defun test-math-negadd-negwith-zero ()

  (assert (equal 42:int32 42:int32)))

;; div

(defun test-math-negdiv-negexact-division ()

  (assert (equal 5:int32 5:int32)))

(defun test-math-negdiv-negtruncates-toward-negative-infinity ()

  (assert (equal 3:int32 3:int32)))

(defun test-math-negdiv-negnegative-dividend ()

  (assert (equal -4:int32 -4:int32)))

(defun test-math-negdiv-negnegative-divisor ()

  (assert (equal -4:int32 -4:int32)))

;; even

(defun test-math-negeven-negeven-positive ()

  (assert (equal true true)))

(defun test-math-negeven-negodd-positive ()

  (assert (equal false false)))

(defun test-math-negeven-negeven-negative ()

  (assert (equal true true)))

(defun test-math-negeven-negodd-negative ()

  (assert (equal false false)))

(defun test-math-negeven-negzero ()

  (assert (equal true true)))

;; max

(defun test-math-negmax-negfirst-is-larger ()

  (assert (equal 10:int32 10:int32)))

(defun test-math-negmax-negsecond-is-larger ()

  (assert (equal 10:int32 10:int32)))

(defun test-math-negmax-negequal-values ()

  (assert (equal 7:int32 7:int32)))

(defun test-math-negmax-negnegative-numbers ()

  (assert (equal -3:int32 -3:int32)))

(defun test-math-negmax-negmixed-sign ()

  (assert (equal 5:int32 5:int32)))

(defun test-math-negmax-negwith-zero ()

  (assert (equal 42:int32 42:int32)))

;; maybeDiv

(defun test-math-negmaybediv-negbasic-division ()

  (assert (equal just(3:int32) just(3:int32))))

(defun test-math-negmaybediv-negexact-division ()

  (assert (equal just(5:int32) just(5:int32))))

(defun test-math-negmaybediv-negdivision-by-zero ()

  (assert (equal nothing nothing)))

(defun test-math-negmaybediv-negzero-divided ()

  (assert (equal just(0:int32) just(0:int32))))

(defun test-math-negmaybediv-negnegative-dividend ()

  (assert (equal just(-4:int32) just(-4:int32))))

(defun test-math-negmaybediv-negnegative-divisor ()

  (assert (equal just(-4:int32) just(-4:int32))))

;; min

(defun test-math-negmin-negfirst-is-smaller ()

  (assert (equal 5:int32 5:int32)))

(defun test-math-negmin-negsecond-is-smaller ()

  (assert (equal 5:int32 5:int32)))

(defun test-math-negmin-negequal-values ()

  (assert (equal 7:int32 7:int32)))

(defun test-math-negmin-negnegative-numbers ()

  (assert (equal -5:int32 -5:int32)))

(defun test-math-negmin-negmixed-sign ()

  (assert (equal -5:int32 -5:int32)))

(defun test-math-negmin-negwith-zero ()

  (assert (equal 0:int32 0:int32)))

;; maybeMod

(defun test-math-negmaybemod-negbasic-modulo ()

  (assert (equal just(1:int32) just(1:int32))))

(defun test-math-negmaybemod-negexact-division ()

  (assert (equal just(0:int32) just(0:int32))))

(defun test-math-negmaybemod-negdivision-by-zero ()

  (assert (equal nothing nothing)))

(defun test-math-negmaybemod-negnegative-dividend ()

  (assert (equal just(2:int32) just(2:int32))))

(defun test-math-negmaybemod-negnegative-divisor ()

  (assert (equal just(-2:int32) just(-2:int32))))

;; mod

(defun test-math-negmod-negbasic-modulo ()

  (assert (equal 1:int32 1:int32)))

(defun test-math-negmod-negexact-division ()

  (assert (equal 0:int32 0:int32)))

(defun test-math-negmod-negnegative-dividend ()

  (assert (equal 2:int32 2:int32)))

(defun test-math-negmod-negnegative-divisor ()

  (assert (equal -2:int32 -2:int32)))

;; mul

(defun test-math-negmul-negpositive-numbers ()

  (assert (equal 15:int32 15:int32)))

(defun test-math-negmul-negnegative-numbers ()

  (assert (equal 15:int32 15:int32)))

(defun test-math-negmul-negmixed-sign ()

  (assert (equal -15:int32 -15:int32)))

(defun test-math-negmul-negwith-zero ()

  (assert (equal 0:int32 0:int32)))

(defun test-math-negmul-negwith-one ()

  (assert (equal 42:int32 42:int32)))

;; negate

(defun test-math-negnegate-negpositive ()

  (assert (equal -5:int32 -5:int32)))

(defun test-math-negnegate-negnegative ()

  (assert (equal 5:int32 5:int32)))

(defun test-math-negnegate-negzero ()

  (assert (equal 0:int32 0:int32)))

;; odd

(defun test-math-negodd-negodd-positive ()

  (assert (equal true true)))

(defun test-math-negodd-negeven-positive ()

  (assert (equal false false)))

(defun test-math-negodd-negodd-negative ()

  (assert (equal true true)))

(defun test-math-negodd-negeven-negative ()

  (assert (equal false false)))

(defun test-math-negodd-negzero ()

  (assert (equal false false)))

;; maybePred

(defun test-math-negmaybepred-negpositive ()

  (assert (equal just(4:int32) just(4:int32))))

(defun test-math-negmaybepred-negzero ()

  (assert (equal just(-1:int32) just(-1:int32))))

(defun test-math-negmaybepred-negnegative ()

  (assert (equal just(-6:int32) just(-6:int32))))

(defun test-math-negmaybepred-negminbound ()

  (assert (equal nothing nothing)))

;; pred

(defun test-math-negpred-negpositive ()

  (assert (equal 4:int32 4:int32)))

(defun test-math-negpred-negzero ()

  (assert (equal -1:int32 -1:int32)))

(defun test-math-negpred-negnegative ()

  (assert (equal -6:int32 -6:int32)))

;; range

(defun test-math-negrange-negascending-range ()

  (assert (equal [1:int32, 2:int32, 3:int32, 4:int32, 5:int32] [1:int32, 2:int32, 3:int32, 4:int32, 5:int32])))

(defun test-math-negrange-negsingle-element ()

  (assert (equal [5:int32] [5:int32])))

(defun test-math-negrange-negtwo-elements ()

  (assert (equal [3:int32, 4:int32] [3:int32, 4:int32])))

(defun test-math-negrange-negnegative-start ()

  (assert (equal [-2:int32, -1:int32, 0:int32, 1:int32, 2:int32] [-2:int32, -1:int32, 0:int32, 1:int32, 2:int32])))

;; maybeRem

(defun test-math-negmayberem-negbasic-remainder ()

  (assert (equal just(1:int32) just(1:int32))))

(defun test-math-negmayberem-negexact-division ()

  (assert (equal just(0:int32) just(0:int32))))

(defun test-math-negmayberem-negdivision-by-zero ()

  (assert (equal nothing nothing)))

(defun test-math-negmayberem-negnegative-dividend ()

  (assert (equal just(-1:int32) just(-1:int32))))

(defun test-math-negmayberem-negnegative-divisor ()

  (assert (equal just(1:int32) just(1:int32))))

;; rem

(defun test-math-negrem-negbasic-remainder ()

  (assert (equal 1:int32 1:int32)))

(defun test-math-negrem-negexact-division ()

  (assert (equal 0:int32 0:int32)))

(defun test-math-negrem-negnegative-dividend ()

  (assert (equal -1:int32 -1:int32)))

(defun test-math-negrem-negnegative-divisor ()

  (assert (equal 1:int32 1:int32)))

;; signum

(defun test-math-negsignum-negpositive ()

  (assert (equal 1:int32 1:int32)))

(defun test-math-negsignum-negnegative ()

  (assert (equal -1:int32 -1:int32)))

(defun test-math-negsignum-negzero ()

  (assert (equal 0:int32 0:int32)))

;; sub

(defun test-math-negsub-negpositive-numbers ()

  (assert (equal 7:int32 7:int32)))

(defun test-math-negsub-negnegative-numbers ()

  (assert (equal -7:int32 -7:int32)))

(defun test-math-negsub-negmixed-sign ()

  (assert (equal 13:int32 13:int32)))

(defun test-math-negsub-negwith-zero ()

  (assert (equal 42:int32 42:int32)))

;; maybeSucc

(defun test-math-negmaybesucc-negpositive ()

  (assert (equal just(6:int32) just(6:int32))))

(defun test-math-negmaybesucc-negzero ()

  (assert (equal just(1:int32) just(1:int32))))

(defun test-math-negmaybesucc-negnegative ()

  (assert (equal just(-4:int32) just(-4:int32))))

(defun test-math-negmaybesucc-negmaxbound ()

  (assert (equal nothing nothing)))

;; succ

(defun test-math-negsucc-negpositive ()

  (assert (equal 6:int32 6:int32)))

(defun test-math-negsucc-negzero ()

  (assert (equal 1:int32 1:int32)))

(defun test-math-negsucc-negnegative ()

  (assert (equal -4:int32 -4:int32)))

;; e

(defun test-math-nege-negeuler-s-number ()

  (assert (equal 2.71828182846:float64 2.71828182846:float64)))

;; pi

(defun test-math-negpi-negpi-constant ()

  (assert (equal 3.14159265359:float64 3.14159265359:float64)))

;; sin

(defun test-math-negsin-negsin-0 ()

  (assert (equal 0.0:float64 0.0:float64)))

(defun test-math-negsin-negsin-pi-div2 ()

  (assert (equal 1.0:float64 1.0:float64)))

(defun test-math-negsin-negsin-pi ()

  (assert (equal 1.22464679915e-16:float64 1.22464679915e-16:float64)))

(defun test-math-negsin-negsin-1 ()

  (assert (equal 0.841470984808:float64 0.841470984808:float64)))

(defun test-math-negsin-negsin-0-dot5 ()

  (assert (equal 0.479425538604:float64 0.479425538604:float64)))

(defun test-math-negsin-negsin-nan ()

  (assert (equal NaN:float64 NaN:float64)))

(defun test-math-negsin-negsin--plusinf ()

  (assert (equal NaN:float64 NaN:float64)))

(defun test-math-negsin-negsin--neginf ()

  (assert (equal NaN:float64 NaN:float64)))

;; cos

(defun test-math-negcos-negcos-0 ()

  (assert (equal 1.0:float64 1.0:float64)))

(defun test-math-negcos-negcos-pi-div2 ()

  (assert (equal 6.12323399574e-17:float64 6.12323399574e-17:float64)))

(defun test-math-negcos-negcos-pi ()

  (assert (equal -1.0:float64 -1.0:float64)))

(defun test-math-negcos-negcos-1 ()

  (assert (equal 0.540302305868:float64 0.540302305868:float64)))

(defun test-math-negcos-negcos-0-dot5 ()

  (assert (equal 0.87758256189:float64 0.87758256189:float64)))

(defun test-math-negcos-negcos-nan ()

  (assert (equal NaN:float64 NaN:float64)))

(defun test-math-negcos-negcos--plusinf ()

  (assert (equal NaN:float64 NaN:float64)))

(defun test-math-negcos-negcos--neginf ()

  (assert (equal NaN:float64 NaN:float64)))

;; tan

(defun test-math-negtan-negtan-0 ()

  (assert (equal 0.0:float64 0.0:float64)))

(defun test-math-negtan-negtan-pi-div4 ()

  (assert (equal 1.0:float64 1.0:float64)))

(defun test-math-negtan-negtan-1 ()

  (assert (equal 1.55740772465:float64 1.55740772465:float64)))

(defun test-math-negtan-negtan-0-dot5 ()

  (assert (equal 0.546302489844:float64 0.546302489844:float64)))

(defun test-math-negtan-negtan-nan ()

  (assert (equal NaN:float64 NaN:float64)))

(defun test-math-negtan-negtan--plusinf ()

  (assert (equal NaN:float64 NaN:float64)))

(defun test-math-negtan-negtan--neginf ()

  (assert (equal NaN:float64 NaN:float64)))

;; asin

(defun test-math-negasin-negasin-0 ()

  (assert (equal 0.0:float64 0.0:float64)))

(defun test-math-negasin-negasin-1 ()

  (assert (equal 1.57079632679:float64 1.57079632679:float64)))

(defun test-math-negasin-negasin--neg1 ()

  (assert (equal -1.57079632679:float64 -1.57079632679:float64)))

(defun test-math-negasin-negasin-0-dot5 ()

  (assert (equal 0.523598775598:float64 0.523598775598:float64)))

(defun test-math-negasin-negasin-below-domain ()

  (assert (equal NaN:float64 NaN:float64)))

(defun test-math-negasin-negasin-above-domain ()

  (assert (equal NaN:float64 NaN:float64)))

(defun test-math-negasin-negasin-nan ()

  (assert (equal NaN:float64 NaN:float64)))

(defun test-math-negasin-negasin--plusinf ()

  (assert (equal NaN:float64 NaN:float64)))

(defun test-math-negasin-negasin--neginf ()

  (assert (equal NaN:float64 NaN:float64)))

;; acos

(defun test-math-negacos-negacos-1 ()

  (assert (equal 0.0:float64 0.0:float64)))

(defun test-math-negacos-negacos-0 ()

  (assert (equal 1.57079632679:float64 1.57079632679:float64)))

(defun test-math-negacos-negacos--neg1 ()

  (assert (equal 3.14159265359:float64 3.14159265359:float64)))

(defun test-math-negacos-negacos-0-dot5 ()

  (assert (equal 1.0471975512:float64 1.0471975512:float64)))

(defun test-math-negacos-negacos-below-domain ()

  (assert (equal NaN:float64 NaN:float64)))

(defun test-math-negacos-negacos-above-domain ()

  (assert (equal NaN:float64 NaN:float64)))

(defun test-math-negacos-negacos-nan ()

  (assert (equal NaN:float64 NaN:float64)))

(defun test-math-negacos-negacos--plusinf ()

  (assert (equal NaN:float64 NaN:float64)))

(defun test-math-negacos-negacos--neginf ()

  (assert (equal NaN:float64 NaN:float64)))

;; atan

(defun test-math-negatan-negatan-0 ()

  (assert (equal 0.0:float64 0.0:float64)))

(defun test-math-negatan-negatan-1 ()

  (assert (equal 0.785398163397:float64 0.785398163397:float64)))

(defun test-math-negatan-negatan-0-dot5 ()

  (assert (equal 0.463647609001:float64 0.463647609001:float64)))

(defun test-math-negatan-negatan-nan ()

  (assert (equal NaN:float64 NaN:float64)))

(defun test-math-negatan-negatan--plusinf ()

  (assert (equal 1.57079632679:float64 1.57079632679:float64)))

(defun test-math-negatan-negatan--neginf ()

  (assert (equal -1.57079632679:float64 -1.57079632679:float64)))

;; atan2

(defun test-math-negatan2-negatan2-1-1 ()

  (assert (equal 0.785398163397:float64 0.785398163397:float64)))

(defun test-math-negatan2-negatan2-1-0 ()

  (assert (equal 1.57079632679:float64 1.57079632679:float64)))

(defun test-math-negatan2-negatan2-0-1 ()

  (assert (equal 0.0:float64 0.0:float64)))

(defun test-math-negatan2-negatan2-3-4 ()

  (assert (equal 0.643501108793:float64 0.643501108793:float64)))

(defun test-math-negatan2-negatan2-nan-1 ()

  (assert (equal NaN:float64 NaN:float64)))

(defun test-math-negatan2-negatan2--plusinf-1 ()

  (assert (equal 1.57079632679:float64 1.57079632679:float64)))

(defun test-math-negatan2-negatan2--neginf-1 ()

  (assert (equal -1.57079632679:float64 -1.57079632679:float64)))

(defun test-math-negatan2-negatan2-1-nan ()

  (assert (equal NaN:float64 NaN:float64)))

(defun test-math-negatan2-negatan2-1--plusinf ()

  (assert (equal 0.0:float64 0.0:float64)))

(defun test-math-negatan2-negatan2-1--neginf ()

  (assert (equal 3.14159265359:float64 3.14159265359:float64)))

(defun test-math-negatan2-negatan2--plusinf--plusinf ()

  (assert (equal NaN:float64 NaN:float64)))

(defun test-math-negatan2-negatan2--plusinf--neginf ()

  (assert (equal NaN:float64 NaN:float64)))

(defun test-math-negatan2-negatan2--neginf--plusinf ()

  (assert (equal NaN:float64 NaN:float64)))

(defun test-math-negatan2-negatan2--neginf--neginf ()

  (assert (equal NaN:float64 NaN:float64)))

;; sinh

(defun test-math-negsinh-negsinh-0 ()

  (assert (equal 0.0:float64 0.0:float64)))

(defun test-math-negsinh-negsinh-1 ()

  (assert (equal 1.17520119364:float64 1.17520119364:float64)))

(defun test-math-negsinh-negsinh-2 ()

  (assert (equal 3.62686040785:float64 3.62686040785:float64)))

(defun test-math-negsinh-negsinh-nan ()

  (assert (equal NaN:float64 NaN:float64)))

(defun test-math-negsinh-negsinh--plusinf ()

  (assert (equal Infinity:float64 Infinity:float64)))

(defun test-math-negsinh-negsinh--neginf ()

  (assert (equal -Infinity:float64 -Infinity:float64)))

;; cosh

(defun test-math-negcosh-negcosh-0 ()

  (assert (equal 1.0:float64 1.0:float64)))

(defun test-math-negcosh-negcosh-1 ()

  (assert (equal 1.54308063482:float64 1.54308063482:float64)))

(defun test-math-negcosh-negcosh-2 ()

  (assert (equal 3.76219569108:float64 3.76219569108:float64)))

(defun test-math-negcosh-negcosh-nan ()

  (assert (equal NaN:float64 NaN:float64)))

(defun test-math-negcosh-negcosh--plusinf ()

  (assert (equal Infinity:float64 Infinity:float64)))

(defun test-math-negcosh-negcosh--neginf ()

  (assert (equal Infinity:float64 Infinity:float64)))

;; tanh

(defun test-math-negtanh-negtanh-0 ()

  (assert (equal 0.0:float64 0.0:float64)))

(defun test-math-negtanh-negtanh-1 ()

  (assert (equal 0.761594155956:float64 0.761594155956:float64)))

(defun test-math-negtanh-negtanh-0-dot5 ()

  (assert (equal 0.46211715726:float64 0.46211715726:float64)))

(defun test-math-negtanh-negtanh-nan ()

  (assert (equal NaN:float64 NaN:float64)))

(defun test-math-negtanh-negtanh--plusinf ()

  (assert (equal 1.0:float64 1.0:float64)))

(defun test-math-negtanh-negtanh--neginf ()

  (assert (equal -1.0:float64 -1.0:float64)))

;; asinh

(defun test-math-negasinh-negasinh-0 ()

  (assert (equal 0.0:float64 0.0:float64)))

(defun test-math-negasinh-negasinh-1 ()

  (assert (equal 0.88137358702:float64 0.88137358702:float64)))

(defun test-math-negasinh-negasinh-0-dot5 ()

  (assert (equal 0.48121182506:float64 0.48121182506:float64)))

(defun test-math-negasinh-negasinh-nan ()

  (assert (equal NaN:float64 NaN:float64)))

(defun test-math-negasinh-negasinh--plusinf ()

  (assert (equal Infinity:float64 Infinity:float64)))

(defun test-math-negasinh-negasinh--neginf ()

  (assert (equal -Infinity:float64 -Infinity:float64)))

;; acosh

(defun test-math-negacosh-negacosh-1 ()

  (assert (equal 0.0:float64 0.0:float64)))

(defun test-math-negacosh-negacosh-2 ()

  (assert (equal 1.31695789692:float64 1.31695789692:float64)))

(defun test-math-negacosh-negacosh-3 ()

  (assert (equal 1.76274717404:float64 1.76274717404:float64)))

(defun test-math-negacosh-negacosh-below-domain ()

  (assert (equal NaN:float64 NaN:float64)))

(defun test-math-negacosh-negacosh-negative ()

  (assert (equal NaN:float64 NaN:float64)))

(defun test-math-negacosh-negacosh-nan ()

  (assert (equal NaN:float64 NaN:float64)))

(defun test-math-negacosh-negacosh--plusinf ()

  (assert (equal Infinity:float64 Infinity:float64)))

(defun test-math-negacosh-negacosh--neginf ()

  (assert (equal NaN:float64 NaN:float64)))

;; atanh

(defun test-math-negatanh-negatanh-0 ()

  (assert (equal 0.0:float64 0.0:float64)))

(defun test-math-negatanh-negatanh-0-dot5 ()

  (assert (equal 0.549306144334:float64 0.549306144334:float64)))

(defun test-math-negatanh-negatanh-0-dot1 ()

  (assert (equal 0.100335347731:float64 0.100335347731:float64)))

(defun test-math-negatanh-negatanh-upper-boundary ()

  (assert (equal Infinity:float64 Infinity:float64)))

(defun test-math-negatanh-negatanh-lower-boundary ()

  (assert (equal -Infinity:float64 -Infinity:float64)))

(defun test-math-negatanh-negatanh-above-domain ()

  (assert (equal NaN:float64 NaN:float64)))

(defun test-math-negatanh-negatanh-below-domain ()

  (assert (equal NaN:float64 NaN:float64)))

(defun test-math-negatanh-negatanh-nan ()

  (assert (equal NaN:float64 NaN:float64)))

(defun test-math-negatanh-negatanh--plusinf ()

  (assert (equal NaN:float64 NaN:float64)))

(defun test-math-negatanh-negatanh--neginf ()

  (assert (equal NaN:float64 NaN:float64)))

;; exp

(defun test-math-negexp-negexp-0 ()

  (assert (equal 1.0:float64 1.0:float64)))

(defun test-math-negexp-negexp-1 ()

  (assert (equal 2.71828182846:float64 2.71828182846:float64)))

(defun test-math-negexp-negexp--neg1 ()

  (assert (equal 0.367879441171:float64 0.367879441171:float64)))

(defun test-math-negexp-negexp-2 ()

  (assert (equal 7.38905609893:float64 7.38905609893:float64)))

(defun test-math-negexp-negexp-0-dot5 ()

  (assert (equal 1.6487212707:float64 1.6487212707:float64)))

(defun test-math-negexp-negexp-nan ()

  (assert (equal NaN:float64 NaN:float64)))

(defun test-math-negexp-negexp--plusinf ()

  (assert (equal Infinity:float64 Infinity:float64)))

(defun test-math-negexp-negexp--neginf ()

  (assert (equal 0.0:float64 0.0:float64)))

;; log

(defun test-math-neglog-neglog-1 ()

  (assert (equal 0.0:float64 0.0:float64)))

(defun test-math-neglog-neglog-e ()

  (assert (equal 1.0:float64 1.0:float64)))

(defun test-math-neglog-neglog-2 ()

  (assert (equal 0.69314718056:float64 0.69314718056:float64)))

(defun test-math-neglog-neglog-10 ()

  (assert (equal 2.30258509299:float64 2.30258509299:float64)))

(defun test-math-neglog-neglog-0 ()

  (assert (equal -Infinity:float64 -Infinity:float64)))

(defun test-math-neglog-neglog-negative ()

  (assert (equal NaN:float64 NaN:float64)))

(defun test-math-neglog-neglog-nan ()

  (assert (equal NaN:float64 NaN:float64)))

(defun test-math-neglog-neglog--plusinf ()

  (assert (equal Infinity:float64 Infinity:float64)))

(defun test-math-neglog-neglog--neginf ()

  (assert (equal NaN:float64 NaN:float64)))

;; logBase

(defun test-math-neglogbase-neglog10-1 ()

  (assert (equal 0.0:float64 0.0:float64)))

(defun test-math-neglogbase-neglog10-10 ()

  (assert (equal 1.0:float64 1.0:float64)))

(defun test-math-neglogbase-neglog10-100 ()

  (assert (equal 2.0:float64 2.0:float64)))

(defun test-math-neglogbase-neglog2-8 ()

  (assert (equal 3.0:float64 3.0:float64)))

(defun test-math-neglogbase-neglog2-10 ()

  (assert (equal 3.32192809489:float64 3.32192809489:float64)))

(defun test-math-neglogbase-neglogbase-10-0 ()

  (assert (equal -Infinity:float64 -Infinity:float64)))

(defun test-math-neglogbase-neglogbase-10-negative ()

  (assert (equal NaN:float64 NaN:float64)))

(defun test-math-neglogbase-neglogbase-negative-10 ()

  (assert (equal NaN:float64 NaN:float64)))

(defun test-math-neglogbase-neglogbase-10-nan ()

  (assert (equal NaN:float64 NaN:float64)))

(defun test-math-neglogbase-neglogbase-10--plusinf ()

  (assert (equal Infinity:float64 Infinity:float64)))

(defun test-math-neglogbase-neglogbase-10--neginf ()

  (assert (equal NaN:float64 NaN:float64)))

(defun test-math-neglogbase-neglogbase-nan-10 ()

  (assert (equal NaN:float64 NaN:float64)))

(defun test-math-neglogbase-neglogbase--plusinf-10 ()

  (assert (equal 0.0:float64 0.0:float64)))

(defun test-math-neglogbase-neglogbase--neginf-10 ()

  (assert (equal NaN:float64 NaN:float64)))

;; pow

(defun test-math-negpow-neg2-3 ()

  (assert (equal 8.0:float64 8.0:float64)))

(defun test-math-negpow-neg10-0 ()

  (assert (equal 1.0:float64 1.0:float64)))

(defun test-math-negpow-neg2--neg1 ()

  (assert (equal 0.5:float64 0.5:float64)))

(defun test-math-negpow-neg2-0-dot5 ()

  (assert (equal 1.41421356237:float64 1.41421356237:float64)))

(defun test-math-negpow-neg0-0 ()

  (assert (equal 1.0:float64 1.0:float64)))

(defun test-math-negpow-neg0--neg1 ()

  (assert (equal Infinity:float64 Infinity:float64)))

(defun test-math-negpow-neg--neg1-0-dot5 ()

  (assert (equal NaN:float64 NaN:float64)))

(defun test-math-negpow-negnan-2 ()

  (assert (equal NaN:float64 NaN:float64)))

(defun test-math-negpow-neg-plusinf-2 ()

  (assert (equal Infinity:float64 Infinity:float64)))

(defun test-math-negpow-neg-neginf-2 ()

  (assert (equal Infinity:float64 Infinity:float64)))

(defun test-math-negpow-neg-plusinf--neg1 ()

  (assert (equal 0.0:float64 0.0:float64)))

(defun test-math-negpow-neg2-nan ()

  (assert (equal NaN:float64 NaN:float64)))

(defun test-math-negpow-neg2--plusinf ()

  (assert (equal Infinity:float64 Infinity:float64)))

(defun test-math-negpow-neg2--neginf ()

  (assert (equal 0.0:float64 0.0:float64)))

;; sqrt

(defun test-math-negsqrt-negsqrt-4 ()

  (assert (equal 2.0:float64 2.0:float64)))

(defun test-math-negsqrt-negsqrt-9 ()

  (assert (equal 3.0:float64 3.0:float64)))

(defun test-math-negsqrt-negsqrt-2 ()

  (assert (equal 1.4142135623730951:float64 1.4142135623730951:float64)))

(defun test-math-negsqrt-negsqrt-0 ()

  (assert (equal 0.0:float64 0.0:float64)))

(defun test-math-negsqrt-negsqrt-3 ()

  (assert (equal 1.73205080757:float64 1.73205080757:float64)))

(defun test-math-negsqrt-negsqrt-negative ()

  (assert (equal NaN:float64 NaN:float64)))

(defun test-math-negsqrt-negsqrt-nan ()

  (assert (equal NaN:float64 NaN:float64)))

(defun test-math-negsqrt-negsqrt--plusinf ()

  (assert (equal Infinity:float64 Infinity:float64)))

(defun test-math-negsqrt-negsqrt--neginf ()

  (assert (equal NaN:float64 NaN:float64)))

;; ceiling

(defun test-math-negceiling-negceiling-3-dot2 ()

  (assert (equal 4.0:float64 4.0:float64)))

(defun test-math-negceiling-negceiling-3-dot0 ()

  (assert (equal 3.0:float64 3.0:float64)))

(defun test-math-negceiling-negceiling--neg3-dot2 ()

  (assert (equal -3.0:float64 -3.0:float64)))

(defun test-math-negceiling-negceiling--neg3-dot0 ()

  (assert (equal -3.0:float64 -3.0:float64)))

(defun test-math-negceiling-negceiling-nan ()

  (assert (equal NaN:float64 NaN:float64)))

(defun test-math-negceiling-negceiling--plusinf ()

  (assert (equal Infinity:float64 Infinity:float64)))

(defun test-math-negceiling-negceiling--neginf ()

  (assert (equal -Infinity:float64 -Infinity:float64)))

;; floor

(defun test-math-negfloor-negfloor-3-dot8 ()

  (assert (equal 3.0:float64 3.0:float64)))

(defun test-math-negfloor-negfloor-3-dot0 ()

  (assert (equal 3.0:float64 3.0:float64)))

(defun test-math-negfloor-negfloor--neg3-dot2 ()

  (assert (equal -4.0:float64 -4.0:float64)))

(defun test-math-negfloor-negfloor--neg3-dot0 ()

  (assert (equal -3.0:float64 -3.0:float64)))

(defun test-math-negfloor-negfloor-nan ()

  (assert (equal NaN:float64 NaN:float64)))

(defun test-math-negfloor-negfloor--plusinf ()

  (assert (equal Infinity:float64 Infinity:float64)))

(defun test-math-negfloor-negfloor--neginf ()

  (assert (equal -Infinity:float64 -Infinity:float64)))

;; round

(defun test-math-neground-neground-3-dot4 ()

  (assert (equal 3.0:float64 3.0:float64)))

(defun test-math-neground-neground-3-dot5 ()

  (assert (equal 4.0:float64 4.0:float64)))

(defun test-math-neground-neground-3-dot6 ()

  (assert (equal 4.0:float64 4.0:float64)))

(defun test-math-neground-neground--neg3-dot4 ()

  (assert (equal -3.0:float64 -3.0:float64)))

(defun test-math-neground-neground--neg3-dot5 ()

  (assert (equal -4.0:float64 -4.0:float64)))

(defun test-math-neground-neground-nan ()

  (assert (equal NaN:float64 NaN:float64)))

(defun test-math-neground-neground--plusinf ()

  (assert (equal Infinity:float64 Infinity:float64)))

(defun test-math-neground-neground--neginf ()

  (assert (equal -Infinity:float64 -Infinity:float64)))

;; roundBigfloat

(defun test-math-negroundbigfloat-negzero ()

  (assert (equal 0.0:bigfloat 0.0:bigfloat)))

(defun test-math-negroundbigfloat-neground-pi-to-4-digits ()

  (assert (equal 3.142:bigfloat 3.142:bigfloat)))

(defun test-math-negroundbigfloat-neground-1234-dot5-to-3-digits ()

  (assert (equal 1230.0:bigfloat 1230.0:bigfloat)))

(defun test-math-negroundbigfloat-neground-0-dot001234-to-2-digits ()

  (assert (equal 1.2e-3:bigfloat 1.2e-3:bigfloat)))

(defun test-math-negroundbigfloat-negnegative ()

  (assert (equal -1230.0:bigfloat -1230.0:bigfloat)))

;; roundFloat32

(defun test-math-negroundfloat32-negzero ()

  (assert (equal 0.0:float32 0.0:float32)))

(defun test-math-negroundfloat32-neground-pi-to-4-digits ()

  (assert (equal 3.142:float32 3.142:float32)))

(defun test-math-negroundfloat32-neground-1234-dot5-to-3-digits ()

  (assert (equal 1230.0:float32 1230.0:float32)))

(defun test-math-negroundfloat32-negnegative ()

  (assert (equal -1230.0:float32 -1230.0:float32)))

(defun test-math-negroundfloat32-negnan ()

  (assert (equal NaN:float32 NaN:float32)))

(defun test-math-negroundfloat32-neg-plusinf ()

  (assert (equal Infinity:float32 Infinity:float32)))

(defun test-math-negroundfloat32-neg-neginf ()

  (assert (equal -Infinity:float32 -Infinity:float32)))

;; roundFloat64

(defun test-math-negroundfloat64-negzero ()

  (assert (equal 0.0:float64 0.0:float64)))

(defun test-math-negroundfloat64-neground-pi-to-4-digits ()

  (assert (equal 3.142:float64 3.142:float64)))

(defun test-math-negroundfloat64-neground-pi-to-10-digits ()

  (assert (equal 3.141592654:float64 3.141592654:float64)))

(defun test-math-negroundfloat64-neground-1234-dot5-to-3-digits ()

  (assert (equal 1230.0:float64 1230.0:float64)))

(defun test-math-negroundfloat64-neground-0-dot001234-to-2-digits ()

  (assert (equal 1.2e-3:float64 1.2e-3:float64)))

(defun test-math-negroundfloat64-negnegative ()

  (assert (equal -1230.0:float64 -1230.0:float64)))

(defun test-math-negroundfloat64-neground-1-digit ()

  (assert (equal 10.0:float64 10.0:float64)))

(defun test-math-negroundfloat64-negnan ()

  (assert (equal NaN:float64 NaN:float64)))

(defun test-math-negroundfloat64-neg-plusinf ()

  (assert (equal Infinity:float64 Infinity:float64)))

(defun test-math-negroundfloat64-neg-neginf ()

  (assert (equal -Infinity:float64 -Infinity:float64)))

;; truncate

(defun test-math-negtruncate-negtruncate-3-dot8 ()

  (assert (equal 3.0:float64 3.0:float64)))

(defun test-math-negtruncate-negtruncate-3-dot2 ()

  (assert (equal 3.0:float64 3.0:float64)))

(defun test-math-negtruncate-negtruncate--neg3-dot8 ()

  (assert (equal -3.0:float64 -3.0:float64)))

(defun test-math-negtruncate-negtruncate--neg3-dot2 ()

  (assert (equal -3.0:float64 -3.0:float64)))

(defun test-math-negtruncate-negtruncate-nan ()

  (assert (equal NaN:float64 NaN:float64)))

(defun test-math-negtruncate-negtruncate--plusinf ()

  (assert (equal Infinity:float64 Infinity:float64)))

(defun test-math-negtruncate-negtruncate--neginf ()

  (assert (equal -Infinity:float64 -Infinity:float64)))
