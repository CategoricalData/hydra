;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.math primitives

(ns test-ns
  (:require [clojure.test :refer :all]))

;; abs

(deftest test-math-negabs-negpositive

  (is (= 5:int32

         5:int32)))

(deftest test-math-negabs-negnegative

  (is (= 5:int32

         5:int32)))

(deftest test-math-negabs-negzero

  (is (= 0:int32

         0:int32)))

;; add

(deftest test-math-negadd-negpositive-numbers

  (is (= 8:int32

         8:int32)))

(deftest test-math-negadd-negnegative-numbers

  (is (= -8:int32

         -8:int32)))

(deftest test-math-negadd-negmixed-sign

  (is (= 7:int32

         7:int32)))

(deftest test-math-negadd-negwith-zero

  (is (= 42:int32

         42:int32)))

;; div

(deftest test-math-negdiv-negexact-division

  (is (= 5:int32

         5:int32)))

(deftest test-math-negdiv-negtruncates-toward-negative-infinity

  (is (= 3:int32

         3:int32)))

(deftest test-math-negdiv-negnegative-dividend

  (is (= -4:int32

         -4:int32)))

(deftest test-math-negdiv-negnegative-divisor

  (is (= -4:int32

         -4:int32)))

;; even

(deftest test-math-negeven-negeven-positive

  (is (= true

         true)))

(deftest test-math-negeven-negodd-positive

  (is (= false

         false)))

(deftest test-math-negeven-negeven-negative

  (is (= true

         true)))

(deftest test-math-negeven-negodd-negative

  (is (= false

         false)))

(deftest test-math-negeven-negzero

  (is (= true

         true)))

;; max

(deftest test-math-negmax-negfirst-is-larger

  (is (= 10:int32

         10:int32)))

(deftest test-math-negmax-negsecond-is-larger

  (is (= 10:int32

         10:int32)))

(deftest test-math-negmax-negequal-values

  (is (= 7:int32

         7:int32)))

(deftest test-math-negmax-negnegative-numbers

  (is (= -3:int32

         -3:int32)))

(deftest test-math-negmax-negmixed-sign

  (is (= 5:int32

         5:int32)))

(deftest test-math-negmax-negwith-zero

  (is (= 42:int32

         42:int32)))

;; maybeDiv

(deftest test-math-negmaybediv-negbasic-division

  (is (= just(3:int32)

         just(3:int32))))

(deftest test-math-negmaybediv-negexact-division

  (is (= just(5:int32)

         just(5:int32))))

(deftest test-math-negmaybediv-negdivision-by-zero

  (is (= nothing

         nothing)))

(deftest test-math-negmaybediv-negzero-divided

  (is (= just(0:int32)

         just(0:int32))))

(deftest test-math-negmaybediv-negnegative-dividend

  (is (= just(-4:int32)

         just(-4:int32))))

(deftest test-math-negmaybediv-negnegative-divisor

  (is (= just(-4:int32)

         just(-4:int32))))

;; min

(deftest test-math-negmin-negfirst-is-smaller

  (is (= 5:int32

         5:int32)))

(deftest test-math-negmin-negsecond-is-smaller

  (is (= 5:int32

         5:int32)))

(deftest test-math-negmin-negequal-values

  (is (= 7:int32

         7:int32)))

(deftest test-math-negmin-negnegative-numbers

  (is (= -5:int32

         -5:int32)))

(deftest test-math-negmin-negmixed-sign

  (is (= -5:int32

         -5:int32)))

(deftest test-math-negmin-negwith-zero

  (is (= 0:int32

         0:int32)))

;; maybeMod

(deftest test-math-negmaybemod-negbasic-modulo

  (is (= just(1:int32)

         just(1:int32))))

(deftest test-math-negmaybemod-negexact-division

  (is (= just(0:int32)

         just(0:int32))))

(deftest test-math-negmaybemod-negdivision-by-zero

  (is (= nothing

         nothing)))

(deftest test-math-negmaybemod-negnegative-dividend

  (is (= just(2:int32)

         just(2:int32))))

(deftest test-math-negmaybemod-negnegative-divisor

  (is (= just(-2:int32)

         just(-2:int32))))

;; mod

(deftest test-math-negmod-negbasic-modulo

  (is (= 1:int32

         1:int32)))

(deftest test-math-negmod-negexact-division

  (is (= 0:int32

         0:int32)))

(deftest test-math-negmod-negnegative-dividend

  (is (= 2:int32

         2:int32)))

(deftest test-math-negmod-negnegative-divisor

  (is (= -2:int32

         -2:int32)))

;; mul

(deftest test-math-negmul-negpositive-numbers

  (is (= 15:int32

         15:int32)))

(deftest test-math-negmul-negnegative-numbers

  (is (= 15:int32

         15:int32)))

(deftest test-math-negmul-negmixed-sign

  (is (= -15:int32

         -15:int32)))

(deftest test-math-negmul-negwith-zero

  (is (= 0:int32

         0:int32)))

(deftest test-math-negmul-negwith-one

  (is (= 42:int32

         42:int32)))

;; negate

(deftest test-math-negnegate-negpositive

  (is (= -5:int32

         -5:int32)))

(deftest test-math-negnegate-negnegative

  (is (= 5:int32

         5:int32)))

(deftest test-math-negnegate-negzero

  (is (= 0:int32

         0:int32)))

;; odd

(deftest test-math-negodd-negodd-positive

  (is (= true

         true)))

(deftest test-math-negodd-negeven-positive

  (is (= false

         false)))

(deftest test-math-negodd-negodd-negative

  (is (= true

         true)))

(deftest test-math-negodd-negeven-negative

  (is (= false

         false)))

(deftest test-math-negodd-negzero

  (is (= false

         false)))

;; maybePred

(deftest test-math-negmaybepred-negpositive

  (is (= just(4:int32)

         just(4:int32))))

(deftest test-math-negmaybepred-negzero

  (is (= just(-1:int32)

         just(-1:int32))))

(deftest test-math-negmaybepred-negnegative

  (is (= just(-6:int32)

         just(-6:int32))))

(deftest test-math-negmaybepred-negminbound

  (is (= nothing

         nothing)))

;; pred

(deftest test-math-negpred-negpositive

  (is (= 4:int32

         4:int32)))

(deftest test-math-negpred-negzero

  (is (= -1:int32

         -1:int32)))

(deftest test-math-negpred-negnegative

  (is (= -6:int32

         -6:int32)))

;; range

(deftest test-math-negrange-negascending-range

  (is (= [1:int32, 2:int32, 3:int32, 4:int32, 5:int32]

         [1:int32, 2:int32, 3:int32, 4:int32, 5:int32])))

(deftest test-math-negrange-negsingle-element

  (is (= [5:int32]

         [5:int32])))

(deftest test-math-negrange-negtwo-elements

  (is (= [3:int32, 4:int32]

         [3:int32, 4:int32])))

(deftest test-math-negrange-negnegative-start

  (is (= [-2:int32, -1:int32, 0:int32, 1:int32, 2:int32]

         [-2:int32, -1:int32, 0:int32, 1:int32, 2:int32])))

;; maybeRem

(deftest test-math-negmayberem-negbasic-remainder

  (is (= just(1:int32)

         just(1:int32))))

(deftest test-math-negmayberem-negexact-division

  (is (= just(0:int32)

         just(0:int32))))

(deftest test-math-negmayberem-negdivision-by-zero

  (is (= nothing

         nothing)))

(deftest test-math-negmayberem-negnegative-dividend

  (is (= just(-1:int32)

         just(-1:int32))))

(deftest test-math-negmayberem-negnegative-divisor

  (is (= just(1:int32)

         just(1:int32))))

;; rem

(deftest test-math-negrem-negbasic-remainder

  (is (= 1:int32

         1:int32)))

(deftest test-math-negrem-negexact-division

  (is (= 0:int32

         0:int32)))

(deftest test-math-negrem-negnegative-dividend

  (is (= -1:int32

         -1:int32)))

(deftest test-math-negrem-negnegative-divisor

  (is (= 1:int32

         1:int32)))

;; signum

(deftest test-math-negsignum-negpositive

  (is (= 1:int32

         1:int32)))

(deftest test-math-negsignum-negnegative

  (is (= -1:int32

         -1:int32)))

(deftest test-math-negsignum-negzero

  (is (= 0:int32

         0:int32)))

;; sub

(deftest test-math-negsub-negpositive-numbers

  (is (= 7:int32

         7:int32)))

(deftest test-math-negsub-negnegative-numbers

  (is (= -7:int32

         -7:int32)))

(deftest test-math-negsub-negmixed-sign

  (is (= 13:int32

         13:int32)))

(deftest test-math-negsub-negwith-zero

  (is (= 42:int32

         42:int32)))

;; maybeSucc

(deftest test-math-negmaybesucc-negpositive

  (is (= just(6:int32)

         just(6:int32))))

(deftest test-math-negmaybesucc-negzero

  (is (= just(1:int32)

         just(1:int32))))

(deftest test-math-negmaybesucc-negnegative

  (is (= just(-4:int32)

         just(-4:int32))))

(deftest test-math-negmaybesucc-negmaxbound

  (is (= nothing

         nothing)))

;; succ

(deftest test-math-negsucc-negpositive

  (is (= 6:int32

         6:int32)))

(deftest test-math-negsucc-negzero

  (is (= 1:int32

         1:int32)))

(deftest test-math-negsucc-negnegative

  (is (= -4:int32

         -4:int32)))

;; e

(deftest test-math-nege-negeuler-s-number

  (is (= 2.71828182846:float64

         2.71828182846:float64)))

;; pi

(deftest test-math-negpi-negpi-constant

  (is (= 3.14159265359:float64

         3.14159265359:float64)))

;; sin

(deftest test-math-negsin-negsin-0

  (is (= 0.0:float64

         0.0:float64)))

(deftest test-math-negsin-negsin-pi-div2

  (is (= 1.0:float64

         1.0:float64)))

(deftest test-math-negsin-negsin-pi

  (is (= 1.22464679915e-16:float64

         1.22464679915e-16:float64)))

(deftest test-math-negsin-negsin-1

  (is (= 0.841470984808:float64

         0.841470984808:float64)))

(deftest test-math-negsin-negsin-0-dot5

  (is (= 0.479425538604:float64

         0.479425538604:float64)))

;; cos

(deftest test-math-negcos-negcos-0

  (is (= 1.0:float64

         1.0:float64)))

(deftest test-math-negcos-negcos-pi-div2

  (is (= 6.12323399574e-17:float64

         6.12323399574e-17:float64)))

(deftest test-math-negcos-negcos-pi

  (is (= -1.0:float64

         -1.0:float64)))

(deftest test-math-negcos-negcos-1

  (is (= 0.540302305868:float64

         0.540302305868:float64)))

(deftest test-math-negcos-negcos-0-dot5

  (is (= 0.87758256189:float64

         0.87758256189:float64)))

;; tan

(deftest test-math-negtan-negtan-0

  (is (= 0.0:float64

         0.0:float64)))

(deftest test-math-negtan-negtan-pi-div4

  (is (= 1.0:float64

         1.0:float64)))

(deftest test-math-negtan-negtan-1

  (is (= 1.55740772465:float64

         1.55740772465:float64)))

(deftest test-math-negtan-negtan-0-dot5

  (is (= 0.546302489844:float64

         0.546302489844:float64)))

;; asin

(deftest test-math-negasin-negasin-0

  (is (= 0.0:float64

         0.0:float64)))

(deftest test-math-negasin-negasin-1

  (is (= 1.57079632679:float64

         1.57079632679:float64)))

(deftest test-math-negasin-negasin--neg1

  (is (= -1.57079632679:float64

         -1.57079632679:float64)))

(deftest test-math-negasin-negasin-0-dot5

  (is (= 0.523598775598:float64

         0.523598775598:float64)))

;; acos

(deftest test-math-negacos-negacos-1

  (is (= 0.0:float64

         0.0:float64)))

(deftest test-math-negacos-negacos-0

  (is (= 1.57079632679:float64

         1.57079632679:float64)))

(deftest test-math-negacos-negacos--neg1

  (is (= 3.14159265359:float64

         3.14159265359:float64)))

(deftest test-math-negacos-negacos-0-dot5

  (is (= 1.0471975512:float64

         1.0471975512:float64)))

;; atan

(deftest test-math-negatan-negatan-0

  (is (= 0.0:float64

         0.0:float64)))

(deftest test-math-negatan-negatan-1

  (is (= 0.785398163397:float64

         0.785398163397:float64)))

(deftest test-math-negatan-negatan-0-dot5

  (is (= 0.463647609001:float64

         0.463647609001:float64)))

;; atan2

(deftest test-math-negatan2-negatan2-1-1

  (is (= 0.785398163397:float64

         0.785398163397:float64)))

(deftest test-math-negatan2-negatan2-1-0

  (is (= 1.57079632679:float64

         1.57079632679:float64)))

(deftest test-math-negatan2-negatan2-0-1

  (is (= 0.0:float64

         0.0:float64)))

(deftest test-math-negatan2-negatan2-3-4

  (is (= 0.643501108793:float64

         0.643501108793:float64)))

;; sinh

(deftest test-math-negsinh-negsinh-0

  (is (= 0.0:float64

         0.0:float64)))

(deftest test-math-negsinh-negsinh-1

  (is (= 1.17520119364:float64

         1.17520119364:float64)))

(deftest test-math-negsinh-negsinh-2

  (is (= 3.62686040785:float64

         3.62686040785:float64)))

;; cosh

(deftest test-math-negcosh-negcosh-0

  (is (= 1.0:float64

         1.0:float64)))

(deftest test-math-negcosh-negcosh-1

  (is (= 1.54308063482:float64

         1.54308063482:float64)))

(deftest test-math-negcosh-negcosh-2

  (is (= 3.76219569108:float64

         3.76219569108:float64)))

;; tanh

(deftest test-math-negtanh-negtanh-0

  (is (= 0.0:float64

         0.0:float64)))

(deftest test-math-negtanh-negtanh-1

  (is (= 0.761594155956:float64

         0.761594155956:float64)))

(deftest test-math-negtanh-negtanh-0-dot5

  (is (= 0.46211715726:float64

         0.46211715726:float64)))

;; asinh

(deftest test-math-negasinh-negasinh-0

  (is (= 0.0:float64

         0.0:float64)))

(deftest test-math-negasinh-negasinh-1

  (is (= 0.88137358702:float64

         0.88137358702:float64)))

(deftest test-math-negasinh-negasinh-0-dot5

  (is (= 0.48121182506:float64

         0.48121182506:float64)))

;; acosh

(deftest test-math-negacosh-negacosh-1

  (is (= 0.0:float64

         0.0:float64)))

(deftest test-math-negacosh-negacosh-2

  (is (= 1.31695789692:float64

         1.31695789692:float64)))

(deftest test-math-negacosh-negacosh-3

  (is (= 1.76274717404:float64

         1.76274717404:float64)))

;; atanh

(deftest test-math-negatanh-negatanh-0

  (is (= 0.0:float64

         0.0:float64)))

(deftest test-math-negatanh-negatanh-0-dot5

  (is (= 0.549306144334:float64

         0.549306144334:float64)))

(deftest test-math-negatanh-negatanh-0-dot1

  (is (= 0.100335347731:float64

         0.100335347731:float64)))

;; exp

(deftest test-math-negexp-negexp-0

  (is (= 1.0:float64

         1.0:float64)))

(deftest test-math-negexp-negexp-1

  (is (= 2.71828182846:float64

         2.71828182846:float64)))

(deftest test-math-negexp-negexp--neg1

  (is (= 0.367879441171:float64

         0.367879441171:float64)))

(deftest test-math-negexp-negexp-2

  (is (= 7.38905609893:float64

         7.38905609893:float64)))

(deftest test-math-negexp-negexp-0-dot5

  (is (= 1.6487212707:float64

         1.6487212707:float64)))

;; log

(deftest test-math-neglog-neglog-1

  (is (= 0.0:float64

         0.0:float64)))

(deftest test-math-neglog-neglog-e

  (is (= 1.0:float64

         1.0:float64)))

(deftest test-math-neglog-neglog-2

  (is (= 0.69314718056:float64

         0.69314718056:float64)))

(deftest test-math-neglog-neglog-10

  (is (= 2.30258509299:float64

         2.30258509299:float64)))

;; logBase

(deftest test-math-neglogbase-neglog10-1

  (is (= 0.0:float64

         0.0:float64)))

(deftest test-math-neglogbase-neglog10-10

  (is (= 1.0:float64

         1.0:float64)))

(deftest test-math-neglogbase-neglog10-100

  (is (= 2.0:float64

         2.0:float64)))

(deftest test-math-neglogbase-neglog2-8

  (is (= 3.0:float64

         3.0:float64)))

(deftest test-math-neglogbase-neglog2-10

  (is (= 3.32192809489:float64

         3.32192809489:float64)))

;; pow

(deftest test-math-negpow-neg2-3

  (is (= 8.0:float64

         8.0:float64)))

(deftest test-math-negpow-neg10-0

  (is (= 1.0:float64

         1.0:float64)))

(deftest test-math-negpow-neg2--neg1

  (is (= 0.5:float64

         0.5:float64)))

(deftest test-math-negpow-neg2-0-dot5

  (is (= 1.41421356237:float64

         1.41421356237:float64)))

;; sqrt

(deftest test-math-negsqrt-negsqrt-4

  (is (= 2.0:float64

         2.0:float64)))

(deftest test-math-negsqrt-negsqrt-9

  (is (= 3.0:float64

         3.0:float64)))

(deftest test-math-negsqrt-negsqrt-2

  (is (= 1.4142135623730951:float64

         1.4142135623730951:float64)))

(deftest test-math-negsqrt-negsqrt-0

  (is (= 0.0:float64

         0.0:float64)))

(deftest test-math-negsqrt-negsqrt-3

  (is (= 1.73205080757:float64

         1.73205080757:float64)))

;; ceiling

(deftest test-math-negceiling-negceiling-3-dot2

  (is (= 4:bigint

         4:bigint)))

(deftest test-math-negceiling-negceiling-3-dot0

  (is (= 3:bigint

         3:bigint)))

(deftest test-math-negceiling-negceiling--neg3-dot2

  (is (= -3:bigint

         -3:bigint)))

(deftest test-math-negceiling-negceiling--neg3-dot0

  (is (= -3:bigint

         -3:bigint)))

;; floor

(deftest test-math-negfloor-negfloor-3-dot8

  (is (= 3:bigint

         3:bigint)))

(deftest test-math-negfloor-negfloor-3-dot0

  (is (= 3:bigint

         3:bigint)))

(deftest test-math-negfloor-negfloor--neg3-dot2

  (is (= -4:bigint

         -4:bigint)))

(deftest test-math-negfloor-negfloor--neg3-dot0

  (is (= -3:bigint

         -3:bigint)))

;; round

(deftest test-math-neground-neground-3-dot4

  (is (= 3:bigint

         3:bigint)))

(deftest test-math-neground-neground-3-dot5

  (is (= 4:bigint

         4:bigint)))

(deftest test-math-neground-neground-3-dot6

  (is (= 4:bigint

         4:bigint)))

(deftest test-math-neground-neground--neg3-dot4

  (is (= -3:bigint

         -3:bigint)))

(deftest test-math-neground-neground--neg3-dot5

  (is (= -4:bigint

         -4:bigint)))

;; roundBigfloat

(deftest test-math-negroundbigfloat-negzero

  (is (= 0.0:bigfloat

         0.0:bigfloat)))

(deftest test-math-negroundbigfloat-neground-pi-to-4-digits

  (is (= 3.142:bigfloat

         3.142:bigfloat)))

(deftest test-math-negroundbigfloat-neground-1234-dot5-to-3-digits

  (is (= 1230.0:bigfloat

         1230.0:bigfloat)))

(deftest test-math-negroundbigfloat-neground-0-dot001234-to-2-digits

  (is (= 1.2e-3:bigfloat

         1.2e-3:bigfloat)))

(deftest test-math-negroundbigfloat-negnegative

  (is (= -1230.0:bigfloat

         -1230.0:bigfloat)))

;; roundFloat32

(deftest test-math-negroundfloat32-negzero

  (is (= 0.0:float32

         0.0:float32)))

(deftest test-math-negroundfloat32-neground-pi-to-4-digits

  (is (= 3.142:float32

         3.142:float32)))

(deftest test-math-negroundfloat32-neground-1234-dot5-to-3-digits

  (is (= 1230.0:float32

         1230.0:float32)))

(deftest test-math-negroundfloat32-negnegative

  (is (= -1230.0:float32

         -1230.0:float32)))

;; roundFloat64

(deftest test-math-negroundfloat64-negzero

  (is (= 0.0:float64

         0.0:float64)))

(deftest test-math-negroundfloat64-neground-pi-to-4-digits

  (is (= 3.142:float64

         3.142:float64)))

(deftest test-math-negroundfloat64-neground-pi-to-10-digits

  (is (= 3.141592654:float64

         3.141592654:float64)))

(deftest test-math-negroundfloat64-neground-1234-dot5-to-3-digits

  (is (= 1230.0:float64

         1230.0:float64)))

(deftest test-math-negroundfloat64-neground-0-dot001234-to-2-digits

  (is (= 1.2e-3:float64

         1.2e-3:float64)))

(deftest test-math-negroundfloat64-negnegative

  (is (= -1230.0:float64

         -1230.0:float64)))

(deftest test-math-negroundfloat64-neground-1-digit

  (is (= 10.0:float64

         10.0:float64)))

;; truncate

(deftest test-math-negtruncate-negtruncate-3-dot8

  (is (= 3:bigint

         3:bigint)))

(deftest test-math-negtruncate-negtruncate-3-dot2

  (is (= 3:bigint

         3:bigint)))

(deftest test-math-negtruncate-negtruncate--neg3-dot8

  (is (= -3:bigint

         -3:bigint)))

(deftest test-math-negtruncate-negtruncate--neg3-dot2

  (is (= -3:bigint

         -3:bigint)))
