;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.math primitives

(import (scheme base))

;; abs

(define (test-math-negabs-negpositive)

  (assert (equal? 5:int32 5:int32)))

(define (test-math-negabs-negnegative)

  (assert (equal? 5:int32 5:int32)))

(define (test-math-negabs-negzero)

  (assert (equal? 0:int32 0:int32)))

;; add

(define (test-math-negadd-negpositive-numbers)

  (assert (equal? 8:int32 8:int32)))

(define (test-math-negadd-negnegative-numbers)

  (assert (equal? -8:int32 -8:int32)))

(define (test-math-negadd-negmixed-sign)

  (assert (equal? 7:int32 7:int32)))

(define (test-math-negadd-negwith-zero)

  (assert (equal? 42:int32 42:int32)))

;; div

(define (test-math-negdiv-negexact-division)

  (assert (equal? 5:int32 5:int32)))

(define (test-math-negdiv-negtruncates-toward-negative-infinity)

  (assert (equal? 3:int32 3:int32)))

(define (test-math-negdiv-negnegative-dividend)

  (assert (equal? -4:int32 -4:int32)))

(define (test-math-negdiv-negnegative-divisor)

  (assert (equal? -4:int32 -4:int32)))

;; even

(define (test-math-negeven-negeven-positive)

  (assert (equal? true true)))

(define (test-math-negeven-negodd-positive)

  (assert (equal? false false)))

(define (test-math-negeven-negeven-negative)

  (assert (equal? true true)))

(define (test-math-negeven-negodd-negative)

  (assert (equal? false false)))

(define (test-math-negeven-negzero)

  (assert (equal? true true)))

;; max

(define (test-math-negmax-negfirst-is-larger)

  (assert (equal? 10:int32 10:int32)))

(define (test-math-negmax-negsecond-is-larger)

  (assert (equal? 10:int32 10:int32)))

(define (test-math-negmax-negequal-values)

  (assert (equal? 7:int32 7:int32)))

(define (test-math-negmax-negnegative-numbers)

  (assert (equal? -3:int32 -3:int32)))

(define (test-math-negmax-negmixed-sign)

  (assert (equal? 5:int32 5:int32)))

(define (test-math-negmax-negwith-zero)

  (assert (equal? 42:int32 42:int32)))

;; min

(define (test-math-negmin-negfirst-is-smaller)

  (assert (equal? 5:int32 5:int32)))

(define (test-math-negmin-negsecond-is-smaller)

  (assert (equal? 5:int32 5:int32)))

(define (test-math-negmin-negequal-values)

  (assert (equal? 7:int32 7:int32)))

(define (test-math-negmin-negnegative-numbers)

  (assert (equal? -5:int32 -5:int32)))

(define (test-math-negmin-negmixed-sign)

  (assert (equal? -5:int32 -5:int32)))

(define (test-math-negmin-negwith-zero)

  (assert (equal? 0:int32 0:int32)))

;; mod

(define (test-math-negmod-negbasic-modulo)

  (assert (equal? 1:int32 1:int32)))

(define (test-math-negmod-negexact-division)

  (assert (equal? 0:int32 0:int32)))

(define (test-math-negmod-negnegative-dividend)

  (assert (equal? 2:int32 2:int32)))

(define (test-math-negmod-negnegative-divisor)

  (assert (equal? -2:int32 -2:int32)))

;; mul

(define (test-math-negmul-negpositive-numbers)

  (assert (equal? 15:int32 15:int32)))

(define (test-math-negmul-negnegative-numbers)

  (assert (equal? 15:int32 15:int32)))

(define (test-math-negmul-negmixed-sign)

  (assert (equal? -15:int32 -15:int32)))

(define (test-math-negmul-negwith-zero)

  (assert (equal? 0:int32 0:int32)))

(define (test-math-negmul-negwith-one)

  (assert (equal? 42:int32 42:int32)))

;; negate

(define (test-math-negnegate-negpositive)

  (assert (equal? -5:int32 -5:int32)))

(define (test-math-negnegate-negnegative)

  (assert (equal? 5:int32 5:int32)))

(define (test-math-negnegate-negzero)

  (assert (equal? 0:int32 0:int32)))

;; odd

(define (test-math-negodd-negodd-positive)

  (assert (equal? true true)))

(define (test-math-negodd-negeven-positive)

  (assert (equal? false false)))

(define (test-math-negodd-negodd-negative)

  (assert (equal? true true)))

(define (test-math-negodd-negeven-negative)

  (assert (equal? false false)))

(define (test-math-negodd-negzero)

  (assert (equal? false false)))

;; pred

(define (test-math-negpred-negpositive)

  (assert (equal? 4:int32 4:int32)))

(define (test-math-negpred-negzero)

  (assert (equal? -1:int32 -1:int32)))

(define (test-math-negpred-negnegative)

  (assert (equal? -6:int32 -6:int32)))

;; range

(define (test-math-negrange-negascending-range)

  (assert (equal? [1:int32, 2:int32, 3:int32, 4:int32, 5:int32] [1:int32, 2:int32, 3:int32, 4:int32, 5:int32])))

(define (test-math-negrange-negsingle-element)

  (assert (equal? [5:int32] [5:int32])))

(define (test-math-negrange-negtwo-elements)

  (assert (equal? [3:int32, 4:int32] [3:int32, 4:int32])))

(define (test-math-negrange-negnegative-start)

  (assert (equal? [-2:int32, -1:int32, 0:int32, 1:int32, 2:int32] [-2:int32, -1:int32, 0:int32, 1:int32, 2:int32])))

;; rem

(define (test-math-negrem-negbasic-remainder)

  (assert (equal? 1:int32 1:int32)))

(define (test-math-negrem-negexact-division)

  (assert (equal? 0:int32 0:int32)))

(define (test-math-negrem-negnegative-dividend)

  (assert (equal? -1:int32 -1:int32)))

(define (test-math-negrem-negnegative-divisor)

  (assert (equal? 1:int32 1:int32)))

;; signum

(define (test-math-negsignum-negpositive)

  (assert (equal? 1:int32 1:int32)))

(define (test-math-negsignum-negnegative)

  (assert (equal? -1:int32 -1:int32)))

(define (test-math-negsignum-negzero)

  (assert (equal? 0:int32 0:int32)))

;; sub

(define (test-math-negsub-negpositive-numbers)

  (assert (equal? 7:int32 7:int32)))

(define (test-math-negsub-negnegative-numbers)

  (assert (equal? -7:int32 -7:int32)))

(define (test-math-negsub-negmixed-sign)

  (assert (equal? 13:int32 13:int32)))

(define (test-math-negsub-negwith-zero)

  (assert (equal? 42:int32 42:int32)))

;; succ

(define (test-math-negsucc-negpositive)

  (assert (equal? 6:int32 6:int32)))

(define (test-math-negsucc-negzero)

  (assert (equal? 1:int32 1:int32)))

(define (test-math-negsucc-negnegative)

  (assert (equal? -4:int32 -4:int32)))

;; e

(define (test-math-nege-negeuler-s-number)

  (assert (equal? 2.71828182846:float64 2.71828182846:float64)))

;; pi

(define (test-math-negpi-negpi-constant)

  (assert (equal? 3.14159265359:float64 3.14159265359:float64)))

;; sin

(define (test-math-negsin-negsin-0)

  (assert (equal? 0.0:float64 0.0:float64)))

(define (test-math-negsin-negsin-pi-div2)

  (assert (equal? 1.0:float64 1.0:float64)))

(define (test-math-negsin-negsin-pi)

  (assert (equal? 1.22464679915e-16:float64 1.22464679915e-16:float64)))

(define (test-math-negsin-negsin-1)

  (assert (equal? 0.841470984808:float64 0.841470984808:float64)))

(define (test-math-negsin-negsin-0-dot5)

  (assert (equal? 0.479425538604:float64 0.479425538604:float64)))

;; cos

(define (test-math-negcos-negcos-0)

  (assert (equal? 1.0:float64 1.0:float64)))

(define (test-math-negcos-negcos-pi-div2)

  (assert (equal? 6.12323399574e-17:float64 6.12323399574e-17:float64)))

(define (test-math-negcos-negcos-pi)

  (assert (equal? -1.0:float64 -1.0:float64)))

(define (test-math-negcos-negcos-1)

  (assert (equal? 0.540302305868:float64 0.540302305868:float64)))

(define (test-math-negcos-negcos-0-dot5)

  (assert (equal? 0.87758256189:float64 0.87758256189:float64)))

;; tan

(define (test-math-negtan-negtan-0)

  (assert (equal? 0.0:float64 0.0:float64)))

(define (test-math-negtan-negtan-pi-div4)

  (assert (equal? 1.0:float64 1.0:float64)))

(define (test-math-negtan-negtan-1)

  (assert (equal? 1.55740772465:float64 1.55740772465:float64)))

(define (test-math-negtan-negtan-0-dot5)

  (assert (equal? 0.546302489844:float64 0.546302489844:float64)))

;; asin

(define (test-math-negasin-negasin-0)

  (assert (equal? 0.0:float64 0.0:float64)))

(define (test-math-negasin-negasin-1)

  (assert (equal? 1.57079632679:float64 1.57079632679:float64)))

(define (test-math-negasin-negasin--neg1)

  (assert (equal? -1.57079632679:float64 -1.57079632679:float64)))

(define (test-math-negasin-negasin-0-dot5)

  (assert (equal? 0.523598775598:float64 0.523598775598:float64)))

;; acos

(define (test-math-negacos-negacos-1)

  (assert (equal? 0.0:float64 0.0:float64)))

(define (test-math-negacos-negacos-0)

  (assert (equal? 1.57079632679:float64 1.57079632679:float64)))

(define (test-math-negacos-negacos--neg1)

  (assert (equal? 3.14159265359:float64 3.14159265359:float64)))

(define (test-math-negacos-negacos-0-dot5)

  (assert (equal? 1.0471975512:float64 1.0471975512:float64)))

;; atan

(define (test-math-negatan-negatan-0)

  (assert (equal? 0.0:float64 0.0:float64)))

(define (test-math-negatan-negatan-1)

  (assert (equal? 0.785398163397:float64 0.785398163397:float64)))

(define (test-math-negatan-negatan-0-dot5)

  (assert (equal? 0.463647609001:float64 0.463647609001:float64)))

;; atan2

(define (test-math-negatan2-negatan2-1-1)

  (assert (equal? 0.785398163397:float64 0.785398163397:float64)))

(define (test-math-negatan2-negatan2-1-0)

  (assert (equal? 1.57079632679:float64 1.57079632679:float64)))

(define (test-math-negatan2-negatan2-0-1)

  (assert (equal? 0.0:float64 0.0:float64)))

(define (test-math-negatan2-negatan2-3-4)

  (assert (equal? 0.643501108793:float64 0.643501108793:float64)))

;; sinh

(define (test-math-negsinh-negsinh-0)

  (assert (equal? 0.0:float64 0.0:float64)))

(define (test-math-negsinh-negsinh-1)

  (assert (equal? 1.17520119364:float64 1.17520119364:float64)))

(define (test-math-negsinh-negsinh-2)

  (assert (equal? 3.62686040785:float64 3.62686040785:float64)))

;; cosh

(define (test-math-negcosh-negcosh-0)

  (assert (equal? 1.0:float64 1.0:float64)))

(define (test-math-negcosh-negcosh-1)

  (assert (equal? 1.54308063482:float64 1.54308063482:float64)))

(define (test-math-negcosh-negcosh-2)

  (assert (equal? 3.76219569108:float64 3.76219569108:float64)))

;; tanh

(define (test-math-negtanh-negtanh-0)

  (assert (equal? 0.0:float64 0.0:float64)))

(define (test-math-negtanh-negtanh-1)

  (assert (equal? 0.761594155956:float64 0.761594155956:float64)))

(define (test-math-negtanh-negtanh-0-dot5)

  (assert (equal? 0.46211715726:float64 0.46211715726:float64)))

;; asinh

(define (test-math-negasinh-negasinh-0)

  (assert (equal? 0.0:float64 0.0:float64)))

(define (test-math-negasinh-negasinh-1)

  (assert (equal? 0.88137358702:float64 0.88137358702:float64)))

(define (test-math-negasinh-negasinh-0-dot5)

  (assert (equal? 0.48121182506:float64 0.48121182506:float64)))

;; acosh

(define (test-math-negacosh-negacosh-1)

  (assert (equal? 0.0:float64 0.0:float64)))

(define (test-math-negacosh-negacosh-2)

  (assert (equal? 1.31695789692:float64 1.31695789692:float64)))

(define (test-math-negacosh-negacosh-3)

  (assert (equal? 1.76274717404:float64 1.76274717404:float64)))

;; atanh

(define (test-math-negatanh-negatanh-0)

  (assert (equal? 0.0:float64 0.0:float64)))

(define (test-math-negatanh-negatanh-0-dot5)

  (assert (equal? 0.549306144334:float64 0.549306144334:float64)))

(define (test-math-negatanh-negatanh-0-dot1)

  (assert (equal? 0.100335347731:float64 0.100335347731:float64)))

;; exp

(define (test-math-negexp-negexp-0)

  (assert (equal? 1.0:float64 1.0:float64)))

(define (test-math-negexp-negexp-1)

  (assert (equal? 2.71828182846:float64 2.71828182846:float64)))

(define (test-math-negexp-negexp--neg1)

  (assert (equal? 0.367879441171:float64 0.367879441171:float64)))

(define (test-math-negexp-negexp-2)

  (assert (equal? 7.38905609893:float64 7.38905609893:float64)))

(define (test-math-negexp-negexp-0-dot5)

  (assert (equal? 1.6487212707:float64 1.6487212707:float64)))

;; log

(define (test-math-neglog-neglog-1)

  (assert (equal? 0.0:float64 0.0:float64)))

(define (test-math-neglog-neglog-e)

  (assert (equal? 1.0:float64 1.0:float64)))

(define (test-math-neglog-neglog-2)

  (assert (equal? 0.69314718056:float64 0.69314718056:float64)))

(define (test-math-neglog-neglog-10)

  (assert (equal? 2.30258509299:float64 2.30258509299:float64)))

;; logBase

(define (test-math-neglogbase-neglog10-1)

  (assert (equal? 0.0:float64 0.0:float64)))

(define (test-math-neglogbase-neglog10-10)

  (assert (equal? 1.0:float64 1.0:float64)))

(define (test-math-neglogbase-neglog10-100)

  (assert (equal? 2.0:float64 2.0:float64)))

(define (test-math-neglogbase-neglog2-8)

  (assert (equal? 3.0:float64 3.0:float64)))

(define (test-math-neglogbase-neglog2-10)

  (assert (equal? 3.32192809489:float64 3.32192809489:float64)))

;; pow

(define (test-math-negpow-neg2-3)

  (assert (equal? 8.0:float64 8.0:float64)))

(define (test-math-negpow-neg10-0)

  (assert (equal? 1.0:float64 1.0:float64)))

(define (test-math-negpow-neg2--neg1)

  (assert (equal? 0.5:float64 0.5:float64)))

(define (test-math-negpow-neg2-0-dot5)

  (assert (equal? 1.41421356237:float64 1.41421356237:float64)))

;; sqrt

(define (test-math-negsqrt-negsqrt-4)

  (assert (equal? 2.0:float64 2.0:float64)))

(define (test-math-negsqrt-negsqrt-9)

  (assert (equal? 3.0:float64 3.0:float64)))

(define (test-math-negsqrt-negsqrt-2)

  (assert (equal? 1.4142135623730951:float64 1.4142135623730951:float64)))

(define (test-math-negsqrt-negsqrt-0)

  (assert (equal? 0.0:float64 0.0:float64)))

(define (test-math-negsqrt-negsqrt-3)

  (assert (equal? 1.73205080757:float64 1.73205080757:float64)))

;; ceiling

(define (test-math-negceiling-negceiling-3-dot2)

  (assert (equal? 4:bigint 4:bigint)))

(define (test-math-negceiling-negceiling-3-dot0)

  (assert (equal? 3:bigint 3:bigint)))

(define (test-math-negceiling-negceiling--neg3-dot2)

  (assert (equal? -3:bigint -3:bigint)))

(define (test-math-negceiling-negceiling--neg3-dot0)

  (assert (equal? -3:bigint -3:bigint)))

;; floor

(define (test-math-negfloor-negfloor-3-dot8)

  (assert (equal? 3:bigint 3:bigint)))

(define (test-math-negfloor-negfloor-3-dot0)

  (assert (equal? 3:bigint 3:bigint)))

(define (test-math-negfloor-negfloor--neg3-dot2)

  (assert (equal? -4:bigint -4:bigint)))

(define (test-math-negfloor-negfloor--neg3-dot0)

  (assert (equal? -3:bigint -3:bigint)))

;; round

(define (test-math-neground-neground-3-dot4)

  (assert (equal? 3:bigint 3:bigint)))

(define (test-math-neground-neground-3-dot5)

  (assert (equal? 4:bigint 4:bigint)))

(define (test-math-neground-neground-3-dot6)

  (assert (equal? 4:bigint 4:bigint)))

(define (test-math-neground-neground--neg3-dot4)

  (assert (equal? -3:bigint -3:bigint)))

(define (test-math-neground-neground--neg3-dot5)

  (assert (equal? -4:bigint -4:bigint)))

;; roundBigfloat

(define (test-math-negroundbigfloat-negzero)

  (assert (equal? 0.0:bigfloat 0.0:bigfloat)))

(define (test-math-negroundbigfloat-neground-pi-to-4-digits)

  (assert (equal? 3.142:bigfloat 3.142:bigfloat)))

(define (test-math-negroundbigfloat-neground-1234-dot5-to-3-digits)

  (assert (equal? 1230.0:bigfloat 1230.0:bigfloat)))

(define (test-math-negroundbigfloat-neground-0-dot001234-to-2-digits)

  (assert (equal? 1.2e-3:bigfloat 1.2e-3:bigfloat)))

(define (test-math-negroundbigfloat-negnegative)

  (assert (equal? -1230.0:bigfloat -1230.0:bigfloat)))

;; roundFloat32

(define (test-math-negroundfloat32-negzero)

  (assert (equal? 0.0:float32 0.0:float32)))

(define (test-math-negroundfloat32-neground-pi-to-4-digits)

  (assert (equal? 3.142:float32 3.142:float32)))

(define (test-math-negroundfloat32-neground-1234-dot5-to-3-digits)

  (assert (equal? 1230.0:float32 1230.0:float32)))

(define (test-math-negroundfloat32-negnegative)

  (assert (equal? -1230.0:float32 -1230.0:float32)))

;; roundFloat64

(define (test-math-negroundfloat64-negzero)

  (assert (equal? 0.0:float64 0.0:float64)))

(define (test-math-negroundfloat64-neground-pi-to-4-digits)

  (assert (equal? 3.142:float64 3.142:float64)))

(define (test-math-negroundfloat64-neground-pi-to-10-digits)

  (assert (equal? 3.141592654:float64 3.141592654:float64)))

(define (test-math-negroundfloat64-neground-1234-dot5-to-3-digits)

  (assert (equal? 1230.0:float64 1230.0:float64)))

(define (test-math-negroundfloat64-neground-0-dot001234-to-2-digits)

  (assert (equal? 1.2e-3:float64 1.2e-3:float64)))

(define (test-math-negroundfloat64-negnegative)

  (assert (equal? -1230.0:float64 -1230.0:float64)))

(define (test-math-negroundfloat64-neground-1-digit)

  (assert (equal? 10.0:float64 10.0:float64)))

;; truncate

(define (test-math-negtruncate-negtruncate-3-dot8)

  (assert (equal? 3:bigint 3:bigint)))

(define (test-math-negtruncate-negtruncate-3-dot2)

  (assert (equal? 3:bigint 3:bigint)))

(define (test-math-negtruncate-negtruncate--neg3-dot8)

  (assert (equal? -3:bigint -3:bigint)))

(define (test-math-negtruncate-negtruncate--neg3-dot2)

  (assert (equal? -3:bigint -3:bigint)))
