;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.literals primitives

(ns test-ns
  (:require [clojure.test :refer :all]))

;; bigintToInt8

(deftest test-literals-negbiginttoint8-negpositive

  (is (= 42:int8

         42:int8)))

(deftest test-literals-negbiginttoint8-negnegative

  (is (= -42:int8

         -42:int8)))

;; bigintToInt16

(deftest test-literals-negbiginttoint16-negpositive

  (is (= 1000:int16

         1000:int16)))

(deftest test-literals-negbiginttoint16-negnegative

  (is (= -1000:int16

         -1000:int16)))

;; bigintToInt32

(deftest test-literals-negbiginttoint32-negpositive

  (is (= 42:int32

         42:int32)))

(deftest test-literals-negbiginttoint32-negnegative

  (is (= -42:int32

         -42:int32)))

(deftest test-literals-negbiginttoint32-negzero

  (is (= 0:int32

         0:int32)))

;; bigintToInt64

(deftest test-literals-negbiginttoint64-negpositive

  (is (= 1000000:int64

         1000000:int64)))

(deftest test-literals-negbiginttoint64-negnegative

  (is (= -1000000:int64

         -1000000:int64)))

;; bigintToUint8

(deftest test-literals-negbiginttouint8-negzero

  (is (= 0:uint8

         0:uint8)))

(deftest test-literals-negbiginttouint8-negtypical-value

  (is (= 100:uint8

         100:uint8)))

;; bigintToUint16

(deftest test-literals-negbiginttouint16-negzero

  (is (= 0:uint16

         0:uint16)))

(deftest test-literals-negbiginttouint16-negtypical-value

  (is (= 1000:uint16

         1000:uint16)))

;; bigintToUint32

(deftest test-literals-negbiginttouint32-negzero

  (is (= 0:uint32

         0:uint32)))

(deftest test-literals-negbiginttouint32-negtypical-value

  (is (= 100000:uint32

         100000:uint32)))

;; bigintToUint64

(deftest test-literals-negbiginttouint64-negzero

  (is (= 0:uint64

         0:uint64)))

(deftest test-literals-negbiginttouint64-negtypical-value

  (is (= 1000000:uint64

         1000000:uint64)))

;; int8ToBigint

(deftest test-literals-negint8tobigint-negpositive

  (is (= 42:bigint

         42:bigint)))

(deftest test-literals-negint8tobigint-negnegative

  (is (= -42:bigint

         -42:bigint)))

(deftest test-literals-negint8tobigint-negmax-value

  (is (= 127:bigint

         127:bigint)))

(deftest test-literals-negint8tobigint-negmin-value

  (is (= -128:bigint

         -128:bigint)))

;; int16ToBigint

(deftest test-literals-negint16tobigint-negpositive

  (is (= 1000:bigint

         1000:bigint)))

(deftest test-literals-negint16tobigint-negnegative

  (is (= -1000:bigint

         -1000:bigint)))

;; int32ToBigint

(deftest test-literals-negint32tobigint-negpositive

  (is (= 42:bigint

         42:bigint)))

(deftest test-literals-negint32tobigint-negnegative

  (is (= -42:bigint

         -42:bigint)))

(deftest test-literals-negint32tobigint-negzero

  (is (= 0:bigint

         0:bigint)))

;; int64ToBigint

(deftest test-literals-negint64tobigint-negpositive

  (is (= 1000000:bigint

         1000000:bigint)))

(deftest test-literals-negint64tobigint-negnegative

  (is (= -1000000:bigint

         -1000000:bigint)))

;; uint8ToBigint

(deftest test-literals-neguint8tobigint-negzero

  (is (= 0:bigint

         0:bigint)))

(deftest test-literals-neguint8tobigint-negmax-value

  (is (= 255:bigint

         255:bigint)))

;; uint16ToBigint

(deftest test-literals-neguint16tobigint-negzero

  (is (= 0:bigint

         0:bigint)))

(deftest test-literals-neguint16tobigint-negtypical-value

  (is (= 1000:bigint

         1000:bigint)))

;; uint32ToBigint

(deftest test-literals-neguint32tobigint-negzero

  (is (= 0:bigint

         0:bigint)))

(deftest test-literals-neguint32tobigint-negtypical-value

  (is (= 100000:bigint

         100000:bigint)))

;; uint64ToBigint

(deftest test-literals-neguint64tobigint-negzero

  (is (= 0:bigint

         0:bigint)))

(deftest test-literals-neguint64tobigint-negtypical-value

  (is (= 1000000:bigint

         1000000:bigint)))

;; float32ToBigfloat

(deftest test-literals-negfloat32tobigfloat-negpositive

  (is (= 2.5:bigfloat

         2.5:bigfloat)))

(deftest test-literals-negfloat32tobigfloat-negnegative

  (is (= -2.5:bigfloat

         -2.5:bigfloat)))

(deftest test-literals-negfloat32tobigfloat-negzero

  (is (= 0.0:bigfloat

         0.0:bigfloat)))

;; float64ToBigfloat

(deftest test-literals-negfloat64tobigfloat-negpositive

  (is (= 3.14159:bigfloat

         3.14159:bigfloat)))

(deftest test-literals-negfloat64tobigfloat-negnegative

  (is (= -2.71828:bigfloat

         -2.71828:bigfloat)))

(deftest test-literals-negfloat64tobigfloat-negzero

  (is (= 0.0:bigfloat

         0.0:bigfloat)))

;; bigfloatToFloat32

(deftest test-literals-negbigfloattofloat32-negpositive

  (is (= 3.14:float32

         3.14:float32)))

(deftest test-literals-negbigfloattofloat32-negnegative

  (is (= -2.5:float32

         -2.5:float32)))

(deftest test-literals-negbigfloattofloat32-negzero

  (is (= 0.0:float32

         0.0:float32)))

;; bigfloatToFloat64

(deftest test-literals-negbigfloattofloat64-negpositive

  (is (= 3.14159:float64

         3.14159:float64)))

(deftest test-literals-negbigfloattofloat64-negnegative

  (is (= -2.71828:float64

         -2.71828:float64)))

(deftest test-literals-negbigfloattofloat64-negzero

  (is (= 0.0:float64

         0.0:float64)))

;; bigintToBigfloat

(deftest test-literals-negbiginttobigfloat-negpositive

  (is (= 42.0:bigfloat

         42.0:bigfloat)))

(deftest test-literals-negbiginttobigfloat-negnegative

  (is (= -42.0:bigfloat

         -42.0:bigfloat)))

(deftest test-literals-negbiginttobigfloat-negzero

  (is (= 0.0:bigfloat

         0.0:bigfloat)))

;; bigfloatToBigint

(deftest test-literals-negbigfloattobigint-negpositive

  (is (= 43:bigint

         43:bigint)))

(deftest test-literals-negbigfloattobigint-negnegative

  (is (= -43:bigint

         -43:bigint)))

(deftest test-literals-negbigfloattobigint-negzero

  (is (= 0:bigint

         0:bigint)))

(deftest test-literals-negbigfloattobigint-neground-down

  (is (= 42:bigint

         42:bigint)))

(deftest test-literals-negbigfloattobigint-neghalf-even-up

  (is (= 42:bigint

         42:bigint)))

(deftest test-literals-negbigfloattobigint-neghalf-even-down

  (is (= 44:bigint

         44:bigint)))

;; showInt8

(deftest test-literals-negshowint8-negpositive

  (is (= "42"

         "42")))

(deftest test-literals-negshowint8-negnegative

  (is (= "-42"

         "-42")))

;; showInt16

(deftest test-literals-negshowint16-negpositive

  (is (= "1000"

         "1000")))

(deftest test-literals-negshowint16-negnegative

  (is (= "-1000"

         "-1000")))

;; showInt32

(deftest test-literals-negshowint32-negpositive

  (is (= "42"

         "42")))

(deftest test-literals-negshowint32-negnegative

  (is (= "-42"

         "-42")))

(deftest test-literals-negshowint32-negzero

  (is (= "0"

         "0")))

;; showInt64

(deftest test-literals-negshowint64-negpositive

  (is (= "1000000"

         "1000000")))

(deftest test-literals-negshowint64-negnegative

  (is (= "-1000000"

         "-1000000")))

;; showUint8

(deftest test-literals-negshowuint8-negzero

  (is (= "0"

         "0")))

(deftest test-literals-negshowuint8-negmax-value

  (is (= "255"

         "255")))

;; showUint16

(deftest test-literals-negshowuint16-negzero

  (is (= "0"

         "0")))

(deftest test-literals-negshowuint16-negtypical-value

  (is (= "1000"

         "1000")))

;; showUint32

(deftest test-literals-negshowuint32-negzero

  (is (= "0"

         "0")))

(deftest test-literals-negshowuint32-negtypical-value

  (is (= "100000"

         "100000")))

;; showUint64

(deftest test-literals-negshowuint64-negzero

  (is (= "0"

         "0")))

(deftest test-literals-negshowuint64-negtypical-value

  (is (= "1000000"

         "1000000")))

;; showBigint

(deftest test-literals-negshowbigint-negpositive

  (is (= "42"

         "42")))

(deftest test-literals-negshowbigint-negnegative

  (is (= "-42"

         "-42")))

(deftest test-literals-negshowbigint-negzero

  (is (= "0"

         "0")))

;; showFloat32

(deftest test-literals-negshowfloat32-negpositive

  (is (= "3.14"

         "3.14")))

(deftest test-literals-negshowfloat32-negnegative

  (is (= "-2.5"

         "-2.5")))

(deftest test-literals-negshowfloat32-negzero

  (is (= "0.0"

         "0.0")))

(deftest test-literals-negshowfloat32-negsmall-positive

  (is (= "5.0e-2"

         "5.0e-2")))

(deftest test-literals-negshowfloat32-negsmall-positive-2

  (is (= "3.0e-2"

         "3.0e-2")))

(deftest test-literals-negshowfloat32-negvery-small

  (is (= "1.0e-3"

         "1.0e-3")))

(deftest test-literals-negshowfloat32-negnormal-decimal

  (is (= "0.1"

         "0.1")))

;; showFloat64

(deftest test-literals-negshowfloat64-negpositive

  (is (= "3.14159"

         "3.14159")))

(deftest test-literals-negshowfloat64-negzero

  (is (= "0.0"

         "0.0")))

(deftest test-literals-negshowfloat64-negsmall-positive

  (is (= "5.0e-2"

         "5.0e-2")))

(deftest test-literals-negshowfloat64-negsmall-positive-2

  (is (= "3.0e-2"

         "3.0e-2")))

(deftest test-literals-negshowfloat64-negvery-small

  (is (= "1.0e-3"

         "1.0e-3")))

(deftest test-literals-negshowfloat64-negnormal-decimal

  (is (= "0.1"

         "0.1")))

;; showBigfloat

(deftest test-literals-negshowbigfloat-negpositive

  (is (= "3.14"

         "3.14")))

(deftest test-literals-negshowbigfloat-negzero

  (is (= "0.0"

         "0.0")))

(deftest test-literals-negshowbigfloat-negsmall-positive

  (is (= "5.0e-2"

         "5.0e-2")))

(deftest test-literals-negshowbigfloat-negsmall-positive-2

  (is (= "3.0e-2"

         "3.0e-2")))

(deftest test-literals-negshowbigfloat-negvery-small

  (is (= "1.0e-3"

         "1.0e-3")))

(deftest test-literals-negshowbigfloat-negnormal-decimal

  (is (= "0.1"

         "0.1")))

;; showBoolean

(deftest test-literals-negshowboolean-negtrue

  (is (= "true"

         "true")))

(deftest test-literals-negshowboolean-negfalse

  (is (= "false"

         "false")))

;; showString

(deftest test-literals-negshowstring-negsimple

  (is (= "\"hello\""

         "\"hello\"")))

(deftest test-literals-negshowstring-negempty

  (is (= "\"\""

         "\"\"")))

(deftest test-literals-negshowstring-neglatin-accented

  (is (= "\"caf\\233\""

         "\"caf\\233\"")))

(deftest test-literals-negshowstring-neggreek-lambda

  (is (= "\"\\955\""

         "\"\\955\"")))

(deftest test-literals-negshowstring-negmixed-ascii-and-non-negascii

  (is (= "\"A\\233B\""

         "\"A\\233B\"")))

(deftest test-literals-negshowstring-negtab

  (is (= "\"\\t\""

         "\"\\t\"")))

(deftest test-literals-negshowstring-negnewline

  (is (= "\"\\n\""

         "\"\\n\"")))

(deftest test-literals-negshowstring-negcarriage-return

  (is (= "\"\\r\""

         "\"\\r\"")))

(deftest test-literals-negshowstring-negbackslash

  (is (= "\"\\\\\""

         "\"\\\\\"")))

(deftest test-literals-negshowstring-negdouble-quote

  (is (= "\"\\\"\""

         "\"\\\"\"")))

(deftest test-literals-negshowstring-negnull

  (is (= "\"\\NUL\""

         "\"\\NUL\"")))

(deftest test-literals-negshowstring-negbell

  (is (= "\"\\a\""

         "\"\\a\"")))

(deftest test-literals-negshowstring-negbackspace

  (is (= "\"\\b\""

         "\"\\b\"")))

(deftest test-literals-negshowstring-negform-feed

  (is (= "\"\\f\""

         "\"\\f\"")))

(deftest test-literals-negshowstring-negvertical-tab

  (is (= "\"\\v\""

         "\"\\v\"")))

(deftest test-literals-negshowstring-negdelete

  (is (= "\"\\DEL\""

         "\"\\DEL\"")))

;; readInt8

(deftest test-literals-negreadint8-negpositive

  (is (= just(42:int8)

         just(42:int8))))

(deftest test-literals-negreadint8-negnegative

  (is (= just(-42:int8)

         just(-42:int8))))

(deftest test-literals-negreadint8-negmax-value

  (is (= just(127:int8)

         just(127:int8))))

(deftest test-literals-negreadint8-negmin-value

  (is (= just(-128:int8)

         just(-128:int8))))

(deftest test-literals-negreadint8-neginvalid

  (is (= nothing

         nothing)))

(deftest test-literals-negreadint8-negoverflow

  (is (= nothing

         nothing)))

;; readInt16

(deftest test-literals-negreadint16-negpositive

  (is (= just(1000:int16)

         just(1000:int16))))

(deftest test-literals-negreadint16-negnegative

  (is (= just(-1000:int16)

         just(-1000:int16))))

(deftest test-literals-negreadint16-neginvalid

  (is (= nothing

         nothing)))

;; readInt32

(deftest test-literals-negreadint32-negpositive

  (is (= just(42:int32)

         just(42:int32))))

(deftest test-literals-negreadint32-negnegative

  (is (= just(-42:int32)

         just(-42:int32))))

(deftest test-literals-negreadint32-neginvalid

  (is (= nothing

         nothing)))

;; readInt64

(deftest test-literals-negreadint64-negpositive

  (is (= just(1000000:int64)

         just(1000000:int64))))

(deftest test-literals-negreadint64-negnegative

  (is (= just(-1000000:int64)

         just(-1000000:int64))))

(deftest test-literals-negreadint64-neginvalid

  (is (= nothing

         nothing)))

;; readUint8

(deftest test-literals-negreaduint8-negzero

  (is (= just(0:uint8)

         just(0:uint8))))

(deftest test-literals-negreaduint8-negtypical

  (is (= just(100:uint8)

         just(100:uint8))))

(deftest test-literals-negreaduint8-negmax-value

  (is (= just(255:uint8)

         just(255:uint8))))

(deftest test-literals-negreaduint8-neginvalid

  (is (= nothing

         nothing)))

(deftest test-literals-negreaduint8-negnegative

  (is (= nothing

         nothing)))

;; readUint16

(deftest test-literals-negreaduint16-negzero

  (is (= just(0:uint16)

         just(0:uint16))))

(deftest test-literals-negreaduint16-negtypical

  (is (= just(1000:uint16)

         just(1000:uint16))))

(deftest test-literals-negreaduint16-neginvalid

  (is (= nothing

         nothing)))

(deftest test-literals-negreaduint16-negnegative

  (is (= nothing

         nothing)))

;; readUint32

(deftest test-literals-negreaduint32-negzero

  (is (= just(0:uint32)

         just(0:uint32))))

(deftest test-literals-negreaduint32-negtypical

  (is (= just(100000:uint32)

         just(100000:uint32))))

(deftest test-literals-negreaduint32-neginvalid

  (is (= nothing

         nothing)))

(deftest test-literals-negreaduint32-negnegative

  (is (= nothing

         nothing)))

;; readUint64

(deftest test-literals-negreaduint64-negzero

  (is (= just(0:uint64)

         just(0:uint64))))

(deftest test-literals-negreaduint64-negtypical

  (is (= just(1000000:uint64)

         just(1000000:uint64))))

(deftest test-literals-negreaduint64-neginvalid

  (is (= nothing

         nothing)))

(deftest test-literals-negreaduint64-negnegative

  (is (= nothing

         nothing)))

;; readBigint

(deftest test-literals-negreadbigint-negpositive

  (is (= just(42:bigint)

         just(42:bigint))))

(deftest test-literals-negreadbigint-negnegative

  (is (= just(-42:bigint)

         just(-42:bigint))))

(deftest test-literals-negreadbigint-negzero

  (is (= just(0:bigint)

         just(0:bigint))))

(deftest test-literals-negreadbigint-neglarge

  (is (= just(123456789012345678901234567890:bigint)

         just(123456789012345678901234567890:bigint))))

(deftest test-literals-negreadbigint-neginvalid

  (is (= nothing

         nothing)))

;; readFloat32

(deftest test-literals-negreadfloat32-negpositive

  (is (= just(3.14:float32)

         just(3.14:float32))))

(deftest test-literals-negreadfloat32-negnegative

  (is (= just(-2.5:float32)

         just(-2.5:float32))))

(deftest test-literals-negreadfloat32-neginvalid

  (is (= nothing

         nothing)))

;; readFloat64

(deftest test-literals-negreadfloat64-negpositive

  (is (= just(3.14159:float64)

         just(3.14159:float64))))

(deftest test-literals-negreadfloat64-negnegative

  (is (= just(-2.71828:float64)

         just(-2.71828:float64))))

(deftest test-literals-negreadfloat64-neginvalid

  (is (= nothing

         nothing)))

;; readBigfloat

(deftest test-literals-negreadbigfloat-negpositive

  (is (= just(3.14:bigfloat)

         just(3.14:bigfloat))))

(deftest test-literals-negreadbigfloat-neginvalid

  (is (= nothing

         nothing)))

;; readBoolean

(deftest test-literals-negreadboolean-negtrue

  (is (= just(true)

         just(true))))

(deftest test-literals-negreadboolean-negfalse

  (is (= just(false)

         just(false))))

(deftest test-literals-negreadboolean-neginvalid

  (is (= nothing

         nothing)))

;; readString

(deftest test-literals-negreadstring-negquoted-string

  (is (= just("hello")

         just("hello"))))

(deftest test-literals-negreadstring-negempty-quoted

  (is (= just("")

         just(""))))

(deftest test-literals-negreadstring-negunquoted

  (is (= nothing

         nothing)))

;; stringToBinary

(deftest test-literals-negstringtobinary-negsimple-base64

  (is (= [binary]

         [binary])))

(deftest test-literals-negstringtobinary-negempty-string

  (is (= [binary]

         [binary])))

;; binaryToString

(deftest test-literals-negbinarytostring-negsimple-binary

  (is (= "aGVsbG8="

         "aGVsbG8=")))

(deftest test-literals-negbinarytostring-negempty-binary

  (is (= ""

         "")))
