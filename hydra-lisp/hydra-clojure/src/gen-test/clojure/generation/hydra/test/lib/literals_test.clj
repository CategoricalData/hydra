;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.literals primitives

(ns generation.hydra.test.lib.literals-test
  (:require [clojure.test :refer :all]))

;; bigintToInt8

(deftest test-literals-negbiginttoint8-negpositive

  (is (= 42

         (hydra_lib_literals_bigint_to_int8 42))))

(deftest test-literals-negbiginttoint8-negnegative

  (is (= -42

         (hydra_lib_literals_bigint_to_int8 -42))))

;; bigintToInt16

(deftest test-literals-negbiginttoint16-negpositive

  (is (= 1000

         (hydra_lib_literals_bigint_to_int16 1000))))

(deftest test-literals-negbiginttoint16-negnegative

  (is (= -1000

         (hydra_lib_literals_bigint_to_int16 -1000))))

;; bigintToInt32

(deftest test-literals-negbiginttoint32-negpositive

  (is (= 42

         (hydra_lib_literals_bigint_to_int32 42))))

(deftest test-literals-negbiginttoint32-negnegative

  (is (= -42

         (hydra_lib_literals_bigint_to_int32 -42))))

(deftest test-literals-negbiginttoint32-negzero

  (is (= 0

         (hydra_lib_literals_bigint_to_int32 0))))

;; bigintToInt64

(deftest test-literals-negbiginttoint64-negpositive

  (is (= 1000000

         (hydra_lib_literals_bigint_to_int64 1000000))))

(deftest test-literals-negbiginttoint64-negnegative

  (is (= -1000000

         (hydra_lib_literals_bigint_to_int64 -1000000))))

;; bigintToUint8

(deftest test-literals-negbiginttouint8-negzero

  (is (= 0

         (hydra_lib_literals_bigint_to_uint8 0))))

(deftest test-literals-negbiginttouint8-negtypical-value

  (is (= 100

         (hydra_lib_literals_bigint_to_uint8 100))))

;; bigintToUint16

(deftest test-literals-negbiginttouint16-negzero

  (is (= 0

         (hydra_lib_literals_bigint_to_uint16 0))))

(deftest test-literals-negbiginttouint16-negtypical-value

  (is (= 1000

         (hydra_lib_literals_bigint_to_uint16 1000))))

;; bigintToUint32

(deftest test-literals-negbiginttouint32-negzero

  (is (= 0

         (hydra_lib_literals_bigint_to_uint32 0))))

(deftest test-literals-negbiginttouint32-negtypical-value

  (is (= 100000

         (hydra_lib_literals_bigint_to_uint32 100000))))

;; bigintToUint64

(deftest test-literals-negbiginttouint64-negzero

  (is (= 0

         (hydra_lib_literals_bigint_to_uint64 0))))

(deftest test-literals-negbiginttouint64-negtypical-value

  (is (= 1000000

         (hydra_lib_literals_bigint_to_uint64 1000000))))

;; int8ToBigint

(deftest test-literals-negint8tobigint-negpositive

  (is (= 42

         (hydra_lib_literals_int8_to_bigint 42))))

(deftest test-literals-negint8tobigint-negnegative

  (is (= -42

         (hydra_lib_literals_int8_to_bigint -42))))

(deftest test-literals-negint8tobigint-negmax-value

  (is (= 127

         (hydra_lib_literals_int8_to_bigint 127))))

(deftest test-literals-negint8tobigint-negmin-value

  (is (= -128

         (hydra_lib_literals_int8_to_bigint -128))))

;; int16ToBigint

(deftest test-literals-negint16tobigint-negpositive

  (is (= 1000

         (hydra_lib_literals_int16_to_bigint 1000))))

(deftest test-literals-negint16tobigint-negnegative

  (is (= -1000

         (hydra_lib_literals_int16_to_bigint -1000))))

;; int32ToBigint

(deftest test-literals-negint32tobigint-negpositive

  (is (= 42

         (hydra_lib_literals_int32_to_bigint 42))))

(deftest test-literals-negint32tobigint-negnegative

  (is (= -42

         (hydra_lib_literals_int32_to_bigint -42))))

(deftest test-literals-negint32tobigint-negzero

  (is (= 0

         (hydra_lib_literals_int32_to_bigint 0))))

;; int64ToBigint

(deftest test-literals-negint64tobigint-negpositive

  (is (= 1000000

         (hydra_lib_literals_int64_to_bigint 1000000))))

(deftest test-literals-negint64tobigint-negnegative

  (is (= -1000000

         (hydra_lib_literals_int64_to_bigint -1000000))))

;; uint8ToBigint

(deftest test-literals-neguint8tobigint-negzero

  (is (= 0

         (hydra_lib_literals_uint8_to_bigint 0))))

(deftest test-literals-neguint8tobigint-negmax-value

  (is (= 255

         (hydra_lib_literals_uint8_to_bigint 255))))

;; uint16ToBigint

(deftest test-literals-neguint16tobigint-negzero

  (is (= 0

         (hydra_lib_literals_uint16_to_bigint 0))))

(deftest test-literals-neguint16tobigint-negtypical-value

  (is (= 1000

         (hydra_lib_literals_uint16_to_bigint 1000))))

;; uint32ToBigint

(deftest test-literals-neguint32tobigint-negzero

  (is (= 0

         (hydra_lib_literals_uint32_to_bigint 0))))

(deftest test-literals-neguint32tobigint-negtypical-value

  (is (= 100000

         (hydra_lib_literals_uint32_to_bigint 100000))))

;; uint64ToBigint

(deftest test-literals-neguint64tobigint-negzero

  (is (= 0

         (hydra_lib_literals_uint64_to_bigint 0))))

(deftest test-literals-neguint64tobigint-negtypical-value

  (is (= 1000000

         (hydra_lib_literals_uint64_to_bigint 1000000))))

;; float32ToBigfloat

(deftest test-literals-negfloat32tobigfloat-negpositive

  (is (= 2.5

         (hydra_lib_literals_float32_to_bigfloat 2.5))))

(deftest test-literals-negfloat32tobigfloat-negnegative

  (is (= -2.5

         (hydra_lib_literals_float32_to_bigfloat -2.5))))

(deftest test-literals-negfloat32tobigfloat-negzero

  (is (= 0.0

         (hydra_lib_literals_float32_to_bigfloat 0.0))))

;; float64ToBigfloat

(deftest test-literals-negfloat64tobigfloat-negpositive

  (is (= 3.14159

         (hydra_lib_literals_float64_to_bigfloat 3.14159))))

(deftest test-literals-negfloat64tobigfloat-negnegative

  (is (= -2.71828

         (hydra_lib_literals_float64_to_bigfloat -2.71828))))

(deftest test-literals-negfloat64tobigfloat-negzero

  (is (= 0.0

         (hydra_lib_literals_float64_to_bigfloat 0.0))))

;; bigfloatToFloat32

(deftest test-literals-negbigfloattofloat32-negpositive

  (is (= 3.140000104904175

         (hydra_lib_literals_bigfloat_to_float32 3.14))))

(deftest test-literals-negbigfloattofloat32-negnegative

  (is (= -2.5

         (hydra_lib_literals_bigfloat_to_float32 -2.5))))

(deftest test-literals-negbigfloattofloat32-negzero

  (is (= 0.0

         (hydra_lib_literals_bigfloat_to_float32 0.0))))

;; bigfloatToFloat64

(deftest test-literals-negbigfloattofloat64-negpositive

  (is (= 3.14159

         (hydra_lib_literals_bigfloat_to_float64 3.14159))))

(deftest test-literals-negbigfloattofloat64-negnegative

  (is (= -2.71828

         (hydra_lib_literals_bigfloat_to_float64 -2.71828))))

(deftest test-literals-negbigfloattofloat64-negzero

  (is (= 0.0

         (hydra_lib_literals_bigfloat_to_float64 0.0))))

;; bigintToBigfloat

(deftest test-literals-negbiginttobigfloat-negpositive

  (is (= 42.0

         (hydra_lib_literals_bigint_to_bigfloat 42))))

(deftest test-literals-negbiginttobigfloat-negnegative

  (is (= -42.0

         (hydra_lib_literals_bigint_to_bigfloat -42))))

(deftest test-literals-negbiginttobigfloat-negzero

  (is (= 0.0

         (hydra_lib_literals_bigint_to_bigfloat 0))))

;; bigfloatToBigint

(deftest test-literals-negbigfloattobigint-negpositive

  (is (= 43

         (hydra_lib_literals_bigfloat_to_bigint 42.7))))

(deftest test-literals-negbigfloattobigint-negnegative

  (is (= -43

         (hydra_lib_literals_bigfloat_to_bigint -42.7))))

(deftest test-literals-negbigfloattobigint-negzero

  (is (= 0

         (hydra_lib_literals_bigfloat_to_bigint 0.0))))

(deftest test-literals-negbigfloattobigint-neground-down

  (is (= 42

         (hydra_lib_literals_bigfloat_to_bigint 42.3))))

(deftest test-literals-negbigfloattobigint-neghalf-even-up

  (is (= 42

         (hydra_lib_literals_bigfloat_to_bigint 42.5))))

(deftest test-literals-negbigfloattobigint-neghalf-even-down

  (is (= 44

         (hydra_lib_literals_bigfloat_to_bigint 43.5))))

;; showInt8

(deftest test-literals-negshowint8-negpositive

  (is (= "42"

         (hydra_lib_literals_show_int8 42))))

(deftest test-literals-negshowint8-negnegative

  (is (= "-42"

         (hydra_lib_literals_show_int8 -42))))

;; showInt16

(deftest test-literals-negshowint16-negpositive

  (is (= "1000"

         (hydra_lib_literals_show_int16 1000))))

(deftest test-literals-negshowint16-negnegative

  (is (= "-1000"

         (hydra_lib_literals_show_int16 -1000))))

;; showInt32

(deftest test-literals-negshowint32-negpositive

  (is (= "42"

         (hydra_lib_literals_show_int32 42))))

(deftest test-literals-negshowint32-negnegative

  (is (= "-42"

         (hydra_lib_literals_show_int32 -42))))

(deftest test-literals-negshowint32-negzero

  (is (= "0"

         (hydra_lib_literals_show_int32 0))))

;; showInt64

(deftest test-literals-negshowint64-negpositive

  (is (= "1000000"

         (hydra_lib_literals_show_int64 1000000))))

(deftest test-literals-negshowint64-negnegative

  (is (= "-1000000"

         (hydra_lib_literals_show_int64 -1000000))))

;; showUint8

(deftest test-literals-negshowuint8-negzero

  (is (= "0"

         (hydra_lib_literals_show_uint8 0))))

(deftest test-literals-negshowuint8-negmax-value

  (is (= "255"

         (hydra_lib_literals_show_uint8 255))))

;; showUint16

(deftest test-literals-negshowuint16-negzero

  (is (= "0"

         (hydra_lib_literals_show_uint16 0))))

(deftest test-literals-negshowuint16-negtypical-value

  (is (= "1000"

         (hydra_lib_literals_show_uint16 1000))))

;; showUint32

(deftest test-literals-negshowuint32-negzero

  (is (= "0"

         (hydra_lib_literals_show_uint32 0))))

(deftest test-literals-negshowuint32-negtypical-value

  (is (= "100000"

         (hydra_lib_literals_show_uint32 100000))))

;; showUint64

(deftest test-literals-negshowuint64-negzero

  (is (= "0"

         (hydra_lib_literals_show_uint64 0))))

(deftest test-literals-negshowuint64-negtypical-value

  (is (= "1000000"

         (hydra_lib_literals_show_uint64 1000000))))

;; showBigint

(deftest test-literals-negshowbigint-negpositive

  (is (= "42"

         (hydra_lib_literals_show_bigint 42))))

(deftest test-literals-negshowbigint-negnegative

  (is (= "-42"

         (hydra_lib_literals_show_bigint -42))))

(deftest test-literals-negshowbigint-negzero

  (is (= "0"

         (hydra_lib_literals_show_bigint 0))))

;; showFloat32

(deftest test-literals-negshowfloat32-negpositive

  (is (= "3.14"

         (hydra_lib_literals_show_float32 3.140000104904175))))

(deftest test-literals-negshowfloat32-negnegative

  (is (= "-2.5"

         (hydra_lib_literals_show_float32 -2.5))))

(deftest test-literals-negshowfloat32-negzero

  (is (= "0.0"

         (hydra_lib_literals_show_float32 0.0))))

(deftest test-literals-negshowfloat32-negsmall-positive

  (is (= "5.0e-2"

         (hydra_lib_literals_show_float32 5.000000074505806e-2))))

(deftest test-literals-negshowfloat32-negsmall-positive-2

  (is (= "3.0e-2"

         (hydra_lib_literals_show_float32 2.9999999329447746e-2))))

(deftest test-literals-negshowfloat32-negvery-small

  (is (= "1.0e-3"

         (hydra_lib_literals_show_float32 1.0000000474974513e-3))))

(deftest test-literals-negshowfloat32-negnormal-decimal

  (is (= "0.1"

         (hydra_lib_literals_show_float32 0.10000000149011612))))

;; showFloat64

(deftest test-literals-negshowfloat64-negpositive

  (is (= "3.14159"

         (hydra_lib_literals_show_float64 3.14159))))

(deftest test-literals-negshowfloat64-negzero

  (is (= "0.0"

         (hydra_lib_literals_show_float64 0.0))))

(deftest test-literals-negshowfloat64-negsmall-positive

  (is (= "5.0e-2"

         (hydra_lib_literals_show_float64 5.0e-2))))

(deftest test-literals-negshowfloat64-negsmall-positive-2

  (is (= "3.0e-2"

         (hydra_lib_literals_show_float64 3.0e-2))))

(deftest test-literals-negshowfloat64-negvery-small

  (is (= "1.0e-3"

         (hydra_lib_literals_show_float64 1.0e-3))))

(deftest test-literals-negshowfloat64-negnormal-decimal

  (is (= "0.1"

         (hydra_lib_literals_show_float64 0.1))))

;; showBigfloat

(deftest test-literals-negshowbigfloat-negpositive

  (is (= "3.14"

         (hydra_lib_literals_show_bigfloat 3.14))))

(deftest test-literals-negshowbigfloat-negzero

  (is (= "0.0"

         (hydra_lib_literals_show_bigfloat 0.0))))

(deftest test-literals-negshowbigfloat-negsmall-positive

  (is (= "5.0e-2"

         (hydra_lib_literals_show_bigfloat 5.0e-2))))

(deftest test-literals-negshowbigfloat-negsmall-positive-2

  (is (= "3.0e-2"

         (hydra_lib_literals_show_bigfloat 3.0e-2))))

(deftest test-literals-negshowbigfloat-negvery-small

  (is (= "1.0e-3"

         (hydra_lib_literals_show_bigfloat 1.0e-3))))

(deftest test-literals-negshowbigfloat-negnormal-decimal

  (is (= "0.1"

         (hydra_lib_literals_show_bigfloat 0.1))))

;; showBoolean

(deftest test-literals-negshowboolean-negtrue

  (is (= "true"

         (hydra_lib_literals_show_boolean true))))

(deftest test-literals-negshowboolean-negfalse

  (is (= "false"

         (hydra_lib_literals_show_boolean false))))

;; showString

(deftest test-literals-negshowstring-negsimple

  (is (= "\"hello\""

         (hydra_lib_literals_show_string "hello"))))

(deftest test-literals-negshowstring-negempty

  (is (= "\"\""

         (hydra_lib_literals_show_string ""))))

(deftest test-literals-negshowstring-neglatin-accented

  (is (= "\"caf\\233\""

         (hydra_lib_literals_show_string "café"))))

(deftest test-literals-negshowstring-neggreek-lambda

  (is (= "\"\\955\""

         (hydra_lib_literals_show_string "λ"))))

(deftest test-literals-negshowstring-negmixed-ascii-and-non-negascii

  (is (= "\"A\\233B\""

         (hydra_lib_literals_show_string "AéB"))))

(deftest test-literals-negshowstring-negtab

  (is (= "\"\\t\""

         (hydra_lib_literals_show_string "\t"))))

(deftest test-literals-negshowstring-negnewline

  (is (= "\"\\n\""

         (hydra_lib_literals_show_string "\n"))))

(deftest test-literals-negshowstring-negcarriage-return

  (is (= "\"\\r\""

         (hydra_lib_literals_show_string "\r"))))

(deftest test-literals-negshowstring-negbackslash

  (is (= "\"\\\\\""

         (hydra_lib_literals_show_string "\\"))))

(deftest test-literals-negshowstring-negdouble-quote

  (is (= "\"\\\"\""

         (hydra_lib_literals_show_string "\""))))

(deftest test-literals-negshowstring-negnull

  (is (= "\"\\NUL\""

         (hydra_lib_literals_show_string " "))))

(deftest test-literals-negshowstring-negbell

  (is (= "\"\\a\""

         (hydra_lib_literals_show_string ""))))

(deftest test-literals-negshowstring-negbackspace

  (is (= "\"\\b\""

         (hydra_lib_literals_show_string ""))))

(deftest test-literals-negshowstring-negform-feed

  (is (= "\"\\f\""

         (hydra_lib_literals_show_string ""))))

(deftest test-literals-negshowstring-negvertical-tab

  (is (= "\"\\v\""

         (hydra_lib_literals_show_string ""))))

(deftest test-literals-negshowstring-negdelete

  (is (= "\"\\DEL\""

         (hydra_lib_literals_show_string ""))))

;; readInt8

(deftest test-literals-negreadint8-negpositive

  (is (= (list :just 42)

         (hydra_lib_literals_read_int8 "42"))))

(deftest test-literals-negreadint8-negnegative

  (is (= (list :just -42)

         (hydra_lib_literals_read_int8 "-42"))))

(deftest test-literals-negreadint8-negmax-value

  (is (= (list :just 127)

         (hydra_lib_literals_read_int8 "127"))))

(deftest test-literals-negreadint8-negmin-value

  (is (= (list :just -128)

         (hydra_lib_literals_read_int8 "-128"))))

(deftest test-literals-negreadint8-neginvalid

  (is (= (list :nothing)

         (hydra_lib_literals_read_int8 "abc"))))

(deftest test-literals-negreadint8-negoverflow

  (is (= (list :nothing)

         (hydra_lib_literals_read_int8 "128"))))

;; readInt16

(deftest test-literals-negreadint16-negpositive

  (is (= (list :just 1000)

         (hydra_lib_literals_read_int16 "1000"))))

(deftest test-literals-negreadint16-negnegative

  (is (= (list :just -1000)

         (hydra_lib_literals_read_int16 "-1000"))))

(deftest test-literals-negreadint16-neginvalid

  (is (= (list :nothing)

         (hydra_lib_literals_read_int16 "abc"))))

;; readInt32

(deftest test-literals-negreadint32-negpositive

  (is (= (list :just 42)

         (hydra_lib_literals_read_int32 "42"))))

(deftest test-literals-negreadint32-negnegative

  (is (= (list :just -42)

         (hydra_lib_literals_read_int32 "-42"))))

(deftest test-literals-negreadint32-neginvalid

  (is (= (list :nothing)

         (hydra_lib_literals_read_int32 "abc"))))

;; readInt64

(deftest test-literals-negreadint64-negpositive

  (is (= (list :just 1000000)

         (hydra_lib_literals_read_int64 "1000000"))))

(deftest test-literals-negreadint64-negnegative

  (is (= (list :just -1000000)

         (hydra_lib_literals_read_int64 "-1000000"))))

(deftest test-literals-negreadint64-neginvalid

  (is (= (list :nothing)

         (hydra_lib_literals_read_int64 "abc"))))

;; readUint8

(deftest test-literals-negreaduint8-negzero

  (is (= (list :just 0)

         (hydra_lib_literals_read_uint8 "0"))))

(deftest test-literals-negreaduint8-negtypical

  (is (= (list :just 100)

         (hydra_lib_literals_read_uint8 "100"))))

(deftest test-literals-negreaduint8-negmax-value

  (is (= (list :just 255)

         (hydra_lib_literals_read_uint8 "255"))))

(deftest test-literals-negreaduint8-neginvalid

  (is (= (list :nothing)

         (hydra_lib_literals_read_uint8 "abc"))))

(deftest test-literals-negreaduint8-negnegative

  (is (= (list :nothing)

         (hydra_lib_literals_read_uint8 "-1"))))

;; readUint16

(deftest test-literals-negreaduint16-negzero

  (is (= (list :just 0)

         (hydra_lib_literals_read_uint16 "0"))))

(deftest test-literals-negreaduint16-negtypical

  (is (= (list :just 1000)

         (hydra_lib_literals_read_uint16 "1000"))))

(deftest test-literals-negreaduint16-neginvalid

  (is (= (list :nothing)

         (hydra_lib_literals_read_uint16 "abc"))))

(deftest test-literals-negreaduint16-negnegative

  (is (= (list :nothing)

         (hydra_lib_literals_read_uint16 "-1"))))

;; readUint32

(deftest test-literals-negreaduint32-negzero

  (is (= (list :just 0)

         (hydra_lib_literals_read_uint32 "0"))))

(deftest test-literals-negreaduint32-negtypical

  (is (= (list :just 100000)

         (hydra_lib_literals_read_uint32 "100000"))))

(deftest test-literals-negreaduint32-neginvalid

  (is (= (list :nothing)

         (hydra_lib_literals_read_uint32 "abc"))))

(deftest test-literals-negreaduint32-negnegative

  (is (= (list :nothing)

         (hydra_lib_literals_read_uint32 "-1"))))

;; readUint64

(deftest test-literals-negreaduint64-negzero

  (is (= (list :just 0)

         (hydra_lib_literals_read_uint64 "0"))))

(deftest test-literals-negreaduint64-negtypical

  (is (= (list :just 1000000)

         (hydra_lib_literals_read_uint64 "1000000"))))

(deftest test-literals-negreaduint64-neginvalid

  (is (= (list :nothing)

         (hydra_lib_literals_read_uint64 "abc"))))

(deftest test-literals-negreaduint64-negnegative

  (is (= (list :nothing)

         (hydra_lib_literals_read_uint64 "-1"))))

;; readBigint

(deftest test-literals-negreadbigint-negpositive

  (is (= (list :just 42)

         (hydra_lib_literals_read_bigint "42"))))

(deftest test-literals-negreadbigint-negnegative

  (is (= (list :just -42)

         (hydra_lib_literals_read_bigint "-42"))))

(deftest test-literals-negreadbigint-negzero

  (is (= (list :just 0)

         (hydra_lib_literals_read_bigint "0"))))

(deftest test-literals-negreadbigint-neglarge

  (is (= (list :just 123456789012345678901234567890)

         (hydra_lib_literals_read_bigint "123456789012345678901234567890"))))

(deftest test-literals-negreadbigint-neginvalid

  (is (= (list :nothing)

         (hydra_lib_literals_read_bigint "abc"))))

;; readFloat32

(deftest test-literals-negreadfloat32-negpositive

  (is (= (list :just 3.140000104904175)

         (hydra_lib_literals_read_float32 "3.14"))))

(deftest test-literals-negreadfloat32-negnegative

  (is (= (list :just -2.5)

         (hydra_lib_literals_read_float32 "-2.5"))))

(deftest test-literals-negreadfloat32-neginvalid

  (is (= (list :nothing)

         (hydra_lib_literals_read_float32 "abc"))))

;; readFloat64

(deftest test-literals-negreadfloat64-negpositive

  (is (= (list :just 3.14159)

         (hydra_lib_literals_read_float64 "3.14159"))))

(deftest test-literals-negreadfloat64-negnegative

  (is (= (list :just -2.71828)

         (hydra_lib_literals_read_float64 "-2.71828"))))

(deftest test-literals-negreadfloat64-neginvalid

  (is (= (list :nothing)

         (hydra_lib_literals_read_float64 "abc"))))

;; readBigfloat

(deftest test-literals-negreadbigfloat-negpositive

  (is (= (list :just 3.14)

         (hydra_lib_literals_read_bigfloat "3.14"))))

(deftest test-literals-negreadbigfloat-neginvalid

  (is (= (list :nothing)

         (hydra_lib_literals_read_bigfloat "abc"))))

;; readBoolean

(deftest test-literals-negreadboolean-negtrue

  (is (= (list :just true)

         (hydra_lib_literals_read_boolean "true"))))

(deftest test-literals-negreadboolean-negfalse

  (is (= (list :just false)

         (hydra_lib_literals_read_boolean "false"))))

(deftest test-literals-negreadboolean-neginvalid

  (is (= (list :nothing)

         (hydra_lib_literals_read_boolean "yes"))))

;; readString

(deftest test-literals-negreadstring-negquoted-string

  (is (= (list :just "hello")

         (hydra_lib_literals_read_string "\"hello\""))))

(deftest test-literals-negreadstring-negempty-quoted

  (is (= (list :just "")

         (hydra_lib_literals_read_string "\"\""))))

(deftest test-literals-negreadstring-negunquoted

  (is (= (list :nothing)

         (hydra_lib_literals_read_string "hello"))))

;; stringToBinary

(deftest test-literals-negstringtobinary-negsimple-base64

  (is (= (list 104 101 108 108 111)

         (hydra_lib_literals_string_to_binary "aGVsbG8="))))

(deftest test-literals-negstringtobinary-negempty-string

  (is (= (list)

         (hydra_lib_literals_string_to_binary ""))))

;; binaryToString

(deftest test-literals-negbinarytostring-negsimple-binary

  (is (= "aGVsbG8="

         (hydra_lib_literals_binary_to_string (list 104 101 108 108 111)))))

(deftest test-literals-negbinarytostring-negempty-binary

  (is (= ""

         (hydra_lib_literals_binary_to_string (list)))))
