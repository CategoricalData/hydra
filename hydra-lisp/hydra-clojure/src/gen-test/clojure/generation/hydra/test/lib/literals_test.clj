;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.literals primitives

(ns generation.hydra.test.lib.literals-test
  (:require [clojure.test :refer :all]))

;; bigintToInt8

(deftest test-biginttoint8-negpositive

  (is (= 42

         (hydra_lib_literals_bigint_to_int8 42))))

(deftest test-biginttoint8-negnegative

  (is (= -42

         (hydra_lib_literals_bigint_to_int8 -42))))

;; bigintToInt16

(deftest test-biginttoint16-negpositive

  (is (= 1000

         (hydra_lib_literals_bigint_to_int16 1000))))

(deftest test-biginttoint16-negnegative

  (is (= -1000

         (hydra_lib_literals_bigint_to_int16 -1000))))

;; bigintToInt32

(deftest test-biginttoint32-negpositive

  (is (= 42

         (hydra_lib_literals_bigint_to_int32 42))))

(deftest test-biginttoint32-negnegative

  (is (= -42

         (hydra_lib_literals_bigint_to_int32 -42))))

(deftest test-biginttoint32-negzero

  (is (= 0

         (hydra_lib_literals_bigint_to_int32 0))))

;; bigintToInt64

(deftest test-biginttoint64-negpositive

  (is (= 1000000

         (hydra_lib_literals_bigint_to_int64 1000000))))

(deftest test-biginttoint64-negnegative

  (is (= -1000000

         (hydra_lib_literals_bigint_to_int64 -1000000))))

;; bigintToUint8

(deftest test-biginttouint8-negzero

  (is (= 0

         (hydra_lib_literals_bigint_to_uint8 0))))

(deftest test-biginttouint8-negtypical-value

  (is (= 100

         (hydra_lib_literals_bigint_to_uint8 100))))

;; bigintToUint16

(deftest test-biginttouint16-negzero

  (is (= 0

         (hydra_lib_literals_bigint_to_uint16 0))))

(deftest test-biginttouint16-negtypical-value

  (is (= 1000

         (hydra_lib_literals_bigint_to_uint16 1000))))

;; bigintToUint32

(deftest test-biginttouint32-negzero

  (is (= 0

         (hydra_lib_literals_bigint_to_uint32 0))))

(deftest test-biginttouint32-negtypical-value

  (is (= 100000

         (hydra_lib_literals_bigint_to_uint32 100000))))

;; bigintToUint64

(deftest test-biginttouint64-negzero

  (is (= 0

         (hydra_lib_literals_bigint_to_uint64 0))))

(deftest test-biginttouint64-negtypical-value

  (is (= 1000000

         (hydra_lib_literals_bigint_to_uint64 1000000))))

;; int8ToBigint

(deftest test-int8tobigint-negpositive

  (is (= 42

         (hydra_lib_literals_int8_to_bigint 42))))

(deftest test-int8tobigint-negnegative

  (is (= -42

         (hydra_lib_literals_int8_to_bigint -42))))

(deftest test-int8tobigint-negmax-value

  (is (= 127

         (hydra_lib_literals_int8_to_bigint 127))))

(deftest test-int8tobigint-negmin-value

  (is (= -128

         (hydra_lib_literals_int8_to_bigint -128))))

;; int16ToBigint

(deftest test-int16tobigint-negpositive

  (is (= 1000

         (hydra_lib_literals_int16_to_bigint 1000))))

(deftest test-int16tobigint-negnegative

  (is (= -1000

         (hydra_lib_literals_int16_to_bigint -1000))))

;; int32ToBigint

(deftest test-int32tobigint-negpositive

  (is (= 42

         (hydra_lib_literals_int32_to_bigint 42))))

(deftest test-int32tobigint-negnegative

  (is (= -42

         (hydra_lib_literals_int32_to_bigint -42))))

(deftest test-int32tobigint-negzero

  (is (= 0

         (hydra_lib_literals_int32_to_bigint 0))))

;; int64ToBigint

(deftest test-int64tobigint-negpositive

  (is (= 1000000

         (hydra_lib_literals_int64_to_bigint 1000000))))

(deftest test-int64tobigint-negnegative

  (is (= -1000000

         (hydra_lib_literals_int64_to_bigint -1000000))))

;; uint8ToBigint

(deftest test-uint8tobigint-negzero

  (is (= 0

         (hydra_lib_literals_uint8_to_bigint 0))))

(deftest test-uint8tobigint-negmax-value

  (is (= 255

         (hydra_lib_literals_uint8_to_bigint 255))))

;; uint16ToBigint

(deftest test-uint16tobigint-negzero

  (is (= 0

         (hydra_lib_literals_uint16_to_bigint 0))))

(deftest test-uint16tobigint-negtypical-value

  (is (= 1000

         (hydra_lib_literals_uint16_to_bigint 1000))))

;; uint32ToBigint

(deftest test-uint32tobigint-negzero

  (is (= 0

         (hydra_lib_literals_uint32_to_bigint 0))))

(deftest test-uint32tobigint-negtypical-value

  (is (= 100000

         (hydra_lib_literals_uint32_to_bigint 100000))))

;; uint64ToBigint

(deftest test-uint64tobigint-negzero

  (is (= 0

         (hydra_lib_literals_uint64_to_bigint 0))))

(deftest test-uint64tobigint-negtypical-value

  (is (= 1000000

         (hydra_lib_literals_uint64_to_bigint 1000000))))

;; float32ToBigfloat

(deftest test-float32tobigfloat-negpositive

  (is (= 2.5

         (hydra_lib_literals_float32_to_bigfloat 2.5))))

(deftest test-float32tobigfloat-negnegative

  (is (= -2.5

         (hydra_lib_literals_float32_to_bigfloat -2.5))))

(deftest test-float32tobigfloat-negzero

  (is (= 0.0

         (hydra_lib_literals_float32_to_bigfloat 0.0))))

;; float64ToBigfloat

(deftest test-float64tobigfloat-negpositive

  (is (= 3.14159

         (hydra_lib_literals_float64_to_bigfloat 3.14159))))

(deftest test-float64tobigfloat-negnegative

  (is (= -2.71828

         (hydra_lib_literals_float64_to_bigfloat -2.71828))))

(deftest test-float64tobigfloat-negzero

  (is (= 0.0

         (hydra_lib_literals_float64_to_bigfloat 0.0))))

;; bigfloatToFloat32

(deftest test-bigfloattofloat32-negpositive

  (is (= 3.140000104904175

         (hydra_lib_literals_bigfloat_to_float32 3.14))))

(deftest test-bigfloattofloat32-negnegative

  (is (= -2.5

         (hydra_lib_literals_bigfloat_to_float32 -2.5))))

(deftest test-bigfloattofloat32-negzero

  (is (= 0.0

         (hydra_lib_literals_bigfloat_to_float32 0.0))))

;; bigfloatToFloat64

(deftest test-bigfloattofloat64-negpositive

  (is (= 3.14159

         (hydra_lib_literals_bigfloat_to_float64 3.14159))))

(deftest test-bigfloattofloat64-negnegative

  (is (= -2.71828

         (hydra_lib_literals_bigfloat_to_float64 -2.71828))))

(deftest test-bigfloattofloat64-negzero

  (is (= 0.0

         (hydra_lib_literals_bigfloat_to_float64 0.0))))

;; bigintToBigfloat

(deftest test-biginttobigfloat-negpositive

  (is (= 42.0

         (hydra_lib_literals_bigint_to_bigfloat 42))))

(deftest test-biginttobigfloat-negnegative

  (is (= -42.0

         (hydra_lib_literals_bigint_to_bigfloat -42))))

(deftest test-biginttobigfloat-negzero

  (is (= 0.0

         (hydra_lib_literals_bigint_to_bigfloat 0))))

;; bigfloatToBigint

(deftest test-bigfloattobigint-negpositive

  (is (= 43

         (hydra_lib_literals_bigfloat_to_bigint 42.7))))

(deftest test-bigfloattobigint-negnegative

  (is (= -43

         (hydra_lib_literals_bigfloat_to_bigint -42.7))))

(deftest test-bigfloattobigint-negzero

  (is (= 0

         (hydra_lib_literals_bigfloat_to_bigint 0.0))))

(deftest test-bigfloattobigint-neground-down

  (is (= 42

         (hydra_lib_literals_bigfloat_to_bigint 42.3))))

(deftest test-bigfloattobigint-neghalf-even-up

  (is (= 42

         (hydra_lib_literals_bigfloat_to_bigint 42.5))))

(deftest test-bigfloattobigint-neghalf-even-down

  (is (= 44

         (hydra_lib_literals_bigfloat_to_bigint 43.5))))

;; showInt8

(deftest test-showint8-negpositive

  (is (= "42"

         (hydra_lib_literals_show_int8 42))))

(deftest test-showint8-negnegative

  (is (= "-42"

         (hydra_lib_literals_show_int8 -42))))

;; showInt16

(deftest test-showint16-negpositive

  (is (= "1000"

         (hydra_lib_literals_show_int16 1000))))

(deftest test-showint16-negnegative

  (is (= "-1000"

         (hydra_lib_literals_show_int16 -1000))))

;; showInt32

(deftest test-showint32-negpositive

  (is (= "42"

         (hydra_lib_literals_show_int32 42))))

(deftest test-showint32-negnegative

  (is (= "-42"

         (hydra_lib_literals_show_int32 -42))))

(deftest test-showint32-negzero

  (is (= "0"

         (hydra_lib_literals_show_int32 0))))

;; showInt64

(deftest test-showint64-negpositive

  (is (= "1000000"

         (hydra_lib_literals_show_int64 1000000))))

(deftest test-showint64-negnegative

  (is (= "-1000000"

         (hydra_lib_literals_show_int64 -1000000))))

;; showUint8

(deftest test-showuint8-negzero

  (is (= "0"

         (hydra_lib_literals_show_uint8 0))))

(deftest test-showuint8-negmax-value

  (is (= "255"

         (hydra_lib_literals_show_uint8 255))))

;; showUint16

(deftest test-showuint16-negzero

  (is (= "0"

         (hydra_lib_literals_show_uint16 0))))

(deftest test-showuint16-negtypical-value

  (is (= "1000"

         (hydra_lib_literals_show_uint16 1000))))

;; showUint32

(deftest test-showuint32-negzero

  (is (= "0"

         (hydra_lib_literals_show_uint32 0))))

(deftest test-showuint32-negtypical-value

  (is (= "100000"

         (hydra_lib_literals_show_uint32 100000))))

;; showUint64

(deftest test-showuint64-negzero

  (is (= "0"

         (hydra_lib_literals_show_uint64 0))))

(deftest test-showuint64-negtypical-value

  (is (= "1000000"

         (hydra_lib_literals_show_uint64 1000000))))

;; showBigint

(deftest test-showbigint-negpositive

  (is (= "42"

         (hydra_lib_literals_show_bigint 42))))

(deftest test-showbigint-negnegative

  (is (= "-42"

         (hydra_lib_literals_show_bigint -42))))

(deftest test-showbigint-negzero

  (is (= "0"

         (hydra_lib_literals_show_bigint 0))))

;; showFloat32

(deftest test-showfloat32-negpositive

  (is (= "3.14"

         (hydra_lib_literals_show_float32 3.140000104904175))))

(deftest test-showfloat32-negnegative

  (is (= "-2.5"

         (hydra_lib_literals_show_float32 -2.5))))

(deftest test-showfloat32-negzero

  (is (= "0.0"

         (hydra_lib_literals_show_float32 0.0))))

(deftest test-showfloat32-negsmall-positive

  (is (= "5.0e-2"

         (hydra_lib_literals_show_float32 5.000000074505806e-2))))

(deftest test-showfloat32-negsmall-positive-2

  (is (= "3.0e-2"

         (hydra_lib_literals_show_float32 2.9999999329447746e-2))))

(deftest test-showfloat32-negvery-small

  (is (= "1.0e-3"

         (hydra_lib_literals_show_float32 1.0000000474974513e-3))))

(deftest test-showfloat32-negnormal-decimal

  (is (= "0.1"

         (hydra_lib_literals_show_float32 0.10000000149011612))))

;; showFloat64

(deftest test-showfloat64-negpositive

  (is (= "3.14159"

         (hydra_lib_literals_show_float64 3.14159))))

(deftest test-showfloat64-negzero

  (is (= "0.0"

         (hydra_lib_literals_show_float64 0.0))))

(deftest test-showfloat64-negsmall-positive

  (is (= "5.0e-2"

         (hydra_lib_literals_show_float64 5.0e-2))))

(deftest test-showfloat64-negsmall-positive-2

  (is (= "3.0e-2"

         (hydra_lib_literals_show_float64 3.0e-2))))

(deftest test-showfloat64-negvery-small

  (is (= "1.0e-3"

         (hydra_lib_literals_show_float64 1.0e-3))))

(deftest test-showfloat64-negnormal-decimal

  (is (= "0.1"

         (hydra_lib_literals_show_float64 0.1))))

;; showBigfloat

(deftest test-showbigfloat-negpositive

  (is (= "3.14"

         (hydra_lib_literals_show_bigfloat 3.14))))

(deftest test-showbigfloat-negzero

  (is (= "0.0"

         (hydra_lib_literals_show_bigfloat 0.0))))

(deftest test-showbigfloat-negsmall-positive

  (is (= "5.0e-2"

         (hydra_lib_literals_show_bigfloat 5.0e-2))))

(deftest test-showbigfloat-negsmall-positive-2

  (is (= "3.0e-2"

         (hydra_lib_literals_show_bigfloat 3.0e-2))))

(deftest test-showbigfloat-negvery-small

  (is (= "1.0e-3"

         (hydra_lib_literals_show_bigfloat 1.0e-3))))

(deftest test-showbigfloat-negnormal-decimal

  (is (= "0.1"

         (hydra_lib_literals_show_bigfloat 0.1))))

;; showBoolean

(deftest test-showboolean-negtrue

  (is (= "true"

         (hydra_lib_literals_show_boolean true))))

(deftest test-showboolean-negfalse

  (is (= "false"

         (hydra_lib_literals_show_boolean false))))

;; showString

(deftest test-showstring-negsimple

  (is (= "\"hello\""

         (hydra_lib_literals_show_string "hello"))))

(deftest test-showstring-negempty

  (is (= "\"\""

         (hydra_lib_literals_show_string ""))))

(deftest test-showstring-neglatin-accented

  (is (= "\"caf\\233\""

         (hydra_lib_literals_show_string "café"))))

(deftest test-showstring-neggreek-lambda

  (is (= "\"\\955\""

         (hydra_lib_literals_show_string "λ"))))

(deftest test-showstring-negmixed-ascii-and-non-negascii

  (is (= "\"A\\233B\""

         (hydra_lib_literals_show_string "AéB"))))

(deftest test-showstring-negtab

  (is (= "\"\\t\""

         (hydra_lib_literals_show_string "\t"))))

(deftest test-showstring-negnewline

  (is (= "\"\\n\""

         (hydra_lib_literals_show_string "\n"))))

(deftest test-showstring-negcarriage-return

  (is (= "\"\\r\""

         (hydra_lib_literals_show_string "\r"))))

(deftest test-showstring-negbackslash

  (is (= "\"\\\\\""

         (hydra_lib_literals_show_string "\\"))))

(deftest test-showstring-negdouble-quote

  (is (= "\"\\\"\""

         (hydra_lib_literals_show_string "\""))))

(deftest test-showstring-negnull

  (is (= "\"\\NUL\""

         (hydra_lib_literals_show_string " "))))

(deftest test-showstring-negbell

  (is (= "\"\\a\""

         (hydra_lib_literals_show_string ""))))

(deftest test-showstring-negbackspace

  (is (= "\"\\b\""

         (hydra_lib_literals_show_string ""))))

(deftest test-showstring-negform-feed

  (is (= "\"\\f\""

         (hydra_lib_literals_show_string ""))))

(deftest test-showstring-negvertical-tab

  (is (= "\"\\v\""

         (hydra_lib_literals_show_string ""))))

(deftest test-showstring-negdelete

  (is (= "\"\\DEL\""

         (hydra_lib_literals_show_string ""))))

;; readInt8

(deftest test-readint8-negpositive

  (is (= 42

         (hydra_lib_literals_read_int8 "42"))))

(deftest test-readint8-negnegative

  (is (= -42

         (hydra_lib_literals_read_int8 "-42"))))

(deftest test-readint8-negmax-value

  (is (= 127

         (hydra_lib_literals_read_int8 "127"))))

(deftest test-readint8-negmin-value

  (is (= -128

         (hydra_lib_literals_read_int8 "-128"))))

(deftest test-readint8-neginvalid

  (is (= nil

         (hydra_lib_literals_read_int8 "abc"))))

(deftest test-readint8-negoverflow

  (is (= nil

         (hydra_lib_literals_read_int8 "128"))))

;; readInt16

(deftest test-readint16-negpositive

  (is (= 1000

         (hydra_lib_literals_read_int16 "1000"))))

(deftest test-readint16-negnegative

  (is (= -1000

         (hydra_lib_literals_read_int16 "-1000"))))

(deftest test-readint16-neginvalid

  (is (= nil

         (hydra_lib_literals_read_int16 "abc"))))

;; readInt32

(deftest test-readint32-negpositive

  (is (= 42

         (hydra_lib_literals_read_int32 "42"))))

(deftest test-readint32-negnegative

  (is (= -42

         (hydra_lib_literals_read_int32 "-42"))))

(deftest test-readint32-neginvalid

  (is (= nil

         (hydra_lib_literals_read_int32 "abc"))))

;; readInt64

(deftest test-readint64-negpositive

  (is (= 1000000

         (hydra_lib_literals_read_int64 "1000000"))))

(deftest test-readint64-negnegative

  (is (= -1000000

         (hydra_lib_literals_read_int64 "-1000000"))))

(deftest test-readint64-neginvalid

  (is (= nil

         (hydra_lib_literals_read_int64 "abc"))))

;; readUint8

(deftest test-readuint8-negzero

  (is (= 0

         (hydra_lib_literals_read_uint8 "0"))))

(deftest test-readuint8-negtypical

  (is (= 100

         (hydra_lib_literals_read_uint8 "100"))))

(deftest test-readuint8-negmax-value

  (is (= 255

         (hydra_lib_literals_read_uint8 "255"))))

(deftest test-readuint8-neginvalid

  (is (= nil

         (hydra_lib_literals_read_uint8 "abc"))))

(deftest test-readuint8-negnegative

  (is (= nil

         (hydra_lib_literals_read_uint8 "-1"))))

;; readUint16

(deftest test-readuint16-negzero

  (is (= 0

         (hydra_lib_literals_read_uint16 "0"))))

(deftest test-readuint16-negtypical

  (is (= 1000

         (hydra_lib_literals_read_uint16 "1000"))))

(deftest test-readuint16-neginvalid

  (is (= nil

         (hydra_lib_literals_read_uint16 "abc"))))

(deftest test-readuint16-negnegative

  (is (= nil

         (hydra_lib_literals_read_uint16 "-1"))))

;; readUint32

(deftest test-readuint32-negzero

  (is (= 0

         (hydra_lib_literals_read_uint32 "0"))))

(deftest test-readuint32-negtypical

  (is (= 100000

         (hydra_lib_literals_read_uint32 "100000"))))

(deftest test-readuint32-neginvalid

  (is (= nil

         (hydra_lib_literals_read_uint32 "abc"))))

(deftest test-readuint32-negnegative

  (is (= nil

         (hydra_lib_literals_read_uint32 "-1"))))

;; readUint64

(deftest test-readuint64-negzero

  (is (= 0

         (hydra_lib_literals_read_uint64 "0"))))

(deftest test-readuint64-negtypical

  (is (= 1000000

         (hydra_lib_literals_read_uint64 "1000000"))))

(deftest test-readuint64-neginvalid

  (is (= nil

         (hydra_lib_literals_read_uint64 "abc"))))

(deftest test-readuint64-negnegative

  (is (= nil

         (hydra_lib_literals_read_uint64 "-1"))))

;; readBigint

(deftest test-readbigint-negpositive

  (is (= 42

         (hydra_lib_literals_read_bigint "42"))))

(deftest test-readbigint-negnegative

  (is (= -42

         (hydra_lib_literals_read_bigint "-42"))))

(deftest test-readbigint-negzero

  (is (= 0

         (hydra_lib_literals_read_bigint "0"))))

(deftest test-readbigint-neglarge

  (is (= 123456789012345678901234567890

         (hydra_lib_literals_read_bigint "123456789012345678901234567890"))))

(deftest test-readbigint-neginvalid

  (is (= nil

         (hydra_lib_literals_read_bigint "abc"))))

;; readFloat32

(deftest test-readfloat32-negpositive

  (is (= 3.140000104904175

         (hydra_lib_literals_read_float32 "3.14"))))

(deftest test-readfloat32-negnegative

  (is (= -2.5

         (hydra_lib_literals_read_float32 "-2.5"))))

(deftest test-readfloat32-neginvalid

  (is (= nil

         (hydra_lib_literals_read_float32 "abc"))))

;; readFloat64

(deftest test-readfloat64-negpositive

  (is (= 3.14159

         (hydra_lib_literals_read_float64 "3.14159"))))

(deftest test-readfloat64-negnegative

  (is (= -2.71828

         (hydra_lib_literals_read_float64 "-2.71828"))))

(deftest test-readfloat64-neginvalid

  (is (= nil

         (hydra_lib_literals_read_float64 "abc"))))

;; readBigfloat

(deftest test-readbigfloat-negpositive

  (is (= 3.14

         (hydra_lib_literals_read_bigfloat "3.14"))))

(deftest test-readbigfloat-neginvalid

  (is (= nil

         (hydra_lib_literals_read_bigfloat "abc"))))

;; readBoolean

(deftest test-readboolean-negtrue

  (is (= true

         (hydra_lib_literals_read_boolean "true"))))

(deftest test-readboolean-negfalse

  (is (= false

         (hydra_lib_literals_read_boolean "false"))))

(deftest test-readboolean-neginvalid

  (is (= nil

         (hydra_lib_literals_read_boolean "yes"))))

;; readString

(deftest test-readstring-negquoted-string

  (is (= "hello"

         (hydra_lib_literals_read_string "\"hello\""))))

(deftest test-readstring-negempty-quoted

  (is (= ""

         (hydra_lib_literals_read_string "\"\""))))

(deftest test-readstring-negunquoted

  (is (= nil

         (hydra_lib_literals_read_string "hello"))))

;; stringToBinary

(deftest test-stringtobinary-negsimple-base64

  (is (= [104 101 108 108 111]

         (hydra_lib_literals_string_to_binary "aGVsbG8="))))

(deftest test-stringtobinary-negempty-string

  (is (= []

         (hydra_lib_literals_string_to_binary ""))))

;; binaryToString

(deftest test-binarytostring-negsimple-binary

  (is (= "aGVsbG8="

         (hydra_lib_literals_binary_to_string [104 101 108 108 111]))))

(deftest test-binarytostring-negempty-binary

  (is (= ""

         (hydra_lib_literals_binary_to_string []))))
