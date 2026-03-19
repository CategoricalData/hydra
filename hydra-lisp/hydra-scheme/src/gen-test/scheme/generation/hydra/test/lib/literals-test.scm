;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.literals primitives

(import (scheme base))

;; bigintToInt8

(define (test-literals-negbiginttoint8-negpositive)

  (assert (equal? 42 (hydra_lib_literals_bigint_to_int8 42))))

(define (test-literals-negbiginttoint8-negnegative)

  (assert (equal? -42 (hydra_lib_literals_bigint_to_int8 -42))))

;; bigintToInt16

(define (test-literals-negbiginttoint16-negpositive)

  (assert (equal? 1000 (hydra_lib_literals_bigint_to_int16 1000))))

(define (test-literals-negbiginttoint16-negnegative)

  (assert (equal? -1000 (hydra_lib_literals_bigint_to_int16 -1000))))

;; bigintToInt32

(define (test-literals-negbiginttoint32-negpositive)

  (assert (equal? 42 (hydra_lib_literals_bigint_to_int32 42))))

(define (test-literals-negbiginttoint32-negnegative)

  (assert (equal? -42 (hydra_lib_literals_bigint_to_int32 -42))))

(define (test-literals-negbiginttoint32-negzero)

  (assert (equal? 0 (hydra_lib_literals_bigint_to_int32 0))))

;; bigintToInt64

(define (test-literals-negbiginttoint64-negpositive)

  (assert (equal? 1000000 (hydra_lib_literals_bigint_to_int64 1000000))))

(define (test-literals-negbiginttoint64-negnegative)

  (assert (equal? -1000000 (hydra_lib_literals_bigint_to_int64 -1000000))))

;; bigintToUint8

(define (test-literals-negbiginttouint8-negzero)

  (assert (equal? 0 (hydra_lib_literals_bigint_to_uint8 0))))

(define (test-literals-negbiginttouint8-negtypical-value)

  (assert (equal? 100 (hydra_lib_literals_bigint_to_uint8 100))))

;; bigintToUint16

(define (test-literals-negbiginttouint16-negzero)

  (assert (equal? 0 (hydra_lib_literals_bigint_to_uint16 0))))

(define (test-literals-negbiginttouint16-negtypical-value)

  (assert (equal? 1000 (hydra_lib_literals_bigint_to_uint16 1000))))

;; bigintToUint32

(define (test-literals-negbiginttouint32-negzero)

  (assert (equal? 0 (hydra_lib_literals_bigint_to_uint32 0))))

(define (test-literals-negbiginttouint32-negtypical-value)

  (assert (equal? 100000 (hydra_lib_literals_bigint_to_uint32 100000))))

;; bigintToUint64

(define (test-literals-negbiginttouint64-negzero)

  (assert (equal? 0 (hydra_lib_literals_bigint_to_uint64 0))))

(define (test-literals-negbiginttouint64-negtypical-value)

  (assert (equal? 1000000 (hydra_lib_literals_bigint_to_uint64 1000000))))

;; int8ToBigint

(define (test-literals-negint8tobigint-negpositive)

  (assert (equal? 42 (hydra_lib_literals_int8_to_bigint 42))))

(define (test-literals-negint8tobigint-negnegative)

  (assert (equal? -42 (hydra_lib_literals_int8_to_bigint -42))))

(define (test-literals-negint8tobigint-negmax-value)

  (assert (equal? 127 (hydra_lib_literals_int8_to_bigint 127))))

(define (test-literals-negint8tobigint-negmin-value)

  (assert (equal? -128 (hydra_lib_literals_int8_to_bigint -128))))

;; int16ToBigint

(define (test-literals-negint16tobigint-negpositive)

  (assert (equal? 1000 (hydra_lib_literals_int16_to_bigint 1000))))

(define (test-literals-negint16tobigint-negnegative)

  (assert (equal? -1000 (hydra_lib_literals_int16_to_bigint -1000))))

;; int32ToBigint

(define (test-literals-negint32tobigint-negpositive)

  (assert (equal? 42 (hydra_lib_literals_int32_to_bigint 42))))

(define (test-literals-negint32tobigint-negnegative)

  (assert (equal? -42 (hydra_lib_literals_int32_to_bigint -42))))

(define (test-literals-negint32tobigint-negzero)

  (assert (equal? 0 (hydra_lib_literals_int32_to_bigint 0))))

;; int64ToBigint

(define (test-literals-negint64tobigint-negpositive)

  (assert (equal? 1000000 (hydra_lib_literals_int64_to_bigint 1000000))))

(define (test-literals-negint64tobigint-negnegative)

  (assert (equal? -1000000 (hydra_lib_literals_int64_to_bigint -1000000))))

;; uint8ToBigint

(define (test-literals-neguint8tobigint-negzero)

  (assert (equal? 0 (hydra_lib_literals_uint8_to_bigint 0))))

(define (test-literals-neguint8tobigint-negmax-value)

  (assert (equal? 255 (hydra_lib_literals_uint8_to_bigint 255))))

;; uint16ToBigint

(define (test-literals-neguint16tobigint-negzero)

  (assert (equal? 0 (hydra_lib_literals_uint16_to_bigint 0))))

(define (test-literals-neguint16tobigint-negtypical-value)

  (assert (equal? 1000 (hydra_lib_literals_uint16_to_bigint 1000))))

;; uint32ToBigint

(define (test-literals-neguint32tobigint-negzero)

  (assert (equal? 0 (hydra_lib_literals_uint32_to_bigint 0))))

(define (test-literals-neguint32tobigint-negtypical-value)

  (assert (equal? 100000 (hydra_lib_literals_uint32_to_bigint 100000))))

;; uint64ToBigint

(define (test-literals-neguint64tobigint-negzero)

  (assert (equal? 0 (hydra_lib_literals_uint64_to_bigint 0))))

(define (test-literals-neguint64tobigint-negtypical-value)

  (assert (equal? 1000000 (hydra_lib_literals_uint64_to_bigint 1000000))))

;; float32ToBigfloat

(define (test-literals-negfloat32tobigfloat-negpositive)

  (assert (equal? 2.5 (hydra_lib_literals_float32_to_bigfloat 2.5))))

(define (test-literals-negfloat32tobigfloat-negnegative)

  (assert (equal? -2.5 (hydra_lib_literals_float32_to_bigfloat -2.5))))

(define (test-literals-negfloat32tobigfloat-negzero)

  (assert (equal? 0.0 (hydra_lib_literals_float32_to_bigfloat 0.0))))

;; float64ToBigfloat

(define (test-literals-negfloat64tobigfloat-negpositive)

  (assert (equal? 3.14159 (hydra_lib_literals_float64_to_bigfloat 3.14159))))

(define (test-literals-negfloat64tobigfloat-negnegative)

  (assert (equal? -2.71828 (hydra_lib_literals_float64_to_bigfloat -2.71828))))

(define (test-literals-negfloat64tobigfloat-negzero)

  (assert (equal? 0.0 (hydra_lib_literals_float64_to_bigfloat 0.0))))

;; bigfloatToFloat32

(define (test-literals-negbigfloattofloat32-negpositive)

  (assert (equal? 3.140000104904175 (hydra_lib_literals_bigfloat_to_float32 3.14))))

(define (test-literals-negbigfloattofloat32-negnegative)

  (assert (equal? -2.5 (hydra_lib_literals_bigfloat_to_float32 -2.5))))

(define (test-literals-negbigfloattofloat32-negzero)

  (assert (equal? 0.0 (hydra_lib_literals_bigfloat_to_float32 0.0))))

;; bigfloatToFloat64

(define (test-literals-negbigfloattofloat64-negpositive)

  (assert (equal? 3.14159 (hydra_lib_literals_bigfloat_to_float64 3.14159))))

(define (test-literals-negbigfloattofloat64-negnegative)

  (assert (equal? -2.71828 (hydra_lib_literals_bigfloat_to_float64 -2.71828))))

(define (test-literals-negbigfloattofloat64-negzero)

  (assert (equal? 0.0 (hydra_lib_literals_bigfloat_to_float64 0.0))))

;; bigintToBigfloat

(define (test-literals-negbiginttobigfloat-negpositive)

  (assert (equal? 42.0 (hydra_lib_literals_bigint_to_bigfloat 42))))

(define (test-literals-negbiginttobigfloat-negnegative)

  (assert (equal? -42.0 (hydra_lib_literals_bigint_to_bigfloat -42))))

(define (test-literals-negbiginttobigfloat-negzero)

  (assert (equal? 0.0 (hydra_lib_literals_bigint_to_bigfloat 0))))

;; bigfloatToBigint

(define (test-literals-negbigfloattobigint-negpositive)

  (assert (equal? 43 (hydra_lib_literals_bigfloat_to_bigint 42.7))))

(define (test-literals-negbigfloattobigint-negnegative)

  (assert (equal? -43 (hydra_lib_literals_bigfloat_to_bigint -42.7))))

(define (test-literals-negbigfloattobigint-negzero)

  (assert (equal? 0 (hydra_lib_literals_bigfloat_to_bigint 0.0))))

(define (test-literals-negbigfloattobigint-neground-down)

  (assert (equal? 42 (hydra_lib_literals_bigfloat_to_bigint 42.3))))

(define (test-literals-negbigfloattobigint-neghalf-even-up)

  (assert (equal? 42 (hydra_lib_literals_bigfloat_to_bigint 42.5))))

(define (test-literals-negbigfloattobigint-neghalf-even-down)

  (assert (equal? 44 (hydra_lib_literals_bigfloat_to_bigint 43.5))))

;; showInt8

(define (test-literals-negshowint8-negpositive)

  (assert (equal? "42" (hydra_lib_literals_show_int8 42))))

(define (test-literals-negshowint8-negnegative)

  (assert (equal? "-42" (hydra_lib_literals_show_int8 -42))))

;; showInt16

(define (test-literals-negshowint16-negpositive)

  (assert (equal? "1000" (hydra_lib_literals_show_int16 1000))))

(define (test-literals-negshowint16-negnegative)

  (assert (equal? "-1000" (hydra_lib_literals_show_int16 -1000))))

;; showInt32

(define (test-literals-negshowint32-negpositive)

  (assert (equal? "42" (hydra_lib_literals_show_int32 42))))

(define (test-literals-negshowint32-negnegative)

  (assert (equal? "-42" (hydra_lib_literals_show_int32 -42))))

(define (test-literals-negshowint32-negzero)

  (assert (equal? "0" (hydra_lib_literals_show_int32 0))))

;; showInt64

(define (test-literals-negshowint64-negpositive)

  (assert (equal? "1000000" (hydra_lib_literals_show_int64 1000000))))

(define (test-literals-negshowint64-negnegative)

  (assert (equal? "-1000000" (hydra_lib_literals_show_int64 -1000000))))

;; showUint8

(define (test-literals-negshowuint8-negzero)

  (assert (equal? "0" (hydra_lib_literals_show_uint8 0))))

(define (test-literals-negshowuint8-negmax-value)

  (assert (equal? "255" (hydra_lib_literals_show_uint8 255))))

;; showUint16

(define (test-literals-negshowuint16-negzero)

  (assert (equal? "0" (hydra_lib_literals_show_uint16 0))))

(define (test-literals-negshowuint16-negtypical-value)

  (assert (equal? "1000" (hydra_lib_literals_show_uint16 1000))))

;; showUint32

(define (test-literals-negshowuint32-negzero)

  (assert (equal? "0" (hydra_lib_literals_show_uint32 0))))

(define (test-literals-negshowuint32-negtypical-value)

  (assert (equal? "100000" (hydra_lib_literals_show_uint32 100000))))

;; showUint64

(define (test-literals-negshowuint64-negzero)

  (assert (equal? "0" (hydra_lib_literals_show_uint64 0))))

(define (test-literals-negshowuint64-negtypical-value)

  (assert (equal? "1000000" (hydra_lib_literals_show_uint64 1000000))))

;; showBigint

(define (test-literals-negshowbigint-negpositive)

  (assert (equal? "42" (hydra_lib_literals_show_bigint 42))))

(define (test-literals-negshowbigint-negnegative)

  (assert (equal? "-42" (hydra_lib_literals_show_bigint -42))))

(define (test-literals-negshowbigint-negzero)

  (assert (equal? "0" (hydra_lib_literals_show_bigint 0))))

;; showFloat32

(define (test-literals-negshowfloat32-negpositive)

  (assert (equal? "3.14" (hydra_lib_literals_show_float32 3.140000104904175))))

(define (test-literals-negshowfloat32-negnegative)

  (assert (equal? "-2.5" (hydra_lib_literals_show_float32 -2.5))))

(define (test-literals-negshowfloat32-negzero)

  (assert (equal? "0.0" (hydra_lib_literals_show_float32 0.0))))

(define (test-literals-negshowfloat32-negsmall-positive)

  (assert (equal? "5.0e-2" (hydra_lib_literals_show_float32 5.000000074505806e-2))))

(define (test-literals-negshowfloat32-negsmall-positive-2)

  (assert (equal? "3.0e-2" (hydra_lib_literals_show_float32 2.9999999329447746e-2))))

(define (test-literals-negshowfloat32-negvery-small)

  (assert (equal? "1.0e-3" (hydra_lib_literals_show_float32 1.0000000474974513e-3))))

(define (test-literals-negshowfloat32-negnormal-decimal)

  (assert (equal? "0.1" (hydra_lib_literals_show_float32 0.10000000149011612))))

;; showFloat64

(define (test-literals-negshowfloat64-negpositive)

  (assert (equal? "3.14159" (hydra_lib_literals_show_float64 3.14159))))

(define (test-literals-negshowfloat64-negzero)

  (assert (equal? "0.0" (hydra_lib_literals_show_float64 0.0))))

(define (test-literals-negshowfloat64-negsmall-positive)

  (assert (equal? "5.0e-2" (hydra_lib_literals_show_float64 5.0e-2))))

(define (test-literals-negshowfloat64-negsmall-positive-2)

  (assert (equal? "3.0e-2" (hydra_lib_literals_show_float64 3.0e-2))))

(define (test-literals-negshowfloat64-negvery-small)

  (assert (equal? "1.0e-3" (hydra_lib_literals_show_float64 1.0e-3))))

(define (test-literals-negshowfloat64-negnormal-decimal)

  (assert (equal? "0.1" (hydra_lib_literals_show_float64 0.1))))

;; showBigfloat

(define (test-literals-negshowbigfloat-negpositive)

  (assert (equal? "3.14" (hydra_lib_literals_show_bigfloat 3.14))))

(define (test-literals-negshowbigfloat-negzero)

  (assert (equal? "0.0" (hydra_lib_literals_show_bigfloat 0.0))))

(define (test-literals-negshowbigfloat-negsmall-positive)

  (assert (equal? "5.0e-2" (hydra_lib_literals_show_bigfloat 5.0e-2))))

(define (test-literals-negshowbigfloat-negsmall-positive-2)

  (assert (equal? "3.0e-2" (hydra_lib_literals_show_bigfloat 3.0e-2))))

(define (test-literals-negshowbigfloat-negvery-small)

  (assert (equal? "1.0e-3" (hydra_lib_literals_show_bigfloat 1.0e-3))))

(define (test-literals-negshowbigfloat-negnormal-decimal)

  (assert (equal? "0.1" (hydra_lib_literals_show_bigfloat 0.1))))

;; showBoolean

(define (test-literals-negshowboolean-negtrue)

  (assert (equal? "true" (hydra_lib_literals_show_boolean #t))))

(define (test-literals-negshowboolean-negfalse)

  (assert (equal? "false" (hydra_lib_literals_show_boolean #f))))

;; showString

(define (test-literals-negshowstring-negsimple)

  (assert (equal? "\"hello\"" (hydra_lib_literals_show_string "hello"))))

(define (test-literals-negshowstring-negempty)

  (assert (equal? "\"\"" (hydra_lib_literals_show_string ""))))

(define (test-literals-negshowstring-neglatin-accented)

  (assert (equal? "\"caf\\233\"" (hydra_lib_literals_show_string "café"))))

(define (test-literals-negshowstring-neggreek-lambda)

  (assert (equal? "\"\\955\"" (hydra_lib_literals_show_string "λ"))))

(define (test-literals-negshowstring-negmixed-ascii-and-non-negascii)

  (assert (equal? "\"A\\233B\"" (hydra_lib_literals_show_string "AéB"))))

(define (test-literals-negshowstring-negtab)

  (assert (equal? "\"\\t\"" (hydra_lib_literals_show_string "\t"))))

(define (test-literals-negshowstring-negnewline)

  (assert (equal? "\"\\n\"" (hydra_lib_literals_show_string "\n"))))

(define (test-literals-negshowstring-negcarriage-return)

  (assert (equal? "\"\\r\"" (hydra_lib_literals_show_string "\r"))))

(define (test-literals-negshowstring-negbackslash)

  (assert (equal? "\"\\\\\"" (hydra_lib_literals_show_string "\\"))))

(define (test-literals-negshowstring-negdouble-quote)

  (assert (equal? "\"\\\"\"" (hydra_lib_literals_show_string "\""))))

(define (test-literals-negshowstring-negnull)

  (assert (equal? "\"\\NUL\"" (hydra_lib_literals_show_string " "))))

(define (test-literals-negshowstring-negbell)

  (assert (equal? "\"\\a\"" (hydra_lib_literals_show_string ""))))

(define (test-literals-negshowstring-negbackspace)

  (assert (equal? "\"\\b\"" (hydra_lib_literals_show_string ""))))

(define (test-literals-negshowstring-negform-feed)

  (assert (equal? "\"\\f\"" (hydra_lib_literals_show_string ""))))

(define (test-literals-negshowstring-negvertical-tab)

  (assert (equal? "\"\\v\"" (hydra_lib_literals_show_string ""))))

(define (test-literals-negshowstring-negdelete)

  (assert (equal? "\"\\DEL\"" (hydra_lib_literals_show_string ""))))

;; readInt8

(define (test-literals-negreadint8-negpositive)

  (assert (equal? (list 'just 42) (hydra_lib_literals_read_int8 "42"))))

(define (test-literals-negreadint8-negnegative)

  (assert (equal? (list 'just -42) (hydra_lib_literals_read_int8 "-42"))))

(define (test-literals-negreadint8-negmax-value)

  (assert (equal? (list 'just 127) (hydra_lib_literals_read_int8 "127"))))

(define (test-literals-negreadint8-negmin-value)

  (assert (equal? (list 'just -128) (hydra_lib_literals_read_int8 "-128"))))

(define (test-literals-negreadint8-neginvalid)

  (assert (equal? (list 'nothing) (hydra_lib_literals_read_int8 "abc"))))

(define (test-literals-negreadint8-negoverflow)

  (assert (equal? (list 'nothing) (hydra_lib_literals_read_int8 "128"))))

;; readInt16

(define (test-literals-negreadint16-negpositive)

  (assert (equal? (list 'just 1000) (hydra_lib_literals_read_int16 "1000"))))

(define (test-literals-negreadint16-negnegative)

  (assert (equal? (list 'just -1000) (hydra_lib_literals_read_int16 "-1000"))))

(define (test-literals-negreadint16-neginvalid)

  (assert (equal? (list 'nothing) (hydra_lib_literals_read_int16 "abc"))))

;; readInt32

(define (test-literals-negreadint32-negpositive)

  (assert (equal? (list 'just 42) (hydra_lib_literals_read_int32 "42"))))

(define (test-literals-negreadint32-negnegative)

  (assert (equal? (list 'just -42) (hydra_lib_literals_read_int32 "-42"))))

(define (test-literals-negreadint32-neginvalid)

  (assert (equal? (list 'nothing) (hydra_lib_literals_read_int32 "abc"))))

;; readInt64

(define (test-literals-negreadint64-negpositive)

  (assert (equal? (list 'just 1000000) (hydra_lib_literals_read_int64 "1000000"))))

(define (test-literals-negreadint64-negnegative)

  (assert (equal? (list 'just -1000000) (hydra_lib_literals_read_int64 "-1000000"))))

(define (test-literals-negreadint64-neginvalid)

  (assert (equal? (list 'nothing) (hydra_lib_literals_read_int64 "abc"))))

;; readUint8

(define (test-literals-negreaduint8-negzero)

  (assert (equal? (list 'just 0) (hydra_lib_literals_read_uint8 "0"))))

(define (test-literals-negreaduint8-negtypical)

  (assert (equal? (list 'just 100) (hydra_lib_literals_read_uint8 "100"))))

(define (test-literals-negreaduint8-negmax-value)

  (assert (equal? (list 'just 255) (hydra_lib_literals_read_uint8 "255"))))

(define (test-literals-negreaduint8-neginvalid)

  (assert (equal? (list 'nothing) (hydra_lib_literals_read_uint8 "abc"))))

(define (test-literals-negreaduint8-negnegative)

  (assert (equal? (list 'nothing) (hydra_lib_literals_read_uint8 "-1"))))

;; readUint16

(define (test-literals-negreaduint16-negzero)

  (assert (equal? (list 'just 0) (hydra_lib_literals_read_uint16 "0"))))

(define (test-literals-negreaduint16-negtypical)

  (assert (equal? (list 'just 1000) (hydra_lib_literals_read_uint16 "1000"))))

(define (test-literals-negreaduint16-neginvalid)

  (assert (equal? (list 'nothing) (hydra_lib_literals_read_uint16 "abc"))))

(define (test-literals-negreaduint16-negnegative)

  (assert (equal? (list 'nothing) (hydra_lib_literals_read_uint16 "-1"))))

;; readUint32

(define (test-literals-negreaduint32-negzero)

  (assert (equal? (list 'just 0) (hydra_lib_literals_read_uint32 "0"))))

(define (test-literals-negreaduint32-negtypical)

  (assert (equal? (list 'just 100000) (hydra_lib_literals_read_uint32 "100000"))))

(define (test-literals-negreaduint32-neginvalid)

  (assert (equal? (list 'nothing) (hydra_lib_literals_read_uint32 "abc"))))

(define (test-literals-negreaduint32-negnegative)

  (assert (equal? (list 'nothing) (hydra_lib_literals_read_uint32 "-1"))))

;; readUint64

(define (test-literals-negreaduint64-negzero)

  (assert (equal? (list 'just 0) (hydra_lib_literals_read_uint64 "0"))))

(define (test-literals-negreaduint64-negtypical)

  (assert (equal? (list 'just 1000000) (hydra_lib_literals_read_uint64 "1000000"))))

(define (test-literals-negreaduint64-neginvalid)

  (assert (equal? (list 'nothing) (hydra_lib_literals_read_uint64 "abc"))))

(define (test-literals-negreaduint64-negnegative)

  (assert (equal? (list 'nothing) (hydra_lib_literals_read_uint64 "-1"))))

;; readBigint

(define (test-literals-negreadbigint-negpositive)

  (assert (equal? (list 'just 42) (hydra_lib_literals_read_bigint "42"))))

(define (test-literals-negreadbigint-negnegative)

  (assert (equal? (list 'just -42) (hydra_lib_literals_read_bigint "-42"))))

(define (test-literals-negreadbigint-negzero)

  (assert (equal? (list 'just 0) (hydra_lib_literals_read_bigint "0"))))

(define (test-literals-negreadbigint-neglarge)

  (assert (equal? (list 'just 123456789012345678901234567890) (hydra_lib_literals_read_bigint "123456789012345678901234567890"))))

(define (test-literals-negreadbigint-neginvalid)

  (assert (equal? (list 'nothing) (hydra_lib_literals_read_bigint "abc"))))

;; readFloat32

(define (test-literals-negreadfloat32-negpositive)

  (assert (equal? (list 'just 3.140000104904175) (hydra_lib_literals_read_float32 "3.14"))))

(define (test-literals-negreadfloat32-negnegative)

  (assert (equal? (list 'just -2.5) (hydra_lib_literals_read_float32 "-2.5"))))

(define (test-literals-negreadfloat32-neginvalid)

  (assert (equal? (list 'nothing) (hydra_lib_literals_read_float32 "abc"))))

;; readFloat64

(define (test-literals-negreadfloat64-negpositive)

  (assert (equal? (list 'just 3.14159) (hydra_lib_literals_read_float64 "3.14159"))))

(define (test-literals-negreadfloat64-negnegative)

  (assert (equal? (list 'just -2.71828) (hydra_lib_literals_read_float64 "-2.71828"))))

(define (test-literals-negreadfloat64-neginvalid)

  (assert (equal? (list 'nothing) (hydra_lib_literals_read_float64 "abc"))))

;; readBigfloat

(define (test-literals-negreadbigfloat-negpositive)

  (assert (equal? (list 'just 3.14) (hydra_lib_literals_read_bigfloat "3.14"))))

(define (test-literals-negreadbigfloat-neginvalid)

  (assert (equal? (list 'nothing) (hydra_lib_literals_read_bigfloat "abc"))))

;; readBoolean

(define (test-literals-negreadboolean-negtrue)

  (assert (equal? (list 'just #t) (hydra_lib_literals_read_boolean "true"))))

(define (test-literals-negreadboolean-negfalse)

  (assert (equal? (list 'just #f) (hydra_lib_literals_read_boolean "false"))))

(define (test-literals-negreadboolean-neginvalid)

  (assert (equal? (list 'nothing) (hydra_lib_literals_read_boolean "yes"))))

;; readString

(define (test-literals-negreadstring-negquoted-string)

  (assert (equal? (list 'just "hello") (hydra_lib_literals_read_string "\"hello\""))))

(define (test-literals-negreadstring-negempty-quoted)

  (assert (equal? (list 'just "") (hydra_lib_literals_read_string "\"\""))))

(define (test-literals-negreadstring-negunquoted)

  (assert (equal? (list 'nothing) (hydra_lib_literals_read_string "hello"))))

;; stringToBinary

(define (test-literals-negstringtobinary-negsimple-base64)

  (assert (equal? (list 104 101 108 108 111) (hydra_lib_literals_string_to_binary "aGVsbG8="))))

(define (test-literals-negstringtobinary-negempty-string)

  (assert (equal? (list) (hydra_lib_literals_string_to_binary ""))))

;; binaryToString

(define (test-literals-negbinarytostring-negsimple-binary)

  (assert (equal? "aGVsbG8=" (hydra_lib_literals_binary_to_string (list 104 101 108 108 111)))))

(define (test-literals-negbinarytostring-negempty-binary)

  (assert (equal? "" (hydra_lib_literals_binary_to_string (list)))))
