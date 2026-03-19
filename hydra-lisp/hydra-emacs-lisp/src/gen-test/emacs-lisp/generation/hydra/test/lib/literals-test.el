;;; Note: this is an automatically generated file. Do not edit. -*- lexical-binding: t; coding: utf-8 -*-
;;; hydra.lib.literals primitives

(require 'ert)

;; bigintToInt8

(ert-deftest test-literals-negbiginttoint8-negpositive ()

  (should (equal 42 (funcall hydra_lib_literals_bigint_to_int8 42))))

(ert-deftest test-literals-negbiginttoint8-negnegative ()

  (should (equal -42 (funcall hydra_lib_literals_bigint_to_int8 -42))))

;; bigintToInt16

(ert-deftest test-literals-negbiginttoint16-negpositive ()

  (should (equal 1000 (funcall hydra_lib_literals_bigint_to_int16 1000))))

(ert-deftest test-literals-negbiginttoint16-negnegative ()

  (should (equal -1000 (funcall hydra_lib_literals_bigint_to_int16 -1000))))

;; bigintToInt32

(ert-deftest test-literals-negbiginttoint32-negpositive ()

  (should (equal 42 (funcall hydra_lib_literals_bigint_to_int32 42))))

(ert-deftest test-literals-negbiginttoint32-negnegative ()

  (should (equal -42 (funcall hydra_lib_literals_bigint_to_int32 -42))))

(ert-deftest test-literals-negbiginttoint32-negzero ()

  (should (equal 0 (funcall hydra_lib_literals_bigint_to_int32 0))))

;; bigintToInt64

(ert-deftest test-literals-negbiginttoint64-negpositive ()

  (should (equal 1000000 (funcall hydra_lib_literals_bigint_to_int64 1000000))))

(ert-deftest test-literals-negbiginttoint64-negnegative ()

  (should (equal -1000000 (funcall hydra_lib_literals_bigint_to_int64 -1000000))))

;; bigintToUint8

(ert-deftest test-literals-negbiginttouint8-negzero ()

  (should (equal 0 (funcall hydra_lib_literals_bigint_to_uint8 0))))

(ert-deftest test-literals-negbiginttouint8-negtypical-value ()

  (should (equal 100 (funcall hydra_lib_literals_bigint_to_uint8 100))))

;; bigintToUint16

(ert-deftest test-literals-negbiginttouint16-negzero ()

  (should (equal 0 (funcall hydra_lib_literals_bigint_to_uint16 0))))

(ert-deftest test-literals-negbiginttouint16-negtypical-value ()

  (should (equal 1000 (funcall hydra_lib_literals_bigint_to_uint16 1000))))

;; bigintToUint32

(ert-deftest test-literals-negbiginttouint32-negzero ()

  (should (equal 0 (funcall hydra_lib_literals_bigint_to_uint32 0))))

(ert-deftest test-literals-negbiginttouint32-negtypical-value ()

  (should (equal 100000 (funcall hydra_lib_literals_bigint_to_uint32 100000))))

;; bigintToUint64

(ert-deftest test-literals-negbiginttouint64-negzero ()

  (should (equal 0 (funcall hydra_lib_literals_bigint_to_uint64 0))))

(ert-deftest test-literals-negbiginttouint64-negtypical-value ()

  (should (equal 1000000 (funcall hydra_lib_literals_bigint_to_uint64 1000000))))

;; int8ToBigint

(ert-deftest test-literals-negint8tobigint-negpositive ()

  (should (equal 42 (funcall hydra_lib_literals_int8_to_bigint 42))))

(ert-deftest test-literals-negint8tobigint-negnegative ()

  (should (equal -42 (funcall hydra_lib_literals_int8_to_bigint -42))))

(ert-deftest test-literals-negint8tobigint-negmax-value ()

  (should (equal 127 (funcall hydra_lib_literals_int8_to_bigint 127))))

(ert-deftest test-literals-negint8tobigint-negmin-value ()

  (should (equal -128 (funcall hydra_lib_literals_int8_to_bigint -128))))

;; int16ToBigint

(ert-deftest test-literals-negint16tobigint-negpositive ()

  (should (equal 1000 (funcall hydra_lib_literals_int16_to_bigint 1000))))

(ert-deftest test-literals-negint16tobigint-negnegative ()

  (should (equal -1000 (funcall hydra_lib_literals_int16_to_bigint -1000))))

;; int32ToBigint

(ert-deftest test-literals-negint32tobigint-negpositive ()

  (should (equal 42 (funcall hydra_lib_literals_int32_to_bigint 42))))

(ert-deftest test-literals-negint32tobigint-negnegative ()

  (should (equal -42 (funcall hydra_lib_literals_int32_to_bigint -42))))

(ert-deftest test-literals-negint32tobigint-negzero ()

  (should (equal 0 (funcall hydra_lib_literals_int32_to_bigint 0))))

;; int64ToBigint

(ert-deftest test-literals-negint64tobigint-negpositive ()

  (should (equal 1000000 (funcall hydra_lib_literals_int64_to_bigint 1000000))))

(ert-deftest test-literals-negint64tobigint-negnegative ()

  (should (equal -1000000 (funcall hydra_lib_literals_int64_to_bigint -1000000))))

;; uint8ToBigint

(ert-deftest test-literals-neguint8tobigint-negzero ()

  (should (equal 0 (funcall hydra_lib_literals_uint8_to_bigint 0))))

(ert-deftest test-literals-neguint8tobigint-negmax-value ()

  (should (equal 255 (funcall hydra_lib_literals_uint8_to_bigint 255))))

;; uint16ToBigint

(ert-deftest test-literals-neguint16tobigint-negzero ()

  (should (equal 0 (funcall hydra_lib_literals_uint16_to_bigint 0))))

(ert-deftest test-literals-neguint16tobigint-negtypical-value ()

  (should (equal 1000 (funcall hydra_lib_literals_uint16_to_bigint 1000))))

;; uint32ToBigint

(ert-deftest test-literals-neguint32tobigint-negzero ()

  (should (equal 0 (funcall hydra_lib_literals_uint32_to_bigint 0))))

(ert-deftest test-literals-neguint32tobigint-negtypical-value ()

  (should (equal 100000 (funcall hydra_lib_literals_uint32_to_bigint 100000))))

;; uint64ToBigint

(ert-deftest test-literals-neguint64tobigint-negzero ()

  (should (equal 0 (funcall hydra_lib_literals_uint64_to_bigint 0))))

(ert-deftest test-literals-neguint64tobigint-negtypical-value ()

  (should (equal 1000000 (funcall hydra_lib_literals_uint64_to_bigint 1000000))))

;; float32ToBigfloat

(ert-deftest test-literals-negfloat32tobigfloat-negpositive ()

  (should (equal 2.5 (funcall hydra_lib_literals_float32_to_bigfloat 2.5))))

(ert-deftest test-literals-negfloat32tobigfloat-negnegative ()

  (should (equal -2.5 (funcall hydra_lib_literals_float32_to_bigfloat -2.5))))

(ert-deftest test-literals-negfloat32tobigfloat-negzero ()

  (should (equal 0.0 (funcall hydra_lib_literals_float32_to_bigfloat 0.0))))

;; float64ToBigfloat

(ert-deftest test-literals-negfloat64tobigfloat-negpositive ()

  (should (equal 3.14159 (funcall hydra_lib_literals_float64_to_bigfloat 3.14159))))

(ert-deftest test-literals-negfloat64tobigfloat-negnegative ()

  (should (equal -2.71828 (funcall hydra_lib_literals_float64_to_bigfloat -2.71828))))

(ert-deftest test-literals-negfloat64tobigfloat-negzero ()

  (should (equal 0.0 (funcall hydra_lib_literals_float64_to_bigfloat 0.0))))

;; bigfloatToFloat32

(ert-deftest test-literals-negbigfloattofloat32-negpositive ()

  (should (equal 3.140000104904175 (funcall hydra_lib_literals_bigfloat_to_float32 3.14))))

(ert-deftest test-literals-negbigfloattofloat32-negnegative ()

  (should (equal -2.5 (funcall hydra_lib_literals_bigfloat_to_float32 -2.5))))

(ert-deftest test-literals-negbigfloattofloat32-negzero ()

  (should (equal 0.0 (funcall hydra_lib_literals_bigfloat_to_float32 0.0))))

;; bigfloatToFloat64

(ert-deftest test-literals-negbigfloattofloat64-negpositive ()

  (should (equal 3.14159 (funcall hydra_lib_literals_bigfloat_to_float64 3.14159))))

(ert-deftest test-literals-negbigfloattofloat64-negnegative ()

  (should (equal -2.71828 (funcall hydra_lib_literals_bigfloat_to_float64 -2.71828))))

(ert-deftest test-literals-negbigfloattofloat64-negzero ()

  (should (equal 0.0 (funcall hydra_lib_literals_bigfloat_to_float64 0.0))))

;; bigintToBigfloat

(ert-deftest test-literals-negbiginttobigfloat-negpositive ()

  (should (equal 42.0 (funcall hydra_lib_literals_bigint_to_bigfloat 42))))

(ert-deftest test-literals-negbiginttobigfloat-negnegative ()

  (should (equal -42.0 (funcall hydra_lib_literals_bigint_to_bigfloat -42))))

(ert-deftest test-literals-negbiginttobigfloat-negzero ()

  (should (equal 0.0 (funcall hydra_lib_literals_bigint_to_bigfloat 0))))

;; bigfloatToBigint

(ert-deftest test-literals-negbigfloattobigint-negpositive ()

  (should (equal 43 (funcall hydra_lib_literals_bigfloat_to_bigint 42.7))))

(ert-deftest test-literals-negbigfloattobigint-negnegative ()

  (should (equal -43 (funcall hydra_lib_literals_bigfloat_to_bigint -42.7))))

(ert-deftest test-literals-negbigfloattobigint-negzero ()

  (should (equal 0 (funcall hydra_lib_literals_bigfloat_to_bigint 0.0))))

(ert-deftest test-literals-negbigfloattobigint-neground-down ()

  (should (equal 42 (funcall hydra_lib_literals_bigfloat_to_bigint 42.3))))

(ert-deftest test-literals-negbigfloattobigint-neghalf-even-up ()

  (should (equal 42 (funcall hydra_lib_literals_bigfloat_to_bigint 42.5))))

(ert-deftest test-literals-negbigfloattobigint-neghalf-even-down ()

  (should (equal 44 (funcall hydra_lib_literals_bigfloat_to_bigint 43.5))))

;; showInt8

(ert-deftest test-literals-negshowint8-negpositive ()

  (should (equal "42" (funcall hydra_lib_literals_show_int8 42))))

(ert-deftest test-literals-negshowint8-negnegative ()

  (should (equal "-42" (funcall hydra_lib_literals_show_int8 -42))))

;; showInt16

(ert-deftest test-literals-negshowint16-negpositive ()

  (should (equal "1000" (funcall hydra_lib_literals_show_int16 1000))))

(ert-deftest test-literals-negshowint16-negnegative ()

  (should (equal "-1000" (funcall hydra_lib_literals_show_int16 -1000))))

;; showInt32

(ert-deftest test-literals-negshowint32-negpositive ()

  (should (equal "42" (funcall hydra_lib_literals_show_int32 42))))

(ert-deftest test-literals-negshowint32-negnegative ()

  (should (equal "-42" (funcall hydra_lib_literals_show_int32 -42))))

(ert-deftest test-literals-negshowint32-negzero ()

  (should (equal "0" (funcall hydra_lib_literals_show_int32 0))))

;; showInt64

(ert-deftest test-literals-negshowint64-negpositive ()

  (should (equal "1000000" (funcall hydra_lib_literals_show_int64 1000000))))

(ert-deftest test-literals-negshowint64-negnegative ()

  (should (equal "-1000000" (funcall hydra_lib_literals_show_int64 -1000000))))

;; showUint8

(ert-deftest test-literals-negshowuint8-negzero ()

  (should (equal "0" (funcall hydra_lib_literals_show_uint8 0))))

(ert-deftest test-literals-negshowuint8-negmax-value ()

  (should (equal "255" (funcall hydra_lib_literals_show_uint8 255))))

;; showUint16

(ert-deftest test-literals-negshowuint16-negzero ()

  (should (equal "0" (funcall hydra_lib_literals_show_uint16 0))))

(ert-deftest test-literals-negshowuint16-negtypical-value ()

  (should (equal "1000" (funcall hydra_lib_literals_show_uint16 1000))))

;; showUint32

(ert-deftest test-literals-negshowuint32-negzero ()

  (should (equal "0" (funcall hydra_lib_literals_show_uint32 0))))

(ert-deftest test-literals-negshowuint32-negtypical-value ()

  (should (equal "100000" (funcall hydra_lib_literals_show_uint32 100000))))

;; showUint64

(ert-deftest test-literals-negshowuint64-negzero ()

  (should (equal "0" (funcall hydra_lib_literals_show_uint64 0))))

(ert-deftest test-literals-negshowuint64-negtypical-value ()

  (should (equal "1000000" (funcall hydra_lib_literals_show_uint64 1000000))))

;; showBigint

(ert-deftest test-literals-negshowbigint-negpositive ()

  (should (equal "42" (funcall hydra_lib_literals_show_bigint 42))))

(ert-deftest test-literals-negshowbigint-negnegative ()

  (should (equal "-42" (funcall hydra_lib_literals_show_bigint -42))))

(ert-deftest test-literals-negshowbigint-negzero ()

  (should (equal "0" (funcall hydra_lib_literals_show_bigint 0))))

;; showFloat32

(ert-deftest test-literals-negshowfloat32-negpositive ()

  (should (equal "3.14" (funcall hydra_lib_literals_show_float32 3.140000104904175))))

(ert-deftest test-literals-negshowfloat32-negnegative ()

  (should (equal "-2.5" (funcall hydra_lib_literals_show_float32 -2.5))))

(ert-deftest test-literals-negshowfloat32-negzero ()

  (should (equal "0.0" (funcall hydra_lib_literals_show_float32 0.0))))

(ert-deftest test-literals-negshowfloat32-negsmall-positive ()

  (should (equal "5.0e-2" (funcall hydra_lib_literals_show_float32 5.000000074505806e-2))))

(ert-deftest test-literals-negshowfloat32-negsmall-positive-2 ()

  (should (equal "3.0e-2" (funcall hydra_lib_literals_show_float32 2.9999999329447746e-2))))

(ert-deftest test-literals-negshowfloat32-negvery-small ()

  (should (equal "1.0e-3" (funcall hydra_lib_literals_show_float32 1.0000000474974513e-3))))

(ert-deftest test-literals-negshowfloat32-negnormal-decimal ()

  (should (equal "0.1" (funcall hydra_lib_literals_show_float32 0.10000000149011612))))

;; showFloat64

(ert-deftest test-literals-negshowfloat64-negpositive ()

  (should (equal "3.14159" (funcall hydra_lib_literals_show_float64 3.14159))))

(ert-deftest test-literals-negshowfloat64-negzero ()

  (should (equal "0.0" (funcall hydra_lib_literals_show_float64 0.0))))

(ert-deftest test-literals-negshowfloat64-negsmall-positive ()

  (should (equal "5.0e-2" (funcall hydra_lib_literals_show_float64 5.0e-2))))

(ert-deftest test-literals-negshowfloat64-negsmall-positive-2 ()

  (should (equal "3.0e-2" (funcall hydra_lib_literals_show_float64 3.0e-2))))

(ert-deftest test-literals-negshowfloat64-negvery-small ()

  (should (equal "1.0e-3" (funcall hydra_lib_literals_show_float64 1.0e-3))))

(ert-deftest test-literals-negshowfloat64-negnormal-decimal ()

  (should (equal "0.1" (funcall hydra_lib_literals_show_float64 0.1))))

;; showBigfloat

(ert-deftest test-literals-negshowbigfloat-negpositive ()

  (should (equal "3.14" (funcall hydra_lib_literals_show_bigfloat 3.14))))

(ert-deftest test-literals-negshowbigfloat-negzero ()

  (should (equal "0.0" (funcall hydra_lib_literals_show_bigfloat 0.0))))

(ert-deftest test-literals-negshowbigfloat-negsmall-positive ()

  (should (equal "5.0e-2" (funcall hydra_lib_literals_show_bigfloat 5.0e-2))))

(ert-deftest test-literals-negshowbigfloat-negsmall-positive-2 ()

  (should (equal "3.0e-2" (funcall hydra_lib_literals_show_bigfloat 3.0e-2))))

(ert-deftest test-literals-negshowbigfloat-negvery-small ()

  (should (equal "1.0e-3" (funcall hydra_lib_literals_show_bigfloat 1.0e-3))))

(ert-deftest test-literals-negshowbigfloat-negnormal-decimal ()

  (should (equal "0.1" (funcall hydra_lib_literals_show_bigfloat 0.1))))

;; showBoolean

(ert-deftest test-literals-negshowboolean-negtrue ()

  (should (equal "true" (funcall hydra_lib_literals_show_boolean t))))

(ert-deftest test-literals-negshowboolean-negfalse ()

  (should (equal "false" (funcall hydra_lib_literals_show_boolean nil))))

;; showString

(ert-deftest test-literals-negshowstring-negsimple ()

  (should (equal "\"hello\"" (funcall hydra_lib_literals_show_string "hello"))))

(ert-deftest test-literals-negshowstring-negempty ()

  (should (equal "\"\"" (funcall hydra_lib_literals_show_string ""))))

(ert-deftest test-literals-negshowstring-neglatin-accented ()

  (should (equal "\"caf\\233\"" (funcall hydra_lib_literals_show_string "café"))))

(ert-deftest test-literals-negshowstring-neggreek-lambda ()

  (should (equal "\"\\955\"" (funcall hydra_lib_literals_show_string "λ"))))

(ert-deftest test-literals-negshowstring-negmixed-ascii-and-non-negascii ()

  (should (equal "\"A\\233B\"" (funcall hydra_lib_literals_show_string "AéB"))))

(ert-deftest test-literals-negshowstring-negtab ()

  (should (equal "\"\\t\"" (funcall hydra_lib_literals_show_string "\t"))))

(ert-deftest test-literals-negshowstring-negnewline ()

  (should (equal "\"\\n\"" (funcall hydra_lib_literals_show_string "\n"))))

(ert-deftest test-literals-negshowstring-negcarriage-return ()

  (should (equal "\"\\r\"" (funcall hydra_lib_literals_show_string "\r"))))

(ert-deftest test-literals-negshowstring-negbackslash ()

  (should (equal "\"\\\\\"" (funcall hydra_lib_literals_show_string "\\"))))

(ert-deftest test-literals-negshowstring-negdouble-quote ()

  (should (equal "\"\\\"\"" (funcall hydra_lib_literals_show_string "\""))))

(ert-deftest test-literals-negshowstring-negnull ()

  (should (equal "\"\\NUL\"" (funcall hydra_lib_literals_show_string " "))))

(ert-deftest test-literals-negshowstring-negbell ()

  (should (equal "\"\\a\"" (funcall hydra_lib_literals_show_string ""))))

(ert-deftest test-literals-negshowstring-negbackspace ()

  (should (equal "\"\\b\"" (funcall hydra_lib_literals_show_string ""))))

(ert-deftest test-literals-negshowstring-negform-feed ()

  (should (equal "\"\\f\"" (funcall hydra_lib_literals_show_string ""))))

(ert-deftest test-literals-negshowstring-negvertical-tab ()

  (should (equal "\"\\v\"" (funcall hydra_lib_literals_show_string ""))))

(ert-deftest test-literals-negshowstring-negdelete ()

  (should (equal "\"\\DEL\"" (funcall hydra_lib_literals_show_string ""))))

;; readInt8

(ert-deftest test-literals-negreadint8-negpositive ()

  (should (equal (list :just 42) (funcall hydra_lib_literals_read_int8 "42"))))

(ert-deftest test-literals-negreadint8-negnegative ()

  (should (equal (list :just -42) (funcall hydra_lib_literals_read_int8 "-42"))))

(ert-deftest test-literals-negreadint8-negmax-value ()

  (should (equal (list :just 127) (funcall hydra_lib_literals_read_int8 "127"))))

(ert-deftest test-literals-negreadint8-negmin-value ()

  (should (equal (list :just -128) (funcall hydra_lib_literals_read_int8 "-128"))))

(ert-deftest test-literals-negreadint8-neginvalid ()

  (should (equal (list :nothing) (funcall hydra_lib_literals_read_int8 "abc"))))

(ert-deftest test-literals-negreadint8-negoverflow ()

  (should (equal (list :nothing) (funcall hydra_lib_literals_read_int8 "128"))))

;; readInt16

(ert-deftest test-literals-negreadint16-negpositive ()

  (should (equal (list :just 1000) (funcall hydra_lib_literals_read_int16 "1000"))))

(ert-deftest test-literals-negreadint16-negnegative ()

  (should (equal (list :just -1000) (funcall hydra_lib_literals_read_int16 "-1000"))))

(ert-deftest test-literals-negreadint16-neginvalid ()

  (should (equal (list :nothing) (funcall hydra_lib_literals_read_int16 "abc"))))

;; readInt32

(ert-deftest test-literals-negreadint32-negpositive ()

  (should (equal (list :just 42) (funcall hydra_lib_literals_read_int32 "42"))))

(ert-deftest test-literals-negreadint32-negnegative ()

  (should (equal (list :just -42) (funcall hydra_lib_literals_read_int32 "-42"))))

(ert-deftest test-literals-negreadint32-neginvalid ()

  (should (equal (list :nothing) (funcall hydra_lib_literals_read_int32 "abc"))))

;; readInt64

(ert-deftest test-literals-negreadint64-negpositive ()

  (should (equal (list :just 1000000) (funcall hydra_lib_literals_read_int64 "1000000"))))

(ert-deftest test-literals-negreadint64-negnegative ()

  (should (equal (list :just -1000000) (funcall hydra_lib_literals_read_int64 "-1000000"))))

(ert-deftest test-literals-negreadint64-neginvalid ()

  (should (equal (list :nothing) (funcall hydra_lib_literals_read_int64 "abc"))))

;; readUint8

(ert-deftest test-literals-negreaduint8-negzero ()

  (should (equal (list :just 0) (funcall hydra_lib_literals_read_uint8 "0"))))

(ert-deftest test-literals-negreaduint8-negtypical ()

  (should (equal (list :just 100) (funcall hydra_lib_literals_read_uint8 "100"))))

(ert-deftest test-literals-negreaduint8-negmax-value ()

  (should (equal (list :just 255) (funcall hydra_lib_literals_read_uint8 "255"))))

(ert-deftest test-literals-negreaduint8-neginvalid ()

  (should (equal (list :nothing) (funcall hydra_lib_literals_read_uint8 "abc"))))

(ert-deftest test-literals-negreaduint8-negnegative ()

  (should (equal (list :nothing) (funcall hydra_lib_literals_read_uint8 "-1"))))

;; readUint16

(ert-deftest test-literals-negreaduint16-negzero ()

  (should (equal (list :just 0) (funcall hydra_lib_literals_read_uint16 "0"))))

(ert-deftest test-literals-negreaduint16-negtypical ()

  (should (equal (list :just 1000) (funcall hydra_lib_literals_read_uint16 "1000"))))

(ert-deftest test-literals-negreaduint16-neginvalid ()

  (should (equal (list :nothing) (funcall hydra_lib_literals_read_uint16 "abc"))))

(ert-deftest test-literals-negreaduint16-negnegative ()

  (should (equal (list :nothing) (funcall hydra_lib_literals_read_uint16 "-1"))))

;; readUint32

(ert-deftest test-literals-negreaduint32-negzero ()

  (should (equal (list :just 0) (funcall hydra_lib_literals_read_uint32 "0"))))

(ert-deftest test-literals-negreaduint32-negtypical ()

  (should (equal (list :just 100000) (funcall hydra_lib_literals_read_uint32 "100000"))))

(ert-deftest test-literals-negreaduint32-neginvalid ()

  (should (equal (list :nothing) (funcall hydra_lib_literals_read_uint32 "abc"))))

(ert-deftest test-literals-negreaduint32-negnegative ()

  (should (equal (list :nothing) (funcall hydra_lib_literals_read_uint32 "-1"))))

;; readUint64

(ert-deftest test-literals-negreaduint64-negzero ()

  (should (equal (list :just 0) (funcall hydra_lib_literals_read_uint64 "0"))))

(ert-deftest test-literals-negreaduint64-negtypical ()

  (should (equal (list :just 1000000) (funcall hydra_lib_literals_read_uint64 "1000000"))))

(ert-deftest test-literals-negreaduint64-neginvalid ()

  (should (equal (list :nothing) (funcall hydra_lib_literals_read_uint64 "abc"))))

(ert-deftest test-literals-negreaduint64-negnegative ()

  (should (equal (list :nothing) (funcall hydra_lib_literals_read_uint64 "-1"))))

;; readBigint

(ert-deftest test-literals-negreadbigint-negpositive ()

  (should (equal (list :just 42) (funcall hydra_lib_literals_read_bigint "42"))))

(ert-deftest test-literals-negreadbigint-negnegative ()

  (should (equal (list :just -42) (funcall hydra_lib_literals_read_bigint "-42"))))

(ert-deftest test-literals-negreadbigint-negzero ()

  (should (equal (list :just 0) (funcall hydra_lib_literals_read_bigint "0"))))

(ert-deftest test-literals-negreadbigint-neglarge ()

  (should (equal (list :just 123456789012345678901234567890) (funcall hydra_lib_literals_read_bigint "123456789012345678901234567890"))))

(ert-deftest test-literals-negreadbigint-neginvalid ()

  (should (equal (list :nothing) (funcall hydra_lib_literals_read_bigint "abc"))))

;; readFloat32

(ert-deftest test-literals-negreadfloat32-negpositive ()

  (should (equal (list :just 3.140000104904175) (funcall hydra_lib_literals_read_float32 "3.14"))))

(ert-deftest test-literals-negreadfloat32-negnegative ()

  (should (equal (list :just -2.5) (funcall hydra_lib_literals_read_float32 "-2.5"))))

(ert-deftest test-literals-negreadfloat32-neginvalid ()

  (should (equal (list :nothing) (funcall hydra_lib_literals_read_float32 "abc"))))

;; readFloat64

(ert-deftest test-literals-negreadfloat64-negpositive ()

  (should (equal (list :just 3.14159) (funcall hydra_lib_literals_read_float64 "3.14159"))))

(ert-deftest test-literals-negreadfloat64-negnegative ()

  (should (equal (list :just -2.71828) (funcall hydra_lib_literals_read_float64 "-2.71828"))))

(ert-deftest test-literals-negreadfloat64-neginvalid ()

  (should (equal (list :nothing) (funcall hydra_lib_literals_read_float64 "abc"))))

;; readBigfloat

(ert-deftest test-literals-negreadbigfloat-negpositive ()

  (should (equal (list :just 3.14) (funcall hydra_lib_literals_read_bigfloat "3.14"))))

(ert-deftest test-literals-negreadbigfloat-neginvalid ()

  (should (equal (list :nothing) (funcall hydra_lib_literals_read_bigfloat "abc"))))

;; readBoolean

(ert-deftest test-literals-negreadboolean-negtrue ()

  (should (equal (list :just t) (funcall hydra_lib_literals_read_boolean "true"))))

(ert-deftest test-literals-negreadboolean-negfalse ()

  (should (equal (list :just nil) (funcall hydra_lib_literals_read_boolean "false"))))

(ert-deftest test-literals-negreadboolean-neginvalid ()

  (should (equal (list :nothing) (funcall hydra_lib_literals_read_boolean "yes"))))

;; readString

(ert-deftest test-literals-negreadstring-negquoted-string ()

  (should (equal (list :just "hello") (funcall hydra_lib_literals_read_string "\"hello\""))))

(ert-deftest test-literals-negreadstring-negempty-quoted ()

  (should (equal (list :just "") (funcall hydra_lib_literals_read_string "\"\""))))

(ert-deftest test-literals-negreadstring-negunquoted ()

  (should (equal (list :nothing) (funcall hydra_lib_literals_read_string "hello"))))

;; stringToBinary

(ert-deftest test-literals-negstringtobinary-negsimple-base64 ()

  (should (equal (list 104 101 108 108 111) (funcall hydra_lib_literals_string_to_binary "aGVsbG8="))))

(ert-deftest test-literals-negstringtobinary-negempty-string ()

  (should (equal (list) (funcall hydra_lib_literals_string_to_binary ""))))

;; binaryToString

(ert-deftest test-literals-negbinarytostring-negsimple-binary ()

  (should (equal "aGVsbG8=" (funcall hydra_lib_literals_binary_to_string (list 104 101 108 108 111)))))

(ert-deftest test-literals-negbinarytostring-negempty-binary ()

  (should (equal "" (funcall hydra_lib_literals_binary_to_string (list)))))
