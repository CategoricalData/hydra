;;; Note: this is an automatically generated file. Do not edit.
;;; hydra.lib.literals primitives

(require 'ert)

;; bigintToInt8

(ert-deftest test-biginttoint8-negpositive ()

  (should (equal 42 (hydra_lib_literals_bigint_to_int8 42))))

(ert-deftest test-biginttoint8-negnegative ()

  (should (equal -42 (hydra_lib_literals_bigint_to_int8 -42))))

;; bigintToInt16

(ert-deftest test-biginttoint16-negpositive ()

  (should (equal 1000 (hydra_lib_literals_bigint_to_int16 1000))))

(ert-deftest test-biginttoint16-negnegative ()

  (should (equal -1000 (hydra_lib_literals_bigint_to_int16 -1000))))

;; bigintToInt32

(ert-deftest test-biginttoint32-negpositive ()

  (should (equal 42 (hydra_lib_literals_bigint_to_int32 42))))

(ert-deftest test-biginttoint32-negnegative ()

  (should (equal -42 (hydra_lib_literals_bigint_to_int32 -42))))

(ert-deftest test-biginttoint32-negzero ()

  (should (equal 0 (hydra_lib_literals_bigint_to_int32 0))))

;; bigintToInt64

(ert-deftest test-biginttoint64-negpositive ()

  (should (equal 1000000 (hydra_lib_literals_bigint_to_int64 1000000))))

(ert-deftest test-biginttoint64-negnegative ()

  (should (equal -1000000 (hydra_lib_literals_bigint_to_int64 -1000000))))

;; bigintToUint8

(ert-deftest test-biginttouint8-negzero ()

  (should (equal 0 (hydra_lib_literals_bigint_to_uint8 0))))

(ert-deftest test-biginttouint8-negtypical-value ()

  (should (equal 100 (hydra_lib_literals_bigint_to_uint8 100))))

;; bigintToUint16

(ert-deftest test-biginttouint16-negzero ()

  (should (equal 0 (hydra_lib_literals_bigint_to_uint16 0))))

(ert-deftest test-biginttouint16-negtypical-value ()

  (should (equal 1000 (hydra_lib_literals_bigint_to_uint16 1000))))

;; bigintToUint32

(ert-deftest test-biginttouint32-negzero ()

  (should (equal 0 (hydra_lib_literals_bigint_to_uint32 0))))

(ert-deftest test-biginttouint32-negtypical-value ()

  (should (equal 100000 (hydra_lib_literals_bigint_to_uint32 100000))))

;; bigintToUint64

(ert-deftest test-biginttouint64-negzero ()

  (should (equal 0 (hydra_lib_literals_bigint_to_uint64 0))))

(ert-deftest test-biginttouint64-negtypical-value ()

  (should (equal 1000000 (hydra_lib_literals_bigint_to_uint64 1000000))))

;; int8ToBigint

(ert-deftest test-int8tobigint-negpositive ()

  (should (equal 42 (hydra_lib_literals_int8_to_bigint 42))))

(ert-deftest test-int8tobigint-negnegative ()

  (should (equal -42 (hydra_lib_literals_int8_to_bigint -42))))

(ert-deftest test-int8tobigint-negmax-value ()

  (should (equal 127 (hydra_lib_literals_int8_to_bigint 127))))

(ert-deftest test-int8tobigint-negmin-value ()

  (should (equal -128 (hydra_lib_literals_int8_to_bigint -128))))

;; int16ToBigint

(ert-deftest test-int16tobigint-negpositive ()

  (should (equal 1000 (hydra_lib_literals_int16_to_bigint 1000))))

(ert-deftest test-int16tobigint-negnegative ()

  (should (equal -1000 (hydra_lib_literals_int16_to_bigint -1000))))

;; int32ToBigint

(ert-deftest test-int32tobigint-negpositive ()

  (should (equal 42 (hydra_lib_literals_int32_to_bigint 42))))

(ert-deftest test-int32tobigint-negnegative ()

  (should (equal -42 (hydra_lib_literals_int32_to_bigint -42))))

(ert-deftest test-int32tobigint-negzero ()

  (should (equal 0 (hydra_lib_literals_int32_to_bigint 0))))

;; int64ToBigint

(ert-deftest test-int64tobigint-negpositive ()

  (should (equal 1000000 (hydra_lib_literals_int64_to_bigint 1000000))))

(ert-deftest test-int64tobigint-negnegative ()

  (should (equal -1000000 (hydra_lib_literals_int64_to_bigint -1000000))))

;; uint8ToBigint

(ert-deftest test-uint8tobigint-negzero ()

  (should (equal 0 (hydra_lib_literals_uint8_to_bigint 0))))

(ert-deftest test-uint8tobigint-negmax-value ()

  (should (equal 255 (hydra_lib_literals_uint8_to_bigint 255))))

;; uint16ToBigint

(ert-deftest test-uint16tobigint-negzero ()

  (should (equal 0 (hydra_lib_literals_uint16_to_bigint 0))))

(ert-deftest test-uint16tobigint-negtypical-value ()

  (should (equal 1000 (hydra_lib_literals_uint16_to_bigint 1000))))

;; uint32ToBigint

(ert-deftest test-uint32tobigint-negzero ()

  (should (equal 0 (hydra_lib_literals_uint32_to_bigint 0))))

(ert-deftest test-uint32tobigint-negtypical-value ()

  (should (equal 100000 (hydra_lib_literals_uint32_to_bigint 100000))))

;; uint64ToBigint

(ert-deftest test-uint64tobigint-negzero ()

  (should (equal 0 (hydra_lib_literals_uint64_to_bigint 0))))

(ert-deftest test-uint64tobigint-negtypical-value ()

  (should (equal 1000000 (hydra_lib_literals_uint64_to_bigint 1000000))))

;; float32ToBigfloat

(ert-deftest test-float32tobigfloat-negpositive ()

  (should (equal 2.5 (hydra_lib_literals_float32_to_bigfloat 2.5))))

(ert-deftest test-float32tobigfloat-negnegative ()

  (should (equal -2.5 (hydra_lib_literals_float32_to_bigfloat -2.5))))

(ert-deftest test-float32tobigfloat-negzero ()

  (should (equal 0.0 (hydra_lib_literals_float32_to_bigfloat 0.0))))

;; float64ToBigfloat

(ert-deftest test-float64tobigfloat-negpositive ()

  (should (equal 3.14159 (hydra_lib_literals_float64_to_bigfloat 3.14159))))

(ert-deftest test-float64tobigfloat-negnegative ()

  (should (equal -2.71828 (hydra_lib_literals_float64_to_bigfloat -2.71828))))

(ert-deftest test-float64tobigfloat-negzero ()

  (should (equal 0.0 (hydra_lib_literals_float64_to_bigfloat 0.0))))

;; bigfloatToFloat32

(ert-deftest test-bigfloattofloat32-negpositive ()

  (should (equal 3.140000104904175 (hydra_lib_literals_bigfloat_to_float32 3.14))))

(ert-deftest test-bigfloattofloat32-negnegative ()

  (should (equal -2.5 (hydra_lib_literals_bigfloat_to_float32 -2.5))))

(ert-deftest test-bigfloattofloat32-negzero ()

  (should (equal 0.0 (hydra_lib_literals_bigfloat_to_float32 0.0))))

;; bigfloatToFloat64

(ert-deftest test-bigfloattofloat64-negpositive ()

  (should (equal 3.14159 (hydra_lib_literals_bigfloat_to_float64 3.14159))))

(ert-deftest test-bigfloattofloat64-negnegative ()

  (should (equal -2.71828 (hydra_lib_literals_bigfloat_to_float64 -2.71828))))

(ert-deftest test-bigfloattofloat64-negzero ()

  (should (equal 0.0 (hydra_lib_literals_bigfloat_to_float64 0.0))))

;; bigintToBigfloat

(ert-deftest test-biginttobigfloat-negpositive ()

  (should (equal 42.0 (hydra_lib_literals_bigint_to_bigfloat 42))))

(ert-deftest test-biginttobigfloat-negnegative ()

  (should (equal -42.0 (hydra_lib_literals_bigint_to_bigfloat -42))))

(ert-deftest test-biginttobigfloat-negzero ()

  (should (equal 0.0 (hydra_lib_literals_bigint_to_bigfloat 0))))

;; bigfloatToBigint

(ert-deftest test-bigfloattobigint-negpositive ()

  (should (equal 43 (hydra_lib_literals_bigfloat_to_bigint 42.7))))

(ert-deftest test-bigfloattobigint-negnegative ()

  (should (equal -43 (hydra_lib_literals_bigfloat_to_bigint -42.7))))

(ert-deftest test-bigfloattobigint-negzero ()

  (should (equal 0 (hydra_lib_literals_bigfloat_to_bigint 0.0))))

(ert-deftest test-bigfloattobigint-neground-down ()

  (should (equal 42 (hydra_lib_literals_bigfloat_to_bigint 42.3))))

(ert-deftest test-bigfloattobigint-neghalf-even-up ()

  (should (equal 42 (hydra_lib_literals_bigfloat_to_bigint 42.5))))

(ert-deftest test-bigfloattobigint-neghalf-even-down ()

  (should (equal 44 (hydra_lib_literals_bigfloat_to_bigint 43.5))))

;; showInt8

(ert-deftest test-showint8-negpositive ()

  (should (equal "42" (hydra_lib_literals_show_int8 42))))

(ert-deftest test-showint8-negnegative ()

  (should (equal "-42" (hydra_lib_literals_show_int8 -42))))

;; showInt16

(ert-deftest test-showint16-negpositive ()

  (should (equal "1000" (hydra_lib_literals_show_int16 1000))))

(ert-deftest test-showint16-negnegative ()

  (should (equal "-1000" (hydra_lib_literals_show_int16 -1000))))

;; showInt32

(ert-deftest test-showint32-negpositive ()

  (should (equal "42" (hydra_lib_literals_show_int32 42))))

(ert-deftest test-showint32-negnegative ()

  (should (equal "-42" (hydra_lib_literals_show_int32 -42))))

(ert-deftest test-showint32-negzero ()

  (should (equal "0" (hydra_lib_literals_show_int32 0))))

;; showInt64

(ert-deftest test-showint64-negpositive ()

  (should (equal "1000000" (hydra_lib_literals_show_int64 1000000))))

(ert-deftest test-showint64-negnegative ()

  (should (equal "-1000000" (hydra_lib_literals_show_int64 -1000000))))

;; showUint8

(ert-deftest test-showuint8-negzero ()

  (should (equal "0" (hydra_lib_literals_show_uint8 0))))

(ert-deftest test-showuint8-negmax-value ()

  (should (equal "255" (hydra_lib_literals_show_uint8 255))))

;; showUint16

(ert-deftest test-showuint16-negzero ()

  (should (equal "0" (hydra_lib_literals_show_uint16 0))))

(ert-deftest test-showuint16-negtypical-value ()

  (should (equal "1000" (hydra_lib_literals_show_uint16 1000))))

;; showUint32

(ert-deftest test-showuint32-negzero ()

  (should (equal "0" (hydra_lib_literals_show_uint32 0))))

(ert-deftest test-showuint32-negtypical-value ()

  (should (equal "100000" (hydra_lib_literals_show_uint32 100000))))

;; showUint64

(ert-deftest test-showuint64-negzero ()

  (should (equal "0" (hydra_lib_literals_show_uint64 0))))

(ert-deftest test-showuint64-negtypical-value ()

  (should (equal "1000000" (hydra_lib_literals_show_uint64 1000000))))

;; showBigint

(ert-deftest test-showbigint-negpositive ()

  (should (equal "42" (hydra_lib_literals_show_bigint 42))))

(ert-deftest test-showbigint-negnegative ()

  (should (equal "-42" (hydra_lib_literals_show_bigint -42))))

(ert-deftest test-showbigint-negzero ()

  (should (equal "0" (hydra_lib_literals_show_bigint 0))))

;; showFloat32

(ert-deftest test-showfloat32-negpositive ()

  (should (equal "3.14" (hydra_lib_literals_show_float32 3.140000104904175))))

(ert-deftest test-showfloat32-negnegative ()

  (should (equal "-2.5" (hydra_lib_literals_show_float32 -2.5))))

(ert-deftest test-showfloat32-negzero ()

  (should (equal "0.0" (hydra_lib_literals_show_float32 0.0))))

(ert-deftest test-showfloat32-negsmall-positive ()

  (should (equal "5.0e-2" (hydra_lib_literals_show_float32 5.000000074505806e-2))))

(ert-deftest test-showfloat32-negsmall-positive-2 ()

  (should (equal "3.0e-2" (hydra_lib_literals_show_float32 2.9999999329447746e-2))))

(ert-deftest test-showfloat32-negvery-small ()

  (should (equal "1.0e-3" (hydra_lib_literals_show_float32 1.0000000474974513e-3))))

(ert-deftest test-showfloat32-negnormal-decimal ()

  (should (equal "0.1" (hydra_lib_literals_show_float32 0.10000000149011612))))

;; showFloat64

(ert-deftest test-showfloat64-negpositive ()

  (should (equal "3.14159" (hydra_lib_literals_show_float64 3.14159))))

(ert-deftest test-showfloat64-negzero ()

  (should (equal "0.0" (hydra_lib_literals_show_float64 0.0))))

(ert-deftest test-showfloat64-negsmall-positive ()

  (should (equal "5.0e-2" (hydra_lib_literals_show_float64 5.0e-2))))

(ert-deftest test-showfloat64-negsmall-positive-2 ()

  (should (equal "3.0e-2" (hydra_lib_literals_show_float64 3.0e-2))))

(ert-deftest test-showfloat64-negvery-small ()

  (should (equal "1.0e-3" (hydra_lib_literals_show_float64 1.0e-3))))

(ert-deftest test-showfloat64-negnormal-decimal ()

  (should (equal "0.1" (hydra_lib_literals_show_float64 0.1))))

;; showBigfloat

(ert-deftest test-showbigfloat-negpositive ()

  (should (equal "3.14" (hydra_lib_literals_show_bigfloat 3.14))))

(ert-deftest test-showbigfloat-negzero ()

  (should (equal "0.0" (hydra_lib_literals_show_bigfloat 0.0))))

(ert-deftest test-showbigfloat-negsmall-positive ()

  (should (equal "5.0e-2" (hydra_lib_literals_show_bigfloat 5.0e-2))))

(ert-deftest test-showbigfloat-negsmall-positive-2 ()

  (should (equal "3.0e-2" (hydra_lib_literals_show_bigfloat 3.0e-2))))

(ert-deftest test-showbigfloat-negvery-small ()

  (should (equal "1.0e-3" (hydra_lib_literals_show_bigfloat 1.0e-3))))

(ert-deftest test-showbigfloat-negnormal-decimal ()

  (should (equal "0.1" (hydra_lib_literals_show_bigfloat 0.1))))

;; showBoolean

(ert-deftest test-showboolean-negtrue ()

  (should (equal "true" (hydra_lib_literals_show_boolean t))))

(ert-deftest test-showboolean-negfalse ()

  (should (equal "false" (hydra_lib_literals_show_boolean nil))))

;; showString

(ert-deftest test-showstring-negsimple ()

  (should (equal "\"hello\"" (hydra_lib_literals_show_string "hello"))))

(ert-deftest test-showstring-negempty ()

  (should (equal "\"\"" (hydra_lib_literals_show_string ""))))

(ert-deftest test-showstring-neglatin-accented ()

  (should (equal "\"caf\\233\"" (hydra_lib_literals_show_string "café"))))

(ert-deftest test-showstring-neggreek-lambda ()

  (should (equal "\"\\955\"" (hydra_lib_literals_show_string "λ"))))

(ert-deftest test-showstring-negmixed-ascii-and-non-negascii ()

  (should (equal "\"A\\233B\"" (hydra_lib_literals_show_string "AéB"))))

(ert-deftest test-showstring-negtab ()

  (should (equal "\"\\t\"" (hydra_lib_literals_show_string "\t"))))

(ert-deftest test-showstring-negnewline ()

  (should (equal "\"\\n\"" (hydra_lib_literals_show_string "\n"))))

(ert-deftest test-showstring-negcarriage-return ()

  (should (equal "\"\\r\"" (hydra_lib_literals_show_string "\r"))))

(ert-deftest test-showstring-negbackslash ()

  (should (equal "\"\\\\\"" (hydra_lib_literals_show_string "\\"))))

(ert-deftest test-showstring-negdouble-quote ()

  (should (equal "\"\\\"\"" (hydra_lib_literals_show_string "\""))))

(ert-deftest test-showstring-negnull ()

  (should (equal "\"\\NUL\"" (hydra_lib_literals_show_string " "))))

(ert-deftest test-showstring-negbell ()

  (should (equal "\"\\a\"" (hydra_lib_literals_show_string ""))))

(ert-deftest test-showstring-negbackspace ()

  (should (equal "\"\\b\"" (hydra_lib_literals_show_string ""))))

(ert-deftest test-showstring-negform-feed ()

  (should (equal "\"\\f\"" (hydra_lib_literals_show_string ""))))

(ert-deftest test-showstring-negvertical-tab ()

  (should (equal "\"\\v\"" (hydra_lib_literals_show_string ""))))

(ert-deftest test-showstring-negdelete ()

  (should (equal "\"\\DEL\"" (hydra_lib_literals_show_string ""))))

;; readInt8

(ert-deftest test-readint8-negpositive ()

  (should (equal 42 (hydra_lib_literals_read_int8 "42"))))

(ert-deftest test-readint8-negnegative ()

  (should (equal -42 (hydra_lib_literals_read_int8 "-42"))))

(ert-deftest test-readint8-negmax-value ()

  (should (equal 127 (hydra_lib_literals_read_int8 "127"))))

(ert-deftest test-readint8-negmin-value ()

  (should (equal -128 (hydra_lib_literals_read_int8 "-128"))))

(ert-deftest test-readint8-neginvalid ()

  (should (equal nil (hydra_lib_literals_read_int8 "abc"))))

(ert-deftest test-readint8-negoverflow ()

  (should (equal nil (hydra_lib_literals_read_int8 "128"))))

;; readInt16

(ert-deftest test-readint16-negpositive ()

  (should (equal 1000 (hydra_lib_literals_read_int16 "1000"))))

(ert-deftest test-readint16-negnegative ()

  (should (equal -1000 (hydra_lib_literals_read_int16 "-1000"))))

(ert-deftest test-readint16-neginvalid ()

  (should (equal nil (hydra_lib_literals_read_int16 "abc"))))

;; readInt32

(ert-deftest test-readint32-negpositive ()

  (should (equal 42 (hydra_lib_literals_read_int32 "42"))))

(ert-deftest test-readint32-negnegative ()

  (should (equal -42 (hydra_lib_literals_read_int32 "-42"))))

(ert-deftest test-readint32-neginvalid ()

  (should (equal nil (hydra_lib_literals_read_int32 "abc"))))

;; readInt64

(ert-deftest test-readint64-negpositive ()

  (should (equal 1000000 (hydra_lib_literals_read_int64 "1000000"))))

(ert-deftest test-readint64-negnegative ()

  (should (equal -1000000 (hydra_lib_literals_read_int64 "-1000000"))))

(ert-deftest test-readint64-neginvalid ()

  (should (equal nil (hydra_lib_literals_read_int64 "abc"))))

;; readUint8

(ert-deftest test-readuint8-negzero ()

  (should (equal 0 (hydra_lib_literals_read_uint8 "0"))))

(ert-deftest test-readuint8-negtypical ()

  (should (equal 100 (hydra_lib_literals_read_uint8 "100"))))

(ert-deftest test-readuint8-negmax-value ()

  (should (equal 255 (hydra_lib_literals_read_uint8 "255"))))

(ert-deftest test-readuint8-neginvalid ()

  (should (equal nil (hydra_lib_literals_read_uint8 "abc"))))

(ert-deftest test-readuint8-negnegative ()

  (should (equal nil (hydra_lib_literals_read_uint8 "-1"))))

;; readUint16

(ert-deftest test-readuint16-negzero ()

  (should (equal 0 (hydra_lib_literals_read_uint16 "0"))))

(ert-deftest test-readuint16-negtypical ()

  (should (equal 1000 (hydra_lib_literals_read_uint16 "1000"))))

(ert-deftest test-readuint16-neginvalid ()

  (should (equal nil (hydra_lib_literals_read_uint16 "abc"))))

(ert-deftest test-readuint16-negnegative ()

  (should (equal nil (hydra_lib_literals_read_uint16 "-1"))))

;; readUint32

(ert-deftest test-readuint32-negzero ()

  (should (equal 0 (hydra_lib_literals_read_uint32 "0"))))

(ert-deftest test-readuint32-negtypical ()

  (should (equal 100000 (hydra_lib_literals_read_uint32 "100000"))))

(ert-deftest test-readuint32-neginvalid ()

  (should (equal nil (hydra_lib_literals_read_uint32 "abc"))))

(ert-deftest test-readuint32-negnegative ()

  (should (equal nil (hydra_lib_literals_read_uint32 "-1"))))

;; readUint64

(ert-deftest test-readuint64-negzero ()

  (should (equal 0 (hydra_lib_literals_read_uint64 "0"))))

(ert-deftest test-readuint64-negtypical ()

  (should (equal 1000000 (hydra_lib_literals_read_uint64 "1000000"))))

(ert-deftest test-readuint64-neginvalid ()

  (should (equal nil (hydra_lib_literals_read_uint64 "abc"))))

(ert-deftest test-readuint64-negnegative ()

  (should (equal nil (hydra_lib_literals_read_uint64 "-1"))))

;; readBigint

(ert-deftest test-readbigint-negpositive ()

  (should (equal 42 (hydra_lib_literals_read_bigint "42"))))

(ert-deftest test-readbigint-negnegative ()

  (should (equal -42 (hydra_lib_literals_read_bigint "-42"))))

(ert-deftest test-readbigint-negzero ()

  (should (equal 0 (hydra_lib_literals_read_bigint "0"))))

(ert-deftest test-readbigint-neglarge ()

  (should (equal 123456789012345678901234567890 (hydra_lib_literals_read_bigint "123456789012345678901234567890"))))

(ert-deftest test-readbigint-neginvalid ()

  (should (equal nil (hydra_lib_literals_read_bigint "abc"))))

;; readFloat32

(ert-deftest test-readfloat32-negpositive ()

  (should (equal 3.140000104904175 (hydra_lib_literals_read_float32 "3.14"))))

(ert-deftest test-readfloat32-negnegative ()

  (should (equal -2.5 (hydra_lib_literals_read_float32 "-2.5"))))

(ert-deftest test-readfloat32-neginvalid ()

  (should (equal nil (hydra_lib_literals_read_float32 "abc"))))

;; readFloat64

(ert-deftest test-readfloat64-negpositive ()

  (should (equal 3.14159 (hydra_lib_literals_read_float64 "3.14159"))))

(ert-deftest test-readfloat64-negnegative ()

  (should (equal -2.71828 (hydra_lib_literals_read_float64 "-2.71828"))))

(ert-deftest test-readfloat64-neginvalid ()

  (should (equal nil (hydra_lib_literals_read_float64 "abc"))))

;; readBigfloat

(ert-deftest test-readbigfloat-negpositive ()

  (should (equal 3.14 (hydra_lib_literals_read_bigfloat "3.14"))))

(ert-deftest test-readbigfloat-neginvalid ()

  (should (equal nil (hydra_lib_literals_read_bigfloat "abc"))))

;; readBoolean

(ert-deftest test-readboolean-negtrue ()

  (should (equal t (hydra_lib_literals_read_boolean "true"))))

(ert-deftest test-readboolean-negfalse ()

  (should (equal nil (hydra_lib_literals_read_boolean "false"))))

(ert-deftest test-readboolean-neginvalid ()

  (should (equal nil (hydra_lib_literals_read_boolean "yes"))))

;; readString

(ert-deftest test-readstring-negquoted-string ()

  (should (equal "hello" (hydra_lib_literals_read_string "\"hello\""))))

(ert-deftest test-readstring-negempty-quoted ()

  (should (equal "" (hydra_lib_literals_read_string "\"\""))))

(ert-deftest test-readstring-negunquoted ()

  (should (equal nil (hydra_lib_literals_read_string "hello"))))

;; stringToBinary

(ert-deftest test-stringtobinary-negsimple-base64 ()

  (should (equal [104 101 108 108 111] (hydra_lib_literals_string_to_binary "aGVsbG8="))))

(ert-deftest test-stringtobinary-negempty-string ()

  (should (equal [] (hydra_lib_literals_string_to_binary ""))))

;; binaryToString

(ert-deftest test-binarytostring-negsimple-binary ()

  (should (equal "aGVsbG8=" (hydra_lib_literals_binary_to_string [104 101 108 108 111]))))

(ert-deftest test-binarytostring-negempty-binary ()

  (should (equal "" (hydra_lib_literals_binary_to_string []))))
