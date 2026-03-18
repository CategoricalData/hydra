;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.literals primitives

;; bigintToInt8

(defun test-biginttoint8-negpositive ()

  (assert (equal 42 (hydra_lib_literals_bigint_to_int8 42))))

(defun test-biginttoint8-negnegative ()

  (assert (equal -42 (hydra_lib_literals_bigint_to_int8 -42))))

;; bigintToInt16

(defun test-biginttoint16-negpositive ()

  (assert (equal 1000 (hydra_lib_literals_bigint_to_int16 1000))))

(defun test-biginttoint16-negnegative ()

  (assert (equal -1000 (hydra_lib_literals_bigint_to_int16 -1000))))

;; bigintToInt32

(defun test-biginttoint32-negpositive ()

  (assert (equal 42 (hydra_lib_literals_bigint_to_int32 42))))

(defun test-biginttoint32-negnegative ()

  (assert (equal -42 (hydra_lib_literals_bigint_to_int32 -42))))

(defun test-biginttoint32-negzero ()

  (assert (equal 0 (hydra_lib_literals_bigint_to_int32 0))))

;; bigintToInt64

(defun test-biginttoint64-negpositive ()

  (assert (equal 1000000 (hydra_lib_literals_bigint_to_int64 1000000))))

(defun test-biginttoint64-negnegative ()

  (assert (equal -1000000 (hydra_lib_literals_bigint_to_int64 -1000000))))

;; bigintToUint8

(defun test-biginttouint8-negzero ()

  (assert (equal 0 (hydra_lib_literals_bigint_to_uint8 0))))

(defun test-biginttouint8-negtypical-value ()

  (assert (equal 100 (hydra_lib_literals_bigint_to_uint8 100))))

;; bigintToUint16

(defun test-biginttouint16-negzero ()

  (assert (equal 0 (hydra_lib_literals_bigint_to_uint16 0))))

(defun test-biginttouint16-negtypical-value ()

  (assert (equal 1000 (hydra_lib_literals_bigint_to_uint16 1000))))

;; bigintToUint32

(defun test-biginttouint32-negzero ()

  (assert (equal 0 (hydra_lib_literals_bigint_to_uint32 0))))

(defun test-biginttouint32-negtypical-value ()

  (assert (equal 100000 (hydra_lib_literals_bigint_to_uint32 100000))))

;; bigintToUint64

(defun test-biginttouint64-negzero ()

  (assert (equal 0 (hydra_lib_literals_bigint_to_uint64 0))))

(defun test-biginttouint64-negtypical-value ()

  (assert (equal 1000000 (hydra_lib_literals_bigint_to_uint64 1000000))))

;; int8ToBigint

(defun test-int8tobigint-negpositive ()

  (assert (equal 42 (hydra_lib_literals_int8_to_bigint 42))))

(defun test-int8tobigint-negnegative ()

  (assert (equal -42 (hydra_lib_literals_int8_to_bigint -42))))

(defun test-int8tobigint-negmax-value ()

  (assert (equal 127 (hydra_lib_literals_int8_to_bigint 127))))

(defun test-int8tobigint-negmin-value ()

  (assert (equal -128 (hydra_lib_literals_int8_to_bigint -128))))

;; int16ToBigint

(defun test-int16tobigint-negpositive ()

  (assert (equal 1000 (hydra_lib_literals_int16_to_bigint 1000))))

(defun test-int16tobigint-negnegative ()

  (assert (equal -1000 (hydra_lib_literals_int16_to_bigint -1000))))

;; int32ToBigint

(defun test-int32tobigint-negpositive ()

  (assert (equal 42 (hydra_lib_literals_int32_to_bigint 42))))

(defun test-int32tobigint-negnegative ()

  (assert (equal -42 (hydra_lib_literals_int32_to_bigint -42))))

(defun test-int32tobigint-negzero ()

  (assert (equal 0 (hydra_lib_literals_int32_to_bigint 0))))

;; int64ToBigint

(defun test-int64tobigint-negpositive ()

  (assert (equal 1000000 (hydra_lib_literals_int64_to_bigint 1000000))))

(defun test-int64tobigint-negnegative ()

  (assert (equal -1000000 (hydra_lib_literals_int64_to_bigint -1000000))))

;; uint8ToBigint

(defun test-uint8tobigint-negzero ()

  (assert (equal 0 (hydra_lib_literals_uint8_to_bigint 0))))

(defun test-uint8tobigint-negmax-value ()

  (assert (equal 255 (hydra_lib_literals_uint8_to_bigint 255))))

;; uint16ToBigint

(defun test-uint16tobigint-negzero ()

  (assert (equal 0 (hydra_lib_literals_uint16_to_bigint 0))))

(defun test-uint16tobigint-negtypical-value ()

  (assert (equal 1000 (hydra_lib_literals_uint16_to_bigint 1000))))

;; uint32ToBigint

(defun test-uint32tobigint-negzero ()

  (assert (equal 0 (hydra_lib_literals_uint32_to_bigint 0))))

(defun test-uint32tobigint-negtypical-value ()

  (assert (equal 100000 (hydra_lib_literals_uint32_to_bigint 100000))))

;; uint64ToBigint

(defun test-uint64tobigint-negzero ()

  (assert (equal 0 (hydra_lib_literals_uint64_to_bigint 0))))

(defun test-uint64tobigint-negtypical-value ()

  (assert (equal 1000000 (hydra_lib_literals_uint64_to_bigint 1000000))))

;; float32ToBigfloat

(defun test-float32tobigfloat-negpositive ()

  (assert (equal 2.5 (hydra_lib_literals_float32_to_bigfloat 2.5))))

(defun test-float32tobigfloat-negnegative ()

  (assert (equal -2.5 (hydra_lib_literals_float32_to_bigfloat -2.5))))

(defun test-float32tobigfloat-negzero ()

  (assert (equal 0.0 (hydra_lib_literals_float32_to_bigfloat 0.0))))

;; float64ToBigfloat

(defun test-float64tobigfloat-negpositive ()

  (assert (equal 3.14159 (hydra_lib_literals_float64_to_bigfloat 3.14159))))

(defun test-float64tobigfloat-negnegative ()

  (assert (equal -2.71828 (hydra_lib_literals_float64_to_bigfloat -2.71828))))

(defun test-float64tobigfloat-negzero ()

  (assert (equal 0.0 (hydra_lib_literals_float64_to_bigfloat 0.0))))

;; bigfloatToFloat32

(defun test-bigfloattofloat32-negpositive ()

  (assert (equal 3.140000104904175 (hydra_lib_literals_bigfloat_to_float32 3.14))))

(defun test-bigfloattofloat32-negnegative ()

  (assert (equal -2.5 (hydra_lib_literals_bigfloat_to_float32 -2.5))))

(defun test-bigfloattofloat32-negzero ()

  (assert (equal 0.0 (hydra_lib_literals_bigfloat_to_float32 0.0))))

;; bigfloatToFloat64

(defun test-bigfloattofloat64-negpositive ()

  (assert (equal 3.14159 (hydra_lib_literals_bigfloat_to_float64 3.14159))))

(defun test-bigfloattofloat64-negnegative ()

  (assert (equal -2.71828 (hydra_lib_literals_bigfloat_to_float64 -2.71828))))

(defun test-bigfloattofloat64-negzero ()

  (assert (equal 0.0 (hydra_lib_literals_bigfloat_to_float64 0.0))))

;; bigintToBigfloat

(defun test-biginttobigfloat-negpositive ()

  (assert (equal 42.0 (hydra_lib_literals_bigint_to_bigfloat 42))))

(defun test-biginttobigfloat-negnegative ()

  (assert (equal -42.0 (hydra_lib_literals_bigint_to_bigfloat -42))))

(defun test-biginttobigfloat-negzero ()

  (assert (equal 0.0 (hydra_lib_literals_bigint_to_bigfloat 0))))

;; bigfloatToBigint

(defun test-bigfloattobigint-negpositive ()

  (assert (equal 43 (hydra_lib_literals_bigfloat_to_bigint 42.7))))

(defun test-bigfloattobigint-negnegative ()

  (assert (equal -43 (hydra_lib_literals_bigfloat_to_bigint -42.7))))

(defun test-bigfloattobigint-negzero ()

  (assert (equal 0 (hydra_lib_literals_bigfloat_to_bigint 0.0))))

(defun test-bigfloattobigint-neground-down ()

  (assert (equal 42 (hydra_lib_literals_bigfloat_to_bigint 42.3))))

(defun test-bigfloattobigint-neghalf-even-up ()

  (assert (equal 42 (hydra_lib_literals_bigfloat_to_bigint 42.5))))

(defun test-bigfloattobigint-neghalf-even-down ()

  (assert (equal 44 (hydra_lib_literals_bigfloat_to_bigint 43.5))))

;; showInt8

(defun test-showint8-negpositive ()

  (assert (equal "42" (hydra_lib_literals_show_int8 42))))

(defun test-showint8-negnegative ()

  (assert (equal "-42" (hydra_lib_literals_show_int8 -42))))

;; showInt16

(defun test-showint16-negpositive ()

  (assert (equal "1000" (hydra_lib_literals_show_int16 1000))))

(defun test-showint16-negnegative ()

  (assert (equal "-1000" (hydra_lib_literals_show_int16 -1000))))

;; showInt32

(defun test-showint32-negpositive ()

  (assert (equal "42" (hydra_lib_literals_show_int32 42))))

(defun test-showint32-negnegative ()

  (assert (equal "-42" (hydra_lib_literals_show_int32 -42))))

(defun test-showint32-negzero ()

  (assert (equal "0" (hydra_lib_literals_show_int32 0))))

;; showInt64

(defun test-showint64-negpositive ()

  (assert (equal "1000000" (hydra_lib_literals_show_int64 1000000))))

(defun test-showint64-negnegative ()

  (assert (equal "-1000000" (hydra_lib_literals_show_int64 -1000000))))

;; showUint8

(defun test-showuint8-negzero ()

  (assert (equal "0" (hydra_lib_literals_show_uint8 0))))

(defun test-showuint8-negmax-value ()

  (assert (equal "255" (hydra_lib_literals_show_uint8 255))))

;; showUint16

(defun test-showuint16-negzero ()

  (assert (equal "0" (hydra_lib_literals_show_uint16 0))))

(defun test-showuint16-negtypical-value ()

  (assert (equal "1000" (hydra_lib_literals_show_uint16 1000))))

;; showUint32

(defun test-showuint32-negzero ()

  (assert (equal "0" (hydra_lib_literals_show_uint32 0))))

(defun test-showuint32-negtypical-value ()

  (assert (equal "100000" (hydra_lib_literals_show_uint32 100000))))

;; showUint64

(defun test-showuint64-negzero ()

  (assert (equal "0" (hydra_lib_literals_show_uint64 0))))

(defun test-showuint64-negtypical-value ()

  (assert (equal "1000000" (hydra_lib_literals_show_uint64 1000000))))

;; showBigint

(defun test-showbigint-negpositive ()

  (assert (equal "42" (hydra_lib_literals_show_bigint 42))))

(defun test-showbigint-negnegative ()

  (assert (equal "-42" (hydra_lib_literals_show_bigint -42))))

(defun test-showbigint-negzero ()

  (assert (equal "0" (hydra_lib_literals_show_bigint 0))))

;; showFloat32

(defun test-showfloat32-negpositive ()

  (assert (equal "3.14" (hydra_lib_literals_show_float32 3.140000104904175))))

(defun test-showfloat32-negnegative ()

  (assert (equal "-2.5" (hydra_lib_literals_show_float32 -2.5))))

(defun test-showfloat32-negzero ()

  (assert (equal "0.0" (hydra_lib_literals_show_float32 0.0))))

(defun test-showfloat32-negsmall-positive ()

  (assert (equal "5.0e-2" (hydra_lib_literals_show_float32 5.000000074505806e-2))))

(defun test-showfloat32-negsmall-positive-2 ()

  (assert (equal "3.0e-2" (hydra_lib_literals_show_float32 2.9999999329447746e-2))))

(defun test-showfloat32-negvery-small ()

  (assert (equal "1.0e-3" (hydra_lib_literals_show_float32 1.0000000474974513e-3))))

(defun test-showfloat32-negnormal-decimal ()

  (assert (equal "0.1" (hydra_lib_literals_show_float32 0.10000000149011612))))

;; showFloat64

(defun test-showfloat64-negpositive ()

  (assert (equal "3.14159" (hydra_lib_literals_show_float64 3.14159))))

(defun test-showfloat64-negzero ()

  (assert (equal "0.0" (hydra_lib_literals_show_float64 0.0))))

(defun test-showfloat64-negsmall-positive ()

  (assert (equal "5.0e-2" (hydra_lib_literals_show_float64 5.0e-2))))

(defun test-showfloat64-negsmall-positive-2 ()

  (assert (equal "3.0e-2" (hydra_lib_literals_show_float64 3.0e-2))))

(defun test-showfloat64-negvery-small ()

  (assert (equal "1.0e-3" (hydra_lib_literals_show_float64 1.0e-3))))

(defun test-showfloat64-negnormal-decimal ()

  (assert (equal "0.1" (hydra_lib_literals_show_float64 0.1))))

;; showBigfloat

(defun test-showbigfloat-negpositive ()

  (assert (equal "3.14" (hydra_lib_literals_show_bigfloat 3.14))))

(defun test-showbigfloat-negzero ()

  (assert (equal "0.0" (hydra_lib_literals_show_bigfloat 0.0))))

(defun test-showbigfloat-negsmall-positive ()

  (assert (equal "5.0e-2" (hydra_lib_literals_show_bigfloat 5.0e-2))))

(defun test-showbigfloat-negsmall-positive-2 ()

  (assert (equal "3.0e-2" (hydra_lib_literals_show_bigfloat 3.0e-2))))

(defun test-showbigfloat-negvery-small ()

  (assert (equal "1.0e-3" (hydra_lib_literals_show_bigfloat 1.0e-3))))

(defun test-showbigfloat-negnormal-decimal ()

  (assert (equal "0.1" (hydra_lib_literals_show_bigfloat 0.1))))

;; showBoolean

(defun test-showboolean-negtrue ()

  (assert (equal "true" (hydra_lib_literals_show_boolean cl:t))))

(defun test-showboolean-negfalse ()

  (assert (equal "false" (hydra_lib_literals_show_boolean cl:nil))))

;; showString

(defun test-showstring-negsimple ()

  (assert (equal "\"hello\"" (hydra_lib_literals_show_string "hello"))))

(defun test-showstring-negempty ()

  (assert (equal "\"\"" (hydra_lib_literals_show_string ""))))

(defun test-showstring-neglatin-accented ()

  (assert (equal "\"caf\\233\"" (hydra_lib_literals_show_string "café"))))

(defun test-showstring-neggreek-lambda ()

  (assert (equal "\"\\955\"" (hydra_lib_literals_show_string "λ"))))

(defun test-showstring-negmixed-ascii-and-non-negascii ()

  (assert (equal "\"A\\233B\"" (hydra_lib_literals_show_string "AéB"))))

(defun test-showstring-negtab ()

  (assert (equal "\"\\t\"" (hydra_lib_literals_show_string "	"))))

(defun test-showstring-negnewline ()

  (assert (equal "\"\\n\"" (hydra_lib_literals_show_string "
"))))

(defun test-showstring-negcarriage-return ()

  (assert (equal "\"\\r\"" (hydra_lib_literals_show_string ""))))

(defun test-showstring-negbackslash ()

  (assert (equal "\"\\\\\"" (hydra_lib_literals_show_string "\\"))))

(defun test-showstring-negdouble-quote ()

  (assert (equal "\"\\\"\"" (hydra_lib_literals_show_string "\""))))

(defun test-showstring-negnull ()

  (assert (equal "\"\\NUL\"" (hydra_lib_literals_show_string " "))))

(defun test-showstring-negbell ()

  (assert (equal "\"\\a\"" (hydra_lib_literals_show_string ""))))

(defun test-showstring-negbackspace ()

  (assert (equal "\"\\b\"" (hydra_lib_literals_show_string ""))))

(defun test-showstring-negform-feed ()

  (assert (equal "\"\\f\"" (hydra_lib_literals_show_string ""))))

(defun test-showstring-negvertical-tab ()

  (assert (equal "\"\\v\"" (hydra_lib_literals_show_string ""))))

(defun test-showstring-negdelete ()

  (assert (equal "\"\\DEL\"" (hydra_lib_literals_show_string ""))))

;; readInt8

(defun test-readint8-negpositive ()

  (assert (equal (list :just 42) (hydra_lib_literals_read_int8 "42"))))

(defun test-readint8-negnegative ()

  (assert (equal (list :just -42) (hydra_lib_literals_read_int8 "-42"))))

(defun test-readint8-negmax-value ()

  (assert (equal (list :just 127) (hydra_lib_literals_read_int8 "127"))))

(defun test-readint8-negmin-value ()

  (assert (equal (list :just -128) (hydra_lib_literals_read_int8 "-128"))))

(defun test-readint8-neginvalid ()

  (assert (equal (list :nothing) (hydra_lib_literals_read_int8 "abc"))))

(defun test-readint8-negoverflow ()

  (assert (equal (list :nothing) (hydra_lib_literals_read_int8 "128"))))

;; readInt16

(defun test-readint16-negpositive ()

  (assert (equal (list :just 1000) (hydra_lib_literals_read_int16 "1000"))))

(defun test-readint16-negnegative ()

  (assert (equal (list :just -1000) (hydra_lib_literals_read_int16 "-1000"))))

(defun test-readint16-neginvalid ()

  (assert (equal (list :nothing) (hydra_lib_literals_read_int16 "abc"))))

;; readInt32

(defun test-readint32-negpositive ()

  (assert (equal (list :just 42) (hydra_lib_literals_read_int32 "42"))))

(defun test-readint32-negnegative ()

  (assert (equal (list :just -42) (hydra_lib_literals_read_int32 "-42"))))

(defun test-readint32-neginvalid ()

  (assert (equal (list :nothing) (hydra_lib_literals_read_int32 "abc"))))

;; readInt64

(defun test-readint64-negpositive ()

  (assert (equal (list :just 1000000) (hydra_lib_literals_read_int64 "1000000"))))

(defun test-readint64-negnegative ()

  (assert (equal (list :just -1000000) (hydra_lib_literals_read_int64 "-1000000"))))

(defun test-readint64-neginvalid ()

  (assert (equal (list :nothing) (hydra_lib_literals_read_int64 "abc"))))

;; readUint8

(defun test-readuint8-negzero ()

  (assert (equal (list :just 0) (hydra_lib_literals_read_uint8 "0"))))

(defun test-readuint8-negtypical ()

  (assert (equal (list :just 100) (hydra_lib_literals_read_uint8 "100"))))

(defun test-readuint8-negmax-value ()

  (assert (equal (list :just 255) (hydra_lib_literals_read_uint8 "255"))))

(defun test-readuint8-neginvalid ()

  (assert (equal (list :nothing) (hydra_lib_literals_read_uint8 "abc"))))

(defun test-readuint8-negnegative ()

  (assert (equal (list :nothing) (hydra_lib_literals_read_uint8 "-1"))))

;; readUint16

(defun test-readuint16-negzero ()

  (assert (equal (list :just 0) (hydra_lib_literals_read_uint16 "0"))))

(defun test-readuint16-negtypical ()

  (assert (equal (list :just 1000) (hydra_lib_literals_read_uint16 "1000"))))

(defun test-readuint16-neginvalid ()

  (assert (equal (list :nothing) (hydra_lib_literals_read_uint16 "abc"))))

(defun test-readuint16-negnegative ()

  (assert (equal (list :nothing) (hydra_lib_literals_read_uint16 "-1"))))

;; readUint32

(defun test-readuint32-negzero ()

  (assert (equal (list :just 0) (hydra_lib_literals_read_uint32 "0"))))

(defun test-readuint32-negtypical ()

  (assert (equal (list :just 100000) (hydra_lib_literals_read_uint32 "100000"))))

(defun test-readuint32-neginvalid ()

  (assert (equal (list :nothing) (hydra_lib_literals_read_uint32 "abc"))))

(defun test-readuint32-negnegative ()

  (assert (equal (list :nothing) (hydra_lib_literals_read_uint32 "-1"))))

;; readUint64

(defun test-readuint64-negzero ()

  (assert (equal (list :just 0) (hydra_lib_literals_read_uint64 "0"))))

(defun test-readuint64-negtypical ()

  (assert (equal (list :just 1000000) (hydra_lib_literals_read_uint64 "1000000"))))

(defun test-readuint64-neginvalid ()

  (assert (equal (list :nothing) (hydra_lib_literals_read_uint64 "abc"))))

(defun test-readuint64-negnegative ()

  (assert (equal (list :nothing) (hydra_lib_literals_read_uint64 "-1"))))

;; readBigint

(defun test-readbigint-negpositive ()

  (assert (equal (list :just 42) (hydra_lib_literals_read_bigint "42"))))

(defun test-readbigint-negnegative ()

  (assert (equal (list :just -42) (hydra_lib_literals_read_bigint "-42"))))

(defun test-readbigint-negzero ()

  (assert (equal (list :just 0) (hydra_lib_literals_read_bigint "0"))))

(defun test-readbigint-neglarge ()

  (assert (equal (list :just 123456789012345678901234567890) (hydra_lib_literals_read_bigint "123456789012345678901234567890"))))

(defun test-readbigint-neginvalid ()

  (assert (equal (list :nothing) (hydra_lib_literals_read_bigint "abc"))))

;; readFloat32

(defun test-readfloat32-negpositive ()

  (assert (equal (list :just 3.140000104904175) (hydra_lib_literals_read_float32 "3.14"))))

(defun test-readfloat32-negnegative ()

  (assert (equal (list :just -2.5) (hydra_lib_literals_read_float32 "-2.5"))))

(defun test-readfloat32-neginvalid ()

  (assert (equal (list :nothing) (hydra_lib_literals_read_float32 "abc"))))

;; readFloat64

(defun test-readfloat64-negpositive ()

  (assert (equal (list :just 3.14159) (hydra_lib_literals_read_float64 "3.14159"))))

(defun test-readfloat64-negnegative ()

  (assert (equal (list :just -2.71828) (hydra_lib_literals_read_float64 "-2.71828"))))

(defun test-readfloat64-neginvalid ()

  (assert (equal (list :nothing) (hydra_lib_literals_read_float64 "abc"))))

;; readBigfloat

(defun test-readbigfloat-negpositive ()

  (assert (equal (list :just 3.14) (hydra_lib_literals_read_bigfloat "3.14"))))

(defun test-readbigfloat-neginvalid ()

  (assert (equal (list :nothing) (hydra_lib_literals_read_bigfloat "abc"))))

;; readBoolean

(defun test-readboolean-negtrue ()

  (assert (equal (list :just cl:t) (hydra_lib_literals_read_boolean "true"))))

(defun test-readboolean-negfalse ()

  (assert (equal (list :just cl:nil) (hydra_lib_literals_read_boolean "false"))))

(defun test-readboolean-neginvalid ()

  (assert (equal (list :nothing) (hydra_lib_literals_read_boolean "yes"))))

;; readString

(defun test-readstring-negquoted-string ()

  (assert (equal (list :just "hello") (hydra_lib_literals_read_string "\"hello\""))))

(defun test-readstring-negempty-quoted ()

  (assert (equal (list :just "") (hydra_lib_literals_read_string "\"\""))))

(defun test-readstring-negunquoted ()

  (assert (equal (list :nothing) (hydra_lib_literals_read_string "hello"))))

;; stringToBinary

(defun test-stringtobinary-negsimple-base64 ()

  (assert (equal #(104 101 108 108 111) (hydra_lib_literals_string_to_binary "aGVsbG8="))))

(defun test-stringtobinary-negempty-string ()

  (assert (equal #() (hydra_lib_literals_string_to_binary ""))))

;; binaryToString

(defun test-binarytostring-negsimple-binary ()

  (assert (equal "aGVsbG8=" (hydra_lib_literals_binary_to_string #(104 101 108 108 111)))))

(defun test-binarytostring-negempty-binary ()

  (assert (equal "" (hydra_lib_literals_binary_to_string #()))))
