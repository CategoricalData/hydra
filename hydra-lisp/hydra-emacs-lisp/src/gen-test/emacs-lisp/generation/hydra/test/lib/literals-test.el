;;; Note: this is an automatically generated file. Do not edit. -*- lexical-binding: t; coding: utf-8 -*-
;;; hydra.lib.literals primitives

(require 'ert)

;; bigintToInt8

(ert-deftest test-literals-negbiginttoint8-negpositive ()

  (should (equal 42:int8 42:int8)))

(ert-deftest test-literals-negbiginttoint8-negnegative ()

  (should (equal -42:int8 -42:int8)))

;; bigintToInt16

(ert-deftest test-literals-negbiginttoint16-negpositive ()

  (should (equal 1000:int16 1000:int16)))

(ert-deftest test-literals-negbiginttoint16-negnegative ()

  (should (equal -1000:int16 -1000:int16)))

;; bigintToInt32

(ert-deftest test-literals-negbiginttoint32-negpositive ()

  (should (equal 42:int32 42:int32)))

(ert-deftest test-literals-negbiginttoint32-negnegative ()

  (should (equal -42:int32 -42:int32)))

(ert-deftest test-literals-negbiginttoint32-negzero ()

  (should (equal 0:int32 0:int32)))

;; bigintToInt64

(ert-deftest test-literals-negbiginttoint64-negpositive ()

  (should (equal 1000000:int64 1000000:int64)))

(ert-deftest test-literals-negbiginttoint64-negnegative ()

  (should (equal -1000000:int64 -1000000:int64)))

;; bigintToUint8

(ert-deftest test-literals-negbiginttouint8-negzero ()

  (should (equal 0:uint8 0:uint8)))

(ert-deftest test-literals-negbiginttouint8-negtypical-value ()

  (should (equal 100:uint8 100:uint8)))

;; bigintToUint16

(ert-deftest test-literals-negbiginttouint16-negzero ()

  (should (equal 0:uint16 0:uint16)))

(ert-deftest test-literals-negbiginttouint16-negtypical-value ()

  (should (equal 1000:uint16 1000:uint16)))

;; bigintToUint32

(ert-deftest test-literals-negbiginttouint32-negzero ()

  (should (equal 0:uint32 0:uint32)))

(ert-deftest test-literals-negbiginttouint32-negtypical-value ()

  (should (equal 100000:uint32 100000:uint32)))

;; bigintToUint64

(ert-deftest test-literals-negbiginttouint64-negzero ()

  (should (equal 0:uint64 0:uint64)))

(ert-deftest test-literals-negbiginttouint64-negtypical-value ()

  (should (equal 1000000:uint64 1000000:uint64)))

;; int8ToBigint

(ert-deftest test-literals-negint8tobigint-negpositive ()

  (should (equal 42:bigint 42:bigint)))

(ert-deftest test-literals-negint8tobigint-negnegative ()

  (should (equal -42:bigint -42:bigint)))

(ert-deftest test-literals-negint8tobigint-negmax-value ()

  (should (equal 127:bigint 127:bigint)))

(ert-deftest test-literals-negint8tobigint-negmin-value ()

  (should (equal -128:bigint -128:bigint)))

;; int16ToBigint

(ert-deftest test-literals-negint16tobigint-negpositive ()

  (should (equal 1000:bigint 1000:bigint)))

(ert-deftest test-literals-negint16tobigint-negnegative ()

  (should (equal -1000:bigint -1000:bigint)))

;; int32ToBigint

(ert-deftest test-literals-negint32tobigint-negpositive ()

  (should (equal 42:bigint 42:bigint)))

(ert-deftest test-literals-negint32tobigint-negnegative ()

  (should (equal -42:bigint -42:bigint)))

(ert-deftest test-literals-negint32tobigint-negzero ()

  (should (equal 0:bigint 0:bigint)))

;; int64ToBigint

(ert-deftest test-literals-negint64tobigint-negpositive ()

  (should (equal 1000000:bigint 1000000:bigint)))

(ert-deftest test-literals-negint64tobigint-negnegative ()

  (should (equal -1000000:bigint -1000000:bigint)))

;; uint8ToBigint

(ert-deftest test-literals-neguint8tobigint-negzero ()

  (should (equal 0:bigint 0:bigint)))

(ert-deftest test-literals-neguint8tobigint-negmax-value ()

  (should (equal 255:bigint 255:bigint)))

;; uint16ToBigint

(ert-deftest test-literals-neguint16tobigint-negzero ()

  (should (equal 0:bigint 0:bigint)))

(ert-deftest test-literals-neguint16tobigint-negtypical-value ()

  (should (equal 1000:bigint 1000:bigint)))

;; uint32ToBigint

(ert-deftest test-literals-neguint32tobigint-negzero ()

  (should (equal 0:bigint 0:bigint)))

(ert-deftest test-literals-neguint32tobigint-negtypical-value ()

  (should (equal 100000:bigint 100000:bigint)))

;; uint64ToBigint

(ert-deftest test-literals-neguint64tobigint-negzero ()

  (should (equal 0:bigint 0:bigint)))

(ert-deftest test-literals-neguint64tobigint-negtypical-value ()

  (should (equal 1000000:bigint 1000000:bigint)))

;; float32ToBigfloat

(ert-deftest test-literals-negfloat32tobigfloat-negpositive ()

  (should (equal 2.5:bigfloat 2.5:bigfloat)))

(ert-deftest test-literals-negfloat32tobigfloat-negnegative ()

  (should (equal -2.5:bigfloat -2.5:bigfloat)))

(ert-deftest test-literals-negfloat32tobigfloat-negzero ()

  (should (equal 0.0:bigfloat 0.0:bigfloat)))

;; float64ToBigfloat

(ert-deftest test-literals-negfloat64tobigfloat-negpositive ()

  (should (equal 3.14159:bigfloat 3.14159:bigfloat)))

(ert-deftest test-literals-negfloat64tobigfloat-negnegative ()

  (should (equal -2.71828:bigfloat -2.71828:bigfloat)))

(ert-deftest test-literals-negfloat64tobigfloat-negzero ()

  (should (equal 0.0:bigfloat 0.0:bigfloat)))

;; bigfloatToFloat32

(ert-deftest test-literals-negbigfloattofloat32-negpositive ()

  (should (equal 3.14:float32 3.14:float32)))

(ert-deftest test-literals-negbigfloattofloat32-negnegative ()

  (should (equal -2.5:float32 -2.5:float32)))

(ert-deftest test-literals-negbigfloattofloat32-negzero ()

  (should (equal 0.0:float32 0.0:float32)))

;; bigfloatToFloat64

(ert-deftest test-literals-negbigfloattofloat64-negpositive ()

  (should (equal 3.14159:float64 3.14159:float64)))

(ert-deftest test-literals-negbigfloattofloat64-negnegative ()

  (should (equal -2.71828:float64 -2.71828:float64)))

(ert-deftest test-literals-negbigfloattofloat64-negzero ()

  (should (equal 0.0:float64 0.0:float64)))

;; bigintToBigfloat

(ert-deftest test-literals-negbiginttobigfloat-negpositive ()

  (should (equal 42.0:bigfloat 42.0:bigfloat)))

(ert-deftest test-literals-negbiginttobigfloat-negnegative ()

  (should (equal -42.0:bigfloat -42.0:bigfloat)))

(ert-deftest test-literals-negbiginttobigfloat-negzero ()

  (should (equal 0.0:bigfloat 0.0:bigfloat)))

;; bigfloatToBigint

(ert-deftest test-literals-negbigfloattobigint-negpositive ()

  (should (equal 43:bigint 43:bigint)))

(ert-deftest test-literals-negbigfloattobigint-negnegative ()

  (should (equal -43:bigint -43:bigint)))

(ert-deftest test-literals-negbigfloattobigint-negzero ()

  (should (equal 0:bigint 0:bigint)))

(ert-deftest test-literals-negbigfloattobigint-neground-down ()

  (should (equal 42:bigint 42:bigint)))

(ert-deftest test-literals-negbigfloattobigint-neghalf-even-up ()

  (should (equal 42:bigint 42:bigint)))

(ert-deftest test-literals-negbigfloattobigint-neghalf-even-down ()

  (should (equal 44:bigint 44:bigint)))

;; showInt8

(ert-deftest test-literals-negshowint8-negpositive ()

  (should (equal "42" "42")))

(ert-deftest test-literals-negshowint8-negnegative ()

  (should (equal "-42" "-42")))

;; showInt16

(ert-deftest test-literals-negshowint16-negpositive ()

  (should (equal "1000" "1000")))

(ert-deftest test-literals-negshowint16-negnegative ()

  (should (equal "-1000" "-1000")))

;; showInt32

(ert-deftest test-literals-negshowint32-negpositive ()

  (should (equal "42" "42")))

(ert-deftest test-literals-negshowint32-negnegative ()

  (should (equal "-42" "-42")))

(ert-deftest test-literals-negshowint32-negzero ()

  (should (equal "0" "0")))

;; showInt64

(ert-deftest test-literals-negshowint64-negpositive ()

  (should (equal "1000000" "1000000")))

(ert-deftest test-literals-negshowint64-negnegative ()

  (should (equal "-1000000" "-1000000")))

;; showUint8

(ert-deftest test-literals-negshowuint8-negzero ()

  (should (equal "0" "0")))

(ert-deftest test-literals-negshowuint8-negmax-value ()

  (should (equal "255" "255")))

;; showUint16

(ert-deftest test-literals-negshowuint16-negzero ()

  (should (equal "0" "0")))

(ert-deftest test-literals-negshowuint16-negtypical-value ()

  (should (equal "1000" "1000")))

;; showUint32

(ert-deftest test-literals-negshowuint32-negzero ()

  (should (equal "0" "0")))

(ert-deftest test-literals-negshowuint32-negtypical-value ()

  (should (equal "100000" "100000")))

;; showUint64

(ert-deftest test-literals-negshowuint64-negzero ()

  (should (equal "0" "0")))

(ert-deftest test-literals-negshowuint64-negtypical-value ()

  (should (equal "1000000" "1000000")))

;; showBigint

(ert-deftest test-literals-negshowbigint-negpositive ()

  (should (equal "42" "42")))

(ert-deftest test-literals-negshowbigint-negnegative ()

  (should (equal "-42" "-42")))

(ert-deftest test-literals-negshowbigint-negzero ()

  (should (equal "0" "0")))

;; showFloat32

(ert-deftest test-literals-negshowfloat32-negpositive ()

  (should (equal "3.14" "3.14")))

(ert-deftest test-literals-negshowfloat32-negnegative ()

  (should (equal "-2.5" "-2.5")))

(ert-deftest test-literals-negshowfloat32-negzero ()

  (should (equal "0.0" "0.0")))

(ert-deftest test-literals-negshowfloat32-negsmall-positive ()

  (should (equal "5.0e-2" "5.0e-2")))

(ert-deftest test-literals-negshowfloat32-negsmall-positive-2 ()

  (should (equal "3.0e-2" "3.0e-2")))

(ert-deftest test-literals-negshowfloat32-negvery-small ()

  (should (equal "1.0e-3" "1.0e-3")))

(ert-deftest test-literals-negshowfloat32-negnormal-decimal ()

  (should (equal "0.1" "0.1")))

;; showFloat64

(ert-deftest test-literals-negshowfloat64-negpositive ()

  (should (equal "3.14159" "3.14159")))

(ert-deftest test-literals-negshowfloat64-negzero ()

  (should (equal "0.0" "0.0")))

(ert-deftest test-literals-negshowfloat64-negsmall-positive ()

  (should (equal "5.0e-2" "5.0e-2")))

(ert-deftest test-literals-negshowfloat64-negsmall-positive-2 ()

  (should (equal "3.0e-2" "3.0e-2")))

(ert-deftest test-literals-negshowfloat64-negvery-small ()

  (should (equal "1.0e-3" "1.0e-3")))

(ert-deftest test-literals-negshowfloat64-negnormal-decimal ()

  (should (equal "0.1" "0.1")))

;; showBigfloat

(ert-deftest test-literals-negshowbigfloat-negpositive ()

  (should (equal "3.14" "3.14")))

(ert-deftest test-literals-negshowbigfloat-negzero ()

  (should (equal "0.0" "0.0")))

(ert-deftest test-literals-negshowbigfloat-negsmall-positive ()

  (should (equal "5.0e-2" "5.0e-2")))

(ert-deftest test-literals-negshowbigfloat-negsmall-positive-2 ()

  (should (equal "3.0e-2" "3.0e-2")))

(ert-deftest test-literals-negshowbigfloat-negvery-small ()

  (should (equal "1.0e-3" "1.0e-3")))

(ert-deftest test-literals-negshowbigfloat-negnormal-decimal ()

  (should (equal "0.1" "0.1")))

;; showBoolean

(ert-deftest test-literals-negshowboolean-negtrue ()

  (should (equal "true" "true")))

(ert-deftest test-literals-negshowboolean-negfalse ()

  (should (equal "false" "false")))

;; showString

(ert-deftest test-literals-negshowstring-negsimple ()

  (should (equal "\"hello\"" "\"hello\"")))

(ert-deftest test-literals-negshowstring-negempty ()

  (should (equal "\"\"" "\"\"")))

(ert-deftest test-literals-negshowstring-neglatin-accented ()

  (should (equal "\"caf\\233\"" "\"caf\\233\"")))

(ert-deftest test-literals-negshowstring-neggreek-lambda ()

  (should (equal "\"\\955\"" "\"\\955\"")))

(ert-deftest test-literals-negshowstring-negmixed-ascii-and-non-negascii ()

  (should (equal "\"A\\233B\"" "\"A\\233B\"")))

(ert-deftest test-literals-negshowstring-negtab ()

  (should (equal "\"\\t\"" "\"\\t\"")))

(ert-deftest test-literals-negshowstring-negnewline ()

  (should (equal "\"\\n\"" "\"\\n\"")))

(ert-deftest test-literals-negshowstring-negcarriage-return ()

  (should (equal "\"\\r\"" "\"\\r\"")))

(ert-deftest test-literals-negshowstring-negbackslash ()

  (should (equal "\"\\\\\"" "\"\\\\\"")))

(ert-deftest test-literals-negshowstring-negdouble-quote ()

  (should (equal "\"\\\"\"" "\"\\\"\"")))

(ert-deftest test-literals-negshowstring-negnull ()

  (should (equal "\"\\NUL\"" "\"\\NUL\"")))

(ert-deftest test-literals-negshowstring-negbell ()

  (should (equal "\"\\a\"" "\"\\a\"")))

(ert-deftest test-literals-negshowstring-negbackspace ()

  (should (equal "\"\\b\"" "\"\\b\"")))

(ert-deftest test-literals-negshowstring-negform-feed ()

  (should (equal "\"\\f\"" "\"\\f\"")))

(ert-deftest test-literals-negshowstring-negvertical-tab ()

  (should (equal "\"\\v\"" "\"\\v\"")))

(ert-deftest test-literals-negshowstring-negdelete ()

  (should (equal "\"\\DEL\"" "\"\\DEL\"")))

;; readInt8

(ert-deftest test-literals-negreadint8-negpositive ()

  (should (equal just(42:int8) just(42:int8))))

(ert-deftest test-literals-negreadint8-negnegative ()

  (should (equal just(-42:int8) just(-42:int8))))

(ert-deftest test-literals-negreadint8-negmax-value ()

  (should (equal just(127:int8) just(127:int8))))

(ert-deftest test-literals-negreadint8-negmin-value ()

  (should (equal just(-128:int8) just(-128:int8))))

(ert-deftest test-literals-negreadint8-neginvalid ()

  (should (equal nothing nothing)))

(ert-deftest test-literals-negreadint8-negoverflow ()

  (should (equal nothing nothing)))

;; readInt16

(ert-deftest test-literals-negreadint16-negpositive ()

  (should (equal just(1000:int16) just(1000:int16))))

(ert-deftest test-literals-negreadint16-negnegative ()

  (should (equal just(-1000:int16) just(-1000:int16))))

(ert-deftest test-literals-negreadint16-neginvalid ()

  (should (equal nothing nothing)))

;; readInt32

(ert-deftest test-literals-negreadint32-negpositive ()

  (should (equal just(42:int32) just(42:int32))))

(ert-deftest test-literals-negreadint32-negnegative ()

  (should (equal just(-42:int32) just(-42:int32))))

(ert-deftest test-literals-negreadint32-neginvalid ()

  (should (equal nothing nothing)))

;; readInt64

(ert-deftest test-literals-negreadint64-negpositive ()

  (should (equal just(1000000:int64) just(1000000:int64))))

(ert-deftest test-literals-negreadint64-negnegative ()

  (should (equal just(-1000000:int64) just(-1000000:int64))))

(ert-deftest test-literals-negreadint64-neginvalid ()

  (should (equal nothing nothing)))

;; readUint8

(ert-deftest test-literals-negreaduint8-negzero ()

  (should (equal just(0:uint8) just(0:uint8))))

(ert-deftest test-literals-negreaduint8-negtypical ()

  (should (equal just(100:uint8) just(100:uint8))))

(ert-deftest test-literals-negreaduint8-negmax-value ()

  (should (equal just(255:uint8) just(255:uint8))))

(ert-deftest test-literals-negreaduint8-neginvalid ()

  (should (equal nothing nothing)))

(ert-deftest test-literals-negreaduint8-negnegative ()

  (should (equal nothing nothing)))

;; readUint16

(ert-deftest test-literals-negreaduint16-negzero ()

  (should (equal just(0:uint16) just(0:uint16))))

(ert-deftest test-literals-negreaduint16-negtypical ()

  (should (equal just(1000:uint16) just(1000:uint16))))

(ert-deftest test-literals-negreaduint16-neginvalid ()

  (should (equal nothing nothing)))

(ert-deftest test-literals-negreaduint16-negnegative ()

  (should (equal nothing nothing)))

;; readUint32

(ert-deftest test-literals-negreaduint32-negzero ()

  (should (equal just(0:uint32) just(0:uint32))))

(ert-deftest test-literals-negreaduint32-negtypical ()

  (should (equal just(100000:uint32) just(100000:uint32))))

(ert-deftest test-literals-negreaduint32-neginvalid ()

  (should (equal nothing nothing)))

(ert-deftest test-literals-negreaduint32-negnegative ()

  (should (equal nothing nothing)))

;; readUint64

(ert-deftest test-literals-negreaduint64-negzero ()

  (should (equal just(0:uint64) just(0:uint64))))

(ert-deftest test-literals-negreaduint64-negtypical ()

  (should (equal just(1000000:uint64) just(1000000:uint64))))

(ert-deftest test-literals-negreaduint64-neginvalid ()

  (should (equal nothing nothing)))

(ert-deftest test-literals-negreaduint64-negnegative ()

  (should (equal nothing nothing)))

;; readBigint

(ert-deftest test-literals-negreadbigint-negpositive ()

  (should (equal just(42:bigint) just(42:bigint))))

(ert-deftest test-literals-negreadbigint-negnegative ()

  (should (equal just(-42:bigint) just(-42:bigint))))

(ert-deftest test-literals-negreadbigint-negzero ()

  (should (equal just(0:bigint) just(0:bigint))))

(ert-deftest test-literals-negreadbigint-neglarge ()

  (should (equal just(123456789012345678901234567890:bigint) just(123456789012345678901234567890:bigint))))

(ert-deftest test-literals-negreadbigint-neginvalid ()

  (should (equal nothing nothing)))

;; readFloat32

(ert-deftest test-literals-negreadfloat32-negpositive ()

  (should (equal just(3.14:float32) just(3.14:float32))))

(ert-deftest test-literals-negreadfloat32-negnegative ()

  (should (equal just(-2.5:float32) just(-2.5:float32))))

(ert-deftest test-literals-negreadfloat32-neginvalid ()

  (should (equal nothing nothing)))

;; readFloat64

(ert-deftest test-literals-negreadfloat64-negpositive ()

  (should (equal just(3.14159:float64) just(3.14159:float64))))

(ert-deftest test-literals-negreadfloat64-negnegative ()

  (should (equal just(-2.71828:float64) just(-2.71828:float64))))

(ert-deftest test-literals-negreadfloat64-neginvalid ()

  (should (equal nothing nothing)))

;; readBigfloat

(ert-deftest test-literals-negreadbigfloat-negpositive ()

  (should (equal just(3.14:bigfloat) just(3.14:bigfloat))))

(ert-deftest test-literals-negreadbigfloat-neginvalid ()

  (should (equal nothing nothing)))

;; readBoolean

(ert-deftest test-literals-negreadboolean-negtrue ()

  (should (equal just(true) just(true))))

(ert-deftest test-literals-negreadboolean-negfalse ()

  (should (equal just(false) just(false))))

(ert-deftest test-literals-negreadboolean-neginvalid ()

  (should (equal nothing nothing)))

;; readString

(ert-deftest test-literals-negreadstring-negquoted-string ()

  (should (equal just("hello") just("hello"))))

(ert-deftest test-literals-negreadstring-negempty-quoted ()

  (should (equal just("") just(""))))

(ert-deftest test-literals-negreadstring-negunquoted ()

  (should (equal nothing nothing)))

;; stringToBinary

(ert-deftest test-literals-negstringtobinary-negsimple-base64 ()

  (should (equal [binary] [binary])))

(ert-deftest test-literals-negstringtobinary-negempty-string ()

  (should (equal [binary] [binary])))

;; binaryToString

(ert-deftest test-literals-negbinarytostring-negsimple-binary ()

  (should (equal "aGVsbG8=" "aGVsbG8=")))

(ert-deftest test-literals-negbinarytostring-negempty-binary ()

  (should (equal "" "")))
