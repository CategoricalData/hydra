;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.literals primitives

;; bigintToInt8

(defun test-literals-negbiginttoint8-negpositive ()

  (assert (equal 42:int8 42:int8)))

(defun test-literals-negbiginttoint8-negnegative ()

  (assert (equal -42:int8 -42:int8)))

;; bigintToInt16

(defun test-literals-negbiginttoint16-negpositive ()

  (assert (equal 1000:int16 1000:int16)))

(defun test-literals-negbiginttoint16-negnegative ()

  (assert (equal -1000:int16 -1000:int16)))

;; bigintToInt32

(defun test-literals-negbiginttoint32-negpositive ()

  (assert (equal 42:int32 42:int32)))

(defun test-literals-negbiginttoint32-negnegative ()

  (assert (equal -42:int32 -42:int32)))

(defun test-literals-negbiginttoint32-negzero ()

  (assert (equal 0:int32 0:int32)))

;; bigintToInt64

(defun test-literals-negbiginttoint64-negpositive ()

  (assert (equal 1000000:int64 1000000:int64)))

(defun test-literals-negbiginttoint64-negnegative ()

  (assert (equal -1000000:int64 -1000000:int64)))

;; bigintToUint8

(defun test-literals-negbiginttouint8-negzero ()

  (assert (equal 0:uint8 0:uint8)))

(defun test-literals-negbiginttouint8-negtypical-value ()

  (assert (equal 100:uint8 100:uint8)))

;; bigintToUint16

(defun test-literals-negbiginttouint16-negzero ()

  (assert (equal 0:uint16 0:uint16)))

(defun test-literals-negbiginttouint16-negtypical-value ()

  (assert (equal 1000:uint16 1000:uint16)))

;; bigintToUint32

(defun test-literals-negbiginttouint32-negzero ()

  (assert (equal 0:uint32 0:uint32)))

(defun test-literals-negbiginttouint32-negtypical-value ()

  (assert (equal 100000:uint32 100000:uint32)))

;; bigintToUint64

(defun test-literals-negbiginttouint64-negzero ()

  (assert (equal 0:uint64 0:uint64)))

(defun test-literals-negbiginttouint64-negtypical-value ()

  (assert (equal 1000000:uint64 1000000:uint64)))

;; int8ToBigint

(defun test-literals-negint8tobigint-negpositive ()

  (assert (equal 42:bigint 42:bigint)))

(defun test-literals-negint8tobigint-negnegative ()

  (assert (equal -42:bigint -42:bigint)))

(defun test-literals-negint8tobigint-negmax-value ()

  (assert (equal 127:bigint 127:bigint)))

(defun test-literals-negint8tobigint-negmin-value ()

  (assert (equal -128:bigint -128:bigint)))

;; int16ToBigint

(defun test-literals-negint16tobigint-negpositive ()

  (assert (equal 1000:bigint 1000:bigint)))

(defun test-literals-negint16tobigint-negnegative ()

  (assert (equal -1000:bigint -1000:bigint)))

;; int32ToBigint

(defun test-literals-negint32tobigint-negpositive ()

  (assert (equal 42:bigint 42:bigint)))

(defun test-literals-negint32tobigint-negnegative ()

  (assert (equal -42:bigint -42:bigint)))

(defun test-literals-negint32tobigint-negzero ()

  (assert (equal 0:bigint 0:bigint)))

;; int64ToBigint

(defun test-literals-negint64tobigint-negpositive ()

  (assert (equal 1000000:bigint 1000000:bigint)))

(defun test-literals-negint64tobigint-negnegative ()

  (assert (equal -1000000:bigint -1000000:bigint)))

;; uint8ToBigint

(defun test-literals-neguint8tobigint-negzero ()

  (assert (equal 0:bigint 0:bigint)))

(defun test-literals-neguint8tobigint-negmax-value ()

  (assert (equal 255:bigint 255:bigint)))

;; uint16ToBigint

(defun test-literals-neguint16tobigint-negzero ()

  (assert (equal 0:bigint 0:bigint)))

(defun test-literals-neguint16tobigint-negtypical-value ()

  (assert (equal 1000:bigint 1000:bigint)))

;; uint32ToBigint

(defun test-literals-neguint32tobigint-negzero ()

  (assert (equal 0:bigint 0:bigint)))

(defun test-literals-neguint32tobigint-negtypical-value ()

  (assert (equal 100000:bigint 100000:bigint)))

;; uint64ToBigint

(defun test-literals-neguint64tobigint-negzero ()

  (assert (equal 0:bigint 0:bigint)))

(defun test-literals-neguint64tobigint-negtypical-value ()

  (assert (equal 1000000:bigint 1000000:bigint)))

;; float32ToBigfloat

(defun test-literals-negfloat32tobigfloat-negpositive ()

  (assert (equal 2.5:bigfloat 2.5:bigfloat)))

(defun test-literals-negfloat32tobigfloat-negnegative ()

  (assert (equal -2.5:bigfloat -2.5:bigfloat)))

(defun test-literals-negfloat32tobigfloat-negzero ()

  (assert (equal 0.0:bigfloat 0.0:bigfloat)))

;; float64ToBigfloat

(defun test-literals-negfloat64tobigfloat-negpositive ()

  (assert (equal 3.14159:bigfloat 3.14159:bigfloat)))

(defun test-literals-negfloat64tobigfloat-negnegative ()

  (assert (equal -2.71828:bigfloat -2.71828:bigfloat)))

(defun test-literals-negfloat64tobigfloat-negzero ()

  (assert (equal 0.0:bigfloat 0.0:bigfloat)))

;; bigfloatToFloat32

(defun test-literals-negbigfloattofloat32-negpositive ()

  (assert (equal 3.14:float32 3.14:float32)))

(defun test-literals-negbigfloattofloat32-negnegative ()

  (assert (equal -2.5:float32 -2.5:float32)))

(defun test-literals-negbigfloattofloat32-negzero ()

  (assert (equal 0.0:float32 0.0:float32)))

;; bigfloatToFloat64

(defun test-literals-negbigfloattofloat64-negpositive ()

  (assert (equal 3.14159:float64 3.14159:float64)))

(defun test-literals-negbigfloattofloat64-negnegative ()

  (assert (equal -2.71828:float64 -2.71828:float64)))

(defun test-literals-negbigfloattofloat64-negzero ()

  (assert (equal 0.0:float64 0.0:float64)))

;; bigintToBigfloat

(defun test-literals-negbiginttobigfloat-negpositive ()

  (assert (equal 42.0:bigfloat 42.0:bigfloat)))

(defun test-literals-negbiginttobigfloat-negnegative ()

  (assert (equal -42.0:bigfloat -42.0:bigfloat)))

(defun test-literals-negbiginttobigfloat-negzero ()

  (assert (equal 0.0:bigfloat 0.0:bigfloat)))

;; bigfloatToBigint

(defun test-literals-negbigfloattobigint-negpositive ()

  (assert (equal 43:bigint 43:bigint)))

(defun test-literals-negbigfloattobigint-negnegative ()

  (assert (equal -43:bigint -43:bigint)))

(defun test-literals-negbigfloattobigint-negzero ()

  (assert (equal 0:bigint 0:bigint)))

(defun test-literals-negbigfloattobigint-neground-down ()

  (assert (equal 42:bigint 42:bigint)))

(defun test-literals-negbigfloattobigint-neghalf-even-up ()

  (assert (equal 42:bigint 42:bigint)))

(defun test-literals-negbigfloattobigint-neghalf-even-down ()

  (assert (equal 44:bigint 44:bigint)))

;; showInt8

(defun test-literals-negshowint8-negpositive ()

  (assert (equal "42" "42")))

(defun test-literals-negshowint8-negnegative ()

  (assert (equal "-42" "-42")))

;; showInt16

(defun test-literals-negshowint16-negpositive ()

  (assert (equal "1000" "1000")))

(defun test-literals-negshowint16-negnegative ()

  (assert (equal "-1000" "-1000")))

;; showInt32

(defun test-literals-negshowint32-negpositive ()

  (assert (equal "42" "42")))

(defun test-literals-negshowint32-negnegative ()

  (assert (equal "-42" "-42")))

(defun test-literals-negshowint32-negzero ()

  (assert (equal "0" "0")))

;; showInt64

(defun test-literals-negshowint64-negpositive ()

  (assert (equal "1000000" "1000000")))

(defun test-literals-negshowint64-negnegative ()

  (assert (equal "-1000000" "-1000000")))

;; showUint8

(defun test-literals-negshowuint8-negzero ()

  (assert (equal "0" "0")))

(defun test-literals-negshowuint8-negmax-value ()

  (assert (equal "255" "255")))

;; showUint16

(defun test-literals-negshowuint16-negzero ()

  (assert (equal "0" "0")))

(defun test-literals-negshowuint16-negtypical-value ()

  (assert (equal "1000" "1000")))

;; showUint32

(defun test-literals-negshowuint32-negzero ()

  (assert (equal "0" "0")))

(defun test-literals-negshowuint32-negtypical-value ()

  (assert (equal "100000" "100000")))

;; showUint64

(defun test-literals-negshowuint64-negzero ()

  (assert (equal "0" "0")))

(defun test-literals-negshowuint64-negtypical-value ()

  (assert (equal "1000000" "1000000")))

;; showBigint

(defun test-literals-negshowbigint-negpositive ()

  (assert (equal "42" "42")))

(defun test-literals-negshowbigint-negnegative ()

  (assert (equal "-42" "-42")))

(defun test-literals-negshowbigint-negzero ()

  (assert (equal "0" "0")))

;; showFloat32

(defun test-literals-negshowfloat32-negpositive ()

  (assert (equal "3.14" "3.14")))

(defun test-literals-negshowfloat32-negnegative ()

  (assert (equal "-2.5" "-2.5")))

(defun test-literals-negshowfloat32-negzero ()

  (assert (equal "0.0" "0.0")))

(defun test-literals-negshowfloat32-negsmall-positive ()

  (assert (equal "5.0e-2" "5.0e-2")))

(defun test-literals-negshowfloat32-negsmall-positive-2 ()

  (assert (equal "3.0e-2" "3.0e-2")))

(defun test-literals-negshowfloat32-negvery-small ()

  (assert (equal "1.0e-3" "1.0e-3")))

(defun test-literals-negshowfloat32-negnormal-decimal ()

  (assert (equal "0.1" "0.1")))

;; showFloat64

(defun test-literals-negshowfloat64-negpositive ()

  (assert (equal "3.14159" "3.14159")))

(defun test-literals-negshowfloat64-negzero ()

  (assert (equal "0.0" "0.0")))

(defun test-literals-negshowfloat64-negsmall-positive ()

  (assert (equal "5.0e-2" "5.0e-2")))

(defun test-literals-negshowfloat64-negsmall-positive-2 ()

  (assert (equal "3.0e-2" "3.0e-2")))

(defun test-literals-negshowfloat64-negvery-small ()

  (assert (equal "1.0e-3" "1.0e-3")))

(defun test-literals-negshowfloat64-negnormal-decimal ()

  (assert (equal "0.1" "0.1")))

;; showBigfloat

(defun test-literals-negshowbigfloat-negpositive ()

  (assert (equal "3.14" "3.14")))

(defun test-literals-negshowbigfloat-negzero ()

  (assert (equal "0.0" "0.0")))

(defun test-literals-negshowbigfloat-negsmall-positive ()

  (assert (equal "5.0e-2" "5.0e-2")))

(defun test-literals-negshowbigfloat-negsmall-positive-2 ()

  (assert (equal "3.0e-2" "3.0e-2")))

(defun test-literals-negshowbigfloat-negvery-small ()

  (assert (equal "1.0e-3" "1.0e-3")))

(defun test-literals-negshowbigfloat-negnormal-decimal ()

  (assert (equal "0.1" "0.1")))

;; showBoolean

(defun test-literals-negshowboolean-negtrue ()

  (assert (equal "true" "true")))

(defun test-literals-negshowboolean-negfalse ()

  (assert (equal "false" "false")))

;; showString

(defun test-literals-negshowstring-negsimple ()

  (assert (equal "\"hello\"" "\"hello\"")))

(defun test-literals-negshowstring-negempty ()

  (assert (equal "\"\"" "\"\"")))

(defun test-literals-negshowstring-neglatin-accented ()

  (assert (equal "\"caf\\233\"" "\"caf\\233\"")))

(defun test-literals-negshowstring-neggreek-lambda ()

  (assert (equal "\"\\955\"" "\"\\955\"")))

(defun test-literals-negshowstring-negmixed-ascii-and-non-negascii ()

  (assert (equal "\"A\\233B\"" "\"A\\233B\"")))

(defun test-literals-negshowstring-negtab ()

  (assert (equal "\"\\t\"" "\"\\t\"")))

(defun test-literals-negshowstring-negnewline ()

  (assert (equal "\"\\n\"" "\"\\n\"")))

(defun test-literals-negshowstring-negcarriage-return ()

  (assert (equal "\"\\r\"" "\"\\r\"")))

(defun test-literals-negshowstring-negbackslash ()

  (assert (equal "\"\\\\\"" "\"\\\\\"")))

(defun test-literals-negshowstring-negdouble-quote ()

  (assert (equal "\"\\\"\"" "\"\\\"\"")))

(defun test-literals-negshowstring-negnull ()

  (assert (equal "\"\\NUL\"" "\"\\NUL\"")))

(defun test-literals-negshowstring-negbell ()

  (assert (equal "\"\\a\"" "\"\\a\"")))

(defun test-literals-negshowstring-negbackspace ()

  (assert (equal "\"\\b\"" "\"\\b\"")))

(defun test-literals-negshowstring-negform-feed ()

  (assert (equal "\"\\f\"" "\"\\f\"")))

(defun test-literals-negshowstring-negvertical-tab ()

  (assert (equal "\"\\v\"" "\"\\v\"")))

(defun test-literals-negshowstring-negdelete ()

  (assert (equal "\"\\DEL\"" "\"\\DEL\"")))

;; readInt8

(defun test-literals-negreadint8-negpositive ()

  (assert (equal just(42:int8) just(42:int8))))

(defun test-literals-negreadint8-negnegative ()

  (assert (equal just(-42:int8) just(-42:int8))))

(defun test-literals-negreadint8-negmax-value ()

  (assert (equal just(127:int8) just(127:int8))))

(defun test-literals-negreadint8-negmin-value ()

  (assert (equal just(-128:int8) just(-128:int8))))

(defun test-literals-negreadint8-neginvalid ()

  (assert (equal nothing nothing)))

(defun test-literals-negreadint8-negoverflow ()

  (assert (equal nothing nothing)))

;; readInt16

(defun test-literals-negreadint16-negpositive ()

  (assert (equal just(1000:int16) just(1000:int16))))

(defun test-literals-negreadint16-negnegative ()

  (assert (equal just(-1000:int16) just(-1000:int16))))

(defun test-literals-negreadint16-neginvalid ()

  (assert (equal nothing nothing)))

;; readInt32

(defun test-literals-negreadint32-negpositive ()

  (assert (equal just(42:int32) just(42:int32))))

(defun test-literals-negreadint32-negnegative ()

  (assert (equal just(-42:int32) just(-42:int32))))

(defun test-literals-negreadint32-neginvalid ()

  (assert (equal nothing nothing)))

;; readInt64

(defun test-literals-negreadint64-negpositive ()

  (assert (equal just(1000000:int64) just(1000000:int64))))

(defun test-literals-negreadint64-negnegative ()

  (assert (equal just(-1000000:int64) just(-1000000:int64))))

(defun test-literals-negreadint64-neginvalid ()

  (assert (equal nothing nothing)))

;; readUint8

(defun test-literals-negreaduint8-negzero ()

  (assert (equal just(0:uint8) just(0:uint8))))

(defun test-literals-negreaduint8-negtypical ()

  (assert (equal just(100:uint8) just(100:uint8))))

(defun test-literals-negreaduint8-negmax-value ()

  (assert (equal just(255:uint8) just(255:uint8))))

(defun test-literals-negreaduint8-neginvalid ()

  (assert (equal nothing nothing)))

(defun test-literals-negreaduint8-negnegative ()

  (assert (equal nothing nothing)))

;; readUint16

(defun test-literals-negreaduint16-negzero ()

  (assert (equal just(0:uint16) just(0:uint16))))

(defun test-literals-negreaduint16-negtypical ()

  (assert (equal just(1000:uint16) just(1000:uint16))))

(defun test-literals-negreaduint16-neginvalid ()

  (assert (equal nothing nothing)))

(defun test-literals-negreaduint16-negnegative ()

  (assert (equal nothing nothing)))

;; readUint32

(defun test-literals-negreaduint32-negzero ()

  (assert (equal just(0:uint32) just(0:uint32))))

(defun test-literals-negreaduint32-negtypical ()

  (assert (equal just(100000:uint32) just(100000:uint32))))

(defun test-literals-negreaduint32-neginvalid ()

  (assert (equal nothing nothing)))

(defun test-literals-negreaduint32-negnegative ()

  (assert (equal nothing nothing)))

;; readUint64

(defun test-literals-negreaduint64-negzero ()

  (assert (equal just(0:uint64) just(0:uint64))))

(defun test-literals-negreaduint64-negtypical ()

  (assert (equal just(1000000:uint64) just(1000000:uint64))))

(defun test-literals-negreaduint64-neginvalid ()

  (assert (equal nothing nothing)))

(defun test-literals-negreaduint64-negnegative ()

  (assert (equal nothing nothing)))

;; readBigint

(defun test-literals-negreadbigint-negpositive ()

  (assert (equal just(42:bigint) just(42:bigint))))

(defun test-literals-negreadbigint-negnegative ()

  (assert (equal just(-42:bigint) just(-42:bigint))))

(defun test-literals-negreadbigint-negzero ()

  (assert (equal just(0:bigint) just(0:bigint))))

(defun test-literals-negreadbigint-neglarge ()

  (assert (equal just(123456789012345678901234567890:bigint) just(123456789012345678901234567890:bigint))))

(defun test-literals-negreadbigint-neginvalid ()

  (assert (equal nothing nothing)))

;; readFloat32

(defun test-literals-negreadfloat32-negpositive ()

  (assert (equal just(3.14:float32) just(3.14:float32))))

(defun test-literals-negreadfloat32-negnegative ()

  (assert (equal just(-2.5:float32) just(-2.5:float32))))

(defun test-literals-negreadfloat32-neginvalid ()

  (assert (equal nothing nothing)))

;; readFloat64

(defun test-literals-negreadfloat64-negpositive ()

  (assert (equal just(3.14159:float64) just(3.14159:float64))))

(defun test-literals-negreadfloat64-negnegative ()

  (assert (equal just(-2.71828:float64) just(-2.71828:float64))))

(defun test-literals-negreadfloat64-neginvalid ()

  (assert (equal nothing nothing)))

;; readBigfloat

(defun test-literals-negreadbigfloat-negpositive ()

  (assert (equal just(3.14:bigfloat) just(3.14:bigfloat))))

(defun test-literals-negreadbigfloat-neginvalid ()

  (assert (equal nothing nothing)))

;; readBoolean

(defun test-literals-negreadboolean-negtrue ()

  (assert (equal just(true) just(true))))

(defun test-literals-negreadboolean-negfalse ()

  (assert (equal just(false) just(false))))

(defun test-literals-negreadboolean-neginvalid ()

  (assert (equal nothing nothing)))

;; readString

(defun test-literals-negreadstring-negquoted-string ()

  (assert (equal just("hello") just("hello"))))

(defun test-literals-negreadstring-negempty-quoted ()

  (assert (equal just("") just(""))))

(defun test-literals-negreadstring-negunquoted ()

  (assert (equal nothing nothing)))

;; stringToBinary

(defun test-literals-negstringtobinary-negsimple-base64 ()

  (assert (equal [binary] [binary])))

(defun test-literals-negstringtobinary-negempty-string ()

  (assert (equal [binary] [binary])))

;; binaryToString

(defun test-literals-negbinarytostring-negsimple-binary ()

  (assert (equal "aGVsbG8=" "aGVsbG8=")))

(defun test-literals-negbinarytostring-negempty-binary ()

  (assert (equal "" "")))
