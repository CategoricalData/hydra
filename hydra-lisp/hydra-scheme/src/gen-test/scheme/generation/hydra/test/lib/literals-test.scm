;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.literals primitives

(import (scheme base))

;; bigintToInt8

(define (test-literals-negbiginttoint8-negpositive)

  (assert (equal? 42:int8 42:int8)))

(define (test-literals-negbiginttoint8-negnegative)

  (assert (equal? -42:int8 -42:int8)))

;; bigintToInt16

(define (test-literals-negbiginttoint16-negpositive)

  (assert (equal? 1000:int16 1000:int16)))

(define (test-literals-negbiginttoint16-negnegative)

  (assert (equal? -1000:int16 -1000:int16)))

;; bigintToInt32

(define (test-literals-negbiginttoint32-negpositive)

  (assert (equal? 42:int32 42:int32)))

(define (test-literals-negbiginttoint32-negnegative)

  (assert (equal? -42:int32 -42:int32)))

(define (test-literals-negbiginttoint32-negzero)

  (assert (equal? 0:int32 0:int32)))

;; bigintToInt64

(define (test-literals-negbiginttoint64-negpositive)

  (assert (equal? 1000000:int64 1000000:int64)))

(define (test-literals-negbiginttoint64-negnegative)

  (assert (equal? -1000000:int64 -1000000:int64)))

;; bigintToUint8

(define (test-literals-negbiginttouint8-negzero)

  (assert (equal? 0:uint8 0:uint8)))

(define (test-literals-negbiginttouint8-negtypical-value)

  (assert (equal? 100:uint8 100:uint8)))

;; bigintToUint16

(define (test-literals-negbiginttouint16-negzero)

  (assert (equal? 0:uint16 0:uint16)))

(define (test-literals-negbiginttouint16-negtypical-value)

  (assert (equal? 1000:uint16 1000:uint16)))

;; bigintToUint32

(define (test-literals-negbiginttouint32-negzero)

  (assert (equal? 0:uint32 0:uint32)))

(define (test-literals-negbiginttouint32-negtypical-value)

  (assert (equal? 100000:uint32 100000:uint32)))

;; bigintToUint64

(define (test-literals-negbiginttouint64-negzero)

  (assert (equal? 0:uint64 0:uint64)))

(define (test-literals-negbiginttouint64-negtypical-value)

  (assert (equal? 1000000:uint64 1000000:uint64)))

;; int8ToBigint

(define (test-literals-negint8tobigint-negpositive)

  (assert (equal? 42:bigint 42:bigint)))

(define (test-literals-negint8tobigint-negnegative)

  (assert (equal? -42:bigint -42:bigint)))

(define (test-literals-negint8tobigint-negmax-value)

  (assert (equal? 127:bigint 127:bigint)))

(define (test-literals-negint8tobigint-negmin-value)

  (assert (equal? -128:bigint -128:bigint)))

;; int16ToBigint

(define (test-literals-negint16tobigint-negpositive)

  (assert (equal? 1000:bigint 1000:bigint)))

(define (test-literals-negint16tobigint-negnegative)

  (assert (equal? -1000:bigint -1000:bigint)))

;; int32ToBigint

(define (test-literals-negint32tobigint-negpositive)

  (assert (equal? 42:bigint 42:bigint)))

(define (test-literals-negint32tobigint-negnegative)

  (assert (equal? -42:bigint -42:bigint)))

(define (test-literals-negint32tobigint-negzero)

  (assert (equal? 0:bigint 0:bigint)))

;; int64ToBigint

(define (test-literals-negint64tobigint-negpositive)

  (assert (equal? 1000000:bigint 1000000:bigint)))

(define (test-literals-negint64tobigint-negnegative)

  (assert (equal? -1000000:bigint -1000000:bigint)))

;; uint8ToBigint

(define (test-literals-neguint8tobigint-negzero)

  (assert (equal? 0:bigint 0:bigint)))

(define (test-literals-neguint8tobigint-negmax-value)

  (assert (equal? 255:bigint 255:bigint)))

;; uint16ToBigint

(define (test-literals-neguint16tobigint-negzero)

  (assert (equal? 0:bigint 0:bigint)))

(define (test-literals-neguint16tobigint-negtypical-value)

  (assert (equal? 1000:bigint 1000:bigint)))

;; uint32ToBigint

(define (test-literals-neguint32tobigint-negzero)

  (assert (equal? 0:bigint 0:bigint)))

(define (test-literals-neguint32tobigint-negtypical-value)

  (assert (equal? 100000:bigint 100000:bigint)))

;; uint64ToBigint

(define (test-literals-neguint64tobigint-negzero)

  (assert (equal? 0:bigint 0:bigint)))

(define (test-literals-neguint64tobigint-negtypical-value)

  (assert (equal? 1000000:bigint 1000000:bigint)))

;; float32ToBigfloat

(define (test-literals-negfloat32tobigfloat-negpositive)

  (assert (equal? 2.5:bigfloat 2.5:bigfloat)))

(define (test-literals-negfloat32tobigfloat-negnegative)

  (assert (equal? -2.5:bigfloat -2.5:bigfloat)))

(define (test-literals-negfloat32tobigfloat-negzero)

  (assert (equal? 0.0:bigfloat 0.0:bigfloat)))

;; float64ToBigfloat

(define (test-literals-negfloat64tobigfloat-negpositive)

  (assert (equal? 3.14159:bigfloat 3.14159:bigfloat)))

(define (test-literals-negfloat64tobigfloat-negnegative)

  (assert (equal? -2.71828:bigfloat -2.71828:bigfloat)))

(define (test-literals-negfloat64tobigfloat-negzero)

  (assert (equal? 0.0:bigfloat 0.0:bigfloat)))

;; bigfloatToFloat32

(define (test-literals-negbigfloattofloat32-negpositive)

  (assert (equal? 3.14:float32 3.14:float32)))

(define (test-literals-negbigfloattofloat32-negnegative)

  (assert (equal? -2.5:float32 -2.5:float32)))

(define (test-literals-negbigfloattofloat32-negzero)

  (assert (equal? 0.0:float32 0.0:float32)))

;; bigfloatToFloat64

(define (test-literals-negbigfloattofloat64-negpositive)

  (assert (equal? 3.14159:float64 3.14159:float64)))

(define (test-literals-negbigfloattofloat64-negnegative)

  (assert (equal? -2.71828:float64 -2.71828:float64)))

(define (test-literals-negbigfloattofloat64-negzero)

  (assert (equal? 0.0:float64 0.0:float64)))

;; bigintToBigfloat

(define (test-literals-negbiginttobigfloat-negpositive)

  (assert (equal? 42.0:bigfloat 42.0:bigfloat)))

(define (test-literals-negbiginttobigfloat-negnegative)

  (assert (equal? -42.0:bigfloat -42.0:bigfloat)))

(define (test-literals-negbiginttobigfloat-negzero)

  (assert (equal? 0.0:bigfloat 0.0:bigfloat)))

;; bigfloatToBigint

(define (test-literals-negbigfloattobigint-negpositive)

  (assert (equal? 43:bigint 43:bigint)))

(define (test-literals-negbigfloattobigint-negnegative)

  (assert (equal? -43:bigint -43:bigint)))

(define (test-literals-negbigfloattobigint-negzero)

  (assert (equal? 0:bigint 0:bigint)))

(define (test-literals-negbigfloattobigint-neground-down)

  (assert (equal? 42:bigint 42:bigint)))

(define (test-literals-negbigfloattobigint-neghalf-even-up)

  (assert (equal? 42:bigint 42:bigint)))

(define (test-literals-negbigfloattobigint-neghalf-even-down)

  (assert (equal? 44:bigint 44:bigint)))

;; showInt8

(define (test-literals-negshowint8-negpositive)

  (assert (equal? "42" "42")))

(define (test-literals-negshowint8-negnegative)

  (assert (equal? "-42" "-42")))

;; showInt16

(define (test-literals-negshowint16-negpositive)

  (assert (equal? "1000" "1000")))

(define (test-literals-negshowint16-negnegative)

  (assert (equal? "-1000" "-1000")))

;; showInt32

(define (test-literals-negshowint32-negpositive)

  (assert (equal? "42" "42")))

(define (test-literals-negshowint32-negnegative)

  (assert (equal? "-42" "-42")))

(define (test-literals-negshowint32-negzero)

  (assert (equal? "0" "0")))

;; showInt64

(define (test-literals-negshowint64-negpositive)

  (assert (equal? "1000000" "1000000")))

(define (test-literals-negshowint64-negnegative)

  (assert (equal? "-1000000" "-1000000")))

;; showUint8

(define (test-literals-negshowuint8-negzero)

  (assert (equal? "0" "0")))

(define (test-literals-negshowuint8-negmax-value)

  (assert (equal? "255" "255")))

;; showUint16

(define (test-literals-negshowuint16-negzero)

  (assert (equal? "0" "0")))

(define (test-literals-negshowuint16-negtypical-value)

  (assert (equal? "1000" "1000")))

;; showUint32

(define (test-literals-negshowuint32-negzero)

  (assert (equal? "0" "0")))

(define (test-literals-negshowuint32-negtypical-value)

  (assert (equal? "100000" "100000")))

;; showUint64

(define (test-literals-negshowuint64-negzero)

  (assert (equal? "0" "0")))

(define (test-literals-negshowuint64-negtypical-value)

  (assert (equal? "1000000" "1000000")))

;; showBigint

(define (test-literals-negshowbigint-negpositive)

  (assert (equal? "42" "42")))

(define (test-literals-negshowbigint-negnegative)

  (assert (equal? "-42" "-42")))

(define (test-literals-negshowbigint-negzero)

  (assert (equal? "0" "0")))

;; showFloat32

(define (test-literals-negshowfloat32-negpositive)

  (assert (equal? "3.14" "3.14")))

(define (test-literals-negshowfloat32-negnegative)

  (assert (equal? "-2.5" "-2.5")))

(define (test-literals-negshowfloat32-negzero)

  (assert (equal? "0.0" "0.0")))

(define (test-literals-negshowfloat32-negsmall-positive)

  (assert (equal? "5.0e-2" "5.0e-2")))

(define (test-literals-negshowfloat32-negsmall-positive-2)

  (assert (equal? "3.0e-2" "3.0e-2")))

(define (test-literals-negshowfloat32-negvery-small)

  (assert (equal? "1.0e-3" "1.0e-3")))

(define (test-literals-negshowfloat32-negnormal-decimal)

  (assert (equal? "0.1" "0.1")))

;; showFloat64

(define (test-literals-negshowfloat64-negpositive)

  (assert (equal? "3.14159" "3.14159")))

(define (test-literals-negshowfloat64-negzero)

  (assert (equal? "0.0" "0.0")))

(define (test-literals-negshowfloat64-negsmall-positive)

  (assert (equal? "5.0e-2" "5.0e-2")))

(define (test-literals-negshowfloat64-negsmall-positive-2)

  (assert (equal? "3.0e-2" "3.0e-2")))

(define (test-literals-negshowfloat64-negvery-small)

  (assert (equal? "1.0e-3" "1.0e-3")))

(define (test-literals-negshowfloat64-negnormal-decimal)

  (assert (equal? "0.1" "0.1")))

;; showBigfloat

(define (test-literals-negshowbigfloat-negpositive)

  (assert (equal? "3.14" "3.14")))

(define (test-literals-negshowbigfloat-negzero)

  (assert (equal? "0.0" "0.0")))

(define (test-literals-negshowbigfloat-negsmall-positive)

  (assert (equal? "5.0e-2" "5.0e-2")))

(define (test-literals-negshowbigfloat-negsmall-positive-2)

  (assert (equal? "3.0e-2" "3.0e-2")))

(define (test-literals-negshowbigfloat-negvery-small)

  (assert (equal? "1.0e-3" "1.0e-3")))

(define (test-literals-negshowbigfloat-negnormal-decimal)

  (assert (equal? "0.1" "0.1")))

;; showBoolean

(define (test-literals-negshowboolean-negtrue)

  (assert (equal? "true" "true")))

(define (test-literals-negshowboolean-negfalse)

  (assert (equal? "false" "false")))

;; showString

(define (test-literals-negshowstring-negsimple)

  (assert (equal? "\"hello\"" "\"hello\"")))

(define (test-literals-negshowstring-negempty)

  (assert (equal? "\"\"" "\"\"")))

(define (test-literals-negshowstring-neglatin-accented)

  (assert (equal? "\"caf\\233\"" "\"caf\\233\"")))

(define (test-literals-negshowstring-neggreek-lambda)

  (assert (equal? "\"\\955\"" "\"\\955\"")))

(define (test-literals-negshowstring-negmixed-ascii-and-non-negascii)

  (assert (equal? "\"A\\233B\"" "\"A\\233B\"")))

(define (test-literals-negshowstring-negtab)

  (assert (equal? "\"\\t\"" "\"\\t\"")))

(define (test-literals-negshowstring-negnewline)

  (assert (equal? "\"\\n\"" "\"\\n\"")))

(define (test-literals-negshowstring-negcarriage-return)

  (assert (equal? "\"\\r\"" "\"\\r\"")))

(define (test-literals-negshowstring-negbackslash)

  (assert (equal? "\"\\\\\"" "\"\\\\\"")))

(define (test-literals-negshowstring-negdouble-quote)

  (assert (equal? "\"\\\"\"" "\"\\\"\"")))

(define (test-literals-negshowstring-negnull)

  (assert (equal? "\"\\NUL\"" "\"\\NUL\"")))

(define (test-literals-negshowstring-negbell)

  (assert (equal? "\"\\a\"" "\"\\a\"")))

(define (test-literals-negshowstring-negbackspace)

  (assert (equal? "\"\\b\"" "\"\\b\"")))

(define (test-literals-negshowstring-negform-feed)

  (assert (equal? "\"\\f\"" "\"\\f\"")))

(define (test-literals-negshowstring-negvertical-tab)

  (assert (equal? "\"\\v\"" "\"\\v\"")))

(define (test-literals-negshowstring-negdelete)

  (assert (equal? "\"\\DEL\"" "\"\\DEL\"")))

;; readInt8

(define (test-literals-negreadint8-negpositive)

  (assert (equal? just(42:int8) just(42:int8))))

(define (test-literals-negreadint8-negnegative)

  (assert (equal? just(-42:int8) just(-42:int8))))

(define (test-literals-negreadint8-negmax-value)

  (assert (equal? just(127:int8) just(127:int8))))

(define (test-literals-negreadint8-negmin-value)

  (assert (equal? just(-128:int8) just(-128:int8))))

(define (test-literals-negreadint8-neginvalid)

  (assert (equal? nothing nothing)))

(define (test-literals-negreadint8-negoverflow)

  (assert (equal? nothing nothing)))

;; readInt16

(define (test-literals-negreadint16-negpositive)

  (assert (equal? just(1000:int16) just(1000:int16))))

(define (test-literals-negreadint16-negnegative)

  (assert (equal? just(-1000:int16) just(-1000:int16))))

(define (test-literals-negreadint16-neginvalid)

  (assert (equal? nothing nothing)))

;; readInt32

(define (test-literals-negreadint32-negpositive)

  (assert (equal? just(42:int32) just(42:int32))))

(define (test-literals-negreadint32-negnegative)

  (assert (equal? just(-42:int32) just(-42:int32))))

(define (test-literals-negreadint32-neginvalid)

  (assert (equal? nothing nothing)))

;; readInt64

(define (test-literals-negreadint64-negpositive)

  (assert (equal? just(1000000:int64) just(1000000:int64))))

(define (test-literals-negreadint64-negnegative)

  (assert (equal? just(-1000000:int64) just(-1000000:int64))))

(define (test-literals-negreadint64-neginvalid)

  (assert (equal? nothing nothing)))

;; readUint8

(define (test-literals-negreaduint8-negzero)

  (assert (equal? just(0:uint8) just(0:uint8))))

(define (test-literals-negreaduint8-negtypical)

  (assert (equal? just(100:uint8) just(100:uint8))))

(define (test-literals-negreaduint8-negmax-value)

  (assert (equal? just(255:uint8) just(255:uint8))))

(define (test-literals-negreaduint8-neginvalid)

  (assert (equal? nothing nothing)))

(define (test-literals-negreaduint8-negnegative)

  (assert (equal? nothing nothing)))

;; readUint16

(define (test-literals-negreaduint16-negzero)

  (assert (equal? just(0:uint16) just(0:uint16))))

(define (test-literals-negreaduint16-negtypical)

  (assert (equal? just(1000:uint16) just(1000:uint16))))

(define (test-literals-negreaduint16-neginvalid)

  (assert (equal? nothing nothing)))

(define (test-literals-negreaduint16-negnegative)

  (assert (equal? nothing nothing)))

;; readUint32

(define (test-literals-negreaduint32-negzero)

  (assert (equal? just(0:uint32) just(0:uint32))))

(define (test-literals-negreaduint32-negtypical)

  (assert (equal? just(100000:uint32) just(100000:uint32))))

(define (test-literals-negreaduint32-neginvalid)

  (assert (equal? nothing nothing)))

(define (test-literals-negreaduint32-negnegative)

  (assert (equal? nothing nothing)))

;; readUint64

(define (test-literals-negreaduint64-negzero)

  (assert (equal? just(0:uint64) just(0:uint64))))

(define (test-literals-negreaduint64-negtypical)

  (assert (equal? just(1000000:uint64) just(1000000:uint64))))

(define (test-literals-negreaduint64-neginvalid)

  (assert (equal? nothing nothing)))

(define (test-literals-negreaduint64-negnegative)

  (assert (equal? nothing nothing)))

;; readBigint

(define (test-literals-negreadbigint-negpositive)

  (assert (equal? just(42:bigint) just(42:bigint))))

(define (test-literals-negreadbigint-negnegative)

  (assert (equal? just(-42:bigint) just(-42:bigint))))

(define (test-literals-negreadbigint-negzero)

  (assert (equal? just(0:bigint) just(0:bigint))))

(define (test-literals-negreadbigint-neglarge)

  (assert (equal? just(123456789012345678901234567890:bigint) just(123456789012345678901234567890:bigint))))

(define (test-literals-negreadbigint-neginvalid)

  (assert (equal? nothing nothing)))

;; readFloat32

(define (test-literals-negreadfloat32-negpositive)

  (assert (equal? just(3.14:float32) just(3.14:float32))))

(define (test-literals-negreadfloat32-negnegative)

  (assert (equal? just(-2.5:float32) just(-2.5:float32))))

(define (test-literals-negreadfloat32-neginvalid)

  (assert (equal? nothing nothing)))

;; readFloat64

(define (test-literals-negreadfloat64-negpositive)

  (assert (equal? just(3.14159:float64) just(3.14159:float64))))

(define (test-literals-negreadfloat64-negnegative)

  (assert (equal? just(-2.71828:float64) just(-2.71828:float64))))

(define (test-literals-negreadfloat64-neginvalid)

  (assert (equal? nothing nothing)))

;; readBigfloat

(define (test-literals-negreadbigfloat-negpositive)

  (assert (equal? just(3.14:bigfloat) just(3.14:bigfloat))))

(define (test-literals-negreadbigfloat-neginvalid)

  (assert (equal? nothing nothing)))

;; readBoolean

(define (test-literals-negreadboolean-negtrue)

  (assert (equal? just(true) just(true))))

(define (test-literals-negreadboolean-negfalse)

  (assert (equal? just(false) just(false))))

(define (test-literals-negreadboolean-neginvalid)

  (assert (equal? nothing nothing)))

;; readString

(define (test-literals-negreadstring-negquoted-string)

  (assert (equal? just("hello") just("hello"))))

(define (test-literals-negreadstring-negempty-quoted)

  (assert (equal? just("") just(""))))

(define (test-literals-negreadstring-negunquoted)

  (assert (equal? nothing nothing)))

;; stringToBinary

(define (test-literals-negstringtobinary-negsimple-base64)

  (assert (equal? [binary] [binary])))

(define (test-literals-negstringtobinary-negempty-string)

  (assert (equal? [binary] [binary])))

;; binaryToString

(define (test-literals-negbinarytostring-negsimple-binary)

  (assert (equal? "aGVsbG8=" "aGVsbG8=")))

(define (test-literals-negbinarytostring-negempty-binary)

  (assert (equal? "" "")))
