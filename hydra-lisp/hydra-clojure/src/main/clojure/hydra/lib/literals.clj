(ns hydra.lib.literals
  (:import [java.util Base64]))

;; Numeric conversion functions (mostly identity in Clojure/JVM)

;; BigFloat conversions
;; Haskell's round: half-to-even (banker's rounding)
(def hydra_lib_literals_bigfloat_to_bigint
  "Convert a bigfloat (Double) to a bigint (Integer)."
  (fn [x] (long (.setScale (BigDecimal/valueOf (double x)) 0 java.math.RoundingMode/HALF_EVEN))))
(def hydra_lib_literals_bigfloat_to_float
  "Convert a bigfloat (Double) to a float."
  (fn [x] (double x)))
(def hydra_lib_literals_bigfloat_to_float32
  "Convert a bigfloat (Double) to a float32 (Float)."
  (fn [x] (float x)))
(def hydra_lib_literals_bigfloat_to_float64
  "Convert a bigfloat (Double) to a float64 (Double)."
  (fn [x] (double x)))

;; BigInt conversions
(def hydra_lib_literals_bigint_to_bigfloat
  "Convert a bigint (Integer) to a bigfloat (Double)."
  (fn [x] (double x)))
(def hydra_lib_literals_bigint_to_int8
  "Convert a bigint (Integer) to an int8."
  (fn [x] (byte x)))
(def hydra_lib_literals_bigint_to_int16
  "Convert a bigint (Integer) to an int16."
  (fn [x] (short x)))
(def hydra_lib_literals_bigint_to_int32
  "Convert a bigint (Integer) to an int32."
  (fn [x] (int x)))
(def hydra_lib_literals_bigint_to_int64
  "Convert a bigint (Integer) to an int64."
  (fn [x] (long x)))
(def hydra_lib_literals_bigint_to_uint8
  "Convert a bigint (Integer) to a uint8."
  (fn [x] (int x)))
(def hydra_lib_literals_bigint_to_uint16
  "Convert a bigint (Integer) to a uint16."
  (fn [x] (int x)))
(def hydra_lib_literals_bigint_to_uint32
  "Convert a bigint (Integer) to a uint32."
  (fn [x] (long x)))
(def hydra_lib_literals_bigint_to_uint64
  "Convert a bigint (Integer) to a uint64."
  (fn [x] (long x)))

;; Binary <-> String (base64)
(def hydra_lib_literals_binary_to_bytes
  "Convert binary to a list of byte values (0-255)."
  (fn [bs] (map #(bit-and % 0xFF) bs)))

(def hydra_lib_literals_binary_to_string
  "Convert binary to string by base64 encoding."
  (fn [bs]
    (let [ba (byte-array (map byte bs))]
      (.encodeToString (Base64/getEncoder) ba))))

;; Float conversions
(def hydra_lib_literals_float32_to_bigfloat
  "Convert a float32 (Float) to a bigfloat (Double)."
  (fn [x] (double x)))
(def hydra_lib_literals_float64_to_bigfloat
  "Convert a float64 (Double) to a bigfloat (Double)."
  (fn [x] (double x)))

;; Precision functions (identity in Clojure)
(def hydra_lib_literals_float
  "Apply a floating-point precision constraint."
  (fn [_prec] (fn [x] (double x))))

;; Integer to BigInt conversions
(def hydra_lib_literals_int
  "Apply an integer precision constraint."
  (fn [_prec] (fn [x] (long x))))
(def hydra_lib_literals_int8_to_bigint
  "Convert an int8 to a bigint (Integer)."
  (fn [x] (long x)))
(def hydra_lib_literals_int16_to_bigint
  "Convert an int16 to a bigint (Integer)."
  (fn [x] (long x)))
(def hydra_lib_literals_int32_to_bigint
  "Convert an int32 to a bigint (Integer)."
  (fn [x] (long x)))
(def hydra_lib_literals_int64_to_bigint
  "Convert an int64 to a bigint (Integer)."
  (fn [x] (long x)))

;; Read functions (return Maybe)
(def hydra_lib_literals_read_bigfloat
  "Parse a string to a bigfloat (Double)."
  (fn [s] (try (list :just (Double/parseDouble s))
               (catch Exception _ (list :nothing)))))

(def hydra_lib_literals_read_bigint
  "Parse a string to a bigint (Integer)."
  (fn [s] (try (list :just (bigint (BigInteger. s)))
               (catch Exception _ (list :nothing)))))

;; read_boolean :: String -> Maybe Bool
(def hydra_lib_literals_read_boolean
  "Parse a string to a boolean."
  (fn [s]
    (cond
      (= s "true") (list :just true)
      (= s "false") (list :just false)
      :else (list :nothing))))

(def hydra_lib_literals_read_float
  "Parse a string to a float (Double)."
  (fn [s] (try (list :just (Double/parseDouble s))
               (catch Exception _ (list :nothing)))))

(def hydra_lib_literals_read_float32
  "Parse a string to a float32 (Float)."
  (fn [s] (try (list :just (Float/parseFloat s))
               (catch Exception _ (list :nothing)))))

;; read_float64 :: String -> Maybe Float64
(def hydra_lib_literals_read_float64
  "Parse a string to a float64 (Double)."
  (fn [s] (try (list :just (Double/parseDouble s))
               (catch Exception _ (list :nothing)))))

(def hydra_lib_literals_read_int
  "Parse a string to an int (Long)."
  (fn [s] (try (list :just (Long/parseLong s))
               (catch Exception _ (list :nothing)))))

;; read_int8 :: String -> Maybe Int8
(def hydra_lib_literals_read_int8
  "Parse a string to an int8 (-128 to 127)."
  (fn [s] (try (list :just (Byte/parseByte s))
               (catch Exception _ (list :nothing)))))

;; read_int16 :: String -> Maybe Int16
(def hydra_lib_literals_read_int16
  "Parse a string to an int16 (-32768 to 32767)."
  (fn [s] (try (list :just (Short/parseShort s))
               (catch Exception _ (list :nothing)))))

;; read_int32 :: String -> Maybe Int32
(def hydra_lib_literals_read_int32
  "Parse a string to an int32."
  (fn [s] (try (list :just (Integer/parseInt s))
               (catch Exception _ (list :nothing)))))

(def hydra_lib_literals_read_int64
  "Parse a string to an int64."
  (fn [s] (try (list :just (Long/parseLong s))
               (catch Exception _ (list :nothing)))))

;; read_string :: String -> Maybe String
;; Haskell semantics: reads a quoted string literal, returns Nothing for unquoted
(def hydra_lib_literals_read_string
  "Parse a string literal."
  (fn [s]
    (if (and (.startsWith s "\"") (.endsWith s "\"") (>= (count s) 2))
      (let [inner (subs s 1 (dec (count s)))]
        ;; Simple unescape: handle \\, \", \n, \t, \r
        (let [sb (StringBuilder.)
              len (count inner)]
          (loop [i 0]
            (if (>= i len)
              (list :just (.toString sb))
              (if (and (= (.charAt inner i) \\) (< (inc i) len))
                (let [c (.charAt inner (inc i))]
                  (case c
                    \\ (do (.append sb \\) (recur (+ i 2)))
                    \" (do (.append sb \") (recur (+ i 2)))
                    \n (do (.append sb \newline) (recur (+ i 2)))
                    \t (do (.append sb \tab) (recur (+ i 2)))
                    \r (do (.append sb \return) (recur (+ i 2)))
                    (do (.append sb \\) (.append sb c) (recur (+ i 2)))))
                (do (.append sb (.charAt inner i)) (recur (inc i))))))))
      (list :nothing))))

(def hydra_lib_literals_read_uint
  "Parse a string to an unsigned int."
  (fn [s] (try (let [n (Long/parseLong s)]
                 (if (>= n 0) (list :just n) (list :nothing)))
               (catch Exception _ (list :nothing)))))

;; read_uint8 :: String -> Maybe Uint8
(def hydra_lib_literals_read_uint8
  "Parse a string to a uint8 (0 to 255)."
  (fn [s] (try (let [n (Integer/parseInt s)]
                 (if (and (>= n 0) (<= n 255)) (list :just n) (list :nothing)))
               (catch Exception _ (list :nothing)))))

;; read_uint16 :: String -> Maybe Uint16
(def hydra_lib_literals_read_uint16
  "Parse a string to a uint16 (0 to 65535)."
  (fn [s] (try (let [n (Integer/parseInt s)]
                 (if (and (>= n 0) (<= n 65535)) (list :just n) (list :nothing)))
               (catch Exception _ (list :nothing)))))

(def hydra_lib_literals_read_uint32
  "Parse a string to a uint32 (0 to 4294967295)."
  (fn [s] (try (let [n (Long/parseLong s)]
                 (if (>= n 0) (list :just n) (list :nothing)))
               (catch Exception _ (list :nothing)))))

(def hydra_lib_literals_read_uint64
  "Parse a string to a uint64 (0 to 18446744073709551615)."
  (fn [s] (try (let [n (BigInteger. ^String s)]
                 (if (and (>= (.signum n) 0)
                          (<= (.compareTo n (BigInteger. "18446744073709551615")) 0))
                   (list :just (.longValue n))
                   (list :nothing)))
               (catch Exception _ (list :nothing)))))

;; Haskell-compatible float formatting
(defn- haskell-show-float
  "Format a floating-point number in Haskell-compatible style."
  [x]
  (let [d (double x)]
    (cond
      (Double/isNaN d) "NaN"
      (Double/isInfinite d) (if (pos? d) "Infinity" "-Infinity")
      (zero? d) "0.0"
      :else
      (let [abs-d (Math/abs d)
            neg? (neg? d)]
        (if (and (>= abs-d 0.1) (< abs-d 1.0E7))
          ;; Fixed notation
          (let [s (str d)]
            (if (.contains s ".") s (str s ".0")))
          ;; Scientific notation (Haskell style: X.YeN)
          (let [s (String/format "%.15e" (object-array [(double d)]))
                ;; Parse mantissa and exponent
                [_ mant exp-str] (re-matches #"(-?\d+\.\d+)e([+-]?\d+)" s)
                exp (Integer/parseInt exp-str)
                ;; Normalize: keep one digit before decimal
                mant-d (Double/parseDouble mant)
                ;; Remove trailing zeros from mantissa
                mant-s (loop [m (str mant-d)]
                         (if (and (.endsWith m "0") (not (.endsWith m ".0")))
                           (recur (subs m 0 (dec (count m))))
                           m))
                ;; Ensure at least one decimal place
                mant-s (if (.contains mant-s ".") mant-s (str mant-s ".0"))]
            (str mant-s "e" exp)))))))

;; Show functions
(def hydra_lib_literals_show_bigfloat
  "Convert a bigfloat (Double) to string."
  (fn [x] (haskell-show-float x)))
(def hydra_lib_literals_show_bigint
  "Convert a bigint (Integer) to string."
  (fn [x] (str x)))
(def hydra_lib_literals_show_boolean
  "Convert a boolean to string."
  (fn [x] (if x "true" "false")))
(def hydra_lib_literals_show_float
  "Convert a float to string."
  (fn [x] (haskell-show-float x)))
(def hydra_lib_literals_show_float32
  "Convert a float32 (Float) to string."
  (fn [x]
    ;; Parse at float precision then format with Haskell rules
    (let [s (Float/toString (float x))
          d (Double/parseDouble s)]
      (haskell-show-float d))))
(def hydra_lib_literals_show_float64
  "Convert a float64 (Double) to string."
  (fn [x] (haskell-show-float x)))
(def hydra_lib_literals_show_int
  "Convert an int to string."
  (fn [x] (str x)))
(def hydra_lib_literals_show_int8
  "Convert an int8 to string."
  (fn [x] (str x)))
(def hydra_lib_literals_show_int16
  "Convert an int16 to string."
  (fn [x] (str x)))
(def hydra_lib_literals_show_int32
  "Convert an int32 to string."
  (fn [x] (str x)))
(def hydra_lib_literals_show_int64
  "Convert an int64 to string."
  (fn [x] (str x)))

;; show_string :: String -> String  (Haskell-compatible quoted with escapes)
(def hydra_lib_literals_show_string
  "Convert a string to a quoted string representation."
  (fn [s]
    (let [sb (StringBuilder.)
          _ (.append sb "\"")
          len (.length s)]
      (loop [i 0]
        (when (< i len)
          (let [cp (.codePointAt s i)
                chars (Character/charCount cp)]
            (cond
              (= cp 0)   (.append sb "\\NUL")
              (= cp 7)   (.append sb "\\a")
              (= cp 8)   (.append sb "\\b")
              (= cp 9)   (.append sb "\\t")
              (= cp 10)  (.append sb "\\n")
              (= cp 11)  (.append sb "\\v")
              (= cp 12)  (.append sb "\\f")
              (= cp 13)  (.append sb "\\r")
              (= cp 92)  (.append sb "\\\\")
              (= cp 34)  (.append sb "\\\"")
              (= cp 127) (.append sb "\\DEL")
              (and (>= cp 32) (<= cp 126)) (.append sb (char cp))
              :else (.append sb (str "\\" cp)))
            (recur (+ i chars)))))
      (.append sb "\"")
      (.toString sb))))

(def hydra_lib_literals_show_uint
  "Convert a uint to string."
  (fn [x] (str x)))
(def hydra_lib_literals_show_uint8
  "Convert a uint8 to string."
  (fn [x] (str x)))
(def hydra_lib_literals_show_uint16
  "Convert a uint16 to string."
  (fn [x] (str x)))
(def hydra_lib_literals_show_uint32
  "Convert a uint32 to string."
  (fn [x] (str x)))
(def hydra_lib_literals_show_uint64
  "Convert a uint64 to string."
  (fn [x] (str x)))

(def hydra_lib_literals_string_to_binary
  "Convert string to binary by base64 decoding."
  (fn [s]
    (try
      (let [bytes (.decode (Base64/getDecoder) (.getBytes s "UTF-8"))]
        (or (seq bytes) []))
      (catch Exception _ []))))

;; Unsigned integer to BigInt conversions
(def hydra_lib_literals_uint
  "Apply an unsigned integer precision constraint."
  (fn [_prec] (fn [x] (long x))))
(def hydra_lib_literals_uint8_to_bigint
  "Convert a uint8 to a bigint (Integer)."
  (fn [x] (long x)))
(def hydra_lib_literals_uint16_to_bigint
  "Convert a uint16 to a bigint (Integer)."
  (fn [x] (long x)))
(def hydra_lib_literals_uint32_to_bigint
  "Convert a uint32 to a bigint (Integer)."
  (fn [x] (long x)))
(def hydra_lib_literals_uint64_to_bigint
  "Convert a uint64 to a bigint (Integer)."
  (fn [x] (long x)))
