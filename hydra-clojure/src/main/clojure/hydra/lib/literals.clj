(ns hydra.lib.literals
  (:import [java.util Base64]))

;; Numeric conversion functions (mostly identity in Clojure/JVM)

;; BigFloat conversions
;; Haskell's round: half-to-even (banker's rounding)
(def hydra_lib_literals_bigfloat_to_bigint
  (fn [x] (long (.setScale (BigDecimal/valueOf (double x)) 0 java.math.RoundingMode/HALF_EVEN))))
(def hydra_lib_literals_bigfloat_to_float (fn [x] (double x)))
(def hydra_lib_literals_bigfloat_to_float32 (fn [x] (float x)))
(def hydra_lib_literals_bigfloat_to_float64 (fn [x] (double x)))

;; BigInt conversions
(def hydra_lib_literals_bigint_to_bigfloat (fn [x] (double x)))
(def hydra_lib_literals_bigint_to_int8 (fn [x] (byte x)))
(def hydra_lib_literals_bigint_to_int16 (fn [x] (short x)))
(def hydra_lib_literals_bigint_to_int32 (fn [x] (int x)))
(def hydra_lib_literals_bigint_to_int64 (fn [x] (long x)))
(def hydra_lib_literals_bigint_to_uint8 (fn [x] (int x)))
(def hydra_lib_literals_bigint_to_uint16 (fn [x] (int x)))
(def hydra_lib_literals_bigint_to_uint32 (fn [x] (long x)))
(def hydra_lib_literals_bigint_to_uint64 (fn [x] (long x)))

;; Float conversions
(def hydra_lib_literals_float32_to_bigfloat (fn [x] (double x)))
(def hydra_lib_literals_float64_to_bigfloat (fn [x] (double x)))

;; Integer to BigInt conversions
(def hydra_lib_literals_int8_to_bigint (fn [x] (long x)))
(def hydra_lib_literals_int16_to_bigint (fn [x] (long x)))
(def hydra_lib_literals_int32_to_bigint (fn [x] (long x)))
(def hydra_lib_literals_int64_to_bigint (fn [x] (long x)))
(def hydra_lib_literals_uint8_to_bigint (fn [x] (long x)))
(def hydra_lib_literals_uint16_to_bigint (fn [x] (long x)))
(def hydra_lib_literals_uint32_to_bigint (fn [x] (long x)))
(def hydra_lib_literals_uint64_to_bigint (fn [x] (long x)))

;; Binary <-> String (base64)
(def hydra_lib_literals_binary_to_string
  (fn [bs]
    (let [ba (byte-array (map byte bs))]
      (.encodeToString (Base64/getEncoder) ba))))

(def hydra_lib_literals_string_to_binary
  (fn [s]
    (try
      (let [bytes (.decode (Base64/getDecoder) (.getBytes s "UTF-8"))]
        (or (seq bytes) []))
      (catch Exception _ []))))

;; Read functions (return Maybe)
(def hydra_lib_literals_read_bigfloat
  (fn [s] (try (list :just (Double/parseDouble s))
               (catch Exception _ (list :nothing)))))

(def hydra_lib_literals_read_bigint
  (fn [s] (try (list :just (bigint (BigInteger. s)))
               (catch Exception _ (list :nothing)))))

(def hydra_lib_literals_read_float
  (fn [s] (try (list :just (Double/parseDouble s))
               (catch Exception _ (list :nothing)))))

(def hydra_lib_literals_read_float32
  (fn [s] (try (list :just (Float/parseFloat s))
               (catch Exception _ (list :nothing)))))

(def hydra_lib_literals_read_int
  (fn [s] (try (list :just (Long/parseLong s))
               (catch Exception _ (list :nothing)))))

(def hydra_lib_literals_read_int64
  (fn [s] (try (list :just (Long/parseLong s))
               (catch Exception _ (list :nothing)))))

(def hydra_lib_literals_read_uint
  (fn [s] (try (let [n (Long/parseLong s)]
                 (if (>= n 0) (list :just n) (list :nothing)))
               (catch Exception _ (list :nothing)))))

(def hydra_lib_literals_read_uint32
  (fn [s] (try (let [n (Long/parseLong s)]
                 (if (>= n 0) (list :just n) (list :nothing)))
               (catch Exception _ (list :nothing)))))

(def hydra_lib_literals_read_uint64
  (fn [s] (try (let [n (Long/parseLong s)]
                 (if (>= n 0) (list :just n) (list :nothing)))
               (catch Exception _ (list :nothing)))))

;; Haskell-compatible float formatting
(defn- haskell-show-float [x]
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
(def hydra_lib_literals_show_bigfloat (fn [x] (haskell-show-float x)))
(def hydra_lib_literals_show_bigint (fn [x] (str x)))
(def hydra_lib_literals_show_float (fn [x] (haskell-show-float x)))
(def hydra_lib_literals_show_float32
  (fn [x]
    ;; Parse at float precision then format with Haskell rules
    (let [s (Float/toString (float x))
          d (Double/parseDouble s)]
      (haskell-show-float d))))
(def hydra_lib_literals_show_float64 (fn [x] (haskell-show-float x)))
(def hydra_lib_literals_show_int (fn [x] (str x)))
(def hydra_lib_literals_show_int8 (fn [x] (str x)))
(def hydra_lib_literals_show_int16 (fn [x] (str x)))
(def hydra_lib_literals_show_int32 (fn [x] (str x)))
(def hydra_lib_literals_show_int64 (fn [x] (str x)))
(def hydra_lib_literals_show_uint (fn [x] (str x)))
(def hydra_lib_literals_show_uint8 (fn [x] (str x)))
(def hydra_lib_literals_show_uint16 (fn [x] (str x)))
(def hydra_lib_literals_show_uint32 (fn [x] (str x)))
(def hydra_lib_literals_show_uint64 (fn [x] (str x)))

;; show_string :: String -> String  (Haskell-compatible quoted with escapes)
(def hydra_lib_literals_show_string
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

;; binary_to_bytes :: Binary -> [Int32]
(def hydra_lib_literals_binary_to_bytes
  (fn [bs] (map #(bit-and % 0xFF) bs)))

;; read_boolean :: String -> Maybe Bool
(def hydra_lib_literals_read_boolean
  (fn [s]
    (cond
      (= s "true") (list :just true)
      (= s "false") (list :just false)
      :else (list :nothing))))

;; read_float64 :: String -> Maybe Float64
(def hydra_lib_literals_read_float64
  (fn [s] (try (list :just (Double/parseDouble s))
               (catch Exception _ (list :nothing)))))

;; read_int8 :: String -> Maybe Int8
(def hydra_lib_literals_read_int8
  (fn [s] (try (list :just (Byte/parseByte s))
               (catch Exception _ (list :nothing)))))

;; read_int16 :: String -> Maybe Int16
(def hydra_lib_literals_read_int16
  (fn [s] (try (list :just (Short/parseShort s))
               (catch Exception _ (list :nothing)))))

;; read_int32 :: String -> Maybe Int32
(def hydra_lib_literals_read_int32
  (fn [s] (try (list :just (Integer/parseInt s))
               (catch Exception _ (list :nothing)))))

;; read_string :: String -> Maybe String
;; Haskell semantics: reads a quoted string literal, returns Nothing for unquoted
(def hydra_lib_literals_read_string
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

;; read_uint8 :: String -> Maybe Uint8
(def hydra_lib_literals_read_uint8
  (fn [s] (try (let [n (Integer/parseInt s)]
                 (if (and (>= n 0) (<= n 255)) (list :just n) (list :nothing)))
               (catch Exception _ (list :nothing)))))

;; read_uint16 :: String -> Maybe Uint16
(def hydra_lib_literals_read_uint16
  (fn [s] (try (let [n (Integer/parseInt s)]
                 (if (and (>= n 0) (<= n 65535)) (list :just n) (list :nothing)))
               (catch Exception _ (list :nothing)))))

;; show_boolean :: Bool -> String
(def hydra_lib_literals_show_boolean (fn [x] (if x "true" "false")))

;; Precision functions (identity in Clojure)
(def hydra_lib_literals_float (fn [_prec] (fn [x] (double x))))
(def hydra_lib_literals_int (fn [_prec] (fn [x] (long x))))
(def hydra_lib_literals_uint (fn [_prec] (fn [x] (long x))))
