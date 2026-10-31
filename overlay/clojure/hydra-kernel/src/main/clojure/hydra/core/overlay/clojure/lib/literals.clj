(ns hydra.core.overlay.clojure.lib.literals
  (:import [java.util Base64]))

;; Numeric conversion functions (mostly identity in Clojure/JVM)

;; BigInt conversions
(def hydra_overlay_clojure_lib_literals_bigint_to_decimal (fn [x] (bigdec x)))
;; Signed conversions reduce modulo 2^width and reinterpret as two's-complement,
;; wrapping silently with no exception (e.g. 255 -> -1 for int8). The checked
;; forms (byte/short/int) throw IllegalArgumentException out of range, so use the
;; unchecked-* casts to honor the kernel's wrap-silently contract.
(def hydra_overlay_clojure_lib_literals_bigint_to_int8 (fn [x] (unchecked-byte x)))
(def hydra_overlay_clojure_lib_literals_bigint_to_int16 (fn [x] (unchecked-short x)))
(def hydra_overlay_clojure_lib_literals_bigint_to_int32 (fn [x] (unchecked-int x)))
(def hydra_overlay_clojure_lib_literals_bigint_to_int64 (fn [x] (unchecked-long x)))
(def hydra_overlay_clojure_lib_literals_bigint_to_uint8 (fn [x] (int x)))
(def hydra_overlay_clojure_lib_literals_bigint_to_uint16 (fn [x] (int x)))
(def hydra_overlay_clojure_lib_literals_bigint_to_uint32 (fn [x] (long x)))
;; Same uint64 caveat as uint64_to_bigint below: (long 2^64-1) narrows to -1.
;; Keep the value as a bigint so the full unsigned range survives. uint8/16/32
;; above all fit a signed long and are left as-is.
(def hydra_overlay_clojure_lib_literals_bigint_to_uint64 (fn [x] (bigint x)))

;; Decimal conversions
(def hydra_overlay_clojure_lib_literals_decimal_to_bigint (fn [x] (.toBigInteger (.setScale (bigdec x) 0 java.math.RoundingMode/HALF_EVEN))))
(def hydra_overlay_clojure_lib_literals_decimal_to_float32 (fn [x] (.floatValue (bigdec x))))
(def hydra_overlay_clojure_lib_literals_decimal_to_float64 (fn [x] (double (.doubleValue (bigdec x)))))

;; Float conversions
;; The JDK float string minus a trailing ".0" (as main's Java Float64ToDecimal does), so a whole value converts at
;; scale 0 (2.0 -> 2, not 2.0): print_decimal is scale-preserving. Float/toString and Double/toString are not
;; shortest-round-trip before JDK 19 (JDK-4511638), and NaN/Infinity are unguarded: both are open (#783 proposal).
(defn- float-string-to-decimal [s]
  (bigdec (if (.endsWith ^String s ".0") (subs s 0 (- (count s) 2)) s)))
(def hydra_overlay_clojure_lib_literals_float32_to_decimal (fn [x] (float-string-to-decimal (Float/toString (float x)))))
(def hydra_overlay_clojure_lib_literals_float32_to_float64 (fn [x] (double x)))
(def hydra_overlay_clojure_lib_literals_float64_to_decimal (fn [x] (float-string-to-decimal (Double/toString (double x)))))
(def hydra_overlay_clojure_lib_literals_float64_to_float32 (fn [x] (float x)))

;; Integer to BigInt conversions
(def hydra_overlay_clojure_lib_literals_int8_to_bigint (fn [x] (long x)))
(def hydra_overlay_clojure_lib_literals_int16_to_bigint (fn [x] (long x)))
(def hydra_overlay_clojure_lib_literals_int32_to_bigint (fn [x] (long x)))
(def hydra_overlay_clojure_lib_literals_int64_to_bigint (fn [x] (long x)))
(def hydra_overlay_clojure_lib_literals_uint8_to_bigint (fn [x] (long x)))
(def hydra_overlay_clojure_lib_literals_uint16_to_bigint (fn [x] (long x)))
(def hydra_overlay_clojure_lib_literals_uint32_to_bigint (fn [x] (long x)))
;; uint64 is the one unsigned width that does NOT fit in a signed long: (long 2^64-1)
;; narrows to -1. Pass the value through unchanged (it is already a bigint here), matching
;; the Python/Common Lisp/Scheme overlays. The uint8/16/32 casts above are safe as-is.
(def hydra_overlay_clojure_lib_literals_uint64_to_bigint (fn [x] (bigint x)))

;; Binary <-> String (base64)
;; Binary is a vector of unsigned ints 0..255 (see files/text bytes->binary), so bytes >127 must
;; go through unchecked-byte (plain `byte` throws "Value out of range" on 128..255). #524.
(def hydra_overlay_clojure_lib_literals_binary_to_base64
  (fn [bs]
    (let [ba (byte-array (map unchecked-byte bs))]
      (.encodeToString (Base64/getEncoder) ba))))

(def hydra_overlay_clojure_lib_literals_base64_to_binary
  (fn [s]
    (try
      (let [bytes (.decode (Base64/getDecoder) (.getBytes s "UTF-8"))]
        (or (seq bytes) []))
      (catch Exception _ []))))

;; Read functions (return Maybe)
(def hydra_overlay_clojure_lib_literals_parse_bigint
  (fn [s] (try (list :given (bigint (BigInteger. s)))
               (catch Exception _ (list :none)))))

(def hydra_overlay_clojure_lib_literals_read_float
  (fn [s] (try (list :given (Double/parseDouble s))
               (catch Exception _ (list :none)))))

(def hydra_overlay_clojure_lib_literals_parse_float32
  (fn [s] (try (list :given (Float/parseFloat s))
               (catch Exception _ (list :none)))))

(def hydra_overlay_clojure_lib_literals_read_int
  (fn [s] (try (list :given (Long/parseLong s))
               (catch Exception _ (list :none)))))

(def hydra_overlay_clojure_lib_literals_parse_int64
  (fn [s] (try (list :given (Long/parseLong s))
               (catch Exception _ (list :none)))))

(def hydra_overlay_clojure_lib_literals_read_uint
  (fn [s] (try (let [n (Long/parseLong s)]
                 (if (>= n 0) (list :given n) (list :none)))
               (catch Exception _ (list :none)))))

(def hydra_overlay_clojure_lib_literals_parse_uint32
  (fn [s] (try (let [n (Long/parseLong s)]
                 (if (and (>= n 0) (<= n 4294967295)) (list :given n) (list :none)))
               (catch Exception _ (list :none)))))

(def hydra_overlay_clojure_lib_literals_parse_uint64
  (fn [s] (try (let [n (BigInteger. ^String s)]
                 (if (and (>= (.signum n) 0)
                          (<= (.compareTo n (BigInteger. "18446744073709551615")) 0))
                   ;; bigint, not .longValue: the latter narrows 2^64-1 to -1
                   (list :given (bigint n))
                   (list :none)))
               (catch Exception _ (list :none)))))

;; Shortest significant digits that round-trip, as [digit-string exponent].
;; Double/toString and Float/toString are NOT shortest-round-trip for subnormals
;; on JDK < 19 (JDK-4511638: Double.MIN_VALUE prints as 4.9E-324), so search
;; increasing precisions with String/format and verify each by reparsing.
(defn- shortest-digits [abs-d float32?]
  (loop [n 1]
    (let [s (String/format java.util.Locale/ROOT (str "%." (dec n) "e") (object-array [abs-d]))
          ok (or (= n 17)
                 (if float32?
                   (= (Float/parseFloat s) (.floatValue (Double. abs-d)))
                   (= (Double/parseDouble s) abs-d)))]
      (if ok
        (let [[_ mant exp-str] (re-matches #"([\d.]+)e([+-]?\d+)" s)
              digits (.replace ^String mant "." "")
              sig (loop [i (count digits)]
                    (if (and (> i 1) (= \0 (.charAt ^String digits (dec i))))
                      (recur (dec i))
                      (subs digits 0 i)))]
          [sig (Integer/parseInt exp-str)])
        (recur (inc n))))))

;; Haskell-compatible float formatting
(defn- haskell-show-float
  ([x] (haskell-show-float x false))
  ([x float32?]
   (let [d (double x)]
     (cond
       (Double/isNaN d) "NaN"
       (Double/isInfinite d) (if (pos? d) "Infinity" "-Infinity")
       (zero? d) (if (neg? (Double/doubleToRawLongBits d)) "-0.0" "0.0")
       :else
       (let [abs-d (Math/abs d)
             neg? (neg? d)]
         (if (and (>= abs-d 0.1) (< abs-d 1.0E7))
           ;; Fixed notation
           (let [s (str d)]
             (if (.contains s ".") s (str s ".0")))
           ;; Scientific notation (Haskell style: X.YeN).
           (let [[sig e] (shortest-digits abs-d float32?)
                 out (if (= 1 (count sig))
                       (str sig ".0")
                       (str (subs sig 0 1) "." (subs sig 1)))]
             (str (if neg? "-" "") out "e" e))))))))

;; Show functions
(def hydra_overlay_clojure_lib_literals_print_bigint (fn [x] (str x)))
(def hydra_overlay_clojure_lib_literals_show_float (fn [x] (haskell-show-float x)))
(def hydra_overlay_clojure_lib_literals_print_float32
  (fn [x]
    ;; Handle NaN/Inf before casting (Clojure's `float` throws on Infinity)
    (let [dx (double x)]
      (cond
        (Double/isNaN dx) "NaN"
        (Double/isInfinite dx) (if (pos? dx) "Infinity" "-Infinity")
        :else
          ;; Parse at float precision then format with Haskell rules
          (let [s (Float/toString (float dx))
                d (Double/parseDouble s)]
            (haskell-show-float d true))))))
(def hydra_overlay_clojure_lib_literals_print_float64 (fn [x] (haskell-show-float x)))
(def hydra_overlay_clojure_lib_literals_show_int (fn [x] (str x)))
(def hydra_overlay_clojure_lib_literals_print_int8 (fn [x] (str x)))
(def hydra_overlay_clojure_lib_literals_print_int16 (fn [x] (str x)))
(def hydra_overlay_clojure_lib_literals_print_int32 (fn [x] (str x)))
(def hydra_overlay_clojure_lib_literals_print_int64 (fn [x] (str x)))
(def hydra_overlay_clojure_lib_literals_show_uint (fn [x] (str x)))
(def hydra_overlay_clojure_lib_literals_print_uint8 (fn [x] (str x)))
(def hydra_overlay_clojure_lib_literals_print_uint16 (fn [x] (str x)))
(def hydra_overlay_clojure_lib_literals_print_uint32 (fn [x] (str x)))
(def hydra_overlay_clojure_lib_literals_print_uint64 (fn [x] (str x)))

;; print_string :: String -> String  (Haskell-compatible quoted with escapes)
(def hydra_overlay_clojure_lib_literals_print_string
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
(def hydra_overlay_clojure_lib_literals_binary_to_bytes
  (fn [bs] (map #(bit-and % 0xFF) bs)))

;; parse_boolean :: String -> Maybe Bool
(def hydra_overlay_clojure_lib_literals_parse_boolean
  (fn [s]
    (cond
      (= s "true") (list :given true)
      (= s "false") (list :given false)
      :else (list :none))))

;; parse_decimal :: String -> Maybe Decimal
(def hydra_overlay_clojure_lib_literals_parse_decimal
  (fn [s] (try (list :given (bigdec s))
               (catch Exception _ (list :none)))))

;; print_decimal :: Decimal -> String
;; Unlike a float literal (which would show "42.0", "1.0e20", "1.0e-10"),
;; printDecimal never adds a trailing ".0" to a whole value -- decimals
;; track scale, not float-style presentation -- and its positional range is
;; wider (adjusted exponent -6 <= a < 21; overlay/haskell/.../Literals.hs)
;; than Double's own show threshold, so e.g. "0.01"/"0.001" print plainly
;; rather than in scientific form.
(def hydra_overlay_clojure_lib_literals_print_decimal
  ;; Scale-preserving, mirroring the Java PrintDecimal: 1.10 prints "1.10", not "1.1" -- decimals are
  ;; scale-sensitive (equal/compare tiebreak on scale), so stripping trailing zeros would break the
  ;; round-trip through parseDecimal back to the original value.
  (fn [x]
    (let [bd (bigdec x)
          scale (.scale bd)
          digits (.toString (.abs (.unscaledValue bd)))
          n (count digits)
          sign (if (neg? (.signum bd)) "-" "")
          e (- n scale 1)]
      (if (or (>= e 21) (< e -6))
        (let [mantissa (if (= n 1)
                         (str digits ".0")
                         (str (subs digits 0 1) "." (subs digits 1)))]
          (str sign mantissa "e" e))
        (str sign
             (cond
               (<= scale 0) (str digits (apply str (repeat (- scale) "0")))
               (< scale n) (str (subs digits 0 (- n scale)) "." (subs digits (- n scale)))
               :else (str "0." (apply str (repeat (- scale n) "0")) digits)))))))

;; parse_float64 :: String -> Maybe Float64
(def hydra_overlay_clojure_lib_literals_parse_float64
  (fn [s] (try (list :given (Double/parseDouble s))
               (catch Exception _ (list :none)))))

;; parse_int8 :: String -> Maybe Int8
(def hydra_overlay_clojure_lib_literals_parse_int8
  (fn [s] (try (list :given (Byte/parseByte s))
               (catch Exception _ (list :none)))))

;; parse_int16 :: String -> Maybe Int16
(def hydra_overlay_clojure_lib_literals_parse_int16
  (fn [s] (try (list :given (Short/parseShort s))
               (catch Exception _ (list :none)))))

;; parse_int32 :: String -> Maybe Int32
(def hydra_overlay_clojure_lib_literals_parse_int32
  (fn [s] (try (list :given (Integer/parseInt s))
               (catch Exception _ (list :none)))))

;; parse_string :: String -> Maybe String
;; Haskell semantics: reads a quoted string literal, returns Nothing for unquoted
(def hydra_overlay_clojure_lib_literals_parse_string
  (fn [s]
    (if (and (.startsWith s "\"") (.endsWith s "\"") (>= (count s) 2))
      (let [inner (subs s 1 (dec (count s)))]
        ;; Simple unescape: handle \\, \", \n, \t, \r
        (let [sb (StringBuilder.)
              len (count inner)]
          (loop [i 0]
            (if (>= i len)
              (list :given (.toString sb))
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
      (list :none))))

;; parse_uint8 :: String -> Maybe Uint8
(def hydra_overlay_clojure_lib_literals_parse_uint8
  (fn [s] (try (let [n (Integer/parseInt s)]
                 (if (and (>= n 0) (<= n 255)) (list :given n) (list :none)))
               (catch Exception _ (list :none)))))

;; parse_uint16 :: String -> Maybe Uint16
(def hydra_overlay_clojure_lib_literals_parse_uint16
  (fn [s] (try (let [n (Integer/parseInt s)]
                 (if (and (>= n 0) (<= n 65535)) (list :given n) (list :none)))
               (catch Exception _ (list :none)))))

;; print_boolean :: Bool -> String
(def hydra_overlay_clojure_lib_literals_print_boolean (fn [x] (if x "true" "false")))

;; Precision functions (identity in Clojure)
(def hydra_overlay_clojure_lib_literals_float (fn [_prec] (fn [x] (double x))))
(def hydra_overlay_clojure_lib_literals_int (fn [_prec] (fn [x] (long x))))
(def hydra_overlay_clojure_lib_literals_uint (fn [_prec] (fn [x] (long x))))
