(ns hydra.overlay.clojure.lib.text
  (:import [java.nio ByteBuffer]
           [java.nio.charset StandardCharsets CharacterCodingException CodingErrorAction]))

;; Clojure implementations of hydra.lib.text primitives (#494).
;;
;; UTF-8 codec primitives bridging Hydra strings and raw bytes (binary). These pair with
;; hydra.lib.files.readFile / writeFile, which are byte-oriented.
;;
;; Runtime representations (Clojure target):
;;   Either        : (list :left v) | (list :right v)
;;   binary        : a Clojure vector/seq of byte ints (each 0..255), matching the
;;                   Clojure target's binary representation (see hydra.overlay.clojure.lib.literals).

;; binary (vector of ints 0..255) -> Java byte-array.
(defn- binary->bytes [data]
  (byte-array (map unchecked-byte data)))

;; Java byte-array -> binary (vector of unsigned ints 0..255).
(defn- bytes->binary [^bytes ba]
  (vec (map #(bit-and % 0xff) ba)))

;; decodeUtf8 :: binary -> Either<string, string>
;; Strict UTF-8 decode: returns (list :right text) on success, or (list :left message) when the
;; bytes are not valid UTF-8, where message is a host-provided description of the failure.
(def hydra_lib_text_decode_utf8
  "Decode a sequence of bytes as UTF-8 text (strict)."
  (fn [data]
    (let [decoder (doto (.newDecoder StandardCharsets/UTF_8)
                    (.onMalformedInput CodingErrorAction/REPORT)
                    (.onUnmappableCharacter CodingErrorAction/REPORT))]
      (try
        (list :right (.toString (.decode decoder (ByteBuffer/wrap (binary->bytes data)))))
        (catch CharacterCodingException e
          (let [m (.getMessage e)]
            (list :left (if (nil? m) "invalid UTF-8" m))))))))

;; encodeUtf8 :: string -> binary
;; Total: every Hydra string is valid Unicode and therefore always encodes.
(def hydra_lib_text_encode_utf8
  "Encode text as a sequence of UTF-8 bytes."
  (fn [text] (bytes->binary (.getBytes ^String text StandardCharsets/UTF_8))))
