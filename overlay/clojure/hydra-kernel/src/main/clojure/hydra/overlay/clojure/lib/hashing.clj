(ns hydra.overlay.clojure.lib.hashing
  (:import [java.security MessageDigest]))

;; Clojure implementations of hydra.lib.hashing primitives (#524).
;;
;; SHA-256 over raw bytes (binary). These pair with hydra.lib.files.readFile, which is
;; byte-oriented, to hash file contents. Pure and total.
;;
;; Runtime representations (Clojure target):
;;   binary        : a Clojure vector/seq of byte ints (each 0..255), matching the
;;                   Clojure target's binary representation (see hydra.overlay.clojure.lib.literals).

;; binary (vector of ints 0..255) -> Java byte-array.
(defn- binary->bytes [data]
  (byte-array (map unchecked-byte data)))

;; Java byte-array -> binary (vector of unsigned ints 0..255).
(defn- bytes->binary [^bytes ba]
  (vec (map #(bit-and % 0xff) ba)))

;; The raw SHA-256 digest of binary, as a Java byte-array.
(defn- digest [data]
  (.digest (MessageDigest/getInstance "SHA-256") (binary->bytes data)))

;; sha256 :: binary -> binary
(def hydra_lib_hashing_sha256
  "Compute the SHA-256 digest of a sequence of bytes."
  (fn [data] (bytes->binary (digest data))))

;; sha256Hex :: binary -> string
(def hydra_lib_hashing_sha256_hex
  "Compute the SHA-256 digest of a sequence of bytes as a lowercase hex string."
  (fn [data] (apply str (map #(format "%02x" (bit-and % 0xff)) (digest data)))))
