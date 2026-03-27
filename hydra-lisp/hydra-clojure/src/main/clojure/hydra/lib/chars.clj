(ns hydra.lib.chars)

;; is_alpha_num :: Char -> Bool
(def hydra_lib_chars_is_alpha_num
  "Check whether a character is alphanumeric."
  (fn [c] (or (Character/isLetterOrDigit c))))

;; is_lower :: Char -> Bool
(def hydra_lib_chars_is_lower
  "Check whether a character is lowercase."
  (fn [c] (Character/isLowerCase c)))

;; is_space :: Char -> Bool
(def hydra_lib_chars_is_space
  "Check whether a character is a whitespace character."
  (fn [c] (Character/isWhitespace c)))

;; is_upper :: Char -> Bool
(def hydra_lib_chars_is_upper
  "Check whether a character is uppercase."
  (fn [c] (Character/isUpperCase c)))

;; to_lower :: Char -> Char
(def hydra_lib_chars_to_lower
  "Convert a character to lowercase."
  (fn [c] (Character/toLowerCase c)))

;; to_upper :: Char -> Char
(def hydra_lib_chars_to_upper
  "Convert a character to uppercase."
  (fn [c] (Character/toUpperCase c)))
