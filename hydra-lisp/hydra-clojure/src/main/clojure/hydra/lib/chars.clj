(ns hydra.lib.chars)

;; is_alpha_num :: Char -> Bool
(def hydra_lib_chars_is_alpha_num
  (fn [c] (or (Character/isLetterOrDigit c))))

;; is_space :: Char -> Bool
(def hydra_lib_chars_is_space
  (fn [c] (Character/isWhitespace c)))

;; is_lower :: Char -> Bool
(def hydra_lib_chars_is_lower
  (fn [c] (Character/isLowerCase c)))

;; is_upper :: Char -> Bool
(def hydra_lib_chars_is_upper
  (fn [c] (Character/isUpperCase c)))

;; to_lower :: Char -> Char
(def hydra_lib_chars_to_lower
  (fn [c] (Character/toLowerCase c)))

;; to_upper :: Char -> Char
(def hydra_lib_chars_to_upper
  (fn [c] (Character/toUpperCase c)))
