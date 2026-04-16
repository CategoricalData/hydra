(ns hydra.lib.strings)

;; cat :: [String] -> String
(def hydra_lib_strings_cat
  "Concatenate a list of strings into a single string."
  (fn [ss] (apply str ss)))

;; cat2 :: String -> String -> String
(def hydra_lib_strings_cat2
  "Concatenate two strings."
  (fn [a] (fn [b] (str a b))))

;; from_list :: [Int] -> String (codepoints to string)
(def hydra_lib_strings_from_list
  "Convert a list of Unicode code points to a string."
  (fn [cs]
    (let [sb (StringBuilder.)]
      (doseq [cp cs]
        (.appendCodePoint sb (int cp)))
      (.toString sb))))

;; intercalate :: String -> [String] -> String
(def hydra_lib_strings_intercalate
  "Join a list of strings with a separator between each element."
  (fn [sep] (fn [ss] (clojure.string/join sep ss))))

;; length :: String -> Int (codepoint count)
(def hydra_lib_strings_length
  "Return the length of a string."
  (fn [s] (.codePointCount s 0 (.length s))))

;; lines :: String -> [String]
;; Haskell semantics: lines "" = [], lines "hello\n" = ["hello"], lines "\n" = [""]
(def hydra_lib_strings_lines
  "Split a string into lines."
  (fn [s]
    (if (= s "")
      ()
      (let [parts (loop [i 0 start 0 acc ()]
                    (cond
                      (>= i (count s))
                      (reverse (cons (subs s start) acc))
                      (= (.charAt s i) \newline)
                      (recur (inc i) (inc i) (cons (subs s start i) acc))
                      :else
                      (recur (inc i) start acc)))]
        ;; Drop trailing empty string if input ended with \n
        (if (and (not (empty? parts)) (= (last parts) "") (.endsWith s "\n"))
          (butlast parts)
          parts)))))

;; maybe_char_at :: Int -> String -> Maybe Int
(def hydra_lib_strings_maybe_char_at
  "Get the Unicode code point at a specific index, returning Nothing if out of bounds."
  (fn [n] (fn [s]
    (if (and (>= n 0) (< n (.codePointCount s 0 (.length s))))
      (list :just (.codePointAt s (int (.offsetByCodePoints s 0 (int n)))))
      (list :nothing)))))

;; null :: String -> Bool
(def hydra_lib_strings_null
  "Check whether a string is empty."
  (fn [s] (= s "")))

;; split_on :: String -> String -> [String]
(def hydra_lib_strings_split_on
  "Split a string on a delimiter string."
  (fn [sep] (fn [s]
    (if (= sep "")
      (cons "" (map str (seq s)))
      (loop [i 0 start 0 acc ()]
        (cond
          (> (+ i (count sep)) (count s))
          (reverse (cons (subs s start) acc))
          (= (subs s i (+ i (count sep))) sep)
          (recur (+ i (count sep)) (+ i (count sep)) (cons (subs s start i) acc))
          :else
          (recur (inc i) start acc)))))))

;; to_list :: String -> [Int] (codepoints)
(def hydra_lib_strings_to_list
  "Convert a string to a list of Unicode code points."
  (fn [s]
    (let [len (.length s)]
      (loop [i 0 acc ()]
        (if (>= i len)
          (reverse acc)
          (let [cp (.codePointAt s i)
                chars (Character/charCount cp)]
            (recur (+ i chars) (cons cp acc))))))))

;; to_lower :: String -> String
(def hydra_lib_strings_to_lower
  "Convert a string to lowercase."
  (fn [s] (clojure.string/lower-case s)))

;; to_upper :: String -> String
(def hydra_lib_strings_to_upper
  "Convert a string to uppercase."
  (fn [s] (clojure.string/upper-case s)))

;; unlines :: [String] -> String
(def hydra_lib_strings_unlines
  "Join a list of strings with newlines, appending a trailing newline."
  (fn [ss] (apply str (map (fn [s] (str s "\n")) ss))))
