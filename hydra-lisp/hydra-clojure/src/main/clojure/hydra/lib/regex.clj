(ns hydra.lib.regex
  (:import [java.util.regex Pattern Matcher]))

;; find :: String -> String -> Maybe String
(def hydra_lib_regex_find
  "Find the first substring matching a regex pattern."
  (fn [pattern] (fn [input]
    (let [m (re-find (re-pattern pattern) input)]
      (if (some? m)
        (if (string? m) m (first m))
        nil)))))

;; find_all :: String -> String -> [String]
(def hydra_lib_regex_find_all
  "Find all non-overlapping substrings matching a regex pattern."
  (fn [pattern] (fn [input]
    (let [results (re-seq (re-pattern pattern) input)]
      (if results
        (map (fn [m] (if (string? m) m (first m))) results)
        ())))))

;; matches :: String -> String -> Bool
(def hydra_lib_regex_matches
  "Check whether an entire string matches a regex pattern."
  (fn [pattern] (fn [input] (boolean (re-matches (re-pattern pattern) input)))))

;; replace :: String -> String -> String -> String
(def hydra_lib_regex_replace
  "Replace the first occurrence of a regex pattern with a replacement string."
  (fn [pattern] (fn [replacement] (fn [input]
    (let [m (re-matcher (re-pattern pattern) input)]
      (if (.find m)
        (str (.substring input 0 (.start m))
             replacement
             (.substring input (.end m)))
        input))))))

;; replace_all :: String -> String -> String -> String
(def hydra_lib_regex_replace_all
  "Replace all non-overlapping occurrences of a regex pattern with a replacement string."
  (fn [pattern] (fn [replacement] (fn [input]
    (.replaceAll (re-matcher (re-pattern pattern) input)
                 (Matcher/quoteReplacement replacement))))))

;; split :: String -> String -> [String]
(def hydra_lib_regex_split
  "Split a string by a regex pattern."
  (fn [pattern] (fn [input]
    (seq (.split (Pattern/compile pattern) input -1)))))
