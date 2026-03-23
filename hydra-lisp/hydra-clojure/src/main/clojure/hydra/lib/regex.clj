(ns hydra.lib.regex
  (:import [java.util.regex Pattern Matcher]))

;; matches :: String -> String -> Bool
(def hydra_lib_regex_matches
  (fn [pattern] (fn [input] (boolean (re-matches (re-pattern pattern) input)))))

;; find :: String -> String -> Maybe String
(def hydra_lib_regex_find
  (fn [pattern] (fn [input]
    (let [m (re-find (re-pattern pattern) input)]
      (if (some? m)
        (if (string? m) m (first m))
        nil)))))

;; find_all :: String -> String -> [String]
(def hydra_lib_regex_find_all
  (fn [pattern] (fn [input]
    (let [results (re-seq (re-pattern pattern) input)]
      (if results
        (map (fn [m] (if (string? m) m (first m))) results)
        ())))))

;; replace :: String -> String -> String -> String
(def hydra_lib_regex_replace
  (fn [pattern] (fn [replacement] (fn [input]
    (let [m (re-matcher (re-pattern pattern) input)]
      (if (.find m)
        (str (.substring input 0 (.start m))
             replacement
             (.substring input (.end m)))
        input))))))

;; replace_all :: String -> String -> String -> String
(def hydra_lib_regex_replace_all
  (fn [pattern] (fn [replacement] (fn [input]
    (.replaceAll (re-matcher (re-pattern pattern) input)
                 (Matcher/quoteReplacement replacement))))))

;; split :: String -> String -> [String]
(def hydra_lib_regex_split
  (fn [pattern] (fn [input]
    (seq (.split (Pattern/compile pattern) input -1)))))
