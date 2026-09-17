(ns hydra.overlay.clojure.lib.regex
  (:import [java.util.regex Pattern Matcher])
  (:require [hydra.parse.regex :refer :all]
            [hydra.print.pcre.regex :refer :all]))

;; Patterns are Hydra-defined and translingual (docs/specification/regex.md). Each primitive first
;; runs the pattern through hydra.parse.regex, then renders the AST to PCRE syntax via
;; hydra.print.pcre.regex (Clojure regexes are java.util.regex, PCRE-like), before handing the
;; rendered pattern to the native engine. An ill-formed pattern (rejected by hydra.parse.regex) is
;; treated as "no match" -- the same portable-failure convention as an empty match. See issue #603.

;; Returns [:given <native-pattern-string>], or [:none] if the pattern does not parse.
(defn- hydra--regex-to-native [pattern]
  (let [parsed (hydra_parse_regex_parse_regex pattern)]
    (if (= (first parsed) :given)
      [:given (hydra_print_pcre_regex_print_regex (second parsed))]
      [:none])))

;; matches :: String -> String -> Bool
(def hydra_overlay_clojure_lib_regex_matches
  (fn [pattern] (fn [input]
    (let [native (hydra--regex-to-native pattern)]
      (if (= (first native) :none)
        false
        (boolean (re-matches (re-pattern (second native)) input)))))))

;; find :: String -> String -> Maybe String
(def hydra_overlay_clojure_lib_regex_find
  (fn [pattern] (fn [input]
    (let [native (hydra--regex-to-native pattern)]
      (if (= (first native) :none)
        nil
        (let [m (re-find (re-pattern (second native)) input)]
          (if (some? m)
            (if (string? m) m (first m))
            nil)))))))

;; find_all :: String -> String -> [String]
(def hydra_overlay_clojure_lib_regex_find_all
  (fn [pattern] (fn [input]
    (let [native (hydra--regex-to-native pattern)]
      (if (= (first native) :none)
        ()
        (let [results (re-seq (re-pattern (second native)) input)]
          (if results
            (map (fn [m] (if (string? m) m (first m))) results)
            ())))))))

;; replace :: String -> String -> String -> String
(def hydra_overlay_clojure_lib_regex_replace
  (fn [pattern] (fn [replacement] (fn [input]
    (let [native (hydra--regex-to-native pattern)]
      (if (= (first native) :none)
        input
        (let [m (re-matcher (re-pattern (second native)) input)]
          (if (.find m)
            (str (.substring input 0 (.start m))
                 replacement
                 (.substring input (.end m)))
            input))))))))

;; replace_all :: String -> String -> String -> String
(def hydra_overlay_clojure_lib_regex_replace_all
  (fn [pattern] (fn [replacement] (fn [input]
    (let [native (hydra--regex-to-native pattern)]
      (if (= (first native) :none)
        input
        (.replaceAll (re-matcher (re-pattern (second native)) input)
                     (Matcher/quoteReplacement replacement))))))))

;; split :: String -> String -> [String]
(def hydra_overlay_clojure_lib_regex_split
  (fn [pattern] (fn [input]
    (let [native (hydra--regex-to-native pattern)]
      (if (= (first native) :none)
        (list input)
        (seq (.split (Pattern/compile (second native)) input -1)))))))
