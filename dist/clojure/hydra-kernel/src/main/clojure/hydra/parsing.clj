(ns hydra.parsing)

(declare hydra_parsing_parse_result-variants)

(defrecord hydra_parsing_parse_error [message remainder])
(defn make-hydra_parsing_parse_error [message remainder] (->hydra_parsing_parse_error message remainder))

(def hydra_parsing_parse_result-variants (list :success :failure))

(defrecord hydra_parsing_parse_success [value remainder])
(defn make-hydra_parsing_parse_success [value remainder] (->hydra_parsing_parse_success value remainder))

(defrecord hydra_parsing_parser [value])
(defn make-hydra_parsing_parser [value] (->hydra_parsing_parser value))
