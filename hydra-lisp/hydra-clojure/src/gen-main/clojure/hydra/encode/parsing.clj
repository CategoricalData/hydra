(ns hydra.encode.parsing
  (:require [hydra.core :refer :all] [hydra.parsing :refer :all]
))

(declare hydra_encode_parsing_parse_error hydra_encode_parsing_parse_success hydra_encode_parsing_parse_result)

(def hydra_encode_parsing_parse_error (fn [x] (list :record (->hydra_core_record "hydra.parsing.ParseError" (list (->hydra_core_field "message" ((fn [x2] (list :literal (list :string x2))) ((fn [v] (:message v)) x))) (->hydra_core_field "remainder" ((fn [x2] (list :literal (list :string x2))) ((fn [v] (:remainder v)) x))))))))

(def hydra_encode_parsing_parse_success (fn [a] (fn [x] (list :record (->hydra_core_record "hydra.parsing.ParseSuccess" (list (->hydra_core_field "value" (a ((fn [v] (:value v)) x))) (->hydra_core_field "remainder" ((fn [x2] (list :literal (list :string x2))) ((fn [v] (:remainder v)) x)))))))))

(def hydra_encode_parsing_parse_result (fn [a] (fn [match_target] ((fn [match_value] (cond (= (first match_target) :success) ((fn [y] (list :union (->hydra_core_injection "hydra.parsing.ParseResult" (->hydra_core_field "success" ((hydra_encode_parsing_parse_success a) y))))) match_value) (= (first match_target) :failure) ((fn [y] (list :union (->hydra_core_injection "hydra.parsing.ParseResult" (->hydra_core_field "failure" (hydra_encode_parsing_parse_error y))))) match_value))) (second match_target)))))
