(ns hydra.dsl.parsing
  (:require [hydra.core :refer :all] [hydra.phantoms :refer :all]
))

(declare hydra_dsl_parsing_parse_error hydra_dsl_parsing_parse_error_message hydra_dsl_parsing_parse_error_remainder hydra_dsl_parsing_parse_error_with_message hydra_dsl_parsing_parse_error_with_remainder hydra_dsl_parsing_parse_result_failure hydra_dsl_parsing_parse_result_success hydra_dsl_parsing_parse_success hydra_dsl_parsing_parse_success_remainder hydra_dsl_parsing_parse_success_value hydra_dsl_parsing_parse_success_with_remainder hydra_dsl_parsing_parse_success_with_value hydra_dsl_parsing_parser hydra_dsl_parsing_un_parser)

(def hydra_dsl_parsing_parse_error (fn [message] (fn [remainder] (list :record (->hydra_core_record "hydra.parsing.ParseError" (list (->hydra_core_field "message" ((fn [v] v) message)) (->hydra_core_field "remainder" ((fn [v] v) remainder))))))))

(def hydra_dsl_parsing_parse_error_message (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.parsing.ParseError" "message")) ((fn [v] v) x)))))

(def hydra_dsl_parsing_parse_error_remainder (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.parsing.ParseError" "remainder")) ((fn [v] v) x)))))

(def hydra_dsl_parsing_parse_error_with_message (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.parsing.ParseError" (list (->hydra_core_field "message" ((fn [v] v) new_val)) (->hydra_core_field "remainder" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.parsing.ParseError" "remainder")) ((fn [v] v) original))))))))))

(def hydra_dsl_parsing_parse_error_with_remainder (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.parsing.ParseError" (list (->hydra_core_field "message" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.parsing.ParseError" "message")) ((fn [v] v) original)))) (->hydra_core_field "remainder" ((fn [v] v) new_val))))))))

(def hydra_dsl_parsing_parse_result_failure (fn [x] (list :inject (->hydra_core_injection "hydra.parsing.ParseResult" (->hydra_core_field "failure" ((fn [v] v) x))))))

(def hydra_dsl_parsing_parse_result_success (fn [x] (list :inject (->hydra_core_injection "hydra.parsing.ParseResult" (->hydra_core_field "success" ((fn [v] v) x))))))

(def hydra_dsl_parsing_parse_success (fn [value] (fn [remainder] (list :record (->hydra_core_record "hydra.parsing.ParseSuccess" (list (->hydra_core_field "value" ((fn [v] v) value)) (->hydra_core_field "remainder" ((fn [v] v) remainder))))))))

(def hydra_dsl_parsing_parse_success_remainder (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.parsing.ParseSuccess" "remainder")) ((fn [v] v) x)))))

(def hydra_dsl_parsing_parse_success_value (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.parsing.ParseSuccess" "value")) ((fn [v] v) x)))))

(def hydra_dsl_parsing_parse_success_with_remainder (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.parsing.ParseSuccess" (list (->hydra_core_field "value" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.parsing.ParseSuccess" "value")) ((fn [v] v) original)))) (->hydra_core_field "remainder" ((fn [v] v) new_val))))))))

(def hydra_dsl_parsing_parse_success_with_value (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.parsing.ParseSuccess" (list (->hydra_core_field "value" ((fn [v] v) new_val)) (->hydra_core_field "remainder" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.parsing.ParseSuccess" "remainder")) ((fn [v] v) original))))))))))

(def hydra_dsl_parsing_parser (fn [x] (list :wrap (->hydra_core_wrapped_term "hydra.parsing.Parser" ((fn [v] v) x)))))

(def hydra_dsl_parsing_un_parser (fn [x] (list :application (->hydra_core_application (list :unwrap "hydra.parsing.Parser") ((fn [v] v) x)))))
