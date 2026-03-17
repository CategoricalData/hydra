(ns hydra.parsers
  (:require [hydra.lib.equality :refer :all] [hydra.lib.lists :refer :all] [hydra.lib.logic :refer :all] [hydra.lib.maybes :refer :all] [hydra.lib.strings :refer :all] [hydra.parsing :refer :all]
))

(declare hydra_parsers_alt hydra_parsers_satisfy hydra_parsers_any_char hydra_parsers_apply hydra_parsers_bind hydra_parsers_pure hydra_parsers_between hydra_parsers_char hydra_parsers_fail hydra_parsers_choice hydra_parsers_eof hydra_parsers_lazy hydra_parsers_many hydra_parsers_some hydra_parsers_map hydra_parsers_optional hydra_parsers_run_parser hydra_parsers_sep_by1 hydra_parsers_sep_by hydra_parsers_string)

(def hydra_parsers_alt (fn [p1] (fn [p2] (let [parse (fn [input] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :success) ((fn [s] (list :success s)) match_value) (= (first match_target) :failure) ((fn [e] (if ((hydra_lib_equality_equal ((fn [v] (:remainder v)) e)) input) (((fn [v] v) p2) input) (list :failure e))) match_value))) (second match_target))) (((fn [v] v) p1) input)))] parse))))

(def hydra_parsers_satisfy (fn [pred] (let [parse (fn [input] (let [codes (hydra_lib_strings_to_list input)] (((hydra_lib_maybes_maybe (list :failure (->hydra_parsing_parse_error "unexpected end of input" input))) (fn [c] (let [rest (hydra_lib_strings_from_list ((hydra_lib_lists_drop 1) codes))] (if (pred c) (list :success (->hydra_parsing_parse_success c rest)) (list :failure (->hydra_parsing_parse_error "character did not satisfy predicate" input)))))) (hydra_lib_lists_safe_head codes))))] parse)))

(def hydra_parsers_any_char (hydra_parsers_satisfy (fn [_] true)))

(def hydra_parsers_apply (fn [pf] (fn [pa] (let [parse (fn [input] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :success) ((fn [sf] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :success) ((fn [sa] (list :success (->hydra_parsing_parse_success (((fn [v] (:value v)) sf) ((fn [v] (:value v)) sa)) ((fn [v] (:remainder v)) sa)))) match_value) (= (first match_target) :failure) ((fn [e] (list :failure e)) match_value))) (second match_target))) (((fn [v] v) pa) ((fn [v] (:remainder v)) sf)))) match_value) (= (first match_target) :failure) ((fn [e] (list :failure e)) match_value))) (second match_target))) (((fn [v] v) pf) input)))] parse))))

(def hydra_parsers_bind (fn [pa] (fn [f] (let [parse (fn [input] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :success) ((fn [s] (((fn [v] v) (f ((fn [v] (:value v)) s))) ((fn [v] (:remainder v)) s))) match_value) (= (first match_target) :failure) ((fn [e] (list :failure e)) match_value))) (second match_target))) (((fn [v] v) pa) input)))] parse))))

(def hydra_parsers_pure (fn [a] (fn [input] (list :success (->hydra_parsing_parse_success a input)))))

(def hydra_parsers_between (fn [open] (fn [close] (fn [p] ((hydra_parsers_bind open) (fn [_] ((hydra_parsers_bind p) (fn [x] ((hydra_parsers_bind close) (fn [_] (hydra_parsers_pure x)))))))))))

(def hydra_parsers_char (fn [c] (hydra_parsers_satisfy (fn [x] ((hydra_lib_equality_equal x) c)))))

(def hydra_parsers_fail (fn [msg] (fn [input] (list :failure (->hydra_parsing_parse_error msg input)))))

(def hydra_parsers_choice (fn [ps] (((hydra_lib_lists_foldl hydra_parsers_alt) (hydra_parsers_fail "no choice matched")) ps)))

(def hydra_parsers_eof (fn [input] (if ((hydra_lib_equality_equal input) "") (list :success (->hydra_parsing_parse_success nil "")) (list :failure (->hydra_parsing_parse_error "expected end of input" input)))))

(def hydra_parsers_lazy (fn [f] (fn [input] (((fn [v] v) (f nil)) input))))

(def hydra_parsers_many (fn [p] ((hydra_parsers_alt (hydra_parsers_some p)) (hydra_parsers_pure (list)))))

(def hydra_parsers_some (fn [p] ((hydra_parsers_bind p) (fn [x] ((hydra_parsers_bind (hydra_parsers_many p)) (fn [xs] (hydra_parsers_pure ((hydra_lib_lists_cons x) xs))))))))

(def hydra_parsers_map (fn [f] (fn [pa] (let [parse (fn [input] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :success) ((fn [s] (list :success (->hydra_parsing_parse_success (f ((fn [v] (:value v)) s)) ((fn [v] (:remainder v)) s)))) match_value) (= (first match_target) :failure) ((fn [e] (list :failure e)) match_value))) (second match_target))) (((fn [v] v) pa) input)))] parse))))

(def hydra_parsers_optional (fn [p] ((hydra_parsers_alt ((hydra_parsers_map hydra_lib_maybes_pure) p)) (hydra_parsers_pure nil))))

(def hydra_parsers_run_parser (fn [p] (fn [input] (((fn [v] v) p) input))))

(def hydra_parsers_sep_by1 (fn [p] (fn [sep] ((hydra_parsers_bind p) (fn [x] ((hydra_parsers_bind (hydra_parsers_many ((hydra_parsers_bind sep) (fn [_] p)))) (fn [xs] (hydra_parsers_pure ((hydra_lib_lists_cons x) xs)))))))))

(def hydra_parsers_sep_by (fn [p] (fn [sep] ((hydra_parsers_alt ((hydra_parsers_sep_by1 p) sep)) (hydra_parsers_pure (list))))))

(def hydra_parsers_string (fn [str] (fn [input] (let [str_codes (hydra_lib_strings_to_list str)] (let [input_codes (hydra_lib_strings_to_list input)] (let [str_len (hydra_lib_lists_length str_codes)] (let [input_prefix ((hydra_lib_lists_take str_len) input_codes)] (if ((hydra_lib_equality_equal str_codes) input_prefix) (list :success (->hydra_parsing_parse_success str (hydra_lib_strings_from_list ((hydra_lib_lists_drop str_len) input_codes)))) (list :failure (->hydra_parsing_parse_error ((hydra_lib_strings_cat2 "expected: ") str) input))))))))))
