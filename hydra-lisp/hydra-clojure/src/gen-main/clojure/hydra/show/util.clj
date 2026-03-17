(ns hydra.show.util
  (:require [hydra.util :refer :all]
))

(declare hydra_show_util_case_convention)

(def hydra_show_util_case_convention (fn [c] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :lower_snake) ((fn [_] "lower_snake_case") match_value) (= (first match_target) :upper_snake) ((fn [_] "UPPER_SNAKE_CASE") match_value) (= (first match_target) :camel) ((fn [_] "camelCase") match_value) (= (first match_target) :pascal) ((fn [_] "PascalCase") match_value))) (second match_target))) c)))
