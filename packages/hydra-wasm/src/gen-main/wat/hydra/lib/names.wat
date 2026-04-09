(module
  (memory $memory 1 )
  (export "memory" (memory $memory) )
  (export "hydra.lib.names.chars" (func $hydra.lib.names.chars) )
  (export "hydra.lib.names.chars_is_alpha_num" (func $hydra.lib.names.chars_is_alpha_num) )
  (export "hydra.lib.names.chars_is_lower" (func $hydra.lib.names.chars_is_lower) )
  (export "hydra.lib.names.chars_is_space" (func $hydra.lib.names.chars_is_space) )
  (export "hydra.lib.names.chars_is_upper" (func $hydra.lib.names.chars_is_upper) )
  (export "hydra.lib.names.chars_to_lower" (func $hydra.lib.names.chars_to_lower) )
  (export "hydra.lib.names.chars_to_upper" (func $hydra.lib.names.chars_to_upper) )
  (export "hydra.lib.names.eithers" (func $hydra.lib.names.eithers) )
  (export "hydra.lib.names.eithers_bimap" (func $hydra.lib.names.eithers_bimap) )
  (export "hydra.lib.names.eithers_bind" (func $hydra.lib.names.eithers_bind) )
  (export "hydra.lib.names.eithers_either" (func $hydra.lib.names.eithers_either) )
  (export "hydra.lib.names.eithers_foldl" (func $hydra.lib.names.eithers_foldl) )
  (export "hydra.lib.names.eithers_from_left" (func $hydra.lib.names.eithers_from_left) )
  (export "hydra.lib.names.eithers_from_right" (func $hydra.lib.names.eithers_from_right) )
  (export "hydra.lib.names.eithers_is_left" (func $hydra.lib.names.eithers_is_left) )
  (export "hydra.lib.names.eithers_is_right" (func $hydra.lib.names.eithers_is_right) )
  (export "hydra.lib.names.eithers_lefts" (func $hydra.lib.names.eithers_lefts) )
  (export "hydra.lib.names.eithers_map" (func $hydra.lib.names.eithers_map) )
  (export "hydra.lib.names.eithers_map_list" (func $hydra.lib.names.eithers_map_list) )
  (export "hydra.lib.names.eithers_map_maybe" (func $hydra.lib.names.eithers_map_maybe) )
  (export "hydra.lib.names.eithers_map_set" (func $hydra.lib.names.eithers_map_set) )
  (export "hydra.lib.names.eithers_partition_eithers" (func $hydra.lib.names.eithers_partition_eithers) )
  (export "hydra.lib.names.eithers_rights" (func $hydra.lib.names.eithers_rights) )
  (export "hydra.lib.names.equality" (func $hydra.lib.names.equality) )
  (export "hydra.lib.names.equality_compare" (func $hydra.lib.names.equality_compare) )
  (export "hydra.lib.names.equality_equal" (func $hydra.lib.names.equality_equal) )
  (export "hydra.lib.names.equality_gt" (func $hydra.lib.names.equality_gt) )
  (export "hydra.lib.names.equality_gte" (func $hydra.lib.names.equality_gte) )
  (export "hydra.lib.names.equality_identity" (func $hydra.lib.names.equality_identity) )
  (export "hydra.lib.names.equality_lt" (func $hydra.lib.names.equality_lt) )
  (export "hydra.lib.names.equality_lte" (func $hydra.lib.names.equality_lte) )
  (export "hydra.lib.names.equality_max" (func $hydra.lib.names.equality_max) )
  (export "hydra.lib.names.equality_min" (func $hydra.lib.names.equality_min) )
  (export "hydra.lib.names.lists" (func $hydra.lib.names.lists) )
  (export "hydra.lib.names.lists_apply" (func $hydra.lib.names.lists_apply) )
  (export "hydra.lib.names.lists_at" (func $hydra.lib.names.lists_at) )
  (export "hydra.lib.names.lists_bind" (func $hydra.lib.names.lists_bind) )
  (export "hydra.lib.names.lists_concat" (func $hydra.lib.names.lists_concat) )
  (export "hydra.lib.names.lists_concat2" (func $hydra.lib.names.lists_concat2) )
  (export "hydra.lib.names.lists_cons" (func $hydra.lib.names.lists_cons) )
  (export "hydra.lib.names.lists_drop" (func $hydra.lib.names.lists_drop) )
  (export "hydra.lib.names.lists_drop_while" (func $hydra.lib.names.lists_drop_while) )
  (export "hydra.lib.names.lists_elem" (func $hydra.lib.names.lists_elem) )
  (export "hydra.lib.names.lists_filter" (func $hydra.lib.names.lists_filter) )
  (export "hydra.lib.names.lists_find" (func $hydra.lib.names.lists_find) )
  (export "hydra.lib.names.lists_foldl" (func $hydra.lib.names.lists_foldl) )
  (export "hydra.lib.names.lists_foldr" (func $hydra.lib.names.lists_foldr) )
  (export "hydra.lib.names.lists_group" (func $hydra.lib.names.lists_group) )
  (export "hydra.lib.names.lists_head" (func $hydra.lib.names.lists_head) )
  (export "hydra.lib.names.lists_init" (func $hydra.lib.names.lists_init) )
  (export "hydra.lib.names.lists_intercalate" (func $hydra.lib.names.lists_intercalate) )
  (export "hydra.lib.names.lists_intersperse" (func $hydra.lib.names.lists_intersperse) )
  (export "hydra.lib.names.lists_last" (func $hydra.lib.names.lists_last) )
  (export "hydra.lib.names.lists_length" (func $hydra.lib.names.lists_length) )
  (export "hydra.lib.names.lists_map" (func $hydra.lib.names.lists_map) )
  (export "hydra.lib.names.lists_maybe_at" (func $hydra.lib.names.lists_maybe_at) )
  (export "hydra.lib.names.lists_maybe_head" (func $hydra.lib.names.lists_maybe_head) )
  (export "hydra.lib.names.lists_maybe_init" (func $hydra.lib.names.lists_maybe_init) )
  (export "hydra.lib.names.lists_maybe_last" (func $hydra.lib.names.lists_maybe_last) )
  (export "hydra.lib.names.lists_maybe_tail" (func $hydra.lib.names.lists_maybe_tail) )
  (export "hydra.lib.names.lists_nub" (func $hydra.lib.names.lists_nub) )
  (export "hydra.lib.names.lists_null" (func $hydra.lib.names.lists_null) )
  (export "hydra.lib.names.lists_partition" (func $hydra.lib.names.lists_partition) )
  (export "hydra.lib.names.lists_pure" (func $hydra.lib.names.lists_pure) )
  (export "hydra.lib.names.lists_replicate" (func $hydra.lib.names.lists_replicate) )
  (export "hydra.lib.names.lists_reverse" (func $hydra.lib.names.lists_reverse) )
  (export "hydra.lib.names.lists_safe_head" (func $hydra.lib.names.lists_safe_head) )
  (export "hydra.lib.names.lists_singleton" (func $hydra.lib.names.lists_singleton) )
  (export "hydra.lib.names.lists_sort" (func $hydra.lib.names.lists_sort) )
  (export "hydra.lib.names.lists_sort_on" (func $hydra.lib.names.lists_sort_on) )
  (export "hydra.lib.names.lists_span" (func $hydra.lib.names.lists_span) )
  (export "hydra.lib.names.lists_tail" (func $hydra.lib.names.lists_tail) )
  (export "hydra.lib.names.lists_take" (func $hydra.lib.names.lists_take) )
  (export "hydra.lib.names.lists_transpose" (func $hydra.lib.names.lists_transpose) )
  (export "hydra.lib.names.lists_zip" (func $hydra.lib.names.lists_zip) )
  (export "hydra.lib.names.lists_zip_with" (func $hydra.lib.names.lists_zip_with) )
  (export "hydra.lib.names.literals" (func $hydra.lib.names.literals) )
  (export "hydra.lib.names.literals_bigfloat_to_bigint" (func $hydra.lib.names.literals_bigfloat_to_bigint) )
  (export "hydra.lib.names.literals_bigfloat_to_float32" (func $hydra.lib.names.literals_bigfloat_to_float32) )
  (export "hydra.lib.names.literals_bigfloat_to_float64" (func $hydra.lib.names.literals_bigfloat_to_float64) )
  (export "hydra.lib.names.literals_bigint_to_bigfloat" (func $hydra.lib.names.literals_bigint_to_bigfloat) )
  (export "hydra.lib.names.literals_bigint_to_int16" (func $hydra.lib.names.literals_bigint_to_int16) )
  (export "hydra.lib.names.literals_bigint_to_int32" (func $hydra.lib.names.literals_bigint_to_int32) )
  (export "hydra.lib.names.literals_bigint_to_int64" (func $hydra.lib.names.literals_bigint_to_int64) )
  (export "hydra.lib.names.literals_bigint_to_int8" (func $hydra.lib.names.literals_bigint_to_int8) )
  (export "hydra.lib.names.literals_bigint_to_uint16" (func $hydra.lib.names.literals_bigint_to_uint16) )
  (export "hydra.lib.names.literals_bigint_to_uint32" (func $hydra.lib.names.literals_bigint_to_uint32) )
  (export "hydra.lib.names.literals_bigint_to_uint64" (func $hydra.lib.names.literals_bigint_to_uint64) )
  (export "hydra.lib.names.literals_bigint_to_uint8" (func $hydra.lib.names.literals_bigint_to_uint8) )
  (export "hydra.lib.names.literals_binary_to_bytes" (func $hydra.lib.names.literals_binary_to_bytes) )
  (export "hydra.lib.names.literals_binary_to_string" (func $hydra.lib.names.literals_binary_to_string) )
  (export "hydra.lib.names.literals_float32_to_bigfloat" (func $hydra.lib.names.literals_float32_to_bigfloat) )
  (export "hydra.lib.names.literals_float64_to_bigfloat" (func $hydra.lib.names.literals_float64_to_bigfloat) )
  (export "hydra.lib.names.literals_int16_to_bigint" (func $hydra.lib.names.literals_int16_to_bigint) )
  (export "hydra.lib.names.literals_int32_to_bigint" (func $hydra.lib.names.literals_int32_to_bigint) )
  (export "hydra.lib.names.literals_int64_to_bigint" (func $hydra.lib.names.literals_int64_to_bigint) )
  (export "hydra.lib.names.literals_int8_to_bigint" (func $hydra.lib.names.literals_int8_to_bigint) )
  (export "hydra.lib.names.literals_read_bigfloat" (func $hydra.lib.names.literals_read_bigfloat) )
  (export "hydra.lib.names.literals_read_bigint" (func $hydra.lib.names.literals_read_bigint) )
  (export "hydra.lib.names.literals_read_boolean" (func $hydra.lib.names.literals_read_boolean) )
  (export "hydra.lib.names.literals_read_float32" (func $hydra.lib.names.literals_read_float32) )
  (export "hydra.lib.names.literals_read_float64" (func $hydra.lib.names.literals_read_float64) )
  (export "hydra.lib.names.literals_read_int16" (func $hydra.lib.names.literals_read_int16) )
  (export "hydra.lib.names.literals_read_int32" (func $hydra.lib.names.literals_read_int32) )
  (export "hydra.lib.names.literals_read_int64" (func $hydra.lib.names.literals_read_int64) )
  (export "hydra.lib.names.literals_read_int8" (func $hydra.lib.names.literals_read_int8) )
  (export "hydra.lib.names.literals_read_string" (func $hydra.lib.names.literals_read_string) )
  (export "hydra.lib.names.literals_read_uint16" (func $hydra.lib.names.literals_read_uint16) )
  (export "hydra.lib.names.literals_read_uint32" (func $hydra.lib.names.literals_read_uint32) )
  (export "hydra.lib.names.literals_read_uint64" (func $hydra.lib.names.literals_read_uint64) )
  (export "hydra.lib.names.literals_read_uint8" (func $hydra.lib.names.literals_read_uint8) )
  (export "hydra.lib.names.literals_show_bigfloat" (func $hydra.lib.names.literals_show_bigfloat) )
  (export "hydra.lib.names.literals_show_bigint" (func $hydra.lib.names.literals_show_bigint) )
  (export "hydra.lib.names.literals_show_boolean" (func $hydra.lib.names.literals_show_boolean) )
  (export "hydra.lib.names.literals_show_float32" (func $hydra.lib.names.literals_show_float32) )
  (export "hydra.lib.names.literals_show_float64" (func $hydra.lib.names.literals_show_float64) )
  (export "hydra.lib.names.literals_show_int16" (func $hydra.lib.names.literals_show_int16) )
  (export "hydra.lib.names.literals_show_int32" (func $hydra.lib.names.literals_show_int32) )
  (export "hydra.lib.names.literals_show_int64" (func $hydra.lib.names.literals_show_int64) )
  (export "hydra.lib.names.literals_show_int8" (func $hydra.lib.names.literals_show_int8) )
  (export "hydra.lib.names.literals_show_string" (func $hydra.lib.names.literals_show_string) )
  (export "hydra.lib.names.literals_show_uint16" (func $hydra.lib.names.literals_show_uint16) )
  (export "hydra.lib.names.literals_show_uint32" (func $hydra.lib.names.literals_show_uint32) )
  (export "hydra.lib.names.literals_show_uint64" (func $hydra.lib.names.literals_show_uint64) )
  (export "hydra.lib.names.literals_show_uint8" (func $hydra.lib.names.literals_show_uint8) )
  (export "hydra.lib.names.literals_string_to_binary" (func $hydra.lib.names.literals_string_to_binary) )
  (export "hydra.lib.names.literals_uint16_to_bigint" (func $hydra.lib.names.literals_uint16_to_bigint) )
  (export "hydra.lib.names.literals_uint32_to_bigint" (func $hydra.lib.names.literals_uint32_to_bigint) )
  (export "hydra.lib.names.literals_uint64_to_bigint" (func $hydra.lib.names.literals_uint64_to_bigint) )
  (export "hydra.lib.names.literals_uint8_to_bigint" (func $hydra.lib.names.literals_uint8_to_bigint) )
  (export "hydra.lib.names.logic" (func $hydra.lib.names.logic) )
  (export "hydra.lib.names.logic_and" (func $hydra.lib.names.logic_and) )
  (export "hydra.lib.names.logic_if_else" (func $hydra.lib.names.logic_if_else) )
  (export "hydra.lib.names.logic_not" (func $hydra.lib.names.logic_not) )
  (export "hydra.lib.names.logic_or" (func $hydra.lib.names.logic_or) )
  (export "hydra.lib.names.maps" (func $hydra.lib.names.maps) )
  (export "hydra.lib.names.maps_alter" (func $hydra.lib.names.maps_alter) )
  (export "hydra.lib.names.maps_bimap" (func $hydra.lib.names.maps_bimap) )
  (export "hydra.lib.names.maps_delete" (func $hydra.lib.names.maps_delete) )
  (export "hydra.lib.names.maps_elems" (func $hydra.lib.names.maps_elems) )
  (export "hydra.lib.names.maps_empty" (func $hydra.lib.names.maps_empty) )
  (export "hydra.lib.names.maps_filter" (func $hydra.lib.names.maps_filter) )
  (export "hydra.lib.names.maps_filter_with_key" (func $hydra.lib.names.maps_filter_with_key) )
  (export "hydra.lib.names.maps_find_with_default" (func $hydra.lib.names.maps_find_with_default) )
  (export "hydra.lib.names.maps_from_list" (func $hydra.lib.names.maps_from_list) )
  (export "hydra.lib.names.maps_insert" (func $hydra.lib.names.maps_insert) )
  (export "hydra.lib.names.maps_keys" (func $hydra.lib.names.maps_keys) )
  (export "hydra.lib.names.maps_lookup" (func $hydra.lib.names.maps_lookup) )
  (export "hydra.lib.names.maps_map" (func $hydra.lib.names.maps_map) )
  (export "hydra.lib.names.maps_map_keys" (func $hydra.lib.names.maps_map_keys) )
  (export "hydra.lib.names.maps_member" (func $hydra.lib.names.maps_member) )
  (export "hydra.lib.names.maps_null" (func $hydra.lib.names.maps_null) )
  (export "hydra.lib.names.maps_singleton" (func $hydra.lib.names.maps_singleton) )
  (export "hydra.lib.names.maps_size" (func $hydra.lib.names.maps_size) )
  (export "hydra.lib.names.maps_to_list" (func $hydra.lib.names.maps_to_list) )
  (export "hydra.lib.names.maps_union" (func $hydra.lib.names.maps_union) )
  (export "hydra.lib.names.math" (func $hydra.lib.names.math) )
  (export "hydra.lib.names.math_abs" (func $hydra.lib.names.math_abs) )
  (export "hydra.lib.names.math_acos" (func $hydra.lib.names.math_acos) )
  (export "hydra.lib.names.math_acosh" (func $hydra.lib.names.math_acosh) )
  (export "hydra.lib.names.math_add" (func $hydra.lib.names.math_add) )
  (export "hydra.lib.names.math_asin" (func $hydra.lib.names.math_asin) )
  (export "hydra.lib.names.math_asinh" (func $hydra.lib.names.math_asinh) )
  (export "hydra.lib.names.math_atan" (func $hydra.lib.names.math_atan) )
  (export "hydra.lib.names.math_atan2" (func $hydra.lib.names.math_atan2) )
  (export "hydra.lib.names.math_atanh" (func $hydra.lib.names.math_atanh) )
  (export "hydra.lib.names.math_ceiling" (func $hydra.lib.names.math_ceiling) )
  (export "hydra.lib.names.math_cos" (func $hydra.lib.names.math_cos) )
  (export "hydra.lib.names.math_cosh" (func $hydra.lib.names.math_cosh) )
  (export "hydra.lib.names.math_div" (func $hydra.lib.names.math_div) )
  (export "hydra.lib.names.math_e" (func $hydra.lib.names.math_e) )
  (export "hydra.lib.names.math_even" (func $hydra.lib.names.math_even) )
  (export "hydra.lib.names.math_exp" (func $hydra.lib.names.math_exp) )
  (export "hydra.lib.names.math_floor" (func $hydra.lib.names.math_floor) )
  (export "hydra.lib.names.math_log" (func $hydra.lib.names.math_log) )
  (export "hydra.lib.names.math_log_base" (func $hydra.lib.names.math_log_base) )
  (export "hydra.lib.names.math_max" (func $hydra.lib.names.math_max) )
  (export "hydra.lib.names.math_maybe_div" (func $hydra.lib.names.math_maybe_div) )
  (export "hydra.lib.names.math_maybe_mod" (func $hydra.lib.names.math_maybe_mod) )
  (export "hydra.lib.names.math_maybe_pred" (func $hydra.lib.names.math_maybe_pred) )
  (export "hydra.lib.names.math_maybe_rem" (func $hydra.lib.names.math_maybe_rem) )
  (export "hydra.lib.names.math_maybe_succ" (func $hydra.lib.names.math_maybe_succ) )
  (export "hydra.lib.names.math_min" (func $hydra.lib.names.math_min) )
  (export "hydra.lib.names.math_mod" (func $hydra.lib.names.math_mod) )
  (export "hydra.lib.names.math_mul" (func $hydra.lib.names.math_mul) )
  (export "hydra.lib.names.math_negate" (func $hydra.lib.names.math_negate) )
  (export "hydra.lib.names.math_odd" (func $hydra.lib.names.math_odd) )
  (export "hydra.lib.names.math_pi" (func $hydra.lib.names.math_pi) )
  (export "hydra.lib.names.math_pow" (func $hydra.lib.names.math_pow) )
  (export "hydra.lib.names.math_pred" (func $hydra.lib.names.math_pred) )
  (export "hydra.lib.names.math_range" (func $hydra.lib.names.math_range) )
  (export "hydra.lib.names.math_rem" (func $hydra.lib.names.math_rem) )
  (export "hydra.lib.names.math_round" (func $hydra.lib.names.math_round) )
  (export "hydra.lib.names.math_round_bigfloat" (func $hydra.lib.names.math_round_bigfloat) )
  (export "hydra.lib.names.math_round_float32" (func $hydra.lib.names.math_round_float32) )
  (export "hydra.lib.names.math_round_float64" (func $hydra.lib.names.math_round_float64) )
  (export "hydra.lib.names.math_signum" (func $hydra.lib.names.math_signum) )
  (export "hydra.lib.names.math_sin" (func $hydra.lib.names.math_sin) )
  (export "hydra.lib.names.math_sinh" (func $hydra.lib.names.math_sinh) )
  (export "hydra.lib.names.math_sqrt" (func $hydra.lib.names.math_sqrt) )
  (export "hydra.lib.names.math_sub" (func $hydra.lib.names.math_sub) )
  (export "hydra.lib.names.math_succ" (func $hydra.lib.names.math_succ) )
  (export "hydra.lib.names.math_tan" (func $hydra.lib.names.math_tan) )
  (export "hydra.lib.names.math_tanh" (func $hydra.lib.names.math_tanh) )
  (export "hydra.lib.names.math_truncate" (func $hydra.lib.names.math_truncate) )
  (export "hydra.lib.names.maybes" (func $hydra.lib.names.maybes) )
  (export "hydra.lib.names.maybes_apply" (func $hydra.lib.names.maybes_apply) )
  (export "hydra.lib.names.maybes_bind" (func $hydra.lib.names.maybes_bind) )
  (export "hydra.lib.names.maybes_cases" (func $hydra.lib.names.maybes_cases) )
  (export "hydra.lib.names.maybes_cat" (func $hydra.lib.names.maybes_cat) )
  (export "hydra.lib.names.maybes_compose" (func $hydra.lib.names.maybes_compose) )
  (export "hydra.lib.names.maybes_from_just" (func $hydra.lib.names.maybes_from_just) )
  (export "hydra.lib.names.maybes_from_maybe" (func $hydra.lib.names.maybes_from_maybe) )
  (export "hydra.lib.names.maybes_is_just" (func $hydra.lib.names.maybes_is_just) )
  (export "hydra.lib.names.maybes_is_nothing" (func $hydra.lib.names.maybes_is_nothing) )
  (export "hydra.lib.names.maybes_map" (func $hydra.lib.names.maybes_map) )
  (export "hydra.lib.names.maybes_map_maybe" (func $hydra.lib.names.maybes_map_maybe) )
  (export "hydra.lib.names.maybes_maybe" (func $hydra.lib.names.maybes_maybe) )
  (export "hydra.lib.names.maybes_pure" (func $hydra.lib.names.maybes_pure) )
  (export "hydra.lib.names.maybes_to_list" (func $hydra.lib.names.maybes_to_list) )
  (export "hydra.lib.names.pairs" (func $hydra.lib.names.pairs) )
  (export "hydra.lib.names.pairs_bimap" (func $hydra.lib.names.pairs_bimap) )
  (export "hydra.lib.names.pairs_first" (func $hydra.lib.names.pairs_first) )
  (export "hydra.lib.names.pairs_second" (func $hydra.lib.names.pairs_second) )
  (export "hydra.lib.names.regex" (func $hydra.lib.names.regex) )
  (export "hydra.lib.names.regex_find" (func $hydra.lib.names.regex_find) )
  (export "hydra.lib.names.regex_find_all" (func $hydra.lib.names.regex_find_all) )
  (export "hydra.lib.names.regex_matches" (func $hydra.lib.names.regex_matches) )
  (export "hydra.lib.names.regex_replace" (func $hydra.lib.names.regex_replace) )
  (export "hydra.lib.names.regex_replace_all" (func $hydra.lib.names.regex_replace_all) )
  (export "hydra.lib.names.regex_split" (func $hydra.lib.names.regex_split) )
  (export "hydra.lib.names.sets" (func $hydra.lib.names.sets) )
  (export "hydra.lib.names.sets_delete" (func $hydra.lib.names.sets_delete) )
  (export "hydra.lib.names.sets_difference" (func $hydra.lib.names.sets_difference) )
  (export "hydra.lib.names.sets_empty" (func $hydra.lib.names.sets_empty) )
  (export "hydra.lib.names.sets_from_list" (func $hydra.lib.names.sets_from_list) )
  (export "hydra.lib.names.sets_insert" (func $hydra.lib.names.sets_insert) )
  (export "hydra.lib.names.sets_intersection" (func $hydra.lib.names.sets_intersection) )
  (export "hydra.lib.names.sets_map" (func $hydra.lib.names.sets_map) )
  (export "hydra.lib.names.sets_member" (func $hydra.lib.names.sets_member) )
  (export "hydra.lib.names.sets_null" (func $hydra.lib.names.sets_null) )
  (export "hydra.lib.names.sets_singleton" (func $hydra.lib.names.sets_singleton) )
  (export "hydra.lib.names.sets_size" (func $hydra.lib.names.sets_size) )
  (export "hydra.lib.names.sets_to_list" (func $hydra.lib.names.sets_to_list) )
  (export "hydra.lib.names.sets_union" (func $hydra.lib.names.sets_union) )
  (export "hydra.lib.names.sets_unions" (func $hydra.lib.names.sets_unions) )
  (export "hydra.lib.names.strings" (func $hydra.lib.names.strings) )
  (export "hydra.lib.names.strings_cat" (func $hydra.lib.names.strings_cat) )
  (export "hydra.lib.names.strings_cat2" (func $hydra.lib.names.strings_cat2) )
  (export "hydra.lib.names.strings_char_at" (func $hydra.lib.names.strings_char_at) )
  (export "hydra.lib.names.strings_from_list" (func $hydra.lib.names.strings_from_list) )
  (export "hydra.lib.names.strings_intercalate" (func $hydra.lib.names.strings_intercalate) )
  (export "hydra.lib.names.strings_length" (func $hydra.lib.names.strings_length) )
  (export "hydra.lib.names.strings_lines" (func $hydra.lib.names.strings_lines) )
  (export "hydra.lib.names.strings_maybe_char_at" (func $hydra.lib.names.strings_maybe_char_at) )
  (export "hydra.lib.names.strings_null" (func $hydra.lib.names.strings_null) )
  (export "hydra.lib.names.strings_split_on" (func $hydra.lib.names.strings_split_on) )
  (export "hydra.lib.names.strings_to_list" (func $hydra.lib.names.strings_to_list) )
  (export "hydra.lib.names.strings_to_lower" (func $hydra.lib.names.strings_to_lower) )
  (export "hydra.lib.names.strings_to_upper" (func $hydra.lib.names.strings_to_upper) )
  (export "hydra.lib.names.strings_unlines" (func $hydra.lib.names.strings_unlines) )
  (export "hydra.lib.names.typeclass" (func $hydra.lib.names.typeclass) )
  (export "hydra.lib.names.typeclass_eq" (func $hydra.lib.names.typeclass_eq) )
  (export "hydra.lib.names.typeclass_ord" (func $hydra.lib.names.typeclass_ord) )
  (func $hydra.lib.names.chars (result i32)
  i32.const 0 ;; string: "hydra.lib.chars"
)
  (func $hydra.lib.names.chars_is_alpha_num (result i32)
  i32.const 0 ;; string: "hydra.lib.chars.isAlphaNum"
)
  (func $hydra.lib.names.chars_is_lower (result i32)
  i32.const 0 ;; string: "hydra.lib.chars.isLower"
)
  (func $hydra.lib.names.chars_is_space (result i32)
  i32.const 0 ;; string: "hydra.lib.chars.isSpace"
)
  (func $hydra.lib.names.chars_is_upper (result i32)
  i32.const 0 ;; string: "hydra.lib.chars.isUpper"
)
  (func $hydra.lib.names.chars_to_lower (result i32)
  i32.const 0 ;; string: "hydra.lib.chars.toLower"
)
  (func $hydra.lib.names.chars_to_upper (result i32)
  i32.const 0 ;; string: "hydra.lib.chars.toUpper"
)
  (func $hydra.lib.names.eithers (result i32)
  i32.const 0 ;; string: "hydra.lib.eithers"
)
  (func $hydra.lib.names.eithers_bimap (result i32)
  i32.const 0 ;; string: "hydra.lib.eithers.bimap"
)
  (func $hydra.lib.names.eithers_bind (result i32)
  i32.const 0 ;; string: "hydra.lib.eithers.bind"
)
  (func $hydra.lib.names.eithers_either (result i32)
  i32.const 0 ;; string: "hydra.lib.eithers.either"
)
  (func $hydra.lib.names.eithers_foldl (result i32)
  i32.const 0 ;; string: "hydra.lib.eithers.foldl"
)
  (func $hydra.lib.names.eithers_from_left (result i32)
  i32.const 0 ;; string: "hydra.lib.eithers.fromLeft"
)
  (func $hydra.lib.names.eithers_from_right (result i32)
  i32.const 0 ;; string: "hydra.lib.eithers.fromRight"
)
  (func $hydra.lib.names.eithers_is_left (result i32)
  i32.const 0 ;; string: "hydra.lib.eithers.isLeft"
)
  (func $hydra.lib.names.eithers_is_right (result i32)
  i32.const 0 ;; string: "hydra.lib.eithers.isRight"
)
  (func $hydra.lib.names.eithers_lefts (result i32)
  i32.const 0 ;; string: "hydra.lib.eithers.lefts"
)
  (func $hydra.lib.names.eithers_map (result i32)
  i32.const 0 ;; string: "hydra.lib.eithers.map"
)
  (func $hydra.lib.names.eithers_map_list (result i32)
  i32.const 0 ;; string: "hydra.lib.eithers.mapList"
)
  (func $hydra.lib.names.eithers_map_maybe (result i32)
  i32.const 0 ;; string: "hydra.lib.eithers.mapMaybe"
)
  (func $hydra.lib.names.eithers_map_set (result i32)
  i32.const 0 ;; string: "hydra.lib.eithers.mapSet"
)
  (func $hydra.lib.names.eithers_partition_eithers (result i32)
  i32.const 0 ;; string: "hydra.lib.eithers.partitionEithers"
)
  (func $hydra.lib.names.eithers_rights (result i32)
  i32.const 0 ;; string: "hydra.lib.eithers.rights"
)
  (func $hydra.lib.names.equality (result i32)
  i32.const 0 ;; string: "hydra.lib.equality"
)
  (func $hydra.lib.names.equality_compare (result i32)
  i32.const 0 ;; string: "hydra.lib.equality.compare"
)
  (func $hydra.lib.names.equality_equal (result i32)
  i32.const 0 ;; string: "hydra.lib.equality.equal"
)
  (func $hydra.lib.names.equality_gt (result i32)
  i32.const 0 ;; string: "hydra.lib.equality.gt"
)
  (func $hydra.lib.names.equality_gte (result i32)
  i32.const 0 ;; string: "hydra.lib.equality.gte"
)
  (func $hydra.lib.names.equality_identity (result i32)
  i32.const 0 ;; string: "hydra.lib.equality.identity"
)
  (func $hydra.lib.names.equality_lt (result i32)
  i32.const 0 ;; string: "hydra.lib.equality.lt"
)
  (func $hydra.lib.names.equality_lte (result i32)
  i32.const 0 ;; string: "hydra.lib.equality.lte"
)
  (func $hydra.lib.names.equality_max (result i32)
  i32.const 0 ;; string: "hydra.lib.equality.max"
)
  (func $hydra.lib.names.equality_min (result i32)
  i32.const 0 ;; string: "hydra.lib.equality.min"
)
  (func $hydra.lib.names.lists (result i32)
  i32.const 0 ;; string: "hydra.lib.lists"
)
  (func $hydra.lib.names.lists_apply (result i32)
  i32.const 0 ;; string: "hydra.lib.lists.apply"
)
  (func $hydra.lib.names.lists_at (result i32)
  i32.const 0 ;; string: "hydra.lib.lists.at"
)
  (func $hydra.lib.names.lists_bind (result i32)
  i32.const 0 ;; string: "hydra.lib.lists.bind"
)
  (func $hydra.lib.names.lists_concat (result i32)
  i32.const 0 ;; string: "hydra.lib.lists.concat"
)
  (func $hydra.lib.names.lists_concat2 (result i32)
  i32.const 0 ;; string: "hydra.lib.lists.concat2"
)
  (func $hydra.lib.names.lists_cons (result i32)
  i32.const 0 ;; string: "hydra.lib.lists.cons"
)
  (func $hydra.lib.names.lists_drop (result i32)
  i32.const 0 ;; string: "hydra.lib.lists.drop"
)
  (func $hydra.lib.names.lists_drop_while (result i32)
  i32.const 0 ;; string: "hydra.lib.lists.dropWhile"
)
  (func $hydra.lib.names.lists_elem (result i32)
  i32.const 0 ;; string: "hydra.lib.lists.elem"
)
  (func $hydra.lib.names.lists_filter (result i32)
  i32.const 0 ;; string: "hydra.lib.lists.filter"
)
  (func $hydra.lib.names.lists_find (result i32)
  i32.const 0 ;; string: "hydra.lib.lists.find"
)
  (func $hydra.lib.names.lists_foldl (result i32)
  i32.const 0 ;; string: "hydra.lib.lists.foldl"
)
  (func $hydra.lib.names.lists_foldr (result i32)
  i32.const 0 ;; string: "hydra.lib.lists.foldr"
)
  (func $hydra.lib.names.lists_group (result i32)
  i32.const 0 ;; string: "hydra.lib.lists.group"
)
  (func $hydra.lib.names.lists_head (result i32)
  i32.const 0 ;; string: "hydra.lib.lists.head"
)
  (func $hydra.lib.names.lists_init (result i32)
  i32.const 0 ;; string: "hydra.lib.lists.init"
)
  (func $hydra.lib.names.lists_intercalate (result i32)
  i32.const 0 ;; string: "hydra.lib.lists.intercalate"
)
  (func $hydra.lib.names.lists_intersperse (result i32)
  i32.const 0 ;; string: "hydra.lib.lists.intersperse"
)
  (func $hydra.lib.names.lists_last (result i32)
  i32.const 0 ;; string: "hydra.lib.lists.last"
)
  (func $hydra.lib.names.lists_length (result i32)
  i32.const 0 ;; string: "hydra.lib.lists.length"
)
  (func $hydra.lib.names.lists_map (result i32)
  i32.const 0 ;; string: "hydra.lib.lists.map"
)
  (func $hydra.lib.names.lists_maybe_at (result i32)
  i32.const 0 ;; string: "hydra.lib.lists.maybeAt"
)
  (func $hydra.lib.names.lists_maybe_head (result i32)
  i32.const 0 ;; string: "hydra.lib.lists.maybeHead"
)
  (func $hydra.lib.names.lists_maybe_init (result i32)
  i32.const 0 ;; string: "hydra.lib.lists.maybeInit"
)
  (func $hydra.lib.names.lists_maybe_last (result i32)
  i32.const 0 ;; string: "hydra.lib.lists.maybeLast"
)
  (func $hydra.lib.names.lists_maybe_tail (result i32)
  i32.const 0 ;; string: "hydra.lib.lists.maybeTail"
)
  (func $hydra.lib.names.lists_nub (result i32)
  i32.const 0 ;; string: "hydra.lib.lists.nub"
)
  (func $hydra.lib.names.lists_null (result i32)
  i32.const 0 ;; string: "hydra.lib.lists.null"
)
  (func $hydra.lib.names.lists_partition (result i32)
  i32.const 0 ;; string: "hydra.lib.lists.partition"
)
  (func $hydra.lib.names.lists_pure (result i32)
  i32.const 0 ;; string: "hydra.lib.lists.pure"
)
  (func $hydra.lib.names.lists_replicate (result i32)
  i32.const 0 ;; string: "hydra.lib.lists.replicate"
)
  (func $hydra.lib.names.lists_reverse (result i32)
  i32.const 0 ;; string: "hydra.lib.lists.reverse"
)
  (func $hydra.lib.names.lists_safe_head (result i32)
  i32.const 0 ;; string: "hydra.lib.lists.safeHead"
)
  (func $hydra.lib.names.lists_singleton (result i32)
  i32.const 0 ;; string: "hydra.lib.lists.singleton"
)
  (func $hydra.lib.names.lists_sort (result i32)
  i32.const 0 ;; string: "hydra.lib.lists.sort"
)
  (func $hydra.lib.names.lists_sort_on (result i32)
  i32.const 0 ;; string: "hydra.lib.lists.sortOn"
)
  (func $hydra.lib.names.lists_span (result i32)
  i32.const 0 ;; string: "hydra.lib.lists.span"
)
  (func $hydra.lib.names.lists_tail (result i32)
  i32.const 0 ;; string: "hydra.lib.lists.tail"
)
  (func $hydra.lib.names.lists_take (result i32)
  i32.const 0 ;; string: "hydra.lib.lists.take"
)
  (func $hydra.lib.names.lists_transpose (result i32)
  i32.const 0 ;; string: "hydra.lib.lists.transpose"
)
  (func $hydra.lib.names.lists_zip (result i32)
  i32.const 0 ;; string: "hydra.lib.lists.zip"
)
  (func $hydra.lib.names.lists_zip_with (result i32)
  i32.const 0 ;; string: "hydra.lib.lists.zipWith"
)
  (func $hydra.lib.names.literals (result i32)
  i32.const 0 ;; string: "hydra.lib.literals"
)
  (func $hydra.lib.names.literals_bigfloat_to_bigint (result i32)
  i32.const 0 ;; string: "hydra.lib.literals.bigfloatToBigint"
)
  (func $hydra.lib.names.literals_bigfloat_to_float32 (result i32)
  i32.const 0 ;; string: "hydra.lib.literals.bigfloatToFloat32"
)
  (func $hydra.lib.names.literals_bigfloat_to_float64 (result i32)
  i32.const 0 ;; string: "hydra.lib.literals.bigfloatToFloat64"
)
  (func $hydra.lib.names.literals_bigint_to_bigfloat (result i32)
  i32.const 0 ;; string: "hydra.lib.literals.bigintToBigfloat"
)
  (func $hydra.lib.names.literals_bigint_to_int16 (result i32)
  i32.const 0 ;; string: "hydra.lib.literals.bigintToInt16"
)
  (func $hydra.lib.names.literals_bigint_to_int32 (result i32)
  i32.const 0 ;; string: "hydra.lib.literals.bigintToInt32"
)
  (func $hydra.lib.names.literals_bigint_to_int64 (result i32)
  i32.const 0 ;; string: "hydra.lib.literals.bigintToInt64"
)
  (func $hydra.lib.names.literals_bigint_to_int8 (result i32)
  i32.const 0 ;; string: "hydra.lib.literals.bigintToInt8"
)
  (func $hydra.lib.names.literals_bigint_to_uint16 (result i32)
  i32.const 0 ;; string: "hydra.lib.literals.bigintToUint16"
)
  (func $hydra.lib.names.literals_bigint_to_uint32 (result i32)
  i32.const 0 ;; string: "hydra.lib.literals.bigintToUint32"
)
  (func $hydra.lib.names.literals_bigint_to_uint64 (result i32)
  i32.const 0 ;; string: "hydra.lib.literals.bigintToUint64"
)
  (func $hydra.lib.names.literals_bigint_to_uint8 (result i32)
  i32.const 0 ;; string: "hydra.lib.literals.bigintToUint8"
)
  (func $hydra.lib.names.literals_binary_to_bytes (result i32)
  i32.const 0 ;; string: "hydra.lib.literals.binaryToBytes"
)
  (func $hydra.lib.names.literals_binary_to_string (result i32)
  i32.const 0 ;; string: "hydra.lib.literals.binaryToString"
)
  (func $hydra.lib.names.literals_float32_to_bigfloat (result i32)
  i32.const 0 ;; string: "hydra.lib.literals.float32ToBigfloat"
)
  (func $hydra.lib.names.literals_float64_to_bigfloat (result i32)
  i32.const 0 ;; string: "hydra.lib.literals.float64ToBigfloat"
)
  (func $hydra.lib.names.literals_int16_to_bigint (result i32)
  i32.const 0 ;; string: "hydra.lib.literals.int16ToBigint"
)
  (func $hydra.lib.names.literals_int32_to_bigint (result i32)
  i32.const 0 ;; string: "hydra.lib.literals.int32ToBigint"
)
  (func $hydra.lib.names.literals_int64_to_bigint (result i32)
  i32.const 0 ;; string: "hydra.lib.literals.int64ToBigint"
)
  (func $hydra.lib.names.literals_int8_to_bigint (result i32)
  i32.const 0 ;; string: "hydra.lib.literals.int8ToBigint"
)
  (func $hydra.lib.names.literals_read_bigfloat (result i32)
  i32.const 0 ;; string: "hydra.lib.literals.readBigfloat"
)
  (func $hydra.lib.names.literals_read_bigint (result i32)
  i32.const 0 ;; string: "hydra.lib.literals.readBigint"
)
  (func $hydra.lib.names.literals_read_boolean (result i32)
  i32.const 0 ;; string: "hydra.lib.literals.readBoolean"
)
  (func $hydra.lib.names.literals_read_float32 (result i32)
  i32.const 0 ;; string: "hydra.lib.literals.readFloat32"
)
  (func $hydra.lib.names.literals_read_float64 (result i32)
  i32.const 0 ;; string: "hydra.lib.literals.readFloat64"
)
  (func $hydra.lib.names.literals_read_int16 (result i32)
  i32.const 0 ;; string: "hydra.lib.literals.readInt16"
)
  (func $hydra.lib.names.literals_read_int32 (result i32)
  i32.const 0 ;; string: "hydra.lib.literals.readInt32"
)
  (func $hydra.lib.names.literals_read_int64 (result i32)
  i32.const 0 ;; string: "hydra.lib.literals.readInt64"
)
  (func $hydra.lib.names.literals_read_int8 (result i32)
  i32.const 0 ;; string: "hydra.lib.literals.readInt8"
)
  (func $hydra.lib.names.literals_read_string (result i32)
  i32.const 0 ;; string: "hydra.lib.literals.readString"
)
  (func $hydra.lib.names.literals_read_uint16 (result i32)
  i32.const 0 ;; string: "hydra.lib.literals.readUint16"
)
  (func $hydra.lib.names.literals_read_uint32 (result i32)
  i32.const 0 ;; string: "hydra.lib.literals.readUint32"
)
  (func $hydra.lib.names.literals_read_uint64 (result i32)
  i32.const 0 ;; string: "hydra.lib.literals.readUint64"
)
  (func $hydra.lib.names.literals_read_uint8 (result i32)
  i32.const 0 ;; string: "hydra.lib.literals.readUint8"
)
  (func $hydra.lib.names.literals_show_bigfloat (result i32)
  i32.const 0 ;; string: "hydra.lib.literals.showBigfloat"
)
  (func $hydra.lib.names.literals_show_bigint (result i32)
  i32.const 0 ;; string: "hydra.lib.literals.showBigint"
)
  (func $hydra.lib.names.literals_show_boolean (result i32)
  i32.const 0 ;; string: "hydra.lib.literals.showBoolean"
)
  (func $hydra.lib.names.literals_show_float32 (result i32)
  i32.const 0 ;; string: "hydra.lib.literals.showFloat32"
)
  (func $hydra.lib.names.literals_show_float64 (result i32)
  i32.const 0 ;; string: "hydra.lib.literals.showFloat64"
)
  (func $hydra.lib.names.literals_show_int16 (result i32)
  i32.const 0 ;; string: "hydra.lib.literals.showInt16"
)
  (func $hydra.lib.names.literals_show_int32 (result i32)
  i32.const 0 ;; string: "hydra.lib.literals.showInt32"
)
  (func $hydra.lib.names.literals_show_int64 (result i32)
  i32.const 0 ;; string: "hydra.lib.literals.showInt64"
)
  (func $hydra.lib.names.literals_show_int8 (result i32)
  i32.const 0 ;; string: "hydra.lib.literals.showInt8"
)
  (func $hydra.lib.names.literals_show_string (result i32)
  i32.const 0 ;; string: "hydra.lib.literals.showString"
)
  (func $hydra.lib.names.literals_show_uint16 (result i32)
  i32.const 0 ;; string: "hydra.lib.literals.showUint16"
)
  (func $hydra.lib.names.literals_show_uint32 (result i32)
  i32.const 0 ;; string: "hydra.lib.literals.showUint32"
)
  (func $hydra.lib.names.literals_show_uint64 (result i32)
  i32.const 0 ;; string: "hydra.lib.literals.showUint64"
)
  (func $hydra.lib.names.literals_show_uint8 (result i32)
  i32.const 0 ;; string: "hydra.lib.literals.showUint8"
)
  (func $hydra.lib.names.literals_string_to_binary (result i32)
  i32.const 0 ;; string: "hydra.lib.literals.stringToBinary"
)
  (func $hydra.lib.names.literals_uint16_to_bigint (result i32)
  i32.const 0 ;; string: "hydra.lib.literals.uint16ToBigint"
)
  (func $hydra.lib.names.literals_uint32_to_bigint (result i32)
  i32.const 0 ;; string: "hydra.lib.literals.uint32ToBigint"
)
  (func $hydra.lib.names.literals_uint64_to_bigint (result i32)
  i32.const 0 ;; string: "hydra.lib.literals.uint64ToBigint"
)
  (func $hydra.lib.names.literals_uint8_to_bigint (result i32)
  i32.const 0 ;; string: "hydra.lib.literals.uint8ToBigint"
)
  (func $hydra.lib.names.logic (result i32)
  i32.const 0 ;; string: "hydra.lib.logic"
)
  (func $hydra.lib.names.logic_and (result i32)
  i32.const 0 ;; string: "hydra.lib.logic.and"
)
  (func $hydra.lib.names.logic_if_else (result i32)
  i32.const 0 ;; string: "hydra.lib.logic.ifElse"
)
  (func $hydra.lib.names.logic_not (result i32)
  i32.const 0 ;; string: "hydra.lib.logic.not"
)
  (func $hydra.lib.names.logic_or (result i32)
  i32.const 0 ;; string: "hydra.lib.logic.or"
)
  (func $hydra.lib.names.maps (result i32)
  i32.const 0 ;; string: "hydra.lib.maps"
)
  (func $hydra.lib.names.maps_alter (result i32)
  i32.const 0 ;; string: "hydra.lib.maps.alter"
)
  (func $hydra.lib.names.maps_bimap (result i32)
  i32.const 0 ;; string: "hydra.lib.maps.bimap"
)
  (func $hydra.lib.names.maps_delete (result i32)
  i32.const 0 ;; string: "hydra.lib.maps.delete"
)
  (func $hydra.lib.names.maps_elems (result i32)
  i32.const 0 ;; string: "hydra.lib.maps.elems"
)
  (func $hydra.lib.names.maps_empty (result i32)
  i32.const 0 ;; string: "hydra.lib.maps.empty"
)
  (func $hydra.lib.names.maps_filter (result i32)
  i32.const 0 ;; string: "hydra.lib.maps.filter"
)
  (func $hydra.lib.names.maps_filter_with_key (result i32)
  i32.const 0 ;; string: "hydra.lib.maps.filterWithKey"
)
  (func $hydra.lib.names.maps_find_with_default (result i32)
  i32.const 0 ;; string: "hydra.lib.maps.findWithDefault"
)
  (func $hydra.lib.names.maps_from_list (result i32)
  i32.const 0 ;; string: "hydra.lib.maps.fromList"
)
  (func $hydra.lib.names.maps_insert (result i32)
  i32.const 0 ;; string: "hydra.lib.maps.insert"
)
  (func $hydra.lib.names.maps_keys (result i32)
  i32.const 0 ;; string: "hydra.lib.maps.keys"
)
  (func $hydra.lib.names.maps_lookup (result i32)
  i32.const 0 ;; string: "hydra.lib.maps.lookup"
)
  (func $hydra.lib.names.maps_map (result i32)
  i32.const 0 ;; string: "hydra.lib.maps.map"
)
  (func $hydra.lib.names.maps_map_keys (result i32)
  i32.const 0 ;; string: "hydra.lib.maps.mapKeys"
)
  (func $hydra.lib.names.maps_member (result i32)
  i32.const 0 ;; string: "hydra.lib.maps.member"
)
  (func $hydra.lib.names.maps_null (result i32)
  i32.const 0 ;; string: "hydra.lib.maps.null"
)
  (func $hydra.lib.names.maps_singleton (result i32)
  i32.const 0 ;; string: "hydra.lib.maps.singleton"
)
  (func $hydra.lib.names.maps_size (result i32)
  i32.const 0 ;; string: "hydra.lib.maps.size"
)
  (func $hydra.lib.names.maps_to_list (result i32)
  i32.const 0 ;; string: "hydra.lib.maps.toList"
)
  (func $hydra.lib.names.maps_union (result i32)
  i32.const 0 ;; string: "hydra.lib.maps.union"
)
  (func $hydra.lib.names.math (result i32)
  i32.const 0 ;; string: "hydra.lib.math"
)
  (func $hydra.lib.names.math_abs (result i32)
  i32.const 0 ;; string: "hydra.lib.math.abs"
)
  (func $hydra.lib.names.math_acos (result i32)
  i32.const 0 ;; string: "hydra.lib.math.acos"
)
  (func $hydra.lib.names.math_acosh (result i32)
  i32.const 0 ;; string: "hydra.lib.math.acosh"
)
  (func $hydra.lib.names.math_add (result i32)
  i32.const 0 ;; string: "hydra.lib.math.add"
)
  (func $hydra.lib.names.math_asin (result i32)
  i32.const 0 ;; string: "hydra.lib.math.asin"
)
  (func $hydra.lib.names.math_asinh (result i32)
  i32.const 0 ;; string: "hydra.lib.math.asinh"
)
  (func $hydra.lib.names.math_atan (result i32)
  i32.const 0 ;; string: "hydra.lib.math.atan"
)
  (func $hydra.lib.names.math_atan2 (result i32)
  i32.const 0 ;; string: "hydra.lib.math.atan2"
)
  (func $hydra.lib.names.math_atanh (result i32)
  i32.const 0 ;; string: "hydra.lib.math.atanh"
)
  (func $hydra.lib.names.math_ceiling (result i32)
  i32.const 0 ;; string: "hydra.lib.math.ceiling"
)
  (func $hydra.lib.names.math_cos (result i32)
  i32.const 0 ;; string: "hydra.lib.math.cos"
)
  (func $hydra.lib.names.math_cosh (result i32)
  i32.const 0 ;; string: "hydra.lib.math.cosh"
)
  (func $hydra.lib.names.math_div (result i32)
  i32.const 0 ;; string: "hydra.lib.math.div"
)
  (func $hydra.lib.names.math_e (result i32)
  i32.const 0 ;; string: "hydra.lib.math.e"
)
  (func $hydra.lib.names.math_even (result i32)
  i32.const 0 ;; string: "hydra.lib.math.even"
)
  (func $hydra.lib.names.math_exp (result i32)
  i32.const 0 ;; string: "hydra.lib.math.exp"
)
  (func $hydra.lib.names.math_floor (result i32)
  i32.const 0 ;; string: "hydra.lib.math.floor"
)
  (func $hydra.lib.names.math_log (result i32)
  i32.const 0 ;; string: "hydra.lib.math.log"
)
  (func $hydra.lib.names.math_log_base (result i32)
  i32.const 0 ;; string: "hydra.lib.math.logBase"
)
  (func $hydra.lib.names.math_max (result i32)
  i32.const 0 ;; string: "hydra.lib.math.max"
)
  (func $hydra.lib.names.math_maybe_div (result i32)
  i32.const 0 ;; string: "hydra.lib.math.maybeDiv"
)
  (func $hydra.lib.names.math_maybe_mod (result i32)
  i32.const 0 ;; string: "hydra.lib.math.maybeMod"
)
  (func $hydra.lib.names.math_maybe_pred (result i32)
  i32.const 0 ;; string: "hydra.lib.math.maybePred"
)
  (func $hydra.lib.names.math_maybe_rem (result i32)
  i32.const 0 ;; string: "hydra.lib.math.maybeRem"
)
  (func $hydra.lib.names.math_maybe_succ (result i32)
  i32.const 0 ;; string: "hydra.lib.math.maybeSucc"
)
  (func $hydra.lib.names.math_min (result i32)
  i32.const 0 ;; string: "hydra.lib.math.min"
)
  (func $hydra.lib.names.math_mod (result i32)
  i32.const 0 ;; string: "hydra.lib.math.mod"
)
  (func $hydra.lib.names.math_mul (result i32)
  i32.const 0 ;; string: "hydra.lib.math.mul"
)
  (func $hydra.lib.names.math_negate (result i32)
  i32.const 0 ;; string: "hydra.lib.math.negate"
)
  (func $hydra.lib.names.math_odd (result i32)
  i32.const 0 ;; string: "hydra.lib.math.odd"
)
  (func $hydra.lib.names.math_pi (result i32)
  i32.const 0 ;; string: "hydra.lib.math.pi"
)
  (func $hydra.lib.names.math_pow (result i32)
  i32.const 0 ;; string: "hydra.lib.math.pow"
)
  (func $hydra.lib.names.math_pred (result i32)
  i32.const 0 ;; string: "hydra.lib.math.pred"
)
  (func $hydra.lib.names.math_range (result i32)
  i32.const 0 ;; string: "hydra.lib.math.range"
)
  (func $hydra.lib.names.math_rem (result i32)
  i32.const 0 ;; string: "hydra.lib.math.rem"
)
  (func $hydra.lib.names.math_round (result i32)
  i32.const 0 ;; string: "hydra.lib.math.round"
)
  (func $hydra.lib.names.math_round_bigfloat (result i32)
  i32.const 0 ;; string: "hydra.lib.math.roundBigfloat"
)
  (func $hydra.lib.names.math_round_float32 (result i32)
  i32.const 0 ;; string: "hydra.lib.math.roundFloat32"
)
  (func $hydra.lib.names.math_round_float64 (result i32)
  i32.const 0 ;; string: "hydra.lib.math.roundFloat64"
)
  (func $hydra.lib.names.math_signum (result i32)
  i32.const 0 ;; string: "hydra.lib.math.signum"
)
  (func $hydra.lib.names.math_sin (result i32)
  i32.const 0 ;; string: "hydra.lib.math.sin"
)
  (func $hydra.lib.names.math_sinh (result i32)
  i32.const 0 ;; string: "hydra.lib.math.sinh"
)
  (func $hydra.lib.names.math_sqrt (result i32)
  i32.const 0 ;; string: "hydra.lib.math.sqrt"
)
  (func $hydra.lib.names.math_sub (result i32)
  i32.const 0 ;; string: "hydra.lib.math.sub"
)
  (func $hydra.lib.names.math_succ (result i32)
  i32.const 0 ;; string: "hydra.lib.math.succ"
)
  (func $hydra.lib.names.math_tan (result i32)
  i32.const 0 ;; string: "hydra.lib.math.tan"
)
  (func $hydra.lib.names.math_tanh (result i32)
  i32.const 0 ;; string: "hydra.lib.math.tanh"
)
  (func $hydra.lib.names.math_truncate (result i32)
  i32.const 0 ;; string: "hydra.lib.math.truncate"
)
  (func $hydra.lib.names.maybes (result i32)
  i32.const 0 ;; string: "hydra.lib.maybes"
)
  (func $hydra.lib.names.maybes_apply (result i32)
  i32.const 0 ;; string: "hydra.lib.maybes.apply"
)
  (func $hydra.lib.names.maybes_bind (result i32)
  i32.const 0 ;; string: "hydra.lib.maybes.bind"
)
  (func $hydra.lib.names.maybes_cases (result i32)
  i32.const 0 ;; string: "hydra.lib.maybes.cases"
)
  (func $hydra.lib.names.maybes_cat (result i32)
  i32.const 0 ;; string: "hydra.lib.maybes.cat"
)
  (func $hydra.lib.names.maybes_compose (result i32)
  i32.const 0 ;; string: "hydra.lib.maybes.compose"
)
  (func $hydra.lib.names.maybes_from_just (result i32)
  i32.const 0 ;; string: "hydra.lib.maybes.fromJust"
)
  (func $hydra.lib.names.maybes_from_maybe (result i32)
  i32.const 0 ;; string: "hydra.lib.maybes.fromMaybe"
)
  (func $hydra.lib.names.maybes_is_just (result i32)
  i32.const 0 ;; string: "hydra.lib.maybes.isJust"
)
  (func $hydra.lib.names.maybes_is_nothing (result i32)
  i32.const 0 ;; string: "hydra.lib.maybes.isNothing"
)
  (func $hydra.lib.names.maybes_map (result i32)
  i32.const 0 ;; string: "hydra.lib.maybes.map"
)
  (func $hydra.lib.names.maybes_map_maybe (result i32)
  i32.const 0 ;; string: "hydra.lib.maybes.mapMaybe"
)
  (func $hydra.lib.names.maybes_maybe (result i32)
  i32.const 0 ;; string: "hydra.lib.maybes.maybe"
)
  (func $hydra.lib.names.maybes_pure (result i32)
  i32.const 0 ;; string: "hydra.lib.maybes.pure"
)
  (func $hydra.lib.names.maybes_to_list (result i32)
  i32.const 0 ;; string: "hydra.lib.maybes.toList"
)
  (func $hydra.lib.names.pairs (result i32)
  i32.const 0 ;; string: "hydra.lib.pairs"
)
  (func $hydra.lib.names.pairs_bimap (result i32)
  i32.const 0 ;; string: "hydra.lib.pairs.bimap"
)
  (func $hydra.lib.names.pairs_first (result i32)
  i32.const 0 ;; string: "hydra.lib.pairs.first"
)
  (func $hydra.lib.names.pairs_second (result i32)
  i32.const 0 ;; string: "hydra.lib.pairs.second"
)
  (func $hydra.lib.names.regex (result i32)
  i32.const 0 ;; string: "hydra.lib.regex"
)
  (func $hydra.lib.names.regex_find (result i32)
  i32.const 0 ;; string: "hydra.lib.regex.find"
)
  (func $hydra.lib.names.regex_find_all (result i32)
  i32.const 0 ;; string: "hydra.lib.regex.findAll"
)
  (func $hydra.lib.names.regex_matches (result i32)
  i32.const 0 ;; string: "hydra.lib.regex.matches"
)
  (func $hydra.lib.names.regex_replace (result i32)
  i32.const 0 ;; string: "hydra.lib.regex.replace"
)
  (func $hydra.lib.names.regex_replace_all (result i32)
  i32.const 0 ;; string: "hydra.lib.regex.replaceAll"
)
  (func $hydra.lib.names.regex_split (result i32)
  i32.const 0 ;; string: "hydra.lib.regex.split"
)
  (func $hydra.lib.names.sets (result i32)
  i32.const 0 ;; string: "hydra.lib.sets"
)
  (func $hydra.lib.names.sets_delete (result i32)
  i32.const 0 ;; string: "hydra.lib.sets.delete"
)
  (func $hydra.lib.names.sets_difference (result i32)
  i32.const 0 ;; string: "hydra.lib.sets.difference"
)
  (func $hydra.lib.names.sets_empty (result i32)
  i32.const 0 ;; string: "hydra.lib.sets.empty"
)
  (func $hydra.lib.names.sets_from_list (result i32)
  i32.const 0 ;; string: "hydra.lib.sets.fromList"
)
  (func $hydra.lib.names.sets_insert (result i32)
  i32.const 0 ;; string: "hydra.lib.sets.insert"
)
  (func $hydra.lib.names.sets_intersection (result i32)
  i32.const 0 ;; string: "hydra.lib.sets.intersection"
)
  (func $hydra.lib.names.sets_map (result i32)
  i32.const 0 ;; string: "hydra.lib.sets.map"
)
  (func $hydra.lib.names.sets_member (result i32)
  i32.const 0 ;; string: "hydra.lib.sets.member"
)
  (func $hydra.lib.names.sets_null (result i32)
  i32.const 0 ;; string: "hydra.lib.sets.null"
)
  (func $hydra.lib.names.sets_singleton (result i32)
  i32.const 0 ;; string: "hydra.lib.sets.singleton"
)
  (func $hydra.lib.names.sets_size (result i32)
  i32.const 0 ;; string: "hydra.lib.sets.size"
)
  (func $hydra.lib.names.sets_to_list (result i32)
  i32.const 0 ;; string: "hydra.lib.sets.toList"
)
  (func $hydra.lib.names.sets_union (result i32)
  i32.const 0 ;; string: "hydra.lib.sets.union"
)
  (func $hydra.lib.names.sets_unions (result i32)
  i32.const 0 ;; string: "hydra.lib.sets.unions"
)
  (func $hydra.lib.names.strings (result i32)
  i32.const 0 ;; string: "hydra.lib.strings"
)
  (func $hydra.lib.names.strings_cat (result i32)
  i32.const 0 ;; string: "hydra.lib.strings.cat"
)
  (func $hydra.lib.names.strings_cat2 (result i32)
  i32.const 0 ;; string: "hydra.lib.strings.cat2"
)
  (func $hydra.lib.names.strings_char_at (result i32)
  i32.const 0 ;; string: "hydra.lib.strings.charAt"
)
  (func $hydra.lib.names.strings_from_list (result i32)
  i32.const 0 ;; string: "hydra.lib.strings.fromList"
)
  (func $hydra.lib.names.strings_intercalate (result i32)
  i32.const 0 ;; string: "hydra.lib.strings.intercalate"
)
  (func $hydra.lib.names.strings_length (result i32)
  i32.const 0 ;; string: "hydra.lib.strings.length"
)
  (func $hydra.lib.names.strings_lines (result i32)
  i32.const 0 ;; string: "hydra.lib.strings.lines"
)
  (func $hydra.lib.names.strings_maybe_char_at (result i32)
  i32.const 0 ;; string: "hydra.lib.strings.maybeCharAt"
)
  (func $hydra.lib.names.strings_null (result i32)
  i32.const 0 ;; string: "hydra.lib.strings.null"
)
  (func $hydra.lib.names.strings_split_on (result i32)
  i32.const 0 ;; string: "hydra.lib.strings.splitOn"
)
  (func $hydra.lib.names.strings_to_list (result i32)
  i32.const 0 ;; string: "hydra.lib.strings.toList"
)
  (func $hydra.lib.names.strings_to_lower (result i32)
  i32.const 0 ;; string: "hydra.lib.strings.toLower"
)
  (func $hydra.lib.names.strings_to_upper (result i32)
  i32.const 0 ;; string: "hydra.lib.strings.toUpper"
)
  (func $hydra.lib.names.strings_unlines (result i32)
  i32.const 0 ;; string: "hydra.lib.strings.unlines"
)
  (func $hydra.lib.names.typeclass (result i32)
  i32.const 0 ;; string: "hydra.typeclass"
)
  (func $hydra.lib.names.typeclass_eq (result i32)
  i32.const 0 ;; string: "hydra.typeclass.Eq"
)
  (func $hydra.lib.names.typeclass_ord (result i32)
  i32.const 0 ;; string: "hydra.typeclass.Ord"
)
)
