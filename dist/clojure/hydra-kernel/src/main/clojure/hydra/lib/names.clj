(ns hydra.lib.names
  (:require [hydra.core :refer :all] [hydra.packaging :refer :all]
))

(declare hydra_lib_names_chars hydra_lib_names_chars_is_alpha_num hydra_lib_names_chars_is_lower hydra_lib_names_chars_is_space hydra_lib_names_chars_is_upper hydra_lib_names_chars_to_lower hydra_lib_names_chars_to_upper hydra_lib_names_eithers hydra_lib_names_eithers_bimap hydra_lib_names_eithers_bind hydra_lib_names_eithers_either hydra_lib_names_eithers_foldl hydra_lib_names_eithers_from_left hydra_lib_names_eithers_from_right hydra_lib_names_eithers_is_left hydra_lib_names_eithers_is_right hydra_lib_names_eithers_lefts hydra_lib_names_eithers_map hydra_lib_names_eithers_map_list hydra_lib_names_eithers_map_maybe hydra_lib_names_eithers_map_set hydra_lib_names_eithers_partition_eithers hydra_lib_names_eithers_rights hydra_lib_names_equality hydra_lib_names_equality_compare hydra_lib_names_equality_equal hydra_lib_names_equality_gt hydra_lib_names_equality_gte hydra_lib_names_equality_identity hydra_lib_names_equality_lt hydra_lib_names_equality_lte hydra_lib_names_equality_max hydra_lib_names_equality_min hydra_lib_names_lists hydra_lib_names_lists_apply hydra_lib_names_lists_bind hydra_lib_names_lists_concat hydra_lib_names_lists_concat2 hydra_lib_names_lists_cons hydra_lib_names_lists_drop hydra_lib_names_lists_drop_while hydra_lib_names_lists_elem hydra_lib_names_lists_filter hydra_lib_names_lists_find hydra_lib_names_lists_foldl hydra_lib_names_lists_foldr hydra_lib_names_lists_group hydra_lib_names_lists_intercalate hydra_lib_names_lists_intersperse hydra_lib_names_lists_length hydra_lib_names_lists_map hydra_lib_names_lists_maybe_at hydra_lib_names_lists_maybe_head hydra_lib_names_lists_maybe_init hydra_lib_names_lists_maybe_last hydra_lib_names_lists_maybe_tail hydra_lib_names_lists_nub hydra_lib_names_lists_null hydra_lib_names_lists_partition hydra_lib_names_lists_pure hydra_lib_names_lists_replicate hydra_lib_names_lists_reverse hydra_lib_names_lists_singleton hydra_lib_names_lists_sort hydra_lib_names_lists_sort_on hydra_lib_names_lists_span hydra_lib_names_lists_take hydra_lib_names_lists_transpose hydra_lib_names_lists_uncons hydra_lib_names_lists_zip hydra_lib_names_lists_zip_with hydra_lib_names_literals hydra_lib_names_literals_bigfloat_to_bigint hydra_lib_names_literals_bigfloat_to_float32 hydra_lib_names_literals_bigfloat_to_float64 hydra_lib_names_literals_bigint_to_bigfloat hydra_lib_names_literals_bigint_to_decimal hydra_lib_names_literals_bigint_to_int16 hydra_lib_names_literals_bigint_to_int32 hydra_lib_names_literals_bigint_to_int64 hydra_lib_names_literals_bigint_to_int8 hydra_lib_names_literals_bigint_to_uint16 hydra_lib_names_literals_bigint_to_uint32 hydra_lib_names_literals_bigint_to_uint64 hydra_lib_names_literals_bigint_to_uint8 hydra_lib_names_literals_binary_to_bytes hydra_lib_names_literals_binary_to_string hydra_lib_names_literals_decimal_to_bigint hydra_lib_names_literals_decimal_to_float32 hydra_lib_names_literals_decimal_to_float64 hydra_lib_names_literals_float32_to_bigfloat hydra_lib_names_literals_float32_to_decimal hydra_lib_names_literals_float64_to_bigfloat hydra_lib_names_literals_float64_to_decimal hydra_lib_names_literals_int16_to_bigint hydra_lib_names_literals_int32_to_bigint hydra_lib_names_literals_int64_to_bigint hydra_lib_names_literals_int8_to_bigint hydra_lib_names_literals_read_bigfloat hydra_lib_names_literals_read_bigint hydra_lib_names_literals_read_boolean hydra_lib_names_literals_read_decimal hydra_lib_names_literals_read_float32 hydra_lib_names_literals_read_float64 hydra_lib_names_literals_read_int16 hydra_lib_names_literals_read_int32 hydra_lib_names_literals_read_int64 hydra_lib_names_literals_read_int8 hydra_lib_names_literals_read_string hydra_lib_names_literals_read_uint16 hydra_lib_names_literals_read_uint32 hydra_lib_names_literals_read_uint64 hydra_lib_names_literals_read_uint8 hydra_lib_names_literals_show_bigfloat hydra_lib_names_literals_show_bigint hydra_lib_names_literals_show_boolean hydra_lib_names_literals_show_decimal hydra_lib_names_literals_show_float32 hydra_lib_names_literals_show_float64 hydra_lib_names_literals_show_int16 hydra_lib_names_literals_show_int32 hydra_lib_names_literals_show_int64 hydra_lib_names_literals_show_int8 hydra_lib_names_literals_show_string hydra_lib_names_literals_show_uint16 hydra_lib_names_literals_show_uint32 hydra_lib_names_literals_show_uint64 hydra_lib_names_literals_show_uint8 hydra_lib_names_literals_string_to_binary hydra_lib_names_literals_uint16_to_bigint hydra_lib_names_literals_uint32_to_bigint hydra_lib_names_literals_uint64_to_bigint hydra_lib_names_literals_uint8_to_bigint hydra_lib_names_logic hydra_lib_names_logic_and hydra_lib_names_logic_if_else hydra_lib_names_logic_not hydra_lib_names_logic_or hydra_lib_names_maps hydra_lib_names_maps_alter hydra_lib_names_maps_bimap hydra_lib_names_maps_delete hydra_lib_names_maps_elems hydra_lib_names_maps_empty hydra_lib_names_maps_filter hydra_lib_names_maps_filter_with_key hydra_lib_names_maps_find_with_default hydra_lib_names_maps_from_list hydra_lib_names_maps_insert hydra_lib_names_maps_keys hydra_lib_names_maps_lookup hydra_lib_names_maps_map hydra_lib_names_maps_map_keys hydra_lib_names_maps_member hydra_lib_names_maps_null hydra_lib_names_maps_singleton hydra_lib_names_maps_size hydra_lib_names_maps_to_list hydra_lib_names_maps_union hydra_lib_names_math hydra_lib_names_math_abs hydra_lib_names_math_acos hydra_lib_names_math_acosh hydra_lib_names_math_add hydra_lib_names_math_add_float64 hydra_lib_names_math_asin hydra_lib_names_math_asinh hydra_lib_names_math_atan hydra_lib_names_math_atan2 hydra_lib_names_math_atanh hydra_lib_names_math_ceiling hydra_lib_names_math_cos hydra_lib_names_math_cosh hydra_lib_names_math_e hydra_lib_names_math_even hydra_lib_names_math_exp hydra_lib_names_math_floor hydra_lib_names_math_log hydra_lib_names_math_log_base hydra_lib_names_math_max hydra_lib_names_math_maybe_div hydra_lib_names_math_maybe_mod hydra_lib_names_math_maybe_pred hydra_lib_names_math_maybe_rem hydra_lib_names_math_maybe_succ hydra_lib_names_math_min hydra_lib_names_math_mul hydra_lib_names_math_mul_float64 hydra_lib_names_math_negate hydra_lib_names_math_negate_float64 hydra_lib_names_math_odd hydra_lib_names_math_pi hydra_lib_names_math_pow hydra_lib_names_math_range hydra_lib_names_math_round hydra_lib_names_math_round_bigfloat hydra_lib_names_math_round_float32 hydra_lib_names_math_round_float64 hydra_lib_names_math_signum hydra_lib_names_math_sin hydra_lib_names_math_sinh hydra_lib_names_math_sqrt hydra_lib_names_math_sub hydra_lib_names_math_sub_float64 hydra_lib_names_math_tan hydra_lib_names_math_tanh hydra_lib_names_math_truncate hydra_lib_names_maybes hydra_lib_names_maybes_apply hydra_lib_names_maybes_bind hydra_lib_names_maybes_cases hydra_lib_names_maybes_cat hydra_lib_names_maybes_compose hydra_lib_names_maybes_from_maybe hydra_lib_names_maybes_is_just hydra_lib_names_maybes_is_nothing hydra_lib_names_maybes_map hydra_lib_names_maybes_map_maybe hydra_lib_names_maybes_maybe hydra_lib_names_maybes_pure hydra_lib_names_maybes_to_list hydra_lib_names_pairs hydra_lib_names_pairs_bimap hydra_lib_names_pairs_first hydra_lib_names_pairs_second hydra_lib_names_regex hydra_lib_names_regex_find hydra_lib_names_regex_find_all hydra_lib_names_regex_matches hydra_lib_names_regex_replace hydra_lib_names_regex_replace_all hydra_lib_names_regex_split hydra_lib_names_sets hydra_lib_names_sets_delete hydra_lib_names_sets_difference hydra_lib_names_sets_empty hydra_lib_names_sets_from_list hydra_lib_names_sets_insert hydra_lib_names_sets_intersection hydra_lib_names_sets_map hydra_lib_names_sets_member hydra_lib_names_sets_null hydra_lib_names_sets_singleton hydra_lib_names_sets_size hydra_lib_names_sets_to_list hydra_lib_names_sets_union hydra_lib_names_sets_unions hydra_lib_names_strings hydra_lib_names_strings_cat hydra_lib_names_strings_cat2 hydra_lib_names_strings_from_list hydra_lib_names_strings_intercalate hydra_lib_names_strings_length hydra_lib_names_strings_lines hydra_lib_names_strings_maybe_char_at hydra_lib_names_strings_null hydra_lib_names_strings_split_on hydra_lib_names_strings_to_list hydra_lib_names_strings_to_lower hydra_lib_names_strings_to_upper hydra_lib_names_strings_unlines hydra_lib_names_typeclass hydra_lib_names_typeclass_eq hydra_lib_names_typeclass_ord)

(def hydra_lib_names_chars "hydra.lib.chars")

(def hydra_lib_names_chars_is_alpha_num "hydra.lib.chars.isAlphaNum")

(def hydra_lib_names_chars_is_lower "hydra.lib.chars.isLower")

(def hydra_lib_names_chars_is_space "hydra.lib.chars.isSpace")

(def hydra_lib_names_chars_is_upper "hydra.lib.chars.isUpper")

(def hydra_lib_names_chars_to_lower "hydra.lib.chars.toLower")

(def hydra_lib_names_chars_to_upper "hydra.lib.chars.toUpper")

(def hydra_lib_names_eithers "hydra.lib.eithers")

(def hydra_lib_names_eithers_bimap "hydra.lib.eithers.bimap")

(def hydra_lib_names_eithers_bind "hydra.lib.eithers.bind")

(def hydra_lib_names_eithers_either "hydra.lib.eithers.either")

(def hydra_lib_names_eithers_foldl "hydra.lib.eithers.foldl")

(def hydra_lib_names_eithers_from_left "hydra.lib.eithers.fromLeft")

(def hydra_lib_names_eithers_from_right "hydra.lib.eithers.fromRight")

(def hydra_lib_names_eithers_is_left "hydra.lib.eithers.isLeft")

(def hydra_lib_names_eithers_is_right "hydra.lib.eithers.isRight")

(def hydra_lib_names_eithers_lefts "hydra.lib.eithers.lefts")

(def hydra_lib_names_eithers_map "hydra.lib.eithers.map")

(def hydra_lib_names_eithers_map_list "hydra.lib.eithers.mapList")

(def hydra_lib_names_eithers_map_maybe "hydra.lib.eithers.mapMaybe")

(def hydra_lib_names_eithers_map_set "hydra.lib.eithers.mapSet")

(def hydra_lib_names_eithers_partition_eithers "hydra.lib.eithers.partitionEithers")

(def hydra_lib_names_eithers_rights "hydra.lib.eithers.rights")

(def hydra_lib_names_equality "hydra.lib.equality")

(def hydra_lib_names_equality_compare "hydra.lib.equality.compare")

(def hydra_lib_names_equality_equal "hydra.lib.equality.equal")

(def hydra_lib_names_equality_gt "hydra.lib.equality.gt")

(def hydra_lib_names_equality_gte "hydra.lib.equality.gte")

(def hydra_lib_names_equality_identity "hydra.lib.equality.identity")

(def hydra_lib_names_equality_lt "hydra.lib.equality.lt")

(def hydra_lib_names_equality_lte "hydra.lib.equality.lte")

(def hydra_lib_names_equality_max "hydra.lib.equality.max")

(def hydra_lib_names_equality_min "hydra.lib.equality.min")

(def hydra_lib_names_lists "hydra.lib.lists")

(def hydra_lib_names_lists_apply "hydra.lib.lists.apply")

(def hydra_lib_names_lists_bind "hydra.lib.lists.bind")

(def hydra_lib_names_lists_concat "hydra.lib.lists.concat")

(def hydra_lib_names_lists_concat2 "hydra.lib.lists.concat2")

(def hydra_lib_names_lists_cons "hydra.lib.lists.cons")

(def hydra_lib_names_lists_drop "hydra.lib.lists.drop")

(def hydra_lib_names_lists_drop_while "hydra.lib.lists.dropWhile")

(def hydra_lib_names_lists_elem "hydra.lib.lists.elem")

(def hydra_lib_names_lists_filter "hydra.lib.lists.filter")

(def hydra_lib_names_lists_find "hydra.lib.lists.find")

(def hydra_lib_names_lists_foldl "hydra.lib.lists.foldl")

(def hydra_lib_names_lists_foldr "hydra.lib.lists.foldr")

(def hydra_lib_names_lists_group "hydra.lib.lists.group")

(def hydra_lib_names_lists_intercalate "hydra.lib.lists.intercalate")

(def hydra_lib_names_lists_intersperse "hydra.lib.lists.intersperse")

(def hydra_lib_names_lists_length "hydra.lib.lists.length")

(def hydra_lib_names_lists_map "hydra.lib.lists.map")

(def hydra_lib_names_lists_maybe_at "hydra.lib.lists.maybeAt")

(def hydra_lib_names_lists_maybe_head "hydra.lib.lists.maybeHead")

(def hydra_lib_names_lists_maybe_init "hydra.lib.lists.maybeInit")

(def hydra_lib_names_lists_maybe_last "hydra.lib.lists.maybeLast")

(def hydra_lib_names_lists_maybe_tail "hydra.lib.lists.maybeTail")

(def hydra_lib_names_lists_nub "hydra.lib.lists.nub")

(def hydra_lib_names_lists_null "hydra.lib.lists.null")

(def hydra_lib_names_lists_partition "hydra.lib.lists.partition")

(def hydra_lib_names_lists_pure "hydra.lib.lists.pure")

(def hydra_lib_names_lists_replicate "hydra.lib.lists.replicate")

(def hydra_lib_names_lists_reverse "hydra.lib.lists.reverse")

(def hydra_lib_names_lists_singleton "hydra.lib.lists.singleton")

(def hydra_lib_names_lists_sort "hydra.lib.lists.sort")

(def hydra_lib_names_lists_sort_on "hydra.lib.lists.sortOn")

(def hydra_lib_names_lists_span "hydra.lib.lists.span")

(def hydra_lib_names_lists_take "hydra.lib.lists.take")

(def hydra_lib_names_lists_transpose "hydra.lib.lists.transpose")

(def hydra_lib_names_lists_uncons "hydra.lib.lists.uncons")

(def hydra_lib_names_lists_zip "hydra.lib.lists.zip")

(def hydra_lib_names_lists_zip_with "hydra.lib.lists.zipWith")

(def hydra_lib_names_literals "hydra.lib.literals")

(def hydra_lib_names_literals_bigfloat_to_bigint "hydra.lib.literals.bigfloatToBigint")

(def hydra_lib_names_literals_bigfloat_to_float32 "hydra.lib.literals.bigfloatToFloat32")

(def hydra_lib_names_literals_bigfloat_to_float64 "hydra.lib.literals.bigfloatToFloat64")

(def hydra_lib_names_literals_bigint_to_bigfloat "hydra.lib.literals.bigintToBigfloat")

(def hydra_lib_names_literals_bigint_to_decimal "hydra.lib.literals.bigintToDecimal")

(def hydra_lib_names_literals_bigint_to_int16 "hydra.lib.literals.bigintToInt16")

(def hydra_lib_names_literals_bigint_to_int32 "hydra.lib.literals.bigintToInt32")

(def hydra_lib_names_literals_bigint_to_int64 "hydra.lib.literals.bigintToInt64")

(def hydra_lib_names_literals_bigint_to_int8 "hydra.lib.literals.bigintToInt8")

(def hydra_lib_names_literals_bigint_to_uint16 "hydra.lib.literals.bigintToUint16")

(def hydra_lib_names_literals_bigint_to_uint32 "hydra.lib.literals.bigintToUint32")

(def hydra_lib_names_literals_bigint_to_uint64 "hydra.lib.literals.bigintToUint64")

(def hydra_lib_names_literals_bigint_to_uint8 "hydra.lib.literals.bigintToUint8")

(def hydra_lib_names_literals_binary_to_bytes "hydra.lib.literals.binaryToBytes")

(def hydra_lib_names_literals_binary_to_string "hydra.lib.literals.binaryToString")

(def hydra_lib_names_literals_decimal_to_bigint "hydra.lib.literals.decimalToBigint")

(def hydra_lib_names_literals_decimal_to_float32 "hydra.lib.literals.decimalToFloat32")

(def hydra_lib_names_literals_decimal_to_float64 "hydra.lib.literals.decimalToFloat64")

(def hydra_lib_names_literals_float32_to_bigfloat "hydra.lib.literals.float32ToBigfloat")

(def hydra_lib_names_literals_float32_to_decimal "hydra.lib.literals.float32ToDecimal")

(def hydra_lib_names_literals_float64_to_bigfloat "hydra.lib.literals.float64ToBigfloat")

(def hydra_lib_names_literals_float64_to_decimal "hydra.lib.literals.float64ToDecimal")

(def hydra_lib_names_literals_int16_to_bigint "hydra.lib.literals.int16ToBigint")

(def hydra_lib_names_literals_int32_to_bigint "hydra.lib.literals.int32ToBigint")

(def hydra_lib_names_literals_int64_to_bigint "hydra.lib.literals.int64ToBigint")

(def hydra_lib_names_literals_int8_to_bigint "hydra.lib.literals.int8ToBigint")

(def hydra_lib_names_literals_read_bigfloat "hydra.lib.literals.readBigfloat")

(def hydra_lib_names_literals_read_bigint "hydra.lib.literals.readBigint")

(def hydra_lib_names_literals_read_boolean "hydra.lib.literals.readBoolean")

(def hydra_lib_names_literals_read_decimal "hydra.lib.literals.readDecimal")

(def hydra_lib_names_literals_read_float32 "hydra.lib.literals.readFloat32")

(def hydra_lib_names_literals_read_float64 "hydra.lib.literals.readFloat64")

(def hydra_lib_names_literals_read_int16 "hydra.lib.literals.readInt16")

(def hydra_lib_names_literals_read_int32 "hydra.lib.literals.readInt32")

(def hydra_lib_names_literals_read_int64 "hydra.lib.literals.readInt64")

(def hydra_lib_names_literals_read_int8 "hydra.lib.literals.readInt8")

(def hydra_lib_names_literals_read_string "hydra.lib.literals.readString")

(def hydra_lib_names_literals_read_uint16 "hydra.lib.literals.readUint16")

(def hydra_lib_names_literals_read_uint32 "hydra.lib.literals.readUint32")

(def hydra_lib_names_literals_read_uint64 "hydra.lib.literals.readUint64")

(def hydra_lib_names_literals_read_uint8 "hydra.lib.literals.readUint8")

(def hydra_lib_names_literals_show_bigfloat "hydra.lib.literals.showBigfloat")

(def hydra_lib_names_literals_show_bigint "hydra.lib.literals.showBigint")

(def hydra_lib_names_literals_show_boolean "hydra.lib.literals.showBoolean")

(def hydra_lib_names_literals_show_decimal "hydra.lib.literals.showDecimal")

(def hydra_lib_names_literals_show_float32 "hydra.lib.literals.showFloat32")

(def hydra_lib_names_literals_show_float64 "hydra.lib.literals.showFloat64")

(def hydra_lib_names_literals_show_int16 "hydra.lib.literals.showInt16")

(def hydra_lib_names_literals_show_int32 "hydra.lib.literals.showInt32")

(def hydra_lib_names_literals_show_int64 "hydra.lib.literals.showInt64")

(def hydra_lib_names_literals_show_int8 "hydra.lib.literals.showInt8")

(def hydra_lib_names_literals_show_string "hydra.lib.literals.showString")

(def hydra_lib_names_literals_show_uint16 "hydra.lib.literals.showUint16")

(def hydra_lib_names_literals_show_uint32 "hydra.lib.literals.showUint32")

(def hydra_lib_names_literals_show_uint64 "hydra.lib.literals.showUint64")

(def hydra_lib_names_literals_show_uint8 "hydra.lib.literals.showUint8")

(def hydra_lib_names_literals_string_to_binary "hydra.lib.literals.stringToBinary")

(def hydra_lib_names_literals_uint16_to_bigint "hydra.lib.literals.uint16ToBigint")

(def hydra_lib_names_literals_uint32_to_bigint "hydra.lib.literals.uint32ToBigint")

(def hydra_lib_names_literals_uint64_to_bigint "hydra.lib.literals.uint64ToBigint")

(def hydra_lib_names_literals_uint8_to_bigint "hydra.lib.literals.uint8ToBigint")

(def hydra_lib_names_logic "hydra.lib.logic")

(def hydra_lib_names_logic_and "hydra.lib.logic.and")

(def hydra_lib_names_logic_if_else "hydra.lib.logic.ifElse")

(def hydra_lib_names_logic_not "hydra.lib.logic.not")

(def hydra_lib_names_logic_or "hydra.lib.logic.or")

(def hydra_lib_names_maps "hydra.lib.maps")

(def hydra_lib_names_maps_alter "hydra.lib.maps.alter")

(def hydra_lib_names_maps_bimap "hydra.lib.maps.bimap")

(def hydra_lib_names_maps_delete "hydra.lib.maps.delete")

(def hydra_lib_names_maps_elems "hydra.lib.maps.elems")

(def hydra_lib_names_maps_empty "hydra.lib.maps.empty")

(def hydra_lib_names_maps_filter "hydra.lib.maps.filter")

(def hydra_lib_names_maps_filter_with_key "hydra.lib.maps.filterWithKey")

(def hydra_lib_names_maps_find_with_default "hydra.lib.maps.findWithDefault")

(def hydra_lib_names_maps_from_list "hydra.lib.maps.fromList")

(def hydra_lib_names_maps_insert "hydra.lib.maps.insert")

(def hydra_lib_names_maps_keys "hydra.lib.maps.keys")

(def hydra_lib_names_maps_lookup "hydra.lib.maps.lookup")

(def hydra_lib_names_maps_map "hydra.lib.maps.map")

(def hydra_lib_names_maps_map_keys "hydra.lib.maps.mapKeys")

(def hydra_lib_names_maps_member "hydra.lib.maps.member")

(def hydra_lib_names_maps_null "hydra.lib.maps.null")

(def hydra_lib_names_maps_singleton "hydra.lib.maps.singleton")

(def hydra_lib_names_maps_size "hydra.lib.maps.size")

(def hydra_lib_names_maps_to_list "hydra.lib.maps.toList")

(def hydra_lib_names_maps_union "hydra.lib.maps.union")

(def hydra_lib_names_math "hydra.lib.math")

(def hydra_lib_names_math_abs "hydra.lib.math.abs")

(def hydra_lib_names_math_acos "hydra.lib.math.acos")

(def hydra_lib_names_math_acosh "hydra.lib.math.acosh")

(def hydra_lib_names_math_add "hydra.lib.math.add")

(def hydra_lib_names_math_add_float64 "hydra.lib.math.addFloat64")

(def hydra_lib_names_math_asin "hydra.lib.math.asin")

(def hydra_lib_names_math_asinh "hydra.lib.math.asinh")

(def hydra_lib_names_math_atan "hydra.lib.math.atan")

(def hydra_lib_names_math_atan2 "hydra.lib.math.atan2")

(def hydra_lib_names_math_atanh "hydra.lib.math.atanh")

(def hydra_lib_names_math_ceiling "hydra.lib.math.ceiling")

(def hydra_lib_names_math_cos "hydra.lib.math.cos")

(def hydra_lib_names_math_cosh "hydra.lib.math.cosh")

(def hydra_lib_names_math_e "hydra.lib.math.e")

(def hydra_lib_names_math_even "hydra.lib.math.even")

(def hydra_lib_names_math_exp "hydra.lib.math.exp")

(def hydra_lib_names_math_floor "hydra.lib.math.floor")

(def hydra_lib_names_math_log "hydra.lib.math.log")

(def hydra_lib_names_math_log_base "hydra.lib.math.logBase")

(def hydra_lib_names_math_max "hydra.lib.math.max")

(def hydra_lib_names_math_maybe_div "hydra.lib.math.maybeDiv")

(def hydra_lib_names_math_maybe_mod "hydra.lib.math.maybeMod")

(def hydra_lib_names_math_maybe_pred "hydra.lib.math.maybePred")

(def hydra_lib_names_math_maybe_rem "hydra.lib.math.maybeRem")

(def hydra_lib_names_math_maybe_succ "hydra.lib.math.maybeSucc")

(def hydra_lib_names_math_min "hydra.lib.math.min")

(def hydra_lib_names_math_mul "hydra.lib.math.mul")

(def hydra_lib_names_math_mul_float64 "hydra.lib.math.mulFloat64")

(def hydra_lib_names_math_negate "hydra.lib.math.negate")

(def hydra_lib_names_math_negate_float64 "hydra.lib.math.negateFloat64")

(def hydra_lib_names_math_odd "hydra.lib.math.odd")

(def hydra_lib_names_math_pi "hydra.lib.math.pi")

(def hydra_lib_names_math_pow "hydra.lib.math.pow")

(def hydra_lib_names_math_range "hydra.lib.math.range")

(def hydra_lib_names_math_round "hydra.lib.math.round")

(def hydra_lib_names_math_round_bigfloat "hydra.lib.math.roundBigfloat")

(def hydra_lib_names_math_round_float32 "hydra.lib.math.roundFloat32")

(def hydra_lib_names_math_round_float64 "hydra.lib.math.roundFloat64")

(def hydra_lib_names_math_signum "hydra.lib.math.signum")

(def hydra_lib_names_math_sin "hydra.lib.math.sin")

(def hydra_lib_names_math_sinh "hydra.lib.math.sinh")

(def hydra_lib_names_math_sqrt "hydra.lib.math.sqrt")

(def hydra_lib_names_math_sub "hydra.lib.math.sub")

(def hydra_lib_names_math_sub_float64 "hydra.lib.math.subFloat64")

(def hydra_lib_names_math_tan "hydra.lib.math.tan")

(def hydra_lib_names_math_tanh "hydra.lib.math.tanh")

(def hydra_lib_names_math_truncate "hydra.lib.math.truncate")

(def hydra_lib_names_maybes "hydra.lib.maybes")

(def hydra_lib_names_maybes_apply "hydra.lib.maybes.apply")

(def hydra_lib_names_maybes_bind "hydra.lib.maybes.bind")

(def hydra_lib_names_maybes_cases "hydra.lib.maybes.cases")

(def hydra_lib_names_maybes_cat "hydra.lib.maybes.cat")

(def hydra_lib_names_maybes_compose "hydra.lib.maybes.compose")

(def hydra_lib_names_maybes_from_maybe "hydra.lib.maybes.fromMaybe")

(def hydra_lib_names_maybes_is_just "hydra.lib.maybes.isJust")

(def hydra_lib_names_maybes_is_nothing "hydra.lib.maybes.isNothing")

(def hydra_lib_names_maybes_map "hydra.lib.maybes.map")

(def hydra_lib_names_maybes_map_maybe "hydra.lib.maybes.mapMaybe")

(def hydra_lib_names_maybes_maybe "hydra.lib.maybes.maybe")

(def hydra_lib_names_maybes_pure "hydra.lib.maybes.pure")

(def hydra_lib_names_maybes_to_list "hydra.lib.maybes.toList")

(def hydra_lib_names_pairs "hydra.lib.pairs")

(def hydra_lib_names_pairs_bimap "hydra.lib.pairs.bimap")

(def hydra_lib_names_pairs_first "hydra.lib.pairs.first")

(def hydra_lib_names_pairs_second "hydra.lib.pairs.second")

(def hydra_lib_names_regex "hydra.lib.regex")

(def hydra_lib_names_regex_find "hydra.lib.regex.find")

(def hydra_lib_names_regex_find_all "hydra.lib.regex.findAll")

(def hydra_lib_names_regex_matches "hydra.lib.regex.matches")

(def hydra_lib_names_regex_replace "hydra.lib.regex.replace")

(def hydra_lib_names_regex_replace_all "hydra.lib.regex.replaceAll")

(def hydra_lib_names_regex_split "hydra.lib.regex.split")

(def hydra_lib_names_sets "hydra.lib.sets")

(def hydra_lib_names_sets_delete "hydra.lib.sets.delete")

(def hydra_lib_names_sets_difference "hydra.lib.sets.difference")

(def hydra_lib_names_sets_empty "hydra.lib.sets.empty")

(def hydra_lib_names_sets_from_list "hydra.lib.sets.fromList")

(def hydra_lib_names_sets_insert "hydra.lib.sets.insert")

(def hydra_lib_names_sets_intersection "hydra.lib.sets.intersection")

(def hydra_lib_names_sets_map "hydra.lib.sets.map")

(def hydra_lib_names_sets_member "hydra.lib.sets.member")

(def hydra_lib_names_sets_null "hydra.lib.sets.null")

(def hydra_lib_names_sets_singleton "hydra.lib.sets.singleton")

(def hydra_lib_names_sets_size "hydra.lib.sets.size")

(def hydra_lib_names_sets_to_list "hydra.lib.sets.toList")

(def hydra_lib_names_sets_union "hydra.lib.sets.union")

(def hydra_lib_names_sets_unions "hydra.lib.sets.unions")

(def hydra_lib_names_strings "hydra.lib.strings")

(def hydra_lib_names_strings_cat "hydra.lib.strings.cat")

(def hydra_lib_names_strings_cat2 "hydra.lib.strings.cat2")

(def hydra_lib_names_strings_from_list "hydra.lib.strings.fromList")

(def hydra_lib_names_strings_intercalate "hydra.lib.strings.intercalate")

(def hydra_lib_names_strings_length "hydra.lib.strings.length")

(def hydra_lib_names_strings_lines "hydra.lib.strings.lines")

(def hydra_lib_names_strings_maybe_char_at "hydra.lib.strings.maybeCharAt")

(def hydra_lib_names_strings_null "hydra.lib.strings.null")

(def hydra_lib_names_strings_split_on "hydra.lib.strings.splitOn")

(def hydra_lib_names_strings_to_list "hydra.lib.strings.toList")

(def hydra_lib_names_strings_to_lower "hydra.lib.strings.toLower")

(def hydra_lib_names_strings_to_upper "hydra.lib.strings.toUpper")

(def hydra_lib_names_strings_unlines "hydra.lib.strings.unlines")

(def hydra_lib_names_typeclass "hydra.typeclass")

(def hydra_lib_names_typeclass_eq "hydra.typeclass.Eq")

(def hydra_lib_names_typeclass_ord "hydra.typeclass.Ord")
