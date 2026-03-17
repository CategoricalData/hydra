(defpackage :hydra.lib.names
(:use :cl :hydra.core :hydra.module)
(:export :hydra_lib_names_chars :hydra_lib_names_chars_is_alpha_num :hydra_lib_names_chars_is_lower :hydra_lib_names_chars_is_space :hydra_lib_names_chars_is_upper :hydra_lib_names_chars_to_lower :hydra_lib_names_chars_to_upper :hydra_lib_names_eithers :hydra_lib_names_eithers_bimap :hydra_lib_names_eithers_bind :hydra_lib_names_eithers_either :hydra_lib_names_eithers_foldl :hydra_lib_names_eithers_from_left :hydra_lib_names_eithers_from_right :hydra_lib_names_eithers_is_left :hydra_lib_names_eithers_is_right :hydra_lib_names_eithers_lefts :hydra_lib_names_eithers_map :hydra_lib_names_eithers_map_list :hydra_lib_names_eithers_map_maybe :hydra_lib_names_eithers_map_set :hydra_lib_names_eithers_partition_eithers :hydra_lib_names_eithers_rights :hydra_lib_names_equality :hydra_lib_names_equality_compare :hydra_lib_names_equality_equal :hydra_lib_names_equality_gt :hydra_lib_names_equality_gte :hydra_lib_names_equality_identity :hydra_lib_names_equality_lt :hydra_lib_names_equality_lte :hydra_lib_names_equality_max :hydra_lib_names_equality_min :hydra_lib_names_lists :hydra_lib_names_lists_apply :hydra_lib_names_lists_at :hydra_lib_names_lists_bind :hydra_lib_names_lists_concat :hydra_lib_names_lists_concat2 :hydra_lib_names_lists_cons :hydra_lib_names_lists_drop :hydra_lib_names_lists_drop_while :hydra_lib_names_lists_elem :hydra_lib_names_lists_filter :hydra_lib_names_lists_find :hydra_lib_names_lists_foldl :hydra_lib_names_lists_foldr :hydra_lib_names_lists_group :hydra_lib_names_lists_head :hydra_lib_names_lists_init :hydra_lib_names_lists_intercalate :hydra_lib_names_lists_intersperse :hydra_lib_names_lists_last :hydra_lib_names_lists_length :hydra_lib_names_lists_map :hydra_lib_names_lists_nub :hydra_lib_names_lists_null :hydra_lib_names_lists_partition :hydra_lib_names_lists_pure :hydra_lib_names_lists_replicate :hydra_lib_names_lists_reverse :hydra_lib_names_lists_safe_head :hydra_lib_names_lists_singleton :hydra_lib_names_lists_sort :hydra_lib_names_lists_sort_on :hydra_lib_names_lists_span :hydra_lib_names_lists_tail :hydra_lib_names_lists_take :hydra_lib_names_lists_transpose :hydra_lib_names_lists_zip :hydra_lib_names_lists_zip_with :hydra_lib_names_literals :hydra_lib_names_literals_bigfloat_to_bigint :hydra_lib_names_literals_bigfloat_to_float32 :hydra_lib_names_literals_bigfloat_to_float64 :hydra_lib_names_literals_bigint_to_bigfloat :hydra_lib_names_literals_bigint_to_int16 :hydra_lib_names_literals_bigint_to_int32 :hydra_lib_names_literals_bigint_to_int64 :hydra_lib_names_literals_bigint_to_int8 :hydra_lib_names_literals_bigint_to_uint16 :hydra_lib_names_literals_bigint_to_uint32 :hydra_lib_names_literals_bigint_to_uint64 :hydra_lib_names_literals_bigint_to_uint8 :hydra_lib_names_literals_binary_to_bytes :hydra_lib_names_literals_binary_to_string :hydra_lib_names_literals_float32_to_bigfloat :hydra_lib_names_literals_float64_to_bigfloat :hydra_lib_names_literals_int16_to_bigint :hydra_lib_names_literals_int32_to_bigint :hydra_lib_names_literals_int64_to_bigint :hydra_lib_names_literals_int8_to_bigint :hydra_lib_names_literals_read_bigfloat :hydra_lib_names_literals_read_bigint :hydra_lib_names_literals_read_boolean :hydra_lib_names_literals_read_float32 :hydra_lib_names_literals_read_float64 :hydra_lib_names_literals_read_int16 :hydra_lib_names_literals_read_int32 :hydra_lib_names_literals_read_int64 :hydra_lib_names_literals_read_int8 :hydra_lib_names_literals_read_string :hydra_lib_names_literals_read_uint16 :hydra_lib_names_literals_read_uint32 :hydra_lib_names_literals_read_uint64 :hydra_lib_names_literals_read_uint8 :hydra_lib_names_literals_show_bigfloat :hydra_lib_names_literals_show_bigint :hydra_lib_names_literals_show_boolean :hydra_lib_names_literals_show_float32 :hydra_lib_names_literals_show_float64 :hydra_lib_names_literals_show_int16 :hydra_lib_names_literals_show_int32 :hydra_lib_names_literals_show_int64 :hydra_lib_names_literals_show_int8 :hydra_lib_names_literals_show_string :hydra_lib_names_literals_show_uint16 :hydra_lib_names_literals_show_uint32 :hydra_lib_names_literals_show_uint64 :hydra_lib_names_literals_show_uint8 :hydra_lib_names_literals_string_to_binary :hydra_lib_names_literals_uint16_to_bigint :hydra_lib_names_literals_uint32_to_bigint :hydra_lib_names_literals_uint64_to_bigint :hydra_lib_names_literals_uint8_to_bigint :hydra_lib_names_logic :hydra_lib_names_logic_and :hydra_lib_names_logic_if_else :hydra_lib_names_logic_not :hydra_lib_names_logic_or :hydra_lib_names_maps :hydra_lib_names_maps_alter :hydra_lib_names_maps_bimap :hydra_lib_names_maps_delete :hydra_lib_names_maps_elems :hydra_lib_names_maps_empty :hydra_lib_names_maps_filter :hydra_lib_names_maps_filter_with_key :hydra_lib_names_maps_find_with_default :hydra_lib_names_maps_from_list :hydra_lib_names_maps_insert :hydra_lib_names_maps_keys :hydra_lib_names_maps_lookup :hydra_lib_names_maps_map :hydra_lib_names_maps_map_keys :hydra_lib_names_maps_member :hydra_lib_names_maps_null :hydra_lib_names_maps_singleton :hydra_lib_names_maps_size :hydra_lib_names_maps_to_list :hydra_lib_names_maps_union :hydra_lib_names_math :hydra_lib_names_math_abs :hydra_lib_names_math_acos :hydra_lib_names_math_acosh :hydra_lib_names_math_add :hydra_lib_names_math_asin :hydra_lib_names_math_asinh :hydra_lib_names_math_atan :hydra_lib_names_math_atan2 :hydra_lib_names_math_atanh :hydra_lib_names_math_ceiling :hydra_lib_names_math_cos :hydra_lib_names_math_cosh :hydra_lib_names_math_div :hydra_lib_names_math_e :hydra_lib_names_math_even :hydra_lib_names_math_exp :hydra_lib_names_math_floor :hydra_lib_names_math_log :hydra_lib_names_math_log_base :hydra_lib_names_math_max :hydra_lib_names_math_min :hydra_lib_names_math_mod :hydra_lib_names_math_mul :hydra_lib_names_math_negate :hydra_lib_names_math_odd :hydra_lib_names_math_pi :hydra_lib_names_math_pow :hydra_lib_names_math_pred :hydra_lib_names_math_range :hydra_lib_names_math_rem :hydra_lib_names_math_round :hydra_lib_names_math_round_bigfloat :hydra_lib_names_math_round_float32 :hydra_lib_names_math_round_float64 :hydra_lib_names_math_signum :hydra_lib_names_math_sin :hydra_lib_names_math_sinh :hydra_lib_names_math_sqrt :hydra_lib_names_math_sub :hydra_lib_names_math_succ :hydra_lib_names_math_tan :hydra_lib_names_math_tanh :hydra_lib_names_math_truncate :hydra_lib_names_maybes :hydra_lib_names_maybes_apply :hydra_lib_names_maybes_bind :hydra_lib_names_maybes_cases :hydra_lib_names_maybes_cat :hydra_lib_names_maybes_compose :hydra_lib_names_maybes_from_just :hydra_lib_names_maybes_from_maybe :hydra_lib_names_maybes_is_just :hydra_lib_names_maybes_is_nothing :hydra_lib_names_maybes_map :hydra_lib_names_maybes_map_maybe :hydra_lib_names_maybes_maybe :hydra_lib_names_maybes_pure :hydra_lib_names_maybes_to_list :hydra_lib_names_pairs :hydra_lib_names_pairs_bimap :hydra_lib_names_pairs_first :hydra_lib_names_pairs_second :hydra_lib_names_sets :hydra_lib_names_sets_delete :hydra_lib_names_sets_difference :hydra_lib_names_sets_empty :hydra_lib_names_sets_from_list :hydra_lib_names_sets_insert :hydra_lib_names_sets_intersection :hydra_lib_names_sets_map :hydra_lib_names_sets_member :hydra_lib_names_sets_null :hydra_lib_names_sets_singleton :hydra_lib_names_sets_size :hydra_lib_names_sets_to_list :hydra_lib_names_sets_union :hydra_lib_names_sets_unions :hydra_lib_names_strings :hydra_lib_names_strings_cat :hydra_lib_names_strings_cat2 :hydra_lib_names_strings_char_at :hydra_lib_names_strings_from_list :hydra_lib_names_strings_intercalate :hydra_lib_names_strings_length :hydra_lib_names_strings_lines :hydra_lib_names_strings_null :hydra_lib_names_strings_split_on :hydra_lib_names_strings_to_list :hydra_lib_names_strings_to_lower :hydra_lib_names_strings_to_upper :hydra_lib_names_strings_unlines :hydra_lib_names_typeclass :hydra_lib_names_typeclass_eq :hydra_lib_names_typeclass_ord))

(in-package :hydra.lib.names)

(cl:defvar hydra_lib_names_chars "hydra.lib.chars")

(cl:defvar hydra_lib_names_chars_is_alpha_num "hydra.lib.chars.isAlphaNum")

(cl:defvar hydra_lib_names_chars_is_lower "hydra.lib.chars.isLower")

(cl:defvar hydra_lib_names_chars_is_space "hydra.lib.chars.isSpace")

(cl:defvar hydra_lib_names_chars_is_upper "hydra.lib.chars.isUpper")

(cl:defvar hydra_lib_names_chars_to_lower "hydra.lib.chars.toLower")

(cl:defvar hydra_lib_names_chars_to_upper "hydra.lib.chars.toUpper")

(cl:defvar hydra_lib_names_eithers "hydra.lib.eithers")

(cl:defvar hydra_lib_names_eithers_bimap "hydra.lib.eithers.bimap")

(cl:defvar hydra_lib_names_eithers_bind "hydra.lib.eithers.bind")

(cl:defvar hydra_lib_names_eithers_either "hydra.lib.eithers.either")

(cl:defvar hydra_lib_names_eithers_foldl "hydra.lib.eithers.foldl")

(cl:defvar hydra_lib_names_eithers_from_left "hydra.lib.eithers.fromLeft")

(cl:defvar hydra_lib_names_eithers_from_right "hydra.lib.eithers.fromRight")

(cl:defvar hydra_lib_names_eithers_is_left "hydra.lib.eithers.isLeft")

(cl:defvar hydra_lib_names_eithers_is_right "hydra.lib.eithers.isRight")

(cl:defvar hydra_lib_names_eithers_lefts "hydra.lib.eithers.lefts")

(cl:defvar hydra_lib_names_eithers_map "hydra.lib.eithers.map")

(cl:defvar hydra_lib_names_eithers_map_list "hydra.lib.eithers.mapList")

(cl:defvar hydra_lib_names_eithers_map_maybe "hydra.lib.eithers.mapMaybe")

(cl:defvar hydra_lib_names_eithers_map_set "hydra.lib.eithers.mapSet")

(cl:defvar hydra_lib_names_eithers_partition_eithers "hydra.lib.eithers.partitionEithers")

(cl:defvar hydra_lib_names_eithers_rights "hydra.lib.eithers.rights")

(cl:defvar hydra_lib_names_equality "hydra.lib.equality")

(cl:defvar hydra_lib_names_equality_compare "hydra.lib.equality.compare")

(cl:defvar hydra_lib_names_equality_equal "hydra.lib.equality.equal")

(cl:defvar hydra_lib_names_equality_gt "hydra.lib.equality.gt")

(cl:defvar hydra_lib_names_equality_gte "hydra.lib.equality.gte")

(cl:defvar hydra_lib_names_equality_identity "hydra.lib.equality.identity")

(cl:defvar hydra_lib_names_equality_lt "hydra.lib.equality.lt")

(cl:defvar hydra_lib_names_equality_lte "hydra.lib.equality.lte")

(cl:defvar hydra_lib_names_equality_max "hydra.lib.equality.max")

(cl:defvar hydra_lib_names_equality_min "hydra.lib.equality.min")

(cl:defvar hydra_lib_names_lists "hydra.lib.lists")

(cl:defvar hydra_lib_names_lists_apply "hydra.lib.lists.apply")

(cl:defvar hydra_lib_names_lists_at "hydra.lib.lists.at")

(cl:defvar hydra_lib_names_lists_bind "hydra.lib.lists.bind")

(cl:defvar hydra_lib_names_lists_concat "hydra.lib.lists.concat")

(cl:defvar hydra_lib_names_lists_concat2 "hydra.lib.lists.concat2")

(cl:defvar hydra_lib_names_lists_cons "hydra.lib.lists.cons")

(cl:defvar hydra_lib_names_lists_drop "hydra.lib.lists.drop")

(cl:defvar hydra_lib_names_lists_drop_while "hydra.lib.lists.dropWhile")

(cl:defvar hydra_lib_names_lists_elem "hydra.lib.lists.elem")

(cl:defvar hydra_lib_names_lists_filter "hydra.lib.lists.filter")

(cl:defvar hydra_lib_names_lists_find "hydra.lib.lists.find")

(cl:defvar hydra_lib_names_lists_foldl "hydra.lib.lists.foldl")

(cl:defvar hydra_lib_names_lists_foldr "hydra.lib.lists.foldr")

(cl:defvar hydra_lib_names_lists_group "hydra.lib.lists.group")

(cl:defvar hydra_lib_names_lists_head "hydra.lib.lists.head")

(cl:defvar hydra_lib_names_lists_init "hydra.lib.lists.init")

(cl:defvar hydra_lib_names_lists_intercalate "hydra.lib.lists.intercalate")

(cl:defvar hydra_lib_names_lists_intersperse "hydra.lib.lists.intersperse")

(cl:defvar hydra_lib_names_lists_last "hydra.lib.lists.last")

(cl:defvar hydra_lib_names_lists_length "hydra.lib.lists.length")

(cl:defvar hydra_lib_names_lists_map "hydra.lib.lists.map")

(cl:defvar hydra_lib_names_lists_nub "hydra.lib.lists.nub")

(cl:defvar hydra_lib_names_lists_null "hydra.lib.lists.null")

(cl:defvar hydra_lib_names_lists_partition "hydra.lib.lists.partition")

(cl:defvar hydra_lib_names_lists_pure "hydra.lib.lists.pure")

(cl:defvar hydra_lib_names_lists_replicate "hydra.lib.lists.replicate")

(cl:defvar hydra_lib_names_lists_reverse "hydra.lib.lists.reverse")

(cl:defvar hydra_lib_names_lists_safe_head "hydra.lib.lists.safeHead")

(cl:defvar hydra_lib_names_lists_singleton "hydra.lib.lists.singleton")

(cl:defvar hydra_lib_names_lists_sort "hydra.lib.lists.sort")

(cl:defvar hydra_lib_names_lists_sort_on "hydra.lib.lists.sortOn")

(cl:defvar hydra_lib_names_lists_span "hydra.lib.lists.span")

(cl:defvar hydra_lib_names_lists_tail "hydra.lib.lists.tail")

(cl:defvar hydra_lib_names_lists_take "hydra.lib.lists.take")

(cl:defvar hydra_lib_names_lists_transpose "hydra.lib.lists.transpose")

(cl:defvar hydra_lib_names_lists_zip "hydra.lib.lists.zip")

(cl:defvar hydra_lib_names_lists_zip_with "hydra.lib.lists.zipWith")

(cl:defvar hydra_lib_names_literals "hydra.lib.literals")

(cl:defvar hydra_lib_names_literals_bigfloat_to_bigint "hydra.lib.literals.bigfloatToBigint")

(cl:defvar hydra_lib_names_literals_bigfloat_to_float32 "hydra.lib.literals.bigfloatToFloat32")

(cl:defvar hydra_lib_names_literals_bigfloat_to_float64 "hydra.lib.literals.bigfloatToFloat64")

(cl:defvar hydra_lib_names_literals_bigint_to_bigfloat "hydra.lib.literals.bigintToBigfloat")

(cl:defvar hydra_lib_names_literals_bigint_to_int16 "hydra.lib.literals.bigintToInt16")

(cl:defvar hydra_lib_names_literals_bigint_to_int32 "hydra.lib.literals.bigintToInt32")

(cl:defvar hydra_lib_names_literals_bigint_to_int64 "hydra.lib.literals.bigintToInt64")

(cl:defvar hydra_lib_names_literals_bigint_to_int8 "hydra.lib.literals.bigintToInt8")

(cl:defvar hydra_lib_names_literals_bigint_to_uint16 "hydra.lib.literals.bigintToUint16")

(cl:defvar hydra_lib_names_literals_bigint_to_uint32 "hydra.lib.literals.bigintToUint32")

(cl:defvar hydra_lib_names_literals_bigint_to_uint64 "hydra.lib.literals.bigintToUint64")

(cl:defvar hydra_lib_names_literals_bigint_to_uint8 "hydra.lib.literals.bigintToUint8")

(cl:defvar hydra_lib_names_literals_binary_to_bytes "hydra.lib.literals.binaryToBytes")

(cl:defvar hydra_lib_names_literals_binary_to_string "hydra.lib.literals.binaryToString")

(cl:defvar hydra_lib_names_literals_float32_to_bigfloat "hydra.lib.literals.float32ToBigfloat")

(cl:defvar hydra_lib_names_literals_float64_to_bigfloat "hydra.lib.literals.float64ToBigfloat")

(cl:defvar hydra_lib_names_literals_int16_to_bigint "hydra.lib.literals.int16ToBigint")

(cl:defvar hydra_lib_names_literals_int32_to_bigint "hydra.lib.literals.int32ToBigint")

(cl:defvar hydra_lib_names_literals_int64_to_bigint "hydra.lib.literals.int64ToBigint")

(cl:defvar hydra_lib_names_literals_int8_to_bigint "hydra.lib.literals.int8ToBigint")

(cl:defvar hydra_lib_names_literals_read_bigfloat "hydra.lib.literals.readBigfloat")

(cl:defvar hydra_lib_names_literals_read_bigint "hydra.lib.literals.readBigint")

(cl:defvar hydra_lib_names_literals_read_boolean "hydra.lib.literals.readBoolean")

(cl:defvar hydra_lib_names_literals_read_float32 "hydra.lib.literals.readFloat32")

(cl:defvar hydra_lib_names_literals_read_float64 "hydra.lib.literals.readFloat64")

(cl:defvar hydra_lib_names_literals_read_int16 "hydra.lib.literals.readInt16")

(cl:defvar hydra_lib_names_literals_read_int32 "hydra.lib.literals.readInt32")

(cl:defvar hydra_lib_names_literals_read_int64 "hydra.lib.literals.readInt64")

(cl:defvar hydra_lib_names_literals_read_int8 "hydra.lib.literals.readInt8")

(cl:defvar hydra_lib_names_literals_read_string "hydra.lib.literals.readString")

(cl:defvar hydra_lib_names_literals_read_uint16 "hydra.lib.literals.readUint16")

(cl:defvar hydra_lib_names_literals_read_uint32 "hydra.lib.literals.readUint32")

(cl:defvar hydra_lib_names_literals_read_uint64 "hydra.lib.literals.readUint64")

(cl:defvar hydra_lib_names_literals_read_uint8 "hydra.lib.literals.readUint8")

(cl:defvar hydra_lib_names_literals_show_bigfloat "hydra.lib.literals.showBigfloat")

(cl:defvar hydra_lib_names_literals_show_bigint "hydra.lib.literals.showBigint")

(cl:defvar hydra_lib_names_literals_show_boolean "hydra.lib.literals.showBoolean")

(cl:defvar hydra_lib_names_literals_show_float32 "hydra.lib.literals.showFloat32")

(cl:defvar hydra_lib_names_literals_show_float64 "hydra.lib.literals.showFloat64")

(cl:defvar hydra_lib_names_literals_show_int16 "hydra.lib.literals.showInt16")

(cl:defvar hydra_lib_names_literals_show_int32 "hydra.lib.literals.showInt32")

(cl:defvar hydra_lib_names_literals_show_int64 "hydra.lib.literals.showInt64")

(cl:defvar hydra_lib_names_literals_show_int8 "hydra.lib.literals.showInt8")

(cl:defvar hydra_lib_names_literals_show_string "hydra.lib.literals.showString")

(cl:defvar hydra_lib_names_literals_show_uint16 "hydra.lib.literals.showUint16")

(cl:defvar hydra_lib_names_literals_show_uint32 "hydra.lib.literals.showUint32")

(cl:defvar hydra_lib_names_literals_show_uint64 "hydra.lib.literals.showUint64")

(cl:defvar hydra_lib_names_literals_show_uint8 "hydra.lib.literals.showUint8")

(cl:defvar hydra_lib_names_literals_string_to_binary "hydra.lib.literals.stringToBinary")

(cl:defvar hydra_lib_names_literals_uint16_to_bigint "hydra.lib.literals.uint16ToBigint")

(cl:defvar hydra_lib_names_literals_uint32_to_bigint "hydra.lib.literals.uint32ToBigint")

(cl:defvar hydra_lib_names_literals_uint64_to_bigint "hydra.lib.literals.uint64ToBigint")

(cl:defvar hydra_lib_names_literals_uint8_to_bigint "hydra.lib.literals.uint8ToBigint")

(cl:defvar hydra_lib_names_logic "hydra.lib.logic")

(cl:defvar hydra_lib_names_logic_and "hydra.lib.logic.and")

(cl:defvar hydra_lib_names_logic_if_else "hydra.lib.logic.ifElse")

(cl:defvar hydra_lib_names_logic_not "hydra.lib.logic.not")

(cl:defvar hydra_lib_names_logic_or "hydra.lib.logic.or")

(cl:defvar hydra_lib_names_maps "hydra.lib.maps")

(cl:defvar hydra_lib_names_maps_alter "hydra.lib.maps.alter")

(cl:defvar hydra_lib_names_maps_bimap "hydra.lib.maps.bimap")

(cl:defvar hydra_lib_names_maps_delete "hydra.lib.maps.delete")

(cl:defvar hydra_lib_names_maps_elems "hydra.lib.maps.elems")

(cl:defvar hydra_lib_names_maps_empty "hydra.lib.maps.empty")

(cl:defvar hydra_lib_names_maps_filter "hydra.lib.maps.filter")

(cl:defvar hydra_lib_names_maps_filter_with_key "hydra.lib.maps.filterWithKey")

(cl:defvar hydra_lib_names_maps_find_with_default "hydra.lib.maps.findWithDefault")

(cl:defvar hydra_lib_names_maps_from_list "hydra.lib.maps.fromList")

(cl:defvar hydra_lib_names_maps_insert "hydra.lib.maps.insert")

(cl:defvar hydra_lib_names_maps_keys "hydra.lib.maps.keys")

(cl:defvar hydra_lib_names_maps_lookup "hydra.lib.maps.lookup")

(cl:defvar hydra_lib_names_maps_map "hydra.lib.maps.map")

(cl:defvar hydra_lib_names_maps_map_keys "hydra.lib.maps.mapKeys")

(cl:defvar hydra_lib_names_maps_member "hydra.lib.maps.member")

(cl:defvar hydra_lib_names_maps_null "hydra.lib.maps.null")

(cl:defvar hydra_lib_names_maps_singleton "hydra.lib.maps.singleton")

(cl:defvar hydra_lib_names_maps_size "hydra.lib.maps.size")

(cl:defvar hydra_lib_names_maps_to_list "hydra.lib.maps.toList")

(cl:defvar hydra_lib_names_maps_union "hydra.lib.maps.union")

(cl:defvar hydra_lib_names_math "hydra.lib.math")

(cl:defvar hydra_lib_names_math_abs "hydra.lib.math.abs")

(cl:defvar hydra_lib_names_math_acos "hydra.lib.math.acos")

(cl:defvar hydra_lib_names_math_acosh "hydra.lib.math.acosh")

(cl:defvar hydra_lib_names_math_add "hydra.lib.math.add")

(cl:defvar hydra_lib_names_math_asin "hydra.lib.math.asin")

(cl:defvar hydra_lib_names_math_asinh "hydra.lib.math.asinh")

(cl:defvar hydra_lib_names_math_atan "hydra.lib.math.atan")

(cl:defvar hydra_lib_names_math_atan2 "hydra.lib.math.atan2")

(cl:defvar hydra_lib_names_math_atanh "hydra.lib.math.atanh")

(cl:defvar hydra_lib_names_math_ceiling "hydra.lib.math.ceiling")

(cl:defvar hydra_lib_names_math_cos "hydra.lib.math.cos")

(cl:defvar hydra_lib_names_math_cosh "hydra.lib.math.cosh")

(cl:defvar hydra_lib_names_math_div "hydra.lib.math.div")

(cl:defvar hydra_lib_names_math_e "hydra.lib.math.e")

(cl:defvar hydra_lib_names_math_even "hydra.lib.math.even")

(cl:defvar hydra_lib_names_math_exp "hydra.lib.math.exp")

(cl:defvar hydra_lib_names_math_floor "hydra.lib.math.floor")

(cl:defvar hydra_lib_names_math_log "hydra.lib.math.log")

(cl:defvar hydra_lib_names_math_log_base "hydra.lib.math.logBase")

(cl:defvar hydra_lib_names_math_max "hydra.lib.math.max")

(cl:defvar hydra_lib_names_math_min "hydra.lib.math.min")

(cl:defvar hydra_lib_names_math_mod "hydra.lib.math.mod")

(cl:defvar hydra_lib_names_math_mul "hydra.lib.math.mul")

(cl:defvar hydra_lib_names_math_negate "hydra.lib.math.negate")

(cl:defvar hydra_lib_names_math_odd "hydra.lib.math.odd")

(cl:defvar hydra_lib_names_math_pi "hydra.lib.math.pi")

(cl:defvar hydra_lib_names_math_pow "hydra.lib.math.pow")

(cl:defvar hydra_lib_names_math_pred "hydra.lib.math.pred")

(cl:defvar hydra_lib_names_math_range "hydra.lib.math.range")

(cl:defvar hydra_lib_names_math_rem "hydra.lib.math.rem")

(cl:defvar hydra_lib_names_math_round "hydra.lib.math.round")

(cl:defvar hydra_lib_names_math_round_bigfloat "hydra.lib.math.roundBigfloat")

(cl:defvar hydra_lib_names_math_round_float32 "hydra.lib.math.roundFloat32")

(cl:defvar hydra_lib_names_math_round_float64 "hydra.lib.math.roundFloat64")

(cl:defvar hydra_lib_names_math_signum "hydra.lib.math.signum")

(cl:defvar hydra_lib_names_math_sin "hydra.lib.math.sin")

(cl:defvar hydra_lib_names_math_sinh "hydra.lib.math.sinh")

(cl:defvar hydra_lib_names_math_sqrt "hydra.lib.math.sqrt")

(cl:defvar hydra_lib_names_math_sub "hydra.lib.math.sub")

(cl:defvar hydra_lib_names_math_succ "hydra.lib.math.succ")

(cl:defvar hydra_lib_names_math_tan "hydra.lib.math.tan")

(cl:defvar hydra_lib_names_math_tanh "hydra.lib.math.tanh")

(cl:defvar hydra_lib_names_math_truncate "hydra.lib.math.truncate")

(cl:defvar hydra_lib_names_maybes "hydra.lib.maybes")

(cl:defvar hydra_lib_names_maybes_apply "hydra.lib.maybes.apply")

(cl:defvar hydra_lib_names_maybes_bind "hydra.lib.maybes.bind")

(cl:defvar hydra_lib_names_maybes_cases "hydra.lib.maybes.cases")

(cl:defvar hydra_lib_names_maybes_cat "hydra.lib.maybes.cat")

(cl:defvar hydra_lib_names_maybes_compose "hydra.lib.maybes.compose")

(cl:defvar hydra_lib_names_maybes_from_just "hydra.lib.maybes.fromJust")

(cl:defvar hydra_lib_names_maybes_from_maybe "hydra.lib.maybes.fromMaybe")

(cl:defvar hydra_lib_names_maybes_is_just "hydra.lib.maybes.isJust")

(cl:defvar hydra_lib_names_maybes_is_nothing "hydra.lib.maybes.isNothing")

(cl:defvar hydra_lib_names_maybes_map "hydra.lib.maybes.map")

(cl:defvar hydra_lib_names_maybes_map_maybe "hydra.lib.maybes.mapMaybe")

(cl:defvar hydra_lib_names_maybes_maybe "hydra.lib.maybes.maybe")

(cl:defvar hydra_lib_names_maybes_pure "hydra.lib.maybes.pure")

(cl:defvar hydra_lib_names_maybes_to_list "hydra.lib.maybes.toList")

(cl:defvar hydra_lib_names_pairs "hydra.lib.pairs")

(cl:defvar hydra_lib_names_pairs_bimap "hydra.lib.pairs.bimap")

(cl:defvar hydra_lib_names_pairs_first "hydra.lib.pairs.first")

(cl:defvar hydra_lib_names_pairs_second "hydra.lib.pairs.second")

(cl:defvar hydra_lib_names_sets "hydra.lib.sets")

(cl:defvar hydra_lib_names_sets_delete "hydra.lib.sets.delete")

(cl:defvar hydra_lib_names_sets_difference "hydra.lib.sets.difference")

(cl:defvar hydra_lib_names_sets_empty "hydra.lib.sets.empty")

(cl:defvar hydra_lib_names_sets_from_list "hydra.lib.sets.fromList")

(cl:defvar hydra_lib_names_sets_insert "hydra.lib.sets.insert")

(cl:defvar hydra_lib_names_sets_intersection "hydra.lib.sets.intersection")

(cl:defvar hydra_lib_names_sets_map "hydra.lib.sets.map")

(cl:defvar hydra_lib_names_sets_member "hydra.lib.sets.member")

(cl:defvar hydra_lib_names_sets_null "hydra.lib.sets.null")

(cl:defvar hydra_lib_names_sets_singleton "hydra.lib.sets.singleton")

(cl:defvar hydra_lib_names_sets_size "hydra.lib.sets.size")

(cl:defvar hydra_lib_names_sets_to_list "hydra.lib.sets.toList")

(cl:defvar hydra_lib_names_sets_union "hydra.lib.sets.union")

(cl:defvar hydra_lib_names_sets_unions "hydra.lib.sets.unions")

(cl:defvar hydra_lib_names_strings "hydra.lib.strings")

(cl:defvar hydra_lib_names_strings_cat "hydra.lib.strings.cat")

(cl:defvar hydra_lib_names_strings_cat2 "hydra.lib.strings.cat2")

(cl:defvar hydra_lib_names_strings_char_at "hydra.lib.strings.charAt")

(cl:defvar hydra_lib_names_strings_from_list "hydra.lib.strings.fromList")

(cl:defvar hydra_lib_names_strings_intercalate "hydra.lib.strings.intercalate")

(cl:defvar hydra_lib_names_strings_length "hydra.lib.strings.length")

(cl:defvar hydra_lib_names_strings_lines "hydra.lib.strings.lines")

(cl:defvar hydra_lib_names_strings_null "hydra.lib.strings.null")

(cl:defvar hydra_lib_names_strings_split_on "hydra.lib.strings.splitOn")

(cl:defvar hydra_lib_names_strings_to_list "hydra.lib.strings.toList")

(cl:defvar hydra_lib_names_strings_to_lower "hydra.lib.strings.toLower")

(cl:defvar hydra_lib_names_strings_to_upper "hydra.lib.strings.toUpper")

(cl:defvar hydra_lib_names_strings_unlines "hydra.lib.strings.unlines")

(cl:defvar hydra_lib_names_typeclass "hydra.typeclass")

(cl:defvar hydra_lib_names_typeclass_eq "hydra.typeclass.Eq")

(cl:defvar hydra_lib_names_typeclass_ord "hydra.typeclass.Ord")
