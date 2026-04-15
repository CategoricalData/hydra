(require 'cl-lib)

(require 'hydra.core)

(require 'hydra.packaging)

(defvar hydra_lib_names_chars "hydra.lib.chars")

(defvar hydra_lib_names_chars_is_alpha_num "hydra.lib.chars.isAlphaNum")

(defvar hydra_lib_names_chars_is_lower "hydra.lib.chars.isLower")

(defvar hydra_lib_names_chars_is_space "hydra.lib.chars.isSpace")

(defvar hydra_lib_names_chars_is_upper "hydra.lib.chars.isUpper")

(defvar hydra_lib_names_chars_to_lower "hydra.lib.chars.toLower")

(defvar hydra_lib_names_chars_to_upper "hydra.lib.chars.toUpper")

(defvar hydra_lib_names_eithers "hydra.lib.eithers")

(defvar hydra_lib_names_eithers_bimap "hydra.lib.eithers.bimap")

(defvar hydra_lib_names_eithers_bind "hydra.lib.eithers.bind")

(defvar hydra_lib_names_eithers_either "hydra.lib.eithers.either")

(defvar hydra_lib_names_eithers_foldl "hydra.lib.eithers.foldl")

(defvar hydra_lib_names_eithers_from_left "hydra.lib.eithers.fromLeft")

(defvar hydra_lib_names_eithers_from_right "hydra.lib.eithers.fromRight")

(defvar hydra_lib_names_eithers_is_left "hydra.lib.eithers.isLeft")

(defvar hydra_lib_names_eithers_is_right "hydra.lib.eithers.isRight")

(defvar hydra_lib_names_eithers_lefts "hydra.lib.eithers.lefts")

(defvar hydra_lib_names_eithers_map "hydra.lib.eithers.map")

(defvar hydra_lib_names_eithers_map_list "hydra.lib.eithers.mapList")

(defvar hydra_lib_names_eithers_map_maybe "hydra.lib.eithers.mapMaybe")

(defvar hydra_lib_names_eithers_map_set "hydra.lib.eithers.mapSet")

(defvar hydra_lib_names_eithers_partition_eithers "hydra.lib.eithers.partitionEithers")

(defvar hydra_lib_names_eithers_rights "hydra.lib.eithers.rights")

(defvar hydra_lib_names_equality "hydra.lib.equality")

(defvar hydra_lib_names_equality_compare "hydra.lib.equality.compare")

(defvar hydra_lib_names_equality_equal "hydra.lib.equality.equal")

(defvar hydra_lib_names_equality_gt "hydra.lib.equality.gt")

(defvar hydra_lib_names_equality_gte "hydra.lib.equality.gte")

(defvar hydra_lib_names_equality_identity "hydra.lib.equality.identity")

(defvar hydra_lib_names_equality_lt "hydra.lib.equality.lt")

(defvar hydra_lib_names_equality_lte "hydra.lib.equality.lte")

(defvar hydra_lib_names_equality_max "hydra.lib.equality.max")

(defvar hydra_lib_names_equality_min "hydra.lib.equality.min")

(defvar hydra_lib_names_lists "hydra.lib.lists")

(defvar hydra_lib_names_lists_apply "hydra.lib.lists.apply")

(defvar hydra_lib_names_lists_at "hydra.lib.lists.at")

(defvar hydra_lib_names_lists_bind "hydra.lib.lists.bind")

(defvar hydra_lib_names_lists_concat "hydra.lib.lists.concat")

(defvar hydra_lib_names_lists_concat2 "hydra.lib.lists.concat2")

(defvar hydra_lib_names_lists_cons "hydra.lib.lists.cons")

(defvar hydra_lib_names_lists_drop "hydra.lib.lists.drop")

(defvar hydra_lib_names_lists_drop_while "hydra.lib.lists.dropWhile")

(defvar hydra_lib_names_lists_elem "hydra.lib.lists.elem")

(defvar hydra_lib_names_lists_filter "hydra.lib.lists.filter")

(defvar hydra_lib_names_lists_find "hydra.lib.lists.find")

(defvar hydra_lib_names_lists_foldl "hydra.lib.lists.foldl")

(defvar hydra_lib_names_lists_foldr "hydra.lib.lists.foldr")

(defvar hydra_lib_names_lists_group "hydra.lib.lists.group")

(defvar hydra_lib_names_lists_head "hydra.lib.lists.head")

(defvar hydra_lib_names_lists_init "hydra.lib.lists.init")

(defvar hydra_lib_names_lists_intercalate "hydra.lib.lists.intercalate")

(defvar hydra_lib_names_lists_intersperse "hydra.lib.lists.intersperse")

(defvar hydra_lib_names_lists_last "hydra.lib.lists.last")

(defvar hydra_lib_names_lists_length "hydra.lib.lists.length")

(defvar hydra_lib_names_lists_map "hydra.lib.lists.map")

(defvar hydra_lib_names_lists_maybe_at "hydra.lib.lists.maybeAt")

(defvar hydra_lib_names_lists_maybe_head "hydra.lib.lists.maybeHead")

(defvar hydra_lib_names_lists_maybe_init "hydra.lib.lists.maybeInit")

(defvar hydra_lib_names_lists_maybe_last "hydra.lib.lists.maybeLast")

(defvar hydra_lib_names_lists_maybe_tail "hydra.lib.lists.maybeTail")

(defvar hydra_lib_names_lists_nub "hydra.lib.lists.nub")

(defvar hydra_lib_names_lists_null "hydra.lib.lists.null")

(defvar hydra_lib_names_lists_partition "hydra.lib.lists.partition")

(defvar hydra_lib_names_lists_pure "hydra.lib.lists.pure")

(defvar hydra_lib_names_lists_replicate "hydra.lib.lists.replicate")

(defvar hydra_lib_names_lists_reverse "hydra.lib.lists.reverse")

(defvar hydra_lib_names_lists_safe_head "hydra.lib.lists.safeHead")

(defvar hydra_lib_names_lists_singleton "hydra.lib.lists.singleton")

(defvar hydra_lib_names_lists_sort "hydra.lib.lists.sort")

(defvar hydra_lib_names_lists_sort_on "hydra.lib.lists.sortOn")

(defvar hydra_lib_names_lists_span "hydra.lib.lists.span")

(defvar hydra_lib_names_lists_tail "hydra.lib.lists.tail")

(defvar hydra_lib_names_lists_take "hydra.lib.lists.take")

(defvar hydra_lib_names_lists_transpose "hydra.lib.lists.transpose")

(defvar hydra_lib_names_lists_uncons "hydra.lib.lists.uncons")

(defvar hydra_lib_names_lists_zip "hydra.lib.lists.zip")

(defvar hydra_lib_names_lists_zip_with "hydra.lib.lists.zipWith")

(defvar hydra_lib_names_literals "hydra.lib.literals")

(defvar hydra_lib_names_literals_bigfloat_to_bigint "hydra.lib.literals.bigfloatToBigint")

(defvar hydra_lib_names_literals_bigfloat_to_float32 "hydra.lib.literals.bigfloatToFloat32")

(defvar hydra_lib_names_literals_bigfloat_to_float64 "hydra.lib.literals.bigfloatToFloat64")

(defvar hydra_lib_names_literals_bigint_to_bigfloat "hydra.lib.literals.bigintToBigfloat")

(defvar hydra_lib_names_literals_bigint_to_int16 "hydra.lib.literals.bigintToInt16")

(defvar hydra_lib_names_literals_bigint_to_int32 "hydra.lib.literals.bigintToInt32")

(defvar hydra_lib_names_literals_bigint_to_int64 "hydra.lib.literals.bigintToInt64")

(defvar hydra_lib_names_literals_bigint_to_int8 "hydra.lib.literals.bigintToInt8")

(defvar hydra_lib_names_literals_bigint_to_uint16 "hydra.lib.literals.bigintToUint16")

(defvar hydra_lib_names_literals_bigint_to_uint32 "hydra.lib.literals.bigintToUint32")

(defvar hydra_lib_names_literals_bigint_to_uint64 "hydra.lib.literals.bigintToUint64")

(defvar hydra_lib_names_literals_bigint_to_uint8 "hydra.lib.literals.bigintToUint8")

(defvar hydra_lib_names_literals_binary_to_bytes "hydra.lib.literals.binaryToBytes")

(defvar hydra_lib_names_literals_binary_to_string "hydra.lib.literals.binaryToString")

(defvar hydra_lib_names_literals_float32_to_bigfloat "hydra.lib.literals.float32ToBigfloat")

(defvar hydra_lib_names_literals_float64_to_bigfloat "hydra.lib.literals.float64ToBigfloat")

(defvar hydra_lib_names_literals_int16_to_bigint "hydra.lib.literals.int16ToBigint")

(defvar hydra_lib_names_literals_int32_to_bigint "hydra.lib.literals.int32ToBigint")

(defvar hydra_lib_names_literals_int64_to_bigint "hydra.lib.literals.int64ToBigint")

(defvar hydra_lib_names_literals_int8_to_bigint "hydra.lib.literals.int8ToBigint")

(defvar hydra_lib_names_literals_read_bigfloat "hydra.lib.literals.readBigfloat")

(defvar hydra_lib_names_literals_read_bigint "hydra.lib.literals.readBigint")

(defvar hydra_lib_names_literals_read_boolean "hydra.lib.literals.readBoolean")

(defvar hydra_lib_names_literals_read_float32 "hydra.lib.literals.readFloat32")

(defvar hydra_lib_names_literals_read_float64 "hydra.lib.literals.readFloat64")

(defvar hydra_lib_names_literals_read_int16 "hydra.lib.literals.readInt16")

(defvar hydra_lib_names_literals_read_int32 "hydra.lib.literals.readInt32")

(defvar hydra_lib_names_literals_read_int64 "hydra.lib.literals.readInt64")

(defvar hydra_lib_names_literals_read_int8 "hydra.lib.literals.readInt8")

(defvar hydra_lib_names_literals_read_string "hydra.lib.literals.readString")

(defvar hydra_lib_names_literals_read_uint16 "hydra.lib.literals.readUint16")

(defvar hydra_lib_names_literals_read_uint32 "hydra.lib.literals.readUint32")

(defvar hydra_lib_names_literals_read_uint64 "hydra.lib.literals.readUint64")

(defvar hydra_lib_names_literals_read_uint8 "hydra.lib.literals.readUint8")

(defvar hydra_lib_names_literals_show_bigfloat "hydra.lib.literals.showBigfloat")

(defvar hydra_lib_names_literals_show_bigint "hydra.lib.literals.showBigint")

(defvar hydra_lib_names_literals_show_boolean "hydra.lib.literals.showBoolean")

(defvar hydra_lib_names_literals_show_float32 "hydra.lib.literals.showFloat32")

(defvar hydra_lib_names_literals_show_float64 "hydra.lib.literals.showFloat64")

(defvar hydra_lib_names_literals_show_int16 "hydra.lib.literals.showInt16")

(defvar hydra_lib_names_literals_show_int32 "hydra.lib.literals.showInt32")

(defvar hydra_lib_names_literals_show_int64 "hydra.lib.literals.showInt64")

(defvar hydra_lib_names_literals_show_int8 "hydra.lib.literals.showInt8")

(defvar hydra_lib_names_literals_show_string "hydra.lib.literals.showString")

(defvar hydra_lib_names_literals_show_uint16 "hydra.lib.literals.showUint16")

(defvar hydra_lib_names_literals_show_uint32 "hydra.lib.literals.showUint32")

(defvar hydra_lib_names_literals_show_uint64 "hydra.lib.literals.showUint64")

(defvar hydra_lib_names_literals_show_uint8 "hydra.lib.literals.showUint8")

(defvar hydra_lib_names_literals_string_to_binary "hydra.lib.literals.stringToBinary")

(defvar hydra_lib_names_literals_uint16_to_bigint "hydra.lib.literals.uint16ToBigint")

(defvar hydra_lib_names_literals_uint32_to_bigint "hydra.lib.literals.uint32ToBigint")

(defvar hydra_lib_names_literals_uint64_to_bigint "hydra.lib.literals.uint64ToBigint")

(defvar hydra_lib_names_literals_uint8_to_bigint "hydra.lib.literals.uint8ToBigint")

(defvar hydra_lib_names_logic "hydra.lib.logic")

(defvar hydra_lib_names_logic_and "hydra.lib.logic.and")

(defvar hydra_lib_names_logic_if_else "hydra.lib.logic.ifElse")

(defvar hydra_lib_names_logic_not "hydra.lib.logic.not")

(defvar hydra_lib_names_logic_or "hydra.lib.logic.or")

(defvar hydra_lib_names_maps "hydra.lib.maps")

(defvar hydra_lib_names_maps_alter "hydra.lib.maps.alter")

(defvar hydra_lib_names_maps_bimap "hydra.lib.maps.bimap")

(defvar hydra_lib_names_maps_delete "hydra.lib.maps.delete")

(defvar hydra_lib_names_maps_elems "hydra.lib.maps.elems")

(defvar hydra_lib_names_maps_empty "hydra.lib.maps.empty")

(defvar hydra_lib_names_maps_filter "hydra.lib.maps.filter")

(defvar hydra_lib_names_maps_filter_with_key "hydra.lib.maps.filterWithKey")

(defvar hydra_lib_names_maps_find_with_default "hydra.lib.maps.findWithDefault")

(defvar hydra_lib_names_maps_from_list "hydra.lib.maps.fromList")

(defvar hydra_lib_names_maps_insert "hydra.lib.maps.insert")

(defvar hydra_lib_names_maps_keys "hydra.lib.maps.keys")

(defvar hydra_lib_names_maps_lookup "hydra.lib.maps.lookup")

(defvar hydra_lib_names_maps_map "hydra.lib.maps.map")

(defvar hydra_lib_names_maps_map_keys "hydra.lib.maps.mapKeys")

(defvar hydra_lib_names_maps_member "hydra.lib.maps.member")

(defvar hydra_lib_names_maps_null "hydra.lib.maps.null")

(defvar hydra_lib_names_maps_singleton "hydra.lib.maps.singleton")

(defvar hydra_lib_names_maps_size "hydra.lib.maps.size")

(defvar hydra_lib_names_maps_to_list "hydra.lib.maps.toList")

(defvar hydra_lib_names_maps_union "hydra.lib.maps.union")

(defvar hydra_lib_names_math "hydra.lib.math")

(defvar hydra_lib_names_math_abs "hydra.lib.math.abs")

(defvar hydra_lib_names_math_acos "hydra.lib.math.acos")

(defvar hydra_lib_names_math_acosh "hydra.lib.math.acosh")

(defvar hydra_lib_names_math_add "hydra.lib.math.add")

(defvar hydra_lib_names_math_add_float64 "hydra.lib.math.addFloat64")

(defvar hydra_lib_names_math_asin "hydra.lib.math.asin")

(defvar hydra_lib_names_math_asinh "hydra.lib.math.asinh")

(defvar hydra_lib_names_math_atan "hydra.lib.math.atan")

(defvar hydra_lib_names_math_atan2 "hydra.lib.math.atan2")

(defvar hydra_lib_names_math_atanh "hydra.lib.math.atanh")

(defvar hydra_lib_names_math_ceiling "hydra.lib.math.ceiling")

(defvar hydra_lib_names_math_cos "hydra.lib.math.cos")

(defvar hydra_lib_names_math_cosh "hydra.lib.math.cosh")

(defvar hydra_lib_names_math_div "hydra.lib.math.div")

(defvar hydra_lib_names_math_e "hydra.lib.math.e")

(defvar hydra_lib_names_math_even "hydra.lib.math.even")

(defvar hydra_lib_names_math_exp "hydra.lib.math.exp")

(defvar hydra_lib_names_math_floor "hydra.lib.math.floor")

(defvar hydra_lib_names_math_log "hydra.lib.math.log")

(defvar hydra_lib_names_math_log_base "hydra.lib.math.logBase")

(defvar hydra_lib_names_math_max "hydra.lib.math.max")

(defvar hydra_lib_names_math_maybe_div "hydra.lib.math.maybeDiv")

(defvar hydra_lib_names_math_maybe_mod "hydra.lib.math.maybeMod")

(defvar hydra_lib_names_math_maybe_pred "hydra.lib.math.maybePred")

(defvar hydra_lib_names_math_maybe_rem "hydra.lib.math.maybeRem")

(defvar hydra_lib_names_math_maybe_succ "hydra.lib.math.maybeSucc")

(defvar hydra_lib_names_math_min "hydra.lib.math.min")

(defvar hydra_lib_names_math_mod "hydra.lib.math.mod")

(defvar hydra_lib_names_math_mul "hydra.lib.math.mul")

(defvar hydra_lib_names_math_mul_float64 "hydra.lib.math.mulFloat64")

(defvar hydra_lib_names_math_negate "hydra.lib.math.negate")

(defvar hydra_lib_names_math_negate_float64 "hydra.lib.math.negateFloat64")

(defvar hydra_lib_names_math_odd "hydra.lib.math.odd")

(defvar hydra_lib_names_math_pi "hydra.lib.math.pi")

(defvar hydra_lib_names_math_pow "hydra.lib.math.pow")

(defvar hydra_lib_names_math_pred "hydra.lib.math.pred")

(defvar hydra_lib_names_math_range "hydra.lib.math.range")

(defvar hydra_lib_names_math_rem "hydra.lib.math.rem")

(defvar hydra_lib_names_math_round "hydra.lib.math.round")

(defvar hydra_lib_names_math_round_bigfloat "hydra.lib.math.roundBigfloat")

(defvar hydra_lib_names_math_round_float32 "hydra.lib.math.roundFloat32")

(defvar hydra_lib_names_math_round_float64 "hydra.lib.math.roundFloat64")

(defvar hydra_lib_names_math_signum "hydra.lib.math.signum")

(defvar hydra_lib_names_math_sin "hydra.lib.math.sin")

(defvar hydra_lib_names_math_sinh "hydra.lib.math.sinh")

(defvar hydra_lib_names_math_sqrt "hydra.lib.math.sqrt")

(defvar hydra_lib_names_math_sub "hydra.lib.math.sub")

(defvar hydra_lib_names_math_sub_float64 "hydra.lib.math.subFloat64")

(defvar hydra_lib_names_math_succ "hydra.lib.math.succ")

(defvar hydra_lib_names_math_tan "hydra.lib.math.tan")

(defvar hydra_lib_names_math_tanh "hydra.lib.math.tanh")

(defvar hydra_lib_names_math_truncate "hydra.lib.math.truncate")

(defvar hydra_lib_names_maybes "hydra.lib.maybes")

(defvar hydra_lib_names_maybes_apply "hydra.lib.maybes.apply")

(defvar hydra_lib_names_maybes_bind "hydra.lib.maybes.bind")

(defvar hydra_lib_names_maybes_cases "hydra.lib.maybes.cases")

(defvar hydra_lib_names_maybes_cat "hydra.lib.maybes.cat")

(defvar hydra_lib_names_maybes_compose "hydra.lib.maybes.compose")

(defvar hydra_lib_names_maybes_from_just "hydra.lib.maybes.fromJust")

(defvar hydra_lib_names_maybes_from_maybe "hydra.lib.maybes.fromMaybe")

(defvar hydra_lib_names_maybes_is_just "hydra.lib.maybes.isJust")

(defvar hydra_lib_names_maybes_is_nothing "hydra.lib.maybes.isNothing")

(defvar hydra_lib_names_maybes_map "hydra.lib.maybes.map")

(defvar hydra_lib_names_maybes_map_maybe "hydra.lib.maybes.mapMaybe")

(defvar hydra_lib_names_maybes_maybe "hydra.lib.maybes.maybe")

(defvar hydra_lib_names_maybes_pure "hydra.lib.maybes.pure")

(defvar hydra_lib_names_maybes_to_list "hydra.lib.maybes.toList")

(defvar hydra_lib_names_pairs "hydra.lib.pairs")

(defvar hydra_lib_names_pairs_bimap "hydra.lib.pairs.bimap")

(defvar hydra_lib_names_pairs_first "hydra.lib.pairs.first")

(defvar hydra_lib_names_pairs_second "hydra.lib.pairs.second")

(defvar hydra_lib_names_regex "hydra.lib.regex")

(defvar hydra_lib_names_regex_find "hydra.lib.regex.find")

(defvar hydra_lib_names_regex_find_all "hydra.lib.regex.findAll")

(defvar hydra_lib_names_regex_matches "hydra.lib.regex.matches")

(defvar hydra_lib_names_regex_replace "hydra.lib.regex.replace")

(defvar hydra_lib_names_regex_replace_all "hydra.lib.regex.replaceAll")

(defvar hydra_lib_names_regex_split "hydra.lib.regex.split")

(defvar hydra_lib_names_sets "hydra.lib.sets")

(defvar hydra_lib_names_sets_delete "hydra.lib.sets.delete")

(defvar hydra_lib_names_sets_difference "hydra.lib.sets.difference")

(defvar hydra_lib_names_sets_empty "hydra.lib.sets.empty")

(defvar hydra_lib_names_sets_from_list "hydra.lib.sets.fromList")

(defvar hydra_lib_names_sets_insert "hydra.lib.sets.insert")

(defvar hydra_lib_names_sets_intersection "hydra.lib.sets.intersection")

(defvar hydra_lib_names_sets_map "hydra.lib.sets.map")

(defvar hydra_lib_names_sets_member "hydra.lib.sets.member")

(defvar hydra_lib_names_sets_null "hydra.lib.sets.null")

(defvar hydra_lib_names_sets_singleton "hydra.lib.sets.singleton")

(defvar hydra_lib_names_sets_size "hydra.lib.sets.size")

(defvar hydra_lib_names_sets_to_list "hydra.lib.sets.toList")

(defvar hydra_lib_names_sets_union "hydra.lib.sets.union")

(defvar hydra_lib_names_sets_unions "hydra.lib.sets.unions")

(defvar hydra_lib_names_strings "hydra.lib.strings")

(defvar hydra_lib_names_strings_cat "hydra.lib.strings.cat")

(defvar hydra_lib_names_strings_cat2 "hydra.lib.strings.cat2")

(defvar hydra_lib_names_strings_char_at "hydra.lib.strings.charAt")

(defvar hydra_lib_names_strings_from_list "hydra.lib.strings.fromList")

(defvar hydra_lib_names_strings_intercalate "hydra.lib.strings.intercalate")

(defvar hydra_lib_names_strings_length "hydra.lib.strings.length")

(defvar hydra_lib_names_strings_lines "hydra.lib.strings.lines")

(defvar hydra_lib_names_strings_maybe_char_at "hydra.lib.strings.maybeCharAt")

(defvar hydra_lib_names_strings_null "hydra.lib.strings.null")

(defvar hydra_lib_names_strings_split_on "hydra.lib.strings.splitOn")

(defvar hydra_lib_names_strings_to_list "hydra.lib.strings.toList")

(defvar hydra_lib_names_strings_to_lower "hydra.lib.strings.toLower")

(defvar hydra_lib_names_strings_to_upper "hydra.lib.strings.toUpper")

(defvar hydra_lib_names_strings_unlines "hydra.lib.strings.unlines")

(defvar hydra_lib_names_typeclass "hydra.typeclass")

(defvar hydra_lib_names_typeclass_eq "hydra.typeclass.Eq")

(defvar hydra_lib_names_typeclass_ord "hydra.typeclass.Ord")

(provide 'hydra.lib.names)
