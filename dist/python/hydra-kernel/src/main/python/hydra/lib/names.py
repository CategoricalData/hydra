# Note: this is an automatically generated file. Do not edit.

r"""Namespaces and primitive names for the Hydra standard library."""

from __future__ import annotations
from functools import lru_cache
from typing import cast
import hydra.core
import hydra.packaging

chars = hydra.packaging.Namespace("hydra.lib.chars")

chars_is_alpha_num = hydra.core.Name("hydra.lib.chars.isAlphaNum")

chars_is_lower = hydra.core.Name("hydra.lib.chars.isLower")

chars_is_space = hydra.core.Name("hydra.lib.chars.isSpace")

chars_is_upper = hydra.core.Name("hydra.lib.chars.isUpper")

chars_to_lower = hydra.core.Name("hydra.lib.chars.toLower")

chars_to_upper = hydra.core.Name("hydra.lib.chars.toUpper")

eithers = hydra.packaging.Namespace("hydra.lib.eithers")

eithers_bimap = hydra.core.Name("hydra.lib.eithers.bimap")

eithers_bind = hydra.core.Name("hydra.lib.eithers.bind")

eithers_either = hydra.core.Name("hydra.lib.eithers.either")

eithers_foldl = hydra.core.Name("hydra.lib.eithers.foldl")

eithers_from_left = hydra.core.Name("hydra.lib.eithers.fromLeft")

eithers_from_right = hydra.core.Name("hydra.lib.eithers.fromRight")

eithers_is_left = hydra.core.Name("hydra.lib.eithers.isLeft")

eithers_is_right = hydra.core.Name("hydra.lib.eithers.isRight")

eithers_lefts = hydra.core.Name("hydra.lib.eithers.lefts")

eithers_map = hydra.core.Name("hydra.lib.eithers.map")

eithers_map_list = hydra.core.Name("hydra.lib.eithers.mapList")

eithers_map_maybe = hydra.core.Name("hydra.lib.eithers.mapMaybe")

eithers_map_set = hydra.core.Name("hydra.lib.eithers.mapSet")

eithers_partition_eithers = hydra.core.Name("hydra.lib.eithers.partitionEithers")

eithers_rights = hydra.core.Name("hydra.lib.eithers.rights")

equality = hydra.packaging.Namespace("hydra.lib.equality")

equality_compare = hydra.core.Name("hydra.lib.equality.compare")

equality_equal = hydra.core.Name("hydra.lib.equality.equal")

equality_gt = hydra.core.Name("hydra.lib.equality.gt")

equality_gte = hydra.core.Name("hydra.lib.equality.gte")

equality_identity = hydra.core.Name("hydra.lib.equality.identity")

equality_lt = hydra.core.Name("hydra.lib.equality.lt")

equality_lte = hydra.core.Name("hydra.lib.equality.lte")

equality_max = hydra.core.Name("hydra.lib.equality.max")

equality_min = hydra.core.Name("hydra.lib.equality.min")

lists = hydra.packaging.Namespace("hydra.lib.lists")

lists_apply = hydra.core.Name("hydra.lib.lists.apply")

lists_at = hydra.core.Name("hydra.lib.lists.at")

lists_bind = hydra.core.Name("hydra.lib.lists.bind")

lists_concat = hydra.core.Name("hydra.lib.lists.concat")

lists_concat2 = hydra.core.Name("hydra.lib.lists.concat2")

lists_cons = hydra.core.Name("hydra.lib.lists.cons")

lists_drop = hydra.core.Name("hydra.lib.lists.drop")

lists_drop_while = hydra.core.Name("hydra.lib.lists.dropWhile")

lists_elem = hydra.core.Name("hydra.lib.lists.elem")

lists_filter = hydra.core.Name("hydra.lib.lists.filter")

lists_find = hydra.core.Name("hydra.lib.lists.find")

lists_foldl = hydra.core.Name("hydra.lib.lists.foldl")

lists_foldr = hydra.core.Name("hydra.lib.lists.foldr")

lists_group = hydra.core.Name("hydra.lib.lists.group")

lists_head = hydra.core.Name("hydra.lib.lists.head")

lists_init = hydra.core.Name("hydra.lib.lists.init")

lists_intercalate = hydra.core.Name("hydra.lib.lists.intercalate")

lists_intersperse = hydra.core.Name("hydra.lib.lists.intersperse")

lists_last = hydra.core.Name("hydra.lib.lists.last")

lists_length = hydra.core.Name("hydra.lib.lists.length")

lists_map = hydra.core.Name("hydra.lib.lists.map")

lists_maybe_at = hydra.core.Name("hydra.lib.lists.maybeAt")

lists_maybe_head = hydra.core.Name("hydra.lib.lists.maybeHead")

lists_maybe_init = hydra.core.Name("hydra.lib.lists.maybeInit")

lists_maybe_last = hydra.core.Name("hydra.lib.lists.maybeLast")

lists_maybe_tail = hydra.core.Name("hydra.lib.lists.maybeTail")

lists_nub = hydra.core.Name("hydra.lib.lists.nub")

lists_null = hydra.core.Name("hydra.lib.lists.null")

lists_partition = hydra.core.Name("hydra.lib.lists.partition")

lists_pure = hydra.core.Name("hydra.lib.lists.pure")

lists_replicate = hydra.core.Name("hydra.lib.lists.replicate")

lists_reverse = hydra.core.Name("hydra.lib.lists.reverse")

lists_safe_head = hydra.core.Name("hydra.lib.lists.safeHead")

lists_singleton = hydra.core.Name("hydra.lib.lists.singleton")

lists_sort = hydra.core.Name("hydra.lib.lists.sort")

lists_sort_on = hydra.core.Name("hydra.lib.lists.sortOn")

lists_span = hydra.core.Name("hydra.lib.lists.span")

lists_tail = hydra.core.Name("hydra.lib.lists.tail")

lists_take = hydra.core.Name("hydra.lib.lists.take")

lists_transpose = hydra.core.Name("hydra.lib.lists.transpose")

lists_uncons = hydra.core.Name("hydra.lib.lists.uncons")

lists_zip = hydra.core.Name("hydra.lib.lists.zip")

lists_zip_with = hydra.core.Name("hydra.lib.lists.zipWith")

literals = hydra.packaging.Namespace("hydra.lib.literals")

literals_bigfloat_to_bigint = hydra.core.Name("hydra.lib.literals.bigfloatToBigint")

literals_bigfloat_to_float32 = hydra.core.Name("hydra.lib.literals.bigfloatToFloat32")

literals_bigfloat_to_float64 = hydra.core.Name("hydra.lib.literals.bigfloatToFloat64")

literals_bigint_to_bigfloat = hydra.core.Name("hydra.lib.literals.bigintToBigfloat")

literals_bigint_to_int16 = hydra.core.Name("hydra.lib.literals.bigintToInt16")

literals_bigint_to_int32 = hydra.core.Name("hydra.lib.literals.bigintToInt32")

literals_bigint_to_int64 = hydra.core.Name("hydra.lib.literals.bigintToInt64")

literals_bigint_to_int8 = hydra.core.Name("hydra.lib.literals.bigintToInt8")

literals_bigint_to_uint16 = hydra.core.Name("hydra.lib.literals.bigintToUint16")

literals_bigint_to_uint32 = hydra.core.Name("hydra.lib.literals.bigintToUint32")

literals_bigint_to_uint64 = hydra.core.Name("hydra.lib.literals.bigintToUint64")

literals_bigint_to_uint8 = hydra.core.Name("hydra.lib.literals.bigintToUint8")

literals_binary_to_bytes = hydra.core.Name("hydra.lib.literals.binaryToBytes")

literals_binary_to_string = hydra.core.Name("hydra.lib.literals.binaryToString")

literals_float32_to_bigfloat = hydra.core.Name("hydra.lib.literals.float32ToBigfloat")

literals_float64_to_bigfloat = hydra.core.Name("hydra.lib.literals.float64ToBigfloat")

literals_int16_to_bigint = hydra.core.Name("hydra.lib.literals.int16ToBigint")

literals_int32_to_bigint = hydra.core.Name("hydra.lib.literals.int32ToBigint")

literals_int64_to_bigint = hydra.core.Name("hydra.lib.literals.int64ToBigint")

literals_int8_to_bigint = hydra.core.Name("hydra.lib.literals.int8ToBigint")

literals_read_bigfloat = hydra.core.Name("hydra.lib.literals.readBigfloat")

literals_read_bigint = hydra.core.Name("hydra.lib.literals.readBigint")

literals_read_boolean = hydra.core.Name("hydra.lib.literals.readBoolean")

literals_read_float32 = hydra.core.Name("hydra.lib.literals.readFloat32")

literals_read_float64 = hydra.core.Name("hydra.lib.literals.readFloat64")

literals_read_int16 = hydra.core.Name("hydra.lib.literals.readInt16")

literals_read_int32 = hydra.core.Name("hydra.lib.literals.readInt32")

literals_read_int64 = hydra.core.Name("hydra.lib.literals.readInt64")

literals_read_int8 = hydra.core.Name("hydra.lib.literals.readInt8")

literals_read_string = hydra.core.Name("hydra.lib.literals.readString")

literals_read_uint16 = hydra.core.Name("hydra.lib.literals.readUint16")

literals_read_uint32 = hydra.core.Name("hydra.lib.literals.readUint32")

literals_read_uint64 = hydra.core.Name("hydra.lib.literals.readUint64")

literals_read_uint8 = hydra.core.Name("hydra.lib.literals.readUint8")

literals_show_bigfloat = hydra.core.Name("hydra.lib.literals.showBigfloat")

literals_show_bigint = hydra.core.Name("hydra.lib.literals.showBigint")

literals_show_boolean = hydra.core.Name("hydra.lib.literals.showBoolean")

literals_show_float32 = hydra.core.Name("hydra.lib.literals.showFloat32")

literals_show_float64 = hydra.core.Name("hydra.lib.literals.showFloat64")

literals_show_int16 = hydra.core.Name("hydra.lib.literals.showInt16")

literals_show_int32 = hydra.core.Name("hydra.lib.literals.showInt32")

literals_show_int64 = hydra.core.Name("hydra.lib.literals.showInt64")

literals_show_int8 = hydra.core.Name("hydra.lib.literals.showInt8")

literals_show_string = hydra.core.Name("hydra.lib.literals.showString")

literals_show_uint16 = hydra.core.Name("hydra.lib.literals.showUint16")

literals_show_uint32 = hydra.core.Name("hydra.lib.literals.showUint32")

literals_show_uint64 = hydra.core.Name("hydra.lib.literals.showUint64")

literals_show_uint8 = hydra.core.Name("hydra.lib.literals.showUint8")

literals_string_to_binary = hydra.core.Name("hydra.lib.literals.stringToBinary")

literals_uint16_to_bigint = hydra.core.Name("hydra.lib.literals.uint16ToBigint")

literals_uint32_to_bigint = hydra.core.Name("hydra.lib.literals.uint32ToBigint")

literals_uint64_to_bigint = hydra.core.Name("hydra.lib.literals.uint64ToBigint")

literals_uint8_to_bigint = hydra.core.Name("hydra.lib.literals.uint8ToBigint")

logic = hydra.packaging.Namespace("hydra.lib.logic")

logic_and = hydra.core.Name("hydra.lib.logic.and")

logic_if_else = hydra.core.Name("hydra.lib.logic.ifElse")

logic_not = hydra.core.Name("hydra.lib.logic.not")

logic_or = hydra.core.Name("hydra.lib.logic.or")

maps = hydra.packaging.Namespace("hydra.lib.maps")

maps_alter = hydra.core.Name("hydra.lib.maps.alter")

maps_bimap = hydra.core.Name("hydra.lib.maps.bimap")

maps_delete = hydra.core.Name("hydra.lib.maps.delete")

maps_elems = hydra.core.Name("hydra.lib.maps.elems")

maps_empty = hydra.core.Name("hydra.lib.maps.empty")

maps_filter = hydra.core.Name("hydra.lib.maps.filter")

maps_filter_with_key = hydra.core.Name("hydra.lib.maps.filterWithKey")

maps_find_with_default = hydra.core.Name("hydra.lib.maps.findWithDefault")

maps_from_list = hydra.core.Name("hydra.lib.maps.fromList")

maps_insert = hydra.core.Name("hydra.lib.maps.insert")

maps_keys = hydra.core.Name("hydra.lib.maps.keys")

maps_lookup = hydra.core.Name("hydra.lib.maps.lookup")

maps_map = hydra.core.Name("hydra.lib.maps.map")

maps_map_keys = hydra.core.Name("hydra.lib.maps.mapKeys")

maps_member = hydra.core.Name("hydra.lib.maps.member")

maps_null = hydra.core.Name("hydra.lib.maps.null")

maps_singleton = hydra.core.Name("hydra.lib.maps.singleton")

maps_size = hydra.core.Name("hydra.lib.maps.size")

maps_to_list = hydra.core.Name("hydra.lib.maps.toList")

maps_union = hydra.core.Name("hydra.lib.maps.union")

math = hydra.packaging.Namespace("hydra.lib.math")

math_abs = hydra.core.Name("hydra.lib.math.abs")

math_acos = hydra.core.Name("hydra.lib.math.acos")

math_acosh = hydra.core.Name("hydra.lib.math.acosh")

math_add = hydra.core.Name("hydra.lib.math.add")

math_add_float64 = hydra.core.Name("hydra.lib.math.addFloat64")

math_asin = hydra.core.Name("hydra.lib.math.asin")

math_asinh = hydra.core.Name("hydra.lib.math.asinh")

math_atan = hydra.core.Name("hydra.lib.math.atan")

math_atan2 = hydra.core.Name("hydra.lib.math.atan2")

math_atanh = hydra.core.Name("hydra.lib.math.atanh")

math_ceiling = hydra.core.Name("hydra.lib.math.ceiling")

math_cos = hydra.core.Name("hydra.lib.math.cos")

math_cosh = hydra.core.Name("hydra.lib.math.cosh")

math_div = hydra.core.Name("hydra.lib.math.div")

math_e = hydra.core.Name("hydra.lib.math.e")

math_even = hydra.core.Name("hydra.lib.math.even")

math_exp = hydra.core.Name("hydra.lib.math.exp")

math_floor = hydra.core.Name("hydra.lib.math.floor")

math_log = hydra.core.Name("hydra.lib.math.log")

math_log_base = hydra.core.Name("hydra.lib.math.logBase")

math_max = hydra.core.Name("hydra.lib.math.max")

math_maybe_div = hydra.core.Name("hydra.lib.math.maybeDiv")

math_maybe_mod = hydra.core.Name("hydra.lib.math.maybeMod")

math_maybe_pred = hydra.core.Name("hydra.lib.math.maybePred")

math_maybe_rem = hydra.core.Name("hydra.lib.math.maybeRem")

math_maybe_succ = hydra.core.Name("hydra.lib.math.maybeSucc")

math_min = hydra.core.Name("hydra.lib.math.min")

math_mod = hydra.core.Name("hydra.lib.math.mod")

math_mul = hydra.core.Name("hydra.lib.math.mul")

math_mul_float64 = hydra.core.Name("hydra.lib.math.mulFloat64")

math_negate = hydra.core.Name("hydra.lib.math.negate")

math_negate_float64 = hydra.core.Name("hydra.lib.math.negateFloat64")

math_odd = hydra.core.Name("hydra.lib.math.odd")

math_pi = hydra.core.Name("hydra.lib.math.pi")

math_pow = hydra.core.Name("hydra.lib.math.pow")

math_pred = hydra.core.Name("hydra.lib.math.pred")

math_range = hydra.core.Name("hydra.lib.math.range")

math_rem = hydra.core.Name("hydra.lib.math.rem")

math_round = hydra.core.Name("hydra.lib.math.round")

math_round_bigfloat = hydra.core.Name("hydra.lib.math.roundBigfloat")

math_round_float32 = hydra.core.Name("hydra.lib.math.roundFloat32")

math_round_float64 = hydra.core.Name("hydra.lib.math.roundFloat64")

math_signum = hydra.core.Name("hydra.lib.math.signum")

math_sin = hydra.core.Name("hydra.lib.math.sin")

math_sinh = hydra.core.Name("hydra.lib.math.sinh")

math_sqrt = hydra.core.Name("hydra.lib.math.sqrt")

math_sub = hydra.core.Name("hydra.lib.math.sub")

math_sub_float64 = hydra.core.Name("hydra.lib.math.subFloat64")

math_succ = hydra.core.Name("hydra.lib.math.succ")

math_tan = hydra.core.Name("hydra.lib.math.tan")

math_tanh = hydra.core.Name("hydra.lib.math.tanh")

math_truncate = hydra.core.Name("hydra.lib.math.truncate")

maybes = hydra.packaging.Namespace("hydra.lib.maybes")

maybes_apply = hydra.core.Name("hydra.lib.maybes.apply")

maybes_bind = hydra.core.Name("hydra.lib.maybes.bind")

maybes_cases = hydra.core.Name("hydra.lib.maybes.cases")

maybes_cat = hydra.core.Name("hydra.lib.maybes.cat")

maybes_compose = hydra.core.Name("hydra.lib.maybes.compose")

maybes_from_just = hydra.core.Name("hydra.lib.maybes.fromJust")

maybes_from_maybe = hydra.core.Name("hydra.lib.maybes.fromMaybe")

maybes_is_just = hydra.core.Name("hydra.lib.maybes.isJust")

maybes_is_nothing = hydra.core.Name("hydra.lib.maybes.isNothing")

maybes_map = hydra.core.Name("hydra.lib.maybes.map")

maybes_map_maybe = hydra.core.Name("hydra.lib.maybes.mapMaybe")

maybes_maybe = hydra.core.Name("hydra.lib.maybes.maybe")

maybes_pure = hydra.core.Name("hydra.lib.maybes.pure")

maybes_to_list = hydra.core.Name("hydra.lib.maybes.toList")

pairs = hydra.packaging.Namespace("hydra.lib.pairs")

pairs_bimap = hydra.core.Name("hydra.lib.pairs.bimap")

pairs_first = hydra.core.Name("hydra.lib.pairs.first")

pairs_second = hydra.core.Name("hydra.lib.pairs.second")

regex = hydra.packaging.Namespace("hydra.lib.regex")

regex_find = hydra.core.Name("hydra.lib.regex.find")

regex_find_all = hydra.core.Name("hydra.lib.regex.findAll")

regex_matches = hydra.core.Name("hydra.lib.regex.matches")

regex_replace = hydra.core.Name("hydra.lib.regex.replace")

regex_replace_all = hydra.core.Name("hydra.lib.regex.replaceAll")

regex_split = hydra.core.Name("hydra.lib.regex.split")

sets = hydra.packaging.Namespace("hydra.lib.sets")

sets_delete = hydra.core.Name("hydra.lib.sets.delete")

sets_difference = hydra.core.Name("hydra.lib.sets.difference")

sets_empty = hydra.core.Name("hydra.lib.sets.empty")

sets_from_list = hydra.core.Name("hydra.lib.sets.fromList")

sets_insert = hydra.core.Name("hydra.lib.sets.insert")

sets_intersection = hydra.core.Name("hydra.lib.sets.intersection")

sets_map = hydra.core.Name("hydra.lib.sets.map")

sets_member = hydra.core.Name("hydra.lib.sets.member")

sets_null = hydra.core.Name("hydra.lib.sets.null")

sets_singleton = hydra.core.Name("hydra.lib.sets.singleton")

sets_size = hydra.core.Name("hydra.lib.sets.size")

sets_to_list = hydra.core.Name("hydra.lib.sets.toList")

sets_union = hydra.core.Name("hydra.lib.sets.union")

sets_unions = hydra.core.Name("hydra.lib.sets.unions")

strings = hydra.packaging.Namespace("hydra.lib.strings")

strings_cat = hydra.core.Name("hydra.lib.strings.cat")

strings_cat2 = hydra.core.Name("hydra.lib.strings.cat2")

strings_char_at = hydra.core.Name("hydra.lib.strings.charAt")

strings_from_list = hydra.core.Name("hydra.lib.strings.fromList")

strings_intercalate = hydra.core.Name("hydra.lib.strings.intercalate")

strings_length = hydra.core.Name("hydra.lib.strings.length")

strings_lines = hydra.core.Name("hydra.lib.strings.lines")

strings_maybe_char_at = hydra.core.Name("hydra.lib.strings.maybeCharAt")

strings_null = hydra.core.Name("hydra.lib.strings.null")

strings_split_on = hydra.core.Name("hydra.lib.strings.splitOn")

strings_to_list = hydra.core.Name("hydra.lib.strings.toList")

strings_to_lower = hydra.core.Name("hydra.lib.strings.toLower")

strings_to_upper = hydra.core.Name("hydra.lib.strings.toUpper")

strings_unlines = hydra.core.Name("hydra.lib.strings.unlines")

typeclass = hydra.packaging.Namespace("hydra.typeclass")

typeclass_eq = hydra.core.Name("hydra.typeclass.Eq")

typeclass_ord = hydra.core.Name("hydra.typeclass.Ord")
