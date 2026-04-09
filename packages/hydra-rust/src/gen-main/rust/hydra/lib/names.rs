#![allow(unused_imports)]
#![allow(non_camel_case_types)]

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::rc::Rc;
use ordered_float::OrderedFloat;
use crate::Either;
use crate::hydra::accessors::*;
use crate::hydra::ast::*;
use crate::hydra::classes::*;
use crate::hydra::coders::*;
use crate::hydra::context::*;
use crate::hydra::core::*;
use crate::hydra::error::*;
use crate::hydra::grammar::*;
use crate::hydra::graph::*;
use crate::hydra::json::model::*;
use crate::hydra::module::*;
use crate::hydra::parsing::*;
use crate::hydra::phantoms::*;
use crate::hydra::query::*;
use crate::hydra::relational::*;
use crate::hydra::tabular::*;
use crate::hydra::testing::*;
use crate::hydra::topology::*;
use crate::hydra::typing::*;
use crate::hydra::util::*;
use crate::hydra::variants::*;

pub fn chars() -> crate::hydra::module::Namespace {
  crate::hydra::module::Namespace(Rc::new(crate::hydra::module::Namespace_Variant(String::from("hydra.lib.chars"))))}

pub fn eithers() -> crate::hydra::module::Namespace {
  crate::hydra::module::Namespace(Rc::new(crate::hydra::module::Namespace_Variant(String::from("hydra.lib.eithers"))))}

pub fn equality() -> crate::hydra::module::Namespace {
  crate::hydra::module::Namespace(Rc::new(crate::hydra::module::Namespace_Variant(String::from("hydra.lib.equality"))))}

pub fn lists() -> crate::hydra::module::Namespace {
  crate::hydra::module::Namespace(Rc::new(crate::hydra::module::Namespace_Variant(String::from("hydra.lib.lists"))))}

pub fn literals() -> crate::hydra::module::Namespace {
  crate::hydra::module::Namespace(Rc::new(crate::hydra::module::Namespace_Variant(String::from("hydra.lib.literals"))))}

pub fn logic() -> crate::hydra::module::Namespace {
  crate::hydra::module::Namespace(Rc::new(crate::hydra::module::Namespace_Variant(String::from("hydra.lib.logic"))))}

pub fn maps() -> crate::hydra::module::Namespace {
  crate::hydra::module::Namespace(Rc::new(crate::hydra::module::Namespace_Variant(String::from("hydra.lib.maps"))))}

pub fn math() -> crate::hydra::module::Namespace {
  crate::hydra::module::Namespace(Rc::new(crate::hydra::module::Namespace_Variant(String::from("hydra.lib.math"))))}

pub fn maybes() -> crate::hydra::module::Namespace {
  crate::hydra::module::Namespace(Rc::new(crate::hydra::module::Namespace_Variant(String::from("hydra.lib.maybes"))))}

pub fn pairs() -> crate::hydra::module::Namespace {
  crate::hydra::module::Namespace(Rc::new(crate::hydra::module::Namespace_Variant(String::from("hydra.lib.pairs"))))}

pub fn sets() -> crate::hydra::module::Namespace {
  crate::hydra::module::Namespace(Rc::new(crate::hydra::module::Namespace_Variant(String::from("hydra.lib.sets"))))}

pub fn strings() -> crate::hydra::module::Namespace {
  crate::hydra::module::Namespace(Rc::new(crate::hydra::module::Namespace_Variant(String::from("hydra.lib.strings"))))}

pub fn typeclass() -> crate::hydra::module::Namespace {
  crate::hydra::module::Namespace(Rc::new(crate::hydra::module::Namespace_Variant(String::from("hydra.typeclass"))))}

pub fn chars_is_alpha_num() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.chars.isAlphaNum"))))}

pub fn chars_is_lower() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.chars.isLower"))))}

pub fn chars_is_space() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.chars.isSpace"))))}

pub fn chars_is_upper() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.chars.isUpper"))))}

pub fn chars_to_lower() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.chars.toLower"))))}

pub fn chars_to_upper() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.chars.toUpper"))))}

pub fn eithers_bind() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.eithers.bind"))))}

pub fn eithers_bimap() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.eithers.bimap"))))}

pub fn eithers_either() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.eithers.either"))))}

pub fn eithers_foldl() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.eithers.foldl"))))}

pub fn eithers_from_left() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.eithers.fromLeft"))))}

pub fn eithers_from_right() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.eithers.fromRight"))))}

pub fn eithers_is_left() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.eithers.isLeft"))))}

pub fn eithers_is_right() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.eithers.isRight"))))}

pub fn eithers_lefts() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.eithers.lefts"))))}

pub fn eithers_map() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.eithers.map"))))}

pub fn eithers_map_list() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.eithers.mapList"))))}

pub fn eithers_map_maybe() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.eithers.mapMaybe"))))}

pub fn eithers_map_set() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.eithers.mapSet"))))}

pub fn eithers_partition_eithers() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.eithers.partitionEithers"))))}

pub fn eithers_rights() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.eithers.rights"))))}

pub fn equality_compare() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.equality.compare"))))}

pub fn equality_equal() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.equality.equal"))))}

pub fn equality_gt() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.equality.gt"))))}

pub fn equality_gte() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.equality.gte"))))}

pub fn equality_identity() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.equality.identity"))))}

pub fn equality_lt() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.equality.lt"))))}

pub fn equality_lte() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.equality.lte"))))}

pub fn equality_max() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.equality.max"))))}

pub fn equality_min() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.equality.min"))))}

pub fn lists_apply() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.lists.apply"))))}

pub fn lists_at() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.lists.at"))))}

pub fn lists_bind() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.lists.bind"))))}

pub fn lists_concat() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.lists.concat"))))}

pub fn lists_concat2() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.lists.concat2"))))}

pub fn lists_cons() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.lists.cons"))))}

pub fn lists_drop() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.lists.drop"))))}

pub fn lists_drop_while() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.lists.dropWhile"))))}

pub fn lists_elem() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.lists.elem"))))}

pub fn lists_filter() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.lists.filter"))))}

pub fn lists_find() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.lists.find"))))}

pub fn lists_foldl() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.lists.foldl"))))}

pub fn lists_foldr() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.lists.foldr"))))}

pub fn lists_group() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.lists.group"))))}

pub fn lists_head() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.lists.head"))))}

pub fn lists_init() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.lists.init"))))}

pub fn lists_intercalate() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.lists.intercalate"))))}

pub fn lists_intersperse() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.lists.intersperse"))))}

pub fn lists_last() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.lists.last"))))}

pub fn lists_length() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.lists.length"))))}

pub fn lists_map() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.lists.map"))))}

pub fn lists_nub() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.lists.nub"))))}

pub fn lists_null() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.lists.null"))))}

pub fn lists_partition() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.lists.partition"))))}

pub fn lists_pure() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.lists.pure"))))}

pub fn lists_replicate() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.lists.replicate"))))}

pub fn lists_reverse() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.lists.reverse"))))}

pub fn lists_safe_head() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.lists.safeHead"))))}

pub fn lists_singleton() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.lists.singleton"))))}

pub fn lists_sort() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.lists.sort"))))}

pub fn lists_sort_on() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.lists.sortOn"))))}

pub fn lists_span() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.lists.span"))))}

pub fn lists_tail() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.lists.tail"))))}

pub fn lists_take() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.lists.take"))))}

pub fn lists_transpose() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.lists.transpose"))))}

pub fn lists_zip() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.lists.zip"))))}

pub fn lists_zip_with() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.lists.zipWith"))))}

pub fn literals_bigfloat_to_bigint() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.literals.bigfloatToBigint"))))}

pub fn literals_bigfloat_to_float32() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.literals.bigfloatToFloat32"))))}

pub fn literals_bigfloat_to_float64() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.literals.bigfloatToFloat64"))))}

pub fn literals_bigint_to_bigfloat() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.literals.bigintToBigfloat"))))}

pub fn literals_bigint_to_int8() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.literals.bigintToInt8"))))}

pub fn literals_bigint_to_int16() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.literals.bigintToInt16"))))}

pub fn literals_bigint_to_int32() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.literals.bigintToInt32"))))}

pub fn literals_bigint_to_int64() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.literals.bigintToInt64"))))}

pub fn literals_bigint_to_uint8() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.literals.bigintToUint8"))))}

pub fn literals_bigint_to_uint16() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.literals.bigintToUint16"))))}

pub fn literals_bigint_to_uint32() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.literals.bigintToUint32"))))}

pub fn literals_bigint_to_uint64() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.literals.bigintToUint64"))))}

pub fn literals_binary_to_bytes() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.literals.binaryToBytes"))))}

pub fn literals_binary_to_string() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.literals.binaryToString"))))}

pub fn literals_float32_to_bigfloat() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.literals.float32ToBigfloat"))))}

pub fn literals_float64_to_bigfloat() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.literals.float64ToBigfloat"))))}

pub fn literals_int8_to_bigint() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.literals.int8ToBigint"))))}

pub fn literals_int16_to_bigint() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.literals.int16ToBigint"))))}

pub fn literals_int32_to_bigint() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.literals.int32ToBigint"))))}

pub fn literals_int64_to_bigint() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.literals.int64ToBigint"))))}

pub fn literals_read_bigfloat() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.literals.readBigfloat"))))}

pub fn literals_read_bigint() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.literals.readBigint"))))}

pub fn literals_read_boolean() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.literals.readBoolean"))))}

pub fn literals_read_float32() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.literals.readFloat32"))))}

pub fn literals_read_float64() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.literals.readFloat64"))))}

pub fn literals_read_int8() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.literals.readInt8"))))}

pub fn literals_read_int16() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.literals.readInt16"))))}

pub fn literals_read_int32() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.literals.readInt32"))))}

pub fn literals_read_int64() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.literals.readInt64"))))}

pub fn literals_read_string() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.literals.readString"))))}

pub fn literals_read_uint8() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.literals.readUint8"))))}

pub fn literals_read_uint16() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.literals.readUint16"))))}

pub fn literals_read_uint32() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.literals.readUint32"))))}

pub fn literals_read_uint64() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.literals.readUint64"))))}

pub fn literals_show_bigfloat() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.literals.showBigfloat"))))}

pub fn literals_show_bigint() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.literals.showBigint"))))}

pub fn literals_show_boolean() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.literals.showBoolean"))))}

pub fn literals_show_float32() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.literals.showFloat32"))))}

pub fn literals_show_float64() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.literals.showFloat64"))))}

pub fn literals_show_int8() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.literals.showInt8"))))}

pub fn literals_show_int16() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.literals.showInt16"))))}

pub fn literals_show_int32() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.literals.showInt32"))))}

pub fn literals_show_int64() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.literals.showInt64"))))}

pub fn literals_show_uint8() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.literals.showUint8"))))}

pub fn literals_show_uint16() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.literals.showUint16"))))}

pub fn literals_show_uint32() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.literals.showUint32"))))}

pub fn literals_show_uint64() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.literals.showUint64"))))}

pub fn literals_show_string() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.literals.showString"))))}

pub fn literals_string_to_binary() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.literals.stringToBinary"))))}

pub fn literals_uint8_to_bigint() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.literals.uint8ToBigint"))))}

pub fn literals_uint16_to_bigint() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.literals.uint16ToBigint"))))}

pub fn literals_uint32_to_bigint() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.literals.uint32ToBigint"))))}

pub fn literals_uint64_to_bigint() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.literals.uint64ToBigint"))))}

pub fn logic_and() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.logic.and"))))}

pub fn logic_if_else() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.logic.ifElse"))))}

pub fn logic_not() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.logic.not"))))}

pub fn logic_or() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.logic.or"))))}

pub fn maps_alter() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.maps.alter"))))}

pub fn maps_bimap() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.maps.bimap"))))}

pub fn maps_delete() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.maps.delete"))))}

pub fn maps_elems() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.maps.elems"))))}

pub fn maps_empty() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.maps.empty"))))}

pub fn maps_filter() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.maps.filter"))))}

pub fn maps_filter_with_key() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.maps.filterWithKey"))))}

pub fn maps_find_with_default() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.maps.findWithDefault"))))}

pub fn maps_from_list() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.maps.fromList"))))}

pub fn maps_insert() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.maps.insert"))))}

pub fn maps_keys() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.maps.keys"))))}

pub fn maps_lookup() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.maps.lookup"))))}

pub fn maps_map() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.maps.map"))))}

pub fn maps_map_keys() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.maps.mapKeys"))))}

pub fn maps_member() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.maps.member"))))}

pub fn maps_null() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.maps.null"))))}

pub fn maps_singleton() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.maps.singleton"))))}

pub fn maps_size() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.maps.size"))))}

pub fn maps_to_list() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.maps.toList"))))}

pub fn maps_union() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.maps.union"))))}

pub fn math_abs() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.math.abs"))))}

pub fn math_acos() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.math.acos"))))}

pub fn math_acosh() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.math.acosh"))))}

pub fn math_add() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.math.add"))))}

pub fn math_asin() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.math.asin"))))}

pub fn math_asinh() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.math.asinh"))))}

pub fn math_atan() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.math.atan"))))}

pub fn math_atan2() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.math.atan2"))))}

pub fn math_atanh() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.math.atanh"))))}

pub fn math_ceiling() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.math.ceiling"))))}

pub fn math_cos() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.math.cos"))))}

pub fn math_cosh() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.math.cosh"))))}

pub fn math_div() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.math.div"))))}

pub fn math_e() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.math.e"))))}

pub fn math_even() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.math.even"))))}

pub fn math_exp() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.math.exp"))))}

pub fn math_floor() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.math.floor"))))}

pub fn math_log() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.math.log"))))}

pub fn math_log_base() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.math.logBase"))))}

pub fn math_max() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.math.max"))))}

pub fn math_min() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.math.min"))))}

pub fn math_mod() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.math.mod"))))}

pub fn math_mul() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.math.mul"))))}

pub fn math_negate() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.math.negate"))))}

pub fn math_odd() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.math.odd"))))}

pub fn math_pi() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.math.pi"))))}

pub fn math_pow() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.math.pow"))))}

pub fn math_pred() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.math.pred"))))}

pub fn math_range() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.math.range"))))}

pub fn math_rem() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.math.rem"))))}

pub fn math_round() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.math.round"))))}

pub fn math_round_bigfloat() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.math.roundBigfloat"))))}

pub fn math_round_float32() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.math.roundFloat32"))))}

pub fn math_round_float64() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.math.roundFloat64"))))}

pub fn math_signum() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.math.signum"))))}

pub fn math_sin() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.math.sin"))))}

pub fn math_sinh() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.math.sinh"))))}

pub fn math_sqrt() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.math.sqrt"))))}

pub fn math_sub() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.math.sub"))))}

pub fn math_succ() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.math.succ"))))}

pub fn math_tan() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.math.tan"))))}

pub fn math_tanh() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.math.tanh"))))}

pub fn math_truncate() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.math.truncate"))))}

pub fn maybes_apply() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.maybes.apply"))))}

pub fn maybes_bind() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.maybes.bind"))))}

pub fn maybes_cases() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.maybes.cases"))))}

pub fn maybes_cat() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.maybes.cat"))))}

pub fn maybes_compose() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.maybes.compose"))))}

pub fn maybes_from_just() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.maybes.fromJust"))))}

pub fn maybes_from_maybe() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.maybes.fromMaybe"))))}

pub fn maybes_is_just() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.maybes.isJust"))))}

pub fn maybes_is_nothing() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.maybes.isNothing"))))}

pub fn maybes_map() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.maybes.map"))))}

pub fn maybes_map_maybe() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.maybes.mapMaybe"))))}

pub fn maybes_maybe() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.maybes.maybe"))))}

pub fn maybes_pure() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.maybes.pure"))))}

pub fn maybes_to_list() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.maybes.toList"))))}

pub fn pairs_bimap() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.pairs.bimap"))))}

pub fn pairs_first() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.pairs.first"))))}

pub fn pairs_second() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.pairs.second"))))}

pub fn sets_delete() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.sets.delete"))))}

pub fn sets_difference() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.sets.difference"))))}

pub fn sets_empty() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.sets.empty"))))}

pub fn sets_from_list() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.sets.fromList"))))}

pub fn sets_insert() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.sets.insert"))))}

pub fn sets_intersection() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.sets.intersection"))))}

pub fn sets_map() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.sets.map"))))}

pub fn sets_member() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.sets.member"))))}

pub fn sets_null() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.sets.null"))))}

pub fn sets_singleton() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.sets.singleton"))))}

pub fn sets_size() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.sets.size"))))}

pub fn sets_to_list() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.sets.toList"))))}

pub fn sets_union() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.sets.union"))))}

pub fn sets_unions() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.sets.unions"))))}

pub fn strings_cat() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.strings.cat"))))}

pub fn strings_cat2() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.strings.cat2"))))}

pub fn strings_char_at() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.strings.charAt"))))}

pub fn strings_from_list() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.strings.fromList"))))}

pub fn strings_intercalate() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.strings.intercalate"))))}

pub fn strings_null() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.strings.null"))))}

pub fn strings_length() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.strings.length"))))}

pub fn strings_lines() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.strings.lines"))))}

pub fn strings_split_on() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.strings.splitOn"))))}

pub fn strings_to_list() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.strings.toList"))))}

pub fn strings_to_lower() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.strings.toLower"))))}

pub fn strings_to_upper() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.strings.toUpper"))))}

pub fn strings_unlines() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.strings.unlines"))))}

pub fn typeclass_eq() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.typeclass.Eq"))))}

pub fn typeclass_ord() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.typeclass.Ord"))))}
