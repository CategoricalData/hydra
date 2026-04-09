#![allow(unused_imports)]
#![allow(non_camel_case_types)]

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::rc::Rc;
use ordered_float::OrderedFloat;
use crate::Either;
use crate::hydra::rewriting::*;
use crate::hydra::literals::*;
use crate::hydra::extract::core::*;
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

pub fn from_json(types: BTreeMap<crate::hydra::core::Name, crate::hydra::core::Type>, tname: crate::hydra::core::Name, typ: crate::hydra::core::Type, value: crate::hydra::json::model::Value) -> Either<String, crate::hydra::core::Term> {
  let stripped = crate::hydra::rewriting::deannotate_type(typ.clone()) ;
  match &*stripped.clone().0 {
    crate::hydra::core::Type_Variant::Literal (v0_) => {
      let v0_ = v0_.clone() ;
      decode_literal(v0_.clone(), value.clone())},
    crate::hydra::core::Type_Variant::List (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let decode_elem = |v: crate::hydra::json::model::Value| from_json(types.clone(), tname.clone(), v0_.clone(), v.clone()) ;
        {
          let arr_result = expect_array(value.clone()) ;
          crate::hydra::lib::eithers::either(|err: String| Left(err.clone()), |arr: Vec<crate::hydra::json::model::Value>| {
            let decoded = crate::hydra::lib::eithers::map_list(decode_elem.clone(), arr.clone()) ;
            crate::hydra::lib::eithers::map(|ts: Vec<crate::hydra::core::Term>| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::List(ts.clone()))), decoded.clone())}, arr_result.clone())}}},
    crate::hydra::core::Type_Variant::Set (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let decode_elem = |v: crate::hydra::json::model::Value| from_json(types.clone(), tname.clone(), v0_.clone(), v.clone()) ;
        {
          let arr_result = expect_array(value.clone()) ;
          crate::hydra::lib::eithers::either(|err: String| Left(err.clone()), |arr: Vec<crate::hydra::json::model::Value>| {
            let decoded = crate::hydra::lib::eithers::map_list(decode_elem.clone(), arr.clone()) ;
            crate::hydra::lib::eithers::map(|elems: Vec<crate::hydra::core::Term>| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Set(crate::hydra::lib::sets::from_list(elems.clone())))), decoded.clone())}, arr_result.clone())}}},
    crate::hydra::core::Type_Variant::Maybe (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let decode_just = |arr: Vec<crate::hydra::json::model::Value>| crate::hydra::lib::eithers::map(|v: crate::hydra::core::Term| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Maybe(Some(v.clone())))), from_json(types.clone(), tname.clone(), v0_.clone(), crate::hydra::lib::lists::head(arr.clone()))) ;
        {
          let decode_maybe_array = |arr: Vec<crate::hydra::json::model::Value>| {
            let len = crate::hydra::lib::lists::length(arr.clone()) ;
            crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::equal(len.clone(), 0i32), Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Maybe(None)))), crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::equal(len.clone(), 1i32), decode_just.clone()(arr.clone()), Left(String::from("expected single-element array for Just"))))} ;
          match &*value.clone().0 {
            crate::hydra::json::model::Value_Variant::Null (v0_) => {
              let v0_ = v0_.clone() ;
              Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Maybe(None))))},
            crate::hydra::json::model::Value_Variant::Array (v0_) => {
              let v0_ = v0_.clone() ;
              decode_maybe_array.clone()(v0_.clone())},
            _ => Left(String::from("expected null or single-element array for Maybe"))}}}},
    crate::hydra::core::Type_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let obj_result = expect_object(value.clone()) ;
        crate::hydra::lib::eithers::either(|err: String| Left(err.clone()), |obj: BTreeMap<String, crate::hydra::json::model::Value>| {
          let decode_field = |ft: crate::hydra::core::FieldType| {
            let fname = ft.clone().0.name.clone() ;
            {
              let ftype = ft.clone().0.type_.clone() ;
              {
                let mval = crate::hydra::lib::maps::lookup(fname.clone().0.0.clone(), obj.clone()) ;
                {
                  let default_val = crate::hydra::json::model::Value(Rc::new(crate::hydra::json::model::Value_Variant::Null)) ;
                  {
                    let json_val = crate::hydra::lib::maybes::from_maybe(default_val.clone(), mval.clone()) ;
                    {
                      let decoded = from_json(types.clone(), tname.clone(), ftype.clone(), json_val.clone()) ;
                      crate::hydra::lib::eithers::map(|v: crate::hydra::core::Term| crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
                        name: fname.clone(),
                        term: v.clone()})), decoded.clone())}}}}}} ;
          {
            let decoded_fields = crate::hydra::lib::eithers::map_list(decode_field.clone(), v0_.clone()) ;
            crate::hydra::lib::eithers::map(|fs: Vec<crate::hydra::core::Field>| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Record(crate::hydra::core::Record(Rc::new(crate::hydra::core::Record_Variant {
              type_name: tname.clone(),
              fields: fs.clone()}))))), decoded_fields.clone())}}, obj_result.clone())}},
    crate::hydra::core::Type_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let decode_variant = |key: String, val: Option<crate::hydra::json::model::Value>, ftype: crate::hydra::core::Type| {
          let json_val = crate::hydra::lib::maybes::from_maybe(crate::hydra::json::model::Value(Rc::new(crate::hydra::json::model::Value_Variant::Null)), val.clone()) ;
          {
            let decoded = from_json(types.clone(), tname.clone(), ftype.clone(), json_val.clone()) ;
            crate::hydra::lib::eithers::map(|v: crate::hydra::core::Term| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
              type_name: tname.clone(),
              field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
                name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(key.clone()))),
                term: v.clone()}))}))))), decoded.clone())}} ;
        {
          let try_field = |key: String, val: Option<crate::hydra::json::model::Value>, ft: crate::hydra::core::FieldType| crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::equal(ft.clone().0.name.clone().0.0.clone(), key.clone()), Some(decode_variant.clone()(key.clone(), val.clone(), ft.clone().0.type_.clone())), None) ;
          {
            let find_and_decode = |key: String, val: Option<crate::hydra::json::model::Value>, fts: Vec<crate::hydra::core::FieldType>| crate::hydra::lib::logic::if_else(crate::hydra::lib::lists::null(fts.clone()), Left(crate::hydra::lib::strings::cat(Vec::from([
              String::from("unknown variant: "),
              key.clone()]))), crate::hydra::lib::maybes::maybe(find_and_decode.clone()(key.clone(), val.clone(), crate::hydra::lib::lists::tail(fts.clone())), |r: Either<String, crate::hydra::core::Term>| r.clone(), try_field.clone()(key.clone(), val.clone(), crate::hydra::lib::lists::head(fts.clone())))) ;
            {
              let decode_single_key = |obj: BTreeMap<String, crate::hydra::json::model::Value>| find_and_decode.clone()(crate::hydra::lib::lists::head(crate::hydra::lib::maps::keys(obj.clone())), crate::hydra::lib::maps::lookup(crate::hydra::lib::lists::head(crate::hydra::lib::maps::keys(obj.clone())), obj.clone()), v0_.clone()) ;
              {
                let process_union = |obj: BTreeMap<String, crate::hydra::json::model::Value>| crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::equal(crate::hydra::lib::lists::length(crate::hydra::lib::maps::keys(obj.clone())), 1i32), decode_single_key.clone()(obj.clone()), Left(String::from("expected single-key object for union"))) ;
                {
                  let obj_result = expect_object(value.clone()) ;
                  crate::hydra::lib::eithers::either(|err: String| Left(err.clone()), |obj: BTreeMap<String, crate::hydra::json::model::Value>| process_union.clone()(obj.clone()), obj_result.clone())}}}}}}},
    crate::hydra::core::Type_Variant::Unit (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let obj_result = expect_object(value.clone()) ;
        crate::hydra::lib::eithers::map(|_2: BTreeMap<String, crate::hydra::json::model::Value>| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Unit)), obj_result.clone())}},
    crate::hydra::core::Type_Variant::Wrap (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let decoded = from_json(types.clone(), tname.clone(), v0_.clone(), value.clone()) ;
        crate::hydra::lib::eithers::map(|v: crate::hydra::core::Term| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
          type_name: tname.clone(),
          body: v.clone()}))))), decoded.clone())}},
    crate::hydra::core::Type_Variant::Map (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let key_type = v0_.clone().0.keys.clone() ;
        {
          let val_type = v0_.clone().0.values.clone() ;
          {
            let arr_result = expect_array(value.clone()) ;
            crate::hydra::lib::eithers::either(|err: String| Left(err.clone()), |arr: Vec<crate::hydra::json::model::Value>| {
              let decode_entry = |entry_json: crate::hydra::json::model::Value| {
                let obj_result = expect_object(entry_json.clone()) ;
                crate::hydra::lib::eithers::either(|err: String| Left(err.clone()), |entry_obj: BTreeMap<String, crate::hydra::json::model::Value>| {
                  let key_json = crate::hydra::lib::maps::lookup(String::from("@key"), entry_obj.clone()) ;
                  {
                    let val_json = crate::hydra::lib::maps::lookup(String::from("@value"), entry_obj.clone()) ;
                    crate::hydra::lib::maybes::maybe(Left(String::from("missing @key in map entry")), |kj: crate::hydra::json::model::Value| crate::hydra::lib::maybes::maybe(Left(String::from("missing @value in map entry")), |vj: crate::hydra::json::model::Value| {
                      let decoded_key = from_json(types.clone(), tname.clone(), key_type.clone(), kj.clone()) ;
                      {
                        let decoded_val = from_json(types.clone(), tname.clone(), val_type.clone(), vj.clone()) ;
                        crate::hydra::lib::eithers::either(|err: String| Left(err.clone()), |k: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|v: crate::hydra::core::Term| (k.clone(), v.clone()), decoded_val.clone()), decoded_key.clone())}}, val_json.clone()), key_json.clone())}}, obj_result.clone())} ;
              {
                let entries = crate::hydra::lib::eithers::map_list(decode_entry.clone(), arr.clone()) ;
                crate::hydra::lib::eithers::map(|es: Vec<(crate::hydra::core::Term, crate::hydra::core::Term)>| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Map(crate::hydra::lib::maps::from_list(es.clone())))), entries.clone())}}, arr_result.clone())}}}},
    crate::hydra::core::Type_Variant::Pair (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let first_type = v0_.clone().0.first.clone() ;
        {
          let second_type = v0_.clone().0.second.clone() ;
          {
            let obj_result = expect_object(value.clone()) ;
            crate::hydra::lib::eithers::either(|err: String| Left(err.clone()), |obj: BTreeMap<String, crate::hydra::json::model::Value>| {
              let first_json = crate::hydra::lib::maps::lookup(String::from("@first"), obj.clone()) ;
              {
                let second_json = crate::hydra::lib::maps::lookup(String::from("@second"), obj.clone()) ;
                crate::hydra::lib::maybes::maybe(Left(String::from("missing @first in pair")), |fj: crate::hydra::json::model::Value| crate::hydra::lib::maybes::maybe(Left(String::from("missing @second in pair")), |sj: crate::hydra::json::model::Value| {
                  let decoded_first = from_json(types.clone(), tname.clone(), first_type.clone(), fj.clone()) ;
                  {
                    let decoded_second = from_json(types.clone(), tname.clone(), second_type.clone(), sj.clone()) ;
                    crate::hydra::lib::eithers::either(|err: String| Left(err.clone()), |f: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|s: crate::hydra::core::Term| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Pair((f.clone(), s.clone())))), decoded_second.clone()), decoded_first.clone())}}, second_json.clone()), first_json.clone())}}, obj_result.clone())}}}},
    crate::hydra::core::Type_Variant::Either (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let left_type = v0_.clone().0.left.clone() ;
        {
          let right_type = v0_.clone().0.right.clone() ;
          {
            let obj_result = expect_object(value.clone()) ;
            crate::hydra::lib::eithers::either(|err: String| Left(err.clone()), |obj: BTreeMap<String, crate::hydra::json::model::Value>| {
              let left_json = crate::hydra::lib::maps::lookup(String::from("@left"), obj.clone()) ;
              {
                let right_json = crate::hydra::lib::maps::lookup(String::from("@right"), obj.clone()) ;
                crate::hydra::lib::maybes::maybe(crate::hydra::lib::maybes::maybe(Left(String::from("expected @left or @right in Either")), |rj: crate::hydra::json::model::Value| {
                  let decoded = from_json(types.clone(), tname.clone(), right_type.clone(), rj.clone()) ;
                  crate::hydra::lib::eithers::map(|v: crate::hydra::core::Term| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Right(v.clone())))), decoded.clone())}, right_json.clone()), |lj: crate::hydra::json::model::Value| {
                  let decoded = from_json(types.clone(), tname.clone(), left_type.clone(), lj.clone()) ;
                  crate::hydra::lib::eithers::map(|v: crate::hydra::core::Term| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Left(v.clone())))), decoded.clone())}, left_json.clone())}}, obj_result.clone())}}}},
    crate::hydra::core::Type_Variant::Variable (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let looked_up = crate::hydra::lib::maps::lookup(v0_.clone(), types.clone()) ;
        crate::hydra::lib::maybes::maybe(Left(crate::hydra::lib::strings::cat(Vec::from([
          String::from("unknown type variable: "),
          v0_.clone().0.0.clone()]))), |resolved_type: crate::hydra::core::Type| from_json(types.clone(), v0_.clone(), resolved_type.clone(), value.clone()), looked_up.clone())}},
    _ => Left(crate::hydra::lib::strings::cat(Vec::from([
      String::from("unsupported type for JSON decoding: "),
      crate::hydra::show::core::type_(typ.clone())])))}}

pub fn decode_literal(lt: crate::hydra::core::LiteralType, value: crate::hydra::json::model::Value) -> Either<String, crate::hydra::core::Term> {
  match &*lt.clone().0 {
    crate::hydra::core::LiteralType_Variant::Binary (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let str_result = expect_string(value.clone()) ;
        crate::hydra::lib::eithers::map(|s: String| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::Binary(crate::hydra::lib::literals::string_to_binary(s.clone()))))))), str_result.clone())}},
    crate::hydra::core::LiteralType_Variant::Boolean (v0_) => {
      let v0_ = v0_.clone() ;
      match &*value.clone().0 {
        crate::hydra::json::model::Value_Variant::Boolean (v0_) => {
          let v0_ = v0_.clone() ;
          Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::Boolean(v0_.clone())))))))},
        _ => Left(String::from("expected boolean"))}},
    crate::hydra::core::LiteralType_Variant::Float (v0_) => {
      let v0_ = v0_.clone() ;
      decode_float(v0_.clone(), value.clone())},
    crate::hydra::core::LiteralType_Variant::Integer (v0_) => {
      let v0_ = v0_.clone() ;
      decode_integer(v0_.clone(), value.clone())},
    crate::hydra::core::LiteralType_Variant::String (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let str_result = expect_string(value.clone()) ;
        crate::hydra::lib::eithers::map(|s: String| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(s.clone())))))), str_result.clone())}}}}

pub fn decode_float(ft: crate::hydra::core::FloatType, value: crate::hydra::json::model::Value) -> Either<String, crate::hydra::core::Term> {
  match &*ft.clone().0 {
    crate::hydra::core::FloatType_Variant::Bigfloat (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let num_result = expect_number(value.clone()) ;
        crate::hydra::lib::eithers::map(|n: OrderedFloat<f64>| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::Float(crate::hydra::core::FloatValue(Rc::new(crate::hydra::core::FloatValue_Variant::Bigfloat(n.clone()))))))))), num_result.clone())}},
    crate::hydra::core::FloatType_Variant::Float32 (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let str_result = expect_string(value.clone()) ;
        crate::hydra::lib::eithers::either(|err: String| Left(err.clone()), |s: String| {
          let parsed = crate::hydra::lib::literals::read_float32(s.clone()) ;
          crate::hydra::lib::maybes::maybe(Left(crate::hydra::lib::strings::cat(Vec::from([
            String::from("invalid float32: "),
            s.clone()]))), |v: OrderedFloat<f32>| Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::Float(crate::hydra::core::FloatValue(Rc::new(crate::hydra::core::FloatValue_Variant::Float32(v.clone())))))))))), parsed.clone())}, str_result.clone())}},
    crate::hydra::core::FloatType_Variant::Float64 (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let num_result = expect_number(value.clone()) ;
        crate::hydra::lib::eithers::map(|n: OrderedFloat<f64>| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::Float(crate::hydra::core::FloatValue(Rc::new(crate::hydra::core::FloatValue_Variant::Float64(crate::hydra::lib::literals::bigfloat_to_float64(n.clone())))))))))), num_result.clone())}}}}

pub fn decode_integer(it: crate::hydra::core::IntegerType, value: crate::hydra::json::model::Value) -> Either<String, crate::hydra::core::Term> {
  match &*it.clone().0 {
    crate::hydra::core::IntegerType_Variant::Bigint (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let str_result = expect_string(value.clone()) ;
        crate::hydra::lib::eithers::either(|err: String| Left(err.clone()), |s: String| {
          let parsed = crate::hydra::lib::literals::read_bigint(s.clone()) ;
          crate::hydra::lib::maybes::maybe(Left(crate::hydra::lib::strings::cat(Vec::from([
            String::from("invalid bigint: "),
            s.clone()]))), |v: String| Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::Integer(crate::hydra::core::IntegerValue(Rc::new(crate::hydra::core::IntegerValue_Variant::Bigint(v.clone())))))))))), parsed.clone())}, str_result.clone())}},
    crate::hydra::core::IntegerType_Variant::Int64 (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let str_result = expect_string(value.clone()) ;
        crate::hydra::lib::eithers::either(|err: String| Left(err.clone()), |s: String| {
          let parsed = crate::hydra::lib::literals::read_int64(s.clone()) ;
          crate::hydra::lib::maybes::maybe(Left(crate::hydra::lib::strings::cat(Vec::from([
            String::from("invalid int64: "),
            s.clone()]))), |v: i64| Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::Integer(crate::hydra::core::IntegerValue(Rc::new(crate::hydra::core::IntegerValue_Variant::Int64(v.clone())))))))))), parsed.clone())}, str_result.clone())}},
    crate::hydra::core::IntegerType_Variant::Uint32 (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let str_result = expect_string(value.clone()) ;
        crate::hydra::lib::eithers::either(|err: String| Left(err.clone()), |s: String| {
          let parsed = crate::hydra::lib::literals::read_uint32(s.clone()) ;
          crate::hydra::lib::maybes::maybe(Left(crate::hydra::lib::strings::cat(Vec::from([
            String::from("invalid uint32: "),
            s.clone()]))), |v: u32| Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::Integer(crate::hydra::core::IntegerValue(Rc::new(crate::hydra::core::IntegerValue_Variant::Uint32(v.clone())))))))))), parsed.clone())}, str_result.clone())}},
    crate::hydra::core::IntegerType_Variant::Uint64 (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let str_result = expect_string(value.clone()) ;
        crate::hydra::lib::eithers::either(|err: String| Left(err.clone()), |s: String| {
          let parsed = crate::hydra::lib::literals::read_uint64(s.clone()) ;
          crate::hydra::lib::maybes::maybe(Left(crate::hydra::lib::strings::cat(Vec::from([
            String::from("invalid uint64: "),
            s.clone()]))), |v: u64| Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::Integer(crate::hydra::core::IntegerValue(Rc::new(crate::hydra::core::IntegerValue_Variant::Uint64(v.clone())))))))))), parsed.clone())}, str_result.clone())}},
    crate::hydra::core::IntegerType_Variant::Int8 (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let num_result = expect_number(value.clone()) ;
        crate::hydra::lib::eithers::map(|n: OrderedFloat<f64>| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::Integer(crate::hydra::core::IntegerValue(Rc::new(crate::hydra::core::IntegerValue_Variant::Int8(crate::hydra::lib::literals::bigint_to_int8(crate::hydra::lib::literals::bigfloat_to_bigint(n.clone()))))))))))), num_result.clone())}},
    crate::hydra::core::IntegerType_Variant::Int16 (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let num_result = expect_number(value.clone()) ;
        crate::hydra::lib::eithers::map(|n: OrderedFloat<f64>| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::Integer(crate::hydra::core::IntegerValue(Rc::new(crate::hydra::core::IntegerValue_Variant::Int16(crate::hydra::lib::literals::bigint_to_int16(crate::hydra::lib::literals::bigfloat_to_bigint(n.clone()))))))))))), num_result.clone())}},
    crate::hydra::core::IntegerType_Variant::Int32 (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let num_result = expect_number(value.clone()) ;
        crate::hydra::lib::eithers::map(|n: OrderedFloat<f64>| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::Integer(crate::hydra::core::IntegerValue(Rc::new(crate::hydra::core::IntegerValue_Variant::Int32(crate::hydra::lib::literals::bigint_to_int32(crate::hydra::lib::literals::bigfloat_to_bigint(n.clone()))))))))))), num_result.clone())}},
    crate::hydra::core::IntegerType_Variant::Uint8 (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let num_result = expect_number(value.clone()) ;
        crate::hydra::lib::eithers::map(|n: OrderedFloat<f64>| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::Integer(crate::hydra::core::IntegerValue(Rc::new(crate::hydra::core::IntegerValue_Variant::Uint8(crate::hydra::lib::literals::bigint_to_uint8(crate::hydra::lib::literals::bigfloat_to_bigint(n.clone()))))))))))), num_result.clone())}},
    crate::hydra::core::IntegerType_Variant::Uint16 (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let num_result = expect_number(value.clone()) ;
        crate::hydra::lib::eithers::map(|n: OrderedFloat<f64>| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::Integer(crate::hydra::core::IntegerValue(Rc::new(crate::hydra::core::IntegerValue_Variant::Uint16(crate::hydra::lib::literals::bigint_to_uint16(crate::hydra::lib::literals::bigfloat_to_bigint(n.clone()))))))))))), num_result.clone())}}}}

pub fn expect_string(value: crate::hydra::json::model::Value) -> Either<String, String> {
  match &*value.clone().0 {
    crate::hydra::json::model::Value_Variant::String (v0_) => {
      let v0_ = v0_.clone() ;
      Right(v0_.clone())},
    _ => Left(String::from("expected string"))}}

pub fn expect_array(value: crate::hydra::json::model::Value) -> Either<String, Vec<crate::hydra::json::model::Value>> {
  match &*value.clone().0 {
    crate::hydra::json::model::Value_Variant::Array (v0_) => {
      let v0_ = v0_.clone() ;
      Right(v0_.clone())},
    _ => Left(String::from("expected array"))}}

pub fn expect_object(value: crate::hydra::json::model::Value) -> Either<String, BTreeMap<String, crate::hydra::json::model::Value>> {
  match &*value.clone().0 {
    crate::hydra::json::model::Value_Variant::Object (v0_) => {
      let v0_ = v0_.clone() ;
      Right(v0_.clone())},
    _ => Left(String::from("expected object"))}}

pub fn expect_number(value: crate::hydra::json::model::Value) -> Either<String, OrderedFloat<f64>> {
  match &*value.clone().0 {
    crate::hydra::json::model::Value_Variant::Number (v0_) => {
      let v0_ = v0_.clone() ;
      Right(v0_.clone())},
    _ => Left(String::from("expected number"))}}
