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

pub fn read_term(s: String) -> Option<crate::hydra::core::Term> {
  Some(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(s.clone())))))))}

pub fn binding(el: crate::hydra::core::Binding) -> String {
  let name = el.clone().0.name.clone().0.0.clone() ;
  let t = el.clone().0.term.clone() ;
  let type_str = crate::hydra::lib::maybes::maybe(String::from(""), |ts: crate::hydra::core::TypeScheme| crate::hydra::lib::strings::cat(Vec::from([
    String::from(":("),
    type_scheme(ts.clone()),
    String::from(")")])), el.clone().0.type_.clone()) ;
  crate::hydra::lib::strings::cat(Vec::from([
    name.clone(),
    type_str.clone(),
    String::from(" = "),
    term(t.clone())]))}

pub fn elimination(elm: crate::hydra::core::Elimination) -> String {
  match &*elm.clone().0 {
    crate::hydra::core::Elimination_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let tname = v0_.clone().0.type_name.clone().0.0.clone() ;
        {
          let fname = v0_.clone().0.field.clone().0.0.clone() ;
          crate::hydra::lib::strings::cat(Vec::from([
            String::from("project("),
            tname.clone(),
            String::from("){"),
            fname.clone(),
            String::from("}")]))}}},
    crate::hydra::core::Elimination_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let tname = v0_.clone().0.type_name.clone().0.0.clone() ;
        {
          let mdef = v0_.clone().0.default_.clone() ;
          {
            let cases = v0_.clone().0.cases.clone() ;
            {
              let default_field = crate::hydra::lib::maybes::maybe(Vec::from([]), |d: crate::hydra::core::Term| Vec::from([
                crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
                  name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("[default]")))),
                  term: d.clone()}))]), mdef.clone()) ;
              {
                let all_fields = crate::hydra::lib::lists::concat(Vec::from([
                  cases.clone(),
                  default_field.clone()])) ;
                crate::hydra::lib::strings::cat(Vec::from([
                  String::from("case("),
                  tname.clone(),
                  String::from(")"),
                  fields(all_fields.clone())]))}}}}}},
    crate::hydra::core::Elimination_Variant::Wrap (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::strings::cat(Vec::from([
        String::from("unwrap("),
        v0_.clone().0.0.clone(),
        String::from(")")]))}}}

pub fn field(field: crate::hydra::core::Field) -> String {
  let fname = field.clone().0.name.clone().0.0.clone() ;
  let fterm = field.clone().0.term.clone() ;
  crate::hydra::lib::strings::cat(Vec::from([
    fname.clone(),
    String::from("="),
    term(fterm.clone())]))}

pub fn field_type(ft: crate::hydra::core::FieldType) -> String {
  let fname = ft.clone().0.name.clone().0.0.clone() ;
  let ftyp = ft.clone().0.type_.clone() ;
  crate::hydra::lib::strings::cat(Vec::from([
    fname.clone(),
    String::from(":"),
    type_(ftyp.clone())]))}

pub fn fields(flds: Vec<crate::hydra::core::Field>) -> String {
  let field_strs = crate::hydra::lib::lists::map(field, flds.clone()) ;
  crate::hydra::lib::strings::cat(Vec::from([
    String::from("{"),
    crate::hydra::lib::strings::intercalate(String::from(", "), field_strs.clone()),
    String::from("}")]))}

pub fn float(fv: crate::hydra::core::FloatValue) -> String {
  match &*fv.clone().0 {
    crate::hydra::core::FloatValue_Variant::Bigfloat (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::strings::cat2(crate::hydra::lib::literals::show_bigfloat(v0_.clone()), String::from(":bigfloat"))},
    crate::hydra::core::FloatValue_Variant::Float32 (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::strings::cat2(crate::hydra::lib::literals::show_float32(v0_.clone()), String::from(":float32"))},
    crate::hydra::core::FloatValue_Variant::Float64 (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::strings::cat2(crate::hydra::lib::literals::show_float64(v0_.clone()), String::from(":float64"))}}}

pub fn float_type(ft: crate::hydra::core::FloatType) -> String {
  match &*ft.clone().0 {
    crate::hydra::core::FloatType_Variant::Bigfloat (v0_) => {
      let v0_ = v0_.clone() ;
      String::from("bigfloat")},
    crate::hydra::core::FloatType_Variant::Float32 (v0_) => {
      let v0_ = v0_.clone() ;
      String::from("float32")},
    crate::hydra::core::FloatType_Variant::Float64 (v0_) => {
      let v0_ = v0_.clone() ;
      String::from("float64")}}}

pub fn function(f: crate::hydra::core::Function) -> String {
  match &*f.clone().0 {
    crate::hydra::core::Function_Variant::Elimination (v0_) => {
      let v0_ = v0_.clone() ;
      elimination(v0_.clone())},
    crate::hydra::core::Function_Variant::Lambda (v0_) => {
      let v0_ = v0_.clone() ;
      lambda(v0_.clone())},
    crate::hydra::core::Function_Variant::Primitive (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::strings::cat2(v0_.clone().0.0.clone(), String::from("!"))}}}

pub fn injection(inj: crate::hydra::core::Injection) -> String {
  let tname = inj.clone().0.type_name.clone() ;
  let f = inj.clone().0.field.clone() ;
  crate::hydra::lib::strings::cat(Vec::from([
    String::from("inject("),
    tname.clone().0.0.clone(),
    String::from(")"),
    fields(Vec::from([
      f.clone()]))]))}

pub fn integer(iv: crate::hydra::core::IntegerValue) -> String {
  match &*iv.clone().0 {
    crate::hydra::core::IntegerValue_Variant::Bigint (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::strings::cat2(crate::hydra::lib::literals::show_bigint(v0_.clone()), String::from(":bigint"))},
    crate::hydra::core::IntegerValue_Variant::Int8 (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::strings::cat2(crate::hydra::lib::literals::show_int8(v0_.clone()), String::from(":int8"))},
    crate::hydra::core::IntegerValue_Variant::Int16 (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::strings::cat2(crate::hydra::lib::literals::show_int16(v0_.clone()), String::from(":int16"))},
    crate::hydra::core::IntegerValue_Variant::Int32 (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::strings::cat2(crate::hydra::lib::literals::show_int32(v0_.clone()), String::from(":int32"))},
    crate::hydra::core::IntegerValue_Variant::Int64 (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::strings::cat2(crate::hydra::lib::literals::show_int64(v0_.clone()), String::from(":int64"))},
    crate::hydra::core::IntegerValue_Variant::Uint8 (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::strings::cat2(crate::hydra::lib::literals::show_uint8(v0_.clone()), String::from(":uint8"))},
    crate::hydra::core::IntegerValue_Variant::Uint16 (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::strings::cat2(crate::hydra::lib::literals::show_uint16(v0_.clone()), String::from(":uint16"))},
    crate::hydra::core::IntegerValue_Variant::Uint32 (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::strings::cat2(crate::hydra::lib::literals::show_uint32(v0_.clone()), String::from(":uint32"))},
    crate::hydra::core::IntegerValue_Variant::Uint64 (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::strings::cat2(crate::hydra::lib::literals::show_uint64(v0_.clone()), String::from(":uint64"))}}}

pub fn integer_type(it: crate::hydra::core::IntegerType) -> String {
  match &*it.clone().0 {
    crate::hydra::core::IntegerType_Variant::Bigint (v0_) => {
      let v0_ = v0_.clone() ;
      String::from("bigint")},
    crate::hydra::core::IntegerType_Variant::Int8 (v0_) => {
      let v0_ = v0_.clone() ;
      String::from("int8")},
    crate::hydra::core::IntegerType_Variant::Int16 (v0_) => {
      let v0_ = v0_.clone() ;
      String::from("int16")},
    crate::hydra::core::IntegerType_Variant::Int32 (v0_) => {
      let v0_ = v0_.clone() ;
      String::from("int32")},
    crate::hydra::core::IntegerType_Variant::Int64 (v0_) => {
      let v0_ = v0_.clone() ;
      String::from("int64")},
    crate::hydra::core::IntegerType_Variant::Uint8 (v0_) => {
      let v0_ = v0_.clone() ;
      String::from("uint8")},
    crate::hydra::core::IntegerType_Variant::Uint16 (v0_) => {
      let v0_ = v0_.clone() ;
      String::from("uint16")},
    crate::hydra::core::IntegerType_Variant::Uint32 (v0_) => {
      let v0_ = v0_.clone() ;
      String::from("uint32")},
    crate::hydra::core::IntegerType_Variant::Uint64 (v0_) => {
      let v0_ = v0_.clone() ;
      String::from("uint64")}}}

pub fn lambda(l: crate::hydra::core::Lambda) -> String {
  let v = l.clone().0.parameter.clone().0.0.clone() ;
  let mt = l.clone().0.domain.clone() ;
  let body = l.clone().0.body.clone() ;
  let type_str = crate::hydra::lib::maybes::maybe(String::from(""), |t: crate::hydra::core::Type| crate::hydra::lib::strings::cat2(String::from(":"), type_(t.clone())), mt.clone()) ;
  crate::hydra::lib::strings::cat(Vec::from([
    String::from("\955"),
    v.clone(),
    type_str.clone(),
    String::from("."),
    term(body.clone())]))}

pub fn let_(l: crate::hydra::core::Let) -> String {
  let bindings = l.clone().0.bindings.clone() ;
  let env = l.clone().0.body.clone() ;
  let binding_strs = crate::hydra::lib::lists::map(binding, bindings.clone()) ;
  crate::hydra::lib::strings::cat(Vec::from([
    String::from("let "),
    crate::hydra::lib::strings::intercalate(String::from(", "), binding_strs.clone()),
    String::from(" in "),
    term(env.clone())]))}

pub fn list(f: impl Fn(T0) -> String + Clone, xs: Vec<T0>) -> String {
  let element_strs = crate::hydra::lib::lists::map(f.clone(), xs.clone()) ;
  crate::hydra::lib::strings::cat(Vec::from([
    String::from("["),
    crate::hydra::lib::strings::intercalate(String::from(", "), element_strs.clone()),
    String::from("]")]))}

pub fn literal(l: crate::hydra::core::Literal) -> String {
  match &*l.clone().0 {
    crate::hydra::core::Literal_Variant::Binary (v0_) => {
      let v0_ = v0_.clone() ;
      String::from("[binary]")},
    crate::hydra::core::Literal_Variant::Boolean (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::logic::if_else(v0_.clone(), String::from("true"), String::from("false"))},
    crate::hydra::core::Literal_Variant::Float (v0_) => {
      let v0_ = v0_.clone() ;
      float(v0_.clone())},
    crate::hydra::core::Literal_Variant::Integer (v0_) => {
      let v0_ = v0_.clone() ;
      integer(v0_.clone())},
    crate::hydra::core::Literal_Variant::String (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::literals::show_string(v0_.clone())}}}

pub fn literal_type(lt: crate::hydra::core::LiteralType) -> String {
  match &*lt.clone().0 {
    crate::hydra::core::LiteralType_Variant::Binary (v0_) => {
      let v0_ = v0_.clone() ;
      String::from("binary")},
    crate::hydra::core::LiteralType_Variant::Boolean (v0_) => {
      let v0_ = v0_.clone() ;
      String::from("boolean")},
    crate::hydra::core::LiteralType_Variant::Float (v0_) => {
      let v0_ = v0_.clone() ;
      float_type(v0_.clone())},
    crate::hydra::core::LiteralType_Variant::Integer (v0_) => {
      let v0_ = v0_.clone() ;
      integer_type(v0_.clone())},
    crate::hydra::core::LiteralType_Variant::String (v0_) => {
      let v0_ = v0_.clone() ;
      String::from("string")}}}

pub fn term(t: crate::hydra::core::Term) -> String {
  let gather_terms = |prev: Vec<crate::hydra::core::Term>, app: crate::hydra::core::Application| {
    let lhs = app.clone().0.function.clone() ;
    {
      let rhs = app.clone().0.argument.clone() ;
      match &*lhs.clone().0 {
        crate::hydra::core::Term_Variant::Application (v0_) => {
          let v0_ = v0_.clone() ;
          gather_terms.clone()(crate::hydra::lib::lists::cons(rhs.clone(), prev.clone()), v0_.clone())},
        _ => crate::hydra::lib::lists::cons(lhs.clone(), crate::hydra::lib::lists::cons(rhs.clone(), prev.clone()))}}} ;
  match &*t.clone().0 {
    crate::hydra::core::Term_Variant::Annotated (v0_) => {
      let v0_ = v0_.clone() ;
      term(v0_.clone().0.body.clone())},
    crate::hydra::core::Term_Variant::Application (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let terms = gather_terms.clone()(Vec::from([]), v0_.clone()) ;
        {
          let term_strs = crate::hydra::lib::lists::map(term, terms.clone()) ;
          crate::hydra::lib::strings::cat(Vec::from([
            String::from("("),
            crate::hydra::lib::strings::intercalate(String::from(" @ "), term_strs.clone()),
            String::from(")")]))}}},
    crate::hydra::core::Term_Variant::Either (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::eithers::either(|l: crate::hydra::core::Term| crate::hydra::lib::strings::cat(Vec::from([
        String::from("left("),
        term(l.clone()),
        String::from(")")])), |r: crate::hydra::core::Term| crate::hydra::lib::strings::cat(Vec::from([
        String::from("right("),
        term(r.clone()),
        String::from(")")])), v0_.clone())},
    crate::hydra::core::Term_Variant::Function (v0_) => {
      let v0_ = v0_.clone() ;
      function(v0_.clone())},
    crate::hydra::core::Term_Variant::Let (v0_) => {
      let v0_ = v0_.clone() ;
      let_(v0_.clone())},
    crate::hydra::core::Term_Variant::List (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let term_strs = crate::hydra::lib::lists::map(term, v0_.clone()) ;
        crate::hydra::lib::strings::cat(Vec::from([
          String::from("["),
          crate::hydra::lib::strings::intercalate(String::from(", "), term_strs.clone()),
          String::from("]")]))}},
    crate::hydra::core::Term_Variant::Literal (v0_) => {
      let v0_ = v0_.clone() ;
      literal(v0_.clone())},
    crate::hydra::core::Term_Variant::Map (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let entry = |p: (crate::hydra::core::Term, crate::hydra::core::Term)| crate::hydra::lib::strings::cat(Vec::from([
          term(crate::hydra::lib::pairs::first(p.clone())),
          String::from("="),
          term(crate::hydra::lib::pairs::second(p.clone()))])) ;
        crate::hydra::lib::strings::cat(Vec::from([
          String::from("{"),
          crate::hydra::lib::strings::intercalate(String::from(", "), crate::hydra::lib::lists::map(entry.clone(), crate::hydra::lib::maps::to_list(v0_.clone()))),
          String::from("}")]))}},
    crate::hydra::core::Term_Variant::Maybe (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::maybes::maybe(String::from("nothing"), |t2: crate::hydra::core::Term| crate::hydra::lib::strings::cat(Vec::from([
        String::from("just("),
        term(t2.clone()),
        String::from(")")])), v0_.clone())},
    crate::hydra::core::Term_Variant::Pair (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::strings::cat(Vec::from([
        String::from("("),
        term(crate::hydra::lib::pairs::first(v0_.clone())),
        String::from(", "),
        term(crate::hydra::lib::pairs::second(v0_.clone())),
        String::from(")")]))},
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let tname = v0_.clone().0.type_name.clone().0.0.clone() ;
        {
          let flds = v0_.clone().0.fields.clone() ;
          crate::hydra::lib::strings::cat(Vec::from([
            String::from("record("),
            tname.clone(),
            String::from(")"),
            fields(flds.clone())]))}}},
    crate::hydra::core::Term_Variant::Set (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::strings::cat(Vec::from([
        String::from("{"),
        crate::hydra::lib::strings::intercalate(String::from(", "), crate::hydra::lib::lists::map(term, crate::hydra::lib::sets::to_list(v0_.clone()))),
        String::from("}")]))},
    crate::hydra::core::Term_Variant::TypeLambda (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let param = v0_.clone().0.parameter.clone().0.0.clone() ;
        {
          let body = v0_.clone().0.body.clone() ;
          crate::hydra::lib::strings::cat(Vec::from([
            String::from("\923"),
            param.clone(),
            String::from("."),
            term(body.clone())]))}}},
    crate::hydra::core::Term_Variant::TypeApplication (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let t2 = v0_.clone().0.body.clone() ;
        {
          let typ = v0_.clone().0.type_.clone() ;
          crate::hydra::lib::strings::cat(Vec::from([
            term(t2.clone()),
            String::from("\10216"),
            type_(typ.clone()),
            String::from("\10217")]))}}},
    crate::hydra::core::Term_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      injection(v0_.clone())},
    crate::hydra::core::Term_Variant::Unit (v0_) => {
      let v0_ = v0_.clone() ;
      String::from("unit")},
    crate::hydra::core::Term_Variant::Variable (v0_) => {
      let v0_ = v0_.clone() ;
      v0_.clone().0.0.clone()},
    crate::hydra::core::Term_Variant::Wrap (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let tname = v0_.clone().0.type_name.clone().0.0.clone() ;
        {
          let term1 = v0_.clone().0.body.clone() ;
          crate::hydra::lib::strings::cat(Vec::from([
            String::from("wrap("),
            tname.clone(),
            String::from("){"),
            term(term1.clone()),
            String::from("}")]))}}}}}

pub fn type_(typ: crate::hydra::core::Type) -> String {
  let show_row_type = |flds: Vec<crate::hydra::core::FieldType>| {
    let field_strs = crate::hydra::lib::lists::map(field_type, flds.clone()) ;
    crate::hydra::lib::strings::cat(Vec::from([
      String::from("{"),
      crate::hydra::lib::strings::intercalate(String::from(", "), field_strs.clone()),
      String::from("}")]))} ;
  let gather_types = |prev: Vec<crate::hydra::core::Type>, app: crate::hydra::core::ApplicationType| {
    let lhs = app.clone().0.function.clone() ;
    {
      let rhs = app.clone().0.argument.clone() ;
      match &*lhs.clone().0 {
        crate::hydra::core::Type_Variant::Application (v0_) => {
          let v0_ = v0_.clone() ;
          gather_types.clone()(crate::hydra::lib::lists::cons(rhs.clone(), prev.clone()), v0_.clone())},
        _ => crate::hydra::lib::lists::cons(lhs.clone(), crate::hydra::lib::lists::cons(rhs.clone(), prev.clone()))}}} ;
  let gather_function_types = |prev: Vec<crate::hydra::core::Type>, t: crate::hydra::core::Type| match &*t.clone().0 {
    crate::hydra::core::Type_Variant::Function (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let dom = v0_.clone().0.domain.clone() ;
        {
          let cod = v0_.clone().0.codomain.clone() ;
          gather_function_types.clone()(crate::hydra::lib::lists::cons(dom.clone(), prev.clone()), cod.clone())}}},
    _ => crate::hydra::lib::lists::reverse(crate::hydra::lib::lists::cons(t.clone(), prev.clone()))} ;
  match &*typ.clone().0 {
    crate::hydra::core::Type_Variant::Annotated (v0_) => {
      let v0_ = v0_.clone() ;
      type_(v0_.clone().0.body.clone())},
    crate::hydra::core::Type_Variant::Application (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let types = gather_types.clone()(Vec::from([]), v0_.clone()) ;
        {
          let type_strs = crate::hydra::lib::lists::map(type_, types.clone()) ;
          crate::hydra::lib::strings::cat(Vec::from([
            String::from("("),
            crate::hydra::lib::strings::intercalate(String::from(" @ "), type_strs.clone()),
            String::from(")")]))}}},
    crate::hydra::core::Type_Variant::Either (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let left_typ = v0_.clone().0.left.clone() ;
        {
          let right_typ = v0_.clone().0.right.clone() ;
          crate::hydra::lib::strings::cat(Vec::from([
            String::from("either<"),
            type_(left_typ.clone()),
            String::from(", "),
            type_(right_typ.clone()),
            String::from(">")]))}}},
    crate::hydra::core::Type_Variant::Forall (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let var = v0_.clone().0.parameter.clone().0.0.clone() ;
        {
          let body = v0_.clone().0.body.clone() ;
          crate::hydra::lib::strings::cat(Vec::from([
            String::from("(\8704"),
            var.clone(),
            String::from("."),
            type_(body.clone()),
            String::from(")")]))}}},
    crate::hydra::core::Type_Variant::Function (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let types = gather_function_types.clone()(Vec::from([]), typ.clone()) ;
        {
          let type_strs = crate::hydra::lib::lists::map(type_, types.clone()) ;
          crate::hydra::lib::strings::cat(Vec::from([
            String::from("("),
            crate::hydra::lib::strings::intercalate(String::from(" \8594 "), type_strs.clone()),
            String::from(")")]))}}},
    crate::hydra::core::Type_Variant::List (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::strings::cat(Vec::from([
        String::from("list<"),
        type_(v0_.clone()),
        String::from(">")]))},
    crate::hydra::core::Type_Variant::Literal (v0_) => {
      let v0_ = v0_.clone() ;
      literal_type(v0_.clone())},
    crate::hydra::core::Type_Variant::Map (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let key_typ = v0_.clone().0.keys.clone() ;
        {
          let val_typ = v0_.clone().0.values.clone() ;
          crate::hydra::lib::strings::cat(Vec::from([
            String::from("map<"),
            type_(key_typ.clone()),
            String::from(", "),
            type_(val_typ.clone()),
            String::from(">")]))}}},
    crate::hydra::core::Type_Variant::Maybe (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::strings::cat(Vec::from([
        String::from("maybe<"),
        type_(v0_.clone()),
        String::from(">")]))},
    crate::hydra::core::Type_Variant::Pair (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let first_typ = v0_.clone().0.first.clone() ;
        {
          let second_typ = v0_.clone().0.second.clone() ;
          crate::hydra::lib::strings::cat(Vec::from([
            String::from("("),
            type_(first_typ.clone()),
            String::from(", "),
            type_(second_typ.clone()),
            String::from(")")]))}}},
    crate::hydra::core::Type_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::strings::cat2(String::from("record"), show_row_type.clone()(v0_.clone()))},
    crate::hydra::core::Type_Variant::Set (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::strings::cat(Vec::from([
        String::from("set<"),
        type_(v0_.clone()),
        String::from(">")]))},
    crate::hydra::core::Type_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::strings::cat2(String::from("union"), show_row_type.clone()(v0_.clone()))},
    crate::hydra::core::Type_Variant::Unit (v0_) => {
      let v0_ = v0_.clone() ;
      String::from("unit")},
    crate::hydra::core::Type_Variant::Variable (v0_) => {
      let v0_ = v0_.clone() ;
      v0_.clone().0.0.clone()},
    crate::hydra::core::Type_Variant::Wrap (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::strings::cat(Vec::from([
        String::from("wrap("),
        type_(v0_.clone()),
        String::from(")")]))}}}

pub fn type_scheme(ts: crate::hydra::core::TypeScheme) -> String {
  let vars = ts.clone().0.variables.clone() ;
  let body = ts.clone().0.type_.clone() ;
  let var_names = crate::hydra::lib::lists::map(|v| v.0.0.clone(), vars.clone()) ;
  let fa = crate::hydra::lib::logic::if_else(crate::hydra::lib::lists::null(vars.clone()), String::from(""), crate::hydra::lib::strings::cat(Vec::from([
    String::from("forall "),
    crate::hydra::lib::strings::intercalate(String::from(","), var_names.clone()),
    String::from(". ")]))) ;
  let to_constraint_pair = |v: crate::hydra::core::Name, c: crate::hydra::core::Name| crate::hydra::lib::strings::cat(Vec::from([
    c.clone().0.0.clone(),
    String::from(" "),
    v.clone().0.0.clone()])) ;
  let to_constraint_pairs = |p: (crate::hydra::core::Name, crate::hydra::core::TypeVariableMetadata)| crate::hydra::lib::lists::map(|v1: crate::hydra::core::Name| to_constraint_pair.clone()(crate::hydra::lib::pairs::first(p.clone()), v1.clone()), crate::hydra::lib::sets::to_list(crate::hydra::lib::pairs::second(p.clone()).0.classes.clone())) ;
  let tc = crate::hydra::lib::maybes::maybe(Vec::from([]), |m: BTreeMap<crate::hydra::core::Name, crate::hydra::core::TypeVariableMetadata>| crate::hydra::lib::lists::concat(crate::hydra::lib::lists::map(to_constraint_pairs.clone(), crate::hydra::lib::maps::to_list(m.clone()))), ts.clone().0.constraints.clone()) ;
  crate::hydra::lib::strings::cat(Vec::from([
    String::from("("),
    fa.clone(),
    crate::hydra::lib::logic::if_else(crate::hydra::lib::lists::null(tc.clone()), String::from(""), crate::hydra::lib::strings::cat(Vec::from([
      String::from("("),
      crate::hydra::lib::strings::intercalate(String::from(", "), tc.clone()),
      String::from(") => ")]))),
    type_(body.clone()),
    String::from(")")]))}
