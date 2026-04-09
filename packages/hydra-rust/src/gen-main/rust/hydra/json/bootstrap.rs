#![allow(unused_imports)]
#![allow(non_camel_case_types)]

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::rc::Rc;
use ordered_float::OrderedFloat;
use crate::Either;
use crate::hydra::core::*;

pub fn types_by_name() -> BTreeMap<crate::hydra::core::Name, crate::hydra::core::Type> {
  BTreeMap::from([
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.coders.AdapterContext")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Record(Vec::from([
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("graph")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.graph.Graph"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The underlying graph of elements and primitives"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("language")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.coders.Language"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The language being encoded or decoded"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("adapters")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Map(crate::hydra::core::MapType(Rc::new(crate::hydra::core::MapType_Variant {
              keys: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Name"))))))),
              values: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Application(crate::hydra::core::ApplicationType(Rc::new(crate::hydra::core::ApplicationType_Variant {
                function: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Application(crate::hydra::core::ApplicationType(Rc::new(crate::hydra::core::ApplicationType_Variant {
                  function: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Application(crate::hydra::core::ApplicationType(Rc::new(crate::hydra::core::ApplicationType_Variant {
                    function: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Application(crate::hydra::core::ApplicationType(Rc::new(crate::hydra::core::ApplicationType_Variant {
                      function: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.util.Adapter"))))))),
                      argument: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Type")))))))}))))),
                    argument: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Type")))))))}))))),
                  argument: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term")))))))}))))),
                argument: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term")))))))})))))}))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A map of type names to adapters for those types"))))))))])})))))}))])))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("An evaluation context together with a source language and a target language"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.coders.CoderDirection")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Union(Vec::from([
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("encode")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Unit))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("decode")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Unit))}))])))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("Indicates either the 'out' or the 'in' direction of a coder"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.coders.Language")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Record(Vec::from([
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("name")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.coders.LanguageName"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The unique name of the language"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("constraints")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.coders.LanguageConstraints"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The constraints which characterize the language"))))))))])})))))}))])))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A named language together with language-specific constraints"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.coders.LanguageConstraints")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Record(Vec::from([
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("eliminationVariants")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Set(crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.variants.EliminationVariant")))))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("All supported elimination variants"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("literalVariants")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Set(crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.variants.LiteralVariant")))))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("All supported literal variants"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("floatTypes")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Set(crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.FloatType")))))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("All supported float types"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("functionVariants")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Set(crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.variants.FunctionVariant")))))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("All supported function variants"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("integerTypes")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Set(crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.IntegerType")))))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("All supported integer types"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("termVariants")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Set(crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.variants.TermVariant")))))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("All supported term variants"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("typeVariants")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Set(crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.variants.TypeVariant")))))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("All supported type variants"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("types")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Function(crate::hydra::core::FunctionType(Rc::new(crate::hydra::core::FunctionType_Variant {
              domain: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Type"))))))),
              codomain: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Literal(crate::hydra::core::LiteralType(Rc::new(crate::hydra::core::LiteralType_Variant::Boolean)))))}))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A logical set of types, as a predicate which tests a type for inclusion"))))))))])})))))}))])))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A set of constraints on valid type and term expressions, characterizing a language"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.coders.LanguageName")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Wrap(crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Literal(crate::hydra::core::LiteralType(Rc::new(crate::hydra::core::LiteralType_Variant::String)))))))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The unique name of a language"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.coders.SymmetricAdapter")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Forall(crate::hydra::core::ForallType(Rc::new(crate::hydra::core::ForallType_Variant {
        parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("t")))),
        body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Forall(crate::hydra::core::ForallType(Rc::new(crate::hydra::core::ForallType_Variant {
          parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("v")))),
          body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Application(crate::hydra::core::ApplicationType(Rc::new(crate::hydra::core::ApplicationType_Variant {
            function: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Application(crate::hydra::core::ApplicationType(Rc::new(crate::hydra::core::ApplicationType_Variant {
              function: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Application(crate::hydra::core::ApplicationType(Rc::new(crate::hydra::core::ApplicationType_Variant {
                function: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Application(crate::hydra::core::ApplicationType(Rc::new(crate::hydra::core::ApplicationType_Variant {
                  function: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.util.Adapter"))))))),
                  argument: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("t")))))))}))))),
                argument: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("t")))))))}))))),
              argument: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("v")))))))}))))),
            argument: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("v")))))))})))))})))))}))))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A bidirectional encoder which maps between the same type and term languages on either side"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.coders.TraversalOrder")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Union(Vec::from([
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("pre")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Unit)),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("Pre-order traversal"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("post")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Unit)),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("Post-order traversal"))))))))])})))))}))])))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("Specifies either a pre-order or post-order traversal"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.coders.TypeAdapter")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Function(crate::hydra::core::FunctionType(Rc::new(crate::hydra::core::FunctionType_Variant {
        domain: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.coders.AdapterContext"))))))),
        codomain: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Function(crate::hydra::core::FunctionType(Rc::new(crate::hydra::core::FunctionType_Variant {
          domain: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Type"))))))),
          codomain: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Either(crate::hydra::core::EitherType(Rc::new(crate::hydra::core::EitherType_Variant {
            left: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Literal(crate::hydra::core::LiteralType(Rc::new(crate::hydra::core::LiteralType_Variant::String))))),
            right: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Application(crate::hydra::core::ApplicationType(Rc::new(crate::hydra::core::ApplicationType_Variant {
              function: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Application(crate::hydra::core::ApplicationType(Rc::new(crate::hydra::core::ApplicationType_Variant {
                function: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.coders.SymmetricAdapter"))))))),
                argument: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Type")))))))}))))),
              argument: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term")))))))})))))})))))})))))}))))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A function which maps a Hydra type to a symmetric adapter between types and terms"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.context.Context")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Record(Vec::from([
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("trace")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::List(crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Literal(crate::hydra::core::LiteralType(Rc::new(crate::hydra::core::LiteralType_Variant::String)))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A stack of context labels describing the current execution path"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("messages")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::List(crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Literal(crate::hydra::core::LiteralType(Rc::new(crate::hydra::core::LiteralType_Variant::String)))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A log of warnings and/or info messages"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("other")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Map(crate::hydra::core::MapType(Rc::new(crate::hydra::core::MapType_Variant {
              keys: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Name"))))))),
              values: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term")))))))}))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A map of string keys to arbitrary terms as values, for application-specific use"))))))))])})))))}))])))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("An execution context for tracing and diagnostics, threaded through function calls"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.context.InContext")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Forall(crate::hydra::core::ForallType(Rc::new(crate::hydra::core::ForallType_Variant {
        parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("e")))),
        body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Record(Vec::from([
          crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
            name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("object")))),
            type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
              body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("e"))))))),
              annotation: BTreeMap::from([
                (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A domain object; typically an error"))))))))])})))))})),
          crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
            name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("context")))),
            type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
              body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.context.Context"))))))),
              annotation: BTreeMap::from([
                (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The execution context at the point of capture"))))))))])})))))}))]))))}))))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A particular domain object (such as an error) together with an execution context"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.AnnotatedTerm")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Record(Vec::from([
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("body")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The term being annotated"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("annotation")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Map(crate::hydra::core::MapType(Rc::new(crate::hydra::core::MapType_Variant {
              keys: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Name"))))))),
              values: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term")))))))}))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The annotation as a map from keys to values"))))))))])})))))}))])))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A term together with an annotation"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.AnnotatedType")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Record(Vec::from([
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("body")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Type"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The type being annotated"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("annotation")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Map(crate::hydra::core::MapType(Rc::new(crate::hydra::core::MapType_Variant {
              keys: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Name"))))))),
              values: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term")))))))}))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The annotation as a map from keys to values"))))))))])})))))}))])))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A type together with an annotation"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Application")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Record(Vec::from([
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("function")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The left-hand side of the application"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("argument")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The right-hand side of the application"))))))))])})))))}))])))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A term which applies a function to an argument"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.ApplicationType")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Record(Vec::from([
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("function")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Type"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The left-hand side of the application"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("argument")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Type"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The right-hand side of the application"))))))))])})))))}))])))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The type-level analog of an application term"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Binding")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Record(Vec::from([
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("name")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Name"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The name of the bound variable"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("term")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The term to which the variable is bound"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("type")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Maybe(crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.TypeScheme")))))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The optional type of the bound term"))))))))])})))))}))])))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A field with an optional type scheme, used to bind variables to terms in a 'let' expression"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.CaseStatement")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Record(Vec::from([
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("typeName")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Name"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The name of the union type"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("default")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Maybe(crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term")))))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("An optional default case, used if none of the explicit cases match"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("cases")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::List(crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Field")))))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A list of case alternatives, one per union field"))))))))])})))))}))])))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A union elimination; a case statement"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.EitherType")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Record(Vec::from([
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("left")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Type"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The 'left' alternative"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("right")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Type"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The 'right' alternative"))))))))])})))))}))])))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A type which provides a choice between a 'left' type and a 'right' type"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Elimination")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Union(Vec::from([
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("record")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Projection"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("Eliminates a record by projecting a given field"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("union")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.CaseStatement"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("Eliminates a union term by matching over the fields of the union. This is a case statement."))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("wrap")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Name"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("Unwrap a wrapped term"))))))))])})))))}))])))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A corresponding elimination for an introduction term"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Field")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Record(Vec::from([
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("name")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Name"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The name of the field"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("term")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The term value of the field"))))))))])})))))}))])))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A name/term pair"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.FieldType")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Record(Vec::from([
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("name")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Name"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The name of the field"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("type")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Type"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The type of the field"))))))))])})))))}))])))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A name/type pair"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.FloatType")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Union(Vec::from([
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("bigfloat")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Unit)),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("An arbitrary-precision floating-point type"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("float32")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Unit)),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A 32-bit floating-point type"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("float64")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Unit)),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A 64-bit floating-point type"))))))))])})))))}))])))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A floating-point type"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.FloatValue")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Union(Vec::from([
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("bigfloat")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Literal(crate::hydra::core::LiteralType(Rc::new(crate::hydra::core::LiteralType_Variant::Float(crate::hydra::core::FloatType(Rc::new(crate::hydra::core::FloatType_Variant::Bigfloat)))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("An arbitrary-precision floating-point value"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("float32")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Literal(crate::hydra::core::LiteralType(Rc::new(crate::hydra::core::LiteralType_Variant::Float(crate::hydra::core::FloatType(Rc::new(crate::hydra::core::FloatType_Variant::Float32)))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A 32-bit floating-point value"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("float64")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Literal(crate::hydra::core::LiteralType(Rc::new(crate::hydra::core::LiteralType_Variant::Float(crate::hydra::core::FloatType(Rc::new(crate::hydra::core::FloatType_Variant::Float64)))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A 64-bit floating-point value"))))))))])})))))}))])))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A floating-point literal value"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.ForallType")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Record(Vec::from([
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("parameter")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Name"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The variable which is bound by the lambda"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("body")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Type"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The body of the lambda"))))))))])})))))}))])))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A universally quantified type; the System F equivalent of a type scheme, and the type-level equivalent of a lambda term."))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Function")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Union(Vec::from([
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("elimination")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Elimination"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("An elimination for any of a few term variants"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("lambda")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Lambda"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A function abstraction (lambda)"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("primitive")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Name"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A reference to a built-in (primitive) function"))))))))])})))))}))])))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A function"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.FunctionType")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Record(Vec::from([
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("domain")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Type"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The domain (input) type of the function"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("codomain")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Type"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The codomain (output) type of the function"))))))))])})))))}))])))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A function type, also known as an arrow type"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Injection")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Record(Vec::from([
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("typeName")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Name"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The name of the union type"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("field")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Field"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The field being injected, including its name and value"))))))))])})))))}))])))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("An instance of a union type; i.e. a string-indexed generalization of inl() or inr()"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.IntegerType")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Union(Vec::from([
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("bigint")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Unit)),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("An arbitrary-precision integer type"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("int8")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Unit)),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("An 8-bit signed integer type"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("int16")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Unit)),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A 16-bit signed integer type"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("int32")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Unit)),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A 32-bit signed integer type"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("int64")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Unit)),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A 64-bit signed integer type"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("uint8")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Unit)),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("An 8-bit unsigned integer type"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("uint16")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Unit)),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A 16-bit unsigned integer type"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("uint32")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Unit)),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A 32-bit unsigned integer type"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("uint64")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Unit)),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A 64-bit unsigned integer type"))))))))])})))))}))])))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("An integer type"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.IntegerValue")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Union(Vec::from([
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("bigint")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Literal(crate::hydra::core::LiteralType(Rc::new(crate::hydra::core::LiteralType_Variant::Integer(crate::hydra::core::IntegerType(Rc::new(crate::hydra::core::IntegerType_Variant::Bigint)))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("An arbitrary-precision integer value"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("int8")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Literal(crate::hydra::core::LiteralType(Rc::new(crate::hydra::core::LiteralType_Variant::Integer(crate::hydra::core::IntegerType(Rc::new(crate::hydra::core::IntegerType_Variant::Int8)))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("An 8-bit signed integer value"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("int16")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Literal(crate::hydra::core::LiteralType(Rc::new(crate::hydra::core::LiteralType_Variant::Integer(crate::hydra::core::IntegerType(Rc::new(crate::hydra::core::IntegerType_Variant::Int16)))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A 16-bit signed integer value (short value)"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("int32")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Literal(crate::hydra::core::LiteralType(Rc::new(crate::hydra::core::LiteralType_Variant::Integer(crate::hydra::core::IntegerType(Rc::new(crate::hydra::core::IntegerType_Variant::Int32)))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A 32-bit signed integer value (int value)"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("int64")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Literal(crate::hydra::core::LiteralType(Rc::new(crate::hydra::core::LiteralType_Variant::Integer(crate::hydra::core::IntegerType(Rc::new(crate::hydra::core::IntegerType_Variant::Int64)))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A 64-bit signed integer value (long value)"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("uint8")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Literal(crate::hydra::core::LiteralType(Rc::new(crate::hydra::core::LiteralType_Variant::Integer(crate::hydra::core::IntegerType(Rc::new(crate::hydra::core::IntegerType_Variant::Uint8)))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("An 8-bit unsigned integer value (byte)"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("uint16")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Literal(crate::hydra::core::LiteralType(Rc::new(crate::hydra::core::LiteralType_Variant::Integer(crate::hydra::core::IntegerType(Rc::new(crate::hydra::core::IntegerType_Variant::Uint16)))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A 16-bit unsigned integer value"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("uint32")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Literal(crate::hydra::core::LiteralType(Rc::new(crate::hydra::core::LiteralType_Variant::Integer(crate::hydra::core::IntegerType(Rc::new(crate::hydra::core::IntegerType_Variant::Uint32)))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A 32-bit unsigned integer value (unsigned int)"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("uint64")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Literal(crate::hydra::core::LiteralType(Rc::new(crate::hydra::core::LiteralType_Variant::Integer(crate::hydra::core::IntegerType(Rc::new(crate::hydra::core::IntegerType_Variant::Uint64)))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A 64-bit unsigned integer value (unsigned long)"))))))))])})))))}))])))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("An integer literal value"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Lambda")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Record(Vec::from([
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("parameter")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Name"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The parameter of the lambda"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("domain")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Maybe(crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Type")))))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("An optional domain type for the lambda"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("body")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The body of the lambda"))))))))])})))))}))])))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A function abstraction (lambda)"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Let")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Record(Vec::from([
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("bindings")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::List(crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Binding")))))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The list of variable bindings"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("body")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The body term in which the variables are bound"))))))))])})))))}))])))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A set of (possibly recursive) 'let' bindings together with a body in which they are bound"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Literal")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Union(Vec::from([
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("binary")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Literal(crate::hydra::core::LiteralType(Rc::new(crate::hydra::core::LiteralType_Variant::Binary))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A binary literal"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("boolean")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Literal(crate::hydra::core::LiteralType(Rc::new(crate::hydra::core::LiteralType_Variant::Boolean))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A boolean literal"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("float")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.FloatValue"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A floating-point literal"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("integer")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.IntegerValue"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("An integer literal"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("string")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Literal(crate::hydra::core::LiteralType(Rc::new(crate::hydra::core::LiteralType_Variant::String))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A string literal"))))))))])})))))}))])))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A term constant; an instance of a literal type"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.LiteralType")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Union(Vec::from([
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("binary")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Unit)),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The type of a binary (byte string) value"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("boolean")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Unit)),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The type of a boolean (true/false) value"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("float")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.FloatType"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The type of a floating-point value"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("integer")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.IntegerType"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The type of an integer value"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("string")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Unit)),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The type of a string value"))))))))])})))))}))])))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("Any of a fixed set of literal types, also called atomic types, base types, primitive types, or type constants"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.MapType")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Record(Vec::from([
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("keys")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Type"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The type of keys in the map"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("values")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Type"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The type of values in the map"))))))))])})))))}))])))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A map type"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Name")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Wrap(crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Literal(crate::hydra::core::LiteralType(Rc::new(crate::hydra::core::LiteralType_Variant::String)))))))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A unique identifier in some context; a string-valued key"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.PairType")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Record(Vec::from([
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("first")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Type"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The first component of the pair"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("second")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Type"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The second component of the pair"))))))))])})))))}))])))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A type which pairs a 'first' type and a 'second' type"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Projection")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Record(Vec::from([
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("typeName")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Name"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The name of the record type"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("field")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Name"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The name of the projected field"))))))))])})))))}))])))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A record elimination; a projection"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Record")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Record(Vec::from([
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("typeName")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Name"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The name of the record type"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("fields")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::List(crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Field")))))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The fields of the record, as a list of name/term pairs"))))))))])})))))}))])))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A record, or labeled tuple; a map of field names to terms"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Union(Vec::from([
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("annotated")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.AnnotatedTerm"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A term annotated with metadata"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("application")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Application"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A function application"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("either")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Either(crate::hydra::core::EitherType(Rc::new(crate::hydra::core::EitherType_Variant {
              left: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term"))))))),
              right: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term")))))))}))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("An either value"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("function")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Function"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A function term"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("let")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Let"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A 'let' term, which binds variables to terms"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("list")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::List(crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term")))))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A list"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("literal")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Literal"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A literal value"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("map")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Map(crate::hydra::core::MapType(Rc::new(crate::hydra::core::MapType_Variant {
              keys: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term"))))))),
              values: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term")))))))}))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A map of keys to values"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("maybe")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Maybe(crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term")))))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("An optional value"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("pair")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Pair(crate::hydra::core::PairType(Rc::new(crate::hydra::core::PairType_Variant {
              first: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term"))))))),
              second: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term")))))))}))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A pair (2-tuple)"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("record")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Record"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A record term"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("set")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Set(crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term")))))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A set of values"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("typeApplication")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.TypeApplicationTerm"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A System F type application term"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("typeLambda")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.TypeLambda"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A System F type abstraction term"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("union")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Injection"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("An injection; an instance of a union type"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("unit")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Unit)),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A unit value; a term with no value"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("variable")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Name"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A variable reference"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("wrap")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.WrappedTerm"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A wrapped term; an instance of a wrapper type (newtype)"))))))))])})))))}))])))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A data term"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Type")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Union(Vec::from([
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("annotated")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.AnnotatedType"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("An annotated type"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("application")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.ApplicationType"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A type application"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("either")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.EitherType"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("An either (sum) type"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("forall")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.ForallType"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A universally quantified (polymorphic) type"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("function")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.FunctionType"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A function type"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("list")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Type"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A list type"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("literal")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.LiteralType"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A literal type"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("map")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.MapType"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A map type"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("maybe")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Type"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("An optional type"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("pair")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.PairType"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A pair (2-tuple) type"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("record")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::List(crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.FieldType")))))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A record type"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("set")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Type"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A set type"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("union")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::List(crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.FieldType")))))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A union type with field names"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("unit")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Unit)),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The unit type"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("variable")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Name"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A type variable"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("wrap")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Type"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A wrapped type (newtype)"))))))))])})))))}))])))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A data type"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.TypeApplicationTerm")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Record(Vec::from([
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("body")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The term being applied to a type"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("type")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Type"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The type argument"))))))))])})))))}))])))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A term applied to a type; a type application"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.TypeLambda")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Record(Vec::from([
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("parameter")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Name"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The type variable introduced by the abstraction"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("body")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The body of the abstraction"))))))))])})))))}))])))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A System F type abstraction term"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.TypeScheme")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Record(Vec::from([
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("variables")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::List(crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Name")))))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The free type variables"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("type")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Type"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The type expression"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("constraints")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Maybe(crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Map(crate::hydra::core::MapType(Rc::new(crate::hydra::core::MapType_Variant {
              keys: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Name"))))))),
              values: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.TypeVariableMetadata")))))))})))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("Optional metadata for type variables, including typeclass constraints. The map keys are type variable names."))))))))])})))))}))])))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A type expression together with free type variables occurring in the expression"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.TypeVariableMetadata")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Record(Vec::from([
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("classes")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Set(crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Name")))))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The set of typeclass constraints on this type variable"))))))))])})))))}))])))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("Metadata associated with a type variable, including typeclass constraints"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.WrappedTerm")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Record(Vec::from([
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("typeName")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Name"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The name of the wrapper type"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("body")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The wrapped term"))))))))])})))))}))])))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A term wrapped in a type name"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.CheckingError")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Union(Vec::from([
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("incorrectUnification")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.IncorrectUnificationError"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A post-unification consistency check failure"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("notAForallType")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.NotAForallTypeError"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A type that is not a forall type when one was expected"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("notAFunctionType")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.NotAFunctionTypeError"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A type that is not a function type when one was expected"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("typeArityMismatch")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.TypeArityMismatchError"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A type constructor applied to the wrong number of arguments"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("typeMismatch")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.TypeMismatchError"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A type mismatch between expected and actual types"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("unboundTypeVariables")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.UnboundTypeVariablesError"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("Type variables that are not bound in scope"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("unequalTypes")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.UnequalTypesError"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("Multiple types that should be equal but are not"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("unsupportedTermVariant")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.UnsupportedTermVariantError"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A term variant that the type checker does not support"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("untypedLambda")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.UntypedLambdaError"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A lambda expression without a type annotation on its parameter"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("untypedLetBinding")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.UntypedLetBindingError"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A let binding without a type annotation"))))))))])})))))}))])))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("An error that occurred during type checking"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.DecodingError")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Wrap(crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Literal(crate::hydra::core::LiteralType(Rc::new(crate::hydra::core::LiteralType_Variant::String)))))))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("An error that occurred during decoding of a term"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.DuplicateBindingError")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Record(Vec::from([
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("name")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Name"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The duplicated binding name"))))))))])})))))}))])))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A duplicate binding name in a let expression"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.DuplicateFieldError")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Record(Vec::from([
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("name")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Name"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The duplicated field name"))))))))])})))))}))])))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A duplicate field name in a record or union type"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.Error")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Union(Vec::from([
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("checking")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.CheckingError"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A type checking error"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("decoding")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.DecodingError"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("An error that occurred during decoding of a term"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("duplicateBinding")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.DuplicateBindingError"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A duplicate binding name error"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("duplicateField")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.DuplicateFieldError"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A duplicate field name error"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("other")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.OtherError"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("Any other error"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("undefinedField")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.UndefinedFieldError"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A reference to an undefined field"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("undefinedTerm")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.UndefinedTermError"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A reference to an undefined term"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("undefinedType")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.UndefinedTypeError"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A reference to an undefined type"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("unexpectedTermVariant")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.UnexpectedTermVariantError"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("An unexpected term variant"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("unexpectedTypeVariant")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.UnexpectedTypeVariantError"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("An unexpected type variant"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("unification")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.UnificationError"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A type unification error"))))))))])})))))}))])))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("An error of any kind, with kernel errors particularly differentiated"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.IncorrectUnificationError")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Record(Vec::from([
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("substitution")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.typing.TypeSubst"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The substitution that failed the consistency check"))))))))])})))))}))])))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A post-unification consistency check failure"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.NotAForallTypeError")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Record(Vec::from([
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("type")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Type"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The actual type encountered"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("typeArguments")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::List(crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Type")))))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The type arguments that were being applied"))))))))])})))))}))])))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A type that is not a forall type when type arguments are being applied"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.NotAFunctionTypeError")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Record(Vec::from([
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("type")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Type"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The actual type encountered"))))))))])})))))}))])))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A type that is not a function type when one was expected in an application"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.OtherError")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Wrap(crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Literal(crate::hydra::core::LiteralType(Rc::new(crate::hydra::core::LiteralType_Variant::String)))))))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("Any other error"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.TypeArityMismatchError")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Record(Vec::from([
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("type")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Type"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The type being checked"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("expectedArity")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Literal(crate::hydra::core::LiteralType(Rc::new(crate::hydra::core::LiteralType_Variant::Integer(crate::hydra::core::IntegerType(Rc::new(crate::hydra::core::IntegerType_Variant::Int32)))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The expected number of type arguments"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("actualArity")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Literal(crate::hydra::core::LiteralType(Rc::new(crate::hydra::core::LiteralType_Variant::Integer(crate::hydra::core::IntegerType(Rc::new(crate::hydra::core::IntegerType_Variant::Int32)))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The actual number of type arguments provided"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("typeArguments")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::List(crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Type")))))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The type arguments that were provided"))))))))])})))))}))])))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A type constructor applied to the wrong number of type arguments"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.TypeMismatchError")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Record(Vec::from([
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("expectedType")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Type"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The expected type"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("actualType")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Type"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The actual type encountered"))))))))])})))))}))])))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A type mismatch between expected and actual types"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.UnboundTypeVariablesError")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Record(Vec::from([
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("variables")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Set(crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Name")))))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The set of unbound type variable names"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("type")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Type"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The type containing the unbound variables"))))))))])})))))}))])))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("Type variables that appear free in a type but are not bound in scope"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.UndefinedFieldError")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Record(Vec::from([
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("fieldName")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Name"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The name of the undefined field"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("typeName")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Name"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The name of the type in which the field was expected"))))))))])})))))}))])))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A reference to a field that does not exist in the given type"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.UndefinedTermError")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Record(Vec::from([
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("name")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Name"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The name of the undefined term"))))))))])})))))}))])))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A reference to a term (element, binding, or primitive) that is not defined"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.UndefinedTypeError")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Record(Vec::from([
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("name")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Name"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The name of the undefined type"))))))))])})))))}))])))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A reference to a type or type variable that is not defined"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.UnequalTypesError")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Record(Vec::from([
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("types")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::List(crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Type")))))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The list of types that are not all equal"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Literal(crate::hydra::core::LiteralType(Rc::new(crate::hydra::core::LiteralType_Variant::String))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A description of the context in which the types were expected to be equal"))))))))])})))))}))])))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("Multiple types that should all be equal but are not"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.UnexpectedTermVariantError")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Record(Vec::from([
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("expectedVariant")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.variants.TermVariant"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The expected term variant"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("actualTerm")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The actual term that was encountered"))))))))])})))))}))])))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("An unexpected term variant was encountered"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.UnexpectedTypeVariantError")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Record(Vec::from([
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("expectedVariant")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.variants.TypeVariant"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The expected type variant"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("actualType")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Type"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The actual type that was encountered"))))))))])})))))}))])))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("An unexpected type variant was encountered"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.UnificationError")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Record(Vec::from([
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("leftType")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Type"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The left-hand type in the unification"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("rightType")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Type"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The right-hand type in the unification"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("message")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Literal(crate::hydra::core::LiteralType(Rc::new(crate::hydra::core::LiteralType_Variant::String))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A human-readable error message"))))))))])})))))}))])))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("An error that occurred during type unification"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.UnsupportedTermVariantError")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Record(Vec::from([
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("termVariant")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.variants.TermVariant"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The unsupported term variant"))))))))])})))))}))])))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A term variant that the type checker does not support"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.UntypedLambdaError")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Record(Vec::from([])))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A lambda expression without a type annotation on its parameter"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.UntypedLetBindingError")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Record(Vec::from([
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("binding")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Binding"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The untyped binding"))))))))])})))))}))])))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A let binding without a type annotation"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.graph.Graph")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Record(Vec::from([
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("boundTerms")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Map(crate::hydra::core::MapType(Rc::new(crate::hydra::core::MapType_Variant {
              keys: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Name"))))))),
              values: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term")))))))}))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The terms bound by all term variables in scope"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("boundTypes")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Map(crate::hydra::core::MapType(Rc::new(crate::hydra::core::MapType_Variant {
              keys: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Name"))))))),
              values: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.TypeScheme")))))))}))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The type schemes of all term variables in scope"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("classConstraints")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Map(crate::hydra::core::MapType(Rc::new(crate::hydra::core::MapType_Variant {
              keys: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Name"))))))),
              values: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.TypeVariableMetadata")))))))}))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A mutable map from type variable names to their accumulated class constraints. This is populated during type inference when operations requiring Eq or Ord are encountered."))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("lambdaVariables")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Set(crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Name")))))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The set of term variables introduced by specifically by lambdas"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("metadata")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Map(crate::hydra::core::MapType(Rc::new(crate::hydra::core::MapType_Variant {
              keys: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Name"))))))),
              values: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term")))))))}))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("Any additional metadata bound to term variables in scope"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("primitives")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Map(crate::hydra::core::MapType(Rc::new(crate::hydra::core::MapType_Variant {
              keys: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Name"))))))),
              values: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.graph.Primitive")))))))}))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("All primitive functions and constants by name"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("schemaTypes")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Map(crate::hydra::core::MapType(Rc::new(crate::hydra::core::MapType_Variant {
              keys: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Name"))))))),
              values: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.TypeScheme")))))))}))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("All schema types (type schemes) in scope"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("typeVariables")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Set(crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Name")))))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The set of type variables introduced specifically by type lambdas"))))))))])})))))}))])))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A graph, or lexical environment which binds names to terms, types, primitives, and metadata"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.graph.Primitive")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Record(Vec::from([
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("name")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Name"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The unique name of the primitive function"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("type")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.TypeScheme"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The type signature of the primitive function"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("implementation")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Function(crate::hydra::core::FunctionType(Rc::new(crate::hydra::core::FunctionType_Variant {
              domain: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.context.Context"))))))),
              codomain: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Function(crate::hydra::core::FunctionType(Rc::new(crate::hydra::core::FunctionType_Variant {
                domain: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.graph.Graph"))))))),
                codomain: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Function(crate::hydra::core::FunctionType(Rc::new(crate::hydra::core::FunctionType_Variant {
                  domain: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::List(crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term")))))))))),
                  codomain: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Either(crate::hydra::core::EitherType(Rc::new(crate::hydra::core::EitherType_Variant {
                    left: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Application(crate::hydra::core::ApplicationType(Rc::new(crate::hydra::core::ApplicationType_Variant {
                      function: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.context.InContext"))))))),
                      argument: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.Error")))))))}))))),
                    right: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term")))))))})))))})))))})))))}))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A concrete implementation of the primitive function. The Context and Graph parameters are needed by higher-order primitives (e.g. lists.map, lists.foldl, eithers.bind) which must evaluate function arguments via term reduction; the Graph provides variable and primitive bindings, while the Context supports tracing and error reporting."))))))))])})))))}))])))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A built-in function or constant"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.graph.TermCoder")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Forall(crate::hydra::core::ForallType(Rc::new(crate::hydra::core::ForallType_Variant {
        parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("a")))),
        body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Record(Vec::from([
          crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
            name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("type")))),
            type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
              body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Type"))))))),
              annotation: BTreeMap::from([
                (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The Hydra type of encoded terms"))))))))])})))))})),
          crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
            name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("encode")))),
            type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
              body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Function(crate::hydra::core::FunctionType(Rc::new(crate::hydra::core::FunctionType_Variant {
                domain: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.context.Context"))))))),
                codomain: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Function(crate::hydra::core::FunctionType(Rc::new(crate::hydra::core::FunctionType_Variant {
                  domain: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.graph.Graph"))))))),
                  codomain: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Function(crate::hydra::core::FunctionType(Rc::new(crate::hydra::core::FunctionType_Variant {
                    domain: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term"))))))),
                    codomain: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Either(crate::hydra::core::EitherType(Rc::new(crate::hydra::core::EitherType_Variant {
                      left: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Application(crate::hydra::core::ApplicationType(Rc::new(crate::hydra::core::ApplicationType_Variant {
                        function: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.context.InContext"))))))),
                        argument: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.Error")))))))}))))),
                      right: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("a")))))))})))))})))))})))))}))))),
              annotation: BTreeMap::from([
                (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("An encode function from terms to native values"))))))))])})))))})),
          crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
            name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("decode")))),
            type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
              body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Function(crate::hydra::core::FunctionType(Rc::new(crate::hydra::core::FunctionType_Variant {
                domain: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.context.Context"))))))),
                codomain: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Function(crate::hydra::core::FunctionType(Rc::new(crate::hydra::core::FunctionType_Variant {
                  domain: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("a"))))))),
                  codomain: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Either(crate::hydra::core::EitherType(Rc::new(crate::hydra::core::EitherType_Variant {
                    left: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Application(crate::hydra::core::ApplicationType(Rc::new(crate::hydra::core::ApplicationType_Variant {
                      function: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.context.InContext"))))))),
                      argument: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.Error")))))))}))))),
                    right: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term")))))))})))))})))))}))))),
              annotation: BTreeMap::from([
                (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A decode function from native values to terms"))))))))])})))))}))]))))}))))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A type together with a coder for mapping terms into arguments for primitive functions, and mapping computed results into terms."))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.module.Definition")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Union(Vec::from([
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("term")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.module.TermDefinition"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A term definition"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("type")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.module.TypeDefinition"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A type definition"))))))))])})))))}))])))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A definition, which may be either a term or type definition"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.module.FileExtension")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Wrap(crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Literal(crate::hydra::core::LiteralType(Rc::new(crate::hydra::core::LiteralType_Variant::String)))))))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A file extension (without the dot), e.g. \"json\" or \"py\""))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.module.Library")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Record(Vec::from([
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("namespace")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.module.Namespace"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A common prefix for all primitive function names in the library"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("prefix")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Literal(crate::hydra::core::LiteralType(Rc::new(crate::hydra::core::LiteralType_Variant::String))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A preferred namespace prefix for function names in the library"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("primitives")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::List(crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.graph.Primitive")))))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The primitives defined in this library"))))))))])})))))}))])))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A library of primitive functions"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.module.Module")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Record(Vec::from([
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("namespace")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.module.Namespace"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A common prefix for all element names in the module"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("elements")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::List(crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Binding")))))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The elements defined in this module"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("termDependencies")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::List(crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.module.Namespace")))))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("Any modules which the term expressions of this module directly depend upon"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("typeDependencies")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::List(crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.module.Namespace")))))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("Any modules which the type expressions of this module directly depend upon"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Maybe(crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Literal(crate::hydra::core::LiteralType(Rc::new(crate::hydra::core::LiteralType_Variant::String)))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("An optional human-readable description of the module"))))))))])})))))}))])))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A logical collection of elements in the same namespace, having dependencies on zero or more other modules"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.module.Namespace")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Wrap(crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Literal(crate::hydra::core::LiteralType(Rc::new(crate::hydra::core::LiteralType_Variant::String)))))))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A prefix for element names"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.module.Namespaces")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Forall(crate::hydra::core::ForallType(Rc::new(crate::hydra::core::ForallType_Variant {
        parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("n")))),
        body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Record(Vec::from([
          crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
            name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("focus")))),
            type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
              body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Pair(crate::hydra::core::PairType(Rc::new(crate::hydra::core::PairType_Variant {
                first: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.module.Namespace"))))))),
                second: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("n")))))))}))))),
              annotation: BTreeMap::from([
                (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The namespace in focus, together with its associated value"))))))))])})))))})),
          crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
            name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("mapping")))),
            type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
              body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Map(crate::hydra::core::MapType(Rc::new(crate::hydra::core::MapType_Variant {
                keys: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.module.Namespace"))))))),
                values: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("n")))))))}))))),
              annotation: BTreeMap::from([
                (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A mapping of namespaces to values"))))))))])})))))}))]))))}))))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A mapping from namespaces to values of type n, with a focus on one namespace"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.module.QualifiedName")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Record(Vec::from([
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("namespace")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Maybe(crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.module.Namespace")))))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The optional namespace"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("local")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Literal(crate::hydra::core::LiteralType(Rc::new(crate::hydra::core::LiteralType_Variant::String))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The local name"))))))))])})))))}))])))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A qualified name consisting of an optional namespace together with a mandatory local name"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.module.TermDefinition")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Record(Vec::from([
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("name")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Name"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The name of the term"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("term")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The term being defined"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("type")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.TypeScheme"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The type scheme of the term, including any class constraints"))))))))])})))))}))])))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A term-level definition, including a name, a term, and the type scheme of the term"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.module.TypeDefinition")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Record(Vec::from([
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("name")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Name"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The name of the type"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("type")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Type"))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The type being defined"))))))))])})))))}))])))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A type-level definition, including a name and the type"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.util.Adapter")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Forall(crate::hydra::core::ForallType(Rc::new(crate::hydra::core::ForallType_Variant {
        parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("t1")))),
        body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Forall(crate::hydra::core::ForallType(Rc::new(crate::hydra::core::ForallType_Variant {
          parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("t2")))),
          body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Forall(crate::hydra::core::ForallType(Rc::new(crate::hydra::core::ForallType_Variant {
            parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("v1")))),
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Forall(crate::hydra::core::ForallType(Rc::new(crate::hydra::core::ForallType_Variant {
              parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("v2")))),
              body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Record(Vec::from([
                crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
                  name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("isLossy")))),
                  type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
                    body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Literal(crate::hydra::core::LiteralType(Rc::new(crate::hydra::core::LiteralType_Variant::Boolean))))),
                    annotation: BTreeMap::from([
                      (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("Whether information may be lost in the course of this adaptation"))))))))])})))))})),
                crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
                  name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("source")))),
                  type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
                    body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("t1"))))))),
                    annotation: BTreeMap::from([
                      (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The source type"))))))))])})))))})),
                crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
                  name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("target")))),
                  type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
                    body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("t2"))))))),
                    annotation: BTreeMap::from([
                      (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The target type"))))))))])})))))})),
                crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
                  name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("coder")))),
                  type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
                    body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Application(crate::hydra::core::ApplicationType(Rc::new(crate::hydra::core::ApplicationType_Variant {
                      function: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Application(crate::hydra::core::ApplicationType(Rc::new(crate::hydra::core::ApplicationType_Variant {
                        function: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.util.Coder"))))))),
                        argument: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("v1")))))))}))))),
                      argument: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("v2")))))))}))))),
                    annotation: BTreeMap::from([
                      (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("The coder for transforming instances of the source type to instances of the target type"))))))))])})))))}))]))))})))))})))))})))))}))))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A two-level bidirectional encoder which adapts types to types and terms to terms"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.util.Bicoder")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Forall(crate::hydra::core::ForallType(Rc::new(crate::hydra::core::ForallType_Variant {
        parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("t1")))),
        body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Forall(crate::hydra::core::ForallType(Rc::new(crate::hydra::core::ForallType_Variant {
          parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("t2")))),
          body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Forall(crate::hydra::core::ForallType(Rc::new(crate::hydra::core::ForallType_Variant {
            parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("v1")))),
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Forall(crate::hydra::core::ForallType(Rc::new(crate::hydra::core::ForallType_Variant {
              parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("v2")))),
              body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Record(Vec::from([
                crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
                  name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("encode")))),
                  type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
                    body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Function(crate::hydra::core::FunctionType(Rc::new(crate::hydra::core::FunctionType_Variant {
                      domain: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("t1"))))))),
                      codomain: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Application(crate::hydra::core::ApplicationType(Rc::new(crate::hydra::core::ApplicationType_Variant {
                        function: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Application(crate::hydra::core::ApplicationType(Rc::new(crate::hydra::core::ApplicationType_Variant {
                          function: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Application(crate::hydra::core::ApplicationType(Rc::new(crate::hydra::core::ApplicationType_Variant {
                            function: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Application(crate::hydra::core::ApplicationType(Rc::new(crate::hydra::core::ApplicationType_Variant {
                              function: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.util.Adapter"))))))),
                              argument: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("t1")))))))}))))),
                            argument: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("t2")))))))}))))),
                          argument: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("v1")))))))}))))),
                        argument: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("v2")))))))})))))}))))),
                    annotation: BTreeMap::from([
                      (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A function from source types to adapters"))))))))])})))))})),
                crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
                  name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("decode")))),
                  type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
                    body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Function(crate::hydra::core::FunctionType(Rc::new(crate::hydra::core::FunctionType_Variant {
                      domain: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("t2"))))))),
                      codomain: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Application(crate::hydra::core::ApplicationType(Rc::new(crate::hydra::core::ApplicationType_Variant {
                        function: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Application(crate::hydra::core::ApplicationType(Rc::new(crate::hydra::core::ApplicationType_Variant {
                          function: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Application(crate::hydra::core::ApplicationType(Rc::new(crate::hydra::core::ApplicationType_Variant {
                            function: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Application(crate::hydra::core::ApplicationType(Rc::new(crate::hydra::core::ApplicationType_Variant {
                              function: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.util.Adapter"))))))),
                              argument: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("t2")))))))}))))),
                            argument: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("t1")))))))}))))),
                          argument: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("v2")))))))}))))),
                        argument: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("v1")))))))})))))}))))),
                    annotation: BTreeMap::from([
                      (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A function from target types to adapters"))))))))])})))))}))]))))})))))})))))})))))}))))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A two-level encoder and decoder, operating both at a type level and an instance (data) level"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.util.CaseConvention")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Union(Vec::from([
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("camel")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Unit))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("pascal")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Unit))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("lowerSnake")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Unit))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("upperSnake")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Unit))}))])))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A naming convention for symbols, such as camelCase or snake_case"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.util.Coder")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Forall(crate::hydra::core::ForallType(Rc::new(crate::hydra::core::ForallType_Variant {
        parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("v1")))),
        body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Forall(crate::hydra::core::ForallType(Rc::new(crate::hydra::core::ForallType_Variant {
          parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("v2")))),
          body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Record(Vec::from([
            crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
              name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("encode")))),
              type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
                body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Function(crate::hydra::core::FunctionType(Rc::new(crate::hydra::core::FunctionType_Variant {
                  domain: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.context.Context"))))))),
                  codomain: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Function(crate::hydra::core::FunctionType(Rc::new(crate::hydra::core::FunctionType_Variant {
                    domain: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("v1"))))))),
                    codomain: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Either(crate::hydra::core::EitherType(Rc::new(crate::hydra::core::EitherType_Variant {
                      left: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Application(crate::hydra::core::ApplicationType(Rc::new(crate::hydra::core::ApplicationType_Variant {
                        function: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.context.InContext"))))))),
                        argument: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.Error")))))))}))))),
                      right: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("v2")))))))})))))})))))}))))),
                annotation: BTreeMap::from([
                  (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A function which encodes source values as target values in a given context"))))))))])})))))})),
            crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
              name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("decode")))),
              type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
                body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Function(crate::hydra::core::FunctionType(Rc::new(crate::hydra::core::FunctionType_Variant {
                  domain: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.context.Context"))))))),
                  codomain: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Function(crate::hydra::core::FunctionType(Rc::new(crate::hydra::core::FunctionType_Variant {
                    domain: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("v2"))))))),
                    codomain: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Either(crate::hydra::core::EitherType(Rc::new(crate::hydra::core::EitherType_Variant {
                      left: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Application(crate::hydra::core::ApplicationType(Rc::new(crate::hydra::core::ApplicationType_Variant {
                        function: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.context.InContext"))))))),
                        argument: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.Error")))))))}))))),
                      right: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("v1")))))))})))))})))))}))))),
                annotation: BTreeMap::from([
                  (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("A function which decodes target values as source values in a given context"))))))))])})))))}))]))))})))))}))))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("An encoder and decoder; a bidirectional transformation between two types"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.util.Comparison")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Union(Vec::from([
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("lessThan")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Unit))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("equalTo")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Unit))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("greaterThan")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Unit))}))])))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("An equality judgement: less than, equal to, or greater than"))))))))])})))))),
    (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.util.Precision")))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
      body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Union(Vec::from([
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("arbitrary")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Unit)),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("Arbitrary precision"))))))))])})))))})),
        crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("bits")))),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Literal(crate::hydra::core::LiteralType(Rc::new(crate::hydra::core::LiteralType_Variant::Integer(crate::hydra::core::IntegerType(Rc::new(crate::hydra::core::IntegerType_Variant::Int32)))))))),
            annotation: BTreeMap::from([
              (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("Precision to a specified number of bits"))))))))])})))))}))])))),
      annotation: BTreeMap::from([
        (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("Numeric precision: arbitrary precision, or precision to a specified number of bits"))))))))])}))))))])}
