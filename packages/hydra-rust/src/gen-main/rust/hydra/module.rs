#![allow(unused_imports)]
#![allow(non_camel_case_types)]

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::rc::Rc;
use ordered_float::OrderedFloat;
use crate::Either;
use crate::hydra::graph::*;
use crate::hydra::core::*;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Definition_Variant {
  Term(TermDefinition),
  Type(TypeDefinition)}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Definition (pub Rc<Definition_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct FileExtension_Variant (pub String);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct FileExtension (pub Rc<FileExtension_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Library_Variant {
  pub namespace: Namespace,
  pub prefix: String,
  pub primitives: Vec<crate::hydra::graph::Primitive>}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Library (pub Rc<Library_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Module_Variant {
  pub namespace: Namespace,
  pub elements: Vec<crate::hydra::core::Binding>,
  pub term_dependencies: Vec<Namespace>,
  pub type_dependencies: Vec<Namespace>,
  pub description: Option<String>}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Module (pub Rc<Module_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Namespace_Variant (pub String);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Namespace (pub Rc<Namespace_Variant>);

pub type Namespaces = ((Namespace, N), BTreeMap<Namespace, N>) ;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct QualifiedName_Variant {
  pub namespace: Option<Namespace>,
  pub local: String}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct QualifiedName (pub Rc<QualifiedName_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct TermDefinition_Variant {
  pub name: crate::hydra::core::Name,
  pub term: crate::hydra::core::Term,
  pub type_: crate::hydra::core::TypeScheme}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct TermDefinition (pub Rc<TermDefinition_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct TypeDefinition_Variant {
  pub name: crate::hydra::core::Name,
  pub type_: crate::hydra::core::Type}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct TypeDefinition (pub Rc<TypeDefinition_Variant>);
