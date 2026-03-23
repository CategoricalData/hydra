package hydra.module

import hydra.core.*

import hydra.graph.*

import hydra.core

import hydra.graph

enum Definition :
   case term(value: hydra.module.TermDefinition) extends Definition
   case `type`(value: hydra.module.TypeDefinition) extends Definition

type FileExtension = scala.Predef.String

case class Library(namespace: hydra.module.Namespace, prefix: scala.Predef.String, primitives: Seq[hydra.graph.Primitive])

case class Module(namespace: hydra.module.Namespace, elements: Seq[hydra.core.Binding], termDependencies: Seq[hydra.module.Namespace], typeDependencies: Seq[hydra.module.Namespace], description: Option[scala.Predef.String])

type Namespace = scala.Predef.String

case class Namespaces[N](focus: Tuple2[hydra.module.Namespace, N], mapping: Map[hydra.module.Namespace, N])

case class QualifiedName(namespace: Option[hydra.module.Namespace], local: scala.Predef.String)

case class TermDefinition(name: hydra.core.Name, term: hydra.core.Term, `type`: hydra.core.TypeScheme)

case class TypeDefinition(name: hydra.core.Name, `type`: hydra.core.Type)
