package hydra.packaging

import hydra.core.*

import hydra.graph.*

import hydra.core

import hydra.graph

enum Definition :
   case term(value: hydra.packaging.TermDefinition) extends Definition
   case `type`(value: hydra.packaging.TypeDefinition) extends Definition

type FileExtension = scala.Predef.String

case class Library(namespace: hydra.packaging.Namespace, prefix: scala.Predef.String, primitives: Seq[hydra.graph.Primitive])

case class Module(namespace: hydra.packaging.Namespace, definitions: Seq[hydra.packaging.Definition],
   termDependencies: Seq[hydra.packaging.Namespace], typeDependencies: Seq[hydra.packaging.Namespace],
   description: Option[scala.Predef.String])

type Namespace = scala.Predef.String

case class Namespaces[N](focus: Tuple2[hydra.packaging.Namespace, N], mapping: Map[hydra.packaging.Namespace, N])

case class Package(name: hydra.packaging.PackageName, modules: Seq[hydra.packaging.Module], dependencies: Seq[hydra.packaging.PackageName],
   description: Option[scala.Predef.String])

type PackageName = scala.Predef.String

case class QualifiedName(namespace: Option[hydra.packaging.Namespace], local: scala.Predef.String)

case class TermDefinition(name: hydra.core.Name, term: hydra.core.Term, `type`: Option[hydra.core.TypeScheme])

case class TypeDefinition(name: hydra.core.Name, `type`: hydra.core.TypeScheme)
