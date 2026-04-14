package hydra.graph

import hydra.context.*

import hydra.core.*

import hydra.errors.*

import hydra.context

import hydra.core

import hydra.errors

case class Graph(boundTerms: Map[hydra.core.Name, hydra.core.Term], boundTypes: Map[hydra.core.Name,
   hydra.core.TypeScheme], classConstraints: Map[hydra.core.Name, hydra.core.TypeVariableMetadata],
   lambdaVariables: scala.collection.immutable.Set[hydra.core.Name], metadata: Map[hydra.core.Name,
   hydra.core.Term], primitives: Map[hydra.core.Name, hydra.graph.Primitive], schemaTypes: Map[hydra.core.Name,
   hydra.core.TypeScheme], typeVariables: scala.collection.immutable.Set[hydra.core.Name])

case class Primitive(name: hydra.core.Name, `type`: hydra.core.TypeScheme, implementation: (hydra.context.Context => hydra.graph.Graph => Seq[hydra.core.Term] => Either[hydra.errors.Error,
   hydra.core.Term]))

case class TermCoder[A](`type`: hydra.core.Type, encode: (hydra.context.Context => hydra.graph.Graph => hydra.core.Term => Either[hydra.errors.Error,
   A]), decode: (hydra.context.Context => A => Either[hydra.errors.Error, hydra.core.Term]))
