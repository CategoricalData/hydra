package hydra.coders

import hydra.context.*

import hydra.core.*

import hydra.errors.*

import hydra.graph.*

import hydra.variants.*

import hydra.context

import hydra.core

import hydra.errors

import hydra.graph

import hydra.variants

case class Adapter[T1, T2, V1, V2](isLossy: Boolean, source: T1, target: T2, coder: hydra.coders.Coder[V1, V2])

case class AdapterContext(graph: hydra.graph.Graph, language: hydra.coders.Language, adapters: Map[hydra.core.Name,
   hydra.coders.Adapter[hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]])

case class Bicoder[T1, T2, V1, V2](encode: (T1 => hydra.coders.Adapter[T1, T2, V1, V2]), decode: (T2 => hydra.coders.Adapter[T2, T1, V2, V1]))

case class Coder[V1, V2](encode: (hydra.context.Context => V1 => Either[hydra.context.InContext[hydra.errors.Error],
   V2]), decode: (hydra.context.Context => V2 => Either[hydra.context.InContext[hydra.errors.Error], V1]))

enum CoderDirection :
   case encode extends CoderDirection
   case decode extends CoderDirection

case class Language(name: hydra.coders.LanguageName, constraints: hydra.coders.LanguageConstraints)

case class LanguageConstraints(eliminationVariants: scala.collection.immutable.Set[hydra.variants.EliminationVariant],
   literalVariants: scala.collection.immutable.Set[hydra.variants.LiteralVariant], floatTypes: scala.collection.immutable.Set[hydra.core.FloatType],
   functionVariants: scala.collection.immutable.Set[hydra.variants.FunctionVariant], integerTypes: scala.collection.immutable.Set[hydra.core.IntegerType],
   termVariants: scala.collection.immutable.Set[hydra.variants.TermVariant], typeVariants: scala.collection.immutable.Set[hydra.variants.TypeVariant],
   types: (hydra.core.Type => Boolean))

type LanguageName = scala.Predef.String

type SymmetricAdapter [T, V] = hydra.coders.Adapter[T, T, V, V]

enum TraversalOrder :
   case pre extends TraversalOrder
   case post extends TraversalOrder

type TypeAdapter = (hydra.coders.AdapterContext => hydra.core.Type => Either[scala.Predef.String, hydra.coders.SymmetricAdapter[hydra.core.Type,
   hydra.core.Term]])
