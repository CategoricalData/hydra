package hydra.coders

import hydra.core.*

import hydra.graph.*

import hydra.util.*

import hydra.variants.*

import hydra.core

import hydra.graph

import hydra.util

import hydra.variants

case class AdapterContext(graph: hydra.graph.Graph, language: hydra.coders.Language, adapters: Map[hydra.core.Name,
   hydra.util.Adapter[hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]])

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

type SymmetricAdapter [T, V] = hydra.util.Adapter[T, T, V, V]

enum TraversalOrder :
   case pre extends TraversalOrder
   case post extends TraversalOrder

type TypeAdapter = (hydra.coders.AdapterContext => hydra.core.Type => Either[scala.Predef.String, hydra.coders.SymmetricAdapter[hydra.core.Type,
   hydra.core.Term]])
