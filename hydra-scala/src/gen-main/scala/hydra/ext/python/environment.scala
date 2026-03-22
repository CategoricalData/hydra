package hydra.ext.python.environment

import hydra.core.*

import hydra.ext.python.syntax.*

import hydra.graph.*

import hydra.module.*

import hydra.core

import hydra.ext.python.syntax

import hydra.graph

import hydra.module

enum PythonVersion :
   case python310 extends PythonVersion
   case python312 extends PythonVersion

case class PythonEnvironment(namespaces: hydra.module.Namespaces[hydra.ext.python.syntax.DottedName],
   boundTypeVariables: Tuple2[Seq[hydra.core.Name], Map[hydra.core.Name, hydra.ext.python.syntax.Name]],
   graph: hydra.graph.Graph, nullaryBindings: scala.collection.immutable.Set[hydra.core.Name], version: hydra.ext.python.environment.PythonVersion,
   skipCasts: Boolean, inlineVariables: scala.collection.immutable.Set[hydra.core.Name])

case class PythonModuleMetadata(namespaces: hydra.module.Namespaces[hydra.ext.python.syntax.DottedName],
   typeVariables: scala.collection.immutable.Set[hydra.core.Name], usesAnnotated: Boolean, usesCallable: Boolean,
   usesCast: Boolean, usesLruCache: Boolean, usesTypeAlias: Boolean, usesDataclass: Boolean, usesDecimal: Boolean,
   usesEither: Boolean, usesEnum: Boolean, usesFrozenDict: Boolean, usesFrozenList: Boolean, usesGeneric: Boolean,
   usesJust: Boolean, usesLeft: Boolean, usesMaybe: Boolean, usesName: Boolean, usesNode: Boolean, usesNothing: Boolean,
   usesRight: Boolean, usesTypeVar: Boolean)

case class PyGraph(graph: hydra.graph.Graph, metadata: hydra.ext.python.environment.PythonModuleMetadata)
