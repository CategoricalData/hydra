package hydra.ext.java.environment

import hydra.core.*

import hydra.ext.java.syntax.*

import hydra.graph.*

import hydra.module.*

import hydra.core

import hydra.ext.java.syntax

import hydra.graph

import hydra.module

enum JavaSymbolClass :
   case constant extends JavaSymbolClass
   case nullaryFunction extends JavaSymbolClass
   case hoistedLambda(value: Int) extends JavaSymbolClass
   case unaryFunction extends JavaSymbolClass
   case localVariable extends JavaSymbolClass

case class JavaFeatures(supportsDiamondOperator: Boolean)

case class Aliases(currentNamespace: hydra.module.Namespace, packages: Map[hydra.module.Namespace, hydra.ext.java.syntax.PackageName], branchVars: scala.collection.immutable.Set[hydra.core.Name], recursiveVars: scala.collection.immutable.Set[hydra.core.Name], inScopeTypeParams: scala.collection.immutable.Set[hydra.core.Name], polymorphicLocals: scala.collection.immutable.Set[hydra.core.Name], inScopeJavaVars: scala.collection.immutable.Set[hydra.core.Name], varRenames: Map[hydra.core.Name, hydra.core.Name], lambdaVars: scala.collection.immutable.Set[hydra.core.Name], typeVarSubst: Map[hydra.core.Name, hydra.core.Name], trustedTypeVars: scala.collection.immutable.Set[hydra.core.Name], methodCodomain: Option[hydra.core.Type], thunkedVars: scala.collection.immutable.Set[hydra.core.Name])

case class JavaEnvironment(aliases: hydra.ext.java.environment.Aliases, graph: hydra.graph.Graph)
