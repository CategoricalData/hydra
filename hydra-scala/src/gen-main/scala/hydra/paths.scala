package hydra.paths

import hydra.core.*

import hydra.core

case class SubtermEdge(source: hydra.paths.SubtermNode, path: hydra.paths.SubtermPath, target: hydra.paths.SubtermNode)

case class SubtermGraph(nodes: Seq[hydra.paths.SubtermNode], edges: Seq[hydra.paths.SubtermEdge])

case class SubtermNode(name: hydra.core.Name, label: scala.Predef.String, id: scala.Predef.String)

type SubtermPath = Seq[hydra.paths.SubtermStep]

enum SubtermStep :
   case annotatedBody extends SubtermStep
   case applicationFunction extends SubtermStep
   case applicationArgument extends SubtermStep
   case lambdaBody extends SubtermStep
   case unionCasesDefault extends SubtermStep
   case unionCasesBranch(value: hydra.core.Name) extends SubtermStep
   case letBody extends SubtermStep
   case letBinding(value: hydra.core.Name) extends SubtermStep
   case listElement(value: Int) extends SubtermStep
   case mapKey(value: Int) extends SubtermStep
   case mapValue(value: Int) extends SubtermStep
   case maybeTerm extends SubtermStep
   case productTerm(value: Int) extends SubtermStep
   case recordField(value: hydra.core.Name) extends SubtermStep
   case setElement(value: Int) extends SubtermStep
   case sumTerm extends SubtermStep
   case typeLambdaBody extends SubtermStep
   case typeApplicationTerm extends SubtermStep
   case injectionTerm extends SubtermStep
   case wrappedTerm extends SubtermStep

case class SubtypeEdge(source: hydra.paths.SubtypeNode, path: hydra.paths.SubtypePath, target: hydra.paths.SubtypeNode)

case class SubtypeGraph(nodes: Seq[hydra.paths.SubtypeNode], edges: Seq[hydra.paths.SubtypeEdge])

case class SubtypeNode(name: hydra.core.Name, label: scala.Predef.String, id: scala.Predef.String)

type SubtypePath = Seq[hydra.paths.SubtypeStep]

enum SubtypeStep :
   case annotatedBody extends SubtypeStep
   case applicationFunction extends SubtypeStep
   case applicationArgument extends SubtypeStep
   case eitherLeft extends SubtypeStep
   case eitherRight extends SubtypeStep
   case forallBody extends SubtypeStep
   case functionDomain extends SubtypeStep
   case functionCodomain extends SubtypeStep
   case listElement extends SubtypeStep
   case mapKeys extends SubtypeStep
   case mapValues extends SubtypeStep
   case maybeElement extends SubtypeStep
   case pairFirst extends SubtypeStep
   case pairSecond extends SubtypeStep
   case recordField(value: hydra.core.Name) extends SubtypeStep
   case setElement extends SubtypeStep
   case unionField(value: hydra.core.Name) extends SubtypeStep
   case wrappedType extends SubtypeStep
