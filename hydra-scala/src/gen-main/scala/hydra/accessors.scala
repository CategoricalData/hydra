package hydra.accessors

import hydra.core.*

import hydra.core

case class AccessorEdge(source: hydra.accessors.AccessorNode, path: hydra.accessors.AccessorPath, target: hydra.accessors.AccessorNode)

case class AccessorGraph(nodes: Seq[hydra.accessors.AccessorNode], edges: Seq[hydra.accessors.AccessorEdge])

case class AccessorNode(name: hydra.core.Name, label: scala.Predef.String, id: scala.Predef.String)

type AccessorPath = Seq[hydra.accessors.TermAccessor]

enum TermAccessor :
   case annotatedBody extends TermAccessor
   case applicationFunction extends TermAccessor
   case applicationArgument extends TermAccessor
   case lambdaBody extends TermAccessor
   case unionCasesDefault extends TermAccessor
   case unionCasesBranch(value: hydra.core.Name) extends TermAccessor
   case letBody extends TermAccessor
   case letBinding(value: hydra.core.Name) extends TermAccessor
   case listElement(value: Int) extends TermAccessor
   case mapKey(value: Int) extends TermAccessor
   case mapValue(value: Int) extends TermAccessor
   case maybeTerm extends TermAccessor
   case productTerm(value: Int) extends TermAccessor
   case recordField(value: hydra.core.Name) extends TermAccessor
   case setElement(value: Int) extends TermAccessor
   case sumTerm extends TermAccessor
   case typeLambdaBody extends TermAccessor
   case typeApplicationTerm extends TermAccessor
   case injectionTerm extends TermAccessor
   case wrappedTerm extends TermAccessor
