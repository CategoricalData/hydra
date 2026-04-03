package hydra.show.paths

import hydra.core.*

import hydra.paths.*

import hydra.lib.lists

import hydra.lib.maps

import hydra.lib.maybes

import hydra.lib.pairs

import hydra.lib.sets

import hydra.lib.strings

def subtermStep(step: hydra.paths.SubtermStep): Option[scala.Predef.String] =
  {
  def idx[T0, T1](i: T0): Option[T1] = None
  def idxSuff[T0](suffix: scala.Predef.String)(i: T0): Option[scala.Predef.String] =
    hydra.lib.maybes.map[scala.Predef.String, scala.Predef.String]((s: scala.Predef.String) => hydra.lib.strings.cat2(s)(suffix))(idx(i))
  step match
    case hydra.paths.SubtermStep.annotatedBody => None
    case hydra.paths.SubtermStep.applicationFunction => Some("fun")
    case hydra.paths.SubtermStep.applicationArgument => Some("arg")
    case hydra.paths.SubtermStep.lambdaBody => Some("body")
    case hydra.paths.SubtermStep.unionCasesDefault => Some("default")
    case hydra.paths.SubtermStep.unionCasesBranch(v_SubtermStep_unionCasesBranch_name) => Some(hydra.lib.strings.cat2(".")(v_SubtermStep_unionCasesBranch_name))
    case hydra.paths.SubtermStep.letBody => Some("in")
    case hydra.paths.SubtermStep.letBinding(v_SubtermStep_letBinding_name) => Some(hydra.lib.strings.cat2(v_SubtermStep_letBinding_name)("="))
    case hydra.paths.SubtermStep.listElement(v_SubtermStep_listElement_i) => idx(v_SubtermStep_listElement_i)
    case hydra.paths.SubtermStep.mapKey(v_SubtermStep_mapKey_i) => idxSuff(".key")(v_SubtermStep_mapKey_i)
    case hydra.paths.SubtermStep.mapValue(v_SubtermStep_mapValue_i) => idxSuff(".value")(v_SubtermStep_mapValue_i)
    case hydra.paths.SubtermStep.maybeTerm => Some("just")
    case hydra.paths.SubtermStep.productTerm(v_SubtermStep_productTerm_i) => idx(v_SubtermStep_productTerm_i)
    case hydra.paths.SubtermStep.recordField(v_SubtermStep_recordField_name) => Some(hydra.lib.strings.cat2(".")(v_SubtermStep_recordField_name))
    case hydra.paths.SubtermStep.setElement(v_SubtermStep_setElement_i) => idx(v_SubtermStep_setElement_i)
    case hydra.paths.SubtermStep.sumTerm => None
    case hydra.paths.SubtermStep.typeLambdaBody => None
    case hydra.paths.SubtermStep.typeApplicationTerm => None
    case hydra.paths.SubtermStep.injectionTerm => None
    case hydra.paths.SubtermStep.wrappedTerm => None
}

def termToSubtermGraph(namespaces: Map[hydra.packaging.Namespace, scala.Predef.String])(term: hydra.core.Term): hydra.paths.SubtermGraph =
  {
  lazy val dontCareStep: hydra.paths.SubtermStep = hydra.paths.SubtermStep.annotatedBody
  def helper(ids: Map[hydra.core.Name, hydra.paths.SubtermNode])(mroot: Option[hydra.paths.SubtermNode])(path: Seq[hydra.paths.SubtermStep])(state: Tuple2[Tuple2[Seq[hydra.paths.SubtermNode],
     Seq[hydra.paths.SubtermEdge]], scala.collection.immutable.Set[scala.Predef.String]])(stepTerm: Tuple2[hydra.paths.SubtermStep,
     hydra.core.Term]): Tuple2[Tuple2[Seq[hydra.paths.SubtermNode], Seq[hydra.paths.SubtermEdge]], scala.collection.immutable.Set[scala.Predef.String]] =
    {
    lazy val step: hydra.paths.SubtermStep = hydra.lib.pairs.first[hydra.paths.SubtermStep, hydra.core.Term](stepTerm)
    lazy val currentTerm: hydra.core.Term = hydra.lib.pairs.second[hydra.paths.SubtermStep, hydra.core.Term](stepTerm)
    lazy val nodesEdges: Tuple2[Seq[hydra.paths.SubtermNode], Seq[hydra.paths.SubtermEdge]] = hydra.lib.pairs.first[Tuple2[Seq[hydra.paths.SubtermNode],
       Seq[hydra.paths.SubtermEdge]], scala.collection.immutable.Set[scala.Predef.String]](state)
    lazy val visited: scala.collection.immutable.Set[scala.Predef.String] = hydra.lib.pairs.second[Tuple2[Seq[hydra.paths.SubtermNode],
       Seq[hydra.paths.SubtermEdge]], scala.collection.immutable.Set[scala.Predef.String]](state)
    lazy val nodes: Seq[hydra.paths.SubtermNode] = hydra.lib.pairs.first[Seq[hydra.paths.SubtermNode], Seq[hydra.paths.SubtermEdge]](nodesEdges)
    lazy val edges: Seq[hydra.paths.SubtermEdge] = hydra.lib.pairs.second[Seq[hydra.paths.SubtermNode], Seq[hydra.paths.SubtermEdge]](nodesEdges)
    lazy val nextPath: Seq[hydra.paths.SubtermStep] = hydra.lib.lists.cons[hydra.paths.SubtermStep](step)(path)
    currentTerm match
      case hydra.core.Term.let(v_Term_let_letExpr) => {
        lazy val bindings: Seq[hydra.core.Binding] = (v_Term_let_letExpr.bindings)
        lazy val env: hydra.core.Term = (v_Term_let_letExpr.body)
        lazy val bindingNames: Seq[hydra.core.Name] = hydra.lib.lists.map[hydra.core.Binding, hydra.core.Name]((x: hydra.core.Binding) => (x.name))(bindings)
        def addBindingName(nodesVisitedIds: Tuple2[Tuple2[Seq[hydra.paths.SubtermNode], scala.collection.immutable.Set[scala.Predef.String]],
           Map[hydra.core.Name, hydra.paths.SubtermNode]])(name: hydra.core.Name): Tuple2[Tuple2[Seq[hydra.paths.SubtermNode],
           scala.collection.immutable.Set[scala.Predef.String]], Map[hydra.core.Name, hydra.paths.SubtermNode]] =
          {
          lazy val currentNodesVisited: Tuple2[Seq[hydra.paths.SubtermNode], scala.collection.immutable.Set[scala.Predef.String]] = hydra.lib.pairs.first[Tuple2[Seq[hydra.paths.SubtermNode],
             scala.collection.immutable.Set[scala.Predef.String]], Map[hydra.core.Name, hydra.paths.SubtermNode]](nodesVisitedIds)
          lazy val currentIds: Map[hydra.core.Name, hydra.paths.SubtermNode] = hydra.lib.pairs.second[Tuple2[Seq[hydra.paths.SubtermNode],
             scala.collection.immutable.Set[scala.Predef.String]], Map[hydra.core.Name, hydra.paths.SubtermNode]](nodesVisitedIds)
          lazy val currentNodes: Seq[hydra.paths.SubtermNode] = hydra.lib.pairs.first[Seq[hydra.paths.SubtermNode],
             scala.collection.immutable.Set[scala.Predef.String]](currentNodesVisited)
          lazy val currentVisited: scala.collection.immutable.Set[scala.Predef.String] = hydra.lib.pairs.second[Seq[hydra.paths.SubtermNode],
             scala.collection.immutable.Set[scala.Predef.String]](currentNodesVisited)
          lazy val rawLabel: scala.Predef.String = hydra.names.compactName(namespaces)(name)
          lazy val uniqueLabel: scala.Predef.String = hydra.names.uniqueLabel(currentVisited)(rawLabel)
          lazy val node: hydra.paths.SubtermNode = hydra.paths.SubtermNode(name, rawLabel, uniqueLabel)
          lazy val newVisited: scala.collection.immutable.Set[scala.Predef.String] = hydra.lib.sets.insert[scala.Predef.String](uniqueLabel)(currentVisited)
          lazy val newNodes: Seq[hydra.paths.SubtermNode] = hydra.lib.lists.cons[hydra.paths.SubtermNode](node)(currentNodes)
          lazy val newIds: Map[hydra.core.Name, hydra.paths.SubtermNode] = hydra.lib.maps.insert[hydra.core.Name,
             hydra.paths.SubtermNode](name)(node)(currentIds)
          Tuple2(Tuple2(newNodes, newVisited), newIds)
        }
        lazy val nodesVisitedIds1: Tuple2[Tuple2[Seq[hydra.paths.SubtermNode], scala.collection.immutable.Set[scala.Predef.String]],
           Map[hydra.core.Name, hydra.paths.SubtermNode]] = hydra.lib.lists.foldl[Tuple2[Tuple2[Seq[hydra.paths.SubtermNode],
           scala.collection.immutable.Set[scala.Predef.String]], Map[hydra.core.Name, hydra.paths.SubtermNode]],
           hydra.core.Name](addBindingName)(Tuple2(Tuple2(Seq(), visited), ids))(bindingNames)
        lazy val nodes1: Seq[hydra.paths.SubtermNode] = hydra.lib.pairs.first[Seq[hydra.paths.SubtermNode],
           scala.collection.immutable.Set[scala.Predef.String]](hydra.lib.pairs.first[Tuple2[Seq[hydra.paths.SubtermNode],
           scala.collection.immutable.Set[scala.Predef.String]], Map[hydra.core.Name, hydra.paths.SubtermNode]](nodesVisitedIds1))
        lazy val visited1: scala.collection.immutable.Set[scala.Predef.String] = hydra.lib.pairs.second[Seq[hydra.paths.SubtermNode],
           scala.collection.immutable.Set[scala.Predef.String]](hydra.lib.pairs.first[Tuple2[Seq[hydra.paths.SubtermNode],
           scala.collection.immutable.Set[scala.Predef.String]], Map[hydra.core.Name, hydra.paths.SubtermNode]](nodesVisitedIds1))
        lazy val ids1: Map[hydra.core.Name, hydra.paths.SubtermNode] = hydra.lib.pairs.second[Tuple2[Seq[hydra.paths.SubtermNode],
           scala.collection.immutable.Set[scala.Predef.String]], Map[hydra.core.Name, hydra.paths.SubtermNode]](nodesVisitedIds1)
        def addBindingTerm(currentState: Tuple2[Tuple2[Seq[hydra.paths.SubtermNode], Seq[hydra.paths.SubtermEdge]],
           scala.collection.immutable.Set[scala.Predef.String]])(nodeBinding: Tuple2[hydra.paths.SubtermNode,
           hydra.core.Binding]): Tuple2[Tuple2[Seq[hydra.paths.SubtermNode], Seq[hydra.paths.SubtermEdge]],
           scala.collection.immutable.Set[scala.Predef.String]] =
          {
          lazy val root: hydra.paths.SubtermNode = hydra.lib.pairs.first[hydra.paths.SubtermNode, hydra.core.Binding](nodeBinding)
          lazy val binding: hydra.core.Binding = hydra.lib.pairs.second[hydra.paths.SubtermNode, hydra.core.Binding](nodeBinding)
          lazy val term1: hydra.core.Term = (binding.term)
          helper(ids1)(Some(root))(Seq())(currentState)(Tuple2(dontCareStep, term1))
        }
        lazy val nodeBindingPairs: Seq[Tuple2[hydra.paths.SubtermNode, hydra.core.Binding]] = hydra.lib.lists.zip[hydra.paths.SubtermNode,
           hydra.core.Binding](nodes1)(bindings)
        lazy val stateAfterBindings: Tuple2[Tuple2[Seq[hydra.paths.SubtermNode], Seq[hydra.paths.SubtermEdge]],
           scala.collection.immutable.Set[scala.Predef.String]] = hydra.lib.lists.foldl[Tuple2[Tuple2[Seq[hydra.paths.SubtermNode],
           Seq[hydra.paths.SubtermEdge]], scala.collection.immutable.Set[scala.Predef.String]], Tuple2[hydra.paths.SubtermNode,
           hydra.core.Binding]](addBindingTerm)(Tuple2(Tuple2(hydra.lib.lists.concat2[hydra.paths.SubtermNode](nodes1)(nodes),
           edges), visited1))(nodeBindingPairs)
        helper(ids1)(mroot)(nextPath)(stateAfterBindings)(Tuple2(hydra.paths.SubtermStep.letBody, env))
      }
      case hydra.core.Term.variable(v_Term_variable_name) => hydra.lib.maybes.maybe[Tuple2[Tuple2[Seq[hydra.paths.SubtermNode],
         Seq[hydra.paths.SubtermEdge]], scala.collection.immutable.Set[scala.Predef.String]], hydra.paths.SubtermNode](state)((root: hydra.paths.SubtermNode) =>
        hydra.lib.maybes.maybe[Tuple2[Tuple2[Seq[hydra.paths.SubtermNode], Seq[hydra.paths.SubtermEdge]],
           scala.collection.immutable.Set[scala.Predef.String]], hydra.paths.SubtermNode](state)((node: hydra.paths.SubtermNode) =>
        {
        lazy val edge: hydra.paths.SubtermEdge = hydra.paths.SubtermEdge(root, hydra.lib.lists.reverse[hydra.paths.SubtermStep](nextPath), node)
        lazy val newEdges: Seq[hydra.paths.SubtermEdge] = hydra.lib.lists.cons[hydra.paths.SubtermEdge](edge)(edges)
        Tuple2(Tuple2(nodes, newEdges), visited)
      })(hydra.lib.maps.lookup[hydra.core.Name, hydra.paths.SubtermNode](v_Term_variable_name)(ids)))(mroot)
      case _ => hydra.lib.lists.foldl[Tuple2[Tuple2[Seq[hydra.paths.SubtermNode], Seq[hydra.paths.SubtermEdge]],
         scala.collection.immutable.Set[scala.Predef.String]], Tuple2[hydra.paths.SubtermStep, hydra.core.Term]]((v1: Tuple2[Tuple2[Seq[hydra.paths.SubtermNode],
         Seq[hydra.paths.SubtermEdge]], scala.collection.immutable.Set[scala.Predef.String]]) =>
        (v2: Tuple2[hydra.paths.SubtermStep, hydra.core.Term]) => helper(ids)(mroot)(nextPath)(v1)(v2))(state)(hydra.rewriting.subtermsWithSteps(currentTerm))
  }
  def initialState[T0, T1, T2]: Tuple2[Tuple2[Seq[T0], Seq[T1]], scala.collection.immutable.Set[T2]] = Tuple2(Tuple2(Seq(), Seq()), hydra.lib.sets.empty[T2])
  lazy val result: Tuple2[Tuple2[Seq[hydra.paths.SubtermNode], Seq[hydra.paths.SubtermEdge]], scala.collection.immutable.Set[scala.Predef.String]] = helper(hydra.lib.maps.empty[hydra.core.Name,
     hydra.paths.SubtermNode])(None)(Seq())(initialState)(Tuple2(dontCareStep, term))
  lazy val finalNodesEdges: Tuple2[Seq[hydra.paths.SubtermNode], Seq[hydra.paths.SubtermEdge]] = hydra.lib.pairs.first[Tuple2[Seq[hydra.paths.SubtermNode],
     Seq[hydra.paths.SubtermEdge]], scala.collection.immutable.Set[scala.Predef.String]](result)
  lazy val finalNodes: Seq[hydra.paths.SubtermNode] = hydra.lib.pairs.first[Seq[hydra.paths.SubtermNode], Seq[hydra.paths.SubtermEdge]](finalNodesEdges)
  lazy val finalEdges: Seq[hydra.paths.SubtermEdge] = hydra.lib.pairs.second[Seq[hydra.paths.SubtermNode], Seq[hydra.paths.SubtermEdge]](finalNodesEdges)
  hydra.paths.SubtermGraph(finalNodes, finalEdges)
}
