package hydra.show.accessors

import hydra.accessors.*

import hydra.core.*

import hydra.lib.lists

import hydra.lib.maps

import hydra.lib.maybes

import hydra.lib.pairs

import hydra.lib.sets

import hydra.lib.strings

def termAccessor(accessor: hydra.accessors.TermAccessor): Option[scala.Predef.String] =
  {
  def idx[T0, T1](i: T0): Option[T1] = None
  def idxSuff[T0](suffix: scala.Predef.String)(i: T0): Option[scala.Predef.String] =
    hydra.lib.maybes.map[scala.Predef.String, scala.Predef.String]((s: scala.Predef.String) => hydra.lib.strings.cat2(s)(suffix))(idx(i))
  accessor match
    case hydra.accessors.TermAccessor.annotatedBody => None
    case hydra.accessors.TermAccessor.applicationFunction => Some("fun")
    case hydra.accessors.TermAccessor.applicationArgument => Some("arg")
    case hydra.accessors.TermAccessor.lambdaBody => Some("body")
    case hydra.accessors.TermAccessor.unionCasesDefault => Some("default")
    case hydra.accessors.TermAccessor.unionCasesBranch(v_TermAccessor_unionCasesBranch_name) => Some(hydra.lib.strings.cat2(".")(v_TermAccessor_unionCasesBranch_name))
    case hydra.accessors.TermAccessor.letBody => Some("in")
    case hydra.accessors.TermAccessor.letBinding(v_TermAccessor_letBinding_name) => Some(hydra.lib.strings.cat2(v_TermAccessor_letBinding_name)("="))
    case hydra.accessors.TermAccessor.listElement(v_TermAccessor_listElement_i) => idx(v_TermAccessor_listElement_i)
    case hydra.accessors.TermAccessor.mapKey(v_TermAccessor_mapKey_i) => idxSuff(".key")(v_TermAccessor_mapKey_i)
    case hydra.accessors.TermAccessor.mapValue(v_TermAccessor_mapValue_i) => idxSuff(".value")(v_TermAccessor_mapValue_i)
    case hydra.accessors.TermAccessor.maybeTerm => Some("just")
    case hydra.accessors.TermAccessor.productTerm(v_TermAccessor_productTerm_i) => idx(v_TermAccessor_productTerm_i)
    case hydra.accessors.TermAccessor.recordField(v_TermAccessor_recordField_name) => Some(hydra.lib.strings.cat2(".")(v_TermAccessor_recordField_name))
    case hydra.accessors.TermAccessor.setElement(v_TermAccessor_setElement_i) => idx(v_TermAccessor_setElement_i)
    case hydra.accessors.TermAccessor.sumTerm => None
    case hydra.accessors.TermAccessor.typeLambdaBody => None
    case hydra.accessors.TermAccessor.typeApplicationTerm => None
    case hydra.accessors.TermAccessor.injectionTerm => None
    case hydra.accessors.TermAccessor.wrappedTerm => None
}

def termToAccessorGraph(namespaces: Map[hydra.module.Namespace, scala.Predef.String])(term: hydra.core.Term): hydra.accessors.AccessorGraph =
  {
  lazy val dontCareAccessor: hydra.accessors.TermAccessor = hydra.accessors.TermAccessor.annotatedBody
  def helper(ids: Map[hydra.core.Name, hydra.accessors.AccessorNode])(mroot: Option[hydra.accessors.AccessorNode])(path: Seq[hydra.accessors.TermAccessor])(state: Tuple2[Tuple2[Seq[hydra.accessors.AccessorNode], Seq[hydra.accessors.AccessorEdge]], scala.collection.immutable.Set[scala.Predef.String]])(accessorTerm: Tuple2[hydra.accessors.TermAccessor, hydra.core.Term]): Tuple2[Tuple2[Seq[hydra.accessors.AccessorNode], Seq[hydra.accessors.AccessorEdge]], scala.collection.immutable.Set[scala.Predef.String]] =
    {
    lazy val accessor: hydra.accessors.TermAccessor = hydra.lib.pairs.first[hydra.accessors.TermAccessor, hydra.core.Term](accessorTerm)
    lazy val currentTerm: hydra.core.Term = hydra.lib.pairs.second[hydra.accessors.TermAccessor, hydra.core.Term](accessorTerm)
    lazy val nodesEdges: Tuple2[Seq[hydra.accessors.AccessorNode], Seq[hydra.accessors.AccessorEdge]] = hydra.lib.pairs.first[Tuple2[Seq[hydra.accessors.AccessorNode], Seq[hydra.accessors.AccessorEdge]], scala.collection.immutable.Set[scala.Predef.String]](state)
    lazy val visited: scala.collection.immutable.Set[scala.Predef.String] = hydra.lib.pairs.second[Tuple2[Seq[hydra.accessors.AccessorNode], Seq[hydra.accessors.AccessorEdge]], scala.collection.immutable.Set[scala.Predef.String]](state)
    lazy val nodes: Seq[hydra.accessors.AccessorNode] = hydra.lib.pairs.first[Seq[hydra.accessors.AccessorNode], Seq[hydra.accessors.AccessorEdge]](nodesEdges)
    lazy val edges: Seq[hydra.accessors.AccessorEdge] = hydra.lib.pairs.second[Seq[hydra.accessors.AccessorNode], Seq[hydra.accessors.AccessorEdge]](nodesEdges)
    lazy val nextPath: Seq[hydra.accessors.TermAccessor] = hydra.lib.lists.cons[hydra.accessors.TermAccessor](accessor)(path)
    currentTerm match
      case hydra.core.Term.let(v_Term_let_letExpr) => {
        lazy val bindings: Seq[hydra.core.Binding] = (v_Term_let_letExpr.bindings)
        lazy val env: hydra.core.Term = (v_Term_let_letExpr.body)
        lazy val bindingNames: Seq[hydra.core.Name] = hydra.lib.lists.map[hydra.core.Binding, hydra.core.Name]((x: hydra.core.Binding) => (x.name))(bindings)
        def addBindingName(nodesVisitedIds: Tuple2[Tuple2[Seq[hydra.accessors.AccessorNode], scala.collection.immutable.Set[scala.Predef.String]], Map[hydra.core.Name, hydra.accessors.AccessorNode]])(name: hydra.core.Name): Tuple2[Tuple2[Seq[hydra.accessors.AccessorNode], scala.collection.immutable.Set[scala.Predef.String]], Map[hydra.core.Name, hydra.accessors.AccessorNode]] =
          {
          lazy val currentNodesVisited: Tuple2[Seq[hydra.accessors.AccessorNode], scala.collection.immutable.Set[scala.Predef.String]] = hydra.lib.pairs.first[Tuple2[Seq[hydra.accessors.AccessorNode], scala.collection.immutable.Set[scala.Predef.String]], Map[hydra.core.Name, hydra.accessors.AccessorNode]](nodesVisitedIds)
          lazy val currentIds: Map[hydra.core.Name, hydra.accessors.AccessorNode] = hydra.lib.pairs.second[Tuple2[Seq[hydra.accessors.AccessorNode], scala.collection.immutable.Set[scala.Predef.String]], Map[hydra.core.Name, hydra.accessors.AccessorNode]](nodesVisitedIds)
          lazy val currentNodes: Seq[hydra.accessors.AccessorNode] = hydra.lib.pairs.first[Seq[hydra.accessors.AccessorNode], scala.collection.immutable.Set[scala.Predef.String]](currentNodesVisited)
          lazy val currentVisited: scala.collection.immutable.Set[scala.Predef.String] = hydra.lib.pairs.second[Seq[hydra.accessors.AccessorNode], scala.collection.immutable.Set[scala.Predef.String]](currentNodesVisited)
          lazy val rawLabel: scala.Predef.String = hydra.names.compactName(namespaces)(name)
          lazy val uniqueLabel: scala.Predef.String = hydra.names.uniqueLabel(currentVisited)(rawLabel)
          lazy val node: hydra.accessors.AccessorNode = hydra.accessors.AccessorNode(name, rawLabel, uniqueLabel)
          lazy val newVisited: scala.collection.immutable.Set[scala.Predef.String] = hydra.lib.sets.insert[scala.Predef.String](uniqueLabel)(currentVisited)
          lazy val newNodes: Seq[hydra.accessors.AccessorNode] = hydra.lib.lists.cons[hydra.accessors.AccessorNode](node)(currentNodes)
          lazy val newIds: Map[hydra.core.Name, hydra.accessors.AccessorNode] = hydra.lib.maps.insert[hydra.core.Name, hydra.accessors.AccessorNode](name)(node)(currentIds)
          Tuple2(Tuple2(newNodes, newVisited), newIds)
        }
        lazy val nodesVisitedIds1: Tuple2[Tuple2[Seq[hydra.accessors.AccessorNode], scala.collection.immutable.Set[scala.Predef.String]], Map[hydra.core.Name, hydra.accessors.AccessorNode]] = hydra.lib.lists.foldl[Tuple2[Tuple2[Seq[hydra.accessors.AccessorNode], scala.collection.immutable.Set[scala.Predef.String]], Map[hydra.core.Name, hydra.accessors.AccessorNode]], hydra.core.Name](addBindingName)(Tuple2(Tuple2(Seq(), visited), ids))(bindingNames)
        lazy val nodes1: Seq[hydra.accessors.AccessorNode] = hydra.lib.pairs.first[Seq[hydra.accessors.AccessorNode], scala.collection.immutable.Set[scala.Predef.String]](hydra.lib.pairs.first[Tuple2[Seq[hydra.accessors.AccessorNode], scala.collection.immutable.Set[scala.Predef.String]], Map[hydra.core.Name, hydra.accessors.AccessorNode]](nodesVisitedIds1))
        lazy val visited1: scala.collection.immutable.Set[scala.Predef.String] = hydra.lib.pairs.second[Seq[hydra.accessors.AccessorNode], scala.collection.immutable.Set[scala.Predef.String]](hydra.lib.pairs.first[Tuple2[Seq[hydra.accessors.AccessorNode], scala.collection.immutable.Set[scala.Predef.String]], Map[hydra.core.Name, hydra.accessors.AccessorNode]](nodesVisitedIds1))
        lazy val ids1: Map[hydra.core.Name, hydra.accessors.AccessorNode] = hydra.lib.pairs.second[Tuple2[Seq[hydra.accessors.AccessorNode], scala.collection.immutable.Set[scala.Predef.String]], Map[hydra.core.Name, hydra.accessors.AccessorNode]](nodesVisitedIds1)
        def addBindingTerm(currentState: Tuple2[Tuple2[Seq[hydra.accessors.AccessorNode], Seq[hydra.accessors.AccessorEdge]], scala.collection.immutable.Set[scala.Predef.String]])(nodeBinding: Tuple2[hydra.accessors.AccessorNode, hydra.core.Binding]): Tuple2[Tuple2[Seq[hydra.accessors.AccessorNode], Seq[hydra.accessors.AccessorEdge]], scala.collection.immutable.Set[scala.Predef.String]] =
          {
          lazy val root: hydra.accessors.AccessorNode = hydra.lib.pairs.first[hydra.accessors.AccessorNode, hydra.core.Binding](nodeBinding)
          lazy val binding: hydra.core.Binding = hydra.lib.pairs.second[hydra.accessors.AccessorNode, hydra.core.Binding](nodeBinding)
          lazy val term1: hydra.core.Term = (binding.term)
          helper(ids1)(Some(root))(Seq())(currentState)(Tuple2(dontCareAccessor, term1))
        }
        lazy val nodeBindingPairs: Seq[Tuple2[hydra.accessors.AccessorNode, hydra.core.Binding]] = hydra.lib.lists.zip[hydra.accessors.AccessorNode, hydra.core.Binding](nodes1)(bindings)
        lazy val stateAfterBindings: Tuple2[Tuple2[Seq[hydra.accessors.AccessorNode], Seq[hydra.accessors.AccessorEdge]], scala.collection.immutable.Set[scala.Predef.String]] = hydra.lib.lists.foldl[Tuple2[Tuple2[Seq[hydra.accessors.AccessorNode], Seq[hydra.accessors.AccessorEdge]], scala.collection.immutable.Set[scala.Predef.String]], Tuple2[hydra.accessors.AccessorNode, hydra.core.Binding]](addBindingTerm)(Tuple2(Tuple2(hydra.lib.lists.concat2[hydra.accessors.AccessorNode](nodes1)(nodes), edges), visited1))(nodeBindingPairs)
        helper(ids1)(mroot)(nextPath)(stateAfterBindings)(Tuple2(hydra.accessors.TermAccessor.letBody, env))
      }
      case hydra.core.Term.variable(v_Term_variable_name) => hydra.lib.maybes.maybe[Tuple2[Tuple2[Seq[hydra.accessors.AccessorNode], Seq[hydra.accessors.AccessorEdge]], scala.collection.immutable.Set[scala.Predef.String]], hydra.accessors.AccessorNode](state)((root: hydra.accessors.AccessorNode) =>
        hydra.lib.maybes.maybe[Tuple2[Tuple2[Seq[hydra.accessors.AccessorNode], Seq[hydra.accessors.AccessorEdge]], scala.collection.immutable.Set[scala.Predef.String]], hydra.accessors.AccessorNode](state)((node: hydra.accessors.AccessorNode) =>
        {
        lazy val edge: hydra.accessors.AccessorEdge = hydra.accessors.AccessorEdge(root, hydra.lib.lists.reverse[hydra.accessors.TermAccessor](nextPath), node)
        lazy val newEdges: Seq[hydra.accessors.AccessorEdge] = hydra.lib.lists.cons[hydra.accessors.AccessorEdge](edge)(edges)
        Tuple2(Tuple2(nodes, newEdges), visited)
      })(hydra.lib.maps.lookup[hydra.core.Name, hydra.accessors.AccessorNode](v_Term_variable_name)(ids)))(mroot)
      case _ => hydra.lib.lists.foldl[Tuple2[Tuple2[Seq[hydra.accessors.AccessorNode], Seq[hydra.accessors.AccessorEdge]], scala.collection.immutable.Set[scala.Predef.String]], Tuple2[hydra.accessors.TermAccessor, hydra.core.Term]]((v1: Tuple2[Tuple2[Seq[hydra.accessors.AccessorNode], Seq[hydra.accessors.AccessorEdge]], scala.collection.immutable.Set[scala.Predef.String]]) =>
        (v2: Tuple2[hydra.accessors.TermAccessor, hydra.core.Term]) => helper(ids)(mroot)(nextPath)(v1)(v2))(state)(hydra.rewriting.subtermsWithAccessors(currentTerm))
  }
  def initialState[T0, T1, T2]: Tuple2[Tuple2[Seq[T0], Seq[T1]], scala.collection.immutable.Set[T2]] = Tuple2(Tuple2(Seq(), Seq()), hydra.lib.sets.empty[T2])
  lazy val result: Tuple2[Tuple2[Seq[hydra.accessors.AccessorNode], Seq[hydra.accessors.AccessorEdge]], scala.collection.immutable.Set[scala.Predef.String]] = helper(hydra.lib.maps.empty[hydra.core.Name, hydra.accessors.AccessorNode])(None)(Seq())(initialState)(Tuple2(dontCareAccessor, term))
  lazy val finalNodesEdges: Tuple2[Seq[hydra.accessors.AccessorNode], Seq[hydra.accessors.AccessorEdge]] = hydra.lib.pairs.first[Tuple2[Seq[hydra.accessors.AccessorNode], Seq[hydra.accessors.AccessorEdge]], scala.collection.immutable.Set[scala.Predef.String]](result)
  lazy val finalNodes: Seq[hydra.accessors.AccessorNode] = hydra.lib.pairs.first[Seq[hydra.accessors.AccessorNode], Seq[hydra.accessors.AccessorEdge]](finalNodesEdges)
  lazy val finalEdges: Seq[hydra.accessors.AccessorEdge] = hydra.lib.pairs.second[Seq[hydra.accessors.AccessorNode], Seq[hydra.accessors.AccessorEdge]](finalNodesEdges)
  hydra.accessors.AccessorGraph(finalNodes, finalEdges)
}
