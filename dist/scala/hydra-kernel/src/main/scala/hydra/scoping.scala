package hydra.scoping

import hydra.core.*

import hydra.graph.*

def extendGraphForLambda(g: hydra.graph.Graph)(lam: hydra.core.Lambda): hydra.graph.Graph =
  {
  lazy val `var`: hydra.core.Name = (lam.parameter)
  hydra.graph.Graph(g.boundTerms, hydra.lib.maybes.maybe[Map[hydra.core.Name, hydra.core.TypeScheme],
     hydra.core.Type](g.boundTypes)((dom: hydra.core.Type) =>
    hydra.lib.maps.insert[hydra.core.Name, hydra.core.TypeScheme](`var`)(hydra.scoping.fTypeToTypeScheme(dom))(g.boundTypes))(lam.domain),
       (g.classConstraints), hydra.lib.sets.insert[hydra.core.Name](`var`)(g.lambdaVariables),
       hydra.lib.maps.delete[hydra.core.Name, hydra.core.Term](`var`)(g.metadata),
       (g.primitives), (g.schemaTypes), (g.typeVariables))
}

def extendGraphForLet(forBinding: (hydra.graph.Graph => hydra.core.Binding => Option[hydra.core.Term]))(g: hydra.graph.Graph)(letrec: hydra.core.Let): hydra.graph.Graph =
  {
  lazy val bindings: Seq[hydra.core.Binding] = (letrec.bindings)
  lazy val g2: hydra.graph.Graph = hydra.scoping.extendGraphWithBindings(bindings)(g)
  hydra.graph.Graph(hydra.lib.maps.union[hydra.core.Name, hydra.core.Term](hydra.lib.maps.fromList[hydra.core.Name,
     hydra.core.Term](hydra.lib.lists.map[hydra.core.Binding, Tuple2[hydra.core.Name,
     hydra.core.Term]]((b: hydra.core.Binding) => Tuple2(b.name, (b.term)))(bindings)))(g.boundTerms),
     hydra.lib.maps.union[hydra.core.Name, hydra.core.TypeScheme](hydra.lib.maps.fromList[hydra.core.Name,
     hydra.core.TypeScheme](hydra.lib.maybes.cat[Tuple2[hydra.core.Name, hydra.core.TypeScheme]](hydra.lib.lists.map[hydra.core.Binding,
     Option[Tuple2[hydra.core.Name, hydra.core.TypeScheme]]]((b: hydra.core.Binding) =>
    hydra.lib.maybes.map[hydra.core.TypeScheme, Tuple2[hydra.core.Name, hydra.core.TypeScheme]]((ts: hydra.core.TypeScheme) => Tuple2(b.name,
       ts))(b.`type`))(bindings))))(g.boundTypes), (g.classConstraints), hydra.lib.lists.foldl[scala.collection.immutable.Set[hydra.core.Name],
       hydra.core.Binding]((s: scala.collection.immutable.Set[hydra.core.Name]) =>
    (b: hydra.core.Binding) => hydra.lib.sets.delete[hydra.core.Name](b.name)(s))(g.lambdaVariables)(bindings),
       (hydra.lib.lists.foldl[hydra.graph.Graph, hydra.core.Binding]((gAcc: hydra.graph.Graph) =>
    (b: hydra.core.Binding) =>
    {
    lazy val m: Map[hydra.core.Name, hydra.core.Term] = (gAcc.metadata)
    {
      lazy val newMeta: Map[hydra.core.Name, hydra.core.Term] = hydra.lib.maybes.maybe[Map[hydra.core.Name,
         hydra.core.Term], hydra.core.Term](hydra.lib.maps.delete[hydra.core.Name,
         hydra.core.Term](b.name)(m))((t: hydra.core.Term) =>
        hydra.lib.maps.insert[hydra.core.Name, hydra.core.Term](b.name)(t)(m))(forBinding(gAcc)(b))
      hydra.graph.Graph(gAcc.boundTerms, (gAcc.boundTypes), (gAcc.classConstraints),
         (gAcc.lambdaVariables), newMeta, (gAcc.primitives), (gAcc.schemaTypes), (gAcc.typeVariables))
    }
  })(g2)(bindings).metadata), (g.primitives), (g.schemaTypes), (g.typeVariables))
}

def extendGraphForTypeLambda(g: hydra.graph.Graph)(tlam: hydra.core.TypeLambda): hydra.graph.Graph =
  {
  lazy val name: hydra.core.Name = (tlam.parameter)
  hydra.graph.Graph(g.boundTerms, (g.boundTypes), (g.classConstraints), (g.lambdaVariables),
     (g.metadata), (g.primitives), (g.schemaTypes), hydra.lib.sets.insert[hydra.core.Name](name)(g.typeVariables))
}

def extendGraphWithBindings(bindings: Seq[hydra.core.Binding])(g: hydra.graph.Graph): hydra.graph.Graph =
  {
  lazy val newTerms: Map[hydra.core.Name, hydra.core.Term] = hydra.lib.maps.fromList[hydra.core.Name,
     hydra.core.Term](hydra.lib.lists.map[hydra.core.Binding, Tuple2[hydra.core.Name,
     hydra.core.Term]]((b: hydra.core.Binding) => Tuple2(b.name, (b.term)))(bindings))
  lazy val newTypes: Map[hydra.core.Name, hydra.core.TypeScheme] = hydra.lib.maps.fromList[hydra.core.Name,
     hydra.core.TypeScheme](hydra.lib.maybes.cat[Tuple2[hydra.core.Name, hydra.core.TypeScheme]](hydra.lib.lists.map[hydra.core.Binding,
     Option[Tuple2[hydra.core.Name, hydra.core.TypeScheme]]]((b: hydra.core.Binding) =>
    hydra.lib.maybes.map[hydra.core.TypeScheme, Tuple2[hydra.core.Name, hydra.core.TypeScheme]]((ts: hydra.core.TypeScheme) => Tuple2(b.name,
       ts))(b.`type`))(bindings)))
  hydra.graph.Graph(hydra.lib.maps.union[hydra.core.Name, hydra.core.Term](newTerms)(g.boundTerms),
     hydra.lib.maps.union[hydra.core.Name, hydra.core.TypeScheme](newTypes)(g.boundTypes),
     (g.classConstraints), (g.lambdaVariables), (g.metadata), (g.primitives), (g.schemaTypes),
     (g.typeVariables))
}

def fTypeToTypeScheme(typ: hydra.core.Type): hydra.core.TypeScheme =
  {
  def stripAnnotations(t: hydra.core.Type): hydra.core.Type =
    t match
    case hydra.core.Type.annotated(v_Type_annotated_at) => stripAnnotations(v_Type_annotated_at.body)
    case _ => t
  def gatherForall(vars: Seq[hydra.core.Name])(typ2: hydra.core.Type): hydra.core.TypeScheme =
    stripAnnotations(typ2) match
    case hydra.core.Type.forall(v_Type_forall_ft) => gatherForall(hydra.lib.lists.cons[hydra.core.Name](v_Type_forall_ft.parameter)(vars))(v_Type_forall_ft.body)
    case _ => hydra.core.TypeScheme(hydra.lib.lists.reverse[hydra.core.Name](vars), typ2, None)
  gatherForall(Seq())(typ)
}

def typeSchemeToFType(ts: hydra.core.TypeScheme): hydra.core.Type =
  {
  lazy val vars: Seq[hydra.core.Name] = (ts.variables)
  lazy val body: hydra.core.Type = (ts.`type`)
  hydra.lib.lists.foldl[hydra.core.Type, hydra.core.Name]((t: hydra.core.Type) =>
    (v: hydra.core.Name) => hydra.core.Type.forall(hydra.core.ForallType(v, t)))(body)(hydra.lib.lists.reverse[hydra.core.Name](vars))
}
