package hydra.lexical

import hydra.context.*

import hydra.core.*

import hydra.errors.*

import hydra.graph.*

def buildGraph(elements: Seq[hydra.core.Binding])(environment: Map[hydra.core.Name, Option[hydra.core.Term]])(primitives: Map[hydra.core.Name,
   hydra.graph.Primitive]): hydra.graph.Graph =
  {
  lazy val elementTerms: Map[hydra.core.Name, hydra.core.Term] = hydra.lib.maps.fromList[hydra.core.Name,
     hydra.core.Term](hydra.lib.lists.map[hydra.core.Binding, Tuple2[hydra.core.Name, hydra.core.Term]]((b: hydra.core.Binding) => Tuple2(b.name,
     (b.term)))(elements))
  lazy val letTerms: Map[hydra.core.Name, hydra.core.Term] = hydra.lib.maps.map[Option[hydra.core.Term],
     hydra.core.Term, hydra.core.Name]((mt: Option[hydra.core.Term]) => hydra.lib.maybes.fromJust[hydra.core.Term](mt))(hydra.lib.maps.filter[Option[hydra.core.Term],
     hydra.core.Name]((mt: Option[hydra.core.Term]) => hydra.lib.maybes.isJust[hydra.core.Term](mt))(environment))
  lazy val elementTypes: Map[hydra.core.Name, hydra.core.TypeScheme] = hydra.lib.maps.fromList[hydra.core.Name,
     hydra.core.TypeScheme](hydra.lib.maybes.cat[Tuple2[hydra.core.Name, hydra.core.TypeScheme]](hydra.lib.lists.map[hydra.core.Binding,
     Option[Tuple2[hydra.core.Name, hydra.core.TypeScheme]]]((b: hydra.core.Binding) =>
    hydra.lib.maybes.map[hydra.core.TypeScheme, Tuple2[hydra.core.Name, hydra.core.TypeScheme]]((ts: hydra.core.TypeScheme) => Tuple2(b.name,
       ts))(b.`type`))(elements)))
  hydra.graph.Graph(hydra.lib.maps.union[hydra.core.Name, hydra.core.Term](elementTerms)(letTerms), elementTypes,
     hydra.lib.maps.empty[hydra.core.Name, hydra.core.TypeVariableMetadata], hydra.lib.sets.fromList[hydra.core.Name](hydra.lib.maps.keys[hydra.core.Name,
     Option[hydra.core.Term]](hydra.lib.maps.filter[Option[hydra.core.Term], hydra.core.Name]((mt: Option[hydra.core.Term]) => hydra.lib.maybes.isNothing[hydra.core.Term](mt))(environment))),
     hydra.lib.maps.empty[hydra.core.Name, hydra.core.Term], primitives, hydra.lib.maps.empty[hydra.core.Name,
     hydra.core.TypeScheme], hydra.lib.sets.empty[hydra.core.Name])
}

def chooseUniqueName(reserved: scala.collection.immutable.Set[hydra.core.Name])(name: hydra.core.Name): hydra.core.Name =
  {
  def tryName(index: Int): hydra.core.Name =
    {
    lazy val candidate: hydra.core.Name = hydra.lib.logic.ifElse[hydra.core.Name](hydra.lib.equality.equal[Int](index)(1))(name)(hydra.lib.strings.cat2(name)(hydra.lib.literals.showInt32(index)))
    hydra.lib.logic.ifElse[hydra.core.Name](hydra.lib.sets.member[hydra.core.Name](candidate)(reserved))(tryName(hydra.lib.math.add(index)(1)))(candidate)
  }
  tryName(1)
}

def dereferenceSchemaType(name: hydra.core.Name)(types: Map[hydra.core.Name, hydra.core.TypeScheme]): Option[hydra.core.TypeScheme] =
  {
  def forType(t: hydra.core.Type): Option[hydra.core.TypeScheme] =
    t match
    case hydra.core.Type.annotated(v_Type_annotated_at) => forType(v_Type_annotated_at.body)
    case hydra.core.Type.forall(v_Type_forall_ft) => hydra.lib.maybes.map[hydra.core.TypeScheme, hydra.core.TypeScheme]((ts: hydra.core.TypeScheme) =>
      hydra.core.TypeScheme(hydra.lib.lists.cons[hydra.core.Name](v_Type_forall_ft.parameter)(ts.variables),
         (ts.`type`), (ts.constraints)))(forType(v_Type_forall_ft.body))
    case hydra.core.Type.variable(v_Type_variable_v) => hydra.lexical.dereferenceSchemaType(v_Type_variable_v)(types)
    case _ => Some(hydra.core.TypeScheme(Seq(), t, None))
  hydra.lib.maybes.bind[hydra.core.TypeScheme, hydra.core.TypeScheme](hydra.lib.maps.lookup[hydra.core.Name,
     hydra.core.TypeScheme](name)(types))((ts: hydra.core.TypeScheme) =>
    hydra.lib.maybes.map[hydra.core.TypeScheme, hydra.core.TypeScheme]((ts2: hydra.core.TypeScheme) =>
    hydra.core.TypeScheme(hydra.lib.lists.concat2[hydra.core.Name](ts.variables)(ts2.variables), (ts2.`type`), (ts2.constraints)))(forType(ts.`type`)))
}

def dereferenceVariable(graph: hydra.graph.Graph)(name: hydra.core.Name): Either[scala.Predef.String, hydra.core.Binding] =
  hydra.lib.maybes.maybe[Either[scala.Predef.String, hydra.core.Binding], hydra.core.Binding](Left(hydra.lib.strings.cat2("no such element: ")(name)))((`right_`: hydra.core.Binding) => Right(`right_`))(hydra.lexical.lookupBinding(graph)(name))

def elementsToGraph(parent: hydra.graph.Graph)(schemaTypes: Map[hydra.core.Name, hydra.core.TypeScheme])(elements: Seq[hydra.core.Binding]): hydra.graph.Graph =
  {
  lazy val prims: Map[hydra.core.Name, hydra.graph.Primitive] = (parent.primitives)
  lazy val g: hydra.graph.Graph = hydra.lexical.buildGraph(elements)(hydra.lib.maps.empty[hydra.core.Name, Option[hydra.core.Term]])(prims)
  hydra.graph.Graph(g.boundTerms, (g.boundTypes), (g.classConstraints), (g.lambdaVariables), (g.metadata), (g.primitives), schemaTypes, (g.typeVariables))
}

lazy val emptyContext: hydra.context.Context = hydra.context.Context(Seq(), Seq(), hydra.lib.maps.empty[hydra.core.Name, hydra.core.Term])

lazy val emptyGraph: hydra.graph.Graph = hydra.graph.Graph(hydra.lib.maps.empty[hydra.core.Name, hydra.core.Term],
   hydra.lib.maps.empty[hydra.core.Name, hydra.core.TypeScheme], hydra.lib.maps.empty[hydra.core.Name,
   hydra.core.TypeVariableMetadata], hydra.lib.sets.empty[hydra.core.Name], hydra.lib.maps.empty[hydra.core.Name,
   hydra.core.Term], hydra.lib.maps.empty[hydra.core.Name, hydra.graph.Primitive], hydra.lib.maps.empty[hydra.core.Name,
   hydra.core.TypeScheme], hydra.lib.sets.empty[hydra.core.Name])

def fieldsOf(t: hydra.core.Type): Seq[hydra.core.FieldType] =
  {
  lazy val stripped: hydra.core.Type = hydra.strip.deannotateType(t)
  stripped match
    case hydra.core.Type.forall(v_Type_forall_forallType) => hydra.lexical.fieldsOf(v_Type_forall_forallType.body)
    case hydra.core.Type.record(v_Type_record_rt) => v_Type_record_rt
    case hydra.core.Type.union(v_Type_union_rt) => v_Type_union_rt
    case _ => Seq()
}

def getField[T0, T1](cx: hydra.context.Context)(m: Map[hydra.core.Name, T0])(fname: hydra.core.Name)(decode: (T0 => Either[hydra.context.InContext[hydra.errors.Error],
   T1])): Either[hydra.context.InContext[hydra.errors.Error], T1] =
  hydra.lib.maybes.maybe[Either[hydra.context.InContext[hydra.errors.Error], T1], T0](Left(hydra.context.InContext(hydra.errors.Error.other(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected field ")(fname))(" not found")),
     cx)))(decode)(hydra.lib.maps.lookup[hydra.core.Name, T0](fname)(m))

def graphToBindings(g: hydra.graph.Graph): Seq[hydra.core.Binding] =
  hydra.lib.lists.map[Tuple2[hydra.core.Name, hydra.core.Term], hydra.core.Binding]((p: Tuple2[hydra.core.Name, hydra.core.Term]) =>
  {
  lazy val name: hydra.core.Name = hydra.lib.pairs.first[hydra.core.Name, hydra.core.Term](p)
  {
    lazy val term: hydra.core.Term = hydra.lib.pairs.second[hydra.core.Name, hydra.core.Term](p)
    hydra.core.Binding(name, term, hydra.lib.maps.lookup[hydra.core.Name, hydra.core.TypeScheme](name)(g.boundTypes))
  }
})(hydra.lib.maps.toList[hydra.core.Name, hydra.core.Term](g.boundTerms))

def lookupBinding(graph: hydra.graph.Graph)(name: hydra.core.Name): Option[hydra.core.Binding] =
  hydra.lib.maybes.map[hydra.core.Term, hydra.core.Binding]((term: hydra.core.Term) =>
  hydra.core.Binding(name, term, hydra.lib.maps.lookup[hydra.core.Name, hydra.core.TypeScheme](name)(graph.boundTypes)))(hydra.lib.maps.lookup[hydra.core.Name,
     hydra.core.Term](name)(graph.boundTerms))

def lookupPrimitive(graph: hydra.graph.Graph)(name: hydra.core.Name): Option[hydra.graph.Primitive] =
  hydra.lib.maps.lookup[hydra.core.Name, hydra.graph.Primitive](name)(graph.primitives)

def lookupTerm(graph: hydra.graph.Graph)(name: hydra.core.Name): Option[hydra.core.Term] = hydra.lib.maps.lookup[hydra.core.Name,
   hydra.core.Term](name)(graph.boundTerms)

def matchEnum[T0](cx: hydra.context.Context)(graph: hydra.graph.Graph)(tname: hydra.core.Name)(pairs: Seq[Tuple2[hydra.core.Name,
   T0]])(v1: hydra.core.Term): Either[hydra.context.InContext[hydra.errors.Error], T0] =
  hydra.lexical.matchUnion(cx)(graph)(tname)(hydra.lib.lists.map[Tuple2[hydra.core.Name, T0], Tuple2[hydra.core.Name,
     (hydra.core.Term) => Either[hydra.context.InContext[hydra.errors.Error], T0]]]((pair: Tuple2[hydra.core.Name,
     T0]) =>
  hydra.lexical.matchUnitField(hydra.lib.pairs.first[hydra.core.Name, T0](pair))(hydra.lib.pairs.second[hydra.core.Name, T0](pair)))(pairs))(v1)

def matchRecord[T0, T1](cx: hydra.context.Context)(graph: T0)(decode: (Map[hydra.core.Name, hydra.core.Term] => Either[hydra.context.InContext[hydra.errors.Error],
   T1]))(term: hydra.core.Term): Either[hydra.context.InContext[hydra.errors.Error], T1] =
  {
  lazy val stripped: hydra.core.Term = hydra.strip.deannotateAndDetypeTerm(term)
  stripped match
    case hydra.core.Term.record(v_Term_record_record) => decode(hydra.lib.maps.fromList[hydra.core.Name,
       hydra.core.Term](hydra.lib.lists.map[hydra.core.Field, Tuple2[hydra.core.Name, hydra.core.Term]]((field: hydra.core.Field) => Tuple2(field.name,
       (field.term)))(v_Term_record_record.fields)))
    case _ => Left(hydra.context.InContext(hydra.errors.Error.other(hydra.lib.strings.cat2("expected a record, got ")(hydra.show.core.term(term))), cx))
}

def matchUnion[T0](cx: hydra.context.Context)(graph: hydra.graph.Graph)(tname: hydra.core.Name)(pairs: Seq[Tuple2[hydra.core.Name,
   (hydra.core.Term => Either[hydra.context.InContext[hydra.errors.Error], T0])]])(term: hydra.core.Term): Either[hydra.context.InContext[hydra.errors.Error],
   T0] =
  {
  lazy val stripped: hydra.core.Term = hydra.strip.deannotateAndDetypeTerm(term)
  lazy val mapping: Map[hydra.core.Name, (hydra.core.Term => Either[hydra.context.InContext[hydra.errors.Error],
     T0])] = hydra.lib.maps.fromList[hydra.core.Name, (hydra.core.Term) => Either[hydra.context.InContext[hydra.errors.Error],
     T0]](pairs)
  stripped match
    case hydra.core.Term.variable(v_Term_variable_name) => hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error],
       hydra.core.Binding, T0](hydra.lexical.requireBinding(cx)(graph)(v_Term_variable_name))((el: hydra.core.Binding) => hydra.lexical.matchUnion(cx)(graph)(tname)(pairs)(el.term))
    case hydra.core.Term.union(v_Term_union_injection) => {
      lazy val exp: Either[hydra.context.InContext[hydra.errors.Error], T0] = {
        lazy val fname: hydra.core.Name = (v_Term_union_injection.field.name)
        {
          lazy val `val`: hydra.core.Term = (v_Term_union_injection.field.term)
          hydra.lib.maybes.maybe[Either[hydra.context.InContext[hydra.errors.Error], T0], (hydra.core.Term) => Either[hydra.context.InContext[hydra.errors.Error],
             T0]](Left(hydra.context.InContext(hydra.errors.Error.other(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("no matching case for field \"")(fname))("\" in union type "))(tname)),
             cx)))((f: (hydra.core.Term => Either[hydra.context.InContext[hydra.errors.Error], T0])) => f(`val`))(hydra.lib.maps.lookup[hydra.core.Name,
             (hydra.core.Term) => Either[hydra.context.InContext[hydra.errors.Error], T0]](fname)(mapping))
        }
      }
      hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.Error], T0]](hydra.lib.equality.equal[scala.Predef.String](v_Term_union_injection.typeName)(tname))(exp)(Left(hydra.context.InContext(hydra.errors.Error.other(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected injection for type ")(tname))(", got "))(hydra.show.core.term(term))),
         cx)))
    }
    case _ => Left(hydra.context.InContext(hydra.errors.Error.other(hydra.lib.strings.cat(Seq("expected inject(",
       tname, ") with one of {", hydra.lib.strings.intercalate(", ")(hydra.lib.lists.map[Tuple2[hydra.core.Name,
       (hydra.core.Term) => Either[hydra.context.InContext[hydra.errors.Error], T0]], scala.Predef.String]((pair: Tuple2[hydra.core.Name,
       (hydra.core.Term => Either[hydra.context.InContext[hydra.errors.Error], T0])]) =>
      hydra.lib.pairs.first[hydra.core.Name, (hydra.core.Term) => Either[hydra.context.InContext[hydra.errors.Error],
         T0]](pair))(pairs)), "}, got ", hydra.show.core.term(stripped)))), cx))
}

def matchUnitField[T0, T1, T2, T3](fname: T0)(x: T1): Tuple2[T0, (T2 => Either[T3, T1])] = Tuple2(fname, (ignored: T2) => Right(x))

def requireBinding(cx: hydra.context.Context)(graph: hydra.graph.Graph)(name: hydra.core.Name): Either[hydra.context.InContext[hydra.errors.Error],
   hydra.core.Binding] =
  {
  lazy val showAll: Boolean = false
  def ellipsis(strings: Seq[scala.Predef.String]): Seq[scala.Predef.String] =
    hydra.lib.logic.ifElse[Seq[scala.Predef.String]](hydra.lib.logic.and(hydra.lib.equality.gt[Int](hydra.lib.lists.length[scala.Predef.String](strings))(3))(hydra.lib.logic.not(showAll)))(hydra.lib.lists.concat2[scala.Predef.String](hydra.lib.lists.take[scala.Predef.String](3)(strings))(Seq("...")))(strings)
  lazy val errMsg: scala.Predef.String = hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("no such element: ")(name))(". Available elements: {"))(hydra.lib.strings.intercalate(", ")(ellipsis(hydra.lib.lists.map[hydra.core.Name,
     scala.Predef.String]((x) => x)(hydra.lib.maps.keys[hydra.core.Name, hydra.core.Term](graph.boundTerms))))))("}")
  hydra.lib.maybes.maybe[Either[hydra.context.InContext[hydra.errors.Error], hydra.core.Binding], hydra.core.Binding](Left(hydra.context.InContext(hydra.errors.Error.other(errMsg),
     cx)))((x: hydra.core.Binding) => Right(x))(hydra.lexical.lookupBinding(graph)(name))
}

def requirePrimitive(cx: hydra.context.Context)(graph: hydra.graph.Graph)(name: hydra.core.Name): Either[hydra.context.InContext[hydra.errors.Error],
   hydra.graph.Primitive] =
  hydra.lib.maybes.maybe[Either[hydra.context.InContext[hydra.errors.Error], hydra.graph.Primitive], hydra.graph.Primitive](Left(hydra.context.InContext(hydra.errors.Error.other(hydra.lib.strings.cat2("no such primitive function: ")(name)),
     cx)))((x: hydra.graph.Primitive) => Right(x))(hydra.lexical.lookupPrimitive(graph)(name))

def requirePrimitiveType(cx: hydra.context.Context)(tx: hydra.graph.Graph)(name: hydra.core.Name): Either[hydra.context.InContext[hydra.errors.Error],
   hydra.core.TypeScheme] =
  {
  lazy val mts: Option[hydra.core.TypeScheme] = hydra.lib.maybes.map[hydra.graph.Primitive, hydra.core.TypeScheme]((_p: hydra.graph.Primitive) => (_p.`type`))(hydra.lib.maps.lookup[hydra.core.Name,
     hydra.graph.Primitive](name)(tx.primitives))
  hydra.lib.maybes.maybe[Either[hydra.context.InContext[hydra.errors.Error], hydra.core.TypeScheme], hydra.core.TypeScheme](Left(hydra.context.InContext(hydra.errors.Error.other(hydra.lib.strings.cat2("no such primitive function: ")(name)),
     cx)))((ts: hydra.core.TypeScheme) => Right(ts))(mts)
}

def requireTerm(cx: hydra.context.Context)(graph: hydra.graph.Graph)(name: hydra.core.Name): Either[hydra.context.InContext[hydra.errors.Error],
   hydra.core.Term] =
  hydra.lib.maybes.maybe[Either[hydra.context.InContext[hydra.errors.Error], hydra.core.Term], hydra.core.Term](Left(hydra.context.InContext(hydra.errors.Error.other(hydra.lib.strings.cat2("no such element: ")(name)),
     cx)))((x: hydra.core.Term) => Right(x))(hydra.lexical.resolveTerm(graph)(name))

def resolveTerm(graph: hydra.graph.Graph)(name: hydra.core.Name): Option[hydra.core.Term] =
  {
  def recurse(term: hydra.core.Term): Option[hydra.core.Term] =
    {
    lazy val stripped: hydra.core.Term = hydra.strip.deannotateTerm(term)
    stripped match
      case hydra.core.Term.variable(v_Term_variable_name_) => hydra.lexical.resolveTerm(graph)(`v_Term_variable_name_`)
      case _ => Some(term)
  }
  hydra.lib.maybes.maybe[Option[hydra.core.Term], hydra.core.Term](None)(recurse)(hydra.lexical.lookupTerm(graph)(name))
}

def stripAndDereferenceTerm(cx: hydra.context.Context)(graph: hydra.graph.Graph)(term: hydra.core.Term): Either[hydra.context.InContext[hydra.errors.Error],
   hydra.core.Term] =
  {
  lazy val stripped: hydra.core.Term = hydra.strip.deannotateAndDetypeTerm(term)
  stripped match
    case hydra.core.Term.variable(v_Term_variable_v) => hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error],
       hydra.core.Term, hydra.core.Term](hydra.lexical.requireTerm(cx)(graph)(v_Term_variable_v))((t: hydra.core.Term) => hydra.lexical.stripAndDereferenceTerm(cx)(graph)(t))
    case _ => Right(stripped)
}

def stripAndDereferenceTermEither(graph: hydra.graph.Graph)(term: hydra.core.Term): Either[scala.Predef.String, hydra.core.Term] =
  {
  lazy val stripped: hydra.core.Term = hydra.strip.deannotateAndDetypeTerm(term)
  stripped match
    case hydra.core.Term.variable(v_Term_variable_v) => hydra.lib.eithers.either[scala.Predef.String,
       hydra.core.Binding, Either[scala.Predef.String, hydra.core.Term]]((`left_`: scala.Predef.String) => Left(`left_`))((binding: hydra.core.Binding) =>
      hydra.lexical.stripAndDereferenceTermEither(graph)(binding.term))(hydra.lexical.dereferenceVariable(graph)(v_Term_variable_v))
    case _ => Right(stripped)
}
