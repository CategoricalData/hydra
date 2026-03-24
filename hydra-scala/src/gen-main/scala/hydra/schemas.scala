package hydra.schemas

import hydra.coders.*

import hydra.context.*

import hydra.core.*

import hydra.errors.*

import hydra.graph.*

import hydra.module.*

import hydra.typing.*

import hydra.variants.*

import hydra.lib.eithers

import hydra.lib.equality

import hydra.lib.lists

import hydra.lib.literals

import hydra.lib.logic

import hydra.lib.maps

import hydra.lib.math

import hydra.lib.maybes

import hydra.lib.pairs

import hydra.lib.sets

import hydra.lib.strings

def addNamesToNamespaces[T0](encodeNamespace: (hydra.module.Namespace => T0))(names: scala.collection.immutable.Set[hydra.core.Name])(ns0: hydra.module.Namespaces[T0]): hydra.module.Namespaces[T0] =
  {
  lazy val nss: scala.collection.immutable.Set[hydra.module.Namespace] = hydra.lib.sets.fromList[hydra.module.Namespace](hydra.lib.maybes.cat[hydra.module.Namespace](hydra.lib.lists.map[hydra.core.Name, Option[hydra.module.Namespace]](hydra.names.namespaceOf)(hydra.lib.sets.toList[hydra.core.Name](names))))
  def toPair(ns: hydra.module.Namespace): Tuple2[hydra.module.Namespace, T0] = Tuple2(ns, encodeNamespace(ns))
  hydra.module.Namespaces(ns0.focus, hydra.lib.maps.union[hydra.module.Namespace, T0](ns0.mapping)(hydra.lib.maps.fromList[hydra.module.Namespace, T0](hydra.lib.lists.map[hydra.module.Namespace, Tuple2[hydra.module.Namespace, T0]](toPair)(hydra.lib.sets.toList[hydra.module.Namespace](nss)))))
}

def definitionDependencyNamespaces(defs: Seq[hydra.module.Definition]): scala.collection.immutable.Set[hydra.module.Namespace] =
  {
  def defNames(`def`: hydra.module.Definition): scala.collection.immutable.Set[hydra.core.Name] =
    `def` match
    case hydra.module.Definition.`type`(v_Definition_type_typeDef) => hydra.rewriting.typeDependencyNames(true)(v_Definition_type_typeDef.`type`)
    case hydra.module.Definition.term(v_Definition_term_termDef) => hydra.rewriting.termDependencyNames(true)(true)(true)(v_Definition_term_termDef.term)
  lazy val allNames: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.sets.unions[hydra.core.Name](hydra.lib.lists.map[hydra.module.Definition, scala.collection.immutable.Set[hydra.core.Name]](defNames)(defs))
  hydra.lib.sets.fromList[hydra.module.Namespace](hydra.lib.maybes.cat[hydra.module.Namespace](hydra.lib.lists.map[hydra.core.Name, Option[hydra.module.Namespace]](hydra.names.namespaceOf)(hydra.lib.sets.toList[hydra.core.Name](allNames))))
}

def dependencyNamespaces(cx: hydra.context.Context)(graph: hydra.graph.Graph)(binds: Boolean)(withPrims: Boolean)(withNoms: Boolean)(withSchema: Boolean)(els: Seq[hydra.core.Binding]): Either[hydra.context.InContext[hydra.errors.Error], scala.collection.immutable.Set[hydra.module.Namespace]] =
  {
  def depNames(el: hydra.core.Binding): Either[hydra.context.InContext[hydra.errors.Error], scala.collection.immutable.Set[hydra.core.Name]] =
    {
    lazy val term: hydra.core.Term = (el.term)
    lazy val deannotatedTerm: hydra.core.Term = hydra.rewriting.deannotateTerm(term)
    lazy val dataNames: scala.collection.immutable.Set[hydra.core.Name] = hydra.rewriting.termDependencyNames(binds)(withPrims)(withNoms)(term)
    lazy val schemaNames: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.logic.ifElse[scala.collection.immutable.Set[hydra.core.Name]](withSchema)(hydra.lib.maybes.maybe[scala.collection.immutable.Set[hydra.core.Name], hydra.core.TypeScheme](hydra.lib.sets.empty[hydra.core.Name])((ts: hydra.core.TypeScheme) => hydra.rewriting.typeDependencyNames(true)(ts.`type`))(el.`type`))(hydra.lib.sets.empty[hydra.core.Name])
    hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.Error], scala.collection.immutable.Set[hydra.core.Name]]](hydra.schemas.isEncodedType(deannotatedTerm))(hydra.lib.eithers.map[hydra.core.Type, scala.collection.immutable.Set[hydra.core.Name], hydra.context.InContext[hydra.errors.Error]]((typ: hydra.core.Type) =>
      hydra.lib.sets.unions[hydra.core.Name](Seq(dataNames, schemaNames, hydra.rewriting.typeDependencyNames(true)(typ))))(hydra.lib.eithers.bimap[hydra.errors.Error, hydra.core.Type, hydra.context.InContext[hydra.errors.Error], hydra.core.Type]((_wc_e: hydra.errors.Error) =>
      hydra.context.InContext(_wc_e, hydra.context.Context(hydra.lib.lists.cons[scala.Predef.String]("dependency namespace (type)")(cx.trace), (cx.messages), (cx.other))))((_wc_a: hydra.core.Type) => _wc_a)(hydra.lib.eithers.bimap[hydra.errors.DecodingError, hydra.core.Type, hydra.errors.Error, hydra.core.Type]((_e: hydra.errors.DecodingError) => hydra.errors.Error.other(_e))((_a: hydra.core.Type) => _a)(hydra.decode.core.`type`(graph)(term)))))(hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.Error], scala.collection.immutable.Set[hydra.core.Name]]](hydra.schemas.isEncodedTerm(deannotatedTerm))(hydra.lib.eithers.map[hydra.core.Term, scala.collection.immutable.Set[hydra.core.Name], hydra.context.InContext[hydra.errors.Error]]((decodedTerm: hydra.core.Term) =>
      hydra.lib.sets.unions[hydra.core.Name](Seq(dataNames, schemaNames, hydra.rewriting.termDependencyNames(binds)(withPrims)(withNoms)(decodedTerm))))(hydra.lib.eithers.bimap[hydra.errors.Error, hydra.core.Term, hydra.context.InContext[hydra.errors.Error], hydra.core.Term]((_wc_e: hydra.errors.Error) =>
      hydra.context.InContext(_wc_e, hydra.context.Context(hydra.lib.lists.cons[scala.Predef.String]("dependency namespace (term)")(cx.trace), (cx.messages), (cx.other))))((_wc_a: hydra.core.Term) => _wc_a)(hydra.lib.eithers.bimap[hydra.errors.DecodingError, hydra.core.Term, hydra.errors.Error, hydra.core.Term]((_e: hydra.errors.DecodingError) => hydra.errors.Error.other(_e))((_a: hydra.core.Term) => _a)(hydra.decode.core.term(graph)(term)))))(Right(hydra.lib.sets.unions[hydra.core.Name](Seq(dataNames, schemaNames)))))
  }
  hydra.lib.eithers.map[Seq[scala.collection.immutable.Set[hydra.core.Name]], scala.collection.immutable.Set[hydra.module.Namespace], hydra.context.InContext[hydra.errors.Error]]((namesList: Seq[scala.collection.immutable.Set[hydra.core.Name]]) =>
    hydra.lib.sets.fromList[hydra.module.Namespace](hydra.lib.maybes.cat[hydra.module.Namespace](hydra.lib.lists.map[hydra.core.Name, Option[hydra.module.Namespace]](hydra.names.namespaceOf)(hydra.lib.sets.toList[hydra.core.Name](hydra.lib.sets.unions[hydra.core.Name](namesList))))))(hydra.lib.eithers.mapList[hydra.core.Binding, scala.collection.immutable.Set[hydra.core.Name], hydra.context.InContext[hydra.errors.Error]](depNames)(els))
}

def dereferenceType(cx: hydra.context.Context)(graph: hydra.graph.Graph)(name: hydra.core.Name): Either[hydra.context.InContext[hydra.errors.Error], Option[hydra.core.Type]] =
  {
  lazy val mel: Option[hydra.core.Binding] = hydra.lexical.dereferenceElement(graph)(name)
  hydra.lib.maybes.maybe[Either[hydra.context.InContext[hydra.errors.Error], Option[hydra.core.Type]], hydra.core.Binding](Right(None))((el: hydra.core.Binding) =>
    hydra.lib.eithers.map[hydra.core.Type, Option[hydra.core.Type], hydra.context.InContext[hydra.errors.Error]](hydra.lib.maybes.pure[hydra.core.Type])(hydra.lib.eithers.bimap[hydra.errors.Error, hydra.core.Type, hydra.context.InContext[hydra.errors.Error], hydra.core.Type]((_wc_e: hydra.errors.Error) => hydra.context.InContext(_wc_e, cx))((_wc_a: hydra.core.Type) => _wc_a)(hydra.lib.eithers.bimap[hydra.errors.DecodingError, hydra.core.Type, hydra.errors.Error, hydra.core.Type]((_e: hydra.errors.DecodingError) => hydra.errors.Error.other(_e))((_a: hydra.core.Type) => _a)(hydra.decode.core.`type`(graph)(el.term)))))(mel)
}

def elementAsTypeApplicationTerm(cx: hydra.context.Context)(el: hydra.core.Binding): Either[hydra.context.InContext[hydra.errors.Error], hydra.core.TypeApplicationTerm] =
  hydra.lib.maybes.maybe[Either[hydra.context.InContext[hydra.errors.Error], hydra.core.TypeApplicationTerm], hydra.core.TypeScheme](Left(hydra.context.InContext(hydra.errors.Error.other("missing element type"), cx)))((ts: hydra.core.TypeScheme) => Right(hydra.core.TypeApplicationTerm(el.term, (ts.`type`))))(el.`type`)

def elementsWithDependencies(cx: hydra.context.Context)(graph: hydra.graph.Graph)(original: Seq[hydra.core.Binding]): Either[hydra.context.InContext[hydra.errors.Error], Seq[hydra.core.Binding]] =
  {
  def depNames(el: hydra.core.Binding): Seq[hydra.core.Name] =
    hydra.lib.sets.toList[hydra.core.Name](hydra.rewriting.termDependencyNames(true)(false)(false)(el.term))
  lazy val allDepNames: Seq[hydra.core.Name] = hydra.lib.lists.nub[hydra.core.Name](hydra.lib.lists.concat2[hydra.core.Name](hydra.lib.lists.map[hydra.core.Binding, hydra.core.Name]((x: hydra.core.Binding) => (x.name))(original))(hydra.lib.lists.concat[hydra.core.Name](hydra.lib.lists.map[hydra.core.Binding, Seq[hydra.core.Name]](depNames)(original))))
  hydra.lib.eithers.mapList[hydra.core.Name, hydra.core.Binding, hydra.context.InContext[hydra.errors.Error]]((name: hydra.core.Name) => hydra.lexical.requireElement(cx)(graph)(name))(allDepNames)
}

def fieldMap(fields: Seq[hydra.core.Field]): Map[hydra.core.Name, hydra.core.Term] =
  {
  def toPair(f: hydra.core.Field): Tuple2[hydra.core.Name, hydra.core.Term] = Tuple2(f.name, (f.term))
  hydra.lib.maps.fromList[hydra.core.Name, hydra.core.Term](hydra.lib.lists.map[hydra.core.Field, Tuple2[hydra.core.Name, hydra.core.Term]](toPair)(fields))
}

def fieldTypeMap(fields: Seq[hydra.core.FieldType]): Map[hydra.core.Name, hydra.core.Type] =
  {
  def toPair(f: hydra.core.FieldType): Tuple2[hydra.core.Name, hydra.core.Type] = Tuple2(f.name, (f.`type`))
  hydra.lib.maps.fromList[hydra.core.Name, hydra.core.Type](hydra.lib.lists.map[hydra.core.FieldType, Tuple2[hydra.core.Name, hydra.core.Type]](toPair)(fields))
}

def fieldTypes(cx: hydra.context.Context)(graph: hydra.graph.Graph)(t: hydra.core.Type): Either[hydra.context.InContext[hydra.errors.Error], Map[hydra.core.Name, hydra.core.Type]] =
  {
  def toMap(fields: Seq[hydra.core.FieldType]): Map[hydra.core.Name, hydra.core.Type] =
    hydra.lib.maps.fromList[hydra.core.Name, hydra.core.Type](hydra.lib.lists.map[hydra.core.FieldType, Tuple2[hydra.core.Name, hydra.core.Type]]((ft: hydra.core.FieldType) => Tuple2(ft.name, (ft.`type`)))(fields))
  hydra.rewriting.deannotateType(t) match
    case hydra.core.Type.forall(v_Type_forall_ft) => hydra.schemas.fieldTypes(cx)(graph)(v_Type_forall_ft.body)
    case hydra.core.Type.record(v_Type_record_rt) => Right(toMap(v_Type_record_rt))
    case hydra.core.Type.union(v_Type_union_rt) => Right(toMap(v_Type_union_rt))
    case hydra.core.Type.variable(v_Type_variable_name) => hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.core.Binding, Map[hydra.core.Name, hydra.core.Type]](hydra.lexical.requireElement(cx)(graph)(v_Type_variable_name))((el: hydra.core.Binding) =>
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.core.Type, Map[hydra.core.Name, hydra.core.Type]](hydra.lib.eithers.bimap[hydra.errors.Error, hydra.core.Type, hydra.context.InContext[hydra.errors.Error], hydra.core.Type]((_wc_e: hydra.errors.Error) => hydra.context.InContext(_wc_e, cx))((_wc_a: hydra.core.Type) => _wc_a)(hydra.lib.eithers.bimap[hydra.errors.DecodingError, hydra.core.Type, hydra.errors.Error, hydra.core.Type]((_e: hydra.errors.DecodingError) => hydra.errors.Error.other(_e))((_a: hydra.core.Type) => _a)(hydra.decode.core.`type`(graph)(el.term))))((decodedType: hydra.core.Type) => hydra.schemas.fieldTypes(cx)(graph)(decodedType)))
    case _ => Left(hydra.context.InContext(hydra.errors.Error.other(hydra.lib.strings.cat(Seq("expected record or union type but found ", hydra.show.core.`type`(t)))), cx))
}

def findFieldType(cx: hydra.context.Context)(fname: hydra.core.Name)(fields: Seq[hydra.core.FieldType]): Either[hydra.context.InContext[hydra.errors.Error], hydra.core.Type] =
  {
  lazy val matchingFields: Seq[hydra.core.FieldType] = hydra.lib.lists.filter[hydra.core.FieldType]((ft: hydra.core.FieldType) =>
    hydra.lib.equality.equal[scala.Predef.String](ft.name)(fname))(fields)
  hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.Error], hydra.core.Type]](hydra.lib.lists.`null`[hydra.core.FieldType](matchingFields))(Left(hydra.context.InContext(hydra.errors.Error.other(hydra.lib.strings.cat2("No such field: ")(fname)), cx)))(hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.Error], hydra.core.Type]](hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.core.FieldType](matchingFields))(1))(Right(hydra.lib.lists.head[hydra.core.FieldType](matchingFields).`type`))(Left(hydra.context.InContext(hydra.errors.Error.other(hydra.lib.strings.cat2("Multiple fields named ")(fname)), cx))))
}

def freshName(cx: hydra.context.Context): Tuple2[hydra.core.Name, hydra.context.Context] =
  {
  lazy val count: Int = hydra.annotations.getCount(hydra.constants.key_freshTypeVariableCount)(cx)
  Tuple2(hydra.schemas.normalTypeVariable(count), hydra.annotations.putCount(hydra.constants.key_freshTypeVariableCount)(hydra.lib.math.add(count)(1))(cx))
}

def freshNames(n: Int)(cx: hydra.context.Context): Tuple2[Seq[hydra.core.Name], hydra.context.Context] =
  {
  def go[T0](acc: Tuple2[Seq[hydra.core.Name], hydra.context.Context])(_x: T0): Tuple2[Seq[hydra.core.Name], hydra.context.Context] =
    {
    lazy val names: Seq[hydra.core.Name] = hydra.lib.pairs.first[Seq[hydra.core.Name], hydra.context.Context](acc)
    lazy val cx0: hydra.context.Context = hydra.lib.pairs.second[Seq[hydra.core.Name], hydra.context.Context](acc)
    lazy val result: Tuple2[hydra.core.Name, hydra.context.Context] = hydra.schemas.freshName(cx0)
    lazy val name: hydra.core.Name = hydra.lib.pairs.first[hydra.core.Name, hydra.context.Context](result)
    lazy val cx1: hydra.context.Context = hydra.lib.pairs.second[hydra.core.Name, hydra.context.Context](result)
    Tuple2(hydra.lib.lists.concat2[hydra.core.Name](names)(hydra.lib.lists.pure[hydra.core.Name](name)), cx1)
  }
  hydra.lib.lists.foldl[Tuple2[Seq[hydra.core.Name], hydra.context.Context], Unit](go)(Tuple2(Seq(), cx))(hydra.lib.lists.replicate[Unit](n)(()))
}

def fTypeIsPolymorphic(typ: hydra.core.Type): Boolean =
  typ match
  case hydra.core.Type.annotated(v_Type_annotated_at) => hydra.schemas.fTypeIsPolymorphic(v_Type_annotated_at.body)
  case hydra.core.Type.forall(_) => true
  case _ => false

def fullyStripAndNormalizeType(typ: hydra.core.Type): hydra.core.Type =
  {
  def go(depth: Int)(subst: Map[hydra.core.Name, hydra.core.Name])(t: hydra.core.Type): Tuple2[Map[hydra.core.Name, hydra.core.Name], hydra.core.Type] =
    hydra.rewriting.deannotateType(t) match
    case hydra.core.Type.forall(v_Type_forall_ft) => {
      lazy val oldVar: hydra.core.Name = (v_Type_forall_ft.parameter)
      {
        lazy val newVar: hydra.core.Name = hydra.lib.strings.cat2("_")(hydra.lib.literals.showInt32(depth))
        go(hydra.lib.math.add(depth)(1))(hydra.lib.maps.insert[hydra.core.Name, hydra.core.Name](oldVar)(newVar)(subst))(v_Type_forall_ft.body)
      }
    }
    case _ => Tuple2(subst, t)
  lazy val result: Tuple2[Map[hydra.core.Name, hydra.core.Name], hydra.core.Type] = go(0)(hydra.lib.maps.empty[hydra.core.Name, hydra.core.Name])(typ)
  lazy val subst: Map[hydra.core.Name, hydra.core.Name] = hydra.lib.pairs.first[Map[hydra.core.Name, hydra.core.Name], hydra.core.Type](result)
  lazy val body: hydra.core.Type = hydra.lib.pairs.second[Map[hydra.core.Name, hydra.core.Name], hydra.core.Type](result)
  hydra.rewriting.substituteTypeVariables(subst)(body)
}

def fullyStripType(typ: hydra.core.Type): hydra.core.Type =
  hydra.rewriting.deannotateType(typ) match
  case hydra.core.Type.forall(v_Type_forall_ft) => hydra.schemas.fullyStripType(v_Type_forall_ft.body)
  case _ => typ

def graphAsLet(bindings: Seq[hydra.core.Binding])(body: hydra.core.Term): hydra.core.Let = hydra.core.Let(bindings, body)

def graphAsTerm(bindings: Seq[hydra.core.Binding])(body: hydra.core.Term): hydra.core.Term = hydra.core.Term.let(hydra.schemas.graphAsLet(bindings)(body))

def graphAsTypes(cx: hydra.context.Context)(graph: hydra.graph.Graph)(els: Seq[hydra.core.Binding]): Either[hydra.context.InContext[hydra.errors.DecodingError], Map[hydra.core.Name, hydra.core.Type]] =
  {
  def toPair(el: hydra.core.Binding): Either[hydra.context.InContext[hydra.errors.DecodingError], Tuple2[hydra.core.Name, hydra.core.Type]] =
    hydra.lib.eithers.map[hydra.core.Type, Tuple2[hydra.core.Name, hydra.core.Type], hydra.context.InContext[hydra.errors.DecodingError]]((typ: hydra.core.Type) => Tuple2(el.name, typ))(hydra.lib.eithers.bimap[hydra.errors.DecodingError, hydra.core.Type, hydra.context.InContext[hydra.errors.DecodingError], hydra.core.Type]((_wc_e: hydra.errors.DecodingError) => hydra.context.InContext(_wc_e, cx))((_wc_a: hydra.core.Type) => _wc_a)(hydra.decode.core.`type`(graph)(el.term)))
  hydra.lib.eithers.map[Seq[Tuple2[hydra.core.Name, hydra.core.Type]], Map[hydra.core.Name, hydra.core.Type], hydra.context.InContext[hydra.errors.DecodingError]](hydra.lib.maps.fromList[hydra.core.Name, hydra.core.Type])(hydra.lib.eithers.mapList[hydra.core.Binding, Tuple2[hydra.core.Name, hydra.core.Type], hydra.context.InContext[hydra.errors.DecodingError]](toPair)(els))
}

def instantiateType(cx: hydra.context.Context)(typ: hydra.core.Type): Tuple2[hydra.core.Type, hydra.context.Context] =
  {
  lazy val result: Tuple2[hydra.core.TypeScheme, hydra.context.Context] = hydra.schemas.instantiateTypeScheme(cx)(hydra.schemas.typeToTypeScheme(typ))
  Tuple2(hydra.rewriting.typeSchemeToFType(hydra.lib.pairs.first[hydra.core.TypeScheme, hydra.context.Context](result)), hydra.lib.pairs.second[hydra.core.TypeScheme, hydra.context.Context](result))
}

def instantiateTypeScheme(cx: hydra.context.Context)(scheme: hydra.core.TypeScheme): Tuple2[hydra.core.TypeScheme, hydra.context.Context] =
  {
  lazy val oldVars: Seq[hydra.core.Name] = (scheme.variables)
  lazy val result: Tuple2[Seq[hydra.core.Name], hydra.context.Context] = hydra.schemas.freshNames(hydra.lib.lists.length[hydra.core.Name](oldVars))(cx)
  lazy val newVars: Seq[hydra.core.Name] = hydra.lib.pairs.first[Seq[hydra.core.Name], hydra.context.Context](result)
  lazy val cx2: hydra.context.Context = hydra.lib.pairs.second[Seq[hydra.core.Name], hydra.context.Context](result)
  lazy val subst: hydra.typing.TypeSubst = hydra.lib.maps.fromList[hydra.core.Name, hydra.core.Type](hydra.lib.lists.zip[hydra.core.Name, hydra.core.Type](oldVars)(hydra.lib.lists.map[hydra.core.Name, hydra.core.Type]((x: hydra.core.Name) => hydra.core.Type.variable(x))(newVars)))
  lazy val nameSubst: Map[hydra.core.Name, hydra.core.Name] = hydra.lib.maps.fromList[hydra.core.Name, hydra.core.Name](hydra.lib.lists.zip[hydra.core.Name, hydra.core.Name](oldVars)(newVars))
  lazy val renamedConstraints: Option[Map[hydra.core.Name, hydra.core.TypeVariableMetadata]] = hydra.lib.maybes.map[Map[hydra.core.Name, hydra.core.TypeVariableMetadata], Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]((oldConstraints: Map[hydra.core.Name, hydra.core.TypeVariableMetadata]) =>
    hydra.lib.maps.fromList[hydra.core.Name, hydra.core.TypeVariableMetadata](hydra.lib.lists.map[Tuple2[hydra.core.Name, hydra.core.TypeVariableMetadata], Tuple2[hydra.core.Name, hydra.core.TypeVariableMetadata]]((kv: Tuple2[hydra.core.Name, hydra.core.TypeVariableMetadata]) =>
    Tuple2(hydra.lib.maybes.fromMaybe[hydra.core.Name](hydra.lib.pairs.first[hydra.core.Name, hydra.core.TypeVariableMetadata](kv))(hydra.lib.maps.lookup[hydra.core.Name, hydra.core.Name](hydra.lib.pairs.first[hydra.core.Name, hydra.core.TypeVariableMetadata](kv))(nameSubst)), hydra.lib.pairs.second[hydra.core.Name, hydra.core.TypeVariableMetadata](kv)))(hydra.lib.maps.toList[hydra.core.Name, hydra.core.TypeVariableMetadata](oldConstraints))))(scheme.constraints)
  Tuple2(hydra.core.TypeScheme(newVars, hydra.substitution.substInType(subst)(scheme.`type`), renamedConstraints), cx2)
}

def isEncodedTerm(t: hydra.core.Term): Boolean =
  hydra.rewriting.deannotateTerm(t) match
  case hydra.core.Term.application(v_Term_application_a) => hydra.schemas.isEncodedTerm(v_Term_application_a.function)
  case hydra.core.Term.union(v_Term_union_i) => hydra.lib.equality.equal[scala.Predef.String]("hydra.core.Term")(v_Term_union_i.typeName)
  case _ => false

def isEncodedType(t: hydra.core.Term): Boolean =
  hydra.rewriting.deannotateTerm(t) match
  case hydra.core.Term.application(v_Term_application_a) => hydra.schemas.isEncodedType(v_Term_application_a.function)
  case hydra.core.Term.union(v_Term_union_i) => hydra.lib.equality.equal[scala.Predef.String]("hydra.core.Type")(v_Term_union_i.typeName)
  case _ => false

def isEnumRowType(rt: Seq[hydra.core.FieldType]): Boolean =
  hydra.lib.lists.foldl[Boolean, Boolean](hydra.lib.logic.and)(true)(hydra.lib.lists.map[hydra.core.FieldType, Boolean]((f: hydra.core.FieldType) =>
  hydra.schemas.isUnitType(hydra.rewriting.deannotateType(f.`type`)))(rt))

def isEnumType(typ: hydra.core.Type): Boolean =
  hydra.rewriting.deannotateType(typ) match
  case hydra.core.Type.union(v_Type_union_rt) => hydra.schemas.isEnumRowType(v_Type_union_rt)
  case _ => false

def isSerializable(cx: hydra.context.Context)(graph: hydra.graph.Graph)(el: hydra.core.Binding): Either[hydra.context.InContext[hydra.errors.Error], Boolean] =
  {
  def variants(typ: hydra.core.Type): Seq[hydra.variants.TypeVariant] =
    hydra.lib.lists.map[hydra.core.Type, hydra.variants.TypeVariant](hydra.reflect.typeVariant)(hydra.rewriting.foldOverType(hydra.coders.TraversalOrder.pre)((m: Seq[hydra.core.Type]) =>
    (t: hydra.core.Type) => hydra.lib.lists.cons[hydra.core.Type](t)(m))(Seq())(typ))
  hydra.lib.eithers.map[Map[hydra.core.Name, hydra.core.Type], Boolean, hydra.context.InContext[hydra.errors.Error]]((deps: Map[hydra.core.Name, hydra.core.Type]) =>
    {
    lazy val allVariants: scala.collection.immutable.Set[hydra.variants.TypeVariant] = hydra.lib.sets.fromList[hydra.variants.TypeVariant](hydra.lib.lists.concat[hydra.variants.TypeVariant](hydra.lib.lists.map[hydra.core.Type, Seq[hydra.variants.TypeVariant]](variants)(hydra.lib.maps.elems[hydra.core.Name, hydra.core.Type](deps))))
    hydra.lib.logic.not(hydra.lib.sets.member[hydra.variants.TypeVariant](hydra.variants.TypeVariant.function)(allVariants))
  })(hydra.schemas.typeDependencies(cx)(graph)(false)(hydra.lib.equality.identity[hydra.core.Type])(el.name))
}

def isSerializableType(typ: hydra.core.Type): Boolean =
  {
  lazy val allVariants: scala.collection.immutable.Set[hydra.variants.TypeVariant] = hydra.lib.sets.fromList[hydra.variants.TypeVariant](hydra.lib.lists.map[hydra.core.Type, hydra.variants.TypeVariant](hydra.reflect.typeVariant)(hydra.rewriting.foldOverType(hydra.coders.TraversalOrder.pre)((m: Seq[hydra.core.Type]) =>
    (t: hydra.core.Type) => hydra.lib.lists.cons[hydra.core.Type](t)(m))(Seq())(typ)))
  hydra.lib.logic.not(hydra.lib.sets.member[hydra.variants.TypeVariant](hydra.variants.TypeVariant.function)(allVariants))
}

def isSerializableByName(cx: hydra.context.Context)(graph: hydra.graph.Graph)(name: hydra.core.Name): Either[hydra.context.InContext[hydra.errors.Error], Boolean] =
  {
  def variants(typ: hydra.core.Type): Seq[hydra.variants.TypeVariant] =
    hydra.lib.lists.map[hydra.core.Type, hydra.variants.TypeVariant](hydra.reflect.typeVariant)(hydra.rewriting.foldOverType(hydra.coders.TraversalOrder.pre)((m: Seq[hydra.core.Type]) =>
    (t: hydra.core.Type) => hydra.lib.lists.cons[hydra.core.Type](t)(m))(Seq())(typ))
  hydra.lib.eithers.map[Map[hydra.core.Name, hydra.core.Type], Boolean, hydra.context.InContext[hydra.errors.Error]]((deps: Map[hydra.core.Name, hydra.core.Type]) =>
    {
    lazy val allVariants: scala.collection.immutable.Set[hydra.variants.TypeVariant] = hydra.lib.sets.fromList[hydra.variants.TypeVariant](hydra.lib.lists.concat[hydra.variants.TypeVariant](hydra.lib.lists.map[hydra.core.Type, Seq[hydra.variants.TypeVariant]](variants)(hydra.lib.maps.elems[hydra.core.Name, hydra.core.Type](deps))))
    hydra.lib.logic.not(hydra.lib.sets.member[hydra.variants.TypeVariant](hydra.variants.TypeVariant.function)(allVariants))
  })(hydra.schemas.typeDependencies(cx)(graph)(false)(hydra.lib.equality.identity[hydra.core.Type])(name))
}

def isNominalType(typ: hydra.core.Type): Boolean =
  hydra.rewriting.deannotateType(typ) match
  case hydra.core.Type.record(_) => true
  case hydra.core.Type.union(_) => true
  case hydra.core.Type.wrap(_) => true
  case hydra.core.Type.forall(v_Type_forall_fa) => hydra.schemas.isNominalType(v_Type_forall_fa.body)
  case _ => false

def isType(t: hydra.core.Type): Boolean =
  hydra.rewriting.deannotateType(t) match
  case hydra.core.Type.application(v_Type_application_a) => hydra.schemas.isType(v_Type_application_a.function)
  case hydra.core.Type.forall(v_Type_forall_l) => hydra.schemas.isType(v_Type_forall_l.body)
  case hydra.core.Type.union(_) => false
  case hydra.core.Type.variable(v_Type_variable_v) => hydra.lib.equality.equal[hydra.core.Name](v_Type_variable_v)("hydra.core.Type")
  case _ => false

def isUnitTerm(v1: hydra.core.Term): Boolean =
  v1 match
  case hydra.core.Term.unit => true
  case _ => false

def isUnitType(v1: hydra.core.Type): Boolean =
  v1 match
  case hydra.core.Type.unit => true
  case _ => false

def moduleContainsBinaryLiterals(mod: hydra.module.Module): Boolean =
  {
  def checkTerm(found: Boolean)(term: hydra.core.Term): Boolean =
    hydra.lib.logic.or(found)(term match
    case hydra.core.Term.literal(v_Term_literal_lit) => v_Term_literal_lit match
      case hydra.core.Literal.binary(_) => true
      case _ => false
    case _ => false)
  def termContainsBinary(term: hydra.core.Term): Boolean =
    hydra.rewriting.foldOverTerm(hydra.coders.TraversalOrder.pre)(checkTerm)(false)(term)
  lazy val defTerms: Seq[hydra.core.Term] = hydra.lib.maybes.cat[hydra.core.Term](hydra.lib.lists.map[hydra.module.Definition, Option[hydra.core.Term]]((d: hydra.module.Definition) =>
    d match
    case hydra.module.Definition.term(v_Definition_term_td) => Some(v_Definition_term_td.term)
    case _ => None)(mod.definitions))
  hydra.lib.lists.foldl[Boolean, hydra.core.Term]((acc: Boolean) =>
    (t: hydra.core.Term) => hydra.lib.logic.or(acc)(termContainsBinary(t)))(false)(defTerms)
}

def moduleDependencyNamespaces(cx: hydra.context.Context)(graph: hydra.graph.Graph)(binds: Boolean)(withPrims: Boolean)(withNoms: Boolean)(withSchema: Boolean)(mod: hydra.module.Module): Either[hydra.context.InContext[hydra.errors.Error], scala.collection.immutable.Set[hydra.module.Namespace]] =
  {
  lazy val allBindings: Seq[hydra.core.Binding] = hydra.lib.maybes.cat[hydra.core.Binding](hydra.lib.lists.map[hydra.module.Definition, Option[hydra.core.Binding]]((d: hydra.module.Definition) =>
    d match
    case hydra.module.Definition.`type`(v_Definition_type_td) => Some(hydra.annotations.typeElement(v_Definition_type_td.name)(v_Definition_type_td.`type`))
    case hydra.module.Definition.term(v_Definition_term_td) => Some(hydra.core.Binding(v_Definition_term_td.name, (v_Definition_term_td.term), (v_Definition_term_td.`type`)))
    case _ => None)(mod.definitions))
  hydra.lib.eithers.map[scala.collection.immutable.Set[hydra.module.Namespace], scala.collection.immutable.Set[hydra.module.Namespace], hydra.context.InContext[hydra.errors.Error]]((deps: scala.collection.immutable.Set[hydra.module.Namespace]) =>
    hydra.lib.sets.delete[hydra.module.Namespace](mod.namespace)(deps))(hydra.schemas.dependencyNamespaces(cx)(graph)(binds)(withPrims)(withNoms)(withSchema)(allBindings))
}

def namespacesForDefinitions[T0](encodeNamespace: (hydra.module.Namespace => T0))(focusNs: hydra.module.Namespace)(defs: Seq[hydra.module.Definition]): hydra.module.Namespaces[T0] =
  {
  lazy val nss: scala.collection.immutable.Set[hydra.module.Namespace] = hydra.lib.sets.delete[hydra.module.Namespace](focusNs)(hydra.schemas.definitionDependencyNamespaces(defs))
  def toPair(ns: hydra.module.Namespace): Tuple2[hydra.module.Namespace, T0] = Tuple2(ns, encodeNamespace(ns))
  hydra.module.Namespaces(toPair(focusNs), hydra.lib.maps.fromList[hydra.module.Namespace, T0](hydra.lib.lists.map[hydra.module.Namespace, Tuple2[hydra.module.Namespace, T0]](toPair)(hydra.lib.sets.toList[hydra.module.Namespace](nss))))
}

def nominalApplication(tname: hydra.core.Name)(args: Seq[hydra.core.Type]): hydra.core.Type =
  hydra.lib.lists.foldl[hydra.core.Type, hydra.core.Type]((t: hydra.core.Type) =>
  (a: hydra.core.Type) =>
  hydra.core.Type.application(hydra.core.ApplicationType(t, a)))(hydra.core.Type.variable(tname))(args)

def normalTypeVariable(i: Int): hydra.core.Name = hydra.lib.strings.cat2("t")(hydra.lib.literals.showInt32(i))

def partitionDefinitions(defs: Seq[hydra.module.Definition]): Tuple2[Seq[hydra.module.TypeDefinition], Seq[hydra.module.TermDefinition]] =
  {
  def getType(`def`: hydra.module.Definition): Option[hydra.module.TypeDefinition] =
    `def` match
    case hydra.module.Definition.`type`(v_Definition_type_td) => Some(v_Definition_type_td)
    case hydra.module.Definition.term(_) => None
  def getTerm(`def`: hydra.module.Definition): Option[hydra.module.TermDefinition] =
    `def` match
    case hydra.module.Definition.`type`(_) => None
    case hydra.module.Definition.term(v_Definition_term_td) => Some(v_Definition_term_td)
  Tuple2(hydra.lib.maybes.cat[hydra.module.TypeDefinition](hydra.lib.lists.map[hydra.module.Definition, Option[hydra.module.TypeDefinition]](getType)(defs)), hydra.lib.maybes.cat[hydra.module.TermDefinition](hydra.lib.lists.map[hydra.module.Definition, Option[hydra.module.TermDefinition]](getTerm)(defs)))
}

def requireRecordType(cx: hydra.context.Context)(graph: hydra.graph.Graph)(name: hydra.core.Name): Either[hydra.context.InContext[hydra.errors.Error], Seq[hydra.core.FieldType]] =
  {
  def toRecord(t: hydra.core.Type): Option[Seq[hydra.core.FieldType]] =
    t match
    case hydra.core.Type.record(v_Type_record_rt) => Some(v_Type_record_rt)
    case _ => None
  hydra.schemas.requireRowType(cx)("record type")(toRecord)(graph)(name)
}

def requireRowType[T0](cx: hydra.context.Context)(label: scala.Predef.String)(getter: (hydra.core.Type => Option[T0]))(graph: hydra.graph.Graph)(name: hydra.core.Name): Either[hydra.context.InContext[hydra.errors.Error], T0] =
  {
  def rawType(t: hydra.core.Type): hydra.core.Type =
    t match
    case hydra.core.Type.annotated(v_Type_annotated_at) => rawType(v_Type_annotated_at.body)
    case hydra.core.Type.forall(v_Type_forall_ft) => rawType(v_Type_forall_ft.body)
    case _ => t
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.core.Type, T0](hydra.schemas.requireType(cx)(graph)(name))((t: hydra.core.Type) =>
    hydra.lib.maybes.maybe[Either[hydra.context.InContext[hydra.errors.Error], T0], T0](Left(hydra.context.InContext(hydra.errors.Error.other(hydra.lib.strings.cat(Seq(name, " does not resolve to a ", label, " type: ", hydra.show.core.`type`(t)))), cx)))((x: T0) => Right(x))(getter(rawType(t))))
}

def requireSchemaType(cx: hydra.context.Context)(types: Map[hydra.core.Name, hydra.core.TypeScheme])(tname: hydra.core.Name): Either[hydra.context.InContext[hydra.errors.Error], Tuple2[hydra.core.TypeScheme, hydra.context.Context]] =
  hydra.lib.maybes.maybe[Either[hydra.context.InContext[hydra.errors.Error], Tuple2[hydra.core.TypeScheme, hydra.context.Context]], hydra.core.TypeScheme](Left(hydra.context.InContext(hydra.errors.Error.other(hydra.lib.strings.cat(Seq("No such schema type: ", tname, ". Available types are: ", hydra.lib.strings.intercalate(", ")(hydra.lib.lists.map[hydra.core.Name, scala.Predef.String]((x) => x)(hydra.lib.maps.keys[hydra.core.Name, hydra.core.TypeScheme](types)))))), cx)))((ts: hydra.core.TypeScheme) =>
  Right(hydra.schemas.instantiateTypeScheme(cx)(hydra.rewriting.deannotateTypeSchemeRecursive(ts))))(hydra.lib.maps.lookup[hydra.core.Name, hydra.core.TypeScheme](tname)(types))

def requireType(cx: hydra.context.Context)(graph: hydra.graph.Graph)(name: hydra.core.Name): Either[hydra.context.InContext[hydra.errors.Error], hydra.core.Type] =
  hydra.lib.maybes.maybe[Either[hydra.context.InContext[hydra.errors.Error], hydra.core.Type], hydra.core.TypeScheme](hydra.lib.maybes.maybe[Either[hydra.context.InContext[hydra.errors.Error], hydra.core.Type], hydra.core.TypeScheme](Left(hydra.context.InContext(hydra.errors.Error.other(hydra.lib.strings.cat2("no such type: ")(name)), cx)))((ts: hydra.core.TypeScheme) => Right(hydra.rewriting.typeSchemeToFType(ts)))(hydra.lib.maps.lookup[hydra.core.Name, hydra.core.TypeScheme](name)(graph.boundTypes)))((ts: hydra.core.TypeScheme) => Right(hydra.rewriting.typeSchemeToFType(ts)))(hydra.lib.maps.lookup[hydra.core.Name, hydra.core.TypeScheme](name)(graph.schemaTypes))

def requireUnionField(cx: hydra.context.Context)(graph: hydra.graph.Graph)(tname: hydra.core.Name)(fname: hydra.core.Name): Either[hydra.context.InContext[hydra.errors.Error], hydra.core.Type] =
  {
  def withRowType(rt: Seq[hydra.core.FieldType]): Either[hydra.context.InContext[hydra.errors.Error], hydra.core.Type] =
    {
    lazy val matches: Seq[hydra.core.FieldType] = hydra.lib.lists.filter[hydra.core.FieldType]((ft: hydra.core.FieldType) => hydra.lib.equality.equal[hydra.core.Name](ft.name)(fname))(rt)
    hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.Error], hydra.core.Type]](hydra.lib.lists.`null`[hydra.core.FieldType](matches))(Left(hydra.context.InContext(hydra.errors.Error.other(hydra.lib.strings.cat(Seq("no field \"", fname, "\" in union type \"", tname))), cx)))(Right(hydra.lib.lists.head[hydra.core.FieldType](matches).`type`))
  }
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Seq[hydra.core.FieldType], hydra.core.Type](hydra.schemas.requireUnionType(cx)(graph)(tname))(withRowType)
}

def requireUnionType(cx: hydra.context.Context)(graph: hydra.graph.Graph)(name: hydra.core.Name): Either[hydra.context.InContext[hydra.errors.Error], Seq[hydra.core.FieldType]] =
  {
  def toUnion(t: hydra.core.Type): Option[Seq[hydra.core.FieldType]] =
    t match
    case hydra.core.Type.union(v_Type_union_rt) => Some(v_Type_union_rt)
    case _ => None
  hydra.schemas.requireRowType(cx)("union")(toUnion)(graph)(name)
}

def resolveType(graph: hydra.graph.Graph)(typ: hydra.core.Type): Option[hydra.core.Type] =
  hydra.rewriting.deannotateType(typ) match
  case hydra.core.Type.variable(v_Type_variable_name) => hydra.lib.maybes.maybe[Option[hydra.core.Type], hydra.core.TypeScheme](hydra.lib.maybes.map[hydra.core.TypeScheme, hydra.core.Type]((ts: hydra.core.TypeScheme) => hydra.rewriting.typeSchemeToFType(ts))(hydra.lib.maps.lookup[hydra.core.Name, hydra.core.TypeScheme](v_Type_variable_name)(graph.boundTypes)))((ts: hydra.core.TypeScheme) => Some(hydra.rewriting.typeSchemeToFType(ts)))(hydra.lib.maps.lookup[hydra.core.Name, hydra.core.TypeScheme](v_Type_variable_name)(graph.schemaTypes))
  case _ => Some(typ)

def schemaGraphToTypingEnvironment(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.context.InContext[hydra.errors.Error], Map[hydra.core.Name, hydra.core.TypeScheme]] =
  {
  def toTypeScheme(vars: Seq[hydra.core.Name])(typ: hydra.core.Type): hydra.core.TypeScheme =
    hydra.rewriting.deannotateType(typ) match
    case hydra.core.Type.forall(v_Type_forall_ft) => toTypeScheme(hydra.lib.lists.cons[hydra.core.Name](v_Type_forall_ft.parameter)(vars))(v_Type_forall_ft.body)
    case _ => hydra.core.TypeScheme(hydra.lib.lists.reverse[hydra.core.Name](vars), typ, None)
  def decodeType(term: hydra.core.Term): Either[hydra.context.InContext[hydra.errors.Error], hydra.core.Type] =
    hydra.lib.eithers.bimap[hydra.errors.Error, hydra.core.Type, hydra.context.InContext[hydra.errors.Error], hydra.core.Type]((_wc_e: hydra.errors.Error) => hydra.context.InContext(_wc_e, cx))((_wc_a: hydra.core.Type) => _wc_a)(hydra.lib.eithers.bimap[hydra.errors.DecodingError, hydra.core.Type, hydra.errors.Error, hydra.core.Type]((_e: hydra.errors.DecodingError) => hydra.errors.Error.other(_e))((_a: hydra.core.Type) => _a)(hydra.decode.core.`type`(g)(term)))
  def decodeTypeScheme(term: hydra.core.Term): Either[hydra.context.InContext[hydra.errors.Error], hydra.core.TypeScheme] =
    hydra.lib.eithers.bimap[hydra.errors.Error, hydra.core.TypeScheme, hydra.context.InContext[hydra.errors.Error], hydra.core.TypeScheme]((_wc_e: hydra.errors.Error) => hydra.context.InContext(_wc_e, cx))((_wc_a: hydra.core.TypeScheme) => _wc_a)(hydra.lib.eithers.bimap[hydra.errors.DecodingError, hydra.core.TypeScheme, hydra.errors.Error, hydra.core.TypeScheme]((_e: hydra.errors.DecodingError) => hydra.errors.Error.other(_e))((_a: hydra.core.TypeScheme) => _a)(hydra.decode.core.typeScheme(g)(term)))
  def toPair(el: hydra.core.Binding): Either[hydra.context.InContext[hydra.errors.Error], Option[Tuple2[hydra.core.Name, hydra.core.TypeScheme]]] =
    {
    def forTerm(term: hydra.core.Term): Either[hydra.context.InContext[hydra.errors.Error], Option[hydra.core.TypeScheme]] =
      term match
      case hydra.core.Term.record(v_Term_record_r) => hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.Error], Option[hydra.core.TypeScheme]]](hydra.lib.equality.equal[hydra.core.Name](v_Term_record_r.typeName)("hydra.core.TypeScheme"))(hydra.lib.eithers.map[hydra.core.TypeScheme, Option[hydra.core.TypeScheme], hydra.context.InContext[hydra.errors.Error]](hydra.lib.maybes.pure[hydra.core.TypeScheme])(decodeTypeScheme(el.term)))(Right(None))
      case hydra.core.Term.union(v_Term_union_i) => hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.Error], Option[hydra.core.TypeScheme]]](hydra.lib.equality.equal[hydra.core.Name](v_Term_union_i.typeName)("hydra.core.Type"))(hydra.lib.eithers.map[hydra.core.Type, Option[hydra.core.TypeScheme], hydra.context.InContext[hydra.errors.Error]]((decoded: hydra.core.Type) => Some(toTypeScheme(Seq())(decoded)))(decodeType(el.term)))(Right(None))
      case _ => Right(None)
    hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Option[hydra.core.TypeScheme], Option[Tuple2[hydra.core.Name, hydra.core.TypeScheme]]](hydra.lib.maybes.maybe[Either[hydra.context.InContext[hydra.errors.Error], Option[hydra.core.TypeScheme]], hydra.core.TypeScheme](hydra.lib.eithers.map[hydra.core.Type, Option[hydra.core.TypeScheme], hydra.context.InContext[hydra.errors.Error]]((typ: hydra.core.Type) => Some(hydra.rewriting.fTypeToTypeScheme(typ)))(decodeType(el.term)))((ts: hydra.core.TypeScheme) =>
      hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.Error], Option[hydra.core.TypeScheme]]](hydra.lib.equality.equal[hydra.core.TypeScheme](ts)(hydra.core.TypeScheme(Seq(), hydra.core.Type.variable("hydra.core.TypeScheme"), None)))(hydra.lib.eithers.map[hydra.core.TypeScheme, Option[hydra.core.TypeScheme], hydra.context.InContext[hydra.errors.Error]](hydra.lib.maybes.pure[hydra.core.TypeScheme])(decodeTypeScheme(el.term)))(hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.Error], Option[hydra.core.TypeScheme]]](hydra.lib.equality.equal[hydra.core.TypeScheme](ts)(hydra.core.TypeScheme(Seq(), hydra.core.Type.variable("hydra.core.Type"), None)))(hydra.lib.eithers.map[hydra.core.Type, Option[hydra.core.TypeScheme], hydra.context.InContext[hydra.errors.Error]]((decoded: hydra.core.Type) => Some(toTypeScheme(Seq())(decoded)))(decodeType(el.term)))(forTerm(hydra.rewriting.deannotateTerm(el.term)))))(el.`type`))((mts: Option[hydra.core.TypeScheme]) =>
      Right(hydra.lib.maybes.map[hydra.core.TypeScheme, Tuple2[hydra.core.Name, hydra.core.TypeScheme]]((ts: hydra.core.TypeScheme) => Tuple2(el.name, ts))(mts)))
  }
  hydra.lib.eithers.map[Seq[Option[Tuple2[hydra.core.Name, hydra.core.TypeScheme]]], Map[hydra.core.Name, hydra.core.TypeScheme], hydra.context.InContext[hydra.errors.Error]]((mpairs: Seq[Option[Tuple2[hydra.core.Name, hydra.core.TypeScheme]]]) =>
    hydra.lib.maps.fromList[hydra.core.Name, hydra.core.TypeScheme](hydra.lib.maybes.cat[Tuple2[hydra.core.Name, hydra.core.TypeScheme]](mpairs)))(hydra.lib.eithers.mapList[hydra.core.Binding, Option[Tuple2[hydra.core.Name, hydra.core.TypeScheme]], hydra.context.InContext[hydra.errors.Error]](toPair)(hydra.lexical.graphToBindings(g)))
}

def termAsBindings(term: hydra.core.Term): Seq[hydra.core.Binding] =
  hydra.rewriting.deannotateTerm(term) match
  case hydra.core.Term.let(v_Term_let_lt) => (v_Term_let_lt.bindings)
  case _ => Seq()

def topologicalSortTypeDefinitions(defs: Seq[hydra.module.TypeDefinition]): Seq[Seq[hydra.module.TypeDefinition]] =
  {
  def toPair(`def`: hydra.module.TypeDefinition): Tuple2[hydra.core.Name, Seq[hydra.core.Name]] =
    Tuple2(`def`.name, hydra.lib.sets.toList[hydra.core.Name](hydra.rewriting.typeDependencyNames(false)(`def`.`type`)))
  lazy val nameToDef: Map[hydra.core.Name, hydra.module.TypeDefinition] = hydra.lib.maps.fromList[hydra.core.Name, hydra.module.TypeDefinition](hydra.lib.lists.map[hydra.module.TypeDefinition, Tuple2[hydra.core.Name, hydra.module.TypeDefinition]]((d: hydra.module.TypeDefinition) => Tuple2(d.name, d))(defs))
  lazy val sorted: Seq[Seq[hydra.core.Name]] = hydra.sorting.topologicalSortComponents(hydra.lib.lists.map[hydra.module.TypeDefinition, Tuple2[hydra.core.Name, Seq[hydra.core.Name]]](toPair)(defs))
  hydra.lib.lists.map[Seq[hydra.core.Name], Seq[hydra.module.TypeDefinition]]((names: Seq[hydra.core.Name]) =>
    hydra.lib.maybes.cat[hydra.module.TypeDefinition](hydra.lib.lists.map[hydra.core.Name, Option[hydra.module.TypeDefinition]]((n: hydra.core.Name) =>
    hydra.lib.maps.lookup[hydra.core.Name, hydra.module.TypeDefinition](n)(nameToDef))(names)))(sorted)
}

def typeDependencies(cx: hydra.context.Context)(graph: hydra.graph.Graph)(withSchema: Boolean)(transform: (hydra.core.Type => hydra.core.Type))(name: hydra.core.Name): Either[hydra.context.InContext[hydra.errors.Error], Map[hydra.core.Name, hydra.core.Type]] =
  {
  def requireType(name2: hydra.core.Name): Either[hydra.context.InContext[hydra.errors.Error], hydra.core.Type] =
    {
    lazy val cx1: hydra.context.Context = hydra.context.Context(hydra.lib.lists.cons[scala.Predef.String](hydra.lib.strings.cat2("type dependencies of ")(name2))(cx.trace), (cx.messages), (cx.other))
    hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.core.Binding, hydra.core.Type](hydra.lexical.requireElement(cx1)(graph)(name2))((el: hydra.core.Binding) =>
      hydra.lib.eithers.bimap[hydra.errors.Error, hydra.core.Type, hydra.context.InContext[hydra.errors.Error], hydra.core.Type]((_wc_e: hydra.errors.Error) => hydra.context.InContext(_wc_e, cx1))((_wc_a: hydra.core.Type) => _wc_a)(hydra.lib.eithers.bimap[hydra.errors.DecodingError, hydra.core.Type, hydra.errors.Error, hydra.core.Type]((_e: hydra.errors.DecodingError) => hydra.errors.Error.other(_e))((_a: hydra.core.Type) => _a)(hydra.decode.core.`type`(graph)(el.term))))
  }
  def toPair(name2: hydra.core.Name): Either[hydra.context.InContext[hydra.errors.Error], Tuple2[hydra.core.Name, hydra.core.Type]] =
    hydra.lib.eithers.map[hydra.core.Type, Tuple2[hydra.core.Name, hydra.core.Type], hydra.context.InContext[hydra.errors.Error]]((typ: hydra.core.Type) => Tuple2(name2, transform(typ)))(requireType(name2))
  def deps(seeds: scala.collection.immutable.Set[hydra.core.Name])(names: Map[hydra.core.Name, hydra.core.Type]): Either[hydra.context.InContext[hydra.errors.Error], Map[hydra.core.Name, hydra.core.Type]] =
    hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.Error], Map[hydra.core.Name, hydra.core.Type]]](hydra.lib.sets.`null`[hydra.core.Name](seeds))(Right(names))(hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Seq[Tuple2[hydra.core.Name, hydra.core.Type]], Map[hydra.core.Name, hydra.core.Type]](hydra.lib.eithers.mapList[hydra.core.Name, Tuple2[hydra.core.Name, hydra.core.Type], hydra.context.InContext[hydra.errors.Error]](toPair)(hydra.lib.sets.toList[hydra.core.Name](seeds)))((pairs: Seq[Tuple2[hydra.core.Name, hydra.core.Type]]) =>
    {
    lazy val newNames: Map[hydra.core.Name, hydra.core.Type] = hydra.lib.maps.union[hydra.core.Name, hydra.core.Type](names)(hydra.lib.maps.fromList[hydra.core.Name, hydra.core.Type](pairs))
    {
      lazy val refs: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.lists.foldl[scala.collection.immutable.Set[hydra.core.Name], scala.collection.immutable.Set[hydra.core.Name]](hydra.lib.sets.union[hydra.core.Name])(hydra.lib.sets.empty[hydra.core.Name])(hydra.lib.lists.map[Tuple2[hydra.core.Name, hydra.core.Type], scala.collection.immutable.Set[hydra.core.Name]]((pair: Tuple2[hydra.core.Name, hydra.core.Type]) =>
        hydra.rewriting.typeDependencyNames(withSchema)(hydra.lib.pairs.second[hydra.core.Name, hydra.core.Type](pair)))(pairs))
      {
        lazy val visited: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.sets.fromList[hydra.core.Name](hydra.lib.maps.keys[hydra.core.Name, hydra.core.Type](names))
        {
          lazy val newSeeds: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.sets.difference[hydra.core.Name](refs)(visited)
          deps(newSeeds)(newNames)
        }
      }
    }
  }))
  deps(hydra.lib.sets.singleton[hydra.core.Name](name))(hydra.lib.maps.empty[hydra.core.Name, hydra.core.Type])
}

def typeToTypeScheme(t0: hydra.core.Type): hydra.core.TypeScheme =
  {
  def helper(vars: Seq[hydra.core.Name])(t: hydra.core.Type): hydra.core.TypeScheme =
    hydra.rewriting.deannotateType(t) match
    case hydra.core.Type.forall(v_Type_forall_ft) => helper(hydra.lib.lists.cons[hydra.core.Name](v_Type_forall_ft.parameter)(vars))(v_Type_forall_ft.body)
    case _ => hydra.core.TypeScheme(hydra.lib.lists.reverse[hydra.core.Name](vars), t, None)
  helper(Seq())(t0)
}

def typesToElements(typeMap: Map[hydra.core.Name, hydra.core.Type]): Seq[hydra.core.Binding] =
  {
  def toElement(pair: Tuple2[hydra.core.Name, hydra.core.Type]): hydra.core.Binding =
    {
    lazy val name: hydra.core.Name = hydra.lib.pairs.first[hydra.core.Name, hydra.core.Type](pair)
    hydra.core.Binding(name, hydra.encode.core.`type`(hydra.lib.pairs.second[hydra.core.Name, hydra.core.Type](pair)), None)
  }
  hydra.lib.lists.map[Tuple2[hydra.core.Name, hydra.core.Type], hydra.core.Binding](toElement)(hydra.lib.maps.toList[hydra.core.Name, hydra.core.Type](typeMap))
}

def withLambdaContext[T0, T1, T2](getContext: (T0 => hydra.graph.Graph))(setContext: (hydra.graph.Graph => T0 => T1))(env: T0)(lam: hydra.core.Lambda)(body: (T1 => T2)): T2 =
  {
  lazy val newContext: hydra.graph.Graph = hydra.rewriting.extendGraphForLambda(getContext(env))(lam)
  body(setContext(newContext)(env))
}

def withLetContext[T0, T1, T2](getContext: (T0 => hydra.graph.Graph))(setContext: (hydra.graph.Graph => T0 => T1))(forBinding: (hydra.graph.Graph => hydra.core.Binding => Option[hydra.core.Term]))(env: T0)(letrec: hydra.core.Let)(body: (T1 => T2)): T2 =
  {
  lazy val newContext: hydra.graph.Graph = hydra.rewriting.extendGraphForLet(forBinding)(getContext(env))(letrec)
  body(setContext(newContext)(env))
}

def withTypeLambdaContext[T0, T1, T2](getContext: (T0 => hydra.graph.Graph))(setContext: (hydra.graph.Graph => T0 => T1))(env: T0)(tlam: hydra.core.TypeLambda)(body: (T1 => T2)): T2 =
  {
  lazy val newContext: hydra.graph.Graph = hydra.rewriting.extendGraphForTypeLambda(getContext(env))(tlam)
  body(setContext(newContext)(env))
}
