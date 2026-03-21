package hydra.annotations

import hydra.classes.*

import hydra.context.*

import hydra.core.*

import hydra.error.*

import hydra.graph.*

import hydra.lib.eithers

import hydra.lib.equality

import hydra.lib.lists

import hydra.lib.logic

import hydra.lib.maps

import hydra.lib.math

import hydra.lib.maybes

import hydra.lib.pairs

import hydra.lib.sets

import hydra.lib.strings

def aggregateAnnotations[T0, T1, T2, T3](getValue: (T0 => Option[T1]))(getX: (T1 => T0))(getAnns: (T1 => Map[T2, T3]))(t: T0): Map[T2, T3] =
  {
  def toPairs(rest: Seq[Seq[Tuple2[T2, T3]]])(t2: T0): Seq[Seq[Tuple2[T2, T3]]] =
    maybes.maybe[Seq[Seq[Tuple2[T2, T3]]], T1](rest)((yy: T1) =>
    toPairs(lists.cons[Seq[Tuple2[T2, T3]]](maps.toList[T2, T3](getAnns(yy)))(rest))(getX(yy)))(getValue(t2))
  maps.fromList[T2, T3](lists.concat[Tuple2[T2, T3]](toPairs(Seq())(t)))
}

def debugIf(cx: hydra.context.Context)(debugId: scala.Predef.String)(message: scala.Predef.String): Either[hydra.context.InContext[hydra.error.Error], Unit] =
  eithers.bind[hydra.context.InContext[hydra.error.Error], Option[scala.Predef.String], Unit](hydra.annotations.getDebugId(cx))((mid: Option[scala.Predef.String]) =>
  logic.ifElse[Either[hydra.context.InContext[hydra.error.Error], Unit]](equality.equal[Option[scala.Predef.String]](mid)(Some(debugId)))(Left(hydra.context.InContext(hydra.error.Error.other(message),
     cx)))(Right(())))

def failOnFlag(cx: hydra.context.Context)(flag: hydra.core.Name)(msg: scala.Predef.String): Either[hydra.context.InContext[hydra.error.Error], Unit] =
  eithers.bind[hydra.context.InContext[hydra.error.Error], Boolean, Unit](hydra.annotations.hasFlag(cx)(flag))((`val`: Boolean) =>
  logic.ifElse[Either[hydra.context.InContext[hydra.error.Error], Unit]](`val`)(Left(hydra.context.InContext(hydra.error.Error.other(msg), cx)))(Right(())))

def getDebugId(cx: hydra.context.Context): Either[hydra.context.InContext[hydra.error.Error], Option[scala.Predef.String]] =
  maybes.maybe[Either[hydra.context.InContext[hydra.error.Error], Option[scala.Predef.String]], hydra.core.Term](Right(None))((term: hydra.core.Term) =>
  eithers.map[scala.Predef.String, Option[scala.Predef.String], hydra.context.InContext[hydra.error.Error]](maybes.pure[scala.Predef.String])(hydra.extract.core.string(cx)(hydra.graph.Graph(maps.empty[hydra.core.Name,
     hydra.core.Term], maps.empty[hydra.core.Name, hydra.core.TypeScheme], maps.empty[hydra.core.Name,
     hydra.core.TypeVariableMetadata], sets.empty[hydra.core.Name], maps.empty[hydra.core.Name, hydra.core.Term],
     maps.empty[hydra.core.Name, hydra.graph.Primitive], maps.empty[hydra.core.Name, hydra.core.TypeScheme],
     sets.empty[hydra.core.Name]))(term)))(hydra.annotations.getAttr(hydra.constants.key_debugId)(cx))

def getAttr(key: hydra.core.Name)(cx: hydra.context.Context): Option[hydra.core.Term] = maps.lookup[hydra.core.Name, hydra.core.Term](key)(cx.other)

def getAttrWithDefault(key: hydra.core.Name)(`def`: hydra.core.Term)(cx: hydra.context.Context): hydra.core.Term = maybes.fromMaybe[hydra.core.Term](`def`)(hydra.annotations.getAttr(key)(cx))

def getCount(key: hydra.core.Name)(cx: hydra.context.Context): Int =
  maybes.maybe[Int, hydra.core.Term](0)((term: hydra.core.Term) =>
  term match
  case hydra.core.Term.literal(v_Term_literal_lit) => v_Term_literal_lit match
    case hydra.core.Literal.integer(v_Literal_integer_iv) => v_Literal_integer_iv match
      case hydra.core.IntegerValue.int32(v_IntegerValue_int32_i) => v_IntegerValue_int32_i
      case _ => 0
    case _ => 0
  case _ => 0)(maps.lookup[hydra.core.Name, hydra.core.Term](key)(cx.other))

def getDescription(cx: hydra.context.Context)(graph: hydra.graph.Graph)(anns: Map[hydra.core.Name, hydra.core.Term]): Either[hydra.context.InContext[hydra.error.Error],
   Option[scala.Predef.String]] =
  maybes.maybe[Either[hydra.context.InContext[hydra.error.Error], Option[scala.Predef.String]], hydra.core.Term](Right(None))((term: hydra.core.Term) =>
  eithers.map[scala.Predef.String, Option[scala.Predef.String], hydra.context.InContext[hydra.error.Error]](maybes.pure[scala.Predef.String])(hydra.extract.core.string(cx)(graph)(term)))(maps.lookup[hydra.core.Name,
     hydra.core.Term]("description")(anns))

def getTermAnnotation(key: hydra.core.Name)(term: hydra.core.Term): Option[hydra.core.Term] =
  maps.lookup[hydra.core.Name, hydra.core.Term](key)(hydra.annotations.termAnnotationInternal(term))

def getTermDescription(cx: hydra.context.Context)(graph: hydra.graph.Graph)(term: hydra.core.Term): Either[hydra.context.InContext[hydra.error.Error],
   Option[scala.Predef.String]] =
  {
  def peel(t: hydra.core.Term): hydra.core.Term =
    t match
    case hydra.core.Term.typeLambda(v_Term_typeLambda_tl) => peel(v_Term_typeLambda_tl.body)
    case hydra.core.Term.typeApplication(v_Term_typeApplication_ta) => peel(v_Term_typeApplication_ta.body)
    case _ => t
  hydra.annotations.getDescription(cx)(graph)(hydra.annotations.termAnnotationInternal(peel(term)))
}

def getType(graph: hydra.graph.Graph)(anns: Map[hydra.core.Name, hydra.core.Term]): Either[hydra.error.DecodingError, Option[hydra.core.Type]] =
  maybes.maybe[Either[hydra.error.DecodingError, Option[hydra.core.Type]], hydra.core.Term](Right(None))((dat: hydra.core.Term) =>
  eithers.map[hydra.core.Type, Option[hydra.core.Type], hydra.error.DecodingError](maybes.pure[hydra.core.Type])(hydra.decode.core.`type`(graph)(dat)))(maps.lookup[hydra.core.Name,
     hydra.core.Term](hydra.constants.key_type)(anns))

def getTypeAnnotation(key: hydra.core.Name)(typ: hydra.core.Type): Option[hydra.core.Term] =
  maps.lookup[hydra.core.Name, hydra.core.Term](key)(hydra.annotations.typeAnnotationInternal(typ))

def getTypeClasses(cx: hydra.context.Context)(graph: hydra.graph.Graph)(term: hydra.core.Term): Either[hydra.context.InContext[hydra.error.Error],
   Map[hydra.core.Name, scala.collection.immutable.Set[hydra.classes.TypeClass]]] =
  {
  def decodeClass(term2: hydra.core.Term): Either[hydra.context.InContext[hydra.error.Error], hydra.classes.TypeClass] =
    {
    val byName: Map[hydra.core.Name, hydra.classes.TypeClass] = maps.fromList[hydra.core.Name, hydra.classes.TypeClass](Seq(Tuple2("equality",
       hydra.classes.TypeClass.equality), Tuple2("ordering", hydra.classes.TypeClass.ordering)))
    eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.core.Name, hydra.classes.TypeClass](hydra.extract.core.unitVariant(cx)("hydra.classes.TypeClass")(graph)(term2))((fn: hydra.core.Name) =>
      maybes.maybe[Either[hydra.context.InContext[hydra.error.Error], hydra.classes.TypeClass], hydra.classes.TypeClass](Left(hydra.context.InContext(hydra.error.Error.other(strings.cat2("unexpected: expected type class, got ")(hydra.show.core.term(term2))),
         cx)))((x: hydra.classes.TypeClass) => Right(x))(maps.lookup[hydra.core.Name, hydra.classes.TypeClass](fn)(byName)))
  }
  maybes.maybe[Either[hydra.context.InContext[hydra.error.Error], Map[hydra.core.Name, scala.collection.immutable.Set[hydra.classes.TypeClass]]],
     hydra.core.Term](Right(maps.empty[hydra.core.Name, scala.collection.immutable.Set[hydra.classes.TypeClass]]))((term2: hydra.core.Term) =>
    hydra.extract.core.map(cx)((t: hydra.core.Term) =>
    eithers.bimap[hydra.error.DecodingError, hydra.core.Name, hydra.context.InContext[hydra.error.Error],
       hydra.core.Name]((de: hydra.error.DecodingError) => hydra.context.InContext(hydra.error.Error.other(de),
       cx))((x: hydra.core.Name) => x)(hydra.decode.core.name(graph)(t)))((v1: hydra.core.Term) => hydra.extract.core.setOf(cx)(decodeClass)(graph)(v1))(graph)(term2))(hydra.annotations.getTermAnnotation(hydra.constants.key_classes)(term))
}

def getTypeDescription(cx: hydra.context.Context)(graph: hydra.graph.Graph)(typ: hydra.core.Type): Either[hydra.context.InContext[hydra.error.Error],
   Option[scala.Predef.String]] =
  hydra.annotations.getDescription(cx)(graph)(hydra.annotations.typeAnnotationInternal(typ))

def isNativeType(el: hydra.core.Binding): Boolean =
  {
  val isFlaggedAsFirstClassType: Boolean = maybes.fromMaybe[Boolean](false)(maybes.map[hydra.core.Term,
     Boolean]((_x: hydra.core.Term) => true)(hydra.annotations.getTermAnnotation(hydra.constants.key_firstClassType)(el.term)))
  maybes.maybe[Boolean, hydra.core.TypeScheme](false)((ts: hydra.core.TypeScheme) =>
    logic.and(equality.equal[hydra.core.TypeScheme](ts)(hydra.core.TypeScheme(Seq(), hydra.core.Type.variable("hydra.core.Type"),
       None)))(logic.not(isFlaggedAsFirstClassType)))(el.`type`)
}

def hasDescription[T0](anns: Map[hydra.core.Name, T0]): Boolean =
  maybes.isJust[T0](maps.lookup[hydra.core.Name, T0](hydra.constants.key_description)(anns))

def hasFlag(cx: hydra.context.Context)(flag: hydra.core.Name): Either[hydra.context.InContext[hydra.error.Error], Boolean] =
  {
  val term: hydra.core.Term = hydra.annotations.getAttrWithDefault(flag)(hydra.core.Term.literal(hydra.core.Literal.boolean(false)))(cx)
  hydra.extract.core.boolean(cx)(hydra.graph.Graph(maps.empty[hydra.core.Name, hydra.core.Term], maps.empty[hydra.core.Name,
     hydra.core.TypeScheme], maps.empty[hydra.core.Name, hydra.core.TypeVariableMetadata], sets.empty[hydra.core.Name],
     maps.empty[hydra.core.Name, hydra.core.Term], maps.empty[hydra.core.Name, hydra.graph.Primitive],
     maps.empty[hydra.core.Name, hydra.core.TypeScheme], sets.empty[hydra.core.Name]))(term)
}

def hasTypeDescription(typ: hydra.core.Type): Boolean = hydra.annotations.hasDescription(hydra.annotations.typeAnnotationInternal(typ))

def nextCount(key: hydra.core.Name)(cx: hydra.context.Context): Tuple2[Int, hydra.context.Context] =
  {
  val count: Int = hydra.annotations.getCount(key)(cx)
  Tuple2(count, hydra.annotations.putCount(key)(math.add(count)(1))(cx))
}

def normalizeTermAnnotations(term: hydra.core.Term): hydra.core.Term =
  {
  val anns: Map[hydra.core.Name, hydra.core.Term] = hydra.annotations.termAnnotationInternal(term)
  val stripped: hydra.core.Term = hydra.rewriting.deannotateTerm(term)
  logic.ifElse[hydra.core.Term](maps.`null`[hydra.core.Name, hydra.core.Term](anns))(stripped)(hydra.core.Term.annotated(hydra.core.AnnotatedTerm(stripped,
     anns)))
}

def normalizeTypeAnnotations(typ: hydra.core.Type): hydra.core.Type =
  {
  val anns: Map[hydra.core.Name, hydra.core.Term] = hydra.annotations.typeAnnotationInternal(typ)
  val stripped: hydra.core.Type = hydra.rewriting.deannotateType(typ)
  logic.ifElse[hydra.core.Type](maps.`null`[hydra.core.Name, hydra.core.Term](anns))(stripped)(hydra.core.Type.annotated(hydra.core.AnnotatedType(stripped,
     anns)))
}

def putAttr(key: hydra.core.Name)(`val`: hydra.core.Term)(cx: hydra.context.Context): hydra.context.Context =
  hydra.context.Context(cx.trace, (cx.messages), maps.insert[hydra.core.Name, hydra.core.Term](key)(`val`)(cx.other))

def putCount(key: hydra.core.Name)(count: Int)(cx: hydra.context.Context): hydra.context.Context =
  hydra.annotations.putAttr(key)(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(count))))(cx)

def resetCount(key: hydra.core.Name)(cx: hydra.context.Context): hydra.context.Context =
  hydra.annotations.putAttr(key)(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(0))))(cx)

def setAnnotation[T0, T1](key: T0)(`val`: Option[T1])(m: Map[T0, T1]): Map[T0, T1] = maps.alter[T1, T0]((_x: Option[T1]) => `val`)(key)(m)

def setDescription(d: Option[scala.Predef.String])(v1: Map[hydra.core.Name, hydra.core.Term]): Map[hydra.core.Name, hydra.core.Term] =
  hydra.annotations.setAnnotation(hydra.constants.key_description)(maybes.map[scala.Predef.String, hydra.core.Term]((`arg_`: scala.Predef.String) => hydra.core.Term.literal(hydra.core.Literal.string(`arg_`)))(d))(v1)

def setTermAnnotation(key: hydra.core.Name)(`val`: Option[hydra.core.Term])(term: hydra.core.Term): hydra.core.Term =
  {
  val `term_`: hydra.core.Term = hydra.rewriting.deannotateTerm(term)
  val anns: Map[hydra.core.Name, hydra.core.Term] = hydra.annotations.setAnnotation(key)(`val`)(hydra.annotations.termAnnotationInternal(term))
  logic.ifElse[hydra.core.Term](maps.`null`[hydra.core.Name, hydra.core.Term](anns))(`term_`)(hydra.core.Term.annotated(hydra.core.AnnotatedTerm(`term_`,
     anns)))
}

def setTermDescription(d: Option[scala.Predef.String])(v1: hydra.core.Term): hydra.core.Term =
  hydra.annotations.setTermAnnotation(hydra.constants.key_description)(maybes.map[scala.Predef.String,
     hydra.core.Term]((s: scala.Predef.String) => hydra.core.Term.literal(hydra.core.Literal.string(s)))(d))(v1)

def setType(mt: Option[hydra.core.Type])(v1: Map[hydra.core.Name, hydra.core.Term]): Map[hydra.core.Name, hydra.core.Term] =
  hydra.annotations.setAnnotation(hydra.constants.key_type)(maybes.map[hydra.core.Type, hydra.core.Term](hydra.encode.core.`type`)(mt))(v1)

def setTypeAnnotation(key: hydra.core.Name)(`val`: Option[hydra.core.Term])(typ: hydra.core.Type): hydra.core.Type =
  {
  val `typ_`: hydra.core.Type = hydra.rewriting.deannotateType(typ)
  val anns: Map[hydra.core.Name, hydra.core.Term] = hydra.annotations.setAnnotation(key)(`val`)(hydra.annotations.typeAnnotationInternal(typ))
  logic.ifElse[hydra.core.Type](maps.`null`[hydra.core.Name, hydra.core.Term](anns))(`typ_`)(hydra.core.Type.annotated(hydra.core.AnnotatedType(`typ_`, anns)))
}

def setTypeClasses(m: Map[hydra.core.Name, scala.collection.immutable.Set[hydra.classes.TypeClass]])(term: hydra.core.Term): hydra.core.Term =
  {
  def encodeClass(tc: hydra.classes.TypeClass): hydra.core.Term =
    tc match
    case hydra.classes.TypeClass.equality => hydra.core.Term.union(hydra.core.Injection("hydra.classes.TypeClass",
       hydra.core.Field("equality", hydra.core.Term.unit)))
    case hydra.classes.TypeClass.ordering => hydra.core.Term.union(hydra.core.Injection("hydra.classes.TypeClass",
       hydra.core.Field("ordering", hydra.core.Term.unit)))
  def encodePair(nameClasses: Tuple2[hydra.core.Name, scala.collection.immutable.Set[hydra.classes.TypeClass]]): Tuple2[hydra.core.Term, hydra.core.Term] =
    {
    val name: hydra.core.Name = pairs.first[hydra.core.Name, scala.collection.immutable.Set[hydra.classes.TypeClass]](nameClasses)
    val classes: scala.collection.immutable.Set[hydra.classes.TypeClass] = pairs.second[hydra.core.Name,
       scala.collection.immutable.Set[hydra.classes.TypeClass]](nameClasses)
    Tuple2(hydra.encode.core.name(name), hydra.core.Term.set(sets.fromList[hydra.core.Term](lists.map[hydra.classes.TypeClass,
       hydra.core.Term](encodeClass)(sets.toList[hydra.classes.TypeClass](classes)))))
  }
  val encoded: Option[hydra.core.Term] = logic.ifElse[Option[hydra.core.Term]](maps.`null`[hydra.core.Name,
     scala.collection.immutable.Set[hydra.classes.TypeClass]](m))(None)(Some(hydra.core.Term.map(maps.fromList[hydra.core.Term,
     hydra.core.Term](lists.map[Tuple2[hydra.core.Name, scala.collection.immutable.Set[hydra.classes.TypeClass]],
     Tuple2[hydra.core.Term, hydra.core.Term]](encodePair)(maps.toList[hydra.core.Name, scala.collection.immutable.Set[hydra.classes.TypeClass]](m))))))
  hydra.annotations.setTermAnnotation(hydra.constants.key_classes)(encoded)(term)
}

def setTypeDescription(d: Option[scala.Predef.String])(v1: hydra.core.Type): hydra.core.Type =
  hydra.annotations.setTypeAnnotation(hydra.constants.key_description)(maybes.map[scala.Predef.String,
     hydra.core.Term]((`arg_`: scala.Predef.String) => hydra.core.Term.literal(hydra.core.Literal.string(`arg_`)))(d))(v1)

def termAnnotationInternal(term: hydra.core.Term): Map[hydra.core.Name, hydra.core.Term] =
  {
  def getAnn(t: hydra.core.Term): Option[hydra.core.AnnotatedTerm] =
    t match
    case hydra.core.Term.annotated(v_Term_annotated_a) => Some(v_Term_annotated_a)
    case _ => None
  hydra.annotations.aggregateAnnotations(getAnn)((at: hydra.core.AnnotatedTerm) => (at.body))((at: hydra.core.AnnotatedTerm) => (at.annotation))(term)
}

def typeAnnotationInternal(typ: hydra.core.Type): Map[hydra.core.Name, hydra.core.Term] =
  {
  def getAnn(t: hydra.core.Type): Option[hydra.core.AnnotatedType] =
    t match
    case hydra.core.Type.annotated(v_Type_annotated_a) => Some(v_Type_annotated_a)
    case _ => None
  hydra.annotations.aggregateAnnotations(getAnn)((at: hydra.core.AnnotatedType) => (at.body))((at: hydra.core.AnnotatedType) => (at.annotation))(typ)
}

def typeElement(name: hydra.core.Name)(typ: hydra.core.Type): hydra.core.Binding =
  {
  val schemaTerm: hydra.core.Term = hydra.core.Term.variable("hydra.core.Type")
  val dataTerm: hydra.core.Term = hydra.annotations.normalizeTermAnnotations(hydra.core.Term.annotated(hydra.core.AnnotatedTerm(hydra.encode.core.`type`(typ),
     maps.fromList[hydra.core.Name, hydra.core.Term](Seq(Tuple2(hydra.constants.key_type, schemaTerm))))))
  hydra.core.Binding(name, dataTerm, Some(hydra.core.TypeScheme(Seq(), hydra.core.Type.variable("hydra.core.Type"), None)))
}

def whenFlag[T0](cx: hydra.context.Context)(flag: hydra.core.Name)(ethen: Either[hydra.context.InContext[hydra.error.Error],
   T0])(eelse: Either[hydra.context.InContext[hydra.error.Error], T0]): Either[hydra.context.InContext[hydra.error.Error],
   T0] =
  eithers.bind[hydra.context.InContext[hydra.error.Error], Boolean, T0](hydra.annotations.hasFlag(cx)(flag))((b: Boolean) =>
  logic.ifElse[Either[hydra.context.InContext[hydra.error.Error], T0]](b)(ethen)(eelse))
