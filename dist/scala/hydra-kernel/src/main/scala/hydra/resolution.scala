package hydra.resolution

import hydra.core.*

import hydra.errors.*

import hydra.graph.*

import hydra.typing.*

def dereferenceType[T0](cx: T0)(graph: hydra.graph.Graph)(name: hydra.core.Name): Either[hydra.errors.Error, Option[hydra.core.Type]] =
  {
  lazy val mel: Option[hydra.core.Binding] = hydra.lexical.lookupBinding(graph)(name)
  hydra.lib.maybes.maybe[Either[hydra.errors.Error, Option[hydra.core.Type]], hydra.core.Binding](Right(None))((el: hydra.core.Binding) =>
    hydra.lib.eithers.map[hydra.core.Type, Option[hydra.core.Type], hydra.errors.Error](hydra.lib.maybes.pure[hydra.core.Type])(hydra.lib.eithers.bimap[hydra.errors.DecodingError,
       hydra.core.Type, hydra.errors.Error, hydra.core.Type]((_e: hydra.errors.DecodingError) =>
    hydra.errors.Error.resolution(hydra.errors.ResolutionError.unexpectedShape(hydra.errors.UnexpectedShapeError("type",
       _e))))((_a: hydra.core.Type) => _a)(hydra.decode.core.`type`(graph)(el.term))))(mel)
}

def fTypeIsPolymorphic(typ: hydra.core.Type): Boolean =
  typ match
  case hydra.core.Type.annotated(v_Type_annotated_at) => hydra.resolution.fTypeIsPolymorphic(v_Type_annotated_at.body)
  case hydra.core.Type.forall(v_Type_forall_ft) => true
  case _ => false

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

def fieldTypes[T0](cx: T0)(graph: hydra.graph.Graph)(t: hydra.core.Type): Either[hydra.errors.Error, Map[hydra.core.Name, hydra.core.Type]] =
  {
  def toMap(fields: Seq[hydra.core.FieldType]): Map[hydra.core.Name, hydra.core.Type] =
    hydra.lib.maps.fromList[hydra.core.Name, hydra.core.Type](hydra.lib.lists.map[hydra.core.FieldType,
       Tuple2[hydra.core.Name, hydra.core.Type]]((ft: hydra.core.FieldType) => Tuple2(ft.name, (ft.`type`)))(fields))
  hydra.strip.deannotateType(t) match
    case hydra.core.Type.forall(v_Type_forall_ft) => hydra.resolution.fieldTypes(cx)(graph)(v_Type_forall_ft.body)
    case hydra.core.Type.record(v_Type_record_rt) => Right(toMap(v_Type_record_rt))
    case hydra.core.Type.union(v_Type_union_rt) => Right(toMap(v_Type_union_rt))
    case hydra.core.Type.variable(v_Type_variable_name) => hydra.lib.maybes.maybe[Either[hydra.errors.Error,
       Map[hydra.core.Name, hydra.core.Type]], hydra.core.TypeScheme](hydra.lib.eithers.bind[hydra.errors.Error,
       hydra.core.Binding, Map[hydra.core.Name, hydra.core.Type]](hydra.lexical.requireBinding(graph)(v_Type_variable_name))((el: hydra.core.Binding) =>
      hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Type, Map[hydra.core.Name, hydra.core.Type]](hydra.lib.eithers.bimap[hydra.errors.DecodingError,
         hydra.core.Type, hydra.errors.Error, hydra.core.Type]((_e: hydra.errors.DecodingError) =>
      hydra.errors.Error.resolution(hydra.errors.ResolutionError.unexpectedShape(hydra.errors.UnexpectedShapeError("type",
         _e))))((_a: hydra.core.Type) => _a)(hydra.decode.core.`type`(graph)(el.term)))((decodedType: hydra.core.Type) => hydra.resolution.fieldTypes(cx)(graph)(decodedType))))((ts: hydra.core.TypeScheme) => hydra.resolution.fieldTypes(cx)(graph)(ts.`type`))(hydra.lib.maps.lookup[hydra.core.Name,
         hydra.core.TypeScheme](v_Type_variable_name)(graph.schemaTypes))
    case _ => Left(hydra.errors.Error.resolution(hydra.errors.ResolutionError.unexpectedShape(hydra.errors.UnexpectedShapeError("record or union type",
       hydra.show.core.`type`(t)))))
}

def findFieldType[T0](cx: T0)(fname: hydra.core.Name)(fields: Seq[hydra.core.FieldType]): Either[hydra.errors.Error, hydra.core.Type] =
  {
  lazy val matchingFields: Seq[hydra.core.FieldType] = hydra.lib.lists.filter[hydra.core.FieldType]((ft: hydra.core.FieldType) =>
    hydra.lib.equality.equal[scala.Predef.String](ft.name)(fname))(fields)
  hydra.lib.logic.ifElse[Either[hydra.errors.Error, hydra.core.Type]](hydra.lib.lists.`null`[hydra.core.FieldType](matchingFields))(Left(hydra.errors.Error.resolution(hydra.errors.ResolutionError.noMatchingField(hydra.errors.NoMatchingFieldError(fname)))))(hydra.lib.logic.ifElse[Either[hydra.errors.Error,
     hydra.core.Type]](hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.core.FieldType](matchingFields))(1))(Right(hydra.lib.lists.head[hydra.core.FieldType](matchingFields).`type`))(Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.multipleFields(hydra.errors.MultipleFieldsError(fname))))))
}

def fullyStripAndNormalizeType(typ: hydra.core.Type): hydra.core.Type =
  {
  def go(depth: Int)(subst: Map[hydra.core.Name, hydra.core.Name])(t: hydra.core.Type): Tuple2[Map[hydra.core.Name, hydra.core.Name], hydra.core.Type] =
    hydra.strip.deannotateType(t) match
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
  hydra.variables.substituteTypeVariables(subst)(body)
}

def fullyStripType(typ: hydra.core.Type): hydra.core.Type =
  hydra.strip.deannotateType(typ) match
  case hydra.core.Type.forall(v_Type_forall_ft) => hydra.resolution.fullyStripType(v_Type_forall_ft.body)
  case _ => typ

def instantiateType(cx: hydra.context.Context)(typ: hydra.core.Type): Tuple2[hydra.core.Type, hydra.context.Context] =
  {
  lazy val result: Tuple2[hydra.core.TypeScheme, hydra.context.Context] = hydra.resolution.instantiateTypeScheme(cx)(hydra.resolution.typeToTypeScheme(typ))
  Tuple2(hydra.scoping.typeSchemeToFType(hydra.lib.pairs.first[hydra.core.TypeScheme, hydra.context.Context](result)),
     hydra.lib.pairs.second[hydra.core.TypeScheme, hydra.context.Context](result))
}

def instantiateTypeScheme(cx: hydra.context.Context)(scheme: hydra.core.TypeScheme): Tuple2[hydra.core.TypeScheme, hydra.context.Context] =
  {
  lazy val oldVars: Seq[hydra.core.Name] = (scheme.variables)
  lazy val result: Tuple2[Seq[hydra.core.Name], hydra.context.Context] = hydra.names.freshNames(hydra.lib.lists.length[hydra.core.Name](oldVars))(cx)
  lazy val newVars: Seq[hydra.core.Name] = hydra.lib.pairs.first[Seq[hydra.core.Name], hydra.context.Context](result)
  lazy val cx2: hydra.context.Context = hydra.lib.pairs.second[Seq[hydra.core.Name], hydra.context.Context](result)
  lazy val subst: hydra.typing.TypeSubst = hydra.lib.maps.fromList[hydra.core.Name, hydra.core.Type](hydra.lib.lists.zip[hydra.core.Name,
     hydra.core.Type](oldVars)(hydra.lib.lists.map[hydra.core.Name, hydra.core.Type]((x: hydra.core.Name) => hydra.core.Type.variable(x))(newVars)))
  lazy val nameSubst: Map[hydra.core.Name, hydra.core.Name] = hydra.lib.maps.fromList[hydra.core.Name,
     hydra.core.Name](hydra.lib.lists.zip[hydra.core.Name, hydra.core.Name](oldVars)(newVars))
  lazy val renamedConstraints: Option[Map[hydra.core.Name, hydra.core.TypeVariableMetadata]] = hydra.lib.maybes.map[Map[hydra.core.Name,
     hydra.core.TypeVariableMetadata], Map[hydra.core.Name, hydra.core.TypeVariableMetadata]]((oldConstraints: Map[hydra.core.Name,
     hydra.core.TypeVariableMetadata]) =>
    hydra.lib.maps.fromList[hydra.core.Name, hydra.core.TypeVariableMetadata](hydra.lib.lists.map[Tuple2[hydra.core.Name,
       hydra.core.TypeVariableMetadata], Tuple2[hydra.core.Name, hydra.core.TypeVariableMetadata]]((kv: Tuple2[hydra.core.Name,
       hydra.core.TypeVariableMetadata]) =>
    Tuple2(hydra.lib.maybes.fromMaybe[hydra.core.Name](hydra.lib.pairs.first[hydra.core.Name, hydra.core.TypeVariableMetadata](kv))(hydra.lib.maps.lookup[hydra.core.Name,
       hydra.core.Name](hydra.lib.pairs.first[hydra.core.Name, hydra.core.TypeVariableMetadata](kv))(nameSubst)),
       hydra.lib.pairs.second[hydra.core.Name, hydra.core.TypeVariableMetadata](kv)))(hydra.lib.maps.toList[hydra.core.Name,
       hydra.core.TypeVariableMetadata](oldConstraints))))(scheme.constraints)
  Tuple2(hydra.core.TypeScheme(newVars, hydra.substitution.substInType(subst)(scheme.`type`), renamedConstraints), cx2)
}

def nominalApplication(tname: hydra.core.Name)(args: Seq[hydra.core.Type]): hydra.core.Type =
  hydra.lib.lists.foldl[hydra.core.Type, hydra.core.Type]((t: hydra.core.Type) =>
  (a: hydra.core.Type) =>
  hydra.core.Type.application(hydra.core.ApplicationType(t, a)))(hydra.core.Type.variable(tname))(args)

def requireRecordType[T0](cx: T0)(graph: hydra.graph.Graph)(name: hydra.core.Name): Either[hydra.errors.Error, Seq[hydra.core.FieldType]] =
  {
  def toRecord(t: hydra.core.Type): Option[Seq[hydra.core.FieldType]] =
    t match
    case hydra.core.Type.record(v_Type_record_rt) => Some(v_Type_record_rt)
    case _ => None
  hydra.resolution.requireRowType(cx)("record type")(toRecord)(graph)(name)
}

def requireRowType[T0, T1](cx: T0)(label: scala.Predef.String)(getter: (hydra.core.Type => Option[T1]))(graph: hydra.graph.Graph)(name: hydra.core.Name): Either[hydra.errors.Error,
   T1] =
  {
  def rawType(t: hydra.core.Type): hydra.core.Type =
    t match
    case hydra.core.Type.annotated(v_Type_annotated_at) => rawType(v_Type_annotated_at.body)
    case hydra.core.Type.forall(v_Type_forall_ft) => rawType(v_Type_forall_ft.body)
    case _ => t
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Type, T1](hydra.resolution.requireType(cx)(graph)(name))((t: hydra.core.Type) =>
    hydra.lib.maybes.maybe[Either[hydra.errors.Error, T1], T1](Left(hydra.errors.Error.resolution(hydra.errors.ResolutionError.unexpectedShape(hydra.errors.UnexpectedShapeError(hydra.lib.strings.cat2(label)(" type"),
       hydra.lib.strings.cat2(name)(hydra.lib.strings.cat2(": ")(hydra.show.core.`type`(t))))))))((x: T1) => Right(x))(getter(rawType(t))))
}

def requireSchemaType(cx: hydra.context.Context)(types: Map[hydra.core.Name, hydra.core.TypeScheme])(tname: hydra.core.Name): Either[hydra.errors.Error,
   Tuple2[hydra.core.TypeScheme, hydra.context.Context]] =
  hydra.lib.maybes.maybe[Either[hydra.errors.Error, Tuple2[hydra.core.TypeScheme, hydra.context.Context]],
     hydra.core.TypeScheme](Left(hydra.errors.Error.resolution(hydra.errors.ResolutionError.noSuchBinding(hydra.errors.NoSuchBindingError(tname)))))((ts: hydra.core.TypeScheme) =>
  Right(hydra.resolution.instantiateTypeScheme(cx)(hydra.strip.deannotateTypeSchemeRecursive(ts))))(hydra.lib.maps.lookup[hydra.core.Name,
     hydra.core.TypeScheme](tname)(types))

def requireType[T0](cx: T0)(graph: hydra.graph.Graph)(name: hydra.core.Name): Either[hydra.errors.Error, hydra.core.Type] =
  hydra.lib.maybes.maybe[Either[hydra.errors.Error, hydra.core.Type], hydra.core.TypeScheme](hydra.lib.maybes.maybe[Either[hydra.errors.Error,
     hydra.core.Type], hydra.core.TypeScheme](Left(hydra.errors.Error.resolution(hydra.errors.ResolutionError.noSuchBinding(hydra.errors.NoSuchBindingError(name)))))((ts: hydra.core.TypeScheme) => Right(hydra.scoping.typeSchemeToFType(ts)))(hydra.lib.maps.lookup[hydra.core.Name,
     hydra.core.TypeScheme](name)(graph.boundTypes)))((ts: hydra.core.TypeScheme) => Right(hydra.scoping.typeSchemeToFType(ts)))(hydra.lib.maps.lookup[hydra.core.Name,
     hydra.core.TypeScheme](name)(graph.schemaTypes))

def requireUnionField[T0](cx: T0)(graph: hydra.graph.Graph)(tname: hydra.core.Name)(fname: hydra.core.Name): Either[hydra.errors.Error, hydra.core.Type] =
  {
  def withRowType(rt: Seq[hydra.core.FieldType]): Either[hydra.errors.Error, hydra.core.Type] =
    {
    lazy val matches: Seq[hydra.core.FieldType] = hydra.lib.lists.filter[hydra.core.FieldType]((ft: hydra.core.FieldType) => hydra.lib.equality.equal[hydra.core.Name](ft.name)(fname))(rt)
    hydra.lib.logic.ifElse[Either[hydra.errors.Error, hydra.core.Type]](hydra.lib.lists.`null`[hydra.core.FieldType](matches))(Left(hydra.errors.Error.resolution(hydra.errors.ResolutionError.noMatchingField(hydra.errors.NoMatchingFieldError(fname)))))(Right(hydra.lib.lists.head[hydra.core.FieldType](matches).`type`))
  }
  hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.core.FieldType], hydra.core.Type](hydra.resolution.requireUnionType(cx)(graph)(tname))(withRowType)
}

def requireUnionType[T0](cx: T0)(graph: hydra.graph.Graph)(name: hydra.core.Name): Either[hydra.errors.Error, Seq[hydra.core.FieldType]] =
  {
  def toUnion(t: hydra.core.Type): Option[Seq[hydra.core.FieldType]] =
    t match
    case hydra.core.Type.union(v_Type_union_rt) => Some(v_Type_union_rt)
    case _ => None
  hydra.resolution.requireRowType(cx)("union")(toUnion)(graph)(name)
}

def resolveType(graph: hydra.graph.Graph)(typ: hydra.core.Type): Option[hydra.core.Type] =
  hydra.strip.deannotateType(typ) match
  case hydra.core.Type.variable(v_Type_variable_name) => hydra.lib.maybes.maybe[Option[hydra.core.Type],
     hydra.core.TypeScheme](hydra.lib.maybes.map[hydra.core.TypeScheme, hydra.core.Type]((ts: hydra.core.TypeScheme) => hydra.scoping.typeSchemeToFType(ts))(hydra.lib.maps.lookup[hydra.core.Name,
     hydra.core.TypeScheme](v_Type_variable_name)(graph.boundTypes)))((ts: hydra.core.TypeScheme) => Some(hydra.scoping.typeSchemeToFType(ts)))(hydra.lib.maps.lookup[hydra.core.Name,
     hydra.core.TypeScheme](v_Type_variable_name)(graph.schemaTypes))
  case _ => Some(typ)

def typeToTypeScheme(t0: hydra.core.Type): hydra.core.TypeScheme =
  {
  def helper(vars: Seq[hydra.core.Name])(t: hydra.core.Type): hydra.core.TypeScheme =
    hydra.strip.deannotateType(t) match
    case hydra.core.Type.forall(v_Type_forall_ft) => helper(hydra.lib.lists.cons[hydra.core.Name](v_Type_forall_ft.parameter)(vars))(v_Type_forall_ft.body)
    case _ => hydra.core.TypeScheme(hydra.lib.lists.reverse[hydra.core.Name](vars), t, None)
  helper(Seq())(t0)
}
