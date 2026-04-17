package hydra.dsls

import hydra.core.*

import hydra.errors.*

import hydra.packaging.*

def collectForallVars(typ: hydra.core.Type): Seq[hydra.core.Name] =
  typ match
  case hydra.core.Type.annotated(v_Type_annotated_at) => hydra.dsls.collectForallVars(v_Type_annotated_at.body)
  case hydra.core.Type.forall(v_Type_forall_ft) => hydra.lib.lists.cons[hydra.core.Name](v_Type_forall_ft.parameter)(hydra.dsls.collectForallVars(v_Type_forall_ft.body))
  case _ => Seq()

def deduplicateBindings(bindings: Seq[hydra.core.Binding]): Seq[hydra.core.Binding] =
  hydra.lib.lists.foldl[Seq[hydra.core.Binding], hydra.core.Binding]((acc: Seq[hydra.core.Binding]) =>
  (b: hydra.core.Binding) =>
  {
  lazy val n: scala.Predef.String = (b.name)
  {
    lazy val usedNames: Seq[scala.Predef.String] = hydra.lib.lists.map[hydra.core.Binding,
       scala.Predef.String]((a: hydra.core.Binding) => (a.name))(acc)
    {
      lazy val uniqueName: scala.Predef.String = hydra.dsls.findUniqueName(n)(usedNames)
      hydra.lib.lists.concat2[hydra.core.Binding](acc)(Seq(hydra.core.Binding(uniqueName, (b.term), (b.`type`))))
    }
  }
})(Seq())(bindings)

def dslBindingName(n: hydra.core.Name): hydra.core.Name =
  {
  lazy val parts: Seq[scala.Predef.String] = hydra.lib.strings.splitOn(".")(n)
  lazy val localPart: scala.Predef.String = hydra.formatting.decapitalize(hydra.names.localNameOf(n))
  lazy val localResult: hydra.core.Name = localPart
  hydra.lib.maybes.maybe[hydra.core.Name, Seq[scala.Predef.String]](localResult)((nsParts: Seq[scala.Predef.String]) =>
    hydra.lib.maybes.maybe[hydra.core.Name, Tuple2[scala.Predef.String, Seq[scala.Predef.String]]](localResult)((nsHeadTail: Tuple2[scala.Predef.String,
       Seq[scala.Predef.String]]) =>
    {
    lazy val dslNsParts: Seq[scala.Predef.String] = hydra.lib.logic.ifElse[Seq[scala.Predef.String]](hydra.lib.equality.equal[scala.Predef.String](hydra.lib.pairs.first[scala.Predef.String,
       Seq[scala.Predef.String]](nsHeadTail))("hydra"))(hydra.lib.lists.concat2[scala.Predef.String](Seq("hydra",
       "dsl"))(hydra.lib.pairs.second[scala.Predef.String, Seq[scala.Predef.String]](nsHeadTail)))(hydra.lib.lists.concat2[scala.Predef.String](Seq("hydra",
       "dsl"))(nsParts))
    hydra.lib.strings.intercalate(".")(hydra.lib.lists.concat2[scala.Predef.String](dslNsParts)(Seq(localPart)))
  })(hydra.lib.lists.uncons[scala.Predef.String](nsParts)))(hydra.lib.lists.maybeInit[scala.Predef.String](parts))
}

def dslDefinitionName(typeName: hydra.core.Name)(localName: scala.Predef.String): hydra.core.Name =
  {
  lazy val parts: Seq[scala.Predef.String] = hydra.lib.strings.splitOn(".")(typeName)
  hydra.lib.maybes.maybe[hydra.core.Name, Seq[scala.Predef.String]](localName)((nsParts: Seq[scala.Predef.String]) =>
    {
    lazy val dslNsParts: Seq[scala.Predef.String] = hydra.lib.maybes.maybe[Seq[scala.Predef.String],
       Tuple2[scala.Predef.String, Seq[scala.Predef.String]]](Seq("hydra", "dsl"))((nsHeadTail: Tuple2[scala.Predef.String,
       Seq[scala.Predef.String]]) =>
      hydra.lib.logic.ifElse[Seq[scala.Predef.String]](hydra.lib.equality.equal[scala.Predef.String](hydra.lib.pairs.first[scala.Predef.String,
         Seq[scala.Predef.String]](nsHeadTail))("hydra"))(hydra.lib.lists.concat2[scala.Predef.String](Seq("hydra",
         "dsl"))(hydra.lib.pairs.second[scala.Predef.String, Seq[scala.Predef.String]](nsHeadTail)))(hydra.lib.lists.concat2[scala.Predef.String](Seq("hydra",
         "dsl"))(nsParts)))(hydra.lib.lists.uncons[scala.Predef.String](nsParts))
    hydra.lib.strings.intercalate(".")(hydra.lib.lists.concat2[scala.Predef.String](dslNsParts)(Seq(localName)))
  })(hydra.lib.lists.maybeInit[scala.Predef.String](parts))
}

def dslModule[T0](cx: T0)(graph: hydra.graph.Graph)(mod: hydra.packaging.Module): Either[hydra.errors.Error,
   Option[hydra.packaging.Module]] =
  hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.core.Binding], Option[hydra.packaging.Module]](hydra.dsls.filterTypeBindings(cx)(graph)(hydra.lib.maybes.cat[hydra.core.Binding](hydra.lib.lists.map[hydra.packaging.Definition,
     Option[hydra.core.Binding]]((d: hydra.packaging.Definition) =>
  d match
  case hydra.packaging.Definition.`type`(v_Definition_type_td) => Some({
    lazy val schemaTerm: hydra.core.Term = hydra.core.Term.variable("hydra.core.Type")
    {
      lazy val dataTerm: hydra.core.Term = hydra.annotations.normalizeTermAnnotations(hydra.core.Term.annotated(hydra.core.AnnotatedTerm(hydra.encode.core.`type`(v_Definition_type_td.`type`.`type`),
         hydra.lib.maps.fromList[hydra.core.Name, hydra.core.Term](Seq(Tuple2(hydra.constants.key_type,
         schemaTerm))))))
      hydra.core.Binding(v_Definition_type_td.name, dataTerm, Some(hydra.core.TypeScheme(Seq(),
         hydra.core.Type.variable("hydra.core.Type"), None)))
    }
  })
  case _ => None)(mod.definitions))))((typeBindings: Seq[hydra.core.Binding]) =>
  hydra.lib.logic.ifElse[Either[hydra.errors.Error, Option[hydra.packaging.Module]]](hydra.lib.lists.`null`[hydra.core.Binding](typeBindings))(Right(None))(hydra.lib.eithers.bind[hydra.errors.Error,
     Seq[Seq[hydra.core.Binding]], Option[hydra.packaging.Module]](hydra.lib.eithers.mapList[hydra.core.Binding,
     Seq[hydra.core.Binding], hydra.errors.Error]((b: hydra.core.Binding) =>
  hydra.lib.eithers.bimap[hydra.errors.DecodingError, Seq[hydra.core.Binding], hydra.errors.Error,
     Seq[hydra.core.Binding]]((_e: hydra.errors.DecodingError) => hydra.errors.Error.decoding(_e))((x: Seq[hydra.core.Binding]) => x)(hydra.dsls.generateBindingsForType(cx)(graph)(b)))(typeBindings))((dslBindings: Seq[Seq[hydra.core.Binding]]) =>
  Right(Some(hydra.packaging.Module(hydra.dsls.dslNamespace(mod.namespace), hydra.lib.lists.map[hydra.core.Binding,
     hydra.packaging.Definition]((b: hydra.core.Binding) =>
  hydra.packaging.Definition.term(hydra.packaging.TermDefinition(b.name, (b.term),
     (b.`type`))))(hydra.dsls.deduplicateBindings(hydra.lib.lists.concat[hydra.core.Binding](dslBindings))),
     hydra.lib.lists.nub[hydra.packaging.Namespace](hydra.lib.lists.map[hydra.packaging.Namespace,
     hydra.packaging.Namespace](hydra.dsls.dslNamespace)(mod.typeDependencies)), hydra.lib.lists.nub[hydra.packaging.Namespace](hydra.lib.lists.concat2[hydra.packaging.Namespace](Seq(mod.namespace,
     "hydra.phantoms"))(mod.typeDependencies)), Some(hydra.lib.strings.cat(Seq("DSL functions for ",
     (mod.namespace))))))))))

def dslNamespace(ns: hydra.packaging.Namespace): hydra.packaging.Namespace =
  {
  lazy val parts: Seq[scala.Predef.String] = hydra.lib.strings.splitOn(".")(ns)
  lazy val prefixFull: hydra.packaging.Namespace = hydra.lib.strings.cat(Seq("hydra.dsl.", ns))
  hydra.lib.maybes.maybe[hydra.packaging.Namespace, Tuple2[scala.Predef.String, Seq[scala.Predef.String]]](prefixFull)((ht: Tuple2[scala.Predef.String,
     Seq[scala.Predef.String]]) =>
    hydra.lib.logic.ifElse[hydra.packaging.Namespace](hydra.lib.equality.equal[scala.Predef.String](hydra.lib.pairs.first[scala.Predef.String,
       Seq[scala.Predef.String]](ht))("hydra"))(hydra.lib.strings.cat(Seq("hydra.dsl.",
       hydra.lib.strings.intercalate(".")(hydra.lib.pairs.second[scala.Predef.String,
       Seq[scala.Predef.String]](ht)))))(prefixFull))(hydra.lib.lists.uncons[scala.Predef.String](parts))
}

def dslTypeScheme(origType: hydra.core.Type)(paramTypes: Seq[hydra.core.Type])(resultType: hydra.core.Type): hydra.core.TypeScheme =
  {
  lazy val typeVars: Seq[hydra.core.Name] = hydra.dsls.collectForallVars(origType)
  lazy val wrappedResult: hydra.core.Type = hydra.core.Type.application(hydra.core.ApplicationType(hydra.core.Type.variable("hydra.phantoms.TTerm"),
     resultType))
  lazy val funType: hydra.core.Type = hydra.lib.lists.foldr[hydra.core.Type, hydra.core.Type]((paramType: hydra.core.Type) =>
    (acc: hydra.core.Type) =>
    hydra.core.Type.function(hydra.core.FunctionType(hydra.core.Type.application(hydra.core.ApplicationType(hydra.core.Type.variable("hydra.phantoms.TTerm"),
       paramType)), acc)))(wrappedResult)(paramTypes)
  hydra.core.TypeScheme(typeVars, funType, None)
}

def filterTypeBindings[T0, T1, T2](cx: T0)(graph: T1)(bindings: Seq[hydra.core.Binding]): Either[T2,
   Seq[hydra.core.Binding]] =
  hydra.lib.eithers.map[Seq[Option[hydra.core.Binding]], Seq[hydra.core.Binding],
     T2](hydra.lib.maybes.cat[hydra.core.Binding])(hydra.lib.eithers.mapList[hydra.core.Binding,
     Option[hydra.core.Binding], T2]((v1: hydra.core.Binding) => hydra.dsls.isDslEligibleBinding(cx)(graph)(v1))(hydra.lib.lists.filter[hydra.core.Binding](hydra.annotations.isNativeType)(bindings)))

def findUniqueName(candidate: scala.Predef.String)(usedNames: Seq[scala.Predef.String]): scala.Predef.String =
  hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.lists.`null`[scala.Predef.String](hydra.lib.lists.filter[scala.Predef.String]((v1) => hydra.lib.equality.equal[scala.Predef.String](candidate)(v1))(usedNames)))(candidate)(hydra.dsls.findUniqueName(hydra.lib.strings.cat(Seq(candidate,
     "_")))(usedNames))

def generateBindingsForType[T0](cx: T0)(graph: hydra.graph.Graph)(b: hydra.core.Binding): Either[hydra.errors.DecodingError,
   Seq[hydra.core.Binding]] =
  {
  lazy val typeName: hydra.core.Name = (b.name)
  hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Type, Seq[hydra.core.Binding]](hydra.decode.core.`type`(graph)(b.term))((rawType: hydra.core.Type) =>
    {
    lazy val typ: hydra.core.Type = hydra.strip.deannotateTypeParameters(hydra.strip.deannotateType(rawType))
    Right(typ match
      case hydra.core.Type.record(v_Type_record_fts) => hydra.lib.lists.concat[hydra.core.Binding](Seq(hydra.dsls.generateRecordConstructor(rawType)(typeName)(v_Type_record_fts),
         hydra.lib.lists.map[hydra.core.FieldType, hydra.core.Binding]((v1: hydra.core.FieldType) => hydra.dsls.generateRecordAccessor(rawType)(typeName)(v1))(v_Type_record_fts),
         hydra.lib.lists.map[hydra.core.FieldType, hydra.core.Binding]((v1: hydra.core.FieldType) =>
        hydra.dsls.generateRecordWithUpdater(rawType)(typeName)(v_Type_record_fts)(v1))(v_Type_record_fts)))
      case hydra.core.Type.union(v_Type_union_fts) => hydra.lib.lists.map[hydra.core.FieldType,
         hydra.core.Binding]((v1: hydra.core.FieldType) => hydra.dsls.generateUnionInjector(rawType)(typeName)(v1))(v_Type_union_fts)
      case hydra.core.Type.wrap(v_Type_wrap_innerType) => hydra.dsls.generateWrappedTypeAccessors(rawType)(typeName)(v_Type_wrap_innerType)
      case _ => Seq())
  })
}

def generateRecordAccessor(origType: hydra.core.Type)(typeName: hydra.core.Name)(ft: hydra.core.FieldType): hydra.core.Binding =
  {
  lazy val fieldName: hydra.core.Name = (ft.name)
  lazy val accessorLocalName: scala.Predef.String = hydra.lib.strings.cat(Seq(hydra.formatting.decapitalize(hydra.names.localNameOf(typeName)),
     hydra.lib.strings.intercalate("")(hydra.lib.lists.map[scala.Predef.String, scala.Predef.String]((s: scala.Predef.String) => hydra.formatting.capitalize(s))(hydra.lib.strings.splitOn(".")(fieldName)))))
  lazy val accessorName: hydra.core.Name = hydra.dsls.dslDefinitionName(typeName)(accessorLocalName)
  lazy val paramDomain: hydra.core.Type = hydra.core.Type.application(hydra.core.ApplicationType(hydra.core.Type.variable("hydra.phantoms.TTerm"),
     hydra.dsls.nominalResultType(typeName)(origType)))
  lazy val body: hydra.core.Term = hydra.core.Term.lambda(hydra.core.Lambda("x", Some(paramDomain),
     hydra.core.Term.wrap(hydra.core.WrappedTerm("hydra.phantoms.TTerm", hydra.core.Term.inject(hydra.core.Injection("hydra.core.Term",
     hydra.core.Field("application", hydra.core.Term.record(hydra.core.Record("hydra.core.Application",
     Seq(hydra.core.Field("function", hydra.core.Term.inject(hydra.core.Injection("hydra.core.Term",
     hydra.core.Field("project", hydra.core.Term.record(hydra.core.Record("hydra.core.Projection",
     Seq(hydra.core.Field("typeName", hydra.core.Term.wrap(hydra.core.WrappedTerm("hydra.core.Name",
     hydra.core.Term.literal(hydra.core.Literal.string(typeName))))), hydra.core.Field("field",
     hydra.core.Term.wrap(hydra.core.WrappedTerm("hydra.core.Name", hydra.core.Term.literal(hydra.core.Literal.string(fieldName)))))))))))),
     hydra.core.Field("argument", hydra.core.Term.application(hydra.core.Application(hydra.core.Term.unwrap("hydra.phantoms.TTerm"),
     hydra.core.Term.variable("x"))))))))))))))
  lazy val ts: hydra.core.TypeScheme = hydra.dsls.dslTypeScheme(origType)(Seq(hydra.dsls.nominalResultType(typeName)(origType)))(ft.`type`)
  hydra.core.Binding(accessorName, body, Some(ts))
}

def generateRecordConstructor(origType: hydra.core.Type)(typeName: hydra.core.Name)(fieldTypes: Seq[hydra.core.FieldType]): Seq[hydra.core.Binding] =
  {
  lazy val dFields: Seq[hydra.core.Term] = hydra.lib.lists.map[hydra.core.FieldType,
     hydra.core.Term]((ft: hydra.core.FieldType) =>
    hydra.core.Term.record(hydra.core.Record("hydra.core.Field", Seq(hydra.core.Field("name",
       hydra.core.Term.wrap(hydra.core.WrappedTerm("hydra.core.Name", hydra.core.Term.literal(hydra.core.Literal.string(ft.name))))),
       hydra.core.Field("term", hydra.core.Term.application(hydra.core.Application(hydra.core.Term.unwrap("hydra.phantoms.TTerm"),
       hydra.core.Term.variable(hydra.formatting.decapitalize(hydra.names.localNameOf(ft.name))))))))))(fieldTypes)
  lazy val recordTerm: hydra.core.Term = hydra.core.Term.wrap(hydra.core.WrappedTerm("hydra.phantoms.TTerm",
     hydra.core.Term.inject(hydra.core.Injection("hydra.core.Term", hydra.core.Field("record",
     hydra.core.Term.record(hydra.core.Record("hydra.core.Record", Seq(hydra.core.Field("typeName",
     hydra.core.Term.wrap(hydra.core.WrappedTerm("hydra.core.Name", hydra.core.Term.literal(hydra.core.Literal.string(typeName))))),
     hydra.core.Field("fields", hydra.core.Term.list(dFields))))))))))
  lazy val paramPairs: Seq[Tuple2[scala.Predef.String, hydra.core.Type]] = hydra.lib.lists.map[hydra.core.FieldType,
     Tuple2[scala.Predef.String, hydra.core.Type]]((ft: hydra.core.FieldType) =>
    Tuple2(hydra.formatting.decapitalize(hydra.names.localNameOf(ft.name)), hydra.core.Type.application(hydra.core.ApplicationType(hydra.core.Type.variable("hydra.phantoms.TTerm"),
       (ft.`type`)))))(fieldTypes)
  lazy val body: hydra.core.Term = hydra.lib.lists.foldl[hydra.core.Term, Tuple2[scala.Predef.String,
     hydra.core.Type]]((acc: hydra.core.Term) =>
    (pp: Tuple2[scala.Predef.String, hydra.core.Type]) =>
    hydra.core.Term.lambda(hydra.core.Lambda(hydra.lib.pairs.first[scala.Predef.String,
       hydra.core.Type](pp), Some(hydra.lib.pairs.second[scala.Predef.String, hydra.core.Type](pp)),
       acc)))(recordTerm)(hydra.lib.lists.reverse[Tuple2[scala.Predef.String, hydra.core.Type]](paramPairs))
  lazy val paramTypes: Seq[hydra.core.Type] = hydra.lib.lists.map[hydra.core.FieldType,
     hydra.core.Type]((ft: hydra.core.FieldType) => (ft.`type`))(fieldTypes)
  lazy val resultType: hydra.core.Type = hydra.dsls.nominalResultType(typeName)(origType)
  lazy val ts: hydra.core.TypeScheme = hydra.dsls.dslTypeScheme(origType)(paramTypes)(resultType)
  Seq(hydra.core.Binding(hydra.dsls.dslBindingName(typeName), body, Some(ts)))
}

def generateRecordWithUpdater(origType: hydra.core.Type)(typeName: hydra.core.Name)(allFields: Seq[hydra.core.FieldType])(targetField: hydra.core.FieldType): hydra.core.Binding =
  {
  lazy val targetFieldName: hydra.core.Name = (targetField.name)
  lazy val updaterLocalName: scala.Predef.String = hydra.lib.strings.cat(Seq(hydra.formatting.decapitalize(hydra.names.localNameOf(typeName)),
     "With", hydra.lib.strings.intercalate("")(hydra.lib.lists.map[scala.Predef.String,
     scala.Predef.String]((s: scala.Predef.String) => hydra.formatting.capitalize(s))(hydra.lib.strings.splitOn(".")(targetFieldName)))))
  lazy val updaterName: hydra.core.Name = hydra.dsls.dslDefinitionName(typeName)(updaterLocalName)
  lazy val dFields: Seq[hydra.core.Term] = hydra.lib.lists.map[hydra.core.FieldType,
     hydra.core.Term]((ft: hydra.core.FieldType) =>
    hydra.core.Term.record(hydra.core.Record("hydra.core.Field", Seq(hydra.core.Field("name",
       hydra.core.Term.wrap(hydra.core.WrappedTerm("hydra.core.Name", hydra.core.Term.literal(hydra.core.Literal.string(ft.name))))),
       hydra.core.Field("term", hydra.lib.logic.ifElse[hydra.core.Term](hydra.lib.equality.equal[scala.Predef.String](ft.name)(targetFieldName))(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.unwrap("hydra.phantoms.TTerm"),
       hydra.core.Term.variable("newVal"))))(hydra.core.Term.inject(hydra.core.Injection("hydra.core.Term",
       hydra.core.Field("application", hydra.core.Term.record(hydra.core.Record("hydra.core.Application",
       Seq(hydra.core.Field("function", hydra.core.Term.inject(hydra.core.Injection("hydra.core.Term",
       hydra.core.Field("project", hydra.core.Term.record(hydra.core.Record("hydra.core.Projection",
       Seq(hydra.core.Field("typeName", hydra.core.Term.wrap(hydra.core.WrappedTerm("hydra.core.Name",
       hydra.core.Term.literal(hydra.core.Literal.string(typeName))))), hydra.core.Field("field",
       hydra.core.Term.wrap(hydra.core.WrappedTerm("hydra.core.Name", hydra.core.Term.literal(hydra.core.Literal.string(ft.name)))))))))))),
       hydra.core.Field("argument", hydra.core.Term.application(hydra.core.Application(hydra.core.Term.unwrap("hydra.phantoms.TTerm"),
       hydra.core.Term.variable("original"))))))))))))))))(allFields)
  lazy val recDomain: hydra.core.Type = hydra.core.Type.application(hydra.core.ApplicationType(hydra.core.Type.variable("hydra.phantoms.TTerm"),
     hydra.dsls.nominalResultType(typeName)(origType)))
  lazy val fieldDomain: hydra.core.Type = hydra.core.Type.application(hydra.core.ApplicationType(hydra.core.Type.variable("hydra.phantoms.TTerm"),
     (targetField.`type`)))
  lazy val body: hydra.core.Term = hydra.core.Term.lambda(hydra.core.Lambda("original",
     Some(recDomain), hydra.core.Term.lambda(hydra.core.Lambda("newVal", Some(fieldDomain),
     hydra.core.Term.wrap(hydra.core.WrappedTerm("hydra.phantoms.TTerm", hydra.core.Term.inject(hydra.core.Injection("hydra.core.Term",
     hydra.core.Field("record", hydra.core.Term.record(hydra.core.Record("hydra.core.Record",
     Seq(hydra.core.Field("typeName", hydra.core.Term.wrap(hydra.core.WrappedTerm("hydra.core.Name",
     hydra.core.Term.literal(hydra.core.Literal.string(typeName))))), hydra.core.Field("fields",
     hydra.core.Term.list(dFields))))))))))))))
  lazy val recType: hydra.core.Type = hydra.dsls.nominalResultType(typeName)(origType)
  lazy val ts: hydra.core.TypeScheme = hydra.dsls.dslTypeScheme(origType)(Seq(recType, (targetField.`type`)))(recType)
  hydra.core.Binding(updaterName, body, Some(ts))
}

def generateUnionInjector(origType: hydra.core.Type)(typeName: hydra.core.Name)(ft: hydra.core.FieldType): hydra.core.Binding =
  {
  lazy val fieldName: hydra.core.Name = (ft.name)
  lazy val fieldType: hydra.core.Type = (ft.`type`)
  lazy val injectorLocalName: scala.Predef.String = hydra.lib.strings.cat(Seq(hydra.formatting.decapitalize(hydra.names.localNameOf(typeName)),
     hydra.lib.strings.intercalate("")(hydra.lib.lists.map[scala.Predef.String, scala.Predef.String]((s: scala.Predef.String) => hydra.formatting.capitalize(s))(hydra.lib.strings.splitOn(".")(fieldName)))))
  lazy val injectorName: hydra.core.Name = hydra.dsls.dslDefinitionName(typeName)(injectorLocalName)
  lazy val isUnit: Boolean = hydra.strip.deannotateType(fieldType) match
    case hydra.core.Type.unit => true
    case _ => false
  lazy val dFieldValue: hydra.core.Term = hydra.lib.logic.ifElse[hydra.core.Term](isUnit)(hydra.core.Term.inject(hydra.core.Injection("hydra.core.Term",
     hydra.core.Field("unit", hydra.core.Term.unit))))(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.unwrap("hydra.phantoms.TTerm"),
     hydra.core.Term.variable("x"))))
  lazy val injectionTerm: hydra.core.Term = hydra.core.Term.wrap(hydra.core.WrappedTerm("hydra.phantoms.TTerm",
     hydra.core.Term.inject(hydra.core.Injection("hydra.core.Term", hydra.core.Field("inject",
     hydra.core.Term.record(hydra.core.Record("hydra.core.Injection", Seq(hydra.core.Field("typeName",
     hydra.core.Term.wrap(hydra.core.WrappedTerm("hydra.core.Name", hydra.core.Term.literal(hydra.core.Literal.string(typeName))))),
     hydra.core.Field("field", hydra.core.Term.record(hydra.core.Record("hydra.core.Field",
     Seq(hydra.core.Field("name", hydra.core.Term.wrap(hydra.core.WrappedTerm("hydra.core.Name",
     hydra.core.Term.literal(hydra.core.Literal.string(fieldName))))), hydra.core.Field("term",
     dFieldValue)))))))))))))
  lazy val variantDomain: hydra.core.Type = hydra.core.Type.application(hydra.core.ApplicationType(hydra.core.Type.variable("hydra.phantoms.TTerm"),
     (ft.`type`)))
  lazy val body: hydra.core.Term = hydra.lib.logic.ifElse[hydra.core.Term](isUnit)(injectionTerm)(hydra.core.Term.lambda(hydra.core.Lambda("x",
     Some(variantDomain), injectionTerm)))
  lazy val unionType: hydra.core.Type = hydra.dsls.nominalResultType(typeName)(origType)
  lazy val ts: hydra.core.TypeScheme = hydra.lib.logic.ifElse[hydra.core.TypeScheme](isUnit)(hydra.dsls.dslTypeScheme(origType)(Seq())(unionType))(hydra.dsls.dslTypeScheme(origType)(Seq(ft.`type`))(unionType))
  hydra.core.Binding(injectorName, body, Some(ts))
}

def generateWrappedTypeAccessors(origType: hydra.core.Type)(typeName: hydra.core.Name)(innerType: hydra.core.Type): Seq[hydra.core.Binding] =
  {
  lazy val localName: scala.Predef.String = hydra.names.localNameOf(typeName)
  lazy val wrapName: hydra.core.Name = hydra.dsls.dslDefinitionName(typeName)(hydra.formatting.decapitalize(localName))
  lazy val unwrapLocalName: scala.Predef.String = hydra.lib.strings.cat(Seq("un", localName))
  lazy val unwrapName: hydra.core.Name = hydra.dsls.dslDefinitionName(typeName)(unwrapLocalName)
  lazy val wrapperType: hydra.core.Type = hydra.dsls.nominalResultType(typeName)(origType)
  lazy val wrapDomain: hydra.core.Type = hydra.core.Type.application(hydra.core.ApplicationType(hydra.core.Type.variable("hydra.phantoms.TTerm"),
     innerType))
  lazy val wrapBody: hydra.core.Term = hydra.core.Term.lambda(hydra.core.Lambda("x",
     Some(wrapDomain), hydra.core.Term.wrap(hydra.core.WrappedTerm("hydra.phantoms.TTerm",
     hydra.core.Term.inject(hydra.core.Injection("hydra.core.Term", hydra.core.Field("wrap",
     hydra.core.Term.record(hydra.core.Record("hydra.core.WrappedTerm", Seq(hydra.core.Field("typeName",
     hydra.core.Term.wrap(hydra.core.WrappedTerm("hydra.core.Name", hydra.core.Term.literal(hydra.core.Literal.string(typeName))))),
     hydra.core.Field("body", hydra.core.Term.application(hydra.core.Application(hydra.core.Term.unwrap("hydra.phantoms.TTerm"),
     hydra.core.Term.variable("x"))))))))))))))
  lazy val unwrapDomain: hydra.core.Type = hydra.core.Type.application(hydra.core.ApplicationType(hydra.core.Type.variable("hydra.phantoms.TTerm"),
     wrapperType))
  lazy val unwrapBody: hydra.core.Term = hydra.core.Term.lambda(hydra.core.Lambda("x",
     Some(unwrapDomain), hydra.core.Term.wrap(hydra.core.WrappedTerm("hydra.phantoms.TTerm",
     hydra.core.Term.inject(hydra.core.Injection("hydra.core.Term", hydra.core.Field("application",
     hydra.core.Term.record(hydra.core.Record("hydra.core.Application", Seq(hydra.core.Field("function",
     hydra.core.Term.inject(hydra.core.Injection("hydra.core.Term", hydra.core.Field("unwrap",
     hydra.core.Term.wrap(hydra.core.WrappedTerm("hydra.core.Name", hydra.core.Term.literal(hydra.core.Literal.string(typeName)))))))),
     hydra.core.Field("argument", hydra.core.Term.application(hydra.core.Application(hydra.core.Term.unwrap("hydra.phantoms.TTerm"),
     hydra.core.Term.variable("x"))))))))))))))
  lazy val wrapTs: hydra.core.TypeScheme = hydra.dsls.dslTypeScheme(origType)(Seq(innerType))(wrapperType)
  lazy val unwrapTs: hydra.core.TypeScheme = hydra.dsls.dslTypeScheme(origType)(Seq(wrapperType))(innerType)
  Seq(hydra.core.Binding(wrapName, wrapBody, Some(wrapTs)), hydra.core.Binding(unwrapName, unwrapBody, Some(unwrapTs)))
}

def isDslEligibleBinding[T0, T1, T2](cx: T0)(graph: T1)(b: hydra.core.Binding): Either[T2, Option[hydra.core.Binding]] =
  {
  lazy val ns: Option[hydra.packaging.Namespace] = hydra.names.namespaceOf(b.name)
  hydra.lib.logic.ifElse[Either[T2, Option[hydra.core.Binding]]](hydra.lib.equality.equal[scala.Predef.String](hydra.lib.maybes.maybe[scala.Predef.String,
     hydra.packaging.Namespace]("")((x) => x)(ns))("hydra.phantoms"))(Right(None))(Right(Some(b)))
}

def nominalResultType(typeName: hydra.core.Name)(origType: hydra.core.Type): hydra.core.Type =
  {
  lazy val vars: Seq[hydra.core.Name] = hydra.dsls.collectForallVars(origType)
  hydra.lib.lists.foldl[hydra.core.Type, hydra.core.Name]((acc: hydra.core.Type) =>
    (v: hydra.core.Name) =>
    hydra.core.Type.application(hydra.core.ApplicationType(acc, hydra.core.Type.variable(v))))(hydra.core.Type.variable(typeName))(vars)
}
