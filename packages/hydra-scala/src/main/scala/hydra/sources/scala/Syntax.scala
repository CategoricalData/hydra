package hydra.sources.scala

import hydra.core.Type
import hydra.overlay.scala.dsl.{Helpers, Types}
import hydra.overlay.scala.dsl.meta.Defs
import hydra.packaging.{Definition, EntityMetadata, Module, ModuleName}

/**
 * Scala syntax — Hydra type definitions for a Scala AST anchored on Scalameta.
 *
 * Departs from Scalameta where Hydra's needs differ: Term is renamed to Data,
 * the FunctionType/FunctionData wrappers are flattened, and arms Hydra never
 * emits (xml literals, quasiquote/macro forms) are omitted.
 */
object Syntax:

  val NS: ModuleName = "hydra.scala.syntax"
  private val CORE_NS: ModuleName = "hydra.core"

  /** Reference a type defined in this module. */
  private def meta(local: String): Type = Helpers.typeref(NS, local)

  /** Build a TypeDefinition in this module. */
  private def define(localName: String, t: Type): Definition =
    Helpers.typeDef(NS, localName, t)

  // ===== Type definitions (alphabetical) =====

  lazy val alternativePatDef: Definition = define("AlternativePat",
    Types.record(
      Types.field("lhs", meta("Pat")),
      Types.field("rhs", meta("Pat"))))

  lazy val andTypeDef: Definition = define("AndType",
    Types.record(
      Types.field("lhs", meta("Type")),
      Types.field("rhs", meta("Type"))))

  lazy val annotModDef: Definition = define("AnnotMod",
    Types.record(
      Types.field("init", meta("Init"))))

  lazy val annotateDataDef: Definition = define("AnnotateData",
    Types.record(
      Types.field("expr", meta("Data")),
      Types.field("annots", Types.list(meta("AnnotMod")))))

  lazy val annotateTypeDef: Definition = define("AnnotateType",
    Types.record(
      Types.field("tpe", meta("Type")),
      Types.field("annots", Types.list(meta("AnnotMod")))))

  lazy val anonymousDataDef: Definition = define("AnonymousData",
    Types.wrap(Types.unit))

  lazy val anonymousNameTypeDef: Definition = define("AnonymousNameType",
    Types.wrap(Types.unit))

  lazy val applyDataDef: Definition = define("ApplyData",
    Types.record(
      Types.field("fun", meta("Data")),
      Types.field("args", Types.list(meta("Data")))))

  lazy val applyInfixDataDef: Definition = define("ApplyInfixData",
    Types.record(
      Types.field("lhs", meta("Data")),
      Types.field("op", meta("NameData")),
      Types.field("targs", Types.list(meta("Type"))),
      Types.field("args", Types.list(meta("Data")))))

  lazy val applyInfixTypeDef: Definition = define("ApplyInfixType",
    Types.record(
      Types.field("lhs", meta("Type")),
      Types.field("op", meta("NameType")),
      Types.field("rhs", meta("Type"))))

  lazy val applyTypeDef: Definition = define("ApplyType",
    Types.record(
      Types.field("tpe", meta("Type")),
      Types.field("args", Types.list(meta("Type")))))

  lazy val applyTypeDataDef: Definition = define("ApplyTypeData",
    Types.record(
      Types.field("lhs", meta("Data")),
      Types.field("op", meta("NameData")),
      Types.field("targs", Types.list(meta("Type"))),
      Types.field("args", Types.list(meta("Data")))))

  lazy val applyUnaryDataDef: Definition = define("ApplyUnaryData",
    Types.record(
      Types.field("op", meta("NameData")),
      Types.field("arg", meta("Data"))))

  lazy val applyUsingDataDef: Definition = define("ApplyUsingData",
    Types.record(
      Types.field("fun", meta("Data")),
      Types.field("targs", Types.list(meta("Data")))))

  lazy val ascribeDataDef: Definition = define("AscribeData",
    Types.record(
      Types.field("expr", meta("Data")),
      Types.field("tpe", meta("Type"))))

  lazy val assignDataDef: Definition = define("AssignData",
    Types.record(
      Types.field("lhs", meta("Data")),
      Types.field("rhs", meta("Data"))))

  lazy val bindPatDef: Definition = define("BindPat",
    Types.record(
      Types.field("lhs", meta("Pat")),
      Types.field("rhs", meta("Pat"))))

  lazy val blockDataDef: Definition = define("BlockData",
    Types.record(
      Types.field("stats", Types.list(meta("Stat")))))

  lazy val byNameTypeDef: Definition = define("ByNameType",
    Types.record(
      Types.field("tpe", meta("Type"))))

  lazy val caseGeneratorEnumeratorDef: Definition = define("CaseGeneratorEnumerator",
    Types.record(
      Types.field("pat", meta("Pat")),
      Types.field("rhs", meta("Data"))))

  lazy val caseTreeDef: Definition = define("CaseTree",
    Types.union(
      Types.field("case", meta("Case")),
      Types.field("typeCase", meta("TypeCase"))))

  lazy val caseDef: Definition = define("Case",
    Types.record(
      Types.field("pat", meta("Pat")),
      Types.field("cond", Types.optional(meta("Data"))),
      Types.field("body", meta("Data"))))

  lazy val classDefnDef: Definition = define("ClassDefn",
    Types.record(
      Types.field("mods", Types.list(meta("Mod"))),
      Types.field("name", meta("NameType")),
      Types.field("tparams", Types.list(meta("ParamType"))),
      Types.field("ctor", meta("PrimaryCtor")),
      Types.field("template", meta("Template"))))

  lazy val contextFunctionDataDef: Definition = define("ContextFunctionData",
    Types.record(
      Types.field("params", Types.list(meta("ParamData"))),
      Types.field("body", meta("Data"))))

  lazy val contextFunctionTypeDef: Definition = define("ContextFunctionType",
    Types.record(
      Types.field("params", Types.list(meta("Type"))),
      Types.field("res", meta("Type"))))

  lazy val ctorDef: Definition = define("Ctor",
    Types.union(
      Types.field("primary", meta("PrimaryCtor")),
      Types.field("secondary", meta("SecondaryCtor"))))

  lazy val dataMemberDef: Definition = define("DataMember",
    Types.union(
      Types.field("pkg", meta("Pkg")),
      Types.field("object", meta("ObjectPkg"))))

  lazy val dataDef: Definition = define("Data",
    Types.union(
      Types.field("lit", meta("Lit")),
      Types.field("ref", meta("RefData")),
      Types.field("interpolate", meta("InterpolateData")),
      Types.field("apply", meta("ApplyData")),
      Types.field("applyUsing", meta("ApplyUsingData")),
      Types.field("applyType", meta("ApplyTypeData")),
      Types.field("assign", meta("AssignData")),
      Types.field("return", meta("ReturnData")),
      Types.field("throw", meta("ThrowData")),
      Types.field("ascribe", meta("AscribeData")),
      Types.field("annotate", meta("AnnotateData")),
      Types.field("tuple", meta("TupleData")),
      Types.field("block", meta("BlockData")),
      Types.field("endMarker", meta("EndMarkerData")),
      Types.field("if", meta("IfData")),
      Types.field("match", meta("MatchData")),
      Types.field("try", meta("TryData")),
      Types.field("tryWithHandler", meta("TryWithHandlerData")),
      Types.field("function", meta("FunctionData")),
      Types.field("contextFunction", meta("ContextFunctionData")),
      Types.field("polyFunction", meta("PolyFunctionData")),
      Types.field("partialFunction", meta("PartialFunctionData")),
      Types.field("while", meta("WhileData")),
      Types.field("do", meta("DoData")),
      Types.field("for", meta("ForData")),
      Types.field("forYield", meta("ForYieldData")),
      Types.field("new", meta("NewData")),
      Types.field("newAnonymous", meta("NewAnonymousData")),
      Types.field("placeholder", Types.unit),
      Types.field("eta", meta("EtaData")),
      Types.field("repeated", meta("RepeatedData")),
      Types.field("param", meta("ParamData"))))

  lazy val declDef: Definition = define("Decl",
    Types.union(
      Types.field("val", meta("ValDecl")),
      Types.field("var", meta("VarDecl")),
      Types.field("def", meta("DefDecl")),
      Types.field("type", meta("TypeDecl")),
      Types.field("given", meta("GivenDecl"))))

  lazy val defDeclDef: Definition = define("DefDecl",
    Types.record(
      Types.field("mods", Types.list(meta("Mod"))),
      Types.field("name", meta("NameData")),
      Types.field("tparams", Types.list(meta("ParamType"))),
      Types.field("paramss", Types.list(Types.list(meta("ParamData")))),
      Types.field("decltpe", meta("Type"))))

  lazy val defDefnDef: Definition = define("DefDefn",
    Types.record(
      Types.field("mods", Types.list(meta("Mod"))),
      Types.field("name", meta("NameData")),
      Types.field("tparams", Types.list(meta("ParamType"))),
      Types.field("paramss", Types.list(Types.list(meta("ParamData")))),
      Types.field("decltpe", Types.optional(meta("Type"))),
      Types.field("body", meta("Data"))))

  lazy val defnDef: Definition = define("Defn",
    Types.union(
      Types.field("val", meta("ValDefn")),
      Types.field("var", meta("VarDefn")),
      Types.field("given", meta("GivenDefn")),
      Types.field("enum", meta("EnumDefn")),
      Types.field("enumCase", meta("EnumCaseDefn")),
      Types.field("repeatedEnumCase", meta("RepeatedEnumCaseDefn")),
      Types.field("givenAlias", meta("GivenAliasDefn")),
      Types.field("extensionGroup", meta("ExtensionGroupDefn")),
      Types.field("def", meta("DefDefn")),
      Types.field("type", meta("TypeDefn")),
      Types.field("class", meta("ClassDefn")),
      Types.field("trait", meta("TraitDefn")),
      Types.field("object", meta("ObjectDefn"))))

  lazy val doDataDef: Definition = define("DoData",
    Types.record(
      Types.field("body", meta("Data")),
      Types.field("expr", meta("Data"))))

  lazy val endMarkerDataDef: Definition = define("EndMarkerData",
    Types.record(
      Types.field("name", meta("NameData"))))

  lazy val enumCaseDefnDef: Definition = define("EnumCaseDefn",
    Types.record(
      Types.field("mods", Types.list(meta("Mod"))),
      Types.field("name", meta("NameData")),
      Types.field("tparams", Types.list(meta("ParamType"))),
      Types.field("ctor", meta("PrimaryCtor")),
      Types.field("inits", Types.list(meta("Init")))))

  lazy val enumDefnDef: Definition = define("EnumDefn",
    Types.record(
      Types.field("mods", Types.list(meta("Mod"))),
      Types.field("name", meta("NameType")),
      Types.field("tparams", Types.list(meta("ParamType"))),
      Types.field("ctor", meta("PrimaryCtor")),
      Types.field("template", meta("Template"))))

  lazy val enumeratorDef: Definition = define("Enumerator",
    Types.union(
      Types.field("generator", meta("GeneratorEnumerator")),
      Types.field("caseGenerator", meta("CaseGeneratorEnumerator")),
      Types.field("val", meta("ValEnumerator")),
      Types.field("guard", meta("GuardEnumerator"))))

  lazy val etaDataDef: Definition = define("EtaData",
    Types.record(
      Types.field("expr", meta("Data"))))

  lazy val existentialTypeDef: Definition = define("ExistentialType",
    Types.record(
      Types.field("tpe", meta("Type")),
      Types.field("stats", Types.list(meta("Stat")))))

  lazy val exportDef: Definition = define("Export",
    Types.record(
      Types.field("importers", Types.list(meta("Importer")))))

  lazy val extensionGroupDefnDef: Definition = define("ExtensionGroupDefn",
    Types.record(
      Types.field("tparams", Types.list(meta("ParamType"))),
      Types.field("parmss", Types.list(Types.list(meta("ParamData")))),
      Types.field("body", meta("Stat"))))

  lazy val extractInfixPatDef: Definition = define("ExtractInfixPat",
    Types.record(
      Types.field("lhs", meta("Pat")),
      Types.field("op", meta("NameData")),
      Types.field("rhs", Types.list(meta("Pat")))))

  lazy val extractPatDef: Definition = define("ExtractPat",
    Types.record(
      Types.field("fun", meta("Data")),
      Types.field("args", Types.list(meta("Pat")))))

  lazy val forDataDef: Definition = define("ForData",
    Types.record(
      Types.field("enums", Types.list(meta("Enumerator")))))

  lazy val forYieldDataDef: Definition = define("ForYieldData",
    Types.record(
      Types.field("enums", Types.list(meta("Enumerator")))))

  lazy val functionDataDef: Definition = define("FunctionData",
    Types.record(
      Types.field("params", Types.list(meta("ParamData"))),
      Types.field("body", meta("Data"))))

  lazy val functionTypeDef: Definition = define("FunctionType",
    Types.record(
      Types.field("params", Types.list(meta("Type"))),
      Types.field("res", meta("Type"))))

  lazy val generatorEnumeratorDef: Definition = define("GeneratorEnumerator",
    Types.record(
      Types.field("pat", meta("Pat")),
      Types.field("rhs", meta("Data"))))

  lazy val givenAliasDefnDef: Definition = define("GivenAliasDefn",
    Types.record(
      Types.field("mods", Types.list(meta("Mod"))),
      Types.field("name", meta("Name")),
      Types.field("tparams", Types.list(Types.list(meta("ParamType")))),
      Types.field("sparams", Types.list(Types.list(meta("ParamData")))),
      Types.field("decltpe", meta("Type")),
      Types.field("body", meta("Data"))))

  lazy val givenDeclDef: Definition = define("GivenDecl",
    Types.record(
      Types.field("mods", Types.list(meta("Mod"))),
      Types.field("name", meta("NameData")),
      Types.field("tparams", Types.list(meta("ParamType"))),
      Types.field("sparams", Types.list(Types.list(meta("ParamData")))),
      Types.field("decltpe", meta("Type"))))

  lazy val givenDefnDef: Definition = define("GivenDefn",
    Types.record(
      Types.field("mods", Types.list(meta("Mod"))),
      Types.field("name", meta("Name")),
      Types.field("tparams", Types.list(Types.list(meta("ParamType")))),
      Types.field("sparams", Types.list(Types.list(meta("ParamData")))),
      Types.field("templ", meta("Template"))))

  lazy val givenImporteeDef: Definition = define("GivenImportee",
    Types.record(
      Types.field("tpe", meta("Type"))))

  lazy val givenPatDef: Definition = define("GivenPat",
    Types.record(
      Types.field("tpe", meta("Type"))))

  lazy val guardEnumeratorDef: Definition = define("GuardEnumerator",
    Types.record(
      Types.field("cond", meta("Data"))))

  lazy val ifDataDef: Definition = define("IfData",
    Types.record(
      Types.field("cond", meta("Data")),
      Types.field("thenp", meta("Data")),
      Types.field("elsep", meta("Data"))))

  lazy val implicitFunctionTypeDef: Definition = define("ImplicitFunctionType",
    Types.record(
      Types.field("params", Types.list(meta("Type"))),
      Types.field("res", meta("Type"))))

  lazy val importExportStatDef: Definition = define("ImportExportStat",
    Types.union(
      Types.field("import", meta("Import")),
      Types.field("export", meta("Export"))))

  lazy val importDef: Definition = define("Import",
    Types.record(
      Types.field("importers", Types.list(meta("Importer")))))

  lazy val importeeDef: Definition = define("Importee",
    Types.union(
      Types.field("wildcard", Types.unit),
      Types.field("given", meta("GivenImportee")),
      Types.field("givenAll", Types.unit),
      Types.field("name", meta("NameImportee")),
      Types.field("rename", meta("RenameImportee")),
      Types.field("unimport", meta("UnimportImportee"))))

  lazy val importerDef: Definition = define("Importer",
    Types.record(
      Types.field("ref", meta("RefData")),
      Types.field("importees", Types.list(meta("Importee")))))

  lazy val initDef: Definition = define("Init",
    Types.record(
      Types.field("tpe", meta("Type")),
      Types.field("name", meta("Name")),
      Types.field("argss", Types.list(Types.list(meta("Data"))))))

  lazy val interpolateDataDef: Definition = define("InterpolateData",
    Types.record(
      Types.field("prefix", meta("NameData")),
      Types.field("parts", Types.list(meta("Lit"))),
      Types.field("args", Types.list(meta("Data")))))

  lazy val interpolatePatDef: Definition = define("InterpolatePat",
    Types.record(
      Types.field("prefix", meta("NameData")),
      Types.field("parts", Types.list(meta("Lit")))))

  lazy val lambdaTypeDef: Definition = define("LambdaType",
    Types.record(
      Types.field("tparams", Types.list(meta("ParamType"))),
      Types.field("tpe", meta("Type"))))

  lazy val litDef: Definition = define("Lit",
    Types.union(
      Types.field("null", Types.unit),
      Types.field("int", Types.int32),
      Types.field("double", Types.float64),
      Types.field("float", Types.float32),
      Types.field("byte", Types.int8),
      Types.field("short", Types.int16),
      Types.field("char", Types.uint16),
      Types.field("long", Types.int64),
      Types.field("boolean", Types.boolean_),
      Types.field("unit", Types.unit),
      Types.field("string", Types.string),
      Types.field("bytes", Types.list(Types.int32)),
      Types.field("symbol", meta("ScalaSymbol"))))

  lazy val matchDataDef: Definition = define("MatchData",
    Types.record(
      Types.field("expr", meta("Data")),
      Types.field("cases", Types.list(meta("Case")))))

  lazy val matchTypeDef: Definition = define("MatchType",
    Types.record(
      Types.field("tpe", meta("Type")),
      Types.field("cases", Types.list(meta("TypeCase")))))

  lazy val memberDef: Definition = define("Member",
    Types.union(
      Types.field("term", meta("DataMember")),
      Types.field("type", meta("TypeMember")),
      Types.field("termParam", meta("ParamData")),
      Types.field("typeParam", meta("ParamType")),
      Types.field("self", meta("Self"))))

  lazy val methodTypeDef: Definition = define("MethodType",
    Types.record(
      Types.field("paramss", Types.list(Types.list(meta("ParamData")))),
      Types.field("tpe", meta("Type"))))

  lazy val modDef: Definition = define("Mod",
    Types.union(
      Types.field("annot", meta("AnnotMod")),
      Types.field("private", meta("PrivateMod")),
      Types.field("protected", meta("ProtectedMod")),
      Types.field("implicit", Types.unit),
      Types.field("final", Types.unit),
      Types.field("sealed", Types.unit),
      Types.field("open", Types.unit),
      Types.field("super", Types.unit),
      Types.field("override", Types.unit),
      Types.field("case", Types.unit),
      Types.field("abstract", Types.unit),
      Types.field("covariant", Types.unit),
      Types.field("contravariant", Types.unit),
      Types.field("lazy", Types.unit),
      Types.field("valParam", Types.unit),
      Types.field("varParam", Types.unit),
      Types.field("infix", Types.unit),
      Types.field("inline", Types.unit),
      Types.field("using", Types.unit),
      Types.field("opaque", Types.unit),
      Types.field("transparent", Types.unit)))

  lazy val nameDef: Definition = define("Name",
    Types.union(
      Types.field("value", Types.string),
      Types.field("anonymous", Types.unit),
      Types.field("indeterminate", meta("PredefString"))))

  lazy val nameDataDef: Definition = define("NameData",
    Types.record(
      Types.field("value", meta("PredefString"))))

  lazy val nameImporteeDef: Definition = define("NameImportee",
    Types.record(
      Types.field("name", meta("Name"))))

  lazy val nameTypeDef: Definition = define("NameType",
    Types.record(
      Types.field("value", Types.string)))

  lazy val newAnonymousDataDef: Definition = define("NewAnonymousData",
    Types.record(
      Types.field("templ", meta("Template"))))

  lazy val newDataDef: Definition = define("NewData",
    Types.record(
      Types.field("init", meta("Init"))))

  lazy val objectDefnDef: Definition = define("ObjectDefn",
    Types.record(
      Types.field("name", meta("NameData"))))

  lazy val objectPkgDef: Definition = define("ObjectPkg",
    Types.record(
      Types.field("mods", Types.list(meta("Mod"))),
      Types.field("name", meta("NameData")),
      Types.field("template", meta("Template"))))

  lazy val orTypeDef: Definition = define("OrType",
    Types.record(
      Types.field("lhs", meta("Type")),
      Types.field("rhs", meta("Type"))))

  lazy val paramDataDef: Definition = define("ParamData",
    Types.record(
      Types.field("mods", Types.list(meta("Mod"))),
      Types.field("name", meta("Name")),
      Types.field("decltpe", Types.optional(meta("Type"))),
      Types.field("default", Types.optional(meta("Data")))))

  lazy val paramTypeDef: Definition = define("ParamType",
    Types.record(
      Types.field("mods", Types.list(meta("Mod"))),
      Types.field("name", meta("Name")),
      Types.field("tparams", Types.list(meta("ParamType"))),
      Types.field("tbounds", Types.list(meta("TypeBounds"))),
      Types.field("vbounds", Types.list(meta("Type"))),
      Types.field("cbounds", Types.list(meta("Type")))))

  lazy val partialFunctionDataDef: Definition = define("PartialFunctionData",
    Types.record(
      Types.field("cases", Types.list(meta("Case")))))

  lazy val patDef: Definition = define("Pat",
    Types.union(
      Types.field("var", meta("VarPat")),
      Types.field("wildcard", Types.unit),
      Types.field("seqWildcard", Types.unit),
      Types.field("bind", meta("BindPat")),
      Types.field("alternative", meta("AlternativePat")),
      Types.field("tuple", meta("TuplePat")),
      Types.field("repeated", meta("RepeatedPat")),
      Types.field("extract", meta("ExtractPat")),
      Types.field("extractInfix", meta("ExtractInfixPat")),
      Types.field("interpolate", meta("InterpolatePat")),
      Types.field("typed", meta("TypedPat")),
      Types.field("given", meta("GivenPat"))))

  lazy val pkgDef: Definition = define("Pkg",
    Types.record(
      Types.field("name", meta("NameData")),
      Types.field("ref", meta("RefData")),
      Types.field("stats", Types.list(meta("Stat")))))

  lazy val placeholderTypeDef: Definition = define("PlaceholderType",
    Types.record(
      Types.field("bounds", meta("TypeBounds"))))

  lazy val polyFunctionDataDef: Definition = define("PolyFunctionData",
    Types.record(
      Types.field("tparams", Types.list(meta("ParamType"))),
      Types.field("body", meta("Data"))))

  lazy val polyFunctionTypeDef: Definition = define("PolyFunctionType",
    Types.record(
      Types.field("tparams", Types.list(meta("ParamType"))),
      Types.field("tpe", meta("Type"))))

  lazy val predefStringDef: Definition = define("PredefString",
    Helpers.doc("A wrapper for strings used in scala.Predef contexts.",
      Types.wrap(Types.string)))

  lazy val primaryCtorDef: Definition = define("PrimaryCtor",
    Types.record(
      Types.field("mods", Types.list(meta("Mod"))),
      Types.field("name", meta("Name")),
      Types.field("paramss", Types.list(Types.list(meta("ParamData"))))))

  lazy val privateModDef: Definition = define("PrivateMod",
    Types.record(
      Types.field("within", meta("Ref"))))

  lazy val projectTypeDef: Definition = define("ProjectType",
    Types.record(
      Types.field("qual", meta("Type")),
      Types.field("name", meta("NameType"))))

  lazy val protectedModDef: Definition = define("ProtectedMod",
    Types.record(
      Types.field("within", meta("Ref"))))

  lazy val refDef: Definition = define("Ref",
    Types.union(
      Types.field("name", meta("Name")),
      Types.field("init", meta("Init"))))

  lazy val refDataDef: Definition = define("RefData",
    Types.union(
      Types.field("this", meta("ThisData")),
      Types.field("super", meta("SuperData")),
      Types.field("name", meta("NameData")),
      Types.field("anonymous", meta("AnonymousData")),
      Types.field("select", meta("SelectData")),
      Types.field("applyUnary", meta("ApplyUnaryData"))))

  lazy val refTypeDef: Definition = define("RefType",
    Types.union(
      Types.field("name", meta("NameType")),
      Types.field("select", meta("SelectType")),
      Types.field("project", meta("ProjectType")),
      Types.field("singleton", meta("SingletonType"))))

  lazy val refineTypeDef: Definition = define("RefineType",
    Types.record(
      Types.field("tpe", Types.optional(meta("Type"))),
      Types.field("stats", Types.list(meta("Stat")))))

  lazy val renameImporteeDef: Definition = define("RenameImportee",
    Types.record(
      Types.field("name", meta("Name")),
      Types.field("rename", meta("Name"))))

  lazy val repeatedDataDef: Definition = define("RepeatedData",
    Types.record(
      Types.field("expr", meta("Data"))))

  lazy val repeatedEnumCaseDefnDef: Definition = define("RepeatedEnumCaseDefn",
    Types.record(
      Types.field("mods", Types.list(meta("Mod"))),
      Types.field("cases", Types.list(meta("NameData")))))

  lazy val repeatedPatDef: Definition = define("RepeatedPat",
    Types.record(
      Types.field("name", meta("NameData"))))

  lazy val repeatedTypeDef: Definition = define("RepeatedType",
    Types.record(
      Types.field("tpe", meta("Type"))))

  lazy val returnDataDef: Definition = define("ReturnData",
    Types.record(
      Types.field("expr", meta("Data"))))

  lazy val scalaSymbolDef: Definition = define("ScalaSymbol",
    Helpers.doc("A Scala 2 symbol literal (corresponds to scala.Symbol).",
      Types.record(
        Types.field("name", Types.string))))

  lazy val secondaryCtorDef: Definition = define("SecondaryCtor",
    Types.record(
      Types.field("mods", Types.list(meta("Mod"))),
      Types.field("name", meta("Name")),
      Types.field("paramss", Types.list(Types.list(meta("ParamData")))),
      Types.field("init", meta("Init")),
      Types.field("stats", Types.list(meta("Stat")))))

  lazy val selectDataDef: Definition = define("SelectData",
    Types.record(
      Types.field("qual", meta("Data")),
      Types.field("name", meta("NameData"))))

  lazy val selectTypeDef: Definition = define("SelectType",
    Types.record(
      Types.field("qual", meta("RefData")),
      Types.field("name", meta("NameType"))))

  lazy val selfDef: Definition = define("Self",
    Types.wrap(Types.unit))

  lazy val singletonTypeDef: Definition = define("SingletonType",
    Types.record(
      Types.field("ref", meta("RefData"))))

  lazy val sourceDef: Definition = define("Source",
    Types.record(
      Types.field("stats", Types.list(meta("Stat")))))

  lazy val statDef: Definition = define("Stat",
    Types.union(
      Types.field("term", meta("Data")),
      Types.field("decl", meta("Decl")),
      Types.field("defn", meta("Defn")),
      Types.field("importExport", meta("ImportExportStat"))))

  lazy val superDataDef: Definition = define("SuperData",
    Types.record(
      Types.field("thisp", meta("Name")),
      Types.field("superp", meta("Name"))))

  lazy val templateDef: Definition = define("Template",
    Types.record(
      Types.field("early", Types.list(meta("Stat"))),
      Types.field("inits", Types.list(meta("Init"))),
      Types.field("self", meta("Self")),
      Types.field("stats", Types.list(meta("Stat")))))

  lazy val thisDataDef: Definition = define("ThisData",
    Types.wrap(Types.unit))

  lazy val throwDataDef: Definition = define("ThrowData",
    Types.record(
      Types.field("expr", meta("Data"))))

  lazy val traitDefnDef: Definition = define("TraitDefn",
    Types.record(
      Types.field("mods", Types.list(meta("Mod"))),
      Types.field("name", meta("NameType")),
      Types.field("tparams", Types.list(meta("ParamType"))),
      Types.field("ctor", meta("PrimaryCtor")),
      Types.field("template", meta("Template"))))

  lazy val treeDef: Definition = define("Tree",
    Helpers.doc("The root of the Scalameta tree hierarchy. Each arm names a major AST category.",
      Types.union(
        Types.field("ref", meta("Ref")),
        Types.field("stat", meta("Stat")),
        Types.field("type", meta("Type")),
        Types.field("bounds", meta("TypeBounds")),
        Types.field("pat", meta("Pat")),
        Types.field("member", meta("Member")),
        Types.field("ctor", meta("Ctor")),
        Types.field("template", meta("Template")),
        Types.field("mod", meta("Mod")),
        Types.field("enumerator", meta("Enumerator")),
        Types.field("importer", meta("Importer")),
        Types.field("importee", meta("Importee")),
        Types.field("caseTree", meta("CaseTree")),
        Types.field("source", meta("Source")))))

  lazy val tryDataDef: Definition = define("TryData",
    Types.record(
      Types.field("expr", meta("Data")),
      Types.field("catchp", Types.list(meta("Case"))),
      Types.field("finallyp", Types.optional(meta("Data")))))

  lazy val tryWithHandlerDataDef: Definition = define("TryWithHandlerData",
    Types.record(
      Types.field("expr", meta("Data")),
      Types.field("catchp", meta("Data")),
      Types.field("finallyp", Types.optional(meta("Data")))))

  lazy val tupleDataDef: Definition = define("TupleData",
    Types.record(
      Types.field("args", Types.list(meta("Data")))))

  lazy val tuplePatDef: Definition = define("TuplePat",
    Types.record(
      Types.field("args", Types.list(meta("Pat")))))

  lazy val tupleTypeDef: Definition = define("TupleType",
    Types.record(
      Types.field("args", Types.list(meta("Type")))))

  lazy val typeBoundsDef: Definition = define("TypeBounds",
    Types.record(
      Types.field("lo", Types.optional(meta("Type"))),
      Types.field("hi", Types.optional(meta("Type")))))

  lazy val typeCaseDef: Definition = define("TypeCase",
    Types.record(
      Types.field("pat", meta("Type")),
      Types.field("body", meta("Type"))))

  lazy val typeDeclDef: Definition = define("TypeDecl",
    Types.record(
      Types.field("mods", Types.list(meta("Mod"))),
      Types.field("name", meta("NameType")),
      Types.field("tparams", Types.list(meta("ParamType"))),
      Types.field("bounds", meta("TypeBounds"))))

  lazy val typeDefnDef: Definition = define("TypeDefn",
    Types.record(
      Types.field("mods", Types.list(meta("Mod"))),
      Types.field("name", meta("NameType")),
      Types.field("tparams", Types.list(meta("ParamType"))),
      Types.field("body", meta("Type"))))

  lazy val typeMemberDef: Definition = define("TypeMember",
    Types.record(
      Types.field("name", meta("NameType"))))

  lazy val typeDef: Definition = define("Type",
    Types.union(
      Types.field("ref", meta("RefType")),
      Types.field("anonymousName", meta("AnonymousNameType")),
      Types.field("apply", meta("ApplyType")),
      Types.field("applyInfix", meta("ApplyInfixType")),
      Types.field("function", meta("FunctionType")),
      Types.field("contextFunction", meta("ContextFunctionType")),
      Types.field("polyFunction", meta("PolyFunctionType")),
      Types.field("implicitFunction", meta("ImplicitFunctionType")),
      Types.field("tuple", meta("TupleType")),
      Types.field("with", meta("WithType")),
      Types.field("and", meta("AndType")),
      Types.field("or", meta("OrType")),
      Types.field("refine", meta("RefineType")),
      Types.field("existential", meta("ExistentialType")),
      Types.field("annotate", meta("AnnotateType")),
      Types.field("lambda", meta("LambdaType")),
      Types.field("method", meta("MethodType")),
      Types.field("placeholder", meta("PlaceholderType")),
      Types.field("byName", meta("ByNameType")),
      Types.field("repeated", meta("RepeatedType")),
      Types.field("var", meta("VarType")),
      Types.field("typedParam", meta("TypedParamType")),
      Types.field("match", meta("MatchType"))))

  lazy val typedParamTypeDef: Definition = define("TypedParamType",
    Types.record(
      Types.field("name", meta("Name")),
      Types.field("typ", meta("Type"))))

  lazy val typedPatDef: Definition = define("TypedPat",
    Types.record(
      Types.field("lhs", meta("Pat")),
      Types.field("rhs", meta("Type"))))

  lazy val unimportImporteeDef: Definition = define("UnimportImportee",
    Types.record(
      Types.field("name", meta("Name"))))

  lazy val valDeclDef: Definition = define("ValDecl",
    Types.record(
      Types.field("mods", Types.list(meta("Mod"))),
      Types.field("pats", Types.list(meta("Pat"))),
      Types.field("decltpe", meta("Type"))))

  lazy val valDefnDef: Definition = define("ValDefn",
    Types.record(
      Types.field("mods", Types.list(meta("Mod"))),
      Types.field("pats", Types.list(meta("Pat"))),
      Types.field("decltpe", Types.optional(meta("Type"))),
      Types.field("rhs", meta("Data"))))

  lazy val valEnumeratorDef: Definition = define("ValEnumerator",
    Types.record(
      Types.field("pat", meta("Pat")),
      Types.field("rhs", meta("Data"))))

  lazy val varDeclDef: Definition = define("VarDecl",
    Types.record(
      Types.field("mods", Types.list(meta("Mod"))),
      Types.field("pats", Types.list(meta("Pat"))),
      Types.field("decltpe", meta("Type"))))

  lazy val varDefnDef: Definition = define("VarDefn",
    Types.record(
      Types.field("mods", Types.list(meta("Mod"))),
      Types.field("pats", Types.list(meta("Pat"))),
      Types.field("decltpe", meta("Type")),
      Types.field("rhs", Types.optional(meta("Data")))))

  lazy val varPatDef: Definition = define("VarPat",
    Types.record(
      Types.field("name", meta("NameData"))))

  lazy val varTypeDef: Definition = define("VarType",
    Types.record(
      Types.field("name", meta("NameType"))))

  lazy val whileDataDef: Definition = define("WhileData",
    Types.record(
      Types.field("expr", meta("Data")),
      Types.field("body", meta("Data"))))

  lazy val withTypeDef: Definition = define("WithType",
    Types.record(
      Types.field("lhs", meta("Type")),
      Types.field("rhs", meta("Type"))))

  // ===== Module assembly =====
  // Order MUST match the order in the Haskell Syntax.hs `definitions` list
  // so the generated JSON has the same definitions array shape.

  val DEFINITIONS: Seq[Definition] = Seq(
    predefStringDef,
    scalaSymbolDef,
    treeDef,
    refDef,
    statDef,
    nameDef,
    litDef,
    dataDef,
    refDataDef,
    thisDataDef,
    superDataDef,
    nameDataDef,
    anonymousDataDef,
    selectDataDef,
    interpolateDataDef,
    applyDataDef,
    applyUsingDataDef,
    applyTypeDataDef,
    applyInfixDataDef,
    applyUnaryDataDef,
    assignDataDef,
    returnDataDef,
    throwDataDef,
    ascribeDataDef,
    annotateDataDef,
    tupleDataDef,
    blockDataDef,
    endMarkerDataDef,
    ifDataDef,
    matchDataDef,
    tryDataDef,
    tryWithHandlerDataDef,
    contextFunctionDataDef,
    functionDataDef,
    polyFunctionDataDef,
    partialFunctionDataDef,
    whileDataDef,
    doDataDef,
    forDataDef,
    forYieldDataDef,
    newDataDef,
    newAnonymousDataDef,
    etaDataDef,
    repeatedDataDef,
    paramDataDef,
    typeDef,
    refTypeDef,
    nameTypeDef,
    anonymousNameTypeDef,
    selectTypeDef,
    projectTypeDef,
    singletonTypeDef,
    applyTypeDef,
    applyInfixTypeDef,
    functionTypeDef,
    polyFunctionTypeDef,
    contextFunctionTypeDef,
    implicitFunctionTypeDef,
    tupleTypeDef,
    withTypeDef,
    andTypeDef,
    orTypeDef,
    refineTypeDef,
    existentialTypeDef,
    annotateTypeDef,
    lambdaTypeDef,
    methodTypeDef,
    placeholderTypeDef,
    typeBoundsDef,
    byNameTypeDef,
    repeatedTypeDef,
    varTypeDef,
    typedParamTypeDef,
    paramTypeDef,
    matchTypeDef,
    patDef,
    varPatDef,
    bindPatDef,
    alternativePatDef,
    tuplePatDef,
    repeatedPatDef,
    extractPatDef,
    extractInfixPatDef,
    interpolatePatDef,
    typedPatDef,
    givenPatDef,
    memberDef,
    dataMemberDef,
    typeMemberDef,
    declDef,
    valDeclDef,
    varDeclDef,
    defDeclDef,
    typeDeclDef,
    givenDeclDef,
    defnDef,
    valDefnDef,
    varDefnDef,
    givenDefnDef,
    enumDefnDef,
    enumCaseDefnDef,
    repeatedEnumCaseDefnDef,
    givenAliasDefnDef,
    extensionGroupDefnDef,
    defDefnDef,
    typeDefnDef,
    classDefnDef,
    traitDefnDef,
    objectDefnDef,
    pkgDef,
    objectPkgDef,
    ctorDef,
    primaryCtorDef,
    secondaryCtorDef,
    initDef,
    selfDef,
    templateDef,
    modDef,
    annotModDef,
    privateModDef,
    protectedModDef,
    enumeratorDef,
    generatorEnumeratorDef,
    caseGeneratorEnumeratorDef,
    valEnumeratorDef,
    guardEnumeratorDef,
    importExportStatDef,
    importDef,
    exportDef,
    importerDef,
    importeeDef,
    givenImporteeDef,
    nameImporteeDef,
    renameImporteeDef,
    unimportImporteeDef,
    caseTreeDef,
    caseDef,
    typeCaseDef,
    sourceDef)

  val module_ : Module = Module(
    name = NS,
    metadata = Some(EntityMetadata(
      description = Some(
        "A Scala syntax model for Hydra, anchored on Scalameta (https://scalameta.org). " +
        "Departs from Scalameta where Hydra's needs differ: Term is renamed to Data, the " +
        "FunctionType/FunctionData wrappers are flattened, and arms Hydra never emits " +
        "(xml literals, quasiquote/macro forms) are omitted."),
      comments = Seq.empty,
      seeAlso = Seq.empty,
      lifecycle = None)),
    dependencies = Helpers.unqualifiedDeps(CORE_NS),
    definitions = DEFINITIONS)

  Defs.checkComplete(this, DEFINITIONS)

end Syntax
