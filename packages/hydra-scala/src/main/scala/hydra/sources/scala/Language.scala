package hydra.sources.scala

import hydra.overlay.scala.dsl.{Helpers, Phantoms}
import hydra.overlay.scala.dsl.meta.Defs
import hydra.packaging.{Definition, EntityMetadata, Module, ModuleName}
import hydra.typed.TypedTerm

import hydra.dsl.{coders => Coders, file => FileDsl, lib => Lib, util => UtilDsl, variants => Variants}
import hydra.dsl.lib.{lists => Lists, sets => Sets}
import hydra.dsl as KernelDsl
import hydra.core as KernelCore

/**
 * Language constraints and reserved words for Scala.
 */
object Language:

  val NS: ModuleName = "hydra.scala.language"

  /** Dependencies list — `Lexical.ns ++ KernelTypes.kernelTypesModuleNames` in the Haskell source. */
  private val KERNEL_DEPS: Seq[ModuleName] = Seq("hydra.lexical") ++ Helpers.kernelTypesModuleNames

  // ===== scalaLanguage =====

  lazy val scalaLanguageDef: Definition =
    val body =
      Phantoms.let(Seq(
        Phantoms.field("literalVariants", Sets.fromList(Phantoms.list(
          Variants.literalVariantBoolean,
          Variants.literalVariantDecimal,
          Variants.literalVariantFloat,
          Variants.literalVariantInteger,
          Variants.literalVariantString))),
        Phantoms.field("floatTypes", Sets.fromList(Phantoms.list(
          KernelDsl.core.floatTypeFloat32,
          KernelDsl.core.floatTypeFloat64))),
        Phantoms.field("integerTypes", Sets.fromList(Phantoms.list(
          KernelDsl.core.integerTypeBigint,
          KernelDsl.core.integerTypeInt8,
          KernelDsl.core.integerTypeInt16,
          KernelDsl.core.integerTypeInt32,
          KernelDsl.core.integerTypeInt64,
          KernelDsl.core.integerTypeUint8,
          KernelDsl.core.integerTypeUint16,
          KernelDsl.core.integerTypeUint32,
          KernelDsl.core.integerTypeUint64))),
        Phantoms.field("termVariants", Sets.fromList(Phantoms.list(
          Variants.termVariantApplication,
          Variants.termVariantEither,
          Variants.termVariantCases,
          Variants.termVariantLambda,
          Variants.termVariantProject,
          Variants.termVariantUnwrap,
          Variants.termVariantTypeApplication,
          Variants.termVariantTypeLambda,
          Variants.termVariantLet,
          Variants.termVariantList,
          Variants.termVariantLiteral,
          Variants.termVariantMap,
          Variants.termVariantOptional,
          Variants.termVariantPair,
          Variants.termVariantRecord,
          Variants.termVariantSet,
          Variants.termVariantInject,
          Variants.termVariantUnit,
          Variants.termVariantVariable,
          Variants.termVariantWrap))),
        Phantoms.field("typeVariants", Sets.fromList(Phantoms.list(
          Variants.typeVariantAnnotated,
          Variants.typeVariantApplication,
          Variants.typeVariantEither,
          Variants.typeVariantEffect,
          Variants.typeVariantFunction,
          Variants.typeVariantList,
          Variants.typeVariantLiteral,
          Variants.typeVariantMap,
          Variants.typeVariantOptional,
          Variants.typeVariantPair,
          Variants.typeVariantRecord,
          Variants.typeVariantSet,
          Variants.typeVariantUnion,
          Variants.typeVariantUnit,
          Variants.typeVariantForall,
          Variants.typeVariantVariable,
          Variants.typeVariantVoid,
          Variants.typeVariantWrap))),
        Phantoms.field("typePredicate", Phantoms.constant(Phantoms.bool(true)))),
        Coders.language(
          Coders.languageName2(Phantoms.string("hydra.scala")))(
          Coders.languageConstraints2(
            Phantoms.`var`("literalVariants"))(
            Phantoms.`var`("floatTypes"))(
            Phantoms.`var`("integerTypes"))(
            Phantoms.`var`("termVariants"))(
            Phantoms.`var`("typeVariants"))(
            Phantoms.`var`("typePredicate")))(
          Sets.fromList(Phantoms.list(
            Coders.languageFeatureNestedCaseStatements,
            Coders.languageFeatureNestedPolymorphicLetBindings)))(
          Coders.caseConventions(
            UtilDsl.caseConventionUpperSnake)(UtilDsl.caseConventionCamel)(UtilDsl.caseConventionPascal)(
            UtilDsl.caseConventionCamel)(UtilDsl.caseConventionCamel)(UtilDsl.caseConventionCamel)(
            UtilDsl.caseConventionCamel)(UtilDsl.caseConventionCamel)(UtilDsl.caseConventionPascal)(
            UtilDsl.caseConventionPascal))(
          FileDsl.fileExtension(Phantoms.string("scala"))))
    Phantoms.define(NS, "scalaLanguage").doc("Language constraints for Scala").to(body)

  // ===== scalaReservedWords =====

  lazy val scalaReservedWordsDef: Definition =
    val keywords = Seq(
      "abstract", "case", "catch", "class", "def", "do", "else", "end", "enum", "export",
      "extends", "false", "final", "finally", "for", "forSome", "given", "if", "implicit",
      "import", "lazy", "macro", "match", "new", "null", "object", "override", "package",
      "private", "protected", "return", "sealed", "super", "then", "this", "throw", "trait",
      "true", "try", "type", "val", "var", "while", "with", "yield")
    val classNames = Seq(
      "Any", "AnyVal", "App", "Array", "Boolean", "Byte", "Char", "Console", "DelayedInit",
      "Double", "DummyExplicit", "Dynamic", "Enumeration", "Equals", "Float", "Function",
      "Int", "Long", "MatchError", "None", "Nothing", "Null", "Option", "PartialFunction",
      "Predef", "Product", "Proxy", "SerialVersionUID", "Short", "Singleton", "Some",
      "Specializable", "StringContext", "Symbol", "Unit", "ValueOf")
    val body = Phantoms.let(Seq(
        Phantoms.field("keywords",
          Phantoms.doc("Scala keywords", Phantoms.listSeq(keywords.map(Phantoms.string)))),
        Phantoms.field("classNames",
          Phantoms.doc("Classes in the Scala Standard Library 2.13.8",
            Phantoms.listSeq(classNames.map(Phantoms.string))))),
      Sets.fromList(Lists.concat(Phantoms.list(
        Phantoms.`var`("keywords"),
        Phantoms.`var`("classNames")))))
    Phantoms.define(NS, "scalaReservedWords").doc("A set of reserved words in Scala").to(body)

  // ===== Module assembly =====

  val DEFINITIONS: Seq[Definition] = Seq(
    scalaLanguageDef,
    scalaReservedWordsDef)

  val module_ : Module = Module(
    name = NS,
    metadata = Some(EntityMetadata(
      description = Some("Language constraints and reserved words for Scala"),
      comments = Seq.empty,
      seeAlso = Seq.empty,
      lifecycle = None)),
    dependencies = KERNEL_DEPS.map(Helpers.unqualifiedDep),
    definitions = DEFINITIONS)

  Defs.checkComplete(this, DEFINITIONS)

end Language
