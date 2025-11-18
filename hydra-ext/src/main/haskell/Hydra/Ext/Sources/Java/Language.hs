{-# LANGUAGE OverloadedStrings #-}

module Hydra.Ext.Sources.Java.Language where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Accessors     as Accessors
import qualified Hydra.Dsl.Annotations   as Annotations
import qualified Hydra.Dsl.Ast           as Ast
import qualified Hydra.Dsl.Bootstrap     as Bootstrap
import qualified Hydra.Dsl.Coders        as Coders
import qualified Hydra.Dsl.Compute       as Compute
import qualified Hydra.Dsl.Core          as Core
import qualified Hydra.Dsl.Grammar       as Grammar
import qualified Hydra.Dsl.Grammars      as Grammars
import qualified Hydra.Dsl.Graph         as Graph
import qualified Hydra.Dsl.Json          as Json
import qualified Hydra.Dsl.Lib.Chars     as Chars
import qualified Hydra.Dsl.Lib.Eithers   as Eithers
import qualified Hydra.Dsl.Lib.Equality  as Equality
import qualified Hydra.Dsl.Lib.Flows     as Flows
import qualified Hydra.Dsl.Lib.Lists     as Lists
import qualified Hydra.Dsl.Lib.Literals  as Literals
import qualified Hydra.Dsl.Lib.Logic     as Logic
import qualified Hydra.Dsl.Lib.Maps      as Maps
import qualified Hydra.Dsl.Lib.Math      as Math
import qualified Hydra.Dsl.Lib.Maybes    as Maybes
import qualified Hydra.Dsl.Lib.Pairs     as Pairs
import qualified Hydra.Dsl.Lib.Sets      as Sets
import           Hydra.Dsl.Lib.Strings   as Strings
import qualified Hydra.Dsl.Literals      as Literals
import qualified Hydra.Dsl.LiteralTypes  as LiteralTypes
import qualified Hydra.Dsl.Meta.Base     as MetaBase
import qualified Hydra.Dsl.Meta.Terms    as MetaTerms
import qualified Hydra.Dsl.Meta.Types    as MetaTypes
import qualified Hydra.Dsl.Module        as Module
import           Hydra.Dsl.Meta.Phantoms as Phantoms
import qualified Hydra.Dsl.Prims         as Prims
import qualified Hydra.Dsl.Tabular       as Tabular
import qualified Hydra.Dsl.Testing       as Testing
import qualified Hydra.Dsl.Terms         as Terms
import qualified Hydra.Dsl.Testing       as Testing
import qualified Hydra.Dsl.Tests         as Tests
import qualified Hydra.Dsl.Topology      as Topology
import qualified Hydra.Dsl.Types         as Types
import qualified Hydra.Dsl.Typing        as Typing
import qualified Hydra.Dsl.Util          as Util
import qualified Hydra.Dsl.Variants      as Variants
import qualified Hydra.Sources.Kernel.Terms.All             as KernelTerms
import qualified Hydra.Sources.Kernel.Types.All             as KernelTypes
import qualified Hydra.Sources.Kernel.Terms.Adapt.Literals  as AdaptLiterals
import qualified Hydra.Sources.Kernel.Terms.Adapt.Modules   as AdaptModules
import qualified Hydra.Sources.Kernel.Terms.Adapt.Simple    as AdaptSimple
import qualified Hydra.Sources.Kernel.Terms.Adapt.Terms     as AdaptTerms
import qualified Hydra.Sources.Kernel.Terms.Adapt.Utils     as AdaptUtils
import qualified Hydra.Sources.Kernel.Terms.Annotations     as Annotations
import qualified Hydra.Sources.Kernel.Terms.Arity           as Arity
import qualified Hydra.Sources.Kernel.Terms.Checking        as Checking
import qualified Hydra.Sources.Kernel.Terms.Constants       as Constants
import qualified Hydra.Sources.Kernel.Terms.Decode.Core     as DecodeCore
import qualified Hydra.Sources.Kernel.Terms.Encode.Core     as EncodeCore
import qualified Hydra.Sources.Kernel.Terms.Extract.Core    as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Extract.Util    as ExtractUtil
import qualified Hydra.Sources.Kernel.Terms.Formatting      as Formatting
import qualified Hydra.Sources.Kernel.Terms.Grammars        as Grammars
import qualified Hydra.Sources.Kernel.Terms.Inference       as Inference
import qualified Hydra.Sources.Kernel.Terms.Languages       as Languages
import qualified Hydra.Sources.Kernel.Terms.Lexical         as Lexical
import qualified Hydra.Sources.Kernel.Terms.Literals        as Literals
import qualified Hydra.Sources.Kernel.Terms.Monads          as Monads
import qualified Hydra.Sources.Kernel.Terms.Names           as Names
import qualified Hydra.Sources.Kernel.Terms.Reduction       as Reduction
import qualified Hydra.Sources.Kernel.Terms.Reflect         as Reflect
import qualified Hydra.Sources.Kernel.Terms.Rewriting       as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Schemas         as Schemas
import qualified Hydra.Sources.Kernel.Terms.Serialization   as Serialization
import qualified Hydra.Sources.Kernel.Terms.Show.Accessors  as ShowAccessors
import qualified Hydra.Sources.Kernel.Terms.Show.Core       as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Show.Graph      as ShowGraph
import qualified Hydra.Sources.Kernel.Terms.Show.Meta       as ShowMeta
import qualified Hydra.Sources.Kernel.Terms.Show.Typing     as ShowTyping
import qualified Hydra.Sources.Kernel.Terms.Sorting         as Sorting
import qualified Hydra.Sources.Kernel.Terms.Substitution    as Substitution
import qualified Hydra.Sources.Kernel.Terms.Tarjan          as Tarjan
import qualified Hydra.Sources.Kernel.Terms.Templates       as Templates
import qualified Hydra.Sources.Kernel.Terms.Unification     as Unification
import           Prelude hiding ((++))
import qualified Data.Int                                   as I
import qualified Data.List                                  as L
import qualified Data.Map                                   as M
import qualified Data.Set                                   as S
import qualified Data.Maybe                                 as Y


javaLanguageDefinition :: String -> TTerm a -> TBinding a
javaLanguageDefinition = definitionInModule javaLanguageModule

javaLanguageModule :: Module
javaLanguageModule = Module (Namespace "hydra.ext.java.language")
  [el javaMaxTupleLengthDef, el javaLanguageDef, el reservedWordsDef]
  [Lexical.module_]
  KernelTypes.kernelTypesModules $
  Just "Language constraints and reserved words for Java"

javaMaxTupleLengthDef :: TBinding Int
javaMaxTupleLengthDef = javaLanguageDefinition "javaMaxTupleLength" $
  doc ("The maximum supported length of a tuple in Hydra-Java. "
    <> "Note: if this constant is changed, also change Tuples.java correspondingly") $
  int32 9

javaLanguageDef :: TBinding Language
javaLanguageDef = javaLanguageDefinition "javaLanguage" $
  doc "Language constraints for Java" $ lets [
  "eliminationVariants">: Sets.fromList $ list [
    Variants.eliminationVariantProduct,
    Variants.eliminationVariantRecord,
    Variants.eliminationVariantUnion,
    Variants.eliminationVariantWrap],
  "literalVariants">: Sets.fromList $ list [
    Variants.literalVariantBoolean, -- boolean
    Variants.literalVariantFloat, -- (see float types)
    Variants.literalVariantInteger, -- (see integer types)
    Variants.literalVariantString], -- string
  "floatTypes">: Sets.fromList $ list [
    Core.floatTypeBigfloat, -- java.math.Bigfloat
    Core.floatTypeFloat32, -- float
    Core.floatTypeFloat64], -- double
  "functionVariants">: Sets.fromList $ list [
    Variants.functionVariantElimination,
    Variants.functionVariantLambda,
    Variants.functionVariantPrimitive],
  "integerTypes">: Sets.fromList $ list [
    Core.integerTypeBigint, -- java.math.BigInteger
    Core.integerTypeInt8, -- byte (signed, 8-bit)
    Core.integerTypeInt16, -- short (signed, 16-bit)
    Core.integerTypeInt32, -- int (signed, 32-bit)
    Core.integerTypeInt64, -- long (signed, 64-bit)
    Core.integerTypeUint16], -- char (unsigned, 16-bit)
  "termVariants">: Sets.fromList $ list [
    Variants.termVariantApplication,
    Variants.termVariantEither,
    Variants.termVariantFunction,
    Variants.termVariantLet,
    Variants.termVariantList,
    Variants.termVariantLiteral,
    Variants.termVariantMap,
    Variants.termVariantMaybe,
    Variants.termVariantPair,
    Variants.termVariantProduct,
    Variants.termVariantRecord,
    Variants.termVariantSet,
    Variants.termVariantUnion,
    Variants.termVariantVariable,
    Variants.termVariantWrap],
  "typeVariants">: Sets.fromList $ list [
    Variants.typeVariantAnnotated,
    Variants.typeVariantApplication,
    Variants.typeVariantEither,
    Variants.typeVariantFunction,
    Variants.typeVariantForall,
    Variants.typeVariantList,
    Variants.typeVariantLiteral,
    Variants.typeVariantMap,
    Variants.typeVariantMaybe,
    Variants.typeVariantPair,
    Variants.typeVariantProduct,
    Variants.typeVariantRecord,
    Variants.typeVariantSet,
    Variants.typeVariantUnion,
    Variants.typeVariantVariable,
    Variants.typeVariantWrap],
  "typePredicate">: lambda "typ" $ cases _Type (var "typ")
    (Just true) [
    _Type_product>>: lambda "types" $
      Equality.lt (Lists.length $ var "types") (ref javaMaxTupleLengthDef)]] $
  Coders.language
    (Coders.languageName $ string "hydra.ext.java")
    (Coders.languageConstraints
      (var "eliminationVariants")
      (var "literalVariants")
      (var "floatTypes")
      (var "functionVariants")
      (var "integerTypes")
      (var "termVariants")
      (var "typeVariants")
      (var "typePredicate"))

reservedWordsDef :: TBinding (S.Set String)
reservedWordsDef = javaLanguageDefinition "reservedWords" $
  doc "A set of reserved words in Java" $ lets [
  "specialNames">:
    doc "Special names reserved for use by Hydra" $
    list $ string <$> ["Elements"],
  "classNames">:
    doc ("java.lang classes as of JDK 7\n"
      <> "See: https://docs.oracle.com/javase/7/docs/api/java/lang/package-summary.html") $
    list $ string <$> [
      "AbstractMethodError", "Appendable", "ArithmeticException", "ArrayIndexOutOfBoundsException",
      "ArrayStoreException", "AssertionError", "AutoCloseable", "Boolean", "BootstrapMethodError", "Byte",
      "CharSequence", "Character", "Class", "ClassCastException", "ClassCircularityError", "ClassFormatError",
      "ClassLoader", "ClassNotFoundException", "ClassValue", "CloneNotSupportedException", "Cloneable", "Comparable",
      "Compiler", "Deprecated", "Double", "Enum", "EnumConstantNotPresentException", "Error", "Exception",
      "ExceptionInInitializerError", "Float", "IllegalAccessError", "IllegalAccessException",
      "IllegalArgumentException", "IllegalMonitorStateException", "IllegalStateException",
      "IllegalThreadStateException", "IncompatibleClassChangeError", "IndexOutOfBoundsException",
      "InheritableThreadLocal", "InstantiationError", "InstantiationException", "Integer", "InternalError",
      "InterruptedException", "Iterable", "LinkageError", "Long", "Math", "NegativeArraySizeException",
      "NoClassDefFoundError", "NoSuchFieldError", "NoSuchFieldException", "NoSuchMethodError", "NoSuchMethodException",
      "NullPointerException", "Number", "NumberFormatException", "Object", "OutOfMemoryError", "Override", "Package",
      "Process", "ProcessBuilder", "Readable", "ReflectiveOperationException", "Runnable", "Runtime",
      "RuntimeException", "RuntimePermission", "SafeVarargs", "SecurityException", "SecurityManager", "Short",
      "StackOverflowError", "StackTraceElement", "StrictMath", "String", "StringBuffer", "StringBuilder",
      "StringIndexOutOfBoundsException", "SuppressWarnings", "System", "Thread", "ThreadDeath",
      "ThreadGroup", "ThreadLocal", "Throwable", "TypeNotPresentException",
      "UnknownError", "UnsatisfiedLinkError", "UnsupportedClassVersionError",
      "UnsupportedOperationException", "VerifyError", "VirtualMachineError", "Void"],
  "keywords">:
    doc ("Keywords and literals are taken from Oracle's Java Tutorials on 2022-05-27; said to be complete for Java 1.8 only\n"
        <> "See: https://docs.oracle.com/javase/tutorial/java/nutsandbolts/_keywords.html") $
    list $ string <$> [
      "abstract", "assert", "boolean", "break", "byte", "case", "catch", "char", "class", "const", "continue",
      "default", "do", "double", "else", "enum", "extends", "final", "finally", "float", "for", "goto", "if",
      "implements", "import", "instanceof", "int", "interface", "long", "native", "new", "package", "private",
      "protected", "public", "return", "short", "static", "strictfp", "super", "switch", "synchronized", "this",
      "throw", "throws", "transient", "try", "void", "volatile", "while"],
  "literals">:
    list $ string <$> ["false", "null", "true"]] $
  Sets.fromList $ Lists.concat $ list [
    var "specialNames",
    var "classNames",
    var "keywords",
    var "literals"]
