-- Note: this is an automatically generated file. Do not edit.
-- | Language constraints and reserved words for Java

module Hydra.Java.Language where
import qualified Hydra.Ast as Ast
import qualified Hydra.Classes as Classes
import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Errors as Errors
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as Model
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Haskell.Lib.Lists as Lists
import qualified Hydra.Haskell.Lib.Sets as Sets
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Query as Query
import qualified Hydra.Relational as Relational
import qualified Hydra.Tabular as Tabular
import qualified Hydra.Testing as Testing
import qualified Hydra.Topology as Topology
import qualified Hydra.Typed as Typed
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Validation as Validation
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Set as S
-- | Language constraints for Java
javaLanguage :: Coders.Language
javaLanguage =
    Coders.Language {
      Coders.languageName = (Coders.LanguageName "hydra.java"),
      Coders.languageConstraints = Coders.LanguageConstraints {
        Coders.languageConstraintsLiteralVariants = literalVariants,
        Coders.languageConstraintsFloatTypes = floatTypes,
        Coders.languageConstraintsIntegerTypes = integerTypes,
        Coders.languageConstraintsTermVariants = termVariants,
        Coders.languageConstraintsTypeVariants = typeVariants,
        Coders.languageConstraintsTypes = typePredicate},
      Coders.languageSupportedFeatures = (Sets.fromList [
        Coders.LanguageFeatureNestedCaseStatements]),
      Coders.languageCaseConventions = Coders.CaseConventions {
        Coders.caseConventionsConstant = Util.CaseConventionUpperSnake,
        Coders.caseConventionsDirectory = Util.CaseConventionCamel,
        Coders.caseConventionsEnumValue = Util.CaseConventionUpperSnake,
        Coders.caseConventionsField = Util.CaseConventionCamel,
        Coders.caseConventionsFile = Util.CaseConventionPascal,
        Coders.caseConventionsModule = Util.CaseConventionCamel,
        Coders.caseConventionsTerm = Util.CaseConventionCamel,
        Coders.caseConventionsTermVariable = Util.CaseConventionCamel,
        Coders.caseConventionsType = Util.CaseConventionPascal,
        Coders.caseConventionsTypeVariable = Util.CaseConventionPascal},
      Coders.languageDefaultFileExtension = (Util.FileExtension "java")}
  where
    literalVariants =
        Sets.fromList [
          Variants.LiteralVariantBinary,
          Variants.LiteralVariantBoolean,
          Variants.LiteralVariantDecimal,
          Variants.LiteralVariantFloat,
          Variants.LiteralVariantInteger,
          Variants.LiteralVariantString]
    floatTypes =
        Sets.fromList [
          Core.FloatTypeFloat32,
          Core.FloatTypeFloat64]
    integerTypes =
        Sets.fromList [
          Core.IntegerTypeBigint,
          Core.IntegerTypeInt8,
          Core.IntegerTypeInt16,
          Core.IntegerTypeInt32,
          Core.IntegerTypeInt64,
          Core.IntegerTypeUint16]
    termVariants =
        Sets.fromList [
          Variants.TermVariantApplication,
          Variants.TermVariantEither,
          Variants.TermVariantCases,
          Variants.TermVariantLambda,
          Variants.TermVariantProject,
          Variants.TermVariantUnwrap,
          Variants.TermVariantTypeApplication,
          Variants.TermVariantTypeLambda,
          Variants.TermVariantLet,
          Variants.TermVariantList,
          Variants.TermVariantLiteral,
          Variants.TermVariantMap,
          Variants.TermVariantMaybe,
          Variants.TermVariantPair,
          Variants.TermVariantRecord,
          Variants.TermVariantSet,
          Variants.TermVariantInject,
          Variants.TermVariantUnit,
          Variants.TermVariantVariable,
          Variants.TermVariantWrap]
    typeVariants =
        Sets.fromList [
          Variants.TypeVariantAnnotated,
          Variants.TypeVariantApplication,
          Variants.TypeVariantEither,
          Variants.TypeVariantFunction,
          Variants.TypeVariantForall,
          Variants.TypeVariantList,
          Variants.TypeVariantLiteral,
          Variants.TypeVariantMap,
          Variants.TypeVariantMaybe,
          Variants.TypeVariantPair,
          Variants.TypeVariantRecord,
          Variants.TypeVariantSet,
          Variants.TypeVariantUnion,
          Variants.TypeVariantUnit,
          Variants.TypeVariantVariable,
          Variants.TypeVariantVoid,
          Variants.TypeVariantWrap]
    typePredicate = \_ -> True
-- | The maximum supported length of a tuple in Hydra-Java. Note: if this constant is changed, also change Tuples.java correspondingly
javaMaxTupleLength :: Int
javaMaxTupleLength = 9
-- | A set of reserved words in Java
reservedWords :: S.Set String
reservedWords =
    Sets.fromList (Lists.concat [
      specialNames,
      classNames,
      keywords,
      literals])
  where
    specialNames = [
      "Elements"]
    classNames =
        [
          "AbstractMethodError",
          "Appendable",
          "ArithmeticException",
          "ArrayIndexOutOfBoundsException",
          "ArrayStoreException",
          "AssertionError",
          "AutoCloseable",
          "Boolean",
          "BootstrapMethodError",
          "Byte",
          "CharSequence",
          "Character",
          "Class",
          "ClassCastException",
          "ClassCircularityError",
          "ClassFormatError",
          "ClassLoader",
          "ClassNotFoundException",
          "ClassValue",
          "CloneNotSupportedException",
          "Cloneable",
          "Comparable",
          "Compiler",
          "Deprecated",
          "Double",
          "Enum",
          "EnumConstantNotPresentException",
          "Error",
          "Exception",
          "ExceptionInInitializerError",
          "Float",
          "IllegalAccessError",
          "IllegalAccessException",
          "IllegalArgumentException",
          "IllegalMonitorStateException",
          "IllegalStateException",
          "IllegalThreadStateException",
          "IncompatibleClassChangeError",
          "IndexOutOfBoundsException",
          "InheritableThreadLocal",
          "InstantiationError",
          "InstantiationException",
          "Integer",
          "InternalError",
          "InterruptedException",
          "Iterable",
          "LinkageError",
          "Long",
          "Math",
          "NegativeArraySizeException",
          "NoClassDefFoundError",
          "NoSuchFieldError",
          "NoSuchFieldException",
          "NoSuchMethodError",
          "NoSuchMethodException",
          "NullPointerException",
          "Number",
          "NumberFormatException",
          "Object",
          "OutOfMemoryError",
          "Override",
          "Package",
          "Process",
          "ProcessBuilder",
          "Readable",
          "ReflectiveOperationException",
          "Runnable",
          "Runtime",
          "RuntimeException",
          "RuntimePermission",
          "SafeVarargs",
          "SecurityException",
          "SecurityManager",
          "Short",
          "StackOverflowError",
          "StackTraceElement",
          "StrictMath",
          "String",
          "StringBuffer",
          "StringBuilder",
          "StringIndexOutOfBoundsException",
          "SuppressWarnings",
          "System",
          "Thread",
          "ThreadDeath",
          "ThreadGroup",
          "ThreadLocal",
          "Throwable",
          "TypeNotPresentException",
          "UnknownError",
          "UnsatisfiedLinkError",
          "UnsupportedClassVersionError",
          "UnsupportedOperationException",
          "VerifyError",
          "VirtualMachineError",
          "Void"]
    keywords =
        [
          "abstract",
          "assert",
          "boolean",
          "break",
          "byte",
          "case",
          "catch",
          "char",
          "class",
          "const",
          "continue",
          "default",
          "do",
          "double",
          "else",
          "enum",
          "extends",
          "final",
          "finally",
          "float",
          "for",
          "goto",
          "if",
          "implements",
          "import",
          "instanceof",
          "int",
          "interface",
          "long",
          "native",
          "new",
          "package",
          "private",
          "protected",
          "public",
          "return",
          "short",
          "static",
          "strictfp",
          "super",
          "switch",
          "synchronized",
          "this",
          "throw",
          "throws",
          "transient",
          "try",
          "void",
          "volatile",
          "while"]
    literals =
        [
          "false",
          "null",
          "true"]
