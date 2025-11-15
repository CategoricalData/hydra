-- | Language constraints and reserved words for Java

module Hydra.Ext.Java.Language where

import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Meta as Meta
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | The maximum supported length of a tuple in Hydra-Java. Note: if this constant is changed, also change Tuples.java correspondingly
javaMaxTupleLength :: Int
javaMaxTupleLength = 9

-- | Language constraints for Java
javaLanguage :: Coders.Language
javaLanguage = Coders.Language {
  Coders.languageName = (Coders.LanguageName "hydra.ext.java"),
  Coders.languageConstraints = Coders.LanguageConstraints {
    Coders.languageConstraintsEliminationVariants = eliminationVariants,
    Coders.languageConstraintsLiteralVariants = literalVariants,
    Coders.languageConstraintsFloatTypes = floatTypes,
    Coders.languageConstraintsFunctionVariants = functionVariants,
    Coders.languageConstraintsIntegerTypes = integerTypes,
    Coders.languageConstraintsTermVariants = termVariants,
    Coders.languageConstraintsTypeVariants = typeVariants,
    Coders.languageConstraintsTypes = typePredicate}} 
  where 
    eliminationVariants = (Sets.fromList [
      Meta.EliminationVariantProduct,
      Meta.EliminationVariantRecord,
      Meta.EliminationVariantUnion,
      Meta.EliminationVariantWrap])
    literalVariants = (Sets.fromList [
      Meta.LiteralVariantBoolean,
      Meta.LiteralVariantFloat,
      Meta.LiteralVariantInteger,
      Meta.LiteralVariantString])
    floatTypes = (Sets.fromList [
      Core.FloatTypeBigfloat,
      Core.FloatTypeFloat32,
      Core.FloatTypeFloat64])
    functionVariants = (Sets.fromList [
      Meta.FunctionVariantElimination,
      Meta.FunctionVariantLambda,
      Meta.FunctionVariantPrimitive])
    integerTypes = (Sets.fromList [
      Core.IntegerTypeBigint,
      Core.IntegerTypeInt8,
      Core.IntegerTypeInt16,
      Core.IntegerTypeInt32,
      Core.IntegerTypeInt64,
      Core.IntegerTypeUint16])
    termVariants = (Sets.fromList [
      Meta.TermVariantApplication,
      Meta.TermVariantEither,
      Meta.TermVariantFunction,
      Meta.TermVariantLet,
      Meta.TermVariantList,
      Meta.TermVariantLiteral,
      Meta.TermVariantMap,
      Meta.TermVariantMaybe,
      Meta.TermVariantPair,
      Meta.TermVariantProduct,
      Meta.TermVariantRecord,
      Meta.TermVariantSet,
      Meta.TermVariantUnion,
      Meta.TermVariantVariable,
      Meta.TermVariantWrap])
    typeVariants = (Sets.fromList [
      Meta.TypeVariantAnnotated,
      Meta.TypeVariantApplication,
      Meta.TypeVariantEither,
      Meta.TypeVariantFunction,
      Meta.TypeVariantForall,
      Meta.TypeVariantList,
      Meta.TypeVariantLiteral,
      Meta.TypeVariantMap,
      Meta.TypeVariantMaybe,
      Meta.TypeVariantPair,
      Meta.TypeVariantProduct,
      Meta.TypeVariantRecord,
      Meta.TypeVariantSet,
      Meta.TypeVariantUnion,
      Meta.TypeVariantVariable,
      Meta.TypeVariantWrap])
    typePredicate = (\typ -> (\x -> case x of
      Core.TypeProduct v1 -> (Equality.lt (Lists.length v1) javaMaxTupleLength)
      _ -> True) typ)

-- | A set of reserved words in Java
reservedWords :: (S.Set String)
reservedWords = (Sets.fromList (Lists.concat [
  specialNames,
  classNames,
  keywords,
  literals])) 
  where 
    specialNames = [
      "Elements"]
    classNames = [
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
    keywords = [
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
    literals = [
      "false",
      "null",
      "true"]
