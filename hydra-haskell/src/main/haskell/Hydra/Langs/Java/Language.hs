module Hydra.Langs.Java.Language where

import Hydra.Kernel

import qualified Data.Set as S


javaLanguage :: Language a
javaLanguage = Language (LanguageName "hydra/langs/java") $ LanguageConstraints {
  languageConstraintsEliminationVariants = S.fromList eliminationVariants,

  languageConstraintsLiteralVariants = S.fromList [
    LiteralVariantBoolean, -- boolean
    LiteralVariantFloat, -- (see float types)
    LiteralVariantInteger, -- (see integer types)
    LiteralVariantString], -- string
  languageConstraintsFloatTypes = S.fromList [
    -- Bigfloat (e.g. as Java's BigDecimal) is excluded for now
    FloatTypeFloat32, -- float
    FloatTypeFloat64], -- double
  languageConstraintsFunctionVariants = S.fromList functionVariants,
  languageConstraintsIntegerTypes = S.fromList [
    IntegerTypeBigint, -- BigInteger
    IntegerTypeInt16, -- short
    IntegerTypeInt32, -- int
    IntegerTypeInt64, -- long
    IntegerTypeUint8, -- byte
    IntegerTypeUint16], -- char
  languageConstraintsTermVariants = S.fromList [
    TermVariantApplication,
    TermVariantFunction,
    TermVariantLet,
    TermVariantList,
    TermVariantLiteral,
    TermVariantMap,
    TermVariantWrap,
    TermVariantOptional,
    TermVariantRecord,
    TermVariantSet,
    TermVariantUnion,
    TermVariantVariable],
  languageConstraintsTypeVariants = S.fromList [
    TypeVariantAnnotated,
    TypeVariantApplication,
    TypeVariantFunction,
    TypeVariantLambda,
    TypeVariantList,
    TypeVariantLiteral,
    TypeVariantMap,
    TypeVariantWrap,
    TypeVariantOptional,
    TypeVariantRecord,
    TypeVariantSet,
    TypeVariantUnion,
    TypeVariantVariable],
  languageConstraintsTypes = const True }

reservedWords :: S.Set String
reservedWords = S.fromList $ specialNames ++ classNames ++ keywords ++ literals
  where
    -- Special names reserved for use by Hydra
    specialNames = ["Elements"]

    -- java.lang classes as of JDK 7
    -- See: https://docs.oracle.com/javase/7/docs/api/java/lang/package-summary.html
    classNames = [
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
      "UnsupportedOperationException", "VerifyError", "VirtualMachineError", "Void"]
    -- Keywords and literals are taken from Oracle's Java Tutorials on 2022-05-27; said to be complete for Java 1.8 only
    -- See: https://docs.oracle.com/javase/tutorial/java/nutsandbolts/_keywords.html
    keywords = [
      "abstract", "assert", "boolean", "break", "byte", "case", "catch", "char", "class", "const", "continue",
      "default", "do", "double", "else", "enum", "extends", "final", "finally", "float", "for", "goto", "if",
      "implements", "import", "instanceof", "int", "interface", "long", "native", "new", "package", "private",
      "protected", "public", "return", "short", "static", "strictfp", "super", "switch", "synchronized", "this",
      "throw", "throws", "transient", "try", "void", "volatile", "while"]
    literals = ["false", "null", "true"]
