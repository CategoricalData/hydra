package hydra.java.language

import hydra.coders.*

import hydra.core.*

import hydra.variants.*

lazy val javaLanguage: hydra.coders.Language = {
  lazy val eliminationVariants: scala.collection.immutable.Set[hydra.variants.EliminationVariant] = hydra.lib.sets.fromList[hydra.variants.EliminationVariant](Seq(hydra.variants.EliminationVariant.record,
     hydra.variants.EliminationVariant.union, hydra.variants.EliminationVariant.wrap))
  lazy val literalVariants: scala.collection.immutable.Set[hydra.variants.LiteralVariant] = hydra.lib.sets.fromList[hydra.variants.LiteralVariant](Seq(hydra.variants.LiteralVariant.binary,
     hydra.variants.LiteralVariant.boolean, hydra.variants.LiteralVariant.decimal, hydra.variants.LiteralVariant.float,
     hydra.variants.LiteralVariant.integer, hydra.variants.LiteralVariant.string))
  lazy val floatTypes: scala.collection.immutable.Set[hydra.core.FloatType] = hydra.lib.sets.fromList[hydra.core.FloatType](Seq(hydra.core.FloatType.bigfloat,
     hydra.core.FloatType.float32, hydra.core.FloatType.float64))
  lazy val functionVariants: scala.collection.immutable.Set[hydra.variants.FunctionVariant] = hydra.lib.sets.fromList[hydra.variants.FunctionVariant](Seq(hydra.variants.FunctionVariant.elimination,
     hydra.variants.FunctionVariant.lambda))
  lazy val integerTypes: scala.collection.immutable.Set[hydra.core.IntegerType] = hydra.lib.sets.fromList[hydra.core.IntegerType](Seq(hydra.core.IntegerType.bigint,
     hydra.core.IntegerType.int8, hydra.core.IntegerType.int16, hydra.core.IntegerType.int32,
     hydra.core.IntegerType.int64, hydra.core.IntegerType.uint16))
  lazy val termVariants: scala.collection.immutable.Set[hydra.variants.TermVariant] = hydra.lib.sets.fromList[hydra.variants.TermVariant](Seq(hydra.variants.TermVariant.application,
     hydra.variants.TermVariant.either, hydra.variants.TermVariant.cases, hydra.variants.TermVariant.lambda,
     hydra.variants.TermVariant.project, hydra.variants.TermVariant.unwrap, hydra.variants.TermVariant.typeApplication,
     hydra.variants.TermVariant.typeLambda, hydra.variants.TermVariant.let, hydra.variants.TermVariant.list,
     hydra.variants.TermVariant.literal, hydra.variants.TermVariant.map, hydra.variants.TermVariant.maybe,
     hydra.variants.TermVariant.pair, hydra.variants.TermVariant.record, hydra.variants.TermVariant.set,
     hydra.variants.TermVariant.inject, hydra.variants.TermVariant.unit, hydra.variants.TermVariant.variable,
     hydra.variants.TermVariant.wrap))
  lazy val typeVariants: scala.collection.immutable.Set[hydra.variants.TypeVariant] = hydra.lib.sets.fromList[hydra.variants.TypeVariant](Seq(hydra.variants.TypeVariant.annotated,
     hydra.variants.TypeVariant.application, hydra.variants.TypeVariant.either, hydra.variants.TypeVariant.function,
     hydra.variants.TypeVariant.forall, hydra.variants.TypeVariant.list, hydra.variants.TypeVariant.literal,
     hydra.variants.TypeVariant.map, hydra.variants.TypeVariant.maybe, hydra.variants.TypeVariant.pair,
     hydra.variants.TypeVariant.record, hydra.variants.TypeVariant.set, hydra.variants.TypeVariant.union,
     hydra.variants.TypeVariant.unit, hydra.variants.TypeVariant.variable, hydra.variants.TypeVariant.void,
     hydra.variants.TypeVariant.wrap))
  def typePredicate[T0](_x: T0): Boolean = true
  hydra.coders.Language("hydra.java", hydra.coders.LanguageConstraints(eliminationVariants,
     literalVariants, floatTypes, functionVariants, integerTypes, termVariants, typeVariants,
     typePredicate))
}

lazy val javaMaxTupleLength: Int = 9

lazy val reservedWords: scala.collection.immutable.Set[scala.Predef.String] = {
  lazy val specialNames: Seq[scala.Predef.String] = Seq("Elements")
  lazy val classNames: Seq[scala.Predef.String] = Seq("AbstractMethodError", "Appendable",
     "ArithmeticException", "ArrayIndexOutOfBoundsException", "ArrayStoreException",
     "AssertionError", "AutoCloseable", "Boolean", "BootstrapMethodError", "Byte",
     "CharSequence", "Character", "Class", "ClassCastException", "ClassCircularityError",
     "ClassFormatError", "ClassLoader", "ClassNotFoundException", "ClassValue", "CloneNotSupportedException",
     "Cloneable", "Comparable", "Compiler", "Deprecated", "Double", "Enum", "EnumConstantNotPresentException",
     "Error", "Exception", "ExceptionInInitializerError", "Float", "IllegalAccessError",
     "IllegalAccessException", "IllegalArgumentException", "IllegalMonitorStateException",
     "IllegalStateException", "IllegalThreadStateException", "IncompatibleClassChangeError",
     "IndexOutOfBoundsException", "InheritableThreadLocal", "InstantiationError",
     "InstantiationException", "Integer", "InternalError", "InterruptedException",
     "Iterable", "LinkageError", "Long", "Math", "NegativeArraySizeException", "NoClassDefFoundError",
     "NoSuchFieldError", "NoSuchFieldException", "NoSuchMethodError", "NoSuchMethodException",
     "NullPointerException", "Number", "NumberFormatException", "Object", "OutOfMemoryError",
     "Override", "Package", "Process", "ProcessBuilder", "Readable", "ReflectiveOperationException",
     "Runnable", "Runtime", "RuntimeException", "RuntimePermission", "SafeVarargs",
     "SecurityException", "SecurityManager", "Short", "StackOverflowError", "StackTraceElement",
     "StrictMath", "String", "StringBuffer", "StringBuilder", "StringIndexOutOfBoundsException",
     "SuppressWarnings", "System", "Thread", "ThreadDeath", "ThreadGroup", "ThreadLocal",
     "Throwable", "TypeNotPresentException", "UnknownError", "UnsatisfiedLinkError",
     "UnsupportedClassVersionError", "UnsupportedOperationException", "VerifyError",
     "VirtualMachineError", "Void")
  lazy val keywords: Seq[scala.Predef.String] = Seq("abstract", "assert", "boolean",
     "break", "byte", "case", "catch", "char", "class", "const", "continue", "default",
     "do", "double", "else", "enum", "extends", "final", "finally", "float", "for",
     "goto", "if", "implements", "import", "instanceof", "int", "interface", "long",
     "native", "new", "package", "private", "protected", "public", "return", "short",
     "static", "strictfp", "super", "switch", "synchronized", "this", "throw", "throws",
     "transient", "try", "void", "volatile", "while")
  lazy val literals: Seq[scala.Predef.String] = Seq("false", "null", "true")
  hydra.lib.sets.fromList[scala.Predef.String](hydra.lib.lists.concat[scala.Predef.String](Seq(specialNames,
     classNames, keywords, literals)))
}
