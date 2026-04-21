// Note: this is an automatically generated file. Do not edit.

/**
 * Language constraints and reserved words for Java
 */



import * as Ast from "../ast.js";
import * as Classes from "../classes.js";
import * as Coders from "../coders.js";
import * as Context from "../context.js";
import * as Core from "../core.js";
import * as ErrorChecking from "../error/checking.js";
import * as ErrorCore from "../error/core.js";
import * as ErrorPackaging from "../error/packaging.js";
import * as Errors from "../errors.js";
import * as Graph from "../graph.js";
import * as JsonModel from "../json/model.js";
import * as Lexical from "../lexical.js";
import * as LibLists from "../lib/lists.js";
import * as LibSets from "../lib/sets.js";
import * as Packaging from "../packaging.js";
import * as Parsing from "../parsing.js";
import * as Paths from "../paths.js";
import * as Phantoms from "../phantoms.js";
import * as Query from "../query.js";
import * as Relational from "../relational.js";
import * as Tabular from "../tabular.js";
import * as Testing from "../testing.js";
import * as Topology from "../topology.js";
import * as Typing from "../typing.js";
import * as Util from "../util.js";
import * as Variants from "../variants.js";

export const javaLanguage: Coders.Language = (() => {
  const eliminationVariants = LibSets.fromList([({ tag: "record" }), ({ tag: "union" }), ({ tag: "wrap" })]);
  const literalVariants = LibSets.fromList([({ tag: "binary" }), ({ tag: "boolean" }), ({ tag: "float" }), ({ tag: "integer" }), ({ tag: "string" })]);
  const floatTypes = LibSets.fromList([({ tag: "bigfloat" }), ({ tag: "float32" }), ({ tag: "float64" })]);
  const functionVariants = LibSets.fromList([({ tag: "elimination" }), ({ tag: "lambda" })]);
  const integerTypes = LibSets.fromList([({ tag: "bigint" }), ({ tag: "int8" }), ({ tag: "int16" }), ({ tag: "int32" }), ({ tag: "int64" }), ({ tag: "uint16" })]);
  const termVariants = LibSets.fromList([({ tag: "application" }), ({ tag: "either" }), ({ tag: "cases" }), ({ tag: "lambda" }), ({ tag: "project" }), ({ tag: "unwrap" }), ({ tag: "typeApplication" }), ({ tag: "typeLambda" }), ({ tag: "let" }), ({ tag: "list" }), ({ tag: "literal" }), ({ tag: "map" }), ({ tag: "maybe" }), ({ tag: "pair" }), ({ tag: "record" }), ({ tag: "set" }), ({ tag: "inject" }), ({ tag: "unit" }), ({ tag: "variable" }), ({ tag: "wrap" })]);
  const typeVariants = LibSets.fromList([({ tag: "annotated" }), ({ tag: "application" }), ({ tag: "either" }), ({ tag: "function" }), ({ tag: "forall" }), ({ tag: "list" }), ({ tag: "literal" }), ({ tag: "map" }), ({ tag: "maybe" }), ({ tag: "pair" }), ({ tag: "record" }), ({ tag: "set" }), ({ tag: "union" }), ({ tag: "unit" }), ({ tag: "variable" }), ({ tag: "void" }), ({ tag: "wrap" })]);
  const typePredicate = ((_: t0) => true);
  return ({
    name: "hydra.java",
    constraints: ({
    eliminationVariants: eliminationVariants,
    literalVariants: literalVariants,
    floatTypes: floatTypes,
    functionVariants: functionVariants,
    integerTypes: integerTypes,
    termVariants: termVariants,
    typeVariants: typeVariants,
    types: typePredicate
  })
  });
})();

export const javaMaxTupleLength: number = 9;

export const reservedWords: ReadonlySet<string> = (() => {
  const specialNames = ["Elements"];
  const classNames = ["AbstractMethodError", "Appendable", "ArithmeticException", "ArrayIndexOutOfBoundsException", "ArrayStoreException", "AssertionError", "AutoCloseable", "Boolean", "BootstrapMethodError", "Byte", "CharSequence", "Character", "Class", "ClassCastException", "ClassCircularityError", "ClassFormatError", "ClassLoader", "ClassNotFoundException", "ClassValue", "CloneNotSupportedException", "Cloneable", "Comparable", "Compiler", "Deprecated", "Double", "Enum", "EnumConstantNotPresentException", "Error", "Exception", "ExceptionInInitializerError", "Float", "IllegalAccessError", "IllegalAccessException", "IllegalArgumentException", "IllegalMonitorStateException", "IllegalStateException", "IllegalThreadStateException", "IncompatibleClassChangeError", "IndexOutOfBoundsException", "InheritableThreadLocal", "InstantiationError", "InstantiationException", "Integer", "InternalError", "InterruptedException", "Iterable", "LinkageError", "Long", "Math", "NegativeArraySizeException", "NoClassDefFoundError", "NoSuchFieldError", "NoSuchFieldException", "NoSuchMethodError", "NoSuchMethodException", "NullPointerException", "Number", "NumberFormatException", "Object", "OutOfMemoryError", "Override", "Package", "Process", "ProcessBuilder", "Readable", "ReflectiveOperationException", "Runnable", "Runtime", "RuntimeException", "RuntimePermission", "SafeVarargs", "SecurityException", "SecurityManager", "Short", "StackOverflowError", "StackTraceElement", "StrictMath", "String", "StringBuffer", "StringBuilder", "StringIndexOutOfBoundsException", "SuppressWarnings", "System", "Thread", "ThreadDeath", "ThreadGroup", "ThreadLocal", "Throwable", "TypeNotPresentException", "UnknownError", "UnsatisfiedLinkError", "UnsupportedClassVersionError", "UnsupportedOperationException", "VerifyError", "VirtualMachineError", "Void"];
  const keywords = ["abstract", "assert", "boolean", "break", "byte", "case", "catch", "char", "class", "const", "continue", "default", "do", "double", "else", "enum", "extends", "final", "finally", "float", "for", "goto", "if", "implements", "import", "instanceof", "int", "interface", "long", "native", "new", "package", "private", "protected", "public", "return", "short", "static", "strictfp", "super", "switch", "synchronized", "this", "throw", "throws", "transient", "try", "void", "volatile", "while"];
  const literals = ["false", "null", "true"];
  return LibSets.fromList(LibLists.concat([specialNames, classNames, keywords, literals]));
})();
