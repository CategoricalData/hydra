// Note: this is an automatically generated file. Do not edit.

package hydra.java;

/**
 * Java naming constants and package name utilities
 */
public interface Names {
  static String acceptMethodName() {
    return "accept";
  }

  static String applyMethodName() {
    return "apply";
  }

  static String compareToMethodName() {
    return "compareTo";
  }

  static String equalsMethodName() {
    return "equals";
  }

  static String getMethodName() {
    return "get";
  }

  static String hashCodeMethodName() {
    return "hashCode";
  }

  static hydra.util.Maybe<hydra.java.syntax.PackageName> hydraCorePackageName() {
    return hydra.util.Maybe.just(hydra.java.Names.javaPackageName(java.util.Arrays.asList(
      "hydra",
      "core")));
  }

  static hydra.util.Maybe<hydra.java.syntax.PackageName> hydraUtilPackageName() {
    return hydra.util.Maybe.just(hydra.java.Names.javaPackageName(java.util.Arrays.asList(
      "hydra",
      "util")));
  }

  static String instanceName() {
    return "instance";
  }

  static hydra.util.Maybe<hydra.java.syntax.PackageName> javaLangPackageName() {
    return hydra.util.Maybe.just(hydra.java.Names.javaPackageName(java.util.Arrays.asList(
      "java",
      "lang")));
  }

  static hydra.java.syntax.PackageName javaPackageName(java.util.List<String> parts) {
    return new hydra.java.syntax.PackageName(hydra.lib.lists.Map.apply(
      (java.util.function.Function<String, hydra.java.syntax.Identifier>) (p -> new hydra.java.syntax.Identifier(p)),
      parts));
  }

  static hydra.util.Maybe<hydra.java.syntax.PackageName> javaUtilFunctionPackageName() {
    return hydra.util.Maybe.just(hydra.java.Names.javaPackageName(java.util.Arrays.asList(
      "java",
      "util",
      "function")));
  }

  static hydra.util.Maybe<hydra.java.syntax.PackageName> javaUtilPackageName() {
    return hydra.util.Maybe.just(hydra.java.Names.javaPackageName(java.util.Arrays.asList(
      "java",
      "util")));
  }

  static String otherInstanceName() {
    return "other";
  }

  static String otherwiseMethodName() {
    return "otherwise";
  }

  static String partialVisitorName() {
    return "PartialVisitor";
  }

  static String setMethodName() {
    return "set";
  }

  static String valueFieldName() {
    return "value";
  }

  static String visitMethodName() {
    return "visit";
  }

  static String visitorName() {
    return "Visitor";
  }

  static String visitorReturnParameter() {
    return "R";
  }
}
