// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.names;

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
  
  static String instanceName() {
    return "instance";
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
  
  static hydra.ext.java.syntax.PackageName javaPackageName(java.util.List<String> parts) {
    return new hydra.ext.java.syntax.PackageName(hydra.lib.lists.Map.apply(
      (java.util.function.Function<String, hydra.ext.java.syntax.Identifier>) (p -> new hydra.ext.java.syntax.Identifier(p)),
      parts));
  }
  
  static hydra.util.Maybe<hydra.ext.java.syntax.PackageName> hydraCorePackageName() {
    return hydra.util.Maybe.just(hydra.ext.java.names.Names.javaPackageName(java.util.List.of(
      "hydra",
      "core")));
  }
  
  static hydra.util.Maybe<hydra.ext.java.syntax.PackageName> hydraUtilPackageName() {
    return hydra.util.Maybe.just(hydra.ext.java.names.Names.javaPackageName(java.util.List.of(
      "hydra",
      "util")));
  }
  
  static hydra.util.Maybe<hydra.ext.java.syntax.PackageName> javaLangPackageName() {
    return hydra.util.Maybe.just(hydra.ext.java.names.Names.javaPackageName(java.util.List.of(
      "java",
      "lang")));
  }
  
  static hydra.util.Maybe<hydra.ext.java.syntax.PackageName> javaUtilFunctionPackageName() {
    return hydra.util.Maybe.just(hydra.ext.java.names.Names.javaPackageName(java.util.List.of(
      "java",
      "util",
      "function")));
  }
  
  static hydra.util.Maybe<hydra.ext.java.syntax.PackageName> javaUtilPackageName() {
    return hydra.util.Maybe.just(hydra.ext.java.names.Names.javaPackageName(java.util.List.of(
      "java",
      "util")));
  }
}
