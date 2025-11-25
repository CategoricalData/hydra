// Note: this is an automatically generated file. Do not edit.

package hydra.names;

/**
 * Functions for working with qualified names.
 */
public interface Names {
  static String compactName(java.util.Map<hydra.module.Namespace, String> namespaces, hydra.core.Name name) {
    hydra.module.QualifiedName qualName = hydra.names.Names.qualifyName((name));
    String local = ((hydra.module.QualifiedName) ((qualName))).local;
    hydra.util.Maybe<hydra.module.Namespace> mns = ((hydra.module.QualifiedName) ((qualName))).namespace;
    return hydra.lib.maybes.Maybe.apply(
      ((name)).value,
      (java.util.function.Function<hydra.module.Namespace, String>) (ns -> hydra.lib.maybes.Maybe.apply(
        (local),
        (java.util.function.Function<String, String>) (pre -> hydra.lib.strings.Cat.apply(java.util.List.of(
          (pre),
          ":",
          (local)))),
        hydra.lib.maps.Lookup.apply(
          (ns),
          (namespaces)))),
      (mns));
  }
  
  static String localNameOf(hydra.core.Name arg_) {
    return ((hydra.module.QualifiedName) (hydra.names.Names.qualifyName((arg_)))).local;
  }
  
  static hydra.util.Maybe<hydra.module.Namespace> namespaceOf(hydra.core.Name arg_) {
    return ((hydra.module.QualifiedName) (hydra.names.Names.qualifyName((arg_)))).namespace;
  }
  
  static String namespaceToFilePath(hydra.util.CaseConvention caseConv, hydra.module.FileExtension ext, hydra.module.Namespace ns) {
    java.util.List<String> parts = hydra.lib.lists.Map.apply(
      (java.util.function.Function<String, String>) (v1 -> hydra.formatting.Formatting.convertCase(
        new hydra.util.CaseConvention.Camel(true),
        (caseConv),
        (v1))),
      hydra.lib.strings.SplitOn.apply(
        ".",
        ((ns)).value));
    return hydra.lib.strings.Cat.apply(java.util.List.of(
      hydra.lib.strings.Cat.apply(java.util.List.of(
        hydra.lib.strings.Intercalate.apply(
          "/",
          (parts)),
        ".")),
      ((ext)).value));
  }
  
  static hydra.core.Name qname(hydra.module.Namespace ns, String name) {
    return new hydra.core.Name(hydra.lib.strings.Cat.apply(java.util.List.of(
      ((ns)).value,
      ".",
      (name))));
  }
  
  static hydra.module.QualifiedName qualifyName(hydra.core.Name name) {
    java.util.List<String> parts = hydra.lib.lists.Reverse.apply(hydra.lib.strings.SplitOn.apply(
      ".",
      ((name)).value));
    return hydra.lib.logic.IfElse.apply(
      hydra.lib.equality.Equal.apply(
        1,
        hydra.lib.lists.Length.apply((parts))),
      new hydra.module.QualifiedName((hydra.util.Maybe<hydra.module.Namespace>) ((hydra.util.Maybe) (hydra.util.Maybe.<hydra.module.Namespace>nothing())), ((name)).value),
      new hydra.module.QualifiedName(hydra.util.Maybe.just(new hydra.module.Namespace(hydra.lib.strings.Intercalate.apply(
        ".",
        hydra.lib.lists.Reverse.apply(hydra.lib.lists.Tail.apply((parts)))))), hydra.lib.lists.Head.apply((parts))));
  }
  
  static String uniqueLabel(java.util.Set<String> visited, String l) {
    return hydra.lib.logic.IfElse.apply(
      hydra.lib.sets.Member.apply(
        (l),
        (visited)),
      hydra.names.Names.uniqueLabel(
        (visited),
        hydra.lib.strings.Cat2.apply(
          (l),
          "'")),
      (l));
  }
  
  static hydra.core.Name unqualifyName(hydra.module.QualifiedName qname) {
    String prefix = hydra.lib.maybes.Maybe.apply(
      "",
      (java.util.function.Function<hydra.module.Namespace, String>) (n -> hydra.lib.strings.Cat.apply(java.util.List.of(
        ((n)).value,
        "."))),
      ((hydra.module.QualifiedName) ((qname))).namespace);
    return new hydra.core.Name(hydra.lib.strings.Cat.apply(java.util.List.of(
      (prefix),
      ((hydra.module.QualifiedName) ((qname))).local)));
  }
}
