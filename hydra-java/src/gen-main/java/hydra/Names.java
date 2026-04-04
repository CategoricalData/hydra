// Note: this is an automatically generated file. Do not edit.

package hydra;

/**
 * Functions for working with qualified names.
 */
public interface Names {
  static String compactName(java.util.Map<hydra.module.Namespace, String> namespaces, hydra.core.Name name) {
    hydra.module.QualifiedName qualName = hydra.Names.qualifyName(name);
    String local = (qualName).local;
    hydra.util.Maybe<hydra.module.Namespace> mns = (qualName).namespace;
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> (name).value,
      (java.util.function.Function<hydra.module.Namespace, String>) (ns -> hydra.lib.maybes.Maybe.applyLazy(
        () -> local,
        (java.util.function.Function<String, String>) (pre -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
          pre,
          ":",
          local))),
        hydra.lib.maps.Lookup.apply(
          ns,
          namespaces))),
      mns);
  }

  static hydra.util.Pair<hydra.core.Name, hydra.context.Context> freshName(hydra.context.Context cx) {
    Integer count = hydra.Annotations.getCount(
      hydra.Constants.key_freshTypeVariableCount(),
      cx);
    return (hydra.util.Pair<hydra.core.Name, hydra.context.Context>) ((hydra.util.Pair<hydra.core.Name, hydra.context.Context>) (new hydra.util.Pair<hydra.core.Name, hydra.context.Context>(hydra.Names.normalTypeVariable(count), hydra.Annotations.putCount(
      hydra.Constants.key_freshTypeVariableCount(),
      hydra.lib.math.Add.apply(
        count,
        1),
      cx))));
  }

  static hydra.util.Pair<java.util.List<hydra.core.Name>, hydra.context.Context> freshNames(Integer n, hydra.context.Context cx) {
    return hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<hydra.util.Pair<java.util.List<hydra.core.Name>, hydra.context.Context>, java.util.function.Function<java.lang.Void, hydra.util.Pair<java.util.List<hydra.core.Name>, hydra.context.Context>>>) (v1 -> (java.util.function.Function<java.lang.Void, hydra.util.Pair<java.util.List<hydra.core.Name>, hydra.context.Context>>) (v2 -> hydra.Names.freshNames_go(
        hydra.Names::freshName,
        v1,
        v2))),
      (hydra.util.Pair<java.util.List<hydra.core.Name>, hydra.context.Context>) ((hydra.util.Pair<java.util.List<hydra.core.Name>, hydra.context.Context>) (new hydra.util.Pair<java.util.List<hydra.core.Name>, hydra.context.Context>((java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList()), cx))),
      hydra.lib.lists.Replicate.apply(
        n,
        null));
  }

  static <T0> hydra.util.Pair<java.util.List<hydra.core.Name>, hydra.context.Context> freshNames_go(java.util.function.Function<hydra.context.Context, hydra.util.Pair<hydra.core.Name, hydra.context.Context>> hydra_names_freshName, hydra.util.Pair<java.util.List<hydra.core.Name>, hydra.context.Context> acc, T0 ignored) {
    hydra.util.Lazy<hydra.context.Context> cx0 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(acc));
    hydra.util.Pair<hydra.core.Name, hydra.context.Context> result = (hydra_names_freshName).apply(cx0.get());
    hydra.util.Lazy<hydra.context.Context> cx1 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(result));
    hydra.util.Lazy<hydra.core.Name> name = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(result));
    hydra.util.Lazy<java.util.List<hydra.core.Name>> names = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(acc));
    return (hydra.util.Pair<java.util.List<hydra.core.Name>, hydra.context.Context>) ((hydra.util.Pair<java.util.List<hydra.core.Name>, hydra.context.Context>) (new hydra.util.Pair<java.util.List<hydra.core.Name>, hydra.context.Context>(hydra.lib.lists.Concat2.apply(
      names.get(),
      hydra.lib.lists.Pure.apply(name.get())), cx1.get())));
  }

  static String localNameOf(hydra.core.Name arg_) {
    return hydra.Names.qualifyName(arg_).local;
  }

  static hydra.util.Maybe<hydra.module.Namespace> namespaceOf(hydra.core.Name arg_) {
    return hydra.Names.qualifyName(arg_).namespace;
  }

  static String namespaceToFilePath(hydra.util.CaseConvention caseConv, hydra.module.FileExtension ext, hydra.module.Namespace ns) {
    hydra.util.Lazy<java.util.List<String>> parts = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<String, String>) (v1 -> hydra.Formatting.convertCase(
        new hydra.util.CaseConvention.Camel(),
        caseConv,
        v1)),
      hydra.lib.strings.SplitOn.apply(
        ".",
        (ns).value)));
    return hydra.lib.strings.Cat2.apply(
      hydra.lib.strings.Cat2.apply(
        hydra.lib.strings.Intercalate.apply(
          "/",
          parts.get()),
        "."),
      (ext).value);
  }

  static hydra.core.Name normalTypeVariable(Integer i) {
    return new hydra.core.Name(hydra.lib.strings.Cat2.apply(
      "t",
      hydra.lib.literals.ShowInt32.apply(i)));
  }

  static hydra.core.Name qname(hydra.module.Namespace ns, String name) {
    return new hydra.core.Name(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
      (ns).value,
      ".",
      name)));
  }

  static hydra.module.QualifiedName qualifyName(hydra.core.Name name) {
    hydra.util.Lazy<java.util.List<String>> parts = new hydra.util.Lazy<>(() -> hydra.lib.lists.Reverse.apply(hydra.lib.strings.SplitOn.apply(
      ".",
      (name).value)));
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.equality.Equal.apply(
        1,
        hydra.lib.lists.Length.apply(parts.get())),
      () -> new hydra.module.QualifiedName((hydra.util.Maybe<hydra.module.Namespace>) (hydra.util.Maybe.<hydra.module.Namespace>nothing()), (name).value),
      () -> new hydra.module.QualifiedName(hydra.util.Maybe.just(new hydra.module.Namespace(hydra.lib.strings.Intercalate.apply(
        ".",
        hydra.lib.lists.Reverse.apply(hydra.lib.lists.Tail.apply(parts.get()))))), hydra.lib.lists.Head.apply(parts.get())));
  }

  static String uniqueLabel(java.util.Set<String> visited, String l) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.sets.Member.apply(
        l,
        visited),
      () -> hydra.Names.uniqueLabel(
        visited,
        hydra.lib.strings.Cat2.apply(
          l,
          "'")),
      () -> l);
  }

  static hydra.core.Name unqualifyName(hydra.module.QualifiedName qname) {
    hydra.util.Lazy<String> prefix = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
      () -> "",
      (java.util.function.Function<hydra.module.Namespace, String>) (n -> hydra.lib.strings.Cat2.apply(
        (n).value,
        ".")),
      (qname).namespace));
    return new hydra.core.Name(hydra.lib.strings.Cat2.apply(
      prefix.get(),
      (qname).local));
  }
}
