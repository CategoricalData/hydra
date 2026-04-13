// Note: this is an automatically generated file. Do not edit.

package hydra.haskell;

/**
 * Utilities for working with Haskell syntax trees
 */
public interface Utils {
  static hydra.haskell.syntax.Pattern applicationPattern(hydra.haskell.syntax.Name name, java.util.List<hydra.haskell.syntax.Pattern> args) {
    return new hydra.haskell.syntax.Pattern.Application(new hydra.haskell.syntax.ApplicationPattern(name, args));
  }

  static hydra.haskell.syntax.Name elementReference(hydra.packaging.Namespaces<hydra.haskell.syntax.ModuleName> namespaces, hydra.core.Name name) {
    hydra.packaging.QualifiedName qname = hydra.Names.qualifyName(name);
    String local = (qname).local;
    String escLocal = hydra.haskell.Utils.sanitizeHaskellName(local);
    hydra.util.Lazy<hydra.util.Pair<hydra.packaging.Namespace, hydra.haskell.syntax.ModuleName>> namespacePair = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.packaging.Namespaces<hydra.haskell.syntax.ModuleName>, hydra.util.Pair<hydra.packaging.Namespace, hydra.haskell.syntax.ModuleName>>) (projected -> projected.focus)).apply(namespaces));
    hydra.util.Lazy<String> gmod = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(namespacePair.get()).value);
    hydra.util.Lazy<hydra.packaging.Namespace> gname = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(namespacePair.get()));
    hydra.util.Maybe<hydra.packaging.Namespace> mns = (qname).namespace;
    hydra.util.Lazy<java.util.Map<hydra.packaging.Namespace, hydra.haskell.syntax.ModuleName>> namespacesMap = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.packaging.Namespaces<hydra.haskell.syntax.ModuleName>, java.util.Map<hydra.packaging.Namespace, hydra.haskell.syntax.ModuleName>>) (projected -> projected.mapping)).apply(namespaces));
    return hydra.lib.maybes.Cases.applyLazy(
      (qname).namespace,
      () -> hydra.haskell.Utils.simpleName(local),
      (java.util.function.Function<hydra.packaging.Namespace, hydra.haskell.syntax.Name>) (ns -> hydra.lib.maybes.Cases.applyLazy(
        hydra.lib.maps.Lookup.apply(
          ns,
          namespacesMap.get()),
        () -> hydra.haskell.Utils.simpleName(local),
        (java.util.function.Function<hydra.haskell.syntax.ModuleName, hydra.haskell.syntax.Name>) (mn -> {
          String aliasStr = (mn).value;
          return hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              ns,
              gname.get()),
            () -> hydra.haskell.Utils.simpleName(escLocal),
            () -> hydra.haskell.Utils.rawName(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
              aliasStr,
              ".",
              hydra.haskell.Utils.sanitizeHaskellName(local)))));
        }))));
  }

  static hydra.haskell.syntax.Expression hsapp(hydra.haskell.syntax.Expression l, hydra.haskell.syntax.Expression r) {
    return new hydra.haskell.syntax.Expression.Application(new hydra.haskell.syntax.ApplicationExpression(l, r));
  }

  static hydra.haskell.syntax.Expression hslambda(hydra.haskell.syntax.Name name, hydra.haskell.syntax.Expression rhs) {
    return new hydra.haskell.syntax.Expression.Lambda(new hydra.haskell.syntax.LambdaExpression(java.util.Arrays.asList(new hydra.haskell.syntax.Pattern.Name(name)), rhs));
  }

  static hydra.haskell.syntax.Expression hslit(hydra.haskell.syntax.Literal lit) {
    return new hydra.haskell.syntax.Expression.Literal(lit);
  }

  static hydra.haskell.syntax.Expression hsvar(String s) {
    return new hydra.haskell.syntax.Expression.Variable(hydra.haskell.Utils.rawName(s));
  }

  static <T0> hydra.util.Either<hydra.errors.Error_, hydra.packaging.Namespaces<hydra.haskell.syntax.ModuleName>> namespacesForModule(hydra.packaging.Module mod, T0 cx, hydra.graph.Graph g) {
    return hydra.lib.eithers.Bind.apply(
      hydra.Analysis.<T0>moduleDependencyNamespaces(
        cx,
        g,
        true,
        true,
        true,
        true,
        mod),
      (java.util.function.Function<java.util.Set<hydra.packaging.Namespace>, hydra.util.Either<hydra.errors.Error_, hydra.packaging.Namespaces<hydra.haskell.syntax.ModuleName>>>) (nss -> {
        hydra.util.Lazy<java.util.List<hydra.packaging.Namespace>> nssAsList = new hydra.util.Lazy<>(() -> hydra.lib.sets.ToList.apply(nss));
        java.util.function.Function<hydra.packaging.Namespace, hydra.haskell.syntax.ModuleName> toModuleName = (java.util.function.Function<hydra.packaging.Namespace, hydra.haskell.syntax.ModuleName>) (namespace -> {
          String namespaceStr = (namespace).value;
          java.util.List<String> parts = hydra.lib.strings.SplitOn.apply(
            ".",
            namespaceStr);
          hydra.util.Lazy<String> lastPart = new hydra.util.Lazy<>(() -> hydra.lib.lists.Last.apply(parts));
          String capitalized = hydra.Formatting.capitalize(lastPart.get());
          return new hydra.haskell.syntax.ModuleName(capitalized);
        });
        java.util.function.Function<hydra.packaging.Namespace, hydra.util.Pair<hydra.packaging.Namespace, hydra.haskell.syntax.ModuleName>> toPair = (java.util.function.Function<hydra.packaging.Namespace, hydra.util.Pair<hydra.packaging.Namespace, hydra.haskell.syntax.ModuleName>>) (name -> (hydra.util.Pair<hydra.packaging.Namespace, hydra.haskell.syntax.ModuleName>) ((hydra.util.Pair<hydra.packaging.Namespace, hydra.haskell.syntax.ModuleName>) (new hydra.util.Pair<hydra.packaging.Namespace, hydra.haskell.syntax.ModuleName>(name, (toModuleName).apply(name)))));
        hydra.util.Lazy<java.util.List<hydra.util.Pair<hydra.packaging.Namespace, hydra.haskell.syntax.ModuleName>>> nssPairs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
          toPair,
          nssAsList.get()));
        hydra.util.Lazy<hydra.util.Pair<java.util.Map<hydra.packaging.Namespace, hydra.haskell.syntax.ModuleName>, java.util.Set<hydra.haskell.syntax.ModuleName>>> finalState = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
          p0 -> p1 -> hydra.haskell.Utils.<hydra.packaging.Namespace>namespacesForModule_addPair(
            p0,
            p1),
          hydra.haskell.Utils.<hydra.packaging.Namespace, hydra.haskell.syntax.ModuleName, hydra.haskell.syntax.ModuleName>namespacesForModule_emptyState(),
          nssPairs.get()));
        hydra.packaging.Namespace ns = (mod).namespace;
        hydra.util.Pair<hydra.packaging.Namespace, hydra.haskell.syntax.ModuleName> focusPair = (toPair).apply(ns);
        hydra.util.Lazy<java.util.Map<hydra.packaging.Namespace, hydra.haskell.syntax.ModuleName>> resultMap = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(finalState.get()));
        return hydra.util.Either.<hydra.errors.Error_, hydra.packaging.Namespaces<hydra.haskell.syntax.ModuleName>>right((hydra.packaging.Namespaces<hydra.haskell.syntax.ModuleName>) (new hydra.packaging.Namespaces<hydra.haskell.syntax.ModuleName>(focusPair, resultMap.get())));
      }));
  }

  static <T1> hydra.util.Pair<java.util.Map<T1, hydra.haskell.syntax.ModuleName>, java.util.Set<hydra.haskell.syntax.ModuleName>> namespacesForModule_addPair(hydra.util.Pair<java.util.Map<T1, hydra.haskell.syntax.ModuleName>, java.util.Set<hydra.haskell.syntax.ModuleName>> state, hydra.util.Pair<T1, hydra.haskell.syntax.ModuleName> namePair) {
    hydra.util.Lazy<hydra.haskell.syntax.ModuleName> alias = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(namePair));
    String aliasStr = alias.get().value;
    hydra.util.Lazy<java.util.Set<hydra.haskell.syntax.ModuleName>> currentSet = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(state));
    hydra.util.Lazy<T1> name = new hydra.util.Lazy<>(() -> hydra.haskell.Utils.<T1>namespacesForModule_name(namePair));
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.sets.Member.apply(
        alias.get(),
        currentSet.get()),
      () -> hydra.haskell.Utils.<T1>namespacesForModule_addPair(
        state,
        (hydra.util.Pair<T1, hydra.haskell.syntax.ModuleName>) ((hydra.util.Pair<T1, hydra.haskell.syntax.ModuleName>) (new hydra.util.Pair<T1, hydra.haskell.syntax.ModuleName>(name.get(), new hydra.haskell.syntax.ModuleName(hydra.lib.strings.Cat2.apply(
          aliasStr,
          "_")))))),
      () -> (hydra.util.Pair<java.util.Map<T1, hydra.haskell.syntax.ModuleName>, java.util.Set<hydra.haskell.syntax.ModuleName>>) ((hydra.util.Pair<java.util.Map<T1, hydra.haskell.syntax.ModuleName>, java.util.Set<hydra.haskell.syntax.ModuleName>>) (new hydra.util.Pair<java.util.Map<T1, hydra.haskell.syntax.ModuleName>, java.util.Set<hydra.haskell.syntax.ModuleName>>(hydra.lib.maps.Insert.apply(
        name.get(),
        alias.get(),
        hydra.haskell.Utils.<T1>namespacesForModule_currentMap(state)), hydra.lib.sets.Insert.apply(
        alias.get(),
        currentSet.get())))));
  }

  static <T1> java.util.Map<T1, hydra.haskell.syntax.ModuleName> namespacesForModule_currentMap(hydra.util.Pair<java.util.Map<T1, hydra.haskell.syntax.ModuleName>, java.util.Set<hydra.haskell.syntax.ModuleName>> state) {
    return hydra.lib.pairs.First.apply(state);
  }

  static <T1, T2, T3> hydra.util.Pair<java.util.Map<T1, T2>, java.util.Set<T3>> namespacesForModule_emptyState() {
    return (hydra.util.Pair<java.util.Map<T1, T2>, java.util.Set<T3>>) ((hydra.util.Pair<java.util.Map<T1, T2>, java.util.Set<T3>>) (new hydra.util.Pair<java.util.Map<T1, T2>, java.util.Set<T3>>((java.util.Map<T1, T2>) ((java.util.Map<T1, T2>) (hydra.lib.maps.Empty.<T1, T2>apply())), (java.util.Set<T3>) (hydra.lib.sets.Empty.<T3>apply()))));
  }

  static <T1> T1 namespacesForModule_name(hydra.util.Pair<T1, hydra.haskell.syntax.ModuleName> namePair) {
    return hydra.lib.pairs.First.apply(namePair);
  }

  static String newtypeAccessorName(hydra.core.Name name) {
    return hydra.lib.strings.Cat2.apply(
      "un",
      hydra.Names.localNameOf(name));
  }

  static hydra.haskell.syntax.Name rawName(String n) {
    return new hydra.haskell.syntax.Name.Normal(new hydra.haskell.syntax.QualifiedName((java.util.List<hydra.haskell.syntax.NamePart>) (java.util.Collections.<hydra.haskell.syntax.NamePart>emptyList()), new hydra.haskell.syntax.NamePart(n)));
  }

  static hydra.haskell.syntax.Name recordFieldReference(hydra.packaging.Namespaces<hydra.haskell.syntax.ModuleName> namespaces, hydra.core.Name sname, hydra.core.Name fname) {
    String fnameStr = (fname).value;
    String capitalized = hydra.Formatting.capitalize(fnameStr);
    String typeNameStr = hydra.haskell.Utils.typeNameForRecord(sname);
    String decapitalized = hydra.Formatting.decapitalize(typeNameStr);
    String nm = hydra.lib.strings.Cat2.apply(
      decapitalized,
      capitalized);
    hydra.packaging.QualifiedName qname = hydra.Names.qualifyName(sname);
    hydra.util.Maybe<hydra.packaging.Namespace> ns = (qname).namespace;
    hydra.packaging.QualifiedName qualName = new hydra.packaging.QualifiedName(ns, nm);
    hydra.core.Name unqualName = hydra.Names.unqualifyName(qualName);
    return hydra.haskell.Utils.elementReference(
      namespaces,
      unqualName);
  }

  static String sanitizeHaskellName(String v1) {
    return hydra.Formatting.sanitizeWithUnderscores(
      hydra.haskell.Language.reservedWords(),
      v1);
  }

  static hydra.haskell.syntax.Name simpleName(String arg_) {
    return hydra.haskell.Utils.rawName(hydra.haskell.Utils.sanitizeHaskellName(arg_));
  }

  static hydra.haskell.syntax.ValueBinding simpleValueBinding(hydra.haskell.syntax.Name hname, hydra.haskell.syntax.Expression rhs, hydra.util.Maybe<hydra.haskell.syntax.LocalBindings> bindings) {
    hydra.util.Lazy<hydra.haskell.syntax.Pattern> pat = new hydra.util.Lazy<>(() -> new hydra.haskell.syntax.Pattern.Application(new hydra.haskell.syntax.ApplicationPattern(hname, (java.util.List<hydra.haskell.syntax.Pattern>) (java.util.Collections.<hydra.haskell.syntax.Pattern>emptyList()))));
    hydra.haskell.syntax.RightHandSide rightHandSide = new hydra.haskell.syntax.RightHandSide(rhs);
    return new hydra.haskell.syntax.ValueBinding.Simple(new hydra.haskell.syntax.SimpleValueBinding(pat.get(), rightHandSide, bindings));
  }

  static hydra.haskell.syntax.Type toTypeApplication(java.util.List<hydra.haskell.syntax.Type> types) {
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<java.util.List<hydra.haskell.syntax.Type>, hydra.haskell.syntax.Type>> app = new java.util.concurrent.atomic.AtomicReference<>();
    app.set((java.util.function.Function<java.util.List<hydra.haskell.syntax.Type>, hydra.haskell.syntax.Type>) (l -> hydra.lib.logic.IfElse.lazy(
      hydra.lib.equality.Gt.apply(
        hydra.lib.lists.Length.apply(l),
        1),
      () -> new hydra.haskell.syntax.Type.Application(new hydra.haskell.syntax.ApplicationType(app.get().apply(hydra.lib.lists.Tail.apply(l)), hydra.lib.lists.Head.apply(l))),
      () -> hydra.lib.lists.Head.apply(l))));
    return app.get().apply(hydra.lib.lists.Reverse.apply(types));
  }

  static String typeNameForRecord(hydra.core.Name sname) {
    String snameStr = (sname).value;
    java.util.List<String> parts = hydra.lib.strings.SplitOn.apply(
      ".",
      snameStr);
    return hydra.lib.lists.Last.apply(parts);
  }

  static hydra.haskell.syntax.Name unionFieldReference(java.util.Set<hydra.core.Name> boundNames, hydra.packaging.Namespaces<hydra.haskell.syntax.ModuleName> namespaces, hydra.core.Name sname, hydra.core.Name fname) {
    String fnameStr = (fname).value;
    String capitalizedFieldName = hydra.Formatting.capitalize(fnameStr);
    String typeNameStr = hydra.haskell.Utils.typeNameForRecord(sname);
    String capitalizedTypeName = hydra.Formatting.capitalize(typeNameStr);
    hydra.packaging.QualifiedName qname = hydra.Names.qualifyName(sname);
    hydra.util.Maybe<hydra.packaging.Namespace> ns = (qname).namespace;
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<String, String>> deconflict = new java.util.concurrent.atomic.AtomicReference<>();
    deconflict.set((java.util.function.Function<String, String>) (name -> {
      hydra.core.Name tname = hydra.Names.unqualifyName(new hydra.packaging.QualifiedName(ns, name));
      return hydra.lib.logic.IfElse.lazy(
        hydra.lib.sets.Member.apply(
          tname,
          boundNames),
        () -> deconflict.get().apply(hydra.lib.strings.Cat2.apply(
          name,
          "_")),
        () -> name);
    }));
    String nm = deconflict.get().apply(hydra.lib.strings.Cat2.apply(
      capitalizedTypeName,
      capitalizedFieldName));
    hydra.packaging.QualifiedName qualName = new hydra.packaging.QualifiedName(ns, nm);
    hydra.core.Name unqualName = hydra.Names.unqualifyName(qualName);
    return hydra.haskell.Utils.elementReference(
      namespaces,
      unqualName);
  }

  static hydra.util.Pair<java.util.List<hydra.core.Name>, hydra.core.Type> unpackForallType(hydra.core.Type t) {
    return hydra.Strip.deannotateType(t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Pair<java.util.List<hydra.core.Name>, hydra.core.Type> otherwise(hydra.core.Type instance) {
        return (hydra.util.Pair<java.util.List<hydra.core.Name>, hydra.core.Type>) ((hydra.util.Pair<java.util.List<hydra.core.Name>, hydra.core.Type>) (new hydra.util.Pair<java.util.List<hydra.core.Name>, hydra.core.Type>((java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList()), t)));
      }

      @Override
      public hydra.util.Pair<java.util.List<hydra.core.Name>, hydra.core.Type> visit(hydra.core.Type.Forall fat) {
        hydra.core.Type tbody = (fat).value.body;
        hydra.util.Pair<java.util.List<hydra.core.Name>, hydra.core.Type> recursiveResult = hydra.haskell.Utils.unpackForallType(tbody);
        hydra.util.Lazy<hydra.core.Type> finalType = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(recursiveResult));
        hydra.core.Name v = (fat).value.parameter;
        hydra.util.Lazy<java.util.List<hydra.core.Name>> vars = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(recursiveResult));
        return (hydra.util.Pair<java.util.List<hydra.core.Name>, hydra.core.Type>) ((hydra.util.Pair<java.util.List<hydra.core.Name>, hydra.core.Type>) (new hydra.util.Pair<java.util.List<hydra.core.Name>, hydra.core.Type>(hydra.lib.lists.Cons.apply(
          v,
          vars.get()), finalType.get())));
      }
    });
  }
}
