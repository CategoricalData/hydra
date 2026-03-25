// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell;

/**
 * Utilities for working with Haskell syntax trees
 */
public interface Utils {
  static hydra.ext.haskell.syntax.Pattern applicationPattern(hydra.ext.haskell.syntax.Name name, hydra.util.ConsList<hydra.ext.haskell.syntax.Pattern> args) {
    return new hydra.ext.haskell.syntax.Pattern.Application(new hydra.ext.haskell.syntax.ApplicationPattern(name, args));
  }

  static hydra.ext.haskell.syntax.Name elementReference(hydra.module.Namespaces<hydra.ext.haskell.syntax.ModuleName> namespaces, hydra.core.Name name) {
    hydra.module.QualifiedName qname = hydra.Names.qualifyName(name);
    String local = (qname).local;
    String escLocal = hydra.ext.haskell.Utils.sanitizeHaskellName(local);
    hydra.util.Lazy<hydra.util.Pair<hydra.module.Namespace, hydra.ext.haskell.syntax.ModuleName>> namespacePair = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.module.Namespaces<hydra.ext.haskell.syntax.ModuleName>, hydra.util.Pair<hydra.module.Namespace, hydra.ext.haskell.syntax.ModuleName>>) (projected -> projected.focus)).apply(namespaces));
    hydra.util.Lazy<String> gmod = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(namespacePair.get()).value);
    hydra.util.Lazy<hydra.module.Namespace> gname = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(namespacePair.get()));
    hydra.util.Maybe<hydra.module.Namespace> mns = (qname).namespace;
    hydra.util.Lazy<hydra.util.PersistentMap<hydra.module.Namespace, hydra.ext.haskell.syntax.ModuleName>> namespacesMap = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.module.Namespaces<hydra.ext.haskell.syntax.ModuleName>, hydra.util.PersistentMap<hydra.module.Namespace, hydra.ext.haskell.syntax.ModuleName>>) (projected -> projected.mapping)).apply(namespaces));
    return hydra.lib.maybes.Cases.applyLazy(
      (qname).namespace,
      () -> hydra.ext.haskell.Utils.simpleName(local),
      (java.util.function.Function<hydra.module.Namespace, hydra.ext.haskell.syntax.Name>) (ns -> hydra.lib.maybes.Cases.applyLazy(
        hydra.lib.maps.Lookup.apply(
          ns,
          namespacesMap.get()),
        () -> hydra.ext.haskell.Utils.simpleName(local),
        (java.util.function.Function<hydra.ext.haskell.syntax.ModuleName, hydra.ext.haskell.syntax.Name>) (mn -> {
          String aliasStr = (mn).value;
          return hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              ns,
              gname.get()),
            () -> hydra.ext.haskell.Utils.simpleName(escLocal),
            () -> hydra.ext.haskell.Utils.rawName(hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
              aliasStr,
              ".",
              hydra.ext.haskell.Utils.sanitizeHaskellName(local)))));
        }))));
  }

  static hydra.ext.haskell.syntax.Expression hsapp(hydra.ext.haskell.syntax.Expression l, hydra.ext.haskell.syntax.Expression r) {
    return new hydra.ext.haskell.syntax.Expression.Application(new hydra.ext.haskell.syntax.ApplicationExpression(l, r));
  }

  static hydra.ext.haskell.syntax.Expression hslambda(hydra.ext.haskell.syntax.Name name, hydra.ext.haskell.syntax.Expression rhs) {
    return new hydra.ext.haskell.syntax.Expression.Lambda(new hydra.ext.haskell.syntax.LambdaExpression(hydra.util.ConsList.of(new hydra.ext.haskell.syntax.Pattern.Name(name)), rhs));
  }

  static hydra.ext.haskell.syntax.Expression hslit(hydra.ext.haskell.syntax.Literal lit) {
    return new hydra.ext.haskell.syntax.Expression.Literal(lit);
  }

  static hydra.ext.haskell.syntax.Expression hsvar(String s) {
    return new hydra.ext.haskell.syntax.Expression.Variable(hydra.ext.haskell.Utils.rawName(s));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.module.Namespaces<hydra.ext.haskell.syntax.ModuleName>> namespacesForModule(hydra.module.Module mod, hydra.context.Context cx, hydra.graph.Graph g) {
    return hydra.lib.eithers.Bind.apply(
      hydra.Schemas.moduleDependencyNamespaces(
        cx,
        g,
        true,
        true,
        true,
        true,
        mod),
      (java.util.function.Function<hydra.util.PersistentSet<hydra.module.Namespace>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.module.Namespaces<hydra.ext.haskell.syntax.ModuleName>>>) (nss -> {
        hydra.util.Lazy<hydra.util.ConsList<hydra.module.Namespace>> nssAsList = new hydra.util.Lazy<>(() -> hydra.lib.sets.ToList.apply(nss));
        java.util.function.Function<hydra.module.Namespace, hydra.ext.haskell.syntax.ModuleName> toModuleName = (java.util.function.Function<hydra.module.Namespace, hydra.ext.haskell.syntax.ModuleName>) (namespace -> {
          String namespaceStr = (namespace).value;
          hydra.util.ConsList<String> parts = hydra.lib.strings.SplitOn.apply(
            ".",
            namespaceStr);
          hydra.util.Lazy<String> lastPart = new hydra.util.Lazy<>(() -> hydra.lib.lists.Last.apply(parts));
          String capitalized = hydra.Formatting.capitalize(lastPart.get());
          return new hydra.ext.haskell.syntax.ModuleName(capitalized);
        });
        java.util.function.Function<hydra.module.Namespace, hydra.util.Pair<hydra.module.Namespace, hydra.ext.haskell.syntax.ModuleName>> toPair = (java.util.function.Function<hydra.module.Namespace, hydra.util.Pair<hydra.module.Namespace, hydra.ext.haskell.syntax.ModuleName>>) (name -> (hydra.util.Pair<hydra.module.Namespace, hydra.ext.haskell.syntax.ModuleName>) ((hydra.util.Pair<hydra.module.Namespace, hydra.ext.haskell.syntax.ModuleName>) (new hydra.util.Pair<hydra.module.Namespace, hydra.ext.haskell.syntax.ModuleName>(name, (toModuleName).apply(name)))));
        hydra.util.Lazy<hydra.util.ConsList<hydra.util.Pair<hydra.module.Namespace, hydra.ext.haskell.syntax.ModuleName>>> nssPairs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
          toPair,
          nssAsList.get()));
        hydra.util.Lazy<hydra.util.Pair<hydra.util.PersistentMap<hydra.module.Namespace, hydra.ext.haskell.syntax.ModuleName>, hydra.util.PersistentSet<hydra.ext.haskell.syntax.ModuleName>>> finalState = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
          p0 -> p1 -> hydra.ext.haskell.Utils.<hydra.module.Namespace>namespacesForModule_addPair(
            p0,
            p1),
          hydra.ext.haskell.Utils.<hydra.module.Namespace, hydra.ext.haskell.syntax.ModuleName, hydra.ext.haskell.syntax.ModuleName>namespacesForModule_emptyState(),
          nssPairs.get()));
        hydra.module.Namespace ns = (mod).namespace;
        hydra.util.Pair<hydra.module.Namespace, hydra.ext.haskell.syntax.ModuleName> focusPair = (toPair).apply(ns);
        hydra.util.Lazy<hydra.util.PersistentMap<hydra.module.Namespace, hydra.ext.haskell.syntax.ModuleName>> resultMap = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(finalState.get()));
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.module.Namespaces<hydra.ext.haskell.syntax.ModuleName>>right((hydra.module.Namespaces<hydra.ext.haskell.syntax.ModuleName>) (new hydra.module.Namespaces<hydra.ext.haskell.syntax.ModuleName>(focusPair, resultMap.get())));
      }));
  }

  static <T0> hydra.util.Pair<hydra.util.PersistentMap<T0, hydra.ext.haskell.syntax.ModuleName>, hydra.util.PersistentSet<hydra.ext.haskell.syntax.ModuleName>> namespacesForModule_addPair(hydra.util.Pair<hydra.util.PersistentMap<T0, hydra.ext.haskell.syntax.ModuleName>, hydra.util.PersistentSet<hydra.ext.haskell.syntax.ModuleName>> state, hydra.util.Pair<T0, hydra.ext.haskell.syntax.ModuleName> namePair) {
    hydra.util.Lazy<hydra.ext.haskell.syntax.ModuleName> alias = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(namePair));
    String aliasStr = alias.get().value;
    hydra.util.Lazy<hydra.util.PersistentSet<hydra.ext.haskell.syntax.ModuleName>> currentSet = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(state));
    hydra.util.Lazy<T0> name = new hydra.util.Lazy<>(() -> hydra.ext.haskell.Utils.<T0>namespacesForModule_name(namePair));
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.sets.Member.apply(
        alias.get(),
        currentSet.get()),
      () -> hydra.ext.haskell.Utils.<T0>namespacesForModule_addPair(
        state,
        (hydra.util.Pair<T0, hydra.ext.haskell.syntax.ModuleName>) ((hydra.util.Pair<T0, hydra.ext.haskell.syntax.ModuleName>) (new hydra.util.Pair<T0, hydra.ext.haskell.syntax.ModuleName>(name.get(), new hydra.ext.haskell.syntax.ModuleName(hydra.lib.strings.Cat2.apply(
          aliasStr,
          "_")))))),
      () -> (hydra.util.Pair<hydra.util.PersistentMap<T0, hydra.ext.haskell.syntax.ModuleName>, hydra.util.PersistentSet<hydra.ext.haskell.syntax.ModuleName>>) ((hydra.util.Pair<hydra.util.PersistentMap<T0, hydra.ext.haskell.syntax.ModuleName>, hydra.util.PersistentSet<hydra.ext.haskell.syntax.ModuleName>>) (new hydra.util.Pair<hydra.util.PersistentMap<T0, hydra.ext.haskell.syntax.ModuleName>, hydra.util.PersistentSet<hydra.ext.haskell.syntax.ModuleName>>(hydra.lib.maps.Insert.apply(
        name.get(),
        alias.get(),
        hydra.ext.haskell.Utils.<T0>namespacesForModule_currentMap(state)), hydra.lib.sets.Insert.apply(
        alias.get(),
        currentSet.get())))));
  }

  static <T0> hydra.util.PersistentMap<T0, hydra.ext.haskell.syntax.ModuleName> namespacesForModule_currentMap(hydra.util.Pair<hydra.util.PersistentMap<T0, hydra.ext.haskell.syntax.ModuleName>, hydra.util.PersistentSet<hydra.ext.haskell.syntax.ModuleName>> state) {
    return hydra.lib.pairs.First.apply(state);
  }

  static <T0, T1, T2> hydra.util.Pair<hydra.util.PersistentMap<T0, T1>, hydra.util.PersistentSet<T2>> namespacesForModule_emptyState() {
    return (hydra.util.Pair<hydra.util.PersistentMap<T0, T1>, hydra.util.PersistentSet<T2>>) ((hydra.util.Pair<hydra.util.PersistentMap<T0, T1>, hydra.util.PersistentSet<T2>>) (new hydra.util.Pair<hydra.util.PersistentMap<T0, T1>, hydra.util.PersistentSet<T2>>((hydra.util.PersistentMap<T0, T1>) ((hydra.util.PersistentMap<T0, T1>) (hydra.lib.maps.Empty.<T0, T1>apply())), (hydra.util.PersistentSet<T2>) (hydra.lib.sets.Empty.<T2>apply()))));
  }

  static <T0> T0 namespacesForModule_name(hydra.util.Pair<T0, hydra.ext.haskell.syntax.ModuleName> namePair) {
    return hydra.lib.pairs.First.apply(namePair);
  }

  static String newtypeAccessorName(hydra.core.Name name) {
    return hydra.lib.strings.Cat2.apply(
      "un",
      hydra.Names.localNameOf(name));
  }

  static hydra.ext.haskell.syntax.Name rawName(String n) {
    return new hydra.ext.haskell.syntax.Name.Normal(new hydra.ext.haskell.syntax.QualifiedName((hydra.util.ConsList<hydra.ext.haskell.syntax.NamePart>) (hydra.util.ConsList.<hydra.ext.haskell.syntax.NamePart>empty()), new hydra.ext.haskell.syntax.NamePart(n)));
  }

  static hydra.ext.haskell.syntax.Name recordFieldReference(hydra.module.Namespaces<hydra.ext.haskell.syntax.ModuleName> namespaces, hydra.core.Name sname, hydra.core.Name fname) {
    String fnameStr = (fname).value;
    String capitalized = hydra.Formatting.capitalize(fnameStr);
    String typeNameStr = hydra.ext.haskell.Utils.typeNameForRecord(sname);
    String decapitalized = hydra.Formatting.decapitalize(typeNameStr);
    String nm = hydra.lib.strings.Cat2.apply(
      decapitalized,
      capitalized);
    hydra.module.QualifiedName qname = hydra.Names.qualifyName(sname);
    hydra.util.Maybe<hydra.module.Namespace> ns = (qname).namespace;
    hydra.module.QualifiedName qualName = new hydra.module.QualifiedName(ns, nm);
    hydra.core.Name unqualName = hydra.Names.unqualifyName(qualName);
    return hydra.ext.haskell.Utils.elementReference(
      namespaces,
      unqualName);
  }

  static String sanitizeHaskellName(String v1) {
    return hydra.Formatting.sanitizeWithUnderscores(
      hydra.ext.haskell.Language.reservedWords(),
      v1);
  }

  static hydra.ext.haskell.syntax.Name simpleName(String arg_) {
    return hydra.ext.haskell.Utils.rawName(hydra.ext.haskell.Utils.sanitizeHaskellName(arg_));
  }

  static hydra.ext.haskell.syntax.ValueBinding simpleValueBinding(hydra.ext.haskell.syntax.Name hname, hydra.ext.haskell.syntax.Expression rhs, hydra.util.Maybe<hydra.ext.haskell.syntax.LocalBindings> bindings) {
    hydra.util.Lazy<hydra.ext.haskell.syntax.Pattern> pat = new hydra.util.Lazy<>(() -> new hydra.ext.haskell.syntax.Pattern.Application(new hydra.ext.haskell.syntax.ApplicationPattern(hname, (hydra.util.ConsList<hydra.ext.haskell.syntax.Pattern>) (hydra.util.ConsList.<hydra.ext.haskell.syntax.Pattern>empty()))));
    hydra.ext.haskell.syntax.RightHandSide rightHandSide = new hydra.ext.haskell.syntax.RightHandSide(rhs);
    return new hydra.ext.haskell.syntax.ValueBinding.Simple(new hydra.ext.haskell.syntax.SimpleValueBinding(pat.get(), rightHandSide, bindings));
  }

  static hydra.ext.haskell.syntax.Type toTypeApplication(hydra.util.ConsList<hydra.ext.haskell.syntax.Type> types) {
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.util.ConsList<hydra.ext.haskell.syntax.Type>, hydra.ext.haskell.syntax.Type>> app = new java.util.concurrent.atomic.AtomicReference<>();
    app.set((java.util.function.Function<hydra.util.ConsList<hydra.ext.haskell.syntax.Type>, hydra.ext.haskell.syntax.Type>) (l -> hydra.lib.logic.IfElse.lazy(
      hydra.lib.equality.Gt.apply(
        hydra.lib.lists.Length.apply(l),
        1),
      () -> new hydra.ext.haskell.syntax.Type.Application(new hydra.ext.haskell.syntax.ApplicationType(app.get().apply(hydra.lib.lists.Tail.apply(l)), hydra.lib.lists.Head.apply(l))),
      () -> hydra.lib.lists.Head.apply(l))));
    return app.get().apply(hydra.lib.lists.Reverse.apply(types));
  }

  static String typeNameForRecord(hydra.core.Name sname) {
    String snameStr = (sname).value;
    hydra.util.ConsList<String> parts = hydra.lib.strings.SplitOn.apply(
      ".",
      snameStr);
    return hydra.lib.lists.Last.apply(parts);
  }

  static hydra.ext.haskell.syntax.Name unionFieldReference(hydra.util.PersistentSet<hydra.core.Name> boundNames, hydra.module.Namespaces<hydra.ext.haskell.syntax.ModuleName> namespaces, hydra.core.Name sname, hydra.core.Name fname) {
    String fnameStr = (fname).value;
    String capitalizedFieldName = hydra.Formatting.capitalize(fnameStr);
    String typeNameStr = hydra.ext.haskell.Utils.typeNameForRecord(sname);
    String capitalizedTypeName = hydra.Formatting.capitalize(typeNameStr);
    hydra.module.QualifiedName qname = hydra.Names.qualifyName(sname);
    hydra.util.Maybe<hydra.module.Namespace> ns = (qname).namespace;
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<String, String>> deconflict = new java.util.concurrent.atomic.AtomicReference<>();
    deconflict.set((java.util.function.Function<String, String>) (name -> {
      hydra.core.Name tname = hydra.Names.unqualifyName(new hydra.module.QualifiedName(ns, name));
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
    hydra.module.QualifiedName qualName = new hydra.module.QualifiedName(ns, nm);
    hydra.core.Name unqualName = hydra.Names.unqualifyName(qualName);
    return hydra.ext.haskell.Utils.elementReference(
      namespaces,
      unqualName);
  }

  static hydra.util.Pair<hydra.util.ConsList<hydra.core.Name>, hydra.core.Type> unpackForallType(hydra.core.Type t) {
    return hydra.Rewriting.deannotateType(t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Pair<hydra.util.ConsList<hydra.core.Name>, hydra.core.Type> otherwise(hydra.core.Type instance) {
        return (hydra.util.Pair<hydra.util.ConsList<hydra.core.Name>, hydra.core.Type>) ((hydra.util.Pair<hydra.util.ConsList<hydra.core.Name>, hydra.core.Type>) (new hydra.util.Pair<hydra.util.ConsList<hydra.core.Name>, hydra.core.Type>((hydra.util.ConsList<hydra.core.Name>) (hydra.util.ConsList.<hydra.core.Name>empty()), t)));
      }

      @Override
      public hydra.util.Pair<hydra.util.ConsList<hydra.core.Name>, hydra.core.Type> visit(hydra.core.Type.Forall fat) {
        hydra.core.Type tbody = (fat).value.body;
        hydra.util.Pair<hydra.util.ConsList<hydra.core.Name>, hydra.core.Type> recursiveResult = hydra.ext.haskell.Utils.unpackForallType(tbody);
        hydra.util.Lazy<hydra.core.Type> finalType = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(recursiveResult));
        hydra.core.Name v = (fat).value.parameter;
        hydra.util.Lazy<hydra.util.ConsList<hydra.core.Name>> vars = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(recursiveResult));
        return (hydra.util.Pair<hydra.util.ConsList<hydra.core.Name>, hydra.core.Type>) ((hydra.util.Pair<hydra.util.ConsList<hydra.core.Name>, hydra.core.Type>) (new hydra.util.Pair<hydra.util.ConsList<hydra.core.Name>, hydra.core.Type>(hydra.lib.lists.Cons.apply(
          v,
          vars.get()), finalType.get())));
      }
    });
  }
}
