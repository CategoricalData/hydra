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
        java.util.function.Function<java.util.List<String>, java.util.function.Function<Integer, hydra.haskell.syntax.ModuleName>> aliasFromSuffix = (java.util.function.Function<java.util.List<String>, java.util.function.Function<Integer, hydra.haskell.syntax.ModuleName>>) (segs -> (java.util.function.Function<Integer, hydra.haskell.syntax.ModuleName>) (n -> {
          hydra.util.Lazy<Integer> dropCount = new hydra.util.Lazy<>(() -> hydra.lib.math.Sub.apply(
            hydra.lib.lists.Length.apply(segs),
            n));
          hydra.util.Lazy<java.util.List<String>> suffix = new hydra.util.Lazy<>(() -> hydra.lib.lists.Drop.apply(
            dropCount.get(),
            segs));
          hydra.util.Lazy<java.util.List<String>> capitalizedSuffix = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
            hydra.Formatting::capitalize,
            suffix.get()));
          return new hydra.haskell.syntax.ModuleName(hydra.lib.strings.Cat.apply(capitalizedSuffix.get()));
        }));
        hydra.util.Lazy<java.util.List<hydra.packaging.Namespace>> nssAsList = new hydra.util.Lazy<>(() -> hydra.lib.sets.ToList.apply(nss));
        hydra.util.Lazy<java.util.Map<hydra.packaging.Namespace, Integer>> initialState = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.packaging.Namespace, hydra.util.Pair<hydra.packaging.Namespace, Integer>>) (nm -> (hydra.util.Pair<hydra.packaging.Namespace, Integer>) ((hydra.util.Pair<hydra.packaging.Namespace, Integer>) (new hydra.util.Pair<hydra.packaging.Namespace, Integer>(nm, 1)))),
          nssAsList.get())));
        java.util.function.Function<hydra.packaging.Namespace, java.util.List<String>> segmentsOf = (java.util.function.Function<hydra.packaging.Namespace, java.util.List<String>>) (namespace -> hydra.lib.strings.SplitOn.apply(
          ".",
          (namespace).value));
        hydra.util.Lazy<Integer> maxSegs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
          (java.util.function.Function<Integer, java.util.function.Function<Integer, Integer>>) (a -> (java.util.function.Function<Integer, Integer>) (b -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Gt.apply(
              a,
              b),
            () -> a,
            () -> b))),
          1,
          hydra.lib.lists.Map.apply(
            (java.util.function.Function<hydra.packaging.Namespace, Integer>) (nm -> hydra.lib.lists.Length.apply((segmentsOf).apply(nm))),
            nssAsList.get())));
        hydra.util.Lazy<java.util.Map<hydra.packaging.Namespace, java.util.List<String>>> segsMap = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.packaging.Namespace, hydra.util.Pair<hydra.packaging.Namespace, java.util.List<String>>>) (nm -> (hydra.util.Pair<hydra.packaging.Namespace, java.util.List<String>>) ((hydra.util.Pair<hydra.packaging.Namespace, java.util.List<String>>) (new hydra.util.Pair<hydra.packaging.Namespace, java.util.List<String>>(nm, (segmentsOf).apply(nm))))),
          nssAsList.get())));
        java.util.function.Function<hydra.packaging.Namespace, java.util.List<String>> segsFor = (java.util.function.Function<hydra.packaging.Namespace, java.util.List<String>>) (nm -> hydra.lib.maybes.FromMaybe.applyLazy(
          () -> (java.util.List<String>) (java.util.Collections.<String>emptyList()),
          hydra.lib.maps.Lookup.apply(
            nm,
            segsMap.get())));
        hydra.util.Lazy<java.util.Map<hydra.packaging.Namespace, Integer>> finalState = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
          (java.util.function.Function<java.util.Map<hydra.packaging.Namespace, Integer>, java.util.function.Function<java.lang.Void, java.util.Map<hydra.packaging.Namespace, Integer>>>) (v1 -> (java.util.function.Function<java.lang.Void, java.util.Map<hydra.packaging.Namespace, Integer>>) (v2 -> hydra.haskell.Utils.<java.lang.Void>namespacesForModule_growStep(
            aliasFromSuffix,
            nssAsList.get(),
            segsFor,
            v1,
            v2))),
          initialState.get(),
          hydra.lib.lists.Replicate.apply(
            maxSegs.get(),
            null)));
        hydra.packaging.Namespace ns = (mod).namespace;
        java.util.function.Function<hydra.packaging.Namespace, hydra.haskell.syntax.ModuleName> toModuleName = (java.util.function.Function<hydra.packaging.Namespace, hydra.haskell.syntax.ModuleName>) (namespace -> (aliasFromSuffix).apply((segmentsOf).apply(namespace)).apply(1));
        hydra.util.Lazy<hydra.util.Pair<hydra.packaging.Namespace, hydra.haskell.syntax.ModuleName>> focusPair = new hydra.util.Lazy<>(() -> (hydra.util.Pair<hydra.packaging.Namespace, hydra.haskell.syntax.ModuleName>) ((hydra.util.Pair<hydra.packaging.Namespace, hydra.haskell.syntax.ModuleName>) (new hydra.util.Pair<hydra.packaging.Namespace, hydra.haskell.syntax.ModuleName>(ns, (toModuleName).apply(ns)))));
        hydra.util.Lazy<java.util.Map<hydra.packaging.Namespace, hydra.haskell.syntax.ModuleName>> resultMap = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.packaging.Namespace, hydra.util.Pair<hydra.packaging.Namespace, hydra.haskell.syntax.ModuleName>>) (nm -> (hydra.util.Pair<hydra.packaging.Namespace, hydra.haskell.syntax.ModuleName>) ((hydra.util.Pair<hydra.packaging.Namespace, hydra.haskell.syntax.ModuleName>) (new hydra.util.Pair<hydra.packaging.Namespace, hydra.haskell.syntax.ModuleName>(nm, (aliasFromSuffix).apply((segsFor).apply(nm)).apply(hydra.haskell.Utils.namespacesForModule_takenFor(
            finalState.get(),
            nm)))))),
          nssAsList.get())));
        return hydra.util.Either.<hydra.errors.Error_, hydra.packaging.Namespaces<hydra.haskell.syntax.ModuleName>>right((hydra.packaging.Namespaces<hydra.haskell.syntax.ModuleName>) (new hydra.packaging.Namespaces<hydra.haskell.syntax.ModuleName>(focusPair.get(), resultMap.get())));
      }));
  }

  static <T1> java.util.Map<hydra.packaging.Namespace, Integer> namespacesForModule_growStep(java.util.function.Function<java.util.List<String>, java.util.function.Function<Integer, hydra.haskell.syntax.ModuleName>> aliasFromSuffix, java.util.List<hydra.packaging.Namespace> nssAsList, java.util.function.Function<hydra.packaging.Namespace, java.util.List<String>> segsFor, java.util.Map<hydra.packaging.Namespace, Integer> state, T1 _ign) {
    hydra.util.Lazy<java.util.List<hydra.util.Pair<hydra.packaging.Namespace, hydra.util.Pair<Integer, hydra.util.Pair<Integer, String>>>>> aliasEntries = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.packaging.Namespace, hydra.util.Pair<hydra.packaging.Namespace, hydra.util.Pair<Integer, hydra.util.Pair<Integer, String>>>>) (nm -> {
        hydra.util.Lazy<Integer> n = new hydra.util.Lazy<>(() -> hydra.haskell.Utils.namespacesForModule_takenFor(
          state,
          nm));
        java.util.List<String> segs = (segsFor).apply(nm);
        String aliasStr = (aliasFromSuffix).apply(segs).apply(n.get()).value;
        hydra.util.Lazy<Integer> segCount = new hydra.util.Lazy<>(() -> hydra.lib.lists.Length.apply(segs));
        return (hydra.util.Pair<hydra.packaging.Namespace, hydra.util.Pair<Integer, hydra.util.Pair<Integer, String>>>) ((hydra.util.Pair<hydra.packaging.Namespace, hydra.util.Pair<Integer, hydra.util.Pair<Integer, String>>>) (new hydra.util.Pair<hydra.packaging.Namespace, hydra.util.Pair<Integer, hydra.util.Pair<Integer, String>>>(nm, (hydra.util.Pair<Integer, hydra.util.Pair<Integer, String>>) ((hydra.util.Pair<Integer, hydra.util.Pair<Integer, String>>) (new hydra.util.Pair<Integer, hydra.util.Pair<Integer, String>>(n.get(), (hydra.util.Pair<Integer, String>) ((hydra.util.Pair<Integer, String>) (new hydra.util.Pair<Integer, String>(segCount.get(), aliasStr)))))))));
      }),
      nssAsList));
    hydra.util.Lazy<java.util.Map<String, Integer>> aliasCounts = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<java.util.Map<String, Integer>, java.util.function.Function<hydra.util.Pair<hydra.packaging.Namespace, hydra.util.Pair<Integer, hydra.util.Pair<Integer, String>>>, java.util.Map<String, Integer>>>) (m -> (java.util.function.Function<hydra.util.Pair<hydra.packaging.Namespace, hydra.util.Pair<Integer, hydra.util.Pair<Integer, String>>>, java.util.Map<String, Integer>>) (e -> {
        hydra.util.Lazy<String> k = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(e))));
        return hydra.lib.maps.Insert.apply(
          k.get(),
          hydra.lib.math.Add.apply(
            1,
            hydra.lib.maybes.FromMaybe.applyLazy(
              () -> 0,
              hydra.lib.maps.Lookup.apply(
                k.get(),
                m))),
          m);
      })),
      (java.util.Map<String, Integer>) ((java.util.Map<String, Integer>) (hydra.lib.maps.Empty.<String, Integer>apply())),
      aliasEntries.get()));
    hydra.util.Lazy<java.util.Map<String, Integer>> aliasMinSegs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<java.util.Map<String, Integer>, java.util.function.Function<hydra.util.Pair<hydra.packaging.Namespace, hydra.util.Pair<Integer, hydra.util.Pair<Integer, String>>>, java.util.Map<String, Integer>>>) (m -> (java.util.function.Function<hydra.util.Pair<hydra.packaging.Namespace, hydra.util.Pair<Integer, hydra.util.Pair<Integer, String>>>, java.util.Map<String, Integer>>) (e -> {
        hydra.util.Lazy<String> k = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(e))));
        hydra.util.Lazy<hydra.util.Maybe<Integer>> existing = new hydra.util.Lazy<>(() -> hydra.lib.maps.Lookup.apply(
          k.get(),
          m));
        hydra.util.Lazy<Integer> segCount = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(e))));
        return hydra.lib.maps.Insert.apply(
          k.get(),
          hydra.lib.maybes.Cases.applyLazy(
            existing.get(),
            () -> segCount.get(),
            (java.util.function.Function<Integer, Integer>) (prev -> hydra.lib.logic.IfElse.lazy(
              hydra.lib.equality.Lt.apply(
                segCount.get(),
                prev),
              () -> segCount.get(),
              () -> prev))),
          m);
      })),
      (java.util.Map<String, Integer>) ((java.util.Map<String, Integer>) (hydra.lib.maps.Empty.<String, Integer>apply())),
      aliasEntries.get()));
    hydra.util.Lazy<java.util.Map<String, Integer>> aliasMinSegsCount = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<java.util.Map<String, Integer>, java.util.function.Function<hydra.util.Pair<hydra.packaging.Namespace, hydra.util.Pair<Integer, hydra.util.Pair<Integer, String>>>, java.util.Map<String, Integer>>>) (m -> (java.util.function.Function<hydra.util.Pair<hydra.packaging.Namespace, hydra.util.Pair<Integer, hydra.util.Pair<Integer, String>>>, java.util.Map<String, Integer>>) (e -> {
        hydra.util.Lazy<String> k = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(e))));
        hydra.util.Lazy<Integer> segCount = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(e))));
        hydra.util.Lazy<Integer> minSegs = new hydra.util.Lazy<>(() -> hydra.lib.maybes.FromMaybe.applyLazy(
          () -> segCount.get(),
          hydra.lib.maps.Lookup.apply(
            k.get(),
            aliasMinSegs.get())));
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Equal.apply(
            segCount.get(),
            minSegs.get()),
          () -> hydra.lib.maps.Insert.apply(
            k.get(),
            hydra.lib.math.Add.apply(
              1,
              hydra.lib.maybes.FromMaybe.applyLazy(
                () -> 0,
                hydra.lib.maps.Lookup.apply(
                  k.get(),
                  m))),
            m),
          () -> m);
      })),
      (java.util.Map<String, Integer>) ((java.util.Map<String, Integer>) (hydra.lib.maps.Empty.<String, Integer>apply())),
      aliasEntries.get()));
    return hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.util.Pair<hydra.packaging.Namespace, hydra.util.Pair<Integer, hydra.util.Pair<Integer, String>>>, hydra.util.Pair<hydra.packaging.Namespace, Integer>>) (e -> {
        hydra.util.Lazy<String> aliasStr = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(e))));
        hydra.util.Lazy<Integer> count = new hydra.util.Lazy<>(() -> hydra.lib.maybes.FromMaybe.applyLazy(
          () -> 0,
          hydra.lib.maps.Lookup.apply(
            aliasStr.get(),
            aliasCounts.get())));
        hydra.util.Lazy<Integer> segCount = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(e))));
        hydra.util.Lazy<Integer> minSegs = new hydra.util.Lazy<>(() -> hydra.lib.maybes.FromMaybe.applyLazy(
          () -> segCount.get(),
          hydra.lib.maps.Lookup.apply(
            aliasStr.get(),
            aliasMinSegs.get())));
        hydra.util.Lazy<Integer> minSegsCount = new hydra.util.Lazy<>(() -> hydra.lib.maybes.FromMaybe.applyLazy(
          () -> 0,
          hydra.lib.maps.Lookup.apply(
            aliasStr.get(),
            aliasMinSegsCount.get())));
        hydra.util.Lazy<Integer> n = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(hydra.lib.pairs.Second.apply(e)));
        hydra.util.Lazy<Boolean> canGrow = new hydra.util.Lazy<>(() -> hydra.lib.logic.And.apply(
          hydra.lib.equality.Gt.apply(
            count.get(),
            1),
          hydra.lib.logic.And.apply(
            hydra.lib.equality.Gt.apply(
              segCount.get(),
              n.get()),
            hydra.lib.logic.Or.apply(
              hydra.lib.equality.Gt.apply(
                segCount.get(),
                minSegs.get()),
              hydra.lib.equality.Gt.apply(
                minSegsCount.get(),
                1)))));
        hydra.util.Lazy<Integer> newN = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
          canGrow.get(),
          () -> hydra.lib.math.Add.apply(
            n.get(),
            1),
          () -> n.get()));
        hydra.util.Lazy<hydra.packaging.Namespace> nm = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(e));
        return (hydra.util.Pair<hydra.packaging.Namespace, Integer>) ((hydra.util.Pair<hydra.packaging.Namespace, Integer>) (new hydra.util.Pair<hydra.packaging.Namespace, Integer>(nm.get(), newN.get())));
      }),
      aliasEntries.get()));
  }

  static <T1> Integer namespacesForModule_takenFor(java.util.Map<T1, Integer> state, T1 nm) {
    return hydra.lib.maybes.FromMaybe.applyLazy(
      () -> 1,
      hydra.lib.maps.Lookup.apply(
        nm,
        state));
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
    hydra.util.Lazy<hydra.haskell.syntax.Type> dummyType = new hydra.util.Lazy<>(() -> new hydra.haskell.syntax.Type.Variable(new hydra.haskell.syntax.Name.Normal(new hydra.haskell.syntax.QualifiedName((java.util.List<hydra.haskell.syntax.NamePart>) (java.util.Collections.<hydra.haskell.syntax.NamePart>emptyList()), new hydra.haskell.syntax.NamePart("")))));
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<java.util.List<hydra.haskell.syntax.Type>, hydra.haskell.syntax.Type>> app = new java.util.concurrent.atomic.AtomicReference<>();
    app.set((java.util.function.Function<java.util.List<hydra.haskell.syntax.Type>, hydra.haskell.syntax.Type>) (l -> hydra.lib.maybes.FromMaybe.applyLazy(
      () -> dummyType.get(),
      hydra.lib.maybes.Map.apply(
        (java.util.function.Function<hydra.util.Pair<hydra.haskell.syntax.Type, java.util.List<hydra.haskell.syntax.Type>>, hydra.haskell.syntax.Type>) (p -> hydra.lib.logic.IfElse.lazy(
          hydra.lib.lists.Null.apply(hydra.lib.pairs.Second.apply(p)),
          () -> hydra.lib.pairs.First.apply(p),
          () -> new hydra.haskell.syntax.Type.Application(new hydra.haskell.syntax.ApplicationType(app.get().apply(hydra.lib.pairs.Second.apply(p)), hydra.lib.pairs.First.apply(p))))),
        hydra.lib.lists.Uncons.apply(l)))));
    return app.get().apply(hydra.lib.lists.Reverse.apply(types));
  }

  static String typeNameForRecord(hydra.core.Name sname) {
    String snameStr = (sname).value;
    java.util.List<String> parts = hydra.lib.strings.SplitOn.apply(
      ".",
      snameStr);
    return hydra.lib.maybes.FromMaybe.applyLazy(
      () -> snameStr,
      hydra.lib.lists.MaybeLast.apply(parts));
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
