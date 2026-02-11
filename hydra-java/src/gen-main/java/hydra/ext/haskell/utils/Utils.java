// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.utils;

/**
 * Utilities for working with Haskell syntax trees
 */
public interface Utils {
  static hydra.ext.haskell.ast.Pattern applicationPattern(hydra.ext.haskell.ast.Name name, java.util.List<hydra.ext.haskell.ast.Pattern> args) {
    return new hydra.ext.haskell.ast.Pattern.Application(new hydra.ext.haskell.ast.ApplicationPattern(name, args));
  }
  
  static hydra.ext.haskell.ast.Name elementReference(hydra.module.Namespaces<hydra.ext.haskell.ast.ModuleName> namespaces, hydra.core.Name name) {
    hydra.module.QualifiedName qname = hydra.names.Names.qualifyName(name);
    String local = (qname).local;
    String escLocal = hydra.ext.haskell.utils.Utils.sanitizeHaskellName(local);
    hydra.util.Lazy<hydra.util.Tuple.Tuple2<hydra.module.Namespace, hydra.ext.haskell.ast.ModuleName>> namespacePair = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.module.Namespaces<hydra.ext.haskell.ast.ModuleName>, hydra.util.Tuple.Tuple2<hydra.module.Namespace, hydra.ext.haskell.ast.ModuleName>>) (projected -> projected.focus)).apply(namespaces));
    hydra.util.Lazy<String> gmod = new hydra.util.Lazy<>(() -> (hydra.lib.pairs.Second.apply(namespacePair.get())).value);
    hydra.util.Lazy<hydra.module.Namespace> gname = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(namespacePair.get()));
    hydra.util.Maybe<hydra.module.Namespace> mns = (qname).namespace;
    hydra.util.Lazy<java.util.Map<hydra.module.Namespace, hydra.ext.haskell.ast.ModuleName>> namespacesMap = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.module.Namespaces<hydra.ext.haskell.ast.ModuleName>, java.util.Map<hydra.module.Namespace, hydra.ext.haskell.ast.ModuleName>>) (projected -> projected.mapping)).apply(namespaces));
    return hydra.lib.maybes.Cases.apply(
      (qname).namespace,
      hydra.ext.haskell.utils.Utils.simpleName(local),
      (java.util.function.Function<hydra.module.Namespace, hydra.ext.haskell.ast.Name>) (ns -> hydra.lib.maybes.Cases.apply(
        hydra.lib.maps.Lookup.apply(
          ns,
          namespacesMap.get()),
        hydra.ext.haskell.utils.Utils.simpleName(local),
        (java.util.function.Function<hydra.ext.haskell.ast.ModuleName, hydra.ext.haskell.ast.Name>) (mn -> {
          String aliasStr = (mn).value;
          return hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              ns,
              gname.get()),
            () -> hydra.ext.haskell.utils.Utils.simpleName(escLocal),
            () -> hydra.ext.haskell.utils.Utils.rawName(hydra.lib.strings.Cat.apply(java.util.List.of(
              aliasStr,
              ".",
              hydra.ext.haskell.utils.Utils.sanitizeHaskellName(local)))));
        }))));
  }
  
  static hydra.ext.haskell.ast.Expression hsapp(hydra.ext.haskell.ast.Expression l, hydra.ext.haskell.ast.Expression r) {
    return new hydra.ext.haskell.ast.Expression.Application(new hydra.ext.haskell.ast.ApplicationExpression(l, r));
  }
  
  static hydra.ext.haskell.ast.Expression hslambda(hydra.ext.haskell.ast.Name name, hydra.ext.haskell.ast.Expression rhs) {
    return new hydra.ext.haskell.ast.Expression.Lambda(new hydra.ext.haskell.ast.LambdaExpression(java.util.List.of(new hydra.ext.haskell.ast.Pattern.Name(name)), rhs));
  }
  
  static hydra.ext.haskell.ast.Expression hslit(hydra.ext.haskell.ast.Literal lit) {
    return new hydra.ext.haskell.ast.Expression.Literal(lit);
  }
  
  static hydra.ext.haskell.ast.Expression hsvar(String s) {
    return new hydra.ext.haskell.ast.Expression.Variable(hydra.ext.haskell.utils.Utils.rawName(s));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.module.Namespaces<hydra.ext.haskell.ast.ModuleName>> namespacesForModule(hydra.module.Module mod) {
    return hydra.lib.flows.Bind.apply(
      hydra.schemas.Schemas.moduleDependencyNamespaces(
        true,
        true,
        true,
        true,
        mod),
      (java.util.function.Function<java.util.Set<hydra.module.Namespace>, hydra.compute.Flow<hydra.graph.Graph, hydra.module.Namespaces<hydra.ext.haskell.ast.ModuleName>>>) (nss -> {
        hydra.util.Lazy<java.util.List<hydra.module.Namespace>> nssAsList = new hydra.util.Lazy<>(() -> hydra.lib.sets.ToList.apply(nss));
        java.util.function.Function<hydra.module.Namespace, hydra.ext.haskell.ast.ModuleName> toModuleName = (java.util.function.Function<hydra.module.Namespace, hydra.ext.haskell.ast.ModuleName>) (namespace -> {
          String namespaceStr = (namespace).value;
          java.util.List<String> parts = hydra.lib.strings.SplitOn.apply(
            ".",
            namespaceStr);
          hydra.util.Lazy<String> lastPart = new hydra.util.Lazy<>(() -> hydra.lib.lists.Last.apply(parts));
          String capitalized = hydra.formatting.Formatting.capitalize(lastPart.get());
          return new hydra.ext.haskell.ast.ModuleName(capitalized);
        });
        java.util.function.Function<hydra.module.Namespace, hydra.util.Tuple.Tuple2<hydra.module.Namespace, hydra.ext.haskell.ast.ModuleName>> toPair = (java.util.function.Function<hydra.module.Namespace, hydra.util.Tuple.Tuple2<hydra.module.Namespace, hydra.ext.haskell.ast.ModuleName>>) (name -> (hydra.util.Tuple.Tuple2<hydra.module.Namespace, hydra.ext.haskell.ast.ModuleName>) ((hydra.util.Tuple.Tuple2<hydra.module.Namespace, hydra.ext.haskell.ast.ModuleName>) (new hydra.util.Tuple.Tuple2<hydra.module.Namespace, hydra.ext.haskell.ast.ModuleName>(name, (toModuleName).apply(name)))));
        hydra.util.Lazy<java.util.List<hydra.util.Tuple.Tuple2<hydra.module.Namespace, hydra.ext.haskell.ast.ModuleName>>> nssPairs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
          toPair,
          nssAsList.get()));
        hydra.util.Lazy<hydra.util.Tuple.Tuple2<java.util.Map<hydra.module.Namespace, hydra.ext.haskell.ast.ModuleName>, java.util.Set<hydra.ext.haskell.ast.ModuleName>>> finalState = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
          p0 -> p1 -> hydra.ext.haskell.utils.Utils.<hydra.module.Namespace>namespacesForModule_addPair(
            p0,
            p1),
          hydra.ext.haskell.utils.Utils.<hydra.module.Namespace, hydra.ext.haskell.ast.ModuleName, hydra.ext.haskell.ast.ModuleName>namespacesForModule_emptyState(),
          nssPairs.get()));
        hydra.module.Namespace ns = (mod).namespace;
        hydra.util.Tuple.Tuple2<hydra.module.Namespace, hydra.ext.haskell.ast.ModuleName> focusPair = (toPair).apply(ns);
        hydra.util.Lazy<java.util.Map<hydra.module.Namespace, hydra.ext.haskell.ast.ModuleName>> resultMap = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(finalState.get()));
        return hydra.lib.flows.Pure.apply((hydra.module.Namespaces<hydra.ext.haskell.ast.ModuleName>) (new hydra.module.Namespaces<hydra.ext.haskell.ast.ModuleName>(focusPair, resultMap.get())));
      }));
  }
  
  static <T0, T1, T2> hydra.util.Tuple.Tuple2<java.util.Map<T0, T1>, java.util.Set<T2>> namespacesForModule_emptyState() {
    return (hydra.util.Tuple.Tuple2<java.util.Map<T0, T1>, java.util.Set<T2>>) ((hydra.util.Tuple.Tuple2<java.util.Map<T0, T1>, java.util.Set<T2>>) (new hydra.util.Tuple.Tuple2<java.util.Map<T0, T1>, java.util.Set<T2>>((java.util.Map<T0, T1>) ((java.util.Map<T0, T1>) (hydra.lib.maps.Empty.<T0, T1>apply())), (java.util.Set<T2>) (hydra.lib.sets.Empty.<T2>apply()))));
  }
  
  static <T0> hydra.util.Tuple.Tuple2<java.util.Map<T0, hydra.ext.haskell.ast.ModuleName>, java.util.Set<hydra.ext.haskell.ast.ModuleName>> namespacesForModule_addPair(hydra.util.Tuple.Tuple2<java.util.Map<T0, hydra.ext.haskell.ast.ModuleName>, java.util.Set<hydra.ext.haskell.ast.ModuleName>> state, hydra.util.Tuple.Tuple2<T0, hydra.ext.haskell.ast.ModuleName> namePair) {
    hydra.util.Lazy<hydra.ext.haskell.ast.ModuleName> alias = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(namePair));
    String aliasStr = (alias.get()).value;
    hydra.util.Lazy<java.util.Set<hydra.ext.haskell.ast.ModuleName>> currentSet = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(state));
    hydra.util.Lazy<T0> name = new hydra.util.Lazy<>(() -> hydra.ext.haskell.utils.Utils.namespacesForModule_name(namePair));
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.sets.Member.apply(
        alias.get(),
        currentSet.get()),
      () -> hydra.ext.haskell.utils.Utils.<T0>namespacesForModule_addPair(
        state,
        (hydra.util.Tuple.Tuple2<T0, hydra.ext.haskell.ast.ModuleName>) ((hydra.util.Tuple.Tuple2<T0, hydra.ext.haskell.ast.ModuleName>) (new hydra.util.Tuple.Tuple2<T0, hydra.ext.haskell.ast.ModuleName>(name.get(), new hydra.ext.haskell.ast.ModuleName(hydra.lib.strings.Cat2.apply(
          aliasStr,
          "_")))))),
      () -> (hydra.util.Tuple.Tuple2<java.util.Map<T0, hydra.ext.haskell.ast.ModuleName>, java.util.Set<hydra.ext.haskell.ast.ModuleName>>) ((hydra.util.Tuple.Tuple2<java.util.Map<T0, hydra.ext.haskell.ast.ModuleName>, java.util.Set<hydra.ext.haskell.ast.ModuleName>>) (new hydra.util.Tuple.Tuple2<java.util.Map<T0, hydra.ext.haskell.ast.ModuleName>, java.util.Set<hydra.ext.haskell.ast.ModuleName>>(hydra.lib.maps.Insert.apply(
        name.get(),
        alias.get(),
        hydra.ext.haskell.utils.Utils.namespacesForModule_currentMap(state)), hydra.lib.sets.Insert.apply(
        alias.get(),
        currentSet.get())))));
  }
  
  static <T0, T1> T0 namespacesForModule_currentMap(hydra.util.Tuple.Tuple2<T0, T1> state) {
    return hydra.lib.pairs.First.apply(state);
  }
  
  static <T0, T1> T0 namespacesForModule_name(hydra.util.Tuple.Tuple2<T0, T1> namePair) {
    return hydra.lib.pairs.First.apply(namePair);
  }
  
  static String newtypeAccessorName(hydra.core.Name name) {
    return hydra.lib.strings.Cat2.apply(
      "un",
      hydra.names.Names.localNameOf(name));
  }
  
  static hydra.ext.haskell.ast.Name rawName(String n) {
    return new hydra.ext.haskell.ast.Name.Normal(new hydra.ext.haskell.ast.QualifiedName((java.util.List<hydra.ext.haskell.ast.NamePart>) (java.util.List.<hydra.ext.haskell.ast.NamePart>of()), new hydra.ext.haskell.ast.NamePart(n)));
  }
  
  static hydra.ext.haskell.ast.Name recordFieldReference(hydra.module.Namespaces<hydra.ext.haskell.ast.ModuleName> namespaces, hydra.core.Name sname, hydra.core.Name fname) {
    String fnameStr = (fname).value;
    String capitalized = hydra.formatting.Formatting.capitalize(fnameStr);
    String typeNameStr = hydra.ext.haskell.utils.Utils.typeNameForRecord(sname);
    String decapitalized = hydra.formatting.Formatting.decapitalize(typeNameStr);
    String nm = hydra.lib.strings.Cat2.apply(
      decapitalized,
      capitalized);
    hydra.module.QualifiedName qname = hydra.names.Names.qualifyName(sname);
    hydra.util.Maybe<hydra.module.Namespace> ns = (qname).namespace;
    hydra.module.QualifiedName qualName = new hydra.module.QualifiedName(ns, nm);
    hydra.core.Name unqualName = hydra.names.Names.unqualifyName(qualName);
    return hydra.ext.haskell.utils.Utils.elementReference(
      namespaces,
      unqualName);
  }
  
  static String sanitizeHaskellName(String v1) {
    return hydra.formatting.Formatting.sanitizeWithUnderscores(
      hydra.ext.haskell.language.Language.reservedWords(),
      v1);
  }
  
  static hydra.ext.haskell.ast.Name simpleName(String arg_) {
    return hydra.ext.haskell.utils.Utils.rawName(hydra.ext.haskell.utils.Utils.sanitizeHaskellName(arg_));
  }
  
  static hydra.ext.haskell.ast.ValueBinding simpleValueBinding(hydra.ext.haskell.ast.Name hname, hydra.ext.haskell.ast.Expression rhs, hydra.util.Maybe<hydra.ext.haskell.ast.LocalBindings> bindings) {
    hydra.util.Lazy<hydra.ext.haskell.ast.Pattern> pat = new hydra.util.Lazy<>(() -> new hydra.ext.haskell.ast.Pattern.Application(new hydra.ext.haskell.ast.ApplicationPattern(hname, (java.util.List<hydra.ext.haskell.ast.Pattern>) (java.util.List.<hydra.ext.haskell.ast.Pattern>of()))));
    hydra.ext.haskell.ast.RightHandSide rightHandSide = new hydra.ext.haskell.ast.RightHandSide(rhs);
    return new hydra.ext.haskell.ast.ValueBinding.Simple(new hydra.ext.haskell.ast.SimpleValueBinding(pat.get(), rightHandSide, bindings));
  }
  
  static hydra.ext.haskell.ast.Type toTypeApplication(java.util.List<hydra.ext.haskell.ast.Type> types) {
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<java.util.List<hydra.ext.haskell.ast.Type>, hydra.ext.haskell.ast.Type>> app = new java.util.concurrent.atomic.AtomicReference<>();
    app.set((java.util.function.Function<java.util.List<hydra.ext.haskell.ast.Type>, hydra.ext.haskell.ast.Type>) (l -> hydra.lib.logic.IfElse.lazy(
      hydra.lib.equality.Gt.apply(
        hydra.lib.lists.Length.apply(l),
        1),
      () -> new hydra.ext.haskell.ast.Type.Application(new hydra.ext.haskell.ast.ApplicationType((app.get()).apply(hydra.lib.lists.Tail.apply(l)), hydra.lib.lists.Head.apply(l))),
      () -> hydra.lib.lists.Head.apply(l))));
    return (app.get()).apply(hydra.lib.lists.Reverse.apply(types));
  }
  
  static String typeNameForRecord(hydra.core.Name sname) {
    String snameStr = (sname).value;
    java.util.List<String> parts = hydra.lib.strings.SplitOn.apply(
      ".",
      snameStr);
    return hydra.lib.lists.Last.apply(parts);
  }
  
  static hydra.ext.haskell.ast.Name unionFieldReference(hydra.graph.Graph g, hydra.module.Namespaces<hydra.ext.haskell.ast.ModuleName> namespaces, hydra.core.Name sname, hydra.core.Name fname) {
    String fnameStr = (fname).value;
    String capitalizedFieldName = hydra.formatting.Formatting.capitalize(fnameStr);
    String typeNameStr = hydra.ext.haskell.utils.Utils.typeNameForRecord(sname);
    String capitalizedTypeName = hydra.formatting.Formatting.capitalize(typeNameStr);
    hydra.module.QualifiedName qname = hydra.names.Names.qualifyName(sname);
    hydra.util.Maybe<hydra.module.Namespace> ns = (qname).namespace;
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<String, String>> deconflict = new java.util.concurrent.atomic.AtomicReference<>();
    deconflict.set((java.util.function.Function<String, String>) (name -> {
      hydra.core.Name tname = hydra.names.Names.unqualifyName(new hydra.module.QualifiedName(ns, name));
      return hydra.lib.logic.IfElse.lazy(
        hydra.lib.maybes.IsJust.apply(hydra.lib.lists.Find.apply(
          (java.util.function.Function<hydra.core.Binding, Boolean>) (b -> hydra.lib.equality.Equal.apply(
            (b).name,
            tname)),
          (g).elements)),
        () -> (deconflict.get()).apply(hydra.lib.strings.Cat2.apply(
          name,
          "_")),
        () -> name);
    }));
    String nm = (deconflict.get()).apply(hydra.lib.strings.Cat2.apply(
      capitalizedTypeName,
      capitalizedFieldName));
    hydra.module.QualifiedName qualName = new hydra.module.QualifiedName(ns, nm);
    hydra.core.Name unqualName = hydra.names.Names.unqualifyName(qualName);
    return hydra.ext.haskell.utils.Utils.elementReference(
      namespaces,
      unqualName);
  }
  
  static <T0> hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Name>, hydra.core.Type> unpackForallType(T0 cx, hydra.core.Type t) {
    return (hydra.rewriting.Rewriting.deannotateType(t)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Name>, hydra.core.Type> otherwise(hydra.core.Type instance) {
        return (hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Name>, hydra.core.Type>) ((hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Name>, hydra.core.Type>) (new hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Name>, hydra.core.Type>((java.util.List<hydra.core.Name>) (java.util.List.<hydra.core.Name>of()), t)));
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Name>, hydra.core.Type> visit(hydra.core.Type.Forall fat) {
        hydra.core.Type tbody = ((fat).value).body;
        hydra.util.Lazy<hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Name>, hydra.core.Type>> recursiveResult = new hydra.util.Lazy<>(() -> hydra.ext.haskell.utils.Utils.<T0>unpackForallType(
          cx,
          tbody));
        hydra.util.Lazy<hydra.core.Type> finalType = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(recursiveResult.get()));
        hydra.core.Name v = ((fat).value).parameter;
        hydra.util.Lazy<java.util.List<hydra.core.Name>> vars = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(recursiveResult.get()));
        return (hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Name>, hydra.core.Type>) ((hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Name>, hydra.core.Type>) (new hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Name>, hydra.core.Type>(hydra.lib.lists.Cons.apply(
          v,
          vars.get()), finalType.get())));
      }
    });
  }
}
