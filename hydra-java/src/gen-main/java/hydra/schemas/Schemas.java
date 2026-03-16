// Note: this is an automatically generated file. Do not edit.

package hydra.schemas;

/**
 * Various functions for dereferencing and decoding schema types.
 */
public interface Schemas {
  static <T0> hydra.module.Namespaces<T0> addNamesToNamespaces(java.util.function.Function<hydra.module.Namespace, T0> encodeNamespace, hydra.util.PersistentSet<hydra.core.Name> names, hydra.module.Namespaces<T0> ns0) {
    hydra.util.Lazy<hydra.util.PersistentSet<hydra.module.Namespace>> nss = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
      hydra.names.Names::namespaceOf,
      hydra.lib.sets.ToList.apply(names)))));
    return (hydra.module.Namespaces<T0>) (new hydra.module.Namespaces<T0>(((java.util.function.Function<hydra.module.Namespaces<T0>, hydra.util.Pair<hydra.module.Namespace, T0>>) (projected -> projected.focus)).apply(ns0), hydra.lib.maps.Union.apply(
      ((java.util.function.Function<hydra.module.Namespaces<T0>, hydra.util.PersistentMap<hydra.module.Namespace, T0>>) (projected -> projected.mapping)).apply(ns0),
      hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.module.Namespace, hydra.util.Pair<hydra.module.Namespace, T0>>) (v1 -> hydra.schemas.Schemas.<T0>addNamesToNamespaces_toPair(
          encodeNamespace,
          v1)),
        hydra.lib.sets.ToList.apply(nss.get()))))));
  }
  
  static <T0> hydra.util.Pair<hydra.module.Namespace, T0> addNamesToNamespaces_toPair(java.util.function.Function<hydra.module.Namespace, T0> encodeNamespace, hydra.module.Namespace ns) {
    return (hydra.util.Pair<hydra.module.Namespace, T0>) ((hydra.util.Pair<hydra.module.Namespace, T0>) (new hydra.util.Pair<hydra.module.Namespace, T0>(ns, (encodeNamespace).apply(ns))));
  }
  
  static hydra.util.PersistentSet<hydra.module.Namespace> definitionDependencyNamespaces(hydra.util.ConsList<hydra.module.Definition> defs) {
    java.util.function.Function<hydra.module.Definition, hydra.util.PersistentSet<hydra.core.Name>> defNames = (java.util.function.Function<hydra.module.Definition, hydra.util.PersistentSet<hydra.core.Name>>) (def -> (def).accept(new hydra.module.Definition.PartialVisitor<>() {
      @Override
      public hydra.util.PersistentSet<hydra.core.Name> visit(hydra.module.Definition.Type typeDef) {
        return hydra.rewriting.Rewriting.typeDependencyNames(
          true,
          ((typeDef).value).type);
      }
      
      @Override
      public hydra.util.PersistentSet<hydra.core.Name> visit(hydra.module.Definition.Term termDef) {
        return hydra.rewriting.Rewriting.termDependencyNames(
          true,
          true,
          true,
          ((termDef).value).term);
      }
    }));
    hydra.util.Lazy<hydra.util.PersistentSet<hydra.core.Name>> allNames = new hydra.util.Lazy<>(() -> hydra.lib.sets.Unions.apply(hydra.lib.lists.Map.apply(
      defNames,
      defs)));
    return hydra.lib.sets.FromList.apply(hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
      hydra.names.Names::namespaceOf,
      hydra.lib.sets.ToList.apply(allNames.get()))));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.PersistentSet<hydra.module.Namespace>> dependencyNamespaces(hydra.context.Context cx, hydra.graph.Graph graph, Boolean binds, Boolean withPrims, Boolean withNoms, Boolean withSchema, hydra.util.ConsList<hydra.core.Binding> els) {
    java.util.function.Function<hydra.core.Binding, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.PersistentSet<hydra.core.Name>>> depNames = (java.util.function.Function<hydra.core.Binding, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.PersistentSet<hydra.core.Name>>>) (el -> {
      hydra.core.Term term = (el).term;
      hydra.util.PersistentSet<hydra.core.Name> dataNames = hydra.rewriting.Rewriting.termDependencyNames(
        binds,
        withPrims,
        withNoms,
        term);
      hydra.core.Term deannotatedTerm = hydra.rewriting.Rewriting.deannotateTerm(term);
      hydra.util.Lazy<hydra.util.PersistentSet<hydra.core.Name>> schemaNames = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
        withSchema,
        () -> hydra.lib.maybes.Maybe.applyLazy(
          () -> (hydra.util.PersistentSet<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
          (java.util.function.Function<hydra.core.TypeScheme, hydra.util.PersistentSet<hydra.core.Name>>) (ts -> hydra.rewriting.Rewriting.typeDependencyNames(
            true,
            (ts).type)),
          (el).type),
        () -> (hydra.util.PersistentSet<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply())));
      return hydra.lib.logic.IfElse.lazy(
        hydra.schemas.Schemas.isEncodedType(deannotatedTerm),
        () -> hydra.lib.eithers.Map.apply(
          (java.util.function.Function<hydra.core.Type, hydra.util.PersistentSet<hydra.core.Name>>) (typ -> hydra.lib.sets.Unions.apply(hydra.util.ConsList.of(
            dataNames,
            schemaNames.get(),
            hydra.rewriting.Rewriting.typeDependencyNames(
              true,
              typ)))),
          hydra.lib.eithers.Bimap.apply(
            (java.util.function.Function<hydra.error.Error_, hydra.context.InContext<hydra.error.Error_>>) (_wc_e -> (hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(_wc_e, new hydra.context.Context(hydra.lib.lists.Cons.apply(
              "dependency namespace (type)",
              (cx).trace), (cx).messages, (cx).other)))),
            (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (_wc_a -> _wc_a),
            hydra.lib.eithers.Bimap.apply(
              (java.util.function.Function<hydra.error.DecodingError, hydra.error.Error_>) (_e -> new hydra.error.Error_.Other(new hydra.error.OtherError((_e).value))),
              (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (_a -> _a),
              hydra.decode.core.Core.type(
                graph,
                term)))),
        () -> hydra.lib.logic.IfElse.lazy(
          hydra.schemas.Schemas.isEncodedTerm(deannotatedTerm),
          () -> hydra.lib.eithers.Map.apply(
            (java.util.function.Function<hydra.core.Term, hydra.util.PersistentSet<hydra.core.Name>>) (decodedTerm -> hydra.lib.sets.Unions.apply(hydra.util.ConsList.of(
              dataNames,
              schemaNames.get(),
              hydra.rewriting.Rewriting.termDependencyNames(
                binds,
                withPrims,
                withNoms,
                decodedTerm)))),
            hydra.lib.eithers.Bimap.apply(
              (java.util.function.Function<hydra.error.Error_, hydra.context.InContext<hydra.error.Error_>>) (_wc_e -> (hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(_wc_e, new hydra.context.Context(hydra.lib.lists.Cons.apply(
                "dependency namespace (term)",
                (cx).trace), (cx).messages, (cx).other)))),
              (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (_wc_a -> _wc_a),
              hydra.lib.eithers.Bimap.apply(
                (java.util.function.Function<hydra.error.DecodingError, hydra.error.Error_>) (_e -> new hydra.error.Error_.Other(new hydra.error.OtherError((_e).value))),
                (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (_a -> _a),
                hydra.decode.core.Core.term(
                  graph,
                  term)))),
          () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.PersistentSet<hydra.core.Name>>right(hydra.lib.sets.Unions.apply(hydra.util.ConsList.of(
            dataNames,
            schemaNames.get())))));
    });
    return hydra.lib.eithers.Map.apply(
      (java.util.function.Function<hydra.util.ConsList<hydra.util.PersistentSet<hydra.core.Name>>, hydra.util.PersistentSet<hydra.module.Namespace>>) (namesList -> hydra.lib.sets.FromList.apply(hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
        hydra.names.Names::namespaceOf,
        hydra.lib.sets.ToList.apply(hydra.lib.sets.Unions.apply(namesList)))))),
      hydra.lib.eithers.MapList.apply(
        depNames,
        els));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.core.Type>> dereferenceType(hydra.context.Context cx, hydra.graph.Graph graph, hydra.core.Name name) {
    hydra.util.Maybe<hydra.core.Binding> mel = hydra.lexical.Lexical.dereferenceElement(
      graph,
      name);
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.core.Type>>right((hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing())),
      (java.util.function.Function<hydra.core.Binding, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.core.Type>>>) (el -> hydra.lib.eithers.Map.apply(
        (java.util.function.Function<hydra.core.Type, hydra.util.Maybe<hydra.core.Type>>) (hydra.lib.maybes.Pure::apply),
        hydra.lib.eithers.Bimap.apply(
          (java.util.function.Function<hydra.error.Error_, hydra.context.InContext<hydra.error.Error_>>) (_wc_e -> (hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(_wc_e, cx))),
          (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (_wc_a -> _wc_a),
          hydra.lib.eithers.Bimap.apply(
            (java.util.function.Function<hydra.error.DecodingError, hydra.error.Error_>) (_e -> new hydra.error.Error_.Other(new hydra.error.OtherError((_e).value))),
            (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (_a -> _a),
            hydra.decode.core.Core.type(
              graph,
              (el).term))))),
      mel);
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.TypeApplicationTerm> elementAsTypeApplicationTerm(hydra.context.Context cx, hydra.core.Binding el) {
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.TypeApplicationTerm>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError("missing element type")), cx))),
      (java.util.function.Function<hydra.core.TypeScheme, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.TypeApplicationTerm>>) (ts -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.TypeApplicationTerm>right(new hydra.core.TypeApplicationTerm((el).term, (ts).type))),
      (el).type);
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.ConsList<hydra.core.Binding>> elementsWithDependencies(hydra.context.Context cx, hydra.graph.Graph graph, hydra.util.ConsList<hydra.core.Binding> original) {
    java.util.function.Function<hydra.core.Binding, hydra.util.ConsList<hydra.core.Name>> depNames = (java.util.function.Function<hydra.core.Binding, hydra.util.ConsList<hydra.core.Name>>) (el -> hydra.lib.sets.ToList.apply(hydra.rewriting.Rewriting.termDependencyNames(
      true,
      false,
      false,
      (el).term)));
    hydra.util.Lazy<hydra.util.ConsList<hydra.core.Name>> allDepNames = new hydra.util.Lazy<>(() -> hydra.lib.lists.Nub.apply(hydra.lib.lists.Concat2.apply(
      hydra.lib.lists.Map.apply(
        projected -> projected.name,
        original),
      hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
        depNames,
        original)))));
    return hydra.lib.eithers.MapList.apply(
      (java.util.function.Function<hydra.core.Name, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Binding>>) (name -> hydra.lexical.Lexical.requireElement(
        cx,
        graph,
        name)),
      allDepNames.get());
  }
  
  static hydra.graph.Graph extendGraphForLambda(hydra.graph.Graph g, hydra.core.Lambda lam) {
    hydra.core.Name var = (lam).parameter;
    return new hydra.graph.Graph((g).boundTerms, hydra.lib.maybes.Maybe.applyLazy(
      () -> (g).boundTypes,
      (java.util.function.Function<hydra.core.Type, hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeScheme>>) (dom -> hydra.lib.maps.Insert.apply(
        var,
        hydra.rewriting.Rewriting.fTypeToTypeScheme(dom),
        (g).boundTypes)),
      (lam).domain), (g).classConstraints, hydra.lib.sets.Insert.apply(
      var,
      (g).lambdaVariables), hydra.lib.maps.Delete.apply(
      var,
      (g).metadata), (g).primitives, (g).schemaTypes, (g).typeVariables);
  }
  
  static hydra.graph.Graph extendGraphForLet(java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>> forBinding, hydra.graph.Graph g, hydra.core.Let letrec) {
    hydra.util.ConsList<hydra.core.Binding> bindings = (letrec).bindings;
    hydra.graph.Graph g2 = hydra.lexical.Lexical.extendGraphWithBindings(
      bindings,
      g);
    return new hydra.graph.Graph(hydra.lib.maps.Union.apply(
      hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.core.Binding, hydra.util.Pair<hydra.core.Name, hydra.core.Term>>) (b -> (hydra.util.Pair<hydra.core.Name, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Name, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Name, hydra.core.Term>((b).name, (b).term)))),
        bindings)),
      (g).boundTerms), hydra.lib.maps.Union.apply(
      hydra.lib.maps.FromList.apply(hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>>>) (b -> hydra.lib.maybes.Map.apply(
          (java.util.function.Function<hydra.core.TypeScheme, hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>>) (ts -> (hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>) ((hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>) (new hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>((b).name, ts)))),
          (b).type)),
        bindings))),
      (g).boundTypes), (g).classConstraints, hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<hydra.util.PersistentSet<hydra.core.Name>, java.util.function.Function<hydra.core.Binding, hydra.util.PersistentSet<hydra.core.Name>>>) (s -> (java.util.function.Function<hydra.core.Binding, hydra.util.PersistentSet<hydra.core.Name>>) (b -> hydra.lib.sets.Delete.apply(
        (b).name,
        s))),
      (g).lambdaVariables,
      bindings), (hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Binding, hydra.graph.Graph>>) (gAcc -> (java.util.function.Function<hydra.core.Binding, hydra.graph.Graph>) (b -> {
        hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term> m = (gAcc).metadata;
        hydra.util.Lazy<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>> newMeta = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
          () -> hydra.lib.maps.Delete.apply(
            (b).name,
            m),
          (java.util.function.Function<hydra.core.Term, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>>) (t -> hydra.lib.maps.Insert.apply(
            (b).name,
            t,
            m)),
          ((forBinding).apply(gAcc)).apply(b)));
        return new hydra.graph.Graph((gAcc).boundTerms, (gAcc).boundTypes, (gAcc).classConstraints, (gAcc).lambdaVariables, newMeta.get(), (gAcc).primitives, (gAcc).schemaTypes, (gAcc).typeVariables);
      })),
      g2,
      bindings)).metadata, (g).primitives, (g).schemaTypes, (g).typeVariables);
  }
  
  static hydra.graph.Graph extendGraphForTypeLambda(hydra.graph.Graph g, hydra.core.TypeLambda tlam) {
    hydra.core.Name name = (tlam).parameter;
    return new hydra.graph.Graph((g).boundTerms, (g).boundTypes, (g).classConstraints, (g).lambdaVariables, (g).metadata, (g).primitives, (g).schemaTypes, hydra.lib.sets.Insert.apply(
      name,
      (g).typeVariables));
  }
  
  static hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term> fieldMap(hydra.util.ConsList<hydra.core.Field> fields) {
    java.util.function.Function<hydra.core.Field, hydra.util.Pair<hydra.core.Name, hydra.core.Term>> toPair = (java.util.function.Function<hydra.core.Field, hydra.util.Pair<hydra.core.Name, hydra.core.Term>>) (f -> (hydra.util.Pair<hydra.core.Name, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Name, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Name, hydra.core.Term>((f).name, (f).term))));
    return hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
      toPair,
      fields));
  }
  
  static hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type> fieldTypeMap(hydra.util.ConsList<hydra.core.FieldType> fields) {
    java.util.function.Function<hydra.core.FieldType, hydra.util.Pair<hydra.core.Name, hydra.core.Type>> toPair = (java.util.function.Function<hydra.core.FieldType, hydra.util.Pair<hydra.core.Name, hydra.core.Type>>) (f -> (hydra.util.Pair<hydra.core.Name, hydra.core.Type>) ((hydra.util.Pair<hydra.core.Name, hydra.core.Type>) (new hydra.util.Pair<hydra.core.Name, hydra.core.Type>((f).name, (f).type))));
    return hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
      toPair,
      fields));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>> fieldTypes(hydra.context.Context cx, hydra.graph.Graph graph, hydra.core.Type t) {
    java.util.function.Function<hydra.util.ConsList<hydra.core.FieldType>, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>> toMap = (java.util.function.Function<hydra.util.ConsList<hydra.core.FieldType>, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>>) (fields -> hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.core.FieldType, hydra.util.Pair<hydra.core.Name, hydra.core.Type>>) (ft -> (hydra.util.Pair<hydra.core.Name, hydra.core.Type>) ((hydra.util.Pair<hydra.core.Name, hydra.core.Type>) (new hydra.util.Pair<hydra.core.Name, hydra.core.Type>((ft).name, (ft).type)))),
      fields)));
    return (hydra.rewriting.Rewriting.deannotateType(t)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>> otherwise(hydra.core.Type instance) {
        return hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
          "expected record or union type but found ",
          hydra.show.core.Core.type(t))))), cx)));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>> visit(hydra.core.Type.Forall ft) {
        return hydra.schemas.Schemas.fieldTypes(
          cx,
          graph,
          ((ft).value).body);
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>> visit(hydra.core.Type.Record rt) {
        return hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>>right((toMap).apply((rt).value));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>> visit(hydra.core.Type.Union rt) {
        return hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>>right((toMap).apply((rt).value));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>> visit(hydra.core.Type.Variable name) {
        return hydra.lib.eithers.Bind.apply(
          hydra.lexical.Lexical.requireElement(
            cx,
            graph,
            (name).value),
          (java.util.function.Function<hydra.core.Binding, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>>>) (el -> hydra.lib.eithers.Bind.apply(
            hydra.lib.eithers.Bimap.apply(
              (java.util.function.Function<hydra.error.Error_, hydra.context.InContext<hydra.error.Error_>>) (_wc_e -> (hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(_wc_e, cx))),
              (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (_wc_a -> _wc_a),
              hydra.lib.eithers.Bimap.apply(
                (java.util.function.Function<hydra.error.DecodingError, hydra.error.Error_>) (_e -> new hydra.error.Error_.Other(new hydra.error.OtherError((_e).value))),
                (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (_a -> _a),
                hydra.decode.core.Core.type(
                  graph,
                  (el).term))),
            (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>>>) (decodedType -> hydra.schemas.Schemas.fieldTypes(
              cx,
              graph,
              decodedType)))));
      }
    });
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Type> findFieldType(hydra.context.Context cx, hydra.core.Name fname, hydra.util.ConsList<hydra.core.FieldType> fields) {
    hydra.util.Lazy<hydra.util.ConsList<hydra.core.FieldType>> matchingFields = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
      (java.util.function.Function<hydra.core.FieldType, Boolean>) (ft -> hydra.lib.equality.Equal.apply(
        ((ft).name).value,
        (fname).value)),
      fields));
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(matchingFields.get()),
      () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Type>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
        "No such field: ",
        (fname).value))), cx))),
      () -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.equality.Equal.apply(
          hydra.lib.lists.Length.apply(matchingFields.get()),
          1),
        () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Type>right((hydra.lib.lists.Head.apply(matchingFields.get())).type),
        () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Type>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
          "Multiple fields named ",
          (fname).value))), cx)))));
  }
  
  static hydra.util.Pair<hydra.core.Name, hydra.context.Context> freshName(hydra.context.Context cx) {
    Integer count = hydra.annotations.Annotations.getCount(
      hydra.constants.Constants.key_freshTypeVariableCount(),
      cx);
    return (hydra.util.Pair<hydra.core.Name, hydra.context.Context>) ((hydra.util.Pair<hydra.core.Name, hydra.context.Context>) (new hydra.util.Pair<hydra.core.Name, hydra.context.Context>(hydra.schemas.Schemas.normalTypeVariable(count), hydra.annotations.Annotations.putCount(
      hydra.constants.Constants.key_freshTypeVariableCount(),
      hydra.lib.math.Add.apply(
        count,
        1),
      cx))));
  }
  
  static hydra.util.Pair<hydra.util.ConsList<hydra.core.Name>, hydra.context.Context> freshNames(Integer n, hydra.context.Context cx) {
    return hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<hydra.util.Pair<hydra.util.ConsList<hydra.core.Name>, hydra.context.Context>, java.util.function.Function<java.lang.Void, hydra.util.Pair<hydra.util.ConsList<hydra.core.Name>, hydra.context.Context>>>) (v1 -> (java.util.function.Function<java.lang.Void, hydra.util.Pair<hydra.util.ConsList<hydra.core.Name>, hydra.context.Context>>) (v2 -> hydra.schemas.Schemas.freshNames_go(
        hydra.schemas.Schemas::freshName,
        v1,
        v2))),
      (hydra.util.Pair<hydra.util.ConsList<hydra.core.Name>, hydra.context.Context>) ((hydra.util.Pair<hydra.util.ConsList<hydra.core.Name>, hydra.context.Context>) (new hydra.util.Pair<hydra.util.ConsList<hydra.core.Name>, hydra.context.Context>((hydra.util.ConsList<hydra.core.Name>) (hydra.util.ConsList.<hydra.core.Name>empty()), cx))),
      hydra.lib.lists.Replicate.apply(
        n,
        null));
  }
  
  static <T0> hydra.util.Pair<hydra.util.ConsList<hydra.core.Name>, hydra.context.Context> freshNames_go(java.util.function.Function<hydra.context.Context, hydra.util.Pair<hydra.core.Name, hydra.context.Context>> hydra_schemas_freshName2, hydra.util.Pair<hydra.util.ConsList<hydra.core.Name>, hydra.context.Context> acc, T0 ignored) {
    hydra.util.Lazy<hydra.context.Context> cx0 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(acc));
    hydra.util.Pair<hydra.core.Name, hydra.context.Context> result = (hydra_schemas_freshName2).apply(cx0.get());
    hydra.util.Lazy<hydra.context.Context> cx1 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(result));
    hydra.util.Lazy<hydra.core.Name> name = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(result));
    hydra.util.Lazy<hydra.util.ConsList<hydra.core.Name>> names = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(acc));
    return (hydra.util.Pair<hydra.util.ConsList<hydra.core.Name>, hydra.context.Context>) ((hydra.util.Pair<hydra.util.ConsList<hydra.core.Name>, hydra.context.Context>) (new hydra.util.Pair<hydra.util.ConsList<hydra.core.Name>, hydra.context.Context>(hydra.lib.lists.Concat2.apply(
      names.get(),
      hydra.lib.lists.Pure.apply(name.get())), cx1.get())));
  }
  
  static Boolean fTypeIsPolymorphic(hydra.core.Type typ) {
    return (typ).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.core.Type instance) {
        return false;
      }
      
      @Override
      public Boolean visit(hydra.core.Type.Annotated at) {
        return hydra.schemas.Schemas.fTypeIsPolymorphic(((at).value).body);
      }
      
      @Override
      public Boolean visit(hydra.core.Type.Forall ft) {
        return true;
      }
    });
  }
  
  static hydra.core.Type fullyStripAndNormalizeType(hydra.core.Type typ) {
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<Integer, java.util.function.Function<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>, java.util.function.Function<hydra.core.Type, hydra.util.Pair<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>, hydra.core.Type>>>>> go = new java.util.concurrent.atomic.AtomicReference<>();
    go.set((java.util.function.Function<Integer, java.util.function.Function<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>, java.util.function.Function<hydra.core.Type, hydra.util.Pair<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>, hydra.core.Type>>>>) (depth -> (java.util.function.Function<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>, java.util.function.Function<hydra.core.Type, hydra.util.Pair<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>, hydra.core.Type>>>) (subst -> (java.util.function.Function<hydra.core.Type, hydra.util.Pair<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>, hydra.core.Type>>) (t -> (hydra.rewriting.Rewriting.deannotateType(t)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Pair<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>, hydra.core.Type> otherwise(hydra.core.Type instance) {
        return (hydra.util.Pair<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>, hydra.core.Type>) ((hydra.util.Pair<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>, hydra.core.Type>) (new hydra.util.Pair<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>, hydra.core.Type>(subst, t)));
      }
      
      @Override
      public hydra.util.Pair<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>, hydra.core.Type> visit(hydra.core.Type.Forall ft) {
        hydra.core.Name newVar = new hydra.core.Name(hydra.lib.strings.Cat2.apply(
          "_",
          hydra.lib.literals.ShowInt32.apply(depth)));
        hydra.core.Name oldVar = ((ft).value).parameter;
        return (((go.get()).apply(hydra.lib.math.Add.apply(
          depth,
          1))).apply(hydra.lib.maps.Insert.apply(
          oldVar,
          newVar,
          subst))).apply(((ft).value).body);
      }
    })))));
    hydra.util.Lazy<hydra.util.Pair<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>, hydra.core.Type>> result = new hydra.util.Lazy<>(() -> (((go.get()).apply(0)).apply((hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply())))).apply(typ));
    hydra.util.Lazy<hydra.core.Type> body = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(result.get()));
    hydra.util.Lazy<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>> subst = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(result.get()));
    return hydra.rewriting.Rewriting.substituteTypeVariables(
      subst.get(),
      body.get());
  }
  
  static hydra.core.Type fullyStripType(hydra.core.Type typ) {
    return (hydra.rewriting.Rewriting.deannotateType(typ)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.core.Type otherwise(hydra.core.Type instance) {
        return typ;
      }
      
      @Override
      public hydra.core.Type visit(hydra.core.Type.Forall ft) {
        return hydra.schemas.Schemas.fullyStripType(((ft).value).body);
      }
    });
  }
  
  static hydra.core.Let graphAsLet(hydra.util.ConsList<hydra.core.Binding> bindings, hydra.core.Term body) {
    return new hydra.core.Let(bindings, body);
  }
  
  static hydra.core.Term graphAsTerm(hydra.util.ConsList<hydra.core.Binding> bindings, hydra.core.Term body) {
    return new hydra.core.Term.Let(hydra.schemas.Schemas.graphAsLet(
      bindings,
      body));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.DecodingError>, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>> graphAsTypes(hydra.context.Context cx, hydra.graph.Graph graph, hydra.util.ConsList<hydra.core.Binding> els) {
    java.util.function.Function<hydra.core.Binding, hydra.util.Either<hydra.context.InContext<hydra.error.DecodingError>, hydra.util.Pair<hydra.core.Name, hydra.core.Type>>> toPair = (java.util.function.Function<hydra.core.Binding, hydra.util.Either<hydra.context.InContext<hydra.error.DecodingError>, hydra.util.Pair<hydra.core.Name, hydra.core.Type>>>) (el -> hydra.lib.eithers.Map.apply(
      (java.util.function.Function<hydra.core.Type, hydra.util.Pair<hydra.core.Name, hydra.core.Type>>) (typ -> (hydra.util.Pair<hydra.core.Name, hydra.core.Type>) ((hydra.util.Pair<hydra.core.Name, hydra.core.Type>) (new hydra.util.Pair<hydra.core.Name, hydra.core.Type>((el).name, typ)))),
      hydra.lib.eithers.Bimap.apply(
        (java.util.function.Function<hydra.error.DecodingError, hydra.context.InContext<hydra.error.DecodingError>>) (_wc_e -> (hydra.context.InContext<hydra.error.DecodingError>) (new hydra.context.InContext<hydra.error.DecodingError>(_wc_e, cx))),
        (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (_wc_a -> _wc_a),
        hydra.decode.core.Core.type(
          graph,
          (el).term))));
    return hydra.lib.eithers.Map.apply(
      (java.util.function.Function<hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, hydra.core.Type>>, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>>) ((java.util.function.Function<hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, hydra.core.Type>>, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>>) (hydra.lib.maps.FromList::apply)),
      hydra.lib.eithers.MapList.apply(
        toPair,
        els));
  }
  
  static hydra.util.Pair<hydra.core.Type, hydra.context.Context> instantiateType(hydra.context.Context cx, hydra.core.Type typ) {
    hydra.util.Pair<hydra.core.TypeScheme, hydra.context.Context> result = hydra.schemas.Schemas.instantiateTypeScheme(
      cx,
      hydra.schemas.Schemas.typeToTypeScheme(typ));
    return (hydra.util.Pair<hydra.core.Type, hydra.context.Context>) ((hydra.util.Pair<hydra.core.Type, hydra.context.Context>) (new hydra.util.Pair<hydra.core.Type, hydra.context.Context>(hydra.rewriting.Rewriting.typeSchemeToFType(hydra.lib.pairs.First.apply(result)), hydra.lib.pairs.Second.apply(result))));
  }
  
  static hydra.util.Pair<hydra.core.TypeScheme, hydra.context.Context> instantiateTypeScheme(hydra.context.Context cx, hydra.core.TypeScheme scheme) {
    hydra.util.ConsList<hydra.core.Name> oldVars = (scheme).variables;
    hydra.util.Lazy<hydra.util.Pair<hydra.util.ConsList<hydra.core.Name>, hydra.context.Context>> result = new hydra.util.Lazy<>(() -> hydra.schemas.Schemas.freshNames(
      hydra.lib.lists.Length.apply(oldVars),
      cx));
    hydra.util.Lazy<hydra.context.Context> cx2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(result.get()));
    hydra.util.Lazy<hydra.util.ConsList<hydra.core.Name>> newVars = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(result.get()));
    hydra.util.Lazy<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>> nameSubst = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(hydra.lib.lists.Zip.apply(
      oldVars,
      newVars.get())));
    hydra.util.Lazy<hydra.util.Maybe<hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeVariableMetadata>>> renamedConstraints = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Map.apply(
      (java.util.function.Function<hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeVariableMetadata>, hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (oldConstraints -> hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.TypeVariableMetadata>, hydra.util.Pair<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (kv -> (hydra.util.Pair<hydra.core.Name, hydra.core.TypeVariableMetadata>) ((hydra.util.Pair<hydra.core.Name, hydra.core.TypeVariableMetadata>) (new hydra.util.Pair<hydra.core.Name, hydra.core.TypeVariableMetadata>(hydra.lib.maybes.FromMaybe.applyLazy(
          () -> hydra.lib.pairs.First.apply(kv),
          hydra.lib.maps.Lookup.apply(
            hydra.lib.pairs.First.apply(kv),
            nameSubst.get())), hydra.lib.pairs.Second.apply(kv))))),
        hydra.lib.maps.ToList.apply(oldConstraints)))),
      (scheme).constraints));
    hydra.util.Lazy<hydra.typing.TypeSubst> subst = new hydra.util.Lazy<>(() -> new hydra.typing.TypeSubst(hydra.lib.maps.FromList.apply(hydra.lib.lists.Zip.apply(
      oldVars,
      hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.core.Name, hydra.core.Type>) (x -> new hydra.core.Type.Variable(x)),
        newVars.get())))));
    return (hydra.util.Pair<hydra.core.TypeScheme, hydra.context.Context>) ((hydra.util.Pair<hydra.core.TypeScheme, hydra.context.Context>) (new hydra.util.Pair<hydra.core.TypeScheme, hydra.context.Context>(new hydra.core.TypeScheme(newVars.get(), hydra.substitution.Substitution.substInType(
      subst.get(),
      (scheme).type), renamedConstraints.get()), cx2.get())));
  }
  
  static Boolean isEncodedTerm(hydra.core.Term t) {
    return (hydra.rewriting.Rewriting.deannotateTerm(t)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.core.Term instance) {
        return false;
      }
      
      @Override
      public Boolean visit(hydra.core.Term.Application a) {
        return hydra.schemas.Schemas.isEncodedTerm(((a).value).function);
      }
      
      @Override
      public Boolean visit(hydra.core.Term.Union i) {
        return hydra.lib.equality.Equal.apply(
          "hydra.core.Term",
          (((i).value).typeName).value);
      }
    });
  }
  
  static Boolean isEncodedType(hydra.core.Term t) {
    return (hydra.rewriting.Rewriting.deannotateTerm(t)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.core.Term instance) {
        return false;
      }
      
      @Override
      public Boolean visit(hydra.core.Term.Application a) {
        return hydra.schemas.Schemas.isEncodedType(((a).value).function);
      }
      
      @Override
      public Boolean visit(hydra.core.Term.Union i) {
        return hydra.lib.equality.Equal.apply(
          "hydra.core.Type",
          (((i).value).typeName).value);
      }
    });
  }
  
  static Boolean isEnumRowType(hydra.util.ConsList<hydra.core.FieldType> rt) {
    return hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<Boolean, java.util.function.Function<Boolean, Boolean>>) (p0 -> p1 -> hydra.lib.logic.And.apply(
        p0,
        p1)),
      true,
      hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.core.FieldType, Boolean>) (f -> hydra.schemas.Schemas.isUnitType(hydra.rewriting.Rewriting.deannotateType((f).type))),
        rt));
  }
  
  static Boolean isEnumType(hydra.core.Type typ) {
    return (hydra.rewriting.Rewriting.deannotateType(typ)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.core.Type instance) {
        return false;
      }
      
      @Override
      public Boolean visit(hydra.core.Type.Union rt) {
        return hydra.schemas.Schemas.isEnumRowType((rt).value);
      }
    });
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Boolean> isSerializable(hydra.context.Context cx, hydra.graph.Graph graph, hydra.core.Binding el) {
    java.util.function.Function<hydra.core.Type, hydra.util.ConsList<hydra.variants.TypeVariant>> variants = (java.util.function.Function<hydra.core.Type, hydra.util.ConsList<hydra.variants.TypeVariant>>) (typ -> hydra.lib.lists.Map.apply(
      hydra.reflect.Reflect::typeVariant,
      hydra.rewriting.Rewriting.foldOverType(
        new hydra.coders.TraversalOrder.Pre(),
        (java.util.function.Function<hydra.util.ConsList<hydra.core.Type>, java.util.function.Function<hydra.core.Type, hydra.util.ConsList<hydra.core.Type>>>) (m -> (java.util.function.Function<hydra.core.Type, hydra.util.ConsList<hydra.core.Type>>) (t -> hydra.lib.lists.Cons.apply(
          t,
          m))),
        (hydra.util.ConsList<hydra.core.Type>) (hydra.util.ConsList.<hydra.core.Type>empty()),
        typ)));
    return hydra.lib.eithers.Map.apply(
      (java.util.function.Function<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>, Boolean>) (deps -> {
        hydra.util.Lazy<hydra.util.PersistentSet<hydra.variants.TypeVariant>> allVariants = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
          variants,
          hydra.lib.maps.Elems.apply(deps)))));
        return hydra.lib.logic.Not.apply(hydra.lib.sets.Member.apply(
          new hydra.variants.TypeVariant.Function(),
          allVariants.get()));
      }),
      hydra.schemas.Schemas.typeDependencies(
        cx,
        graph,
        false,
        (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (hydra.lib.equality.Identity::apply),
        (el).name));
  }
  
  static Boolean isSerializableType(hydra.core.Type typ) {
    hydra.util.Lazy<hydra.util.PersistentSet<hydra.variants.TypeVariant>> allVariants = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.lib.lists.Map.apply(
      hydra.reflect.Reflect::typeVariant,
      hydra.rewriting.Rewriting.foldOverType(
        new hydra.coders.TraversalOrder.Pre(),
        (java.util.function.Function<hydra.util.ConsList<hydra.core.Type>, java.util.function.Function<hydra.core.Type, hydra.util.ConsList<hydra.core.Type>>>) (m -> (java.util.function.Function<hydra.core.Type, hydra.util.ConsList<hydra.core.Type>>) (t -> hydra.lib.lists.Cons.apply(
          t,
          m))),
        (hydra.util.ConsList<hydra.core.Type>) (hydra.util.ConsList.<hydra.core.Type>empty()),
        typ))));
    return hydra.lib.logic.Not.apply(hydra.lib.sets.Member.apply(
      new hydra.variants.TypeVariant.Function(),
      allVariants.get()));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Boolean> isSerializableByName(hydra.context.Context cx, hydra.graph.Graph graph, hydra.core.Name name) {
    java.util.function.Function<hydra.core.Type, hydra.util.ConsList<hydra.variants.TypeVariant>> variants = (java.util.function.Function<hydra.core.Type, hydra.util.ConsList<hydra.variants.TypeVariant>>) (typ -> hydra.lib.lists.Map.apply(
      hydra.reflect.Reflect::typeVariant,
      hydra.rewriting.Rewriting.foldOverType(
        new hydra.coders.TraversalOrder.Pre(),
        (java.util.function.Function<hydra.util.ConsList<hydra.core.Type>, java.util.function.Function<hydra.core.Type, hydra.util.ConsList<hydra.core.Type>>>) (m -> (java.util.function.Function<hydra.core.Type, hydra.util.ConsList<hydra.core.Type>>) (t -> hydra.lib.lists.Cons.apply(
          t,
          m))),
        (hydra.util.ConsList<hydra.core.Type>) (hydra.util.ConsList.<hydra.core.Type>empty()),
        typ)));
    return hydra.lib.eithers.Map.apply(
      (java.util.function.Function<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>, Boolean>) (deps -> {
        hydra.util.Lazy<hydra.util.PersistentSet<hydra.variants.TypeVariant>> allVariants = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
          variants,
          hydra.lib.maps.Elems.apply(deps)))));
        return hydra.lib.logic.Not.apply(hydra.lib.sets.Member.apply(
          new hydra.variants.TypeVariant.Function(),
          allVariants.get()));
      }),
      hydra.schemas.Schemas.typeDependencies(
        cx,
        graph,
        false,
        (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (hydra.lib.equality.Identity::apply),
        name));
  }
  
  static Boolean isType(hydra.core.Type t) {
    return (hydra.rewriting.Rewriting.deannotateType(t)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.core.Type instance) {
        return false;
      }
      
      @Override
      public Boolean visit(hydra.core.Type.Application a) {
        return hydra.schemas.Schemas.isType(((a).value).function);
      }
      
      @Override
      public Boolean visit(hydra.core.Type.Forall l) {
        return hydra.schemas.Schemas.isType(((l).value).body);
      }
      
      @Override
      public Boolean visit(hydra.core.Type.Union rt) {
        return false;
      }
      
      @Override
      public Boolean visit(hydra.core.Type.Variable v) {
        return hydra.lib.equality.Equal.apply(
          (v).value,
          new hydra.core.Name("hydra.core.Type"));
      }
    });
  }
  
  static Boolean isUnitTerm(hydra.core.Term v1) {
    return (v1).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.core.Term instance) {
        return false;
      }
      
      @Override
      public Boolean visit(hydra.core.Term.Unit ignored) {
        return true;
      }
    });
  }
  
  static Boolean isUnitType(hydra.core.Type v1) {
    return (v1).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.core.Type instance) {
        return false;
      }
      
      @Override
      public Boolean visit(hydra.core.Type.Unit ignored) {
        return true;
      }
    });
  }
  
  static Boolean moduleContainsBinaryLiterals(hydra.module.Module mod) {
    java.util.function.Function<Boolean, java.util.function.Function<hydra.core.Term, Boolean>> checkTerm = (java.util.function.Function<Boolean, java.util.function.Function<hydra.core.Term, Boolean>>) (found -> (java.util.function.Function<hydra.core.Term, Boolean>) (term -> hydra.lib.logic.Or.apply(
      found,
      (term).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public Boolean otherwise(hydra.core.Term instance) {
          return false;
        }
        
        @Override
        public Boolean visit(hydra.core.Term.Literal lit) {
          return ((lit).value).accept(new hydra.core.Literal.PartialVisitor<>() {
            @Override
            public Boolean otherwise(hydra.core.Literal instance) {
              return false;
            }
            
            @Override
            public Boolean visit(hydra.core.Literal.Binary ignored) {
              return true;
            }
          });
        }
      }))));
    java.util.function.Function<hydra.core.Term, Boolean> termContainsBinary = (java.util.function.Function<hydra.core.Term, Boolean>) (term -> hydra.rewriting.Rewriting.foldOverTerm(
      new hydra.coders.TraversalOrder.Pre(),
      checkTerm,
      false,
      term));
    return hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<Boolean, java.util.function.Function<hydra.core.Binding, Boolean>>) (acc -> (java.util.function.Function<hydra.core.Binding, Boolean>) (el -> hydra.lib.logic.Or.apply(
        acc,
        (termContainsBinary).apply((el).term)))),
      false,
      (mod).elements);
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.PersistentSet<hydra.module.Namespace>> moduleDependencyNamespaces(hydra.context.Context cx, hydra.graph.Graph graph, Boolean binds, Boolean withPrims, Boolean withNoms, Boolean withSchema, hydra.module.Module mod) {
    return hydra.lib.eithers.Map.apply(
      (java.util.function.Function<hydra.util.PersistentSet<hydra.module.Namespace>, hydra.util.PersistentSet<hydra.module.Namespace>>) (deps -> hydra.lib.sets.Delete.apply(
        (mod).namespace,
        deps)),
      hydra.schemas.Schemas.dependencyNamespaces(
        cx,
        graph,
        binds,
        withPrims,
        withNoms,
        withSchema,
        (mod).elements));
  }
  
  static <T0> hydra.module.Namespaces<T0> namespacesForDefinitions(java.util.function.Function<hydra.module.Namespace, T0> encodeNamespace, hydra.module.Namespace focusNs, hydra.util.ConsList<hydra.module.Definition> defs) {
    hydra.util.Lazy<hydra.util.PersistentSet<hydra.module.Namespace>> nss = new hydra.util.Lazy<>(() -> hydra.lib.sets.Delete.apply(
      focusNs,
      hydra.schemas.Schemas.definitionDependencyNamespaces(defs)));
    java.util.function.Function<hydra.module.Namespace, hydra.util.Pair<hydra.module.Namespace, T0>> toPair = (java.util.function.Function<hydra.module.Namespace, hydra.util.Pair<hydra.module.Namespace, T0>>) (v1 -> hydra.schemas.Schemas.<T0>namespacesForDefinitions_toPair(
      encodeNamespace,
      v1));
    return (hydra.module.Namespaces<T0>) (new hydra.module.Namespaces<T0>((toPair).apply(focusNs), hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
      toPair,
      hydra.lib.sets.ToList.apply(nss.get())))));
  }
  
  static <T0> hydra.util.Pair<hydra.module.Namespace, T0> namespacesForDefinitions_toPair(java.util.function.Function<hydra.module.Namespace, T0> encodeNamespace, hydra.module.Namespace ns) {
    return (hydra.util.Pair<hydra.module.Namespace, T0>) ((hydra.util.Pair<hydra.module.Namespace, T0>) (new hydra.util.Pair<hydra.module.Namespace, T0>(ns, (encodeNamespace).apply(ns))));
  }
  
  static hydra.core.Type nominalApplication(hydra.core.Name tname, hydra.util.ConsList<hydra.core.Type> args) {
    return hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<hydra.core.Type, java.util.function.Function<hydra.core.Type, hydra.core.Type>>) (t -> (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (a -> new hydra.core.Type.Application(new hydra.core.ApplicationType(t, a)))),
      new hydra.core.Type.Variable(tname),
      args);
  }
  
  static hydra.core.Name normalTypeVariable(Integer i) {
    return new hydra.core.Name(hydra.lib.strings.Cat2.apply(
      "t",
      hydra.lib.literals.ShowInt32.apply(i)));
  }
  
  static hydra.util.Pair<hydra.util.ConsList<hydra.module.TypeDefinition>, hydra.util.ConsList<hydra.module.TermDefinition>> partitionDefinitions(hydra.util.ConsList<hydra.module.Definition> defs) {
    java.util.function.Function<hydra.module.Definition, hydra.util.Maybe<hydra.module.TermDefinition>> getTerm = (java.util.function.Function<hydra.module.Definition, hydra.util.Maybe<hydra.module.TermDefinition>>) (def -> (def).accept(new hydra.module.Definition.PartialVisitor<>() {
      @Override
      public hydra.util.Maybe<hydra.module.TermDefinition> visit(hydra.module.Definition.Type ignored) {
        return (hydra.util.Maybe<hydra.module.TermDefinition>) (hydra.util.Maybe.<hydra.module.TermDefinition>nothing());
      }
      
      @Override
      public hydra.util.Maybe<hydra.module.TermDefinition> visit(hydra.module.Definition.Term td) {
        return hydra.util.Maybe.just((td).value);
      }
    }));
    java.util.function.Function<hydra.module.Definition, hydra.util.Maybe<hydra.module.TypeDefinition>> getType = (java.util.function.Function<hydra.module.Definition, hydra.util.Maybe<hydra.module.TypeDefinition>>) (def -> (def).accept(new hydra.module.Definition.PartialVisitor<>() {
      @Override
      public hydra.util.Maybe<hydra.module.TypeDefinition> visit(hydra.module.Definition.Type td) {
        return hydra.util.Maybe.just((td).value);
      }
      
      @Override
      public hydra.util.Maybe<hydra.module.TypeDefinition> visit(hydra.module.Definition.Term ignored) {
        return (hydra.util.Maybe<hydra.module.TypeDefinition>) (hydra.util.Maybe.<hydra.module.TypeDefinition>nothing());
      }
    }));
    return (hydra.util.Pair<hydra.util.ConsList<hydra.module.TypeDefinition>, hydra.util.ConsList<hydra.module.TermDefinition>>) ((hydra.util.Pair<hydra.util.ConsList<hydra.module.TypeDefinition>, hydra.util.ConsList<hydra.module.TermDefinition>>) (new hydra.util.Pair<hydra.util.ConsList<hydra.module.TypeDefinition>, hydra.util.ConsList<hydra.module.TermDefinition>>(hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
      getType,
      defs)), hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
      getTerm,
      defs)))));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.ConsList<hydra.core.FieldType>> requireRecordType(hydra.context.Context cx, hydra.graph.Graph graph, hydra.core.Name name) {
    java.util.function.Function<hydra.core.Type, hydra.util.Maybe<hydra.util.ConsList<hydra.core.FieldType>>> toRecord = (java.util.function.Function<hydra.core.Type, hydra.util.Maybe<hydra.util.ConsList<hydra.core.FieldType>>>) (t -> (t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Maybe<hydra.util.ConsList<hydra.core.FieldType>> otherwise(hydra.core.Type instance) {
        return (hydra.util.Maybe<hydra.util.ConsList<hydra.core.FieldType>>) (hydra.util.Maybe.<hydra.util.ConsList<hydra.core.FieldType>>nothing());
      }
      
      @Override
      public hydra.util.Maybe<hydra.util.ConsList<hydra.core.FieldType>> visit(hydra.core.Type.Record rt) {
        return hydra.util.Maybe.just((rt).value);
      }
    }));
    return hydra.schemas.Schemas.requireRowType(
      cx,
      "record type",
      toRecord,
      graph,
      name);
  }
  
  static <T0> hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T0> requireRowType(hydra.context.Context cx, String label, java.util.function.Function<hydra.core.Type, hydra.util.Maybe<T0>> getter, hydra.graph.Graph graph, hydra.core.Name name) {
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.core.Type, hydra.core.Type>> rawType = new java.util.concurrent.atomic.AtomicReference<>();
    rawType.set((java.util.function.Function<hydra.core.Type, hydra.core.Type>) (t -> (t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.core.Type otherwise(hydra.core.Type instance) {
        return t;
      }
      
      @Override
      public hydra.core.Type visit(hydra.core.Type.Annotated at) {
        return (rawType.get()).apply(((at).value).body);
      }
      
      @Override
      public hydra.core.Type visit(hydra.core.Type.Forall ft) {
        return (rawType.get()).apply(((ft).value).body);
      }
    })));
    return hydra.lib.eithers.Bind.apply(
      hydra.schemas.Schemas.requireType(
        cx,
        graph,
        name),
      (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T0>>) (t -> hydra.lib.maybes.Maybe.applyLazy(
        () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, T0>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
          (name).value,
          " does not resolve to a ",
          label,
          " type: ",
          hydra.show.core.Core.type(t))))), cx))),
        (java.util.function.Function<T0, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T0>>) (x -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, T0>right(x)),
        (getter).apply((rawType.get()).apply(t)))));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.TypeScheme, hydra.context.Context>> requireSchemaType(hydra.context.Context cx, hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeScheme> types, hydra.core.Name tname) {
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.TypeScheme, hydra.context.Context>>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
        "No such schema type: ",
        (tname).value,
        ". Available types are: ",
        hydra.lib.strings.Intercalate.apply(
          ", ",
          hydra.lib.lists.Map.apply(
            wrapped -> (wrapped).value,
            hydra.lib.maps.Keys.apply(types))))))), cx))),
      (java.util.function.Function<hydra.core.TypeScheme, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.TypeScheme, hydra.context.Context>>>) (ts -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.TypeScheme, hydra.context.Context>>right(hydra.schemas.Schemas.instantiateTypeScheme(
        cx,
        hydra.rewriting.Rewriting.deannotateTypeSchemeRecursive(ts)))),
      hydra.lib.maps.Lookup.apply(
        tname,
        types));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Type> requireType(hydra.context.Context cx, hydra.graph.Graph graph, hydra.core.Name name) {
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> hydra.lib.maybes.Maybe.applyLazy(
        () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Type>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
          "no such type: ",
          (name).value))), cx))),
        (java.util.function.Function<hydra.core.TypeScheme, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Type>>) (ts -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Type>right(hydra.rewriting.Rewriting.typeSchemeToFType(ts))),
        hydra.lib.maps.Lookup.apply(
          name,
          (graph).boundTypes)),
      (java.util.function.Function<hydra.core.TypeScheme, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Type>>) (ts -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Type>right(hydra.rewriting.Rewriting.typeSchemeToFType(ts))),
      hydra.lib.maps.Lookup.apply(
        name,
        (graph).schemaTypes));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Type> requireUnionField(hydra.context.Context cx, hydra.graph.Graph graph, hydra.core.Name tname, hydra.core.Name fname) {
    java.util.function.Function<hydra.util.ConsList<hydra.core.FieldType>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Type>> withRowType = (java.util.function.Function<hydra.util.ConsList<hydra.core.FieldType>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Type>>) (rt -> {
      hydra.util.Lazy<hydra.util.ConsList<hydra.core.FieldType>> matches = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
        (java.util.function.Function<hydra.core.FieldType, Boolean>) (ft -> hydra.lib.equality.Equal.apply(
          (ft).name,
          fname)),
        rt));
      return hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(matches.get()),
        () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Type>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
          "no field \"",
          (fname).value,
          "\" in union type \"",
          (tname).value)))), cx))),
        () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Type>right((hydra.lib.lists.Head.apply(matches.get())).type));
    });
    return hydra.lib.eithers.Bind.apply(
      hydra.schemas.Schemas.requireUnionType(
        cx,
        graph,
        tname),
      withRowType);
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.ConsList<hydra.core.FieldType>> requireUnionType(hydra.context.Context cx, hydra.graph.Graph graph, hydra.core.Name name) {
    java.util.function.Function<hydra.core.Type, hydra.util.Maybe<hydra.util.ConsList<hydra.core.FieldType>>> toUnion = (java.util.function.Function<hydra.core.Type, hydra.util.Maybe<hydra.util.ConsList<hydra.core.FieldType>>>) (t -> (t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Maybe<hydra.util.ConsList<hydra.core.FieldType>> otherwise(hydra.core.Type instance) {
        return (hydra.util.Maybe<hydra.util.ConsList<hydra.core.FieldType>>) (hydra.util.Maybe.<hydra.util.ConsList<hydra.core.FieldType>>nothing());
      }
      
      @Override
      public hydra.util.Maybe<hydra.util.ConsList<hydra.core.FieldType>> visit(hydra.core.Type.Union rt) {
        return hydra.util.Maybe.just((rt).value);
      }
    }));
    return hydra.schemas.Schemas.requireRowType(
      cx,
      "union",
      toUnion,
      graph,
      name);
  }
  
  static hydra.util.Maybe<hydra.core.Type> resolveType(hydra.graph.Graph graph, hydra.core.Type typ) {
    return (hydra.rewriting.Rewriting.deannotateType(typ)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Maybe<hydra.core.Type> otherwise(hydra.core.Type instance) {
        return hydra.util.Maybe.just(typ);
      }
      
      @Override
      public hydra.util.Maybe<hydra.core.Type> visit(hydra.core.Type.Variable name) {
        return hydra.lib.maybes.Maybe.applyLazy(
          () -> hydra.lib.maybes.Map.apply(
            (java.util.function.Function<hydra.core.TypeScheme, hydra.core.Type>) (ts -> hydra.rewriting.Rewriting.typeSchemeToFType(ts)),
            hydra.lib.maps.Lookup.apply(
              (name).value,
              (graph).boundTypes)),
          (java.util.function.Function<hydra.core.TypeScheme, hydra.util.Maybe<hydra.core.Type>>) (ts -> hydra.util.Maybe.just(hydra.rewriting.Rewriting.typeSchemeToFType(ts))),
          hydra.lib.maps.Lookup.apply(
            (name).value,
            (graph).schemaTypes));
      }
    });
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeScheme>> schemaGraphToTypingEnvironment(hydra.context.Context cx, hydra.graph.Graph g) {
    java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Type>> decodeType = (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Type>>) (term -> hydra.lib.eithers.Bimap.apply(
      (java.util.function.Function<hydra.error.Error_, hydra.context.InContext<hydra.error.Error_>>) (_wc_e -> (hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(_wc_e, cx))),
      (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (_wc_a -> _wc_a),
      hydra.lib.eithers.Bimap.apply(
        (java.util.function.Function<hydra.error.DecodingError, hydra.error.Error_>) (_e -> new hydra.error.Error_.Other(new hydra.error.OtherError((_e).value))),
        (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (_a -> _a),
        hydra.decode.core.Core.type(
          g,
          term))));
    java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.TypeScheme>> decodeTypeScheme = (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.TypeScheme>>) (term -> hydra.lib.eithers.Bimap.apply(
      (java.util.function.Function<hydra.error.Error_, hydra.context.InContext<hydra.error.Error_>>) (_wc_e -> (hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(_wc_e, cx))),
      (java.util.function.Function<hydra.core.TypeScheme, hydra.core.TypeScheme>) (_wc_a -> _wc_a),
      hydra.lib.eithers.Bimap.apply(
        (java.util.function.Function<hydra.error.DecodingError, hydra.error.Error_>) (_e -> new hydra.error.Error_.Other(new hydra.error.OtherError((_e).value))),
        (java.util.function.Function<hydra.core.TypeScheme, hydra.core.TypeScheme>) (_a -> _a),
        hydra.decode.core.Core.typeScheme(
          g,
          term))));
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.util.ConsList<hydra.core.Name>, java.util.function.Function<hydra.core.Type, hydra.core.TypeScheme>>> toTypeScheme = new java.util.concurrent.atomic.AtomicReference<>();
    toTypeScheme.set((java.util.function.Function<hydra.util.ConsList<hydra.core.Name>, java.util.function.Function<hydra.core.Type, hydra.core.TypeScheme>>) (vars -> (java.util.function.Function<hydra.core.Type, hydra.core.TypeScheme>) (typ -> (hydra.rewriting.Rewriting.deannotateType(typ)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.core.TypeScheme otherwise(hydra.core.Type instance) {
        return new hydra.core.TypeScheme(hydra.lib.lists.Reverse.apply(vars), typ, (hydra.util.Maybe<hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing()));
      }
      
      @Override
      public hydra.core.TypeScheme visit(hydra.core.Type.Forall ft) {
        return ((toTypeScheme.get()).apply(hydra.lib.lists.Cons.apply(
          ((ft).value).parameter,
          vars))).apply(((ft).value).body);
      }
    }))));
    java.util.function.Function<hydra.core.Binding, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>>>> toPair = (java.util.function.Function<hydra.core.Binding, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>>>>) (el -> {
      java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.core.TypeScheme>>> forTerm = (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.core.TypeScheme>>>) (term -> (term).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.core.TypeScheme>> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.core.TypeScheme>>right((hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()));
        }
        
        @Override
        public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.core.TypeScheme>> visit(hydra.core.Term.Record r) {
          return hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              ((r).value).typeName,
              new hydra.core.Name("hydra.core.TypeScheme")),
            () -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.core.TypeScheme, hydra.util.Maybe<hydra.core.TypeScheme>>) (hydra.lib.maybes.Pure::apply),
              (decodeTypeScheme).apply((el).term)),
            () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.core.TypeScheme>>right((hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())));
        }
        
        @Override
        public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.core.TypeScheme>> visit(hydra.core.Term.Union i) {
          return hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              ((i).value).typeName,
              new hydra.core.Name("hydra.core.Type")),
            () -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.core.Type, hydra.util.Maybe<hydra.core.TypeScheme>>) (decoded -> hydra.util.Maybe.just(((toTypeScheme.get()).apply((hydra.util.ConsList<hydra.core.Name>) (hydra.util.ConsList.<hydra.core.Name>empty()))).apply(decoded))),
              (decodeType).apply((el).term)),
            () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.core.TypeScheme>>right((hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())));
        }
      }));
      return hydra.lib.eithers.Bind.apply(
        hydra.lib.maybes.Maybe.applyLazy(
          () -> hydra.lib.eithers.Map.apply(
            (java.util.function.Function<hydra.core.Type, hydra.util.Maybe<hydra.core.TypeScheme>>) (typ -> hydra.util.Maybe.just(hydra.rewriting.Rewriting.fTypeToTypeScheme(typ))),
            (decodeType).apply((el).term)),
          (java.util.function.Function<hydra.core.TypeScheme, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.core.TypeScheme>>>) (ts -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              ts,
              new hydra.core.TypeScheme((hydra.util.ConsList<hydra.core.Name>) (hydra.util.ConsList.<hydra.core.Name>empty()), new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.TypeScheme")), (hydra.util.Maybe<hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing()))),
            () -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.core.TypeScheme, hydra.util.Maybe<hydra.core.TypeScheme>>) (hydra.lib.maybes.Pure::apply),
              (decodeTypeScheme).apply((el).term)),
            () -> hydra.lib.logic.IfElse.lazy(
              hydra.lib.equality.Equal.apply(
                ts,
                new hydra.core.TypeScheme((hydra.util.ConsList<hydra.core.Name>) (hydra.util.ConsList.<hydra.core.Name>empty()), new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Type")), (hydra.util.Maybe<hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing()))),
              () -> hydra.lib.eithers.Map.apply(
                (java.util.function.Function<hydra.core.Type, hydra.util.Maybe<hydra.core.TypeScheme>>) (decoded -> hydra.util.Maybe.just(((toTypeScheme.get()).apply((hydra.util.ConsList<hydra.core.Name>) (hydra.util.ConsList.<hydra.core.Name>empty()))).apply(decoded))),
                (decodeType).apply((el).term)),
              () -> (forTerm).apply(hydra.rewriting.Rewriting.deannotateTerm((el).term))))),
          (el).type),
        (java.util.function.Function<hydra.util.Maybe<hydra.core.TypeScheme>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>>>>) (mts -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>>>right(hydra.lib.maybes.Map.apply(
          (java.util.function.Function<hydra.core.TypeScheme, hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>>) (ts -> (hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>) ((hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>) (new hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>((el).name, ts)))),
          mts))));
    });
    return hydra.lib.eithers.Map.apply(
      (java.util.function.Function<hydra.util.ConsList<hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>>>, hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeScheme>>) (mpairs -> hydra.lib.maps.FromList.apply(hydra.lib.maybes.Cat.apply(mpairs))),
      hydra.lib.eithers.MapList.apply(
        toPair,
        hydra.lexical.Lexical.graphToBindings(g)));
  }
  
  static hydra.util.ConsList<hydra.core.Binding> termAsBindings(hydra.core.Term term) {
    return (hydra.rewriting.Rewriting.deannotateTerm(term)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.ConsList<hydra.core.Binding> otherwise(hydra.core.Term instance) {
        return (hydra.util.ConsList<hydra.core.Binding>) (hydra.util.ConsList.<hydra.core.Binding>empty());
      }
      
      @Override
      public hydra.util.ConsList<hydra.core.Binding> visit(hydra.core.Term.Let lt) {
        return ((lt).value).bindings;
      }
    });
  }
  
  static hydra.util.ConsList<hydra.util.ConsList<hydra.module.TypeDefinition>> topologicalSortTypeDefinitions(hydra.util.ConsList<hydra.module.TypeDefinition> defs) {
    hydra.util.Lazy<hydra.util.PersistentMap<hydra.core.Name, hydra.module.TypeDefinition>> nameToDef = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.module.TypeDefinition, hydra.util.Pair<hydra.core.Name, hydra.module.TypeDefinition>>) (d -> (hydra.util.Pair<hydra.core.Name, hydra.module.TypeDefinition>) ((hydra.util.Pair<hydra.core.Name, hydra.module.TypeDefinition>) (new hydra.util.Pair<hydra.core.Name, hydra.module.TypeDefinition>((d).name, d)))),
      defs)));
    java.util.function.Function<hydra.module.TypeDefinition, hydra.util.Pair<hydra.core.Name, hydra.util.ConsList<hydra.core.Name>>> toPair = (java.util.function.Function<hydra.module.TypeDefinition, hydra.util.Pair<hydra.core.Name, hydra.util.ConsList<hydra.core.Name>>>) (def -> (hydra.util.Pair<hydra.core.Name, hydra.util.ConsList<hydra.core.Name>>) ((hydra.util.Pair<hydra.core.Name, hydra.util.ConsList<hydra.core.Name>>) (new hydra.util.Pair<hydra.core.Name, hydra.util.ConsList<hydra.core.Name>>((def).name, hydra.lib.sets.ToList.apply(hydra.rewriting.Rewriting.typeDependencyNames(
      false,
      (def).type))))));
    hydra.util.Lazy<hydra.util.ConsList<hydra.util.ConsList<hydra.core.Name>>> sorted = new hydra.util.Lazy<>(() -> hydra.sorting.Sorting.topologicalSortComponents(hydra.lib.lists.Map.apply(
      toPair,
      defs)));
    return hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.util.ConsList<hydra.core.Name>, hydra.util.ConsList<hydra.module.TypeDefinition>>) (names -> hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.core.Name, hydra.util.Maybe<hydra.module.TypeDefinition>>) (n -> hydra.lib.maps.Lookup.apply(
          n,
          nameToDef.get())),
        names))),
      sorted.get());
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>> typeDependencies(hydra.context.Context cx, hydra.graph.Graph graph, Boolean withSchema, java.util.function.Function<hydra.core.Type, hydra.core.Type> transform, hydra.core.Name name) {
    java.util.function.Function<hydra.core.Name, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Type>> requireType = (java.util.function.Function<hydra.core.Name, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Type>>) (name2 -> {
      hydra.util.Lazy<hydra.context.Context> cx1 = new hydra.util.Lazy<>(() -> new hydra.context.Context(hydra.lib.lists.Cons.apply(
        hydra.lib.strings.Cat2.apply(
          "type dependencies of ",
          (name2).value),
        (cx).trace), (cx).messages, (cx).other));
      return hydra.lib.eithers.Bind.apply(
        hydra.lexical.Lexical.requireElement(
          cx1.get(),
          graph,
          name2),
        (java.util.function.Function<hydra.core.Binding, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Type>>) (el -> hydra.lib.eithers.Bimap.apply(
          (java.util.function.Function<hydra.error.Error_, hydra.context.InContext<hydra.error.Error_>>) (_wc_e -> (hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(_wc_e, cx1.get()))),
          (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (_wc_a -> _wc_a),
          hydra.lib.eithers.Bimap.apply(
            (java.util.function.Function<hydra.error.DecodingError, hydra.error.Error_>) (_e -> new hydra.error.Error_.Other(new hydra.error.OtherError((_e).value))),
            (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (_a -> _a),
            hydra.decode.core.Core.type(
              graph,
              (el).term)))));
    });
    java.util.function.Function<hydra.core.Name, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Name, hydra.core.Type>>> toPair = (java.util.function.Function<hydra.core.Name, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Name, hydra.core.Type>>>) (name2 -> hydra.lib.eithers.Map.apply(
      (java.util.function.Function<hydra.core.Type, hydra.util.Pair<hydra.core.Name, hydra.core.Type>>) (typ -> (hydra.util.Pair<hydra.core.Name, hydra.core.Type>) ((hydra.util.Pair<hydra.core.Name, hydra.core.Type>) (new hydra.util.Pair<hydra.core.Name, hydra.core.Type>(name2, (transform).apply(typ))))),
      (requireType).apply(name2)));
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.util.PersistentSet<hydra.core.Name>, java.util.function.Function<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>>>>> deps = new java.util.concurrent.atomic.AtomicReference<>();
    deps.set((java.util.function.Function<hydra.util.PersistentSet<hydra.core.Name>, java.util.function.Function<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>>>>) (seeds -> (java.util.function.Function<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>>>) (names -> hydra.lib.logic.IfElse.lazy(
      hydra.lib.sets.Null.apply(seeds),
      () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>>right(names),
      () -> hydra.lib.eithers.Bind.apply(
        hydra.lib.eithers.MapList.apply(
          toPair,
          hydra.lib.sets.ToList.apply(seeds)),
        (java.util.function.Function<hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, hydra.core.Type>>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>>>) (pairs -> {
          hydra.util.Lazy<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>> newNames = new hydra.util.Lazy<>(() -> hydra.lib.maps.Union.apply(
            names,
            hydra.lib.maps.FromList.apply(pairs)));
          hydra.util.Lazy<hydra.util.PersistentSet<hydra.core.Name>> refs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
            (java.util.function.Function<hydra.util.PersistentSet<hydra.core.Name>, java.util.function.Function<hydra.util.PersistentSet<hydra.core.Name>, hydra.util.PersistentSet<hydra.core.Name>>>) ((java.util.function.Function<hydra.util.PersistentSet<hydra.core.Name>, java.util.function.Function<hydra.util.PersistentSet<hydra.core.Name>, hydra.util.PersistentSet<hydra.core.Name>>>) (p0 -> p1 -> hydra.lib.sets.Union.apply(
              p0,
              p1))),
            (hydra.util.PersistentSet<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
            hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.Type>, hydra.util.PersistentSet<hydra.core.Name>>) (pair -> hydra.rewriting.Rewriting.typeDependencyNames(
                withSchema,
                hydra.lib.pairs.Second.apply(pair))),
              pairs)));
          hydra.util.Lazy<hydra.util.PersistentSet<hydra.core.Name>> visited = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.lib.maps.Keys.apply(names)));
          hydra.util.Lazy<hydra.util.PersistentSet<hydra.core.Name>> newSeeds = new hydra.util.Lazy<>(() -> hydra.lib.sets.Difference.apply(
            refs.get(),
            visited.get()));
          return ((deps.get()).apply(newSeeds.get())).apply(newNames.get());
        }))))));
    return ((deps.get()).apply(hydra.lib.sets.Singleton.apply(name))).apply((hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())));
  }
  
  static hydra.core.TypeScheme typeToTypeScheme(hydra.core.Type t0) {
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.util.ConsList<hydra.core.Name>, java.util.function.Function<hydra.core.Type, hydra.core.TypeScheme>>> helper = new java.util.concurrent.atomic.AtomicReference<>();
    helper.set((java.util.function.Function<hydra.util.ConsList<hydra.core.Name>, java.util.function.Function<hydra.core.Type, hydra.core.TypeScheme>>) (vars -> (java.util.function.Function<hydra.core.Type, hydra.core.TypeScheme>) (t -> (hydra.rewriting.Rewriting.deannotateType(t)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.core.TypeScheme otherwise(hydra.core.Type instance) {
        return new hydra.core.TypeScheme(hydra.lib.lists.Reverse.apply(vars), t, (hydra.util.Maybe<hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing()));
      }
      
      @Override
      public hydra.core.TypeScheme visit(hydra.core.Type.Forall ft) {
        return ((helper.get()).apply(hydra.lib.lists.Cons.apply(
          ((ft).value).parameter,
          vars))).apply(((ft).value).body);
      }
    }))));
    return ((helper.get()).apply((hydra.util.ConsList<hydra.core.Name>) (hydra.util.ConsList.<hydra.core.Name>empty()))).apply(t0);
  }
  
  static hydra.util.ConsList<hydra.core.Binding> typesToElements(hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type> typeMap) {
    java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.Type>, hydra.core.Binding> toElement = (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.Type>, hydra.core.Binding>) (pair -> {
      hydra.util.Lazy<hydra.core.Name> name = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(pair));
      return new hydra.core.Binding(name.get(), hydra.encode.core.Core.type(hydra.lib.pairs.Second.apply(pair)), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()));
    });
    return hydra.lib.lists.Map.apply(
      toElement,
      hydra.lib.maps.ToList.apply(typeMap));
  }
  
  static <T0, T1, T2> T2 withLambdaContext(java.util.function.Function<T0, hydra.graph.Graph> getContext, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<T0, T1>> setContext, T0 env, hydra.core.Lambda lam, java.util.function.Function<T1, T2> body) {
    hydra.graph.Graph newContext = hydra.schemas.Schemas.extendGraphForLambda(
      (getContext).apply(env),
      lam);
    return (body).apply(((setContext).apply(newContext)).apply(env));
  }
  
  static <T0, T1, T2> T2 withLetContext(java.util.function.Function<T0, hydra.graph.Graph> getContext, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<T0, T1>> setContext, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>> forBinding, T0 env, hydra.core.Let letrec, java.util.function.Function<T1, T2> body) {
    hydra.graph.Graph newContext = hydra.schemas.Schemas.extendGraphForLet(
      forBinding,
      (getContext).apply(env),
      letrec);
    return (body).apply(((setContext).apply(newContext)).apply(env));
  }
  
  static <T0, T1, T2> T2 withTypeLambdaContext(java.util.function.Function<T0, hydra.graph.Graph> getContext, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<T0, T1>> setContext, T0 env, hydra.core.TypeLambda tlam, java.util.function.Function<T1, T2> body) {
    hydra.graph.Graph newContext = hydra.schemas.Schemas.extendGraphForTypeLambda(
      (getContext).apply(env),
      tlam);
    return (body).apply(((setContext).apply(newContext)).apply(env));
  }
}
