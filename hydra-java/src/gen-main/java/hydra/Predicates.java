// Note: this is an automatically generated file. Do not edit.

package hydra;

/**
 * Type and term classification predicates
 */
public interface Predicates {
  static Boolean isEncodedTerm(hydra.core.Term t) {
    return hydra.Strip.deannotateTerm(t).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.core.Term instance) {
        return false;
      }

      @Override
      public Boolean visit(hydra.core.Term.Application a) {
        return hydra.Predicates.isEncodedTerm((a).value.function);
      }

      @Override
      public Boolean visit(hydra.core.Term.Union i) {
        return hydra.lib.equality.Equal.apply(
          "hydra.core.Term",
          (i).value.typeName.value);
      }
    });
  }

  static Boolean isEncodedType(hydra.core.Term t) {
    return hydra.Strip.deannotateTerm(t).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.core.Term instance) {
        return false;
      }

      @Override
      public Boolean visit(hydra.core.Term.Application a) {
        return hydra.Predicates.isEncodedType((a).value.function);
      }

      @Override
      public Boolean visit(hydra.core.Term.Union i) {
        return hydra.lib.equality.Equal.apply(
          "hydra.core.Type",
          (i).value.typeName.value);
      }
    });
  }

  static Boolean isEnumRowType(java.util.List<hydra.core.FieldType> rt) {
    return hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<Boolean, java.util.function.Function<Boolean, Boolean>>) (p0 -> p1 -> hydra.lib.logic.And.apply(
        p0,
        p1)),
      true,
      hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.core.FieldType, Boolean>) (f -> hydra.Predicates.isUnitType(hydra.Strip.deannotateType((f).type))),
        rt));
  }

  static Boolean isEnumType(hydra.core.Type typ) {
    return hydra.Strip.deannotateType(typ).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.core.Type instance) {
        return false;
      }

      @Override
      public Boolean visit(hydra.core.Type.Union rt) {
        return hydra.Predicates.isEnumRowType((rt).value);
      }
    });
  }

  static Boolean isNominalType(hydra.core.Type typ) {
    return hydra.Strip.deannotateType(typ).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.core.Type instance) {
        return false;
      }

      @Override
      public Boolean visit(hydra.core.Type.Record rt) {
        return true;
      }

      @Override
      public Boolean visit(hydra.core.Type.Union rt) {
        return true;
      }

      @Override
      public Boolean visit(hydra.core.Type.Wrap wt) {
        return true;
      }

      @Override
      public Boolean visit(hydra.core.Type.Forall fa) {
        return hydra.Predicates.isNominalType((fa).value.body);
      }
    });
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, Boolean> isSerializable(hydra.context.Context cx, hydra.graph.Graph graph, hydra.core.Binding el) {
    java.util.function.Function<hydra.core.Type, java.util.List<hydra.variants.TypeVariant>> variants = (java.util.function.Function<hydra.core.Type, java.util.List<hydra.variants.TypeVariant>>) (typ -> hydra.lib.lists.Map.apply(
      hydra.Reflect::typeVariant,
      hydra.Rewriting.foldOverType(
        new hydra.coders.TraversalOrder.Pre(),
        (java.util.function.Function<java.util.List<hydra.core.Type>, java.util.function.Function<hydra.core.Type, java.util.List<hydra.core.Type>>>) (m -> (java.util.function.Function<hydra.core.Type, java.util.List<hydra.core.Type>>) (t -> hydra.lib.lists.Cons.apply(
          t,
          m))),
        (java.util.List<hydra.core.Type>) (java.util.Collections.<hydra.core.Type>emptyList()),
        typ)));
    return hydra.lib.eithers.Map.apply(
      (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.Type>, Boolean>) (deps -> {
        hydra.util.Lazy<java.util.Set<hydra.variants.TypeVariant>> allVariants = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
          variants,
          hydra.lib.maps.Elems.apply(deps)))));
        return hydra.lib.logic.Not.apply(hydra.lib.sets.Member.apply(
          new hydra.variants.TypeVariant.Function(),
          allVariants.get()));
      }),
      hydra.Predicates.typeDependencies(
        cx,
        graph,
        false,
        (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (hydra.lib.equality.Identity::apply),
        (el).name));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, Boolean> isSerializableByName(hydra.context.Context cx, hydra.graph.Graph graph, hydra.core.Name name) {
    java.util.function.Function<hydra.core.Type, java.util.List<hydra.variants.TypeVariant>> variants = (java.util.function.Function<hydra.core.Type, java.util.List<hydra.variants.TypeVariant>>) (typ -> hydra.lib.lists.Map.apply(
      hydra.Reflect::typeVariant,
      hydra.Rewriting.foldOverType(
        new hydra.coders.TraversalOrder.Pre(),
        (java.util.function.Function<java.util.List<hydra.core.Type>, java.util.function.Function<hydra.core.Type, java.util.List<hydra.core.Type>>>) (m -> (java.util.function.Function<hydra.core.Type, java.util.List<hydra.core.Type>>) (t -> hydra.lib.lists.Cons.apply(
          t,
          m))),
        (java.util.List<hydra.core.Type>) (java.util.Collections.<hydra.core.Type>emptyList()),
        typ)));
    return hydra.lib.eithers.Map.apply(
      (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.Type>, Boolean>) (deps -> {
        hydra.util.Lazy<java.util.Set<hydra.variants.TypeVariant>> allVariants = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
          variants,
          hydra.lib.maps.Elems.apply(deps)))));
        return hydra.lib.logic.Not.apply(hydra.lib.sets.Member.apply(
          new hydra.variants.TypeVariant.Function(),
          allVariants.get()));
      }),
      hydra.Predicates.typeDependencies(
        cx,
        graph,
        false,
        (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (hydra.lib.equality.Identity::apply),
        name));
  }

  static Boolean isSerializableType(hydra.core.Type typ) {
    hydra.util.Lazy<java.util.Set<hydra.variants.TypeVariant>> allVariants = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.lib.lists.Map.apply(
      hydra.Reflect::typeVariant,
      hydra.Rewriting.foldOverType(
        new hydra.coders.TraversalOrder.Pre(),
        (java.util.function.Function<java.util.List<hydra.core.Type>, java.util.function.Function<hydra.core.Type, java.util.List<hydra.core.Type>>>) (m -> (java.util.function.Function<hydra.core.Type, java.util.List<hydra.core.Type>>) (t -> hydra.lib.lists.Cons.apply(
          t,
          m))),
        (java.util.List<hydra.core.Type>) (java.util.Collections.<hydra.core.Type>emptyList()),
        typ))));
    return hydra.lib.logic.Not.apply(hydra.lib.sets.Member.apply(
      new hydra.variants.TypeVariant.Function(),
      allVariants.get()));
  }

  static Boolean isType(hydra.core.Type t) {
    return hydra.Strip.deannotateType(t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.core.Type instance) {
        return false;
      }

      @Override
      public Boolean visit(hydra.core.Type.Application a) {
        return hydra.Predicates.isType((a).value.function);
      }

      @Override
      public Boolean visit(hydra.core.Type.Forall l) {
        return hydra.Predicates.isType((l).value.body);
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

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.Map<hydra.core.Name, hydra.core.Type>> typeDependencies(hydra.context.Context cx, hydra.graph.Graph graph, Boolean withSchema, java.util.function.Function<hydra.core.Type, hydra.core.Type> transform, hydra.core.Name name) {
    java.util.function.Function<hydra.core.Name, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Type>> requireType = (java.util.function.Function<hydra.core.Name, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Type>>) (name2 -> {
      hydra.util.Lazy<hydra.context.Context> cx1 = new hydra.util.Lazy<>(() -> new hydra.context.Context(hydra.lib.lists.Cons.apply(
        hydra.lib.strings.Cat2.apply(
          "type dependencies of ",
          (name2).value),
        (cx).trace), (cx).messages, (cx).other));
      return hydra.lib.eithers.Bind.apply(
        hydra.Lexical.requireElement(
          cx1.get(),
          graph,
          name2),
        (java.util.function.Function<hydra.core.Binding, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Type>>) (el -> hydra.lib.eithers.Bimap.apply(
          (java.util.function.Function<hydra.errors.Error_, hydra.context.InContext<hydra.errors.Error_>>) (_wc_e -> (hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(_wc_e, cx1.get()))),
          (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (_wc_a -> _wc_a),
          hydra.lib.eithers.Bimap.apply(
            (java.util.function.Function<hydra.errors.DecodingError, hydra.errors.Error_>) (_e -> new hydra.errors.Error_.Other(new hydra.errors.OtherError((_e).value))),
            (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (_a -> _a),
            hydra.decode.Core.type(
              graph,
              (el).term)))));
    });
    java.util.function.Function<hydra.core.Name, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Pair<hydra.core.Name, hydra.core.Type>>> toPair = (java.util.function.Function<hydra.core.Name, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Pair<hydra.core.Name, hydra.core.Type>>>) (name2 -> hydra.lib.eithers.Map.apply(
      (java.util.function.Function<hydra.core.Type, hydra.util.Pair<hydra.core.Name, hydra.core.Type>>) (typ -> (hydra.util.Pair<hydra.core.Name, hydra.core.Type>) ((hydra.util.Pair<hydra.core.Name, hydra.core.Type>) (new hydra.util.Pair<hydra.core.Name, hydra.core.Type>(name2, (transform).apply(typ))))),
      (requireType).apply(name2)));
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<java.util.Set<hydra.core.Name>, java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.Type>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.Map<hydra.core.Name, hydra.core.Type>>>>> deps = new java.util.concurrent.atomic.AtomicReference<>();
    deps.set((java.util.function.Function<java.util.Set<hydra.core.Name>, java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.Type>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.Map<hydra.core.Name, hydra.core.Type>>>>) (seeds -> (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.Type>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.Map<hydra.core.Name, hydra.core.Type>>>) (names -> hydra.lib.logic.IfElse.lazy(
      hydra.lib.sets.Null.apply(seeds),
      () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, java.util.Map<hydra.core.Name, hydra.core.Type>>right(names),
      () -> hydra.lib.eithers.Bind.apply(
        hydra.lib.eithers.MapList.apply(
          toPair,
          hydra.lib.sets.ToList.apply(seeds)),
        (java.util.function.Function<java.util.List<hydra.util.Pair<hydra.core.Name, hydra.core.Type>>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.Map<hydra.core.Name, hydra.core.Type>>>) (pairs -> {
          hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.Type>> newNames = new hydra.util.Lazy<>(() -> hydra.lib.maps.Union.apply(
            names,
            hydra.lib.maps.FromList.apply(pairs)));
          hydra.util.Lazy<java.util.Set<hydra.core.Name>> refs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
            (java.util.function.Function<java.util.Set<hydra.core.Name>, java.util.function.Function<java.util.Set<hydra.core.Name>, java.util.Set<hydra.core.Name>>>) ((java.util.function.Function<java.util.Set<hydra.core.Name>, java.util.function.Function<java.util.Set<hydra.core.Name>, java.util.Set<hydra.core.Name>>>) (p0 -> p1 -> hydra.lib.sets.Union.apply(
              p0,
              p1))),
            (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
            hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.Type>, java.util.Set<hydra.core.Name>>) (pair -> hydra.Dependencies.typeDependencyNames(
                withSchema,
                hydra.lib.pairs.Second.apply(pair))),
              pairs)));
          hydra.util.Lazy<java.util.Set<hydra.core.Name>> visited = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.lib.maps.Keys.apply(names)));
          hydra.util.Lazy<java.util.Set<hydra.core.Name>> newSeeds = new hydra.util.Lazy<>(() -> hydra.lib.sets.Difference.apply(
            refs.get(),
            visited.get()));
          return deps.get().apply(newSeeds.get()).apply(newNames.get());
        }))))));
    return deps.get().apply(hydra.lib.sets.Singleton.apply(name)).apply((java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())));
  }
}
