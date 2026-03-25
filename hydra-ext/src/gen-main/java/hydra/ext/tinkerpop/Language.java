// Note: this is an automatically generated file. Do not edit.

package hydra.ext.tinkerpop;

/**
 * Language constraints based on TinkerPop Graph.Features
 */
public interface Language {
  static <T0> hydra.coders.Language tinkerpopLanguage(hydra.coders.LanguageName name, hydra.ext.org.apache.tinkerpop.features.Features features, hydra.ext.org.apache.tinkerpop.features.ExtraFeatures<T0> extras) {
    hydra.ext.org.apache.tinkerpop.features.DataTypeFeatures vpFeatures = (features).vertex.properties.dataTypeFeatures;
    hydra.util.Lazy<hydra.util.PersistentSet<hydra.core.FloatType>> floatTypes = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.lib.maybes.Cat.apply(hydra.util.ConsList.of(
      hydra.ext.tinkerpop.Language.tinkerpopLanguage_cond(
        new hydra.core.FloatType.Float32(),
        (vpFeatures).supportsFloatValues),
      hydra.ext.tinkerpop.Language.tinkerpopLanguage_cond(
        new hydra.core.FloatType.Float64(),
        (vpFeatures).supportsDoubleValues)))));
    hydra.util.Lazy<hydra.util.PersistentSet<hydra.core.IntegerType>> integerTypes = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.lib.maybes.Cat.apply(hydra.util.ConsList.of(
      hydra.ext.tinkerpop.Language.tinkerpopLanguage_cond(
        new hydra.core.IntegerType.Int32(),
        (vpFeatures).supportsIntegerValues),
      hydra.ext.tinkerpop.Language.tinkerpopLanguage_cond(
        new hydra.core.IntegerType.Int64(),
        (vpFeatures).supportsLongValues)))));
    hydra.util.Lazy<hydra.util.PersistentSet<hydra.variants.LiteralVariant>> literalVariants = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.lib.maybes.Cat.apply(hydra.util.ConsList.of(
      hydra.ext.tinkerpop.Language.tinkerpopLanguage_cond(
        new hydra.variants.LiteralVariant.Binary(),
        (vpFeatures).supportsByteArrayValues),
      hydra.ext.tinkerpop.Language.tinkerpopLanguage_cond(
        new hydra.variants.LiteralVariant.Boolean_(),
        (vpFeatures).supportsBooleanValues),
      hydra.ext.tinkerpop.Language.tinkerpopLanguage_cond(
        new hydra.variants.LiteralVariant.Float_(),
        hydra.lib.logic.Or.apply(
          (vpFeatures).supportsFloatValues,
          (vpFeatures).supportsDoubleValues)),
      hydra.ext.tinkerpop.Language.tinkerpopLanguage_cond(
        new hydra.variants.LiteralVariant.Integer_(),
        hydra.lib.logic.Or.apply(
          (vpFeatures).supportsIntegerValues,
          (vpFeatures).supportsLongValues)),
      hydra.ext.tinkerpop.Language.tinkerpopLanguage_cond(
        new hydra.variants.LiteralVariant.String_(),
        (vpFeatures).supportsStringValues)))));
    Boolean supportsLists = hydra.lib.logic.Or.apply(
      (vpFeatures).supportsBooleanArrayValues,
      hydra.lib.logic.Or.apply(
        (vpFeatures).supportsByteArrayValues,
        hydra.lib.logic.Or.apply(
          (vpFeatures).supportsDoubleArrayValues,
          hydra.lib.logic.Or.apply(
            (vpFeatures).supportsFloatArrayValues,
            hydra.lib.logic.Or.apply(
              (vpFeatures).supportsIntegerArrayValues,
              hydra.lib.logic.Or.apply(
                (vpFeatures).supportsLongArrayValues,
                (vpFeatures).supportsStringArrayValues))))));
    Boolean supportsLiterals = true;
    Boolean supportsMaps = (vpFeatures).supportsMapValues;
    hydra.util.Lazy<hydra.util.PersistentSet<hydra.variants.TermVariant>> termVariants = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.lib.maybes.Cat.apply(hydra.util.ConsList.of(
      hydra.ext.tinkerpop.Language.tinkerpopLanguage_cond(
        new hydra.variants.TermVariant.List(),
        supportsLists),
      hydra.ext.tinkerpop.Language.tinkerpopLanguage_cond(
        new hydra.variants.TermVariant.Literal(),
        supportsLiterals),
      hydra.ext.tinkerpop.Language.tinkerpopLanguage_cond(
        new hydra.variants.TermVariant.Map(),
        supportsMaps),
      hydra.lib.maybes.Pure.apply(new hydra.variants.TermVariant.Maybe())))));
    java.util.function.Function<hydra.core.Type, Boolean> typePredicate = (java.util.function.Function<hydra.core.Type, Boolean>) (typ -> {
      hydra.core.Type dt = hydra.Rewriting.deannotateType(typ);
      return (dt).accept(new hydra.core.Type.PartialVisitor<>() {
        @Override
        public hydra.coders.Language otherwise(hydra.core.Type instance) {
          return true;
        }

        @Override
        public hydra.coders.Language visit(hydra.core.Type.List t) {
          return hydra.Rewriting.deannotateType((t).value).accept(new hydra.core.Type.PartialVisitor<>() {
            @Override
            public hydra.coders.Language otherwise(hydra.core.Type instance) {
              return false;
            }

            @Override
            public hydra.coders.Language visit(hydra.core.Type.Literal lt) {
              return (lt).value.accept(new hydra.core.LiteralType.PartialVisitor<>() {
                @Override
                public hydra.coders.Language otherwise(hydra.core.LiteralType instance) {
                  return false;
                }

                @Override
                public hydra.coders.Language visit(hydra.core.LiteralType.Boolean_ ignored) {
                  return (vpFeatures).supportsBooleanArrayValues;
                }

                @Override
                public hydra.coders.Language visit(hydra.core.LiteralType.Float_ ft) {
                  return (ft).value.accept(new hydra.core.FloatType.PartialVisitor<>() {
                    @Override
                    public hydra.coders.Language otherwise(hydra.core.FloatType instance) {
                      return false;
                    }

                    @Override
                    public hydra.coders.Language visit(hydra.core.FloatType.Float64 ignored) {
                      return (vpFeatures).supportsDoubleArrayValues;
                    }

                    @Override
                    public hydra.coders.Language visit(hydra.core.FloatType.Float32 ignored) {
                      return (vpFeatures).supportsFloatArrayValues;
                    }
                  });
                }

                @Override
                public hydra.coders.Language visit(hydra.core.LiteralType.Integer_ it) {
                  return (it).value.accept(new hydra.core.IntegerType.PartialVisitor<>() {
                    @Override
                    public hydra.coders.Language otherwise(hydra.core.IntegerType instance) {
                      return false;
                    }

                    @Override
                    public hydra.coders.Language visit(hydra.core.IntegerType.Uint8 ignored) {
                      return (vpFeatures).supportsByteArrayValues;
                    }

                    @Override
                    public hydra.coders.Language visit(hydra.core.IntegerType.Int32 ignored) {
                      return (vpFeatures).supportsIntegerArrayValues;
                    }

                    @Override
                    public hydra.coders.Language visit(hydra.core.IntegerType.Int64 ignored) {
                      return (vpFeatures).supportsLongArrayValues;
                    }
                  });
                }

                @Override
                public hydra.coders.Language visit(hydra.core.LiteralType.String_ ignored) {
                  return (vpFeatures).supportsStringArrayValues;
                }
              });
            }
          });
        }

        @Override
        public hydra.coders.Language visit(hydra.core.Type.Literal ignored) {
          return true;
        }

        @Override
        public hydra.coders.Language visit(hydra.core.Type.Map mt) {
          return ((java.util.function.Function<hydra.ext.org.apache.tinkerpop.features.ExtraFeatures<T0>, java.util.function.Function<hydra.core.Type, Boolean>>) (projected -> projected.supportsMapKey)).apply(extras).apply((mt).value.keys);
        }

        @Override
        public hydra.coders.Language visit(hydra.core.Type.Wrap ignored) {
          return true;
        }

        @Override
        public hydra.coders.Language visit(hydra.core.Type.Maybe ot) {
          return hydra.Rewriting.deannotateType((ot).value).accept(new hydra.core.Type.PartialVisitor<>() {
            @Override
            public hydra.coders.Language otherwise(hydra.core.Type instance) {
              return false;
            }

            @Override
            public hydra.coders.Language visit(hydra.core.Type.Literal ignored) {
              return true;
            }
          });
        }
      });
    });
    hydra.util.Lazy<hydra.util.PersistentSet<hydra.variants.TypeVariant>> typeVariants = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.lib.maybes.Cat.apply(hydra.util.ConsList.of(
      hydra.ext.tinkerpop.Language.tinkerpopLanguage_cond(
        new hydra.variants.TypeVariant.List(),
        supportsLists),
      hydra.ext.tinkerpop.Language.tinkerpopLanguage_cond(
        new hydra.variants.TypeVariant.Literal(),
        supportsLiterals),
      hydra.ext.tinkerpop.Language.tinkerpopLanguage_cond(
        new hydra.variants.TypeVariant.Map(),
        supportsMaps),
      hydra.lib.maybes.Pure.apply(new hydra.variants.TypeVariant.Maybe()),
      hydra.lib.maybes.Pure.apply(new hydra.variants.TypeVariant.Wrap())))));
    return new hydra.coders.Language(name, new hydra.coders.LanguageConstraints(hydra.ext.tinkerpop.Language.<hydra.variants.EliminationVariant>tinkerpopLanguage_eliminationVariants(), literalVariants.get(), floatTypes.get(), hydra.ext.tinkerpop.Language.<hydra.variants.FunctionVariant>tinkerpopLanguage_functionVariants(), integerTypes.get(), termVariants.get(), typeVariants.get(), typePredicate));
  }

  static <T1> hydra.util.Maybe<T1> tinkerpopLanguage_cond(T1 v, Boolean b) {
    return hydra.lib.logic.IfElse.lazy(
      b,
      () -> hydra.lib.maybes.Pure.apply(v),
      () -> (hydra.util.Maybe<T1>) (hydra.util.Maybe.<T1>nothing()));
  }

  static <T1> hydra.util.PersistentSet<T1> tinkerpopLanguage_eliminationVariants() {
    return (hydra.util.PersistentSet<T1>) (hydra.lib.sets.Empty.<T1>apply());
  }

  static <T1> hydra.util.PersistentSet<T1> tinkerpopLanguage_functionVariants() {
    return (hydra.util.PersistentSet<T1>) (hydra.lib.sets.Empty.<T1>apply());
  }
}
