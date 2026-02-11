// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.json.language;

/**
 * Language constraints for JSON
 */
public interface Language {
  static hydra.coders.Language jsonLanguage() {
    hydra.util.Lazy<java.util.Set<hydra.core.FloatType>> floatTypes = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(java.util.List.of(new hydra.core.FloatType.Bigfloat())));
    hydra.util.Lazy<java.util.Set<hydra.core.IntegerType>> integerTypes = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(java.util.List.of(new hydra.core.IntegerType.Bigint())));
    hydra.util.Lazy<java.util.Set<hydra.variants.LiteralVariant>> literalVariants = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(java.util.List.of(
      new hydra.variants.LiteralVariant.Boolean_(),
      new hydra.variants.LiteralVariant.Float_(),
      new hydra.variants.LiteralVariant.Integer_(),
      new hydra.variants.LiteralVariant.String_())));
    hydra.util.Lazy<java.util.Set<hydra.variants.TermVariant>> termVariants = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(java.util.List.of(
      new hydra.variants.TermVariant.List(),
      new hydra.variants.TermVariant.Literal(),
      new hydra.variants.TermVariant.Map(),
      new hydra.variants.TermVariant.Maybe(),
      new hydra.variants.TermVariant.Record())));
    java.util.function.Function<hydra.core.Type, Boolean> typePredicate = (java.util.function.Function<hydra.core.Type, Boolean>) (typ -> (hydra.rewriting.Rewriting.deannotateType(typ)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.core.Type instance) {
        return true;
      }
      
      @Override
      public Boolean visit(hydra.core.Type.Maybe innerType) {
        return ((innerType).value).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public Boolean otherwise(hydra.core.Type instance) {
            return true;
          }
          
          @Override
          public Boolean visit(hydra.core.Type.Maybe ignored) {
            return false;
          }
        });
      }
    }));
    hydra.util.Lazy<java.util.Set<hydra.variants.TypeVariant>> typeVariants = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(java.util.List.of(
      new hydra.variants.TypeVariant.List(),
      new hydra.variants.TypeVariant.Literal(),
      new hydra.variants.TypeVariant.Map(),
      new hydra.variants.TypeVariant.Maybe(),
      new hydra.variants.TypeVariant.Record())));
    return new hydra.coders.Language(new hydra.coders.LanguageName("hydra.ext.org.json"), new hydra.coders.LanguageConstraints(hydra.ext.org.json.language.Language.<hydra.variants.EliminationVariant>jsonLanguage_eliminationVariants(), literalVariants.get(), floatTypes.get(), hydra.ext.org.json.language.Language.<hydra.variants.FunctionVariant>jsonLanguage_functionVariants(), integerTypes.get(), termVariants.get(), typeVariants.get(), typePredicate));
  }
  
  static <T0> java.util.Set<T0> jsonLanguage_eliminationVariants() {
    return (java.util.Set<T0>) (hydra.lib.sets.Empty.<T0>apply());
  }
  
  static <T0> java.util.Set<T0> jsonLanguage_functionVariants() {
    return (java.util.Set<T0>) (hydra.lib.sets.Empty.<T0>apply());
  }
}
