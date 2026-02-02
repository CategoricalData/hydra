// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.json.language;

/**
 * Language constraints for JSON
 */
public interface Language {
  static hydra.coders.Language jsonLanguage() {
    java.util.Set<hydra.core.FloatType> floatTypes = hydra.lib.sets.FromList.apply(java.util.List.of(new hydra.core.FloatType.Bigfloat(true)));
    java.util.Set<hydra.core.IntegerType> integerTypes = hydra.lib.sets.FromList.apply(java.util.List.of(new hydra.core.IntegerType.Bigint(true)));
    java.util.Set<hydra.variants.LiteralVariant> literalVariants = hydra.lib.sets.FromList.apply(java.util.List.of(
      new hydra.variants.LiteralVariant.Boolean_(true),
      new hydra.variants.LiteralVariant.Float_(true),
      new hydra.variants.LiteralVariant.Integer_(true),
      new hydra.variants.LiteralVariant.String_(true)));
    java.util.Set<hydra.variants.TermVariant> termVariants = hydra.lib.sets.FromList.apply(java.util.List.of(
      new hydra.variants.TermVariant.List(true),
      new hydra.variants.TermVariant.Literal(true),
      new hydra.variants.TermVariant.Map(true),
      new hydra.variants.TermVariant.Maybe(true),
      new hydra.variants.TermVariant.Record(true)));
    java.util.function.Function<hydra.core.Type, Boolean> typePredicate = (java.util.function.Function<hydra.core.Type, Boolean>) (typ -> (hydra.rewriting.Rewriting.deannotateType((typ))).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.core.Type instance) {
        return true;
      }
      
      @Override
      public Boolean visit(hydra.core.Type.Maybe innerType) {
        return (((innerType)).value).accept(new hydra.core.Type.PartialVisitor<>() {
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
    java.util.Set<hydra.variants.TypeVariant> typeVariants = hydra.lib.sets.FromList.apply(java.util.List.of(
      new hydra.variants.TypeVariant.List(true),
      new hydra.variants.TypeVariant.Literal(true),
      new hydra.variants.TypeVariant.Map(true),
      new hydra.variants.TypeVariant.Maybe(true),
      new hydra.variants.TypeVariant.Record(true)));
    return new hydra.coders.Language(new hydra.coders.LanguageName("hydra.ext.org.json"), new hydra.coders.LanguageConstraints(hydra.ext.org.json.language.Language.<hydra.variants.EliminationVariant>jsonLanguage_eliminationVariants(), (literalVariants), (floatTypes), hydra.ext.org.json.language.Language.<hydra.variants.FunctionVariant>jsonLanguage_functionVariants(), (integerTypes), (termVariants), (typeVariants), (typePredicate)));
  }
  
  static <T0> java.util.Set<T0> jsonLanguage_eliminationVariants() {
    return (java.util.Set<T0>) (hydra.lib.sets.Empty.<T0>apply());
  }
  
  static <T0> java.util.Set<T0> jsonLanguage_functionVariants() {
    return (java.util.Set<T0>) (hydra.lib.sets.Empty.<T0>apply());
  }
}
