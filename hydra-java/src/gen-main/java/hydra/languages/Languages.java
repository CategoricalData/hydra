// Note: this is an automatically generated file. Do not edit.

package hydra.languages;

/**
 * Language constraints for Hydra Core
 */
public interface Languages {
  static hydra.coders.Language hydraLanguage() {
    hydra.util.Lazy<java.util.Set<hydra.variants.EliminationVariant>> eliminationVariants = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.reflect.Reflect.eliminationVariants()));
    hydra.util.Lazy<java.util.Set<hydra.core.FloatType>> floatTypes = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.reflect.Reflect.floatTypes()));
    hydra.util.Lazy<java.util.Set<hydra.variants.FunctionVariant>> functionVariants = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.reflect.Reflect.functionVariants()));
    hydra.util.Lazy<java.util.Set<hydra.core.IntegerType>> integerTypes = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.reflect.Reflect.integerTypes()));
    hydra.util.Lazy<java.util.Set<hydra.variants.LiteralVariant>> literalVariants = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.reflect.Reflect.literalVariants()));
    hydra.util.Lazy<java.util.Set<hydra.variants.TermVariant>> termVariants = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.reflect.Reflect.termVariants()));
    hydra.util.Lazy<java.util.Set<hydra.variants.TypeVariant>> typeVariants = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.reflect.Reflect.typeVariants()));
    java.util.function.Function<hydra.core.Type, Boolean> types = (java.util.function.Function<hydra.core.Type, Boolean>) (t -> ((t)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.core.Type instance) {
        return true;
      }
    }));
    return new hydra.coders.Language(new hydra.coders.LanguageName("hydra.core"), new hydra.coders.LanguageConstraints(eliminationVariants.get(), literalVariants.get(), floatTypes.get(), functionVariants.get(), integerTypes.get(), termVariants.get(), typeVariants.get(), (types)));
  }
}
