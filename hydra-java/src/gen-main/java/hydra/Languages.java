// Note: this is an automatically generated file. Do not edit.

package hydra;

/**
 * Language constraints for Hydra Core
 */
public interface Languages {
  static hydra.coders.Language hydraLanguage() {
    hydra.util.Lazy<hydra.util.PersistentSet<hydra.variants.EliminationVariant>> eliminationVariants = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.Reflect.eliminationVariants()));
    hydra.util.Lazy<hydra.util.PersistentSet<hydra.core.FloatType>> floatTypes = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.Reflect.floatTypes()));
    hydra.util.Lazy<hydra.util.PersistentSet<hydra.variants.FunctionVariant>> functionVariants = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.Reflect.functionVariants()));
    hydra.util.Lazy<hydra.util.PersistentSet<hydra.core.IntegerType>> integerTypes = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.Reflect.integerTypes()));
    hydra.util.Lazy<hydra.util.PersistentSet<hydra.variants.LiteralVariant>> literalVariants = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.Reflect.literalVariants()));
    hydra.util.Lazy<hydra.util.PersistentSet<hydra.variants.TermVariant>> termVariants = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.Reflect.termVariants()));
    hydra.util.Lazy<hydra.util.PersistentSet<hydra.variants.TypeVariant>> typeVariants = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.Reflect.typeVariants()));
    java.util.function.Function<hydra.core.Type, Boolean> types = (java.util.function.Function<hydra.core.Type, Boolean>) (t -> (t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.core.Type instance) {
        return true;
      }
    }));
    return new hydra.coders.Language(new hydra.coders.LanguageName("hydra.core"), new hydra.coders.LanguageConstraints(eliminationVariants.get(), literalVariants.get(), floatTypes.get(), functionVariants.get(), integerTypes.get(), termVariants.get(), typeVariants.get(), types));
  }
}
