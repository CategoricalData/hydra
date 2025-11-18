// Note: this is an automatically generated file. Do not edit.

package hydra.languages;

/**
 * Language constraints for Hydra Core
 */
public interface Languages {
  static hydra.coders.Language hydraLanguage() {
    java.util.Set<hydra.variants.EliminationVariant> eliminationVariants = hydra.lib.sets.FromList.apply((hydra.reflect.Reflect.eliminationVariants));
    java.util.Set<hydra.core.FloatType> floatTypes = hydra.lib.sets.FromList.apply((hydra.reflect.Reflect.floatTypes));
    java.util.Set<hydra.variants.FunctionVariant> functionVariants = hydra.lib.sets.FromList.apply((hydra.reflect.Reflect.functionVariants));
    java.util.Set<hydra.core.IntegerType> integerTypes = hydra.lib.sets.FromList.apply((hydra.reflect.Reflect.integerTypes));
    java.util.Set<hydra.variants.LiteralVariant> literalVariants = hydra.lib.sets.FromList.apply((hydra.reflect.Reflect.literalVariants));
    java.util.Set<hydra.variants.TermVariant> termVariants = hydra.lib.sets.FromList.apply((hydra.reflect.Reflect.termVariants));
    java.util.Set<hydra.variants.TypeVariant> typeVariants = hydra.lib.sets.FromList.apply((hydra.reflect.Reflect.typeVariants));
    java.util.function.Function<hydra.core.Type, Boolean> types = (java.util.function.Function<hydra.core.Type, Boolean>) (t -> ((t)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.core.Type instance) {
        return true;
      }
    }));
    return new hydra.coders.Language(new hydra.coders.LanguageName("hydra.core"), new hydra.coders.LanguageConstraints((eliminationVariants), (literalVariants), (floatTypes), (functionVariants), (integerTypes), (termVariants), (typeVariants), (types)));
  }
}
