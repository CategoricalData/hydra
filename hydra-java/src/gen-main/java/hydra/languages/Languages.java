// Note: this is an automatically generated file. Do not edit.

package hydra.languages;

/**
 * Language constraints for Hydra Core
 */
public interface Languages {
  static hydra.coders.Language hydraLanguage() {
    java.util.Set<hydra.meta.EliminationVariant> eliminationVariants = hydra.lib.sets.FromList.apply((hydra.variants.Variants.eliminationVariants));
    java.util.Set<hydra.core.FloatType> floatTypes = hydra.lib.sets.FromList.apply((hydra.variants.Variants.floatTypes));
    java.util.Set<hydra.meta.FunctionVariant> functionVariants = hydra.lib.sets.FromList.apply((hydra.variants.Variants.functionVariants));
    java.util.Set<hydra.core.IntegerType> integerTypes = hydra.lib.sets.FromList.apply((hydra.variants.Variants.integerTypes));
    java.util.Set<hydra.meta.LiteralVariant> literalVariants = hydra.lib.sets.FromList.apply((hydra.variants.Variants.literalVariants));
    java.util.Set<hydra.meta.TermVariant> termVariants = hydra.lib.sets.FromList.apply((hydra.variants.Variants.termVariants));
    java.util.Set<hydra.meta.TypeVariant> typeVariants = hydra.lib.sets.FromList.apply((hydra.variants.Variants.typeVariants));
    java.util.function.Function<hydra.core.Type, Boolean> types = (java.util.function.Function<hydra.core.Type, Boolean>) (t -> ((t)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.core.Type instance) {
        return true;
      }
    }));
    return new hydra.coders.Language(new hydra.coders.LanguageName("hydra.core"), new hydra.coders.LanguageConstraints((eliminationVariants), (literalVariants), (floatTypes), (functionVariants), (integerTypes), (termVariants), (typeVariants), (types)));
  }
}
