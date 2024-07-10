// Note: this is an automatically generated file. Do not edit.

package hydra.coreLanguage;

/**
 * Language constraints for Hydra Core
 */
public interface CoreLanguage {
  static <A> hydra.coders.Language<A> hydraCoreLanguage() {
    return new hydra.coders.Language(new hydra.coders.LanguageName("hydra/core"), new hydra.coders.LanguageConstraints(hydra.lib.sets.FromList.apply((hydra.basics.Basics.eliminationVariants)), hydra.lib.sets.FromList.apply((hydra.basics.Basics.literalVariants)), hydra.lib.sets.FromList.apply((hydra.basics.Basics.floatTypes)), hydra.lib.sets.FromList.apply((hydra.basics.Basics.functionVariants)), hydra.lib.sets.FromList.apply((hydra.basics.Basics.integerTypes)), hydra.lib.sets.FromList.apply((hydra.basics.Basics.termVariants)), hydra.lib.sets.FromList.apply((hydra.basics.Basics.typeVariants)), (java.util.function.Function<hydra.core.Type<A>, Boolean>) (ignored -> true)));
  }
}