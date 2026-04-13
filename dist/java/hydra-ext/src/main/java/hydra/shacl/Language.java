// Note: this is an automatically generated file. Do not edit.

package hydra.shacl;

/**
 * Language constraints for W3C SHACL
 */
public interface Language {
  static hydra.coders.Language shaclLanguage() {
    hydra.util.Lazy<java.util.Set<hydra.core.FloatType>> floatTypes = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(java.util.Arrays.asList(
      new hydra.core.FloatType.Float32(),
      new hydra.core.FloatType.Float64())));
    hydra.util.Lazy<java.util.Set<hydra.core.IntegerType>> integerTypes = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(java.util.Arrays.asList(
      new hydra.core.IntegerType.Int32(),
      new hydra.core.IntegerType.Int64())));
    hydra.util.Lazy<java.util.Set<hydra.variants.LiteralVariant>> literalVariants = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(java.util.Arrays.asList(
      new hydra.variants.LiteralVariant.Boolean_(),
      new hydra.variants.LiteralVariant.Float_(),
      new hydra.variants.LiteralVariant.Integer_(),
      new hydra.variants.LiteralVariant.String_())));
    hydra.util.Lazy<java.util.Set<hydra.variants.TermVariant>> termVariants = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(java.util.Arrays.asList(
      new hydra.variants.TermVariant.List(),
      new hydra.variants.TermVariant.Literal(),
      new hydra.variants.TermVariant.Map(),
      new hydra.variants.TermVariant.Wrap(),
      new hydra.variants.TermVariant.Maybe(),
      new hydra.variants.TermVariant.Record(),
      new hydra.variants.TermVariant.Set(),
      new hydra.variants.TermVariant.Union())));
    hydra.util.Lazy<java.util.Set<hydra.variants.TypeVariant>> typeVariants = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(java.util.Arrays.asList(
      new hydra.variants.TypeVariant.Annotated(),
      new hydra.variants.TypeVariant.List(),
      new hydra.variants.TypeVariant.Literal(),
      new hydra.variants.TypeVariant.Map(),
      new hydra.variants.TypeVariant.Wrap(),
      new hydra.variants.TypeVariant.Maybe(),
      new hydra.variants.TypeVariant.Record(),
      new hydra.variants.TypeVariant.Set(),
      new hydra.variants.TypeVariant.Union())));
    return new hydra.coders.Language(new hydra.coders.LanguageName("hydra.shacl"), new hydra.coders.LanguageConstraints(hydra.shacl.Language.<hydra.variants.EliminationVariant>shaclLanguage_eliminationVariants(), literalVariants.get(), floatTypes.get(), hydra.shacl.Language.<hydra.variants.FunctionVariant>shaclLanguage_functionVariants(), integerTypes.get(), termVariants.get(), typeVariants.get(), p0 -> hydra.shacl.Language.<hydra.core.Type>shaclLanguage_typePredicate(p0)));
  }

  static <T0> java.util.Set<T0> shaclLanguage_eliminationVariants() {
    return (java.util.Set<T0>) (hydra.lib.sets.Empty.<T0>apply());
  }

  static <T0> java.util.Set<T0> shaclLanguage_functionVariants() {
    return (java.util.Set<T0>) (hydra.lib.sets.Empty.<T0>apply());
  }

  static <T0> Boolean shaclLanguage_typePredicate(T0 ignored) {
    return true;
  }
}
