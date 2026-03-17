// Note: this is an automatically generated file. Do not edit.

package hydra.ext.shacl.language;

/**
 * Language constraints for W3C SHACL
 */
public interface Language {
  static hydra.coders.Language shaclLanguage() {
    hydra.util.Lazy<hydra.util.PersistentSet<hydra.core.FloatType>> floatTypes = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.util.ConsList.of(
      new hydra.core.FloatType.Float32(),
      new hydra.core.FloatType.Float64())));
    hydra.util.Lazy<hydra.util.PersistentSet<hydra.core.IntegerType>> integerTypes = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.util.ConsList.of(
      new hydra.core.IntegerType.Int32(),
      new hydra.core.IntegerType.Int64())));
    hydra.util.Lazy<hydra.util.PersistentSet<hydra.variants.LiteralVariant>> literalVariants = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.util.ConsList.of(
      new hydra.variants.LiteralVariant.Boolean_(),
      new hydra.variants.LiteralVariant.Float_(),
      new hydra.variants.LiteralVariant.Integer_(),
      new hydra.variants.LiteralVariant.String_())));
    hydra.util.Lazy<hydra.util.PersistentSet<hydra.variants.TermVariant>> termVariants = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.util.ConsList.of(
      new hydra.variants.TermVariant.List(),
      new hydra.variants.TermVariant.Literal(),
      new hydra.variants.TermVariant.Map(),
      new hydra.variants.TermVariant.Wrap(),
      new hydra.variants.TermVariant.Maybe(),
      new hydra.variants.TermVariant.Record(),
      new hydra.variants.TermVariant.Set(),
      new hydra.variants.TermVariant.Union())));
    hydra.util.Lazy<hydra.util.PersistentSet<hydra.variants.TypeVariant>> typeVariants = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.util.ConsList.of(
      new hydra.variants.TypeVariant.Annotated(),
      new hydra.variants.TypeVariant.List(),
      new hydra.variants.TypeVariant.Literal(),
      new hydra.variants.TypeVariant.Map(),
      new hydra.variants.TypeVariant.Wrap(),
      new hydra.variants.TypeVariant.Maybe(),
      new hydra.variants.TypeVariant.Record(),
      new hydra.variants.TypeVariant.Set(),
      new hydra.variants.TypeVariant.Union())));
    return new hydra.coders.Language(new hydra.coders.LanguageName("hydra.ext.shacl"), new hydra.coders.LanguageConstraints(hydra.ext.shacl.language.Language.<hydra.variants.EliminationVariant>shaclLanguage_eliminationVariants(), literalVariants.get(), floatTypes.get(), hydra.ext.shacl.language.Language.<hydra.variants.FunctionVariant>shaclLanguage_functionVariants(), integerTypes.get(), termVariants.get(), typeVariants.get(), p0 -> hydra.ext.shacl.language.Language.<hydra.core.Type>shaclLanguage_typePredicate(p0)));
  }

  static <T0> hydra.util.PersistentSet<T0> shaclLanguage_eliminationVariants() {
    return (hydra.util.PersistentSet<T0>) (hydra.lib.sets.Empty.<T0>apply());
  }

  static <T0> hydra.util.PersistentSet<T0> shaclLanguage_functionVariants() {
    return (hydra.util.PersistentSet<T0>) (hydra.lib.sets.Empty.<T0>apply());
  }

  static <T0> Boolean shaclLanguage_typePredicate(T0 ignored) {
    return true;
  }
}
