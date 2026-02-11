// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.language;

/**
 * Language constraints and reserved words for Haskell
 */
public interface Language {
  static hydra.coders.Language haskellLanguage() {
    hydra.util.Lazy<java.util.Set<hydra.variants.EliminationVariant>> eliminationVariants = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(java.util.List.of(
      new hydra.variants.EliminationVariant.Record(),
      new hydra.variants.EliminationVariant.Union(),
      new hydra.variants.EliminationVariant.Wrap())));
    hydra.util.Lazy<java.util.Set<hydra.core.FloatType>> floatTypes = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(java.util.List.of(
      new hydra.core.FloatType.Float32(),
      new hydra.core.FloatType.Float64())));
    hydra.util.Lazy<java.util.Set<hydra.variants.FunctionVariant>> functionVariants = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(java.util.List.of(
      new hydra.variants.FunctionVariant.Elimination(),
      new hydra.variants.FunctionVariant.Lambda(),
      new hydra.variants.FunctionVariant.Primitive())));
    hydra.util.Lazy<java.util.Set<hydra.core.IntegerType>> integerTypes = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(java.util.List.of(
      new hydra.core.IntegerType.Bigint(),
      new hydra.core.IntegerType.Int8(),
      new hydra.core.IntegerType.Int16(),
      new hydra.core.IntegerType.Int32(),
      new hydra.core.IntegerType.Int64())));
    hydra.util.Lazy<java.util.Set<hydra.variants.LiteralVariant>> literalVariants = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(java.util.List.of(
      new hydra.variants.LiteralVariant.Binary(),
      new hydra.variants.LiteralVariant.Boolean_(),
      new hydra.variants.LiteralVariant.Float_(),
      new hydra.variants.LiteralVariant.Integer_(),
      new hydra.variants.LiteralVariant.String_())));
    hydra.util.Lazy<java.util.Set<hydra.variants.TermVariant>> termVariants = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(java.util.List.of(
      new hydra.variants.TermVariant.Annotated(),
      new hydra.variants.TermVariant.Application(),
      new hydra.variants.TermVariant.Either(),
      new hydra.variants.TermVariant.Function(),
      new hydra.variants.TermVariant.Let(),
      new hydra.variants.TermVariant.List(),
      new hydra.variants.TermVariant.Literal(),
      new hydra.variants.TermVariant.Map(),
      new hydra.variants.TermVariant.Maybe(),
      new hydra.variants.TermVariant.Pair(),
      new hydra.variants.TermVariant.Record(),
      new hydra.variants.TermVariant.Set(),
      new hydra.variants.TermVariant.Union(),
      new hydra.variants.TermVariant.Unit(),
      new hydra.variants.TermVariant.Variable(),
      new hydra.variants.TermVariant.Wrap())));
    hydra.util.Lazy<java.util.Set<hydra.variants.TypeVariant>> typeVariants = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(java.util.List.of(
      new hydra.variants.TypeVariant.Annotated(),
      new hydra.variants.TypeVariant.Application(),
      new hydra.variants.TypeVariant.Either(),
      new hydra.variants.TypeVariant.Function(),
      new hydra.variants.TypeVariant.Forall(),
      new hydra.variants.TypeVariant.List(),
      new hydra.variants.TypeVariant.Literal(),
      new hydra.variants.TypeVariant.Map(),
      new hydra.variants.TypeVariant.Maybe(),
      new hydra.variants.TypeVariant.Pair(),
      new hydra.variants.TypeVariant.Record(),
      new hydra.variants.TypeVariant.Set(),
      new hydra.variants.TypeVariant.Union(),
      new hydra.variants.TypeVariant.Unit(),
      new hydra.variants.TypeVariant.Variable(),
      new hydra.variants.TypeVariant.Wrap())));
    return new hydra.coders.Language(new hydra.coders.LanguageName("hydra.ext.haskell"), new hydra.coders.LanguageConstraints(eliminationVariants.get(), literalVariants.get(), floatTypes.get(), functionVariants.get(), integerTypes.get(), termVariants.get(), typeVariants.get(), p0 -> hydra.ext.haskell.language.Language.<hydra.core.Type>haskellLanguage_typePredicate(p0)));
  }
  
  static <T0> Boolean haskellLanguage_typePredicate(T0 ignored) {
    return true;
  }
  
  static java.util.Set<String> reservedWords() {
    java.util.List<String> keywordSymbols = java.util.List.of(
      "case",
      "class",
      "data",
      "default",
      "deriving",
      "do",
      "else",
      "forall",
      "foreign",
      "if",
      "import",
      "in",
      "infix",
      "infixl",
      "infixr",
      "instance",
      "let",
      "module",
      "newtype",
      "of",
      "then",
      "type",
      "where");
    java.util.List<String> reservedSymbols = java.util.List.of(
      "Bool",
      "Double",
      "False",
      "Float",
      "Int",
      "Integer",
      "Just",
      "Maybe",
      "Nothing",
      "Ord",
      "Show",
      "String",
      "True");
    return hydra.lib.sets.FromList.apply(hydra.lib.lists.Concat2.apply(
      keywordSymbols,
      reservedSymbols));
  }
}
