// Note: this is an automatically generated file. Do not edit.

package hydra.scala;

/**
 * Language constraints and reserved words for Scala
 */
public interface Language {
  static hydra.coders.Language scalaLanguage() {
    hydra.util.Lazy<java.util.Set<hydra.variants.EliminationVariant>> eliminationVariants = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(java.util.Arrays.asList(
      new hydra.variants.EliminationVariant.Record(),
      new hydra.variants.EliminationVariant.Union(),
      new hydra.variants.EliminationVariant.Wrap())));
    hydra.util.Lazy<java.util.Set<hydra.core.FloatType>> floatTypes = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(java.util.Arrays.asList(
      new hydra.core.FloatType.Bigfloat(),
      new hydra.core.FloatType.Float32(),
      new hydra.core.FloatType.Float64())));
    hydra.util.Lazy<java.util.Set<hydra.variants.FunctionVariant>> functionVariants = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(java.util.Arrays.asList(
      new hydra.variants.FunctionVariant.Elimination(),
      new hydra.variants.FunctionVariant.Lambda())));
    hydra.util.Lazy<java.util.Set<hydra.core.IntegerType>> integerTypes = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(java.util.Arrays.asList(
      new hydra.core.IntegerType.Bigint(),
      new hydra.core.IntegerType.Int8(),
      new hydra.core.IntegerType.Int16(),
      new hydra.core.IntegerType.Int32(),
      new hydra.core.IntegerType.Int64(),
      new hydra.core.IntegerType.Uint8(),
      new hydra.core.IntegerType.Uint16(),
      new hydra.core.IntegerType.Uint32(),
      new hydra.core.IntegerType.Uint64())));
    hydra.util.Lazy<java.util.Set<hydra.variants.LiteralVariant>> literalVariants = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(java.util.Arrays.asList(
      new hydra.variants.LiteralVariant.Boolean_(),
      new hydra.variants.LiteralVariant.Float_(),
      new hydra.variants.LiteralVariant.Integer_(),
      new hydra.variants.LiteralVariant.String_())));
    hydra.util.Lazy<java.util.Set<hydra.variants.TermVariant>> termVariants = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(java.util.Arrays.asList(
      new hydra.variants.TermVariant.Application(),
      new hydra.variants.TermVariant.Either(),
      new hydra.variants.TermVariant.Cases(),
      new hydra.variants.TermVariant.Lambda(),
      new hydra.variants.TermVariant.Project(),
      new hydra.variants.TermVariant.Unwrap(),
      new hydra.variants.TermVariant.TypeApplication(),
      new hydra.variants.TermVariant.TypeLambda(),
      new hydra.variants.TermVariant.Let(),
      new hydra.variants.TermVariant.List(),
      new hydra.variants.TermVariant.Literal(),
      new hydra.variants.TermVariant.Map(),
      new hydra.variants.TermVariant.Maybe(),
      new hydra.variants.TermVariant.Pair(),
      new hydra.variants.TermVariant.Record(),
      new hydra.variants.TermVariant.Set(),
      new hydra.variants.TermVariant.Inject(),
      new hydra.variants.TermVariant.Unit(),
      new hydra.variants.TermVariant.Variable(),
      new hydra.variants.TermVariant.Wrap())));
    hydra.util.Lazy<java.util.Set<hydra.variants.TypeVariant>> typeVariants = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(java.util.Arrays.asList(
      new hydra.variants.TypeVariant.Annotated(),
      new hydra.variants.TypeVariant.Application(),
      new hydra.variants.TypeVariant.Either(),
      new hydra.variants.TypeVariant.Function(),
      new hydra.variants.TypeVariant.List(),
      new hydra.variants.TypeVariant.Literal(),
      new hydra.variants.TypeVariant.Map(),
      new hydra.variants.TypeVariant.Maybe(),
      new hydra.variants.TypeVariant.Pair(),
      new hydra.variants.TypeVariant.Record(),
      new hydra.variants.TypeVariant.Set(),
      new hydra.variants.TypeVariant.Union(),
      new hydra.variants.TypeVariant.Unit(),
      new hydra.variants.TypeVariant.Forall(),
      new hydra.variants.TypeVariant.Variable(),
      new hydra.variants.TypeVariant.Void_(),
      new hydra.variants.TypeVariant.Wrap())));
    return new hydra.coders.Language(new hydra.coders.LanguageName("hydra.scala"), new hydra.coders.LanguageConstraints(eliminationVariants.get(), literalVariants.get(), floatTypes.get(), functionVariants.get(), integerTypes.get(), termVariants.get(), typeVariants.get(), p0 -> hydra.scala.Language.<hydra.core.Type>scalaLanguage_typePredicate(p0)));
  }

  static <T0> Boolean scalaLanguage_typePredicate(T0 ignored) {
    return true;
  }

  static java.util.Set<String> scalaReservedWords() {
    java.util.List<String> classNames = java.util.Arrays.asList(
      "Any",
      "AnyVal",
      "App",
      "Array",
      "Boolean",
      "Byte",
      "Char",
      "Console",
      "DelayedInit",
      "Double",
      "DummyExplicit",
      "Dynamic",
      "Enumeration",
      "Equals",
      "Float",
      "Function",
      "Int",
      "Long",
      "MatchError",
      "None",
      "Nothing",
      "Null",
      "Option",
      "PartialFunction",
      "Predef",
      "Product",
      "Proxy",
      "SerialVersionUID",
      "Short",
      "Singleton",
      "Some",
      "Specializable",
      "StringContext",
      "Symbol",
      "Unit",
      "ValueOf");
    java.util.List<String> keywords = java.util.Arrays.asList(
      "abstract",
      "case",
      "catch",
      "class",
      "def",
      "do",
      "else",
      "end",
      "enum",
      "export",
      "extends",
      "false",
      "final",
      "finally",
      "for",
      "forSome",
      "given",
      "if",
      "implicit",
      "import",
      "lazy",
      "macro",
      "match",
      "new",
      "null",
      "object",
      "override",
      "package",
      "private",
      "protected",
      "return",
      "sealed",
      "super",
      "then",
      "this",
      "throw",
      "trait",
      "true",
      "try",
      "type",
      "val",
      "var",
      "while",
      "with",
      "yield");
    return hydra.lib.sets.FromList.apply(hydra.lib.lists.Concat.apply(java.util.Arrays.asList(
      keywords,
      classNames,
      hydra.scala.Language.<String>scalaReservedWords_hydraScalaKeywords())));
  }

  static <T0> java.util.List<T0> scalaReservedWords_hydraScalaKeywords() {
    return (java.util.List<T0>) (java.util.Collections.<T0>emptyList());
  }
}
