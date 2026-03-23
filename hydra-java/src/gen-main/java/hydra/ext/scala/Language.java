// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala;

/**
 * Language constraints and reserved words for Scala
 */
public interface Language {
  static hydra.coders.Language scalaLanguage() {
    hydra.util.Lazy<hydra.util.PersistentSet<hydra.variants.EliminationVariant>> eliminationVariants = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.util.ConsList.of(
      new hydra.variants.EliminationVariant.Record(),
      new hydra.variants.EliminationVariant.Union(),
      new hydra.variants.EliminationVariant.Wrap())));
    hydra.util.Lazy<hydra.util.PersistentSet<hydra.core.FloatType>> floatTypes = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.util.ConsList.of(
      new hydra.core.FloatType.Bigfloat(),
      new hydra.core.FloatType.Float32(),
      new hydra.core.FloatType.Float64())));
    hydra.util.Lazy<hydra.util.PersistentSet<hydra.variants.FunctionVariant>> functionVariants = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.util.ConsList.of(
      new hydra.variants.FunctionVariant.Elimination(),
      new hydra.variants.FunctionVariant.Lambda(),
      new hydra.variants.FunctionVariant.Primitive())));
    hydra.util.Lazy<hydra.util.PersistentSet<hydra.core.IntegerType>> integerTypes = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.util.ConsList.of(
      new hydra.core.IntegerType.Bigint(),
      new hydra.core.IntegerType.Int8(),
      new hydra.core.IntegerType.Int16(),
      new hydra.core.IntegerType.Int32(),
      new hydra.core.IntegerType.Int64(),
      new hydra.core.IntegerType.Uint8(),
      new hydra.core.IntegerType.Uint16(),
      new hydra.core.IntegerType.Uint32(),
      new hydra.core.IntegerType.Uint64())));
    hydra.util.Lazy<hydra.util.PersistentSet<hydra.variants.LiteralVariant>> literalVariants = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.util.ConsList.of(
      new hydra.variants.LiteralVariant.Boolean_(),
      new hydra.variants.LiteralVariant.Float_(),
      new hydra.variants.LiteralVariant.Integer_(),
      new hydra.variants.LiteralVariant.String_())));
    hydra.util.Lazy<hydra.util.PersistentSet<hydra.variants.TermVariant>> termVariants = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.util.ConsList.of(
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
    hydra.util.Lazy<hydra.util.PersistentSet<hydra.variants.TypeVariant>> typeVariants = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.util.ConsList.of(
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
    return new hydra.coders.Language(new hydra.coders.LanguageName("hydra.ext.scala"), new hydra.coders.LanguageConstraints(eliminationVariants.get(), literalVariants.get(), floatTypes.get(), functionVariants.get(), integerTypes.get(), termVariants.get(), typeVariants.get(), p0 -> hydra.ext.scala.Language.<hydra.core.Type>scalaLanguage_typePredicate(p0)));
  }

  static <T0> Boolean scalaLanguage_typePredicate(T0 ignored) {
    return true;
  }

  static hydra.util.PersistentSet<String> scalaReservedWords() {
    hydra.util.ConsList<String> classNames = hydra.util.ConsList.of(
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
    hydra.util.ConsList<String> keywords = hydra.util.ConsList.of(
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
    return hydra.lib.sets.FromList.apply(hydra.lib.lists.Concat.apply(hydra.util.ConsList.of(
      keywords,
      classNames,
      hydra.ext.scala.Language.<String>scalaReservedWords_hydraScalaKeywords())));
  }

  static <T0> hydra.util.ConsList<T0> scalaReservedWords_hydraScalaKeywords() {
    return hydra.util.ConsList.empty();
  }
}
