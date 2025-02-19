// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.language;

/**
 * Language constraints and reserved words for Python 3
 */
public interface Language {
  static hydra.coders.Language pythonLanguage() {
    return new hydra.coders.Language(new hydra.coders.LanguageName("hydra.ext.python"), new hydra.coders.LanguageConstraints(hydra.lib.sets.FromList.apply(java.util.Arrays.asList(
      new hydra.mantle.EliminationVariant.List(),
      new hydra.mantle.EliminationVariant.Optional(),
      new hydra.mantle.EliminationVariant.Product(),
      new hydra.mantle.EliminationVariant.Record(),
      new hydra.mantle.EliminationVariant.Union(),
      new hydra.mantle.EliminationVariant.Wrap())), hydra.lib.sets.FromList.apply(java.util.Arrays.asList(
      new hydra.mantle.LiteralVariant.Binary(),
      new hydra.mantle.LiteralVariant.Boolean_(),
      new hydra.mantle.LiteralVariant.Float_(),
      new hydra.mantle.LiteralVariant.Integer_(),
      new hydra.mantle.LiteralVariant.String_())), hydra.lib.sets.FromList.apply(java.util.Arrays.asList(new hydra.core.FloatType.Float64())), hydra.lib.sets.FromList.apply(java.util.Arrays.asList(
      new hydra.mantle.FunctionVariant.Elimination(),
      new hydra.mantle.FunctionVariant.Lambda(),
      new hydra.mantle.FunctionVariant.Primitive())), hydra.lib.sets.FromList.apply(java.util.Arrays.asList(new hydra.core.IntegerType.Bigint())), hydra.lib.sets.FromList.apply(java.util.Arrays.asList(
      new hydra.mantle.TermVariant.Application(),
      new hydra.mantle.TermVariant.Function(),
      new hydra.mantle.TermVariant.Let(),
      new hydra.mantle.TermVariant.List(),
      new hydra.mantle.TermVariant.Literal(),
      new hydra.mantle.TermVariant.Map(),
      new hydra.mantle.TermVariant.Optional(),
      new hydra.mantle.TermVariant.Product(),
      new hydra.mantle.TermVariant.Record(),
      new hydra.mantle.TermVariant.Set(),
      new hydra.mantle.TermVariant.Union(),
      new hydra.mantle.TermVariant.Variable(),
      new hydra.mantle.TermVariant.Wrap())), hydra.lib.sets.FromList.apply(java.util.Arrays.asList(
      new hydra.mantle.TypeVariant.Annotated(),
      new hydra.mantle.TypeVariant.Application(),
      new hydra.mantle.TypeVariant.Function(),
      new hydra.mantle.TypeVariant.Lambda(),
      new hydra.mantle.TypeVariant.List(),
      new hydra.mantle.TypeVariant.Literal(),
      new hydra.mantle.TypeVariant.Map(),
      new hydra.mantle.TypeVariant.Optional(),
      new hydra.mantle.TypeVariant.Product(),
      new hydra.mantle.TypeVariant.Record(),
      new hydra.mantle.TypeVariant.Set(),
      new hydra.mantle.TypeVariant.Union(),
      new hydra.mantle.TypeVariant.Variable(),
      new hydra.mantle.TypeVariant.Wrap())), (java.util.function.Function<hydra.core.Type, Boolean>) (ignored -> true)));
  }
  
  static java.util.Set<String> pythonReservedWords() {
    java.util.List<String> keywords = java.util.Arrays.asList(
      "False",
      "None",
      "True",
      "and",
      "as",
      "assert",
      "async",
      "await",
      "break",
      "class",
      "continue",
      "def",
      "del",
      "elif",
      "else",
      "except",
      "finally",
      "for",
      "from",
      "global",
      "if",
      "import",
      "in",
      "is",
      "lambda",
      "nonlocal",
      "not",
      "or",
      "pass",
      "raise",
      "return",
      "try",
      "while",
      "with",
      "yield");
    return hydra.lib.sets.FromList.apply((keywords));
  }
}