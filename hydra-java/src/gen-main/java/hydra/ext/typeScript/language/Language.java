// Note: this is an automatically generated file. Do not edit.

package hydra.ext.typeScript.language;

/**
 * Language constraints for TypeScript
 */
public interface Language {
  static hydra.coders.Language typeScriptLanguage() {
    return new hydra.coders.Language(new hydra.coders.LanguageName("hydra.langs.typeScript"), new hydra.coders.LanguageConstraints(hydra.lib.sets.Empty.apply(), hydra.lib.sets.FromList.apply(java.util.Arrays.asList(
      new hydra.mantle.LiteralVariant.Boolean_(),
      new hydra.mantle.LiteralVariant.Float_(),
      new hydra.mantle.LiteralVariant.Integer_(),
      new hydra.mantle.LiteralVariant.String_())), hydra.lib.sets.FromList.apply(java.util.Arrays.asList(new hydra.core.FloatType.Float64())), hydra.lib.sets.Empty.apply(), hydra.lib.sets.FromList.apply(java.util.Arrays.asList(new hydra.core.IntegerType.Bigint())), hydra.lib.sets.FromList.apply(java.util.Arrays.asList(
      new hydra.mantle.TermVariant.List(),
      new hydra.mantle.TermVariant.Literal(),
      new hydra.mantle.TermVariant.Map(),
      new hydra.mantle.TermVariant.Optional(),
      new hydra.mantle.TermVariant.Record(),
      new hydra.mantle.TermVariant.Union())), hydra.lib.sets.FromList.apply(java.util.Arrays.asList(
      new hydra.mantle.TypeVariant.Annotated(),
      new hydra.mantle.TypeVariant.List(),
      new hydra.mantle.TypeVariant.Literal(),
      new hydra.mantle.TypeVariant.Map(),
      new hydra.mantle.TypeVariant.Optional(),
      new hydra.mantle.TypeVariant.Record(),
      new hydra.mantle.TypeVariant.Union(),
      new hydra.mantle.TypeVariant.Variable())), (java.util.function.Function<hydra.core.Type, Boolean>) (s4 -> ((s4)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.core.Type instance) {
        return true;
      }
      
      @Override
      public Boolean visit(hydra.core.Type.Map instance) {
        return (hydra.strip.Strip.stripType(((instance.value)).values)).accept(new hydra.core.Type.PartialVisitor<>() {
          @Override
          public Boolean otherwise(hydra.core.Type instance) {
            return true;
          }
          
          @Override
          public Boolean visit(hydra.core.Type.Optional instance) {
            return false;
          }
        });
      }
    }))));
  }
  
  static java.util.Set<String> typeScriptReservedWords() {
    java.util.List<String> strictModeReservedWords = java.util.Arrays.asList(
      "as",
      "implements",
      "interface",
      "let",
      "package",
      "private",
      "protected",
      "public",
      "static",
      "yield");
    java.util.List<String> reservedWords = java.util.Arrays.asList(
      "delete",
      "do",
      "else",
      "enum",
      "export",
      "extends",
      "false",
      "finally",
      "for",
      "function",
      "if",
      "import",
      "in",
      "instanceof",
      "new",
      "null",
      "return",
      "super",
      "switch",
      "this",
      "throw",
      "true",
      "try",
      "typeof",
      "var",
      "void",
      "while",
      "with");
    java.util.List<String> contextuallKeywords = java.util.Arrays.asList(
      "any",
      "boolean",
      "constructor",
      "declare",
      "from",
      "get",
      "module",
      "number",
      "of",
      "require",
      "set",
      "string",
      "symbol",
      "type");
    return hydra.lib.sets.FromList.apply(hydra.lib.lists.Concat.apply(java.util.Arrays.asList(
      (reservedWords),
      (strictModeReservedWords),
      (contextuallKeywords))));
  }
}