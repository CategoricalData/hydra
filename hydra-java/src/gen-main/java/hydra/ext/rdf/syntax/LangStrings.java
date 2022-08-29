package hydra.ext.rdf.syntax;

/**
 * A convenience type which provides at most one string value per language, and optionally a value without a language
 */
public class LangStrings {
  /**
   * A convenience type which provides at most one string value per language, and optionally a value without a language
   */
  public final java.util.Map<java.util.Optional<hydra.ext.rdf.syntax.LanguageTag>, String> value;
  
  public LangStrings (java.util.Map<java.util.Optional<hydra.ext.rdf.syntax.LanguageTag>, String> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LangStrings)) {
      return false;
    }
    LangStrings o = (LangStrings) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}