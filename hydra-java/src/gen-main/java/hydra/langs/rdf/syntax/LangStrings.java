// Note: this is an automatically generated file. Do not edit.

package hydra.langs.rdf.syntax;

import java.io.Serializable;

/**
 * A convenience type which provides at most one string value per language, and optionally a value without a language
 */
public class LangStrings implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/rdf/syntax.LangStrings");
  
  /**
   * A convenience type which provides at most one string value per language, and optionally a value without a language
   */
  public final java.util.Map<java.util.Optional<hydra.langs.rdf.syntax.LanguageTag>, String> value;
  
  public LangStrings (java.util.Map<java.util.Optional<hydra.langs.rdf.syntax.LanguageTag>, String> value) {
    if (value == null) {
      throw new IllegalArgumentException("null value for 'value' argument");
    }
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