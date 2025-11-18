// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.rdf.syntax;

import hydra.util.Maybe;

import java.io.Serializable;

/**
 * A convenience type which provides at most one string value per language, and optionally a value without a language
 */
public class LangStrings implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.w3.rdf.syntax.LangStrings");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final java.util.Map<Maybe<LanguageTag>, String> value;
  
  public LangStrings (java.util.Map<Maybe<LanguageTag>, String> value) {
    java.util.Objects.requireNonNull((value));
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
