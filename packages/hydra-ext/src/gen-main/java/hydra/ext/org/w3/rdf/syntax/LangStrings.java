// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.rdf.syntax;

import java.io.Serializable;

/**
 * A convenience type which provides at most one string value per language, and optionally a value without a language
 */
public class LangStrings implements Serializable, Comparable<LangStrings> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.w3.rdf.syntax.LangStrings");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final java.util.Map<hydra.util.Maybe<hydra.ext.org.w3.rdf.syntax.LanguageTag>, String> value;

  public LangStrings (java.util.Map<hydra.util.Maybe<hydra.ext.org.w3.rdf.syntax.LanguageTag>, String> value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LangStrings)) {
      return false;
    }
    LangStrings o = (LangStrings) other;
    return java.util.Objects.equals(
      this.value,
      o.value);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(value);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(LangStrings other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
