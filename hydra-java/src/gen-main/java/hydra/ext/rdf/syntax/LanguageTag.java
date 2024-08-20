// Note: this is an automatically generated file. Do not edit.

package hydra.ext.rdf.syntax;

import java.io.Serializable;

/**
 * A BCP47 language tag
 */
public class LanguageTag implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/rdf/syntax.LanguageTag");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final String value;
  
  public LanguageTag (String value) {
    java.util.Objects.requireNonNull((value));
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LanguageTag)) {
      return false;
    }
    LanguageTag o = (LanguageTag) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}
