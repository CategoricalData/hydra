// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.xml.schema;

import java.io.Serializable;

public class Language implements Serializable, Comparable<Language> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.w3.xml.schema.Language");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final String value;

  public Language (String value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Language)) {
      return false;
    }
    Language o = (Language) other;
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
  public int compareTo(Language other) {
    return ((Comparable) value).compareTo(other.value);
  }
}
