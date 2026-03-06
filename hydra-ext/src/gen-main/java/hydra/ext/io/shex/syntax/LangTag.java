// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class LangTag implements Serializable, Comparable<LangTag> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.LangTag");
  
  public static final hydra.core.Name VALUE = new hydra.core.Name("value");
  
  public final String value;
  
  public LangTag (String value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LangTag)) {
      return false;
    }
    LangTag o = (LangTag) other;
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
  public int compareTo(LangTag other) {
    return ((Comparable) value).compareTo(other.value);
  }
}
