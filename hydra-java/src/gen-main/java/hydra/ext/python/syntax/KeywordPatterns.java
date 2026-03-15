// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class KeywordPatterns implements Serializable, Comparable<KeywordPatterns> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.python.syntax.KeywordPatterns");
  
  public static final hydra.core.Name VALUE = new hydra.core.Name("value");
  
  public final hydra.util.ConsList<hydra.ext.python.syntax.KeywordPattern> value;
  
  public KeywordPatterns (hydra.util.ConsList<hydra.ext.python.syntax.KeywordPattern> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof KeywordPatterns)) {
      return false;
    }
    KeywordPatterns o = (KeywordPatterns) other;
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
  public int compareTo(KeywordPatterns other) {
    return Integer.compare(
      value.hashCode(),
      other.value.hashCode());
  }
}
