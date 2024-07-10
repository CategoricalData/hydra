// Note: this is an automatically generated file. Do not edit.

package hydra.langs.java.syntax;

import java.io.Serializable;

public class DefaultValue implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/java/syntax.DefaultValue");
  
  public final hydra.langs.java.syntax.ElementValue value;
  
  public DefaultValue (hydra.langs.java.syntax.ElementValue value) {
    if (value == null) {
      throw new IllegalArgumentException("null value for 'value' argument");
    }
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DefaultValue)) {
      return false;
    }
    DefaultValue o = (DefaultValue) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}