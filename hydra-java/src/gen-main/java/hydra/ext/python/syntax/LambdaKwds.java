// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class LambdaKwds implements Serializable, Comparable<LambdaKwds> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.LambdaKwds");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final hydra.ext.python.syntax.LambdaParamNoDefault value;
  
  public LambdaKwds (hydra.ext.python.syntax.LambdaParamNoDefault value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LambdaKwds)) {
      return false;
    }
    LambdaKwds o = (LambdaKwds) other;
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
  public int compareTo(LambdaKwds other) {
    return ((Comparable) value).compareTo(other.value);
  }
}
