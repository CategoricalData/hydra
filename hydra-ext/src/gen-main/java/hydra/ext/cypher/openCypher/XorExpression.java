// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class XorExpression implements Serializable, Comparable<XorExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.openCypher.XorExpression");
  
  public static final hydra.core.Name VALUE = new hydra.core.Name("value");
  
  public final java.util.List<hydra.ext.cypher.openCypher.AndExpression> value;
  
  public XorExpression (java.util.List<hydra.ext.cypher.openCypher.AndExpression> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof XorExpression)) {
      return false;
    }
    XorExpression o = (XorExpression) other;
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
  public int compareTo(XorExpression other) {
    return Integer.compare(
      value.hashCode(),
      other.value.hashCode());
  }
}
