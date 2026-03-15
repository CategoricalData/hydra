// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.owl.syntax;

import java.io.Serializable;

public class ObjectUnionOf implements Serializable, Comparable<ObjectUnionOf> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.w3.owl.syntax.ObjectUnionOf");
  
  public static final hydra.core.Name VALUE = new hydra.core.Name("value");
  
  public final hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.ClassExpression> value;
  
  public ObjectUnionOf (hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.ClassExpression> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ObjectUnionOf)) {
      return false;
    }
    ObjectUnionOf o = (ObjectUnionOf) other;
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
  public int compareTo(ObjectUnionOf other) {
    return Integer.compare(
      value.hashCode(),
      other.value.hashCode());
  }
}
