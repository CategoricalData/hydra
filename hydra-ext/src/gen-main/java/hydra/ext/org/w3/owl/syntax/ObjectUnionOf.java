// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.owl.syntax;

import java.io.Serializable;

public class ObjectUnionOf implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.w3.owl.syntax.ObjectUnionOf");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final java.util.List<hydra.ext.org.w3.owl.syntax.ClassExpression> value;
  
  public ObjectUnionOf (java.util.List<hydra.ext.org.w3.owl.syntax.ClassExpression> value) {
    java.util.Objects.requireNonNull((value));
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ObjectUnionOf)) {
      return false;
    }
    ObjectUnionOf o = (ObjectUnionOf) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}