// Note: this is an automatically generated file. Do not edit.

package hydra.ext.owl.syntax;

import java.io.Serializable;

public class ObjectOneOf implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/owl/syntax.ObjectOneOf");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final java.util.List<hydra.ext.owl.syntax.Individual> value;
  
  public ObjectOneOf (java.util.List<hydra.ext.owl.syntax.Individual> value) {
    java.util.Objects.requireNonNull((value));
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ObjectOneOf)) {
      return false;
    }
    ObjectOneOf o = (ObjectOneOf) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}
