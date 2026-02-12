// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class SingleElementAnnotation implements Serializable, Comparable<SingleElementAnnotation> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.SingleElementAnnotation");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final hydra.ext.java.syntax.TypeName name;
  
  public final hydra.util.Maybe<hydra.ext.java.syntax.ElementValue> value;
  
  public SingleElementAnnotation (hydra.ext.java.syntax.TypeName name, hydra.util.Maybe<hydra.ext.java.syntax.ElementValue> value) {
    this.name = name;
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SingleElementAnnotation)) {
      return false;
    }
    SingleElementAnnotation o = (SingleElementAnnotation) other;
    return java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.value,
      o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name) + 3 * java.util.Objects.hashCode(value);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(SingleElementAnnotation other) {
    int cmp = 0;
    cmp = ((Comparable) name).compareTo(other.name);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      value.hashCode(),
      other.value.hashCode());
  }
  
  public SingleElementAnnotation withName(hydra.ext.java.syntax.TypeName name) {
    return new SingleElementAnnotation(name, value);
  }
  
  public SingleElementAnnotation withValue(hydra.util.Maybe<hydra.ext.java.syntax.ElementValue> value) {
    return new SingleElementAnnotation(name, value);
  }
}
