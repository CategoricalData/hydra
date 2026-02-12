// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class StarAnnotation implements Serializable, Comparable<StarAnnotation> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.StarAnnotation");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final hydra.ext.python.syntax.StarExpression value;
  
  public StarAnnotation (hydra.ext.python.syntax.StarExpression value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof StarAnnotation)) {
      return false;
    }
    StarAnnotation o = (StarAnnotation) other;
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
  public int compareTo(StarAnnotation other) {
    return ((Comparable) value).compareTo(other.value);
  }
}
