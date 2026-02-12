// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class Kwarg implements Serializable, Comparable<Kwarg> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.Kwarg");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final hydra.ext.python.syntax.Name name;
  
  public final hydra.ext.python.syntax.Expression value;
  
  public Kwarg (hydra.ext.python.syntax.Name name, hydra.ext.python.syntax.Expression value) {
    this.name = name;
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Kwarg)) {
      return false;
    }
    Kwarg o = (Kwarg) other;
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
  public int compareTo(Kwarg other) {
    int cmp = 0;
    cmp = ((Comparable) name).compareTo(other.name);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) value).compareTo(other.value);
  }
  
  public Kwarg withName(hydra.ext.python.syntax.Name name) {
    return new Kwarg(name, value);
  }
  
  public Kwarg withValue(hydra.ext.python.syntax.Expression value) {
    return new Kwarg(name, value);
  }
}
