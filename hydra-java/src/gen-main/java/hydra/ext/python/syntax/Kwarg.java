// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class Kwarg implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.Kwarg");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final hydra.ext.python.syntax.Name name;
  
  public final hydra.ext.python.syntax.Expression value;
  
  public Kwarg (hydra.ext.python.syntax.Name name, hydra.ext.python.syntax.Expression value) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((value));
    this.name = name;
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Kwarg)) {
      return false;
    }
    Kwarg o = (Kwarg) (other);
    return name.equals(o.name) && value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * value.hashCode();
  }
  
  public Kwarg withName(hydra.ext.python.syntax.Name name) {
    java.util.Objects.requireNonNull((name));
    return new Kwarg(name, value);
  }
  
  public Kwarg withValue(hydra.ext.python.syntax.Expression value) {
    java.util.Objects.requireNonNull((value));
    return new Kwarg(name, value);
  }
}