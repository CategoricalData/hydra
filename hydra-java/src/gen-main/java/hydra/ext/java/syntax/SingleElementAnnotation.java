// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class SingleElementAnnotation implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.SingleElementAnnotation");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final hydra.ext.java.syntax.TypeName name;
  
  public final hydra.util.Opt<hydra.ext.java.syntax.ElementValue> value;
  
  public SingleElementAnnotation (hydra.ext.java.syntax.TypeName name, hydra.util.Opt<hydra.ext.java.syntax.ElementValue> value) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((value));
    this.name = name;
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SingleElementAnnotation)) {
      return false;
    }
    SingleElementAnnotation o = (SingleElementAnnotation) (other);
    return name.equals(o.name) && value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * value.hashCode();
  }
  
  public SingleElementAnnotation withName(hydra.ext.java.syntax.TypeName name) {
    java.util.Objects.requireNonNull((name));
    return new SingleElementAnnotation(name, value);
  }
  
  public SingleElementAnnotation withValue(hydra.util.Opt<hydra.ext.java.syntax.ElementValue> value) {
    java.util.Objects.requireNonNull((value));
    return new SingleElementAnnotation(name, value);
  }
}