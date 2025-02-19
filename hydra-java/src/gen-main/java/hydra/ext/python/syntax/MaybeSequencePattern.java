// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class MaybeSequencePattern implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.MaybeSequencePattern");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final java.util.List<hydra.ext.python.syntax.MaybeStarPattern> value;
  
  public MaybeSequencePattern (java.util.List<hydra.ext.python.syntax.MaybeStarPattern> value) {
    java.util.Objects.requireNonNull((value));
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MaybeSequencePattern)) {
      return false;
    }
    MaybeSequencePattern o = (MaybeSequencePattern) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}