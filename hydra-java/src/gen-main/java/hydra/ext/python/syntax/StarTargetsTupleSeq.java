// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class StarTargetsTupleSeq implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.StarTargetsTupleSeq");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final java.util.List<hydra.ext.python.syntax.StarTarget> value;
  
  public StarTargetsTupleSeq (java.util.List<hydra.ext.python.syntax.StarTarget> value) {
    java.util.Objects.requireNonNull((value));
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof StarTargetsTupleSeq)) {
      return false;
    }
    StarTargetsTupleSeq o = (StarTargetsTupleSeq) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}