// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class StarTargetsListSeq implements Serializable, Comparable<StarTargetsListSeq> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.StarTargetsListSeq");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final java.util.List<hydra.ext.python.syntax.StarTarget> value;
  
  public StarTargetsListSeq (java.util.List<hydra.ext.python.syntax.StarTarget> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof StarTargetsListSeq)) {
      return false;
    }
    StarTargetsListSeq o = (StarTargetsListSeq) other;
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
  public int compareTo(StarTargetsListSeq other) {
    return Integer.compare(
      value.hashCode(),
      other.value.hashCode());
  }
}
