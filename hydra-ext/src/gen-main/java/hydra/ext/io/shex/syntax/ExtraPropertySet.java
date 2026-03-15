// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class ExtraPropertySet implements Serializable, Comparable<ExtraPropertySet> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.ExtraPropertySet");
  
  public static final hydra.core.Name VALUE = new hydra.core.Name("value");
  
  public final hydra.util.ConsList<hydra.ext.io.shex.syntax.Predicate> value;
  
  public ExtraPropertySet (hydra.util.ConsList<hydra.ext.io.shex.syntax.Predicate> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ExtraPropertySet)) {
      return false;
    }
    ExtraPropertySet o = (ExtraPropertySet) other;
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
  public int compareTo(ExtraPropertySet other) {
    return Integer.compare(
      value.hashCode(),
      other.value.hashCode());
  }
}
