// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class TPrimaryAndSlices implements Serializable, Comparable<TPrimaryAndSlices> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.TPrimaryAndSlices");
  
  public static final hydra.core.Name FIELD_NAME_PRIMARY = new hydra.core.Name("primary");
  
  public static final hydra.core.Name FIELD_NAME_SLICES = new hydra.core.Name("slices");
  
  public final hydra.ext.python.syntax.TPrimary primary;
  
  public final hydra.ext.python.syntax.Slices slices;
  
  public TPrimaryAndSlices (hydra.ext.python.syntax.TPrimary primary, hydra.ext.python.syntax.Slices slices) {
    this.primary = primary;
    this.slices = slices;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TPrimaryAndSlices)) {
      return false;
    }
    TPrimaryAndSlices o = (TPrimaryAndSlices) other;
    return java.util.Objects.equals(
      this.primary,
      o.primary) && java.util.Objects.equals(
      this.slices,
      o.slices);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(primary) + 3 * java.util.Objects.hashCode(slices);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TPrimaryAndSlices other) {
    int cmp = 0;
    cmp = ((Comparable) primary).compareTo(other.primary);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) slices).compareTo(other.slices);
  }
  
  public TPrimaryAndSlices withPrimary(hydra.ext.python.syntax.TPrimary primary) {
    return new TPrimaryAndSlices(primary, slices);
  }
  
  public TPrimaryAndSlices withSlices(hydra.ext.python.syntax.Slices slices) {
    return new TPrimaryAndSlices(primary, slices);
  }
}
