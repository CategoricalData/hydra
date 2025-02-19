// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class TPrimaryAndSlices implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.TPrimaryAndSlices");
  
  public static final hydra.core.Name FIELD_NAME_PRIMARY = new hydra.core.Name("primary");
  
  public static final hydra.core.Name FIELD_NAME_SLICES = new hydra.core.Name("slices");
  
  public final hydra.ext.python.syntax.TPrimary primary;
  
  public final hydra.ext.python.syntax.Slices slices;
  
  public TPrimaryAndSlices (hydra.ext.python.syntax.TPrimary primary, hydra.ext.python.syntax.Slices slices) {
    java.util.Objects.requireNonNull((primary));
    java.util.Objects.requireNonNull((slices));
    this.primary = primary;
    this.slices = slices;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TPrimaryAndSlices)) {
      return false;
    }
    TPrimaryAndSlices o = (TPrimaryAndSlices) (other);
    return primary.equals(o.primary) && slices.equals(o.slices);
  }
  
  @Override
  public int hashCode() {
    return 2 * primary.hashCode() + 3 * slices.hashCode();
  }
  
  public TPrimaryAndSlices withPrimary(hydra.ext.python.syntax.TPrimary primary) {
    java.util.Objects.requireNonNull((primary));
    return new TPrimaryAndSlices(primary, slices);
  }
  
  public TPrimaryAndSlices withSlices(hydra.ext.python.syntax.Slices slices) {
    java.util.Objects.requireNonNull((slices));
    return new TPrimaryAndSlices(primary, slices);
  }
}