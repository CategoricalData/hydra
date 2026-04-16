// Note: this is an automatically generated file. Do not edit.

package com.gdblab.pathAlgebra.syntax;

import java.io.Serializable;

public class RpqRestrictor implements Serializable, Comparable<RpqRestrictor> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("com.gdblab.pathAlgebra.syntax.RpqRestrictor");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final com.gdblab.pathAlgebra.syntax.RestrictorExt value;

  public RpqRestrictor (com.gdblab.pathAlgebra.syntax.RestrictorExt value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RpqRestrictor)) {
      return false;
    }
    RpqRestrictor o = (RpqRestrictor) other;
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
  public int compareTo(RpqRestrictor other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
